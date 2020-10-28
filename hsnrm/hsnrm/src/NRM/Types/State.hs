{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Types.State
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.State
  ( NRMState (..),
    ExtraPassiveSensor (..),

    -- * Useful maps
    cmdIDMap,
    pidMap,
    awaitingCmdIDMap,

    -- * lookups
    lookupProcess,

    -- * Rendering views
    showSliceList,
    showSlices,

    -- * Lenses
    _cmdID,
    _sliceID,
  )
where

import Control.Lens
import Data.Aeson hiding ((.=))
import Data.Coerce
import Data.Data
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.Map as M
import Data.MessagePack
import Data.Scientific
import LensMap.Core
import NRM.Slices.Dummy
import NRM.Slices.Nodeos
import NRM.Slices.Singularity
import qualified NRM.Types.Actuator as A
import NRM.Types.Cmd
import NRM.Types.CmdID as CmdID
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Controller
import NRM.Types.MemBuffer
import NRM.Types.Process as P
import NRM.Types.Sensor as S
import NRM.Types.Slice as C
import NRM.Types.Topology
import NRM.Types.Units
import Protolude
import System.Process.Typed
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data NRMState
  = NRMState
      { pus :: M.Map PUID PU,
        cores :: M.Map CoreID Core,
        packages :: M.Map PackageID Package,
        slices :: Map SliceID Slice,
        dummyRuntime :: Maybe DummyRuntime,
        singularityRuntime :: Maybe SingularityRuntime,
        nodeosRuntime :: Maybe NodeosRuntime,
        controller :: Maybe Controller,
        extraStaticActuators :: Map Text Cfg.ExtraActuator,
        extraStaticPassiveSensors :: Map Text ExtraPassiveSensor
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

instance HasLensMap (Text, Cfg.ExtraActuator) A.ActuatorKey A.Actuator where
  lenses (actuatorID, _) =
    M.singleton
      (A.ExtraActuatorKey actuatorID)
      ( ScopedLens
          (_2 . lens getter setter)
      )
    where
      getter :: Cfg.ExtraActuator -> A.Actuator
      getter (coerce -> extraActuator) = A.Actuator
        { actions = Cfg.actions extraActuator,
          referenceAction = Cfg.referenceAction extraActuator,
          go = \value ->
            runProcess_ $
              System.Process.Typed.proc
                (toS $ Cfg.actuatorBinary extraActuator)
                ((toS <$> Cfg.actuatorArguments extraActuator) <> [show value])
        }
      setter :: Cfg.ExtraActuator -> A.Actuator -> Cfg.ExtraActuator
      setter oldExtraActuator actuator = coerce $
        oldExtraActuator &~ do
          #referenceAction .= A.referenceAction actuator
          #actions .= A.actions actuator

data ExtraPassiveSensor
  = ExtraPassiveSensor
      { extraPassiveSensor :: Cfg.ExtraPassiveSensor,
        lastRead :: Maybe (Time, Double),
        frequency :: Frequency,
        history :: MemBuffer
      }
  deriving (Eq, Show, Generic, MessagePack, ToJSON, FromJSON)

instance
  HasLensMap
    (Text, ExtraPassiveSensor)
    PassiveSensorKey
    PassiveSensor
  where
  lenses (sensorID, _) =
    M.singleton
      (S.ExtraPassiveSensorKey sensorID)
      ( ScopedLens
          (_2 . lens getter setter)
      )
    where
      getter :: ExtraPassiveSensor -> S.PassiveSensor
      getter ExtraPassiveSensor {..} =
        S.PassiveSensor
          { passiveMeta = S.SensorMeta
              { tags = Cfg.tags extraPassiveSensor,
                range = Cfg.toInterval $ Cfg.range extraPassiveSensor,
                lastReferenceMeasurements = history,
                last = lastRead,
                cumulative = Cfg.sensorBehavior extraPassiveSensor
              },
            frequency = frequency,
            perform =
              -- this megaparsec-based code might be given a module and improved
              -- if/when the need arises
              fmap toRealFloat
                . parseMaybe (lexeme L.scientific)
                . toS
                <$> readProcessStdout_
                  ( System.Process.Typed.proc
                      (toS $ Cfg.sensorBinary extraPassiveSensor)
                      (toS <$> Cfg.sensorArguments extraPassiveSensor)
                  )
          }
      setter :: ExtraPassiveSensor -> S.PassiveSensor -> ExtraPassiveSensor
      setter p passiveSensor =
        p &~ do
          #extraPassiveSensor . #range
            .= Cfg.toRange (passiveSensor ^. S._meta . #range)
          #history .= passiveSensor ^. S._meta . #lastReferenceMeasurements
          #lastRead .= passiveSensor ^. S._meta . #last

instance HasLensMap NRMState A.ActuatorKey A.Actuator where
  lenses s =
    mconcat
      [ addPath #packages <$> lenses (packages s),
        addPath #extraStaticActuators <$> lenses (extraStaticActuators s)
      ]

instance HasLensMap NRMState ActiveSensorKey ActiveSensor where
  lenses s =
    mconcat
      [ addPath #slices <$> lenses (slices s)
      ]

instance HasLensMap NRMState PassiveSensorKey PassiveSensor where
  lenses s =
    mconcat
      [ addPath #packages <$> lenses (packages s),
        addPath #extraStaticPassiveSensors <$> lenses (extraStaticPassiveSensors s)
      ]

instance JSONSchema NRMState where
  schema _ = schema (Proxy :: Proxy Text)

showSliceList :: [(SliceID, Slice)] -> Text
showSliceList l =
  mconcat $
    l <&> \(sliceID, Slice {..}) ->
      "slice: ID " <> C.toText sliceID <> "\n" <> mconcat (descCmd <$> M.toList cmds)
  where
    descCmd (cmdID, cmdCore -> CmdCore {..}) =
      " command: ID " <> CmdID.toText cmdID
        <> descSpec cmdPath arguments
        <> "\n"
    descSpec (Command cmd) args =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ showArg <$> args)
    showArg (Arg a) = a

-- | Renders a textual view of running slices
showSlices :: NRMState -> Text
showSlices NRMState {..} =
  showSliceList $ M.toList slices

lookupProcess :: ProcessID -> NRMState -> Maybe (CmdID, Cmd, SliceID, Slice)
lookupProcess cmdID st = M.lookup cmdID (pidMap st)

-- | NRM state map view by ProcessID.
pidMap :: NRMState -> M.Map ProcessID (CmdID, Cmd, SliceID, Slice)
pidMap s = mconcat $ M.toList (slices s) <&> mkMap
  where
    mkMap :: forall c. (c, Slice) -> M.Map ProcessID (CmdID, Cmd, c, Slice)
    mkMap x@(_, c) =
      M.fromList $
        zip
          (pid <$> M.elems (cmds c))
          (M.toList (cmds c) <&> mkTriple x)

mkTriple :: (c, d) -> (a, b) -> (a, b, c, d)
mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

lookupCmd :: CmdID -> NRMState -> Maybe (Cmd, SliceID, Slice)
lookupCmd cmdID st = M.lookup cmdID (cmdIDMap st)

-- | NRM state map view by cmdID of "running" commands.
cmdIDMap :: NRMState -> M.Map CmdID (Cmd, SliceID, Slice)
cmdIDMap = mkCmdIDMap cmds

-- | NRM state map view by cmdID of "awaiting" commands.
awaitingCmdIDMap :: NRMState -> M.Map CmdID (CmdCore, SliceID, Slice)
awaitingCmdIDMap = mkCmdIDMap awaiting

mkCmdIDMap ::
  Ord k =>
  (Slice -> M.Map k a) ->
  NRMState ->
  M.Map k (a, SliceID, Slice)
mkCmdIDMap accessor s = mconcat $ M.toList (slices s) <&> mkMap
  where
    mkMap x@(_, c) =
      M.fromList $
        zip
          (M.keys $ accessor c)
          (M.elems (accessor c) <&> mk x)
    mk :: (b, c) -> a -> (a, b, c)
    mk (cid, c) cm = (cm, cid, c)

-- Lenses
_sliceID :: SliceID -> Lens' NRMState (Maybe Slice)
_sliceID sliceID = #slices . at sliceID

_cmdID :: CmdID -> Lens' NRMState (Maybe Cmd)
_cmdID cmdID = lens getter setter
  where
    getter :: NRMState -> Maybe Cmd
    getter st = lookupCmd cmdID st <&> \(cmd, _, _) -> cmd
    setter st (Just cmd) =
      lookupCmd cmdID st & \case
        Just (_, sliceID, slice) ->
          st & _sliceID sliceID ?~ (slice & (#cmds . at cmdID) ?~ cmd)
        Nothing -> st
    setter st Nothing =
      lookupCmd cmdID st & \case
        Just (_, sliceID, _) ->
          st & _sliceID sliceID %~ mayRemoveSlice
        Nothing -> st
    mayRemoveSlice :: Maybe Slice -> Maybe Slice
    mayRemoveSlice x =
      x >>= \slice ->
        if length (cmds slice) == 1
          then Nothing
          else Just $ slice & #cmds . at cmdID .~ Nothing
