{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Text
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module CPD.Text
  ( showProblemDhall,
  )
where

import CPD.Core
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyT
import Dhall
import qualified Dhall.Lint as Lint
import qualified Dhall.TypeCheck as Dhall
import Protolude

{-import qualified Data.Map as DM-}

{-showSensors :: [SensorKV] -> Text-}
{-showSensors = show-}

{-showSensors :: DM.Map SensorID Sensor -> Text-}
{-showSensors sl = mconcat $ (\(id, x) -> showSensor id x <> "\n ") <$> DM.toList sl-}

{-showSensor :: SensorID -> Sensor -> Text-}
{-showSensor id Sensor {..} =-}
{-"ID  " <> show id <> "Source  " <> show source <>-}
{-" tags:" <>-}
{-(mconcat . intersperse " " $ show <$> sensorTags) <>-}
{-" " <>-}
{-show sensorMeta <>-}
{-" " <>-}
{-show sensorDesc <>-}
{-" \n"-}
{-showActuators :: [ActuatorKV] -> Text-}
{-showActuators = show-}

{-showActuators sl = mconcat $ (\(id, x) -> showActuator id x <> "\n ") <$> DM.toList sl-}

{-showActuator :: ActuatorID -> Actuator -> Text-}
{-showActuator _ _ = "  "-}
{-showObjective :: Objective -> Text-}
{-showObjective = show-}
showProblemDhall :: Problem -> Text
showProblemDhall p = PrettyT.renderStrict $ Pretty.layoutSmart prettyOpts (Pretty.pretty expr)
  where
    expr = Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) p

prettyOpts :: Pretty.LayoutOptions
prettyOpts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0
    }
