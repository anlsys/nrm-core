{-|

Module      : Nrm.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Sensor
  (  )
where

{-import Nrm.Types.Metadata-}
{-import Nrm.Types.Topology as T-}
{-import Nrm.Types.Units as U-}
{-import Protolude-}

{-data PluginCfg-}
  {-= PluginCfg-}
      {-{ jsonCfg :: Text-}
      {-, path :: Text-}
      {-}-}

{-data LibnrmCfg-}
  {-= LibnrmCfg-}
      {-{ ratelimitLibnrm :: Int-}
      {-}-}

{-data PerfwrapperCfg-}
  {-= PerfwrapperCfg-}
      {-{ ratelimitPerfwrapper :: Int-}
      {-}-}

{-data RaplCfg-}
  {-= RaplCfg-}
      {-{ someth :: Int-}
      {-}-}

{-data PackageSensor = RAPL (Sensor RaplCfg Double) | Plugin (Sensor PluginCfg Double)-}

{-data ProcessSensor = PefwrapperSensor (Sensor PerfwrapperCfg Double)-}

{-data ThreadSensor = LibnrmSensor (Sensor LibnrmCfg Double)-}

{-data SensorFlow = DownstreamAgent | CallbackSensor {frequency :: Int}-}

{-data ActiveSensor cfg input output-}
  {-= ActiveSensor-}
      {-{ range :: Range output-}
      {-, cfg :: cfg-}
      {-}-}

{-callPackageSensor :: PackageSensor -> T.PackageID -> IO output-}
{-callPackageSensor (RAPL _) packageID = undefined -- call internal plugin-}
{-callPackageSensor (Plugin _) packageID = undefined -- call plugin with cfg and packageID-}
