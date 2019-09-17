-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ sensors :
    List
    { sensorKey :
        { sensorID : Text }
    , sensorDesc :
        { source :
            Text
        , sensorMeta :
            { range :
                { min : Double, max : Double }
            , frequency :
                < MaxFrequency :
                    { maxFrequency : { fromHz : Double } }
                | FixedFrequency :
                    { fixedFrequency : { fromHz : Double } }
                >
            }
        }
    }
, actuators :
    List
    { actuatorKey :
        { actuatorID : Text }
    , actuatorDesc :
        { target : Text, actuatorMeta : Text }
    }
, objective :
    { linearCombination :
        List { w : Double, x : { sensorID : Text } }
    , direction :
        < Minimize | Maximize >
    }
}
