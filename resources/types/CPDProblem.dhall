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
    { _1 :
        { sensorID : Text }
    , _2 :
        { source :
            Text
        , sensorMeta :
            { range :
                { min : Double, max : Double }
            , frequency :
                { maxFrequency : { fromHz : Double } }
            }
        }
    }
, actuators :
    List
    { _1 :
        { actuatorID : Text }
    , _2 :
        List < DiscreteText : { _1 : Text } | DiscreteDouble : { _1 : Double } >
    }
, objective :
    { linearCombination :
        List { w : Double, x : { sensorID : Text } }
    , direction :
        < Minimize | Maximize >
    }
}
