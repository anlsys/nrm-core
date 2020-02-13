let Cfg = ../resources/types/Cfg.dhall

in      ../resources/defaults/Cfg.dhall
      ⫽ { controlCfg =
            Some
            { minimumControlInterval =
                { fromuS = 1000000.0 }
            , staticPower =
                { fromuW = 2.0e8 }
            , learnCfg =
                < Lagrange :
                    { lagrangeConstraint : Double }
                | Knapsack :
                    { knapsackConstraint : Double }
                >.Lagrange
                { lagrangeConstraint = 1.0 }
            , speedThreshold =
                0.9
            , referenceMeasurementRoundInterval =
                +6
            }
        , verbose =
            < Error | Info | Debug >.Info
        }
    : Cfg
