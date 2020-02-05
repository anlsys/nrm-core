let ExitCode = < ExitSuccess | ExitFailure : { _1 : Integer } >

let FileCheck =
      { filename : Text, filecheck : { avoids : List Text, wants : List Text } }

in  { Cfg =
        ./Cfg.dhall
    , FileCheck =
        FileCheck
    , EnvVar =
        { varname : Text, value : Text }
    , Cleaning =
        < Keep | Remove >
    , ExitCode =
        ExitCode
    , Verbosity =
        < Normal | Verbose >
    , Cmd =
        { name :
            Text
        , exitcode :
            Optional ExitCode
        , args :
            List Text
        , vars :
            List { varname : Text, value : Text }
        , passvars :
            List Text
        , out :
            { filename : Text, filecheck : List Text }
        , err :
            { filename : Text, filecheck : List Text }
        , postchecks :
            List FileCheck
        , timeout :
            Optional Integer
        , otherwd :
            Optional Text
        }
    }
