let types = ./types.dhall

in  { emptyCmd =
          λ(args : { name : Text, outfile : Text, errfile : Text })
        →   { name = "echo"
            , exitcode = Some
                < ExitSuccess | ExitFailure : { _1 : Integer } >.ExitSuccess
            , args = [] : List Text
            , vars = [] : List { varname : Text, value : Text }
            , passvars = [ "PATH" ] : List Text
            , out = { filename = args.outfile, filecheck = [] : List Text }
            , err = { filename = args.errfile, filecheck = [] : List Text }
            , postchecks = [] : List types.FileCheck
            , timeout = None Integer
            , otherwd = None Text
            }
          : types.Cmd
    }
