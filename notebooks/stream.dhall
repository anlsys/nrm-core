        let all = ./dev/dhrun/all-tests.dh
          "../dev/dhrun/assets/"
          "../resources/defaults/Cfg.dhall // { verbose=<Normal|Verbose|Debug>.Debug }"
          "../resources/examples/"
          in all.stream  False (<Cap : Text | NoCap : {}>.Cap "201")
