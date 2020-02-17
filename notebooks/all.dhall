let all =
      ./dev/dhrun/all-tests.dh
      "../dev/dhrun/assets/"
      "../notebooks/control.dhall // { verbose=<Normal|Verbose|Debug>.Debug }"
      "../resources/examples/"

in  all.amg False (< Cap : Text | NoCap : {} >.Cap "200")
