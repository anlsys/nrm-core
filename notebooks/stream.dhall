let all = ./all.dhall

in  all.stream False (< Cap : Text | NoCap : {} >.Cap "200")
