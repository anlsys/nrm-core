--version

-Nrm+NRM

-Container-> Slice

only pass container list view in list reply

dhall `expected`:  https://hackage.haskell.org/package/dhall-1.25.0/docs/Dhall.html#t:Type

configuration and manifest newtypes

clean 'TODO's

properly split message types in sub-newtypes

locate and remove {-*-} comments

-start from message format.

-eliminate fromJust's

-eliminate "panic"

- add: 
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wmonomorphism-restriction
    -Wimplicit-prelude
    -Wmissing-local-signatures

	

    -Wmissing-exported-signatures
    -Wmissing-export-lists
    -Wmissing-import-lists
    -Wmissing-home-modules
    -Widentities

	

    -Wredundant-constraints
    -Wpartial-fields
    -Wmissed-specialisations
    -Wall-missed-specialisations

- introduce lib-runtime error messages

- change model to a list of behaviors

- write the python code with _getattrs_ ?

- properly do NoSuchCmd NoSuchContainer

- remove unused accessors and updaters from state interface.

- upstream req fix for no such cmd

- remove parseID -like functions
