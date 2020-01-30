{ src }:
let

  pkgs = import src { };

in { inherit (pkgs) libnrm; }
