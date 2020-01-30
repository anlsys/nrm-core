{ hnrm }:
let

  pkgs = import hnrm { src = hnrm; };

in { inherit (pkgs) git hello; }
