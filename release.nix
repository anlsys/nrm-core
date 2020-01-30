{ hnrm }:
let

  pkgs = import hnrm { };

in { inherit (pkgs) git hello; }
