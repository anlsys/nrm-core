let pkgs = import dev/default.nix { };
in {

  inherit (pkgs) libnrm;

}
