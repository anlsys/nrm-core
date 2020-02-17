{ useGhcide ? false

, stream ? false

}:
let pkgs = import ./dev/default.nix { inherit useGhcide; };
in pkgs.nrm.overrideAttrs (o: {

  buildInputs = o.buildInputs ++ pkgs.lib.optional stream pkgs.stream-raw;

})
