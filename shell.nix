{ useGhcide ? false

, stream ? false

}:
let pkgs = import ./dev/default.nix { inherit useGhcide; };
in pkgs.hack.overrideAttrs (o: {

  buildInputs = o.buildInputs ++ pkgs.lib.optional stream pkgs.stream;

})
