{ useGhcide ? false

, stream ? false

, streamIterationCount ? "400"

}:
let pkgs = import ./dev/default.nix { inherit useGhcide; };
in pkgs.hack.overrideAttrs (o: {

  buildInputs = o.buildInputs ++ pkgs.lib.optional stream
    (pkgs.stream.override { iterationCount = streamIterationCount; });

})
