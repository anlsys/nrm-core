let pkgs = (import ../dev/default.nix { });
in pkgs.expe.overrideAttrs (o: {

  buildInputs = o.buildInputs
    ++ [ (pkgs.stream.override { iterationCount = "400"; })];

})
