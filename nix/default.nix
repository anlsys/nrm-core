{ pkgs, hnrm-src ? ../. }: pkgs // rec {
  lib = import ./utils.nix;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        call-haskell-from-anything =
          super.call-haskell-from-anything.overrideAttrs (o: {
            src = ../call-haskell-from-anything;
            buildInputs = o.buildInputs ++ [ pkgs.python3 ];
          });
        regex = pkgs.haskell.lib.doJailbreak super.regex;
        hnrm = (self.callCabal2nix "hnrm" (lib.filter hnrm-src)) { };
      };
  };
  hnrm = haskellPackages.hnrm;
}
