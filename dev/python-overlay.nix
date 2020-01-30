_: pkgs:
let
  fetched = s: (pkgs.nix-update-source.fetch s).src;
  noCheck = p: p.overridePythonAttrs (_: { doCheck = false; });
  noCheckAll = pkgs.lib.mapAttrs (name: p: noCheck p);
  packageOverrides = pself: psuper:
    noCheckAll {
      importlab = pself.callPackage ./pkgs/importlab { };
      pyzmq = psuper.pyzmq.override { zeromq = pkgs.zeromq; };
      pytype = pself.callPackage ./pkgs/pytype {
        src = fetched ./pkgs/pytype/pin.json;
      };
      nb_black = pself.callPackage ./pkgs/nb_black {
        src = pkgs.fetchFromGitHub {
          owner = "dnanhkhoa";
          repo = "nb_black";
          rev = "cf4a07f83ab4fbfa2a2728fdb8a0605704c830dd";
          sha256 = "11qapvda8jk8pagbk7nipr137jm58i68nr45yar8qg8p3cvanjzf";
        };
      };
    };

in rec {
  python37 = pkgs.python37.override (old: {
    packageOverrides =
      pkgs.lib.composeExtensions (old.packageOverrides or (_: _: { }))
      packageOverrides;
  });
  python37Packages = python37.passthru.pkgs;
}
