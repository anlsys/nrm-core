{ src }:
_: pkgs:
let
  fetched = s: (pkgs.nix-update-source.fetch s).src;
  noCheck = p: p.overridePythonAttrs (_: { doCheck = false; });
  noCheckAll = pkgs.lib.mapAttrs (name: p: noCheck p);
  packageOverrides = pself: psuper:
    {
      pynrm = pself.callPackage ./pkgs/pynrm {
        src = src + "/pynrm";
        hsnrm = pkgs.haskellPackages.hsnrm-bin;
      };
    } // noCheckAll {
      #jupyter_client = psuper.jupyter_client;
      #nbformat = psuper.nbformat;
      #ipykernel = psuper.ipykernel;
      #jupyter_core = psuper.jupyter_core;
      #networkx = psuper.networkx;
      #nbconvert = psuper.nbconvert;

      importlab = pself.callPackage (src + "/dev/pkgs/importlab") { };
      pyzmq = psuper.pyzmq.override { zeromq = pkgs.zeromq; };
      nb_black = pself.callPackage (src + "/dev/pkgs/nb_black") {
        src = pkgs.fetchFromGitHub {
          owner = "dnanhkhoa";
          repo = "nb_black";
          rev = "cf4a07f83ab4fbfa2a2728fdb8a0605704c830dd";
          sha256 = "11qapvda8jk8pagbk7nipr137jm58i68nr45yar8qg8p3cvanjzf";
        };
      };
    };

in rec {
  python = pkgs.python3.override (old: {
    packageOverrides =
      pkgs.lib.composeExtensions (old.packageOverrides or (_: _: { }))
      packageOverrides;
  });
  pythonPackages = python.passthru.pkgs;
}
