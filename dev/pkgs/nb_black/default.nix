{ src, stdenv, pythonPackages }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nb_black";
  propagatedBuildInputs =
    [ pythonPackages.black pythonPackages.yapf pythonPackages.ipython ];
}
