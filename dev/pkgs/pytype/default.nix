{ src, stdenv, pythonPackages, cmake, bison, flex, python, ninja }:

pythonPackages.buildPythonPackage {
  inherit src;
  name = "pytype";
  nativeBuildInputs = [ cmake ninja ];
  propagatedBuildInputs = [
    ninja
    pythonPackages.importlab
    pythonPackages.six
    pythonPackages.pyyaml
    pythonPackages.typed-ast
  ];
  buildInputs = [ bison flex python ];
  checkPhase = "true";
  patches = ./dep.patch;
}
