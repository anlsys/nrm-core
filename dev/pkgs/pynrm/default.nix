{ src, stdenv, pythonPackages, hwloc, linuxPackages }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  propagatedBuildInputs = [
    pythonPackages.six
    pythonPackages.tornado
    pythonPackages.pyzmq
    pythonPackages.docopt
    pythonPackages.scipy
    pythonPackages.jsonschema
  ];
  checkPhase = "true";
}
