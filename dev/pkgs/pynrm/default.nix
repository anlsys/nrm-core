{ src, stdenv, pythonPackages, hwloc, linuxPackages }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  propagatedBuildInputs = [
    pythonPackages.six
    pythonPackages.numpy
    pythonPackages.tornado
    pythonPackages.pyyaml
    pythonPackages.pyzmq
    hwloc
    linuxPackages.perf
    pythonPackages.docopt
    pythonPackages.scipy
    pythonPackages.warlock
    pythonPackages.jsonschema
  ];
  checkPhase = "true";
}
