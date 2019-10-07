{ src, stdenv, pythonPackages, hwloc, linuxPackages }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  propagatedBuildInputs = [
    pythonPackages.tornado
    pythonPackages.pyzmq
    pythonPackages.jsonschema
    pythonPackages.msgpack
    pythonPackages.warlock
  ];
  checkPhase = "true";
}
