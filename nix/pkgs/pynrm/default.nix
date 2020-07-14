{ src, stdenv, pythonPackages, hwloc, linuxPackages, hsnrm }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  buildInputs = [ hsnrm ];
  propagatedBuildInputs = [
    hsnrm
    linuxPackages.perf
    pythonPackages.tornado
    pythonPackages.pyzmq
    pythonPackages.pyyaml
    pythonPackages.jsonschema
    pythonPackages.msgpack
    pythonPackages.warlock
  ];
  checkPhase = "true";
  preBuild = ''
    substituteInPlace bin/nrmd \
      --replace "os.environ[\"NRMSO\"]" \"${hsnrm}/bin/nrm.so\"
  '';
}
