{ src, stdenv, pythonPackages, hwloc, linuxPackages, hsnrm, resources}:
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
    rm nrm/schemas/downstreamEvent.json
    cp ${resources}/share/nrm/downstreamEvent.json nrm/schemas/
    substituteInPlace bin/nrmd \
      --replace "os.environ[\"NRMSO\"]" \"${hsnrm}/bin/nrm.so\"
  '';
}
