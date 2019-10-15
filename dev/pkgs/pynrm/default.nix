{ src, stdenv, pythonPackages, hwloc, linuxPackages, hsnrm, resources }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  buildInputs = [ hsnrm ];
  propagatedBuildInputs = [
    hsnrm
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
    cp ${resources}/downstreamEvent.json nrm/schemas/
    substituteInPlace bin/nrmd --replace build/build/x86_64-linux/ghc-8.6.5/hsnrm-1.0.0/x/nrm.so/build/nrm.so/nrm.so ${hsnrm}/bin/nrm.so
  '';
}
