{ src, stdenv, pythonPackages, hwloc, linuxPackages, hsnrm }:
pythonPackages.buildPythonPackage {
  inherit src;
  name = "nrm";
  propagatedBuildInputs = [
    pythonPackages.tornado
    pythonPackages.pyzmq
    pythonPackages.pyyaml
    pythonPackages.jsonschema
    pythonPackages.msgpack
    pythonPackages.warlock
  ];
  checkPhase = "true";
  preInstall = ''
    ${hsnrm}/bin/generate downstream $out/nrm/schemas
  '';
}
