{ stdenv, fetchFromGitHub, pythonPackages }:

pythonPackages.buildPythonPackage rec {
  name = "importlab";

  src = fetchFromGitHub {
    owner = "google";
    repo = "importlab";
    rev = "171d0b5687dd6a9d2c5b01b4b2c3ecce2d79dddb";
    sha256 = "1qlz2b7271na0gpr6qgr43rqrmgg2g9gf1whbsg29d346578bbgv";
  };

  propagatedBuildInputs = [ pythonPackages.networkx pythonPackages.six ];

  checkPhase = "true";
}
