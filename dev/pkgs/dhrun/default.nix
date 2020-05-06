{ mkDerivation, aeson, aeson-extra, aeson-pretty, ansi-terminal
, base, bytestring, conduit, conduit-extra, containers
, data-default, dhall, dhall-json, directory, filepath
, generic-lens, generic-random, Glob, hspec, lens, mtl
, neat-interpolation, optparse-applicative, prettyprinter, process
, protolude, quickcheck-text, stdenv, tasty, tasty-golden
, tasty-hspec, tasty-hunit, tasty-quickcheck, text, time, unix
, unliftio, unliftio-core, yaml, src
}:
mkDerivation {
  pname = "dhrun";
  version = "1.0.1";
  inherit src;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-extra aeson-pretty ansi-terminal base bytestring
    conduit conduit-extra containers data-default dhall dhall-json
    directory filepath generic-lens lens mtl neat-interpolation
    optparse-applicative prettyprinter process protolude text time unix
    unliftio-core yaml
  ];
  executableHaskellDepends = [ protolude ];
  testHaskellDepends = [
    aeson aeson-extra base bytestring data-default dhall directory
    filepath generic-random Glob hspec mtl protolude quickcheck-text
    tasty tasty-golden tasty-hspec tasty-hunit tasty-quickcheck text
    unliftio yaml
  ];
  doHaddock = false;
  description = "Dhall/YAML configurable concurrent integration test executor";
  license = stdenv.lib.licenses.mit;
}
