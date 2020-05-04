{ mkDerivation, aeson, aeson-extra, aeson-pretty, async, base
, binary, brick, bytestring, conduit, conduit-extra, containers
, data-default, data-msgpack, dhall, dhall-json, directory
, editor-open, enclosed-exceptions, filepath, flat, generic-aeson
, generic-data, generic-deriving, generic-lens, hbandit, hxt
, hxt-xpath, intervals, json-schema, lens, MonadRandom, mtl
, mtl-compat, neat-interpolation, optparse-applicative
, pretty-simple, prettyprinter, protolude, random
, recursion-schemes, refined, regex, resourcet, src, stdenv
, storable-endian, template-haskell, text, transformers
, typed-process, units, units-defs, unix, unordered-containers
, uuid, vcs-revision, vector, yaml, zeromq4-conduit
, zeromq4-haskell
}:
mkDerivation {
  pname = "nrmlib";
  version = "1.0.0";
  inherit src;
  libraryHaskellDepends = [
    aeson aeson-extra aeson-pretty async base binary brick bytestring
    conduit conduit-extra containers data-default data-msgpack dhall
    dhall-json directory editor-open enclosed-exceptions filepath flat
    generic-aeson generic-data generic-deriving generic-lens hbandit
    hxt hxt-xpath intervals json-schema lens MonadRandom mtl mtl-compat
    neat-interpolation optparse-applicative pretty-simple prettyprinter
    protolude random recursion-schemes refined regex resourcet
    storable-endian template-haskell text transformers typed-process
    units units-defs unix unordered-containers uuid vcs-revision vector
    yaml zeromq4-conduit zeromq4-haskell
  ];
  description = "hsnrm";
  license = stdenv.lib.licenses.bsd3;
}
