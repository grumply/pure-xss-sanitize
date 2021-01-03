{ mkDerivation, base, containers, pure-core, pure-txt, xss-sanitize, stdenv
}:
mkDerivation {
  pname = "pure-xss-sanitize";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers pure-core pure-txt xss-sanitize ];
  homepage = "github.com/grumply/pure-xss-sanitize";
  license = stdenv.lib.licenses.bsd3;
}
