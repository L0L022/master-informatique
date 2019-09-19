{ mkDerivation, base, extra, stdenv }:
mkDerivation {
  pname = "tp2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base extra ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
