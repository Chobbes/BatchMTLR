{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "BashMTLR";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ directoryTree ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    homepage = "https://github.com/Chobbes/BatchMTLR";
    description = "Batch running of MTLR training for patient specific survival prediction.";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
