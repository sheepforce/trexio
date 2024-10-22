final: prev: {
  trexio = final.callPackage ./trexio.nix { };

  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev: {
      trexio = hfinal.callCabal2nix "trexio" ../haskell {
        inherit (final) trexio;
        filepath = hfinal.filepath_1_5_3_0;
      };
    };
  };
}
