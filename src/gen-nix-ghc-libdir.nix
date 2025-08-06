{ np ? import /home/dan/pro/nixpkgs { overlays = []; }
, system ? builtins.currentSystem or "x86_64-linux"
, cabal-project-name
, nix-ghc-libdir-out-file
, ghc ? "ghc9122"
, sourceRegexes ? [ "^(LICENSE|.*\\.(hs|cabal))$" ]
}:

let
  hp = np.haskell.packages.${ghc};

  inherit (np) lib;
  dropExt = s: builtins.head (builtins.split "[.]" (builtins.baseNameOf s));
  justProName = dropExt cabal-project-name;
  cabOpts = "--subpath " + cabal-project-name;
  cabalProject = builtins.trace ("cabopts " + cabOpts + " ; pro name = " + justProName)
    (hp.callCabal2nixWithOptions
    # (hp.callCabal2nix
      justProName
      (builtins.dirOf cabal-project-name) # (lib.sourceByRegex ./. sourceRegexes)
      "" #  (cabOpts)
      {});

  shell = hp.shellFor {
    packages = p: [ cabalProject ];
    nativeBuildInputs = (with np; []);
    shellHook = ''
      function gen_nix_ghc_libdir () {
        [ -f ${nix-ghc-libdir-out-file} ] && { echo "File ${nix-ghc-libdir-out-file} already exists. Fail"; exit 1; }
        echo -n ${(hp.ghcWithPackages (h: cabalProject.getCabalDeps.libraryHaskellDepends))}/lib/ghc-${hp.ghc.version}/lib/package.conf.d | tee ${nix-ghc-libdir-out-file}
      }
    '';
  };
in shell
