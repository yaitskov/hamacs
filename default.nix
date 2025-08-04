{ system ? builtins.currentSystem or "x86_64-linux"
, sources ? import (if builtins.pathExists /home/dan/pro/nixpkgs
                    then ./nix/offline/sources.nix
                    else ./nix/sources.nix)
, ghc ? "ghc9122"
}:
let
  np = import sources.nixpkgs { overlays = []; config = {}; };
  hp = np.haskell.packages.${ghc};

  inherit (np.haskell.lib) dontHaddock;
  inherit (np) lib;
  inherit (lib) strings pipe;
  inherit (strings) concatStringsSep;

  sourceRegexes = [ "^(LICENSE|run.el|(src|cbits|packages).*)$" "^.*\\.cabal$" ];
  # extraLibs is required because cabal2Nix does not support Custom build type
  # where these deps can be specified
  extraLibs = [
    "--extra-lib-dirs=${hp.ghc}/lib/ghc-9.12.2/lib/x86_64-linux-ghc-9.12.2-ae43"
    "--ghc-option=-optl=-lHSrts-1.0.2_thr-ghc9.12.2"
  ];
  linkExtraLibs = drv:
    drv.overrideAttrs(oa: {
      dontPatchELF = true; # strips rts library when false
      configureFlags = (oa.configureFlags or []) ++ extraLibs;
    });
  soShortCut = drv:
    drv.overrideAttrs(oa: {
      installPhase =
        (oa.installPhase or "") + ''
        ln -s $out/lib/ghc-9.12.2/lib/x86_64-linux-ghc-9.12.2*/*hamacs*.so $out/lib/hamacs.so
        '';
    });
  emacs-integration-test = drv:
    drv.overrideAttrs (oa: {
      buildInputs = (oa.builtInputs or []) ++ [np.emacs np.tree];
      checkPhase = (oa.checkPhase or "") + ''
        echo Emacs Integration Tests
        export NIX_GHC_LIBDIR=${(hp.ghcWithPackages (h: hamacs.getCabalDeps.libraryHaskellDepends))}/lib/ghc-9.12.2/lib
        ln -s dist/build/libHShamacs*.so hamacs.so
        emacs -Q -L . --batch -l run.el
      '';
    });

  hamacs =
    (hp.callCabal2nix "hamacs" (lib.sourceByRegex ./. sourceRegexes) { })
      |> emacs-integration-test |> soShortCut |> linkExtraLibs |> dontHaddock ;

  shell = hp.shellFor {
    packages = p: [ hamacs ];
    nativeBuildInputs = (with np; [
      cabal-install
      ghcid
      hlint
      niv
      emacs
      cachix
    ]) ++ [ hp.haskell-language-server ];
    shellHook = ''
      export PS1='$ '
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref

      function cabal() {
        case "$1" in
          build)
            shift
            ${np.cabal-install}/bin/cabal build ${concatStringsSep " " extraLibs} "$@" ;;
          *) ${np.cabal-install}/bin/cabal "$@" ;;
        esac
      }
      export -f cabal
    '';
  };
in {
  inherit shell;
  inherit hamacs;
}
