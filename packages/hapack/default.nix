{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
, nix-ghc-libdir-out-file ? "/nix-ghc-libdir-out-file is not set"
, sources ? import (if builtins.pathExists /home/dan/pro/nixpkgs
                    then ./nix/offline/sources.nix
                    else ./nix/sources.nix)
}:
let
  np = import sources.nixpkgs {
    overlays = [ (n: o: { hamacs = (import ../.. { inherit sources ghc system; }).hamacs; }) ];
  };
  hp = np.haskell.packages.${ghc};

  inherit (np.haskell.lib) dontHaddock;
  inherit (np) lib;

  sourceRegexes = [ "^.*\\.(hs|cabal|el)$" ];

  hapack =
    (hp.callCabal2nix "hapack" (lib.sourceByRegex ./. sourceRegexes) { })
      |> dontHaddock;

  hamacsEnv = hp.shellFor {
    packages = p: [ hapack ];
    nativeBuildInputs = with np; [ emacs ];
    shellHook = ''
      function mk_link_to_hamacs () {
        ln -s -f ${np.hamacs}/lib/hamacs.so hamacs.so
        ln -s -f ${np.emacs}/bin/emacs emacs
      }

      function gen_nix_ghc_libdir () {
        [ -f ${nix-ghc-libdir-out-file} ] && { echo "File ${nix-ghc-libdir-out-file} already exists. Fail"; exit 1; }
        echo -n ${(hp.ghcWithPackages (h: hapack.getCabalDeps.libraryHaskellDepends))}/lib/ghc-${hp.ghc.version}/lib/package.conf.d | tee ${nix-ghc-libdir-out-file}
      }
    '';
  };

  shell = hp.shellFor {
    packages = p: [ hapack ];
    nativeBuildInputs = (with np; [
      cabal-install
      ghcid
      hlint
      niv
      emacs
    ]) ++ [ hp.haskell-language-server ];
    shellHook = ''
      export PS1='$ '
      # extract path to emacs-module.h from prefix
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
      echo "Run [hamacs-test] instead of [cabal test]"
      function hamacs-test() {
        emacs -Q -L ${np.hamacs}/lib --batch -l run.el
      }
    '';
  };
in {
  inherit shell;
  inherit hamacsEnv;
  inherit hapack;
}
