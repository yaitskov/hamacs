{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  inherit (pkgs.haskell.lib) dontHaddock;
  inherit (pkgs) lib;
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  sources = [
    "^src.*$"
    "^LICENSE$"
    "^cbits.*$"
    "configure$"
    "^.*\\.cabal$"
  ];

  base = dontHaddock (hsPkgs.callCabal2nix "hamacs" (lib.sourceByRegex ./. sources) { });
  hamacs-overlay = _hf: _hp: { hamacs = base; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay hamacs-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.hamacs ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
    ]) ++ [ hls ];
    shellHook = ''
      export PS1='$ '
      # extract path to emacs-module.h from prefix
      # emacs --batch  --no-init-file  --eval '(message "%s" system-configuration-options)'
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
    '';
  };

  hamacs = hsPkgs.hamacs;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit hamacs;
}
