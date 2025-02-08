{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc947"
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

  base = dontHaddock (hsPkgs.callCabal2nix "elisp-ghci" (lib.sourceByRegex ./. sources) { });
  elisp-ghci-overlay = _hf: _hp: { elisp-ghci = base; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay elisp-ghci-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.elisp-ghci ];
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

  elisp-ghci = hsPkgs.elisp-ghci;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit elisp-ghci;
}
