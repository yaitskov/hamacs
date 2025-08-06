{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
, nix-ghc-libdir-out-file
}: (import ./. {
  inherit system;
  inherit ghc;
  inherit nix-ghc-libdir-out-file;
}).hamacsEnv
