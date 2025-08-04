![](https://github.com/github/docs/actions/workflows/test.yml/badge.svg)

[![Licence](https://img.shields.io/github/license/Ileriayo/markdown-badges?style=for-the-badge)](./LICENSE)
![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![GitHub Actions](https://img.shields.io/badge/github%20actions-%232671E5.svg?style=for-the-badge&logo=githubactions&logoColor=white)
![Nix](https://img.shields.io/badge/NIX-5277C3.svg?style=for-the-badge&logo=NixOS&logoColor=white)

[![](https://raw.githubusercontent.com/dch82/Nixpkgs-Badges/main/nixpkgs-badge-light.svg)](https://search.nixos.org/packages?size=1&show={{PACKAGENAME}})


# Hamacs

Hamacs - Haskell as a second language for Emacs.

## Motivation

This is not the first attempt of Emacs invasion with Haskell.  Hamacs
project takes root from
[haskelisp](https://hackage.haskell.org/package/haskelisp).
[Emacs-module](https://hackage.haskell.org/package/emacs-module) is
another one. Both packages looks similar, but `emacs-module` has more
elaborate types and well maintained, but I was not able to find any
project using them.

`Emacs-module` readme decisively explains drawbacks of sticking to
Lisp these days, but why it is not popular?

I think these solutions haven't got traction because:

* compilation requirement -
  [elisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/)
  is an interpretable language. It is an advantage in an embedded
  environment, which makes the development cycle shorter and promotes
  quick prototyping. Plus building Haskell programs is pretty far from
  perfection.

* concerned open sourceness - this follows straight from the above
  bullet - building package from source is labor intensive and an
  typical user will be inclined to download opaque binary files, which
  automatically raise a safety issue, nonetheless software is
  distributed under GPL license.

* lack of app store - such tool has little value without packages
  providing practical usefulness to end user, who is an ultimate
  driver, because he wants features and discovers bugs and the process
  triggers develoment in all subsequent layers down to OS kernel. So
  without a few killer apps such tool would never get a place in the
  Sun.


Integration with Haskell faces lots of technical issues in comparison
with Python and Lua, but these mainstream scripting languages has
fundamental issue - they are as dynamically typed as Lisp. So this fact
undermines the whole idea for integration with them.

After all cold and logical inference, as daily Emacs user, I just want
to enjoy developing extensions (from an idea to installation step) and
as for now it is a terriable experience. I implemented a few Emacs
packages (rectangular-indent, color-mode, bullet-proof-general, and
niv-mode) and besides tedious debugging I found that publishing
packages to Melpa is way harder than to
[Hackage](https://hackage.haskell.org) due less inclusive polices. As
result I was not able to publish anything there.

## Installation

Installation process is handled with dedicated package
[boot-hamacs](https://github.com/yaitskov/boot-hamacs). It is a Lisp
package installing [nix](https://nix.dev/) under the hood.

Once it is over the portal to honderland is open.

## Overview

An Emacs extention built on top of Hamacs project is a
[Cabal](https://www.haskell.org/cabal/) package and it can be built
via `cabal build`. This way Hamacs extensions can be distributed via
Hackage repository. Though Hamacs has its own package loader dealing
wit Cabal file immediately. Info extracted from a Cabal file is fed to
Haskell interpreter [hint](https://hackage.haskell.org/package/hint).

Hamacs allows to expose Haskell to Emacs with doc strings with option to mark
them as interactive, access Emacs API from Haskell and interleave them
in cases as with `save-excursion`.

Haskell functions defined in Cabal exposed modules are exported by
default.  Names are prefixed with package name. Function names can be
adopted to Emacs naming convention.

## Hello world

Simplest hamacs package require Cabal file and Haskell source one.
Let's consider `rectangular-indet` as first hamacs package adoptation.

### Original version in Emacs Lisp

``` emacs-lisp
(defun mulstring (str n)
  "Multiplay string N times."
  (if (> n 0)
      (concat str
	      (mulstring str (- n 1)))
    ""))

(defun myshift ()
  "Shift region.
by number of columns between cursor column and column
with first non space char of the line with the cursor."
  (interactive)
  (let (a
        (mx (max (point) (mark)))
	(mn (min (point) (mark)))
        )
    (save-excursion
      (goto-char mn)
      (beginning-of-line)
      (setq a (- (re-search-forward "[^ ]")
		 mn 1))
      (beginning-of-line)
      (if (> a 0)
	  (replace-regexp (concat "^"
				  (mulstring " " a))
			  ""
			  nil
			  (point)
			  mx)
	(replace-regexp "^"
			(mulstring " " (- a))
			nil
			(point)
			mx)))
    (goto-char mn)))
```

### Hamacs equivalent

Both files are located in the folder named after the package.

#### rectangular-indet.cabal
```cabal
cabal-version: 3.8
name: rectangular-indet
version: 0.0.1
license: GPL-3.0-only
library
  exposed-modules: Lib
  build-depends: base, hamacs
  hs-source-dirs: .
```

#### Lib.hs

``` haskell
module Lib where

import Control.Lens
import Data.Generics.Labels as X ()
import Data.MinMax1 (minmaxP)
import Data.Text qualified as T
import Emacs
import Relude

{-# ANN exe (DocString """Shift region by number of columns between
                          cursor column and column with first non space char
                          of the line with the cursor.""") #-}
{-# ANN exe Interactive #-}
exe :: EmacsM ()
exe = do
  (mn, mx) <- minmaxP <$> point <*> mark
  saveExcursion do
    void $ gotoChar mn
    void $ beginningOfLine
    a <- (\x -> x - mn  - 1) <$> reSearchForward "[^ ]"
    void $ beginningOfLine
    cp <- point
    let customize o = o & #start ?~ cp & #end ?~ mx
    if a > 0
      then replaceRegexp
           (EmacsRegexp $ "^" <> T.replicate (unBufPos a) " ")
           ""
           customize
      else replaceRegexp "^"
           (T.replicate (unBufPos (-a)) " ")
           customize
    void $ gotoChar mn
```

### Loading hamacs package

``` emacs-lisp
(add-to-list 'load-path "hamacs-packages")
(require 'hamacs)
(hamacs-load-package "rectangular-shift"))

;; Tests:
(with-temp-buffer
  (insert "hello")
  (push-mark)
  (rectangular-shift-exe)
  (cl-assert (equal (buffer-string) "     hello") t "increase indent of single line"))

(with-temp-buffer
  (insert "hello")
  (beginning-of-line)
  (push-mark)
  (insert "     ")
  (rectangular-shift-exe)
  (cl-assert (equal (buffer-string) "hello") t "decrease indent of single line"))
```

## Package repository

Hamacs packages can be discovered by `Hamacs` category keyword on
[Hackage](https://hackage.haskell.org/packages/search?terms=%28category%3A+Emacs%29):

> (category: Hamacs)
