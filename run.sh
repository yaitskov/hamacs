#!/bin/bash

set -e

emacs -Q -L $PWD --batch -l run.el
