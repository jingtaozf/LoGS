#!/bin/sh

# I don't understand why this isn't working!
openmcl -I ./LoGS.image -e '(progn (load "CLUnit.lisp") (load "tests.lisp") (quit))'
