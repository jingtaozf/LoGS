#!/bin/sh

alisp -I ./LoGS.dxl -e '(progn (load "CLUnit")(load "tests.lisp")(exit))'
