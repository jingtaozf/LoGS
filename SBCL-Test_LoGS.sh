#!/bin/sh

sbcl --core ./LoGS-sbcl.core --eval '(progn (load "CLUnit")(load "tests.lisp")(cl-user::quit))'
