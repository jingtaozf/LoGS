#!/bin/sh

lisp -core ./LoGS.core -eval '(load "CLUnit")(load "tests.lisp")(quit)'
