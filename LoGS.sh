#!/bin/sh

LOGS_CORE=LoGS.core
MYRULES=myrules.lisp

#lisp -core $LOGS_CORE -eval "(in-package :LoGS) (load (compile-file \"$MYRULES\")) (setf *messages* (make-instance \'file-follower :filename \"messages\")) (main) (cl-user::quit)"

echo lisp -core $LOGS_CORE -eval \
	"(and " \
	"(in-package :org.prewett.LoGS)" \
	"(load (compile-file \"$MYRULES\"))" \
	"(setf *messages* (make-instance 'file-follower :filename \"messages\"))" \
	"(main) (cl-user::quit))" \

