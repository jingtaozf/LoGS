
.PHONY: ensure_lisp all

LISP ?= sbcl
LISP_LOC := $(shell which $(LISP) )
file_list := $(shell find . -name '*.lisp')
LOGS_CORE = LoGS-$(LISP).core
LOGS_EXE = LoGS

all: LoGS

ensure_lisp:
	which $(LISP)
	echo LISP_LOC: $(LISP_LOC)

.PHONY: ensure_lisp

$(LOGS_CORE): ensure_lisp
	sbcl --eval "(progn (require 'asdf) (require 'logs) (in-package :org.prewett.LoGS) (SAVE-LISP-AND-DIE \"$(LOGS_CORE)\"))"

LoGS-debug.core: ensure_lisp
	sbcl --eval "(progn (defconstant +debug+ t) (require 'asdf) (require 'logs) (in-package :org.prewett.LoGS) (SAVE-LISP-AND-DIE "LoGS-debug.core"))"

LoGS-debug: LoGS-debug.core
	sbcl --core LoGS-debug.core --eval "(SB-EXT:SAVE-LISP-AND-DIE \"$(LOGS_EXE)\" :executable t :save-runtime-options t :toplevel #'main)"

$(LOGS_EXE): $(LOGS_CORE)
	sbcl --core $(LOGS_CORE) --eval "(SB-EXT:SAVE-LISP-AND-DIE \"$(LOGS_EXE)\" :executable t :save-runtime-options t :toplevel #'main)"

.PHONY: clean
clean:
	rm -f $(LOGS_CORE) $(LOGS_EXE)
