#!/bin/sh

# Logs extensible (common-lisp based) log/event analysis engine/language
# Copyright (C) 2003-2008 James Earl Prewett

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


LOGS_CORE="LoGS-sbcl.core"
LOGS_EXE="logs-replay"
# CYBERTIGGYR_TIME="time.lisp"

sbcl --eval '(require :asdf)' --eval "(progn (push #p\"../../\" asdf:*central-registry*) (require 'cybertiggyr-time) (require 'logs) (in-package :org.prewett.LoGS) (SAVE-LISP-AND-DIE \"$LOGS_CORE\"))"
sbcl --core $LOGS_CORE --load "logs-replay.lisp" --eval "(SB-EXT:SAVE-LISP-AND-DIE \"$LOGS_EXE\" :executable t :toplevel #'main)"
