;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2006 James Earl Prewett

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defsystem "logs"
    :description "Log analysis engine"
    :version "0.1.2-pre"
    :author "James E. Prewett <Jim@Prewett.org>"
    :licence "GPL"
    :serial t
    :depends-on (:cl-ppcre :cl-cli) 
    :components ((:file "package")
                 (:file "LoGS")
                 (:module "data_structures" 
                          :components ((:file "doubly-linked-list")
                                       (:file "priority-queue"))
                          :depends-on ("LoGS"))
                 (:file "message")
                 (:file "named-object")
                 (:file "timeout-object")
                 (:file "relative-timeout-object")
                 (:file "killable-item")
                 (:file "collection")
                 (:file "limited-collection")
                 (:file "context")
                 (:file "window")
                 (:module "Data_Sources"
                          :components ((:file 
                                        #+cmu 
                                        "File-Follower_CMUCL_low"
                                        #+sbcl
                                        "File-Follower_SBCL_low"
                                        #+openmcl
                                        "File-Follower_OpenMCL_low"
                                        #+allegro
                                        "File-Follower_Allegro_low"
                                        #+clisp
                                        "File-Follower_CLISP_low"
                                        #+lispworks
                                        "File-Follower_LispWorks_low")
                                       (:file "Data-Source")
                                       (:file "List-Follower")
                                       (:file "File-Follower")
                                       (:file "PBS-File-Follower")
                                       (:file "Spawn")
                                       (:file "STDIN-Follower")
                                       (:file "Multi-Follower")
                                       (:file "Buffered-SQL-Follower")))
                 (:file "rule")
                 (:file "ruleset")
                 (:file "actions")
                 (:file "Parlance")
                 (:file "LoGS-command-line")
                 (:file "mainline")
                 (:module "Language" 
                          :components ((:file "defpackage")
                                       (:file "LoGS-time")
                                       (:file "rdl"))
                          :depends-on ("LoGS"))
                 ))
