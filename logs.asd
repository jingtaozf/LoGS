;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
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
                 (:module "Language"
                          :components ((:file "Logic")))))


