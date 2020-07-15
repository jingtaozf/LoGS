;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2018 James Earl Prewett

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

(in-package :cl-user)

;; we need the sb-posix package under SBCL in order to open FIFOs non-blocking
#+sbcl
(require :sb-posix)

#+acl
(require :osi) ;; for file stat, etc.

; set this to something *BIG* 
;; this is 1/2 of my physical memory; that seems to work well; YMMV
;#+cmu
;(LISP::%SET-BYTES-CONSED-BETWEEN-GCS 65712128)

;#+sbcl
;(setf (SB-EXT:BYTES-CONSED-BETWEEN-GCS) 65712128)

; Turn off gc messages
#+cmu
(setq ext:*gc-verbose* NIL)

#+(or ecl cmu sbcl allegro openmcl lispworks clisp)
(defpackage :org.prewett.LoGS
  (:nicknames :LoGS)
  (:use :cl
        #+allegro :clos
        #+cmu :pcl
        #+sbcl :sb-mop
        #+lispworks :hcl
        :cl-user
        :cl-cli ;; my command-line processing code
        :cl-ppcre
        #+sbcl
        :sb-sys
        #+sbcl
        :sb-unix
	)
  #+sbcl
  (:import-from :SB-EXT #:QUIT #:RUN-PROGRAM)
  #+sbcl
  (:import-from :sb-unix #:unix-stat #:unix-open #:o_rdonly #:SIGINT)
  #+sbcl
  (:import-from :sb-sys #:make-fd-stream)
  #+openmcl
  (:import-from :ccl #:make-fd-stream)
  #+cmu
  (:import-from :extensions #:quit #:RUN-PROGRAM)
  #+cmu
  (:shadowing-import-from :pcl #:standard-class #:built-in-class
                          #:find-class #:class-name #:class-of)
  (:export main
           get-context 
           add-to-context
           *now*
           *LoGS-internal-time-units-per-second*
           mail
           ruleset
           rule
           enqueue
           *root-ruleset*
           *environment*
           match-all
           exec
           exec-returning-value
           write-to-file ;; XXX should this be exported? XXX
           file-write
           pipe
           get-LoGS-env-var
           message
           ensure-context
           doubly-linked-list
           data
           tail
           dll-insert
           head
           llink
           rlink
           dll-delete
           list-entries
           check-rule
           check-rules
           check-limits
           dead-p
           file-follower
           list-follower
           timeout
           timeout-fn
           environment
           process-files
           match
           no-match
           add-item
           get-logline
           *messages*
           *message*
           process-command-line
           filestream
           name
           expire-context
           next-timeout
           single-with-suppress
           *timeout-object-timeout-queue*
           pair
           single
           suppress-until
           suppress
           filter
           alias-context
           delete-context
           context-exceeded-limit-p
           *RELATIVE-TIMEOUT-OBJECT-TIMEOUT-QUEUE*
           REMOVE-CONTEXT-IF-STALE
           *contexts*
           CONTEXT-EXCEEDED-LIMIT-P
           get-rule
           filename
           get-line
           RULE-EXCEEDED-LIMIT-P
           match-none
           elements
           ecount
           rule-tail
           rule-after
           rule-before
           rule-head
           priority-queue
           data
           *current-rule*
           *ruleset*
           ;; *rule*
           context
           window
           pbs-file-follower
           spawn
           stdin-follower
           multi-follower
           *contexts-hash*
           *contexts-alias-hash*
           timeout
           with-LoGS-env-vars
           ))

#-(or ecl cmu sbcl allegro openmcl lispworks clisp)
(error "LoGS is not supported on your Lisp")
