;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2007 James Earl Prewett

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
        :org.prewett.cl-cli ;; my command-line processing code
        :cl-ppcre)
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
           get-context add-to-context
           *now*
           *LoGS-internal-time-units-per-second*
           mail
           ruleset
           rule
           enqueue
           *root-ruleset*
           match-all
           exec
           exec-returning-value
           write-to-file ;; XXX should this be exported? XXX
           file-write
           pipe
           get-LoGS-env-var
           ))

#-(or ecl cmu sbcl allegro openmcl lispworks clisp)
(error "LoGS is not supported on your Lisp")
