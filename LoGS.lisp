; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2005 James Earl Prewett

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; proper optimizations?
(declaim  (OPTIMIZE (SPEED 3) (debug 0) (SAFETY 0)))
;(declaim (optimize (speed 0) (debug 3) (safety 3)))

;; freeze the LoGS classes if we're on cmucl 19
#+CMU19
(declaim (EXTENSIONS:FREEZE-TYPE file-follower Multi-Follower priority-queue context collection limited-collection doubly-linked-list doubly-linked-list-item priority-queue-item timeout-object relative-timeout-object rule ruleset killable-item message string-message))

;; we need the sb-posix package under SBCL in order to open FIFOs non-blocking
#+sbcl
(require :sb-posix)

#+acl
(require :osi) ;; for file stat, etc.

; set this to something *BIG* 
;; this is 1/2 of my physical memory; that seems to work well; YMMV
#+cmu
(LISP::%SET-BYTES-CONSED-BETWEEN-GCS 131424256)

#+sbcl
(setf (SB-EXT:BYTES-CONSED-BETWEEN-GCS) 65712128)


; Turn off gc messages
#+cmu
(setq ext:*gc-verbose* ())

;; define the LoGS package
(defpackage :LoGS
  (:use :cl
	#+allegro :clos
	#+cmu :pcl
        #+sbcl :sb-mop
	#+lispworks :hcl
        :cl-user)
  #+sbcl
  (:import-from :SB-EXT #:QUIT #:RUN-PROGRAM)
  #+sbcl
  (:import-from :sb-unix #:unix-stat #:unix-open #:o_rdonly)
  #+sbcl
  (:import-from :sb-sys #:make-fd-stream)
  #+cmu
  (:import-from :extensions #:quit #:RUN-PROGRAM)
  #+cmu
  (:shadowing-import-from :pcl #:standard-class #:built-in-class
                          #:find-class #:class-name #:class-of))

(in-package :LoGS)

;; this is a constant so we can optimize out the checks for production runs
(defconstant +debug+ () "should debugging information be displayed?")

(defparameter *use-internal-real-time* t 
  "should LoGS use the internal-real-time?")

(defvar *now* (get-internal-real-time)
  "the current time.  Currently an integer like (get-internal-real-time).
  This may change, do not rely on this.")
(declaim (type INTEGER *now*))

;; 0.03 seems to be a good value for my laptop.
;; how do I properly set this?
(defparameter *LoGS-sleep-time* .03
  "How long to sleep when there is no input")

(defparameter *remember-file* ()
  "Should we remember with file a message comes from?")

(defun load-LoGS-file (filename &key directory)
  "load the named file"
  (load 
   (compile-file
    (make-pathname :name filename
                   :type "lisp"
                   :version ()
                   :directory directory))))

;;;; XXX CLEAN THIS SECTION UP XXX!
;; load fundamental data structures
(load-LoGS-file "doubly-linked-list" :directory '(:relative "data_structures"))
(load-LoGS-file "priority-queue" :directory '(:relative "data_structures"))

;; load LoGS data structures
(load-LoGS-file "message")
(load-LoGS-file "named-object")
(load-LoGS-file "timeout-object")
(load-LoGS-file "relative-timeout-object")
(load-LoGS-file "killable-item")
(load-LoGS-file "collection")
(load-LoGS-file "limited-collection")
(load-LoGS-file "context")



;; load low-level file stuff
#+cmu
(load-LoGS-file "File-Follower_CMUCL.low" :directory '(:relative "Data_Sources"))
#+sbcl
(load-LoGS-file "File-Follower_SBCL.low" :directory '(:relative "Data_Sources"))
#+openmcl
(load-LoGS-file "File-Follower_OpenMCL.low" :directory '(:relative "Data_Sources"))
#+allegro
(load-LoGS-file "File-Follower_Allegro.low" :directory '(:relative "Data_Sources"))

;; load message producers
(load-LoGS-file "Data-Source" :directory '(:relative "Data_Sources"))

(load-LoGS-file "List-Follower" :directory '(:relative "Data_Sources"))
(load-LoGS-file "File-Follower" :directory '(:relative "Data_Sources"))

(load-LoGS-file "Multi-Follower" :directory '(:relative "Data_Sources"))

;; load rules
(load-LoGS-file "rule")
(load-LoGS-file "ruleset")
(load-LoGS-file "actions")

(load-LoGS-file "Parlance")

;; XXX This is where the loser stars go XXX
(defvar *messages* ()
  "The current message source.")
(defvar *ruleset* (make-instance 'ruleset)
  "The current ruleset.  It may be nested within another ruleset")
(defvar *root-ruleset* (make-instance 'ruleset :name 'root-ruleset)
  "The top-most ruleset; kinda like / in the filesystem")
(defvar *message* ()
  "The message currently being considered.")
(defvar *matches* ()
  "The last thing that matched.")
(defvar *sub-matches* ()
  "the last set of sub-matches (nil if there were no sub-matches)")
(defvar *current-rule* () 
  "The rule currently being compared to the message.")
(defvar *run-forever* ()
  "Should we keep going once we've reached the end of the file?")

;; does nothing... yet.
(defvar *die-die-die* ()
  "a list of functions to call when LoGS is done running (before exiting).")

;; none of this belongs here!

(defgeneric check-limits (thing)
  (:method-combination OR)
  (:documentation "Check to see if the object has exceeded one or more of its limits"))

;; command line options
#+cmu
(load-LoGS-file "command-line_CMUCL")

;; signal processing
;; someday we should have some!
;; these are just here till I get to it... 
#+cmu
(defun sigint-handler (signal code scp)
  (declare (ignore signal code scp))
  (with-interrupts
    (break "someone hit ctrl-c" t)))

#+cmu
(defun sighup-handler (signal code scp)
  (declare (ignore signal code scp))
  (with-interrupts
      (break "someone sent us a hup" t)))

(defun main ()
  "Main is the current LoGS mainline.  As of this revision, it tries
to mimic Logsurfer's behaviour (modulo context handling).  Main iterates over 
incoming messages, checking each against the current ruleset. 

Main currently does:
1. Processes an incoming line if there is one.
2. Check to see if any rules need to be removed.
3. Check to see if any contexts need to be removed."
  (loop as *message* = (get-logline *messages*)
     ;; exit if there is no message and we're not running forever
     when (and (not *run-forever*) (not *message*))
     do
       (return)
     ;; if we are running forever and there is no message sleep 
     when (and *run-forever* (not *message*))
     do
       (sleep *LoGS-sleep-time*)
       
     ;; update the internal time
     when *use-internal-real-time*
     do
       (setq *now* (get-internal-real-time))
       
     ;; check the message against the ruleset if it exists
     ;; and check the timeout objects
     when t
     do
       (when (and *message* (head *root-ruleset*))
         (check-rules *message* *root-ruleset*))
       
       (when (head *timeout-object-timeout-queue*)
         (check-limits *timeout-object-timeout-queue*))
       
       (when (head *relative-timeout-object-timeout-queue*)
         (check-limits *relative-timeout-object-timeout-queue*))))
