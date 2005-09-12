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
(declaim (EXTENSIONS:FREEZE-TYPE NAMED-OBJECT FROM-MESSAGE RULESET TIMEOUT-OBJECT RULE DOUBLY-LINKED-LIST PRIORITY-QUEUE RELATIVE-TIMEOUT-OBJECT MESSAGE MULTI-FOLLOWER PRIORITY-QUEUE-ITEM COLLECTION PBS-FILE-FOLLOWER CONTEXT LIST-FOLLOWER STRING-MESSAGE KILLABLE-ITEM FILE-FOLLOWER DATA-SOURCE DOUBLY-LINKED-LIST-ITEM LIMITED-COLLECTION WINDOW))

;; we need the sb-posix package under SBCL in order to open FIFOs non-blocking
#+sbcl
(require :sb-posix)

#+acl
(require :osi) ;; for file stat, etc.

; set this to something *BIG* 
;; this is 1/2 of my physical memory; that seems to work well; YMMV
#+cmu
(LISP::%SET-BYTES-CONSED-BETWEEN-GCS 65712128)

#+sbcl
(setf (SB-EXT:BYTES-CONSED-BETWEEN-GCS) 65712128)

; Turn off gc messages
#+cmu
(setq ext:*gc-verbose* NIL)

;; define the LoGS package
(defpackage :LoGS
  (:use :cl
	#+allegro :clos
	#+cmu :pcl
        #+sbcl :sb-mop
	#+lispworks :hcl
        :cl-user
        :cl-cli ;; my command-line processing code
        :cl-ppcre)
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
(defconstant +debug+ NIL "should debugging information be displayed?")
(declaim (type (or t nil) +debug+))

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
(declaim (type FLOAT *LOGS-sleep-time*))

(defparameter *remember-file* ()
  "Should we remember with file a message comes from?")
(declaim (type (or t nil) *remember-file*))

(defparameter *tag-messages* ()
  "should we allow messages to be tagged?")

;; this is a constant so we can optimize out the checks for production runs
(defconstant +enable-rule-count+ t "will we allow rules/rulesets to be counted?")

(defconstant +use-sql+ () "should we build in the buffered-sql-follower?")

(defparameter *count-rules* t
  "should we keep track of rule counts?")

;; load up all subordinate files
;(load "load-LoGS.lisp")

;; XXX This is where the loser stars go XXX
(defvar *messages* ()
  "The current message source.")
(defvar *root-ruleset* ()
  "The top-most ruleset; kinda like / in the filesystem")
(defvar *ruleset* *root-ruleset*
  "The current ruleset.  It may be nested within another ruleset")
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
(defvar *run-before-exit* ()
  "a list of functions to call when LoGS is done running (before exiting).")

(defgeneric check-limits (thing)
  (:method-combination OR)
  (:documentation "Check to see if the object has exceeded one or more of its limits"))

(defun quit-LoGS ()
  #+allegro
  (exit)
  #-allegro
  (quit))

(defun main ()
  (progn
    ;; process any command line options
    (when +debug+
      (format t "processing options~%"))
    (let ((args (get-application-args)))
      (process-command-line *opts* args))
    ;; process any files
    (process-files)
    (mapcar
     (lambda (function)
       (funcall function))
     *run-before-exit*)))

;; pretty much the former mainline
;; adding the option processing to the mainline made testing more difficult
;; so I broke the processing out to a separate function.
(defun process-files ()    
  
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
