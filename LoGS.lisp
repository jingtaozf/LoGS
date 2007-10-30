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

; proper optimizations?
(declaim (OPTIMIZE speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
;; (declaim (optimize (speed 0) (debug 3) (safety 3)))

(in-package :org.prewett.LoGS)

;; freeze the LoGS classes if we're on cmucl 19
#+CMU19
(declaim (EXTENSIONS:FREEZE-TYPE NAMED-OBJECT RULESET TIMEOUT-OBJECT RULE DOUBLY-LINKED-LIST PRIORITY-QUEUE RELATIVE-TIMEOUT-OBJECT MESSAGE MULTI-FOLLOWER PRIORITY-QUEUE-ITEM COLLECTION PBS-FILE-FOLLOWER CONTEXT LIST-FOLLOWER STRING-MESSAGE KILLABLE-ITEM FILE-FOLLOWER DATA-SOURCE DOUBLY-LINKED-LIST-ITEM LIMITED-COLLECTION WINDOW))

(eval-when (:compile-toplevel)
  (defconstant +LoGS-version+ "0.1.2-pre"))
    
;; this is a constant so we can optimize out the checks for production runs
(defconstant +debug+ NIL "The +debug+ constant causes additional debugging information to be displayed while LoGS is running. Currently, debbuging is either on or off (by default, it is off). Since debugging code is splattered througout LoGS, it is important that this be a compile-time option so that the compiler may remove debugging statements when debugging is not needed.")

(defvar *LoGS-internal-time-units-per-second* internal-time-units-per-second)

(defmacro LoGS-debug (message &rest rest)
  `(when +debug+
     (format t ,message ,@rest)))

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
(defconstant +enable-rule-count+ t "The +enable-rule-count+ constant allows for counts to be kept when the *count-rules* variable is non-NIL. Note that this constant allows for the counts to be kept, while the *count-rules* parameter causes those counts to be kept.")

(defconstant +use-sql+ () "The +use-sql+ constant allows for the Buffered-SQL-Follower and related SQL code to be built into LoGS")

(defparameter *count-rules* t
  "should we keep track of rule counts?")

(defvar *write-pid-to-file* ()
  "should we write the PID of LoGS to a file?")

;; XXX This is where the loser stars go XXX
(defvar *messages* ()
  "The current message source.")
(defvar *root-ruleset* ()
  "The top-most ruleset; kinda like / in the filesystem")
(defvar *ruleset* *root-ruleset*
  "The current ruleset.  It may be nested within another ruleset")
(defvar *message* ()
  "The message currently being considered.")
(defvar *current-rule* () 
  "The rule currently being compared to the message.")
(defvar *run-forever* ()
  "Setting this variable to non-NIL causes LoGS to not exit when there is no more input immediately available.")
(defvar *run-before-exit* ()
  "a list of functions to call when LoGS is done running (before exiting).")
(defvar *timestamp-format* "%b %d %H:%M:%S"
  "The format the timestamp is in.  Currently, this uses time.lisp's format")
(defvar *timestamp-start* 0
  "the start of the timestamp on each log line; used in 'offline mode'")
(defvar *timestamp-end* 15
  "the end of the timestamp on each log line; used in 'offline mode'")
(defvar *parse-timestamp* ()
  "should LoGS set *NOW* from the timestamp on each line?")
(defvar *quit-lisp-when-done* t
  "Should we exit the Lisp process when we're done processing files?")

(defvar *opts* ()
  "a list of command-line options that LoGS understands")

(defgeneric check-limits (thing)
  (:method-combination OR)
  (:documentation "Check to see if the object has exceeded one or more of its limits"))

;; this function is here so that we can have a single function to 
;; call to exit LoGS/Lisp
(defun quit-LoGS ()
  #+allegro
  (excl::exit)
  #-allegro
  (cl-user::quit))

;; load a file, compile it if needed
(defun load-LoGS-file (filename)
  (let ((the-filename (make-pathname 
                       :name filename 
                       :type "lisp" 
                       :defaults (parse-namestring *load-truename*))))
    (let ((compiled-filename (compile-file-pathname the-filename)))
      (when (or 
             (not (probe-file compiled-filename)) ; no compiled file
             (> (file-write-date the-filename)
                (file-write-date compiled-filename))) ; old compiled file
        (compile-file the-filename))
      (load compiled-filename))))

(defun handle-ctrl-c (signal code scp)
  (declare (ignore signal code scp))
  (format t "got a ctrl-c, exiting~%")
  (cleanup *messages*)
  (quit-LoGS))

#+cmu
(system:enable-interrupt Unix:SIGINT #'handle-ctrl-c)