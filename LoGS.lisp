; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2004 James Earl Prewett

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

;(proclaim '(optimize (speed 3)))

; proper optimizations?
;(declaim  (OPTIMIZE (SPEED 3) (size 0) (SAFETY 0) (compile-speed 0)))
(declaim  (OPTIMIZE (SPEED 3) (debug 0) (SAFETY 0)))
;(declaim (optimize (speed 0) (debug 3) (safety 2)))
; set this to something *BIG* 

(declaim (EXTENSIONS:FREEZE-TYPE file-follower priority-queue context collection doubly-linked-list doubly-linked-list-item priority-queue-item timeout-object rule ruleset killable-item message string-message))


#+cmu
(LISP::%SET-BYTES-CONSED-BETWEEN-GCS 114500000)

; turn off gc messages
#+cmu
(setq ext:*gc-verbose* ())


;; define the LoGS package
(defpackage :LoGS
(:use :cl
	#+allegro :clos
	#+cmu :pcl
	#+lispworks :hcl
        :cl-user)
#+cmu
(:import-from :extensions #:quit)
#+cmu
  (:shadowing-import-from :pcl #:standard-class #:built-in-class
                          #:find-class #:class-name #:class-of))

(in-package :LoGS)

(defvar *debug* ())
(defvar *use-internal-real-time* t "should LoGS use the intenal-real-time?")

(defvar *now* (get-internal-real-time)
  "the current time.  Currently an integer like (get-internal-real-time).
  This may change, do not rely on this.")

;; this cruft is to make file loading more portable
(defparameter *LoGS-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*))
  "Where LoGS lives.")

(defparameter *LoGS-sleep-time* .03
  "How long to sleep when there is no input")

(defun load-LoGS-file (filename)
  (load 
   (compile-file
    (make-pathname :name filename
                   :type "lisp"
                   :version ()
                   :defaults *LoGS-base-directory*))))

;(defun load-LoGS-file (filename)
;  (load filename))

;; load fundamental data structures
(load-LoGS-file "data_structures/doubly-linked-list")
(load-LoGS-file "data_structures/priority-queue")

;; load LoGS data structures
(load-LoGS-file "message")
(load-LoGS-file "named-object")
(load-LoGS-file "timeout-object")
(load-LoGS-file "killable-item")
(load-LoGS-file "collection")
(load-LoGS-file "context")

;; load message producers
(load-LoGS-file "Data_Sources/File-Follower")

;; load rules
(load-LoGS-file "rule")
(load-LoGS-file "ruleset")
(load-LoGS-file "actions")

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

(defvar *die-die-die* ()
  "a list of functions to call when LoGS is done running (before exiting).")

;; (defvar *rule-timeout-queue* 
;;   (make-instance 'priority-queue 
;;                  :comparison-function 
;;                  (lambda (x y)
;;                    (let ((t-x (timeout x))
;;                          (t-y (timeout y)))
;;                      (declare (fixnum t-x t-y))
;;                      (> t-x t-y))))
;;   "A priority queue to hold rules that can time out.")

(defgeneric get-logline (message-source)
  (:documentation
   "Get-logline returns the next message from the message source or 
nil if there is no such next message."))

(defgeneric check-rules (message ruleset)
  (:documentation
   "Check-rules checks the given message against 
the given ruleset until it finds a rule that 
both matches and continuep is nil."))

(defmethod check-rules ((message message) (ruleset doubly-linked-list))
  (let ((head (head ruleset)))
    (when
        *debug*
      (format t "checking rules: ~A ~A~%" (name ruleset) (message message)))
    (loop with *current-rule* = head
          and found = ()
          when (not *current-rule*)
          do
          (return found)
          when *current-rule*
          do 
          (let ((data (data *current-rule*)))
            (if (dead-p data)
                (dll-delete ruleset *current-rule*)
                (and 
                 (setq found (check-rule data message))
                 (unless (continuep data)
                   (return t)))))
          (let ((rlink (rlink *current-rule*)))
            (if (eq rlink head)
                (return found)
                (setq *current-rule* rlink))))))

(defgeneric check-limits (thing))

(defmethod check-limits ((rule rule))
  (and (rule-exceeded-limit-p rule *now*)
       (setf (dead-p rule) t)))

(defmethod check-limits ((context context))
  (let ((ret (context-exceeded-limit-p context *now*)))
    (when ret
      (run-context-actions context)
      (dll-delete *contexts* context))
    ret))

;; is this good?
(defmethod check-limits ((timeout-object timeout-object))
  (exceeded-timeout-p timeout-object *now*))

(defmethod check-limits ((pq priority-queue))
  (loop as dlli = (head pq)
        when *debug*
        do (format t "checking pq: ~A dlli: ~A~%" pq dlli)
        when (not dlli)
        do
        (return)
        when (not (check-limits (data dlli)))
        do 
        (return)
        when t
        do
        (dll-delete pq dlli)
        (setf dlli (rlink dlli))))

(defgeneric run-context-actions (context)
  (:documentation
   "Run the actions associated with a context."))

(defmethod run-context-actions ((context context))
  "Run the actions associated with a context."
  (when (actions context)
    (mapcar 
     (lambda (x) 
       (declare (function x))
       (funcall x context))
     (actions context))))

#+cmu
(ext:defswitch "-no-internal-time" #'(lambda (switch) 
                                       (setq
                                        *use-internal-real-time* ())))

#+cmu
(ext:defswitch "-file" #'(lambda (switch)
                           (let ((filename (car (ext:cmd-switch-words switch)))
                                 (position (cadr (ext:cmd-switch-words switch))))
                             (setq *messages* 
                                   (make-instance 'File-Follower 
                                                  :FileName 
                                                  filename))
                             ;; do some file position logic here
                             (when position
                               (set-file-follower-position
                                *messages*
                                position)))))
#+cmu
(ext:defswitch "-ruleset" 
    #'(lambda (switch)
        (let ((filename (car (ext:cmd-switch-words switch))))
          (load (compile-file filename)))))

#+cmu
(ext:defswitch "-run-forever"
    #'(lambda (switch)
        (declare (ignore switch))
        (setq *run-forever* t)))

(defun main ()
  "Main is the current LoGS mainline.  As of this revision, it tries
to mimic Logsurfer's behaviour (modulo context handling).  Main iterates over 
incoming messages, checking each against the current ruleset. 

Main currently does:
1. Processes an incoming line if there is one.
2. Check to see if any rules need to be removed.
3. Check to see if any contexts need to be removed."
  (loop as *message* = (get-logline *messages*)
        when (and (not *run-forever*) (not *message*))
        do
        (return)

        ;; don't burn the whole CPU when theres nothing to do
        when (and *run-forever* (not *message*))
        do
        (sleep *LoGS-sleep-time*)

        when *use-internal-real-time*
        do
        (setq *now* (get-internal-real-time))

        when t
        do
        (when (and *message* (head *root-ruleset*))
          (check-rules *message* *root-ruleset*))
        
        (when (head *timeout-object-timeout-queue*)
          (check-limits *timeout-object-timeout-queue*))))


