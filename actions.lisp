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

(in-package :org.prewett.LoGS)

(defvar *mail-program* "/usr/bin/mail")

(defmacro exec (program &rest args)
  `(lambda (messages) 
    (declare (ignore messages))
    #+cmu
    (extensions:run-program ,program (quote ,args) :wait () :output t)
    #+sbcl
    (run-program ,program (quote ,args) :wait () :output t :search t)
    #+openmcl
    (run-program ,program (quote ,args) :wait () :output t)
    #+allegro
    (excl:run-shell-command (format () "~A ~{ ~A~}" ,program ,args)
                            :output *standard-output*)
    ))

(defun do-exec (program args)
  (lambda (messages) 
    (declare (ignore messages))
    #+cmu
    (extensions:run-program program 
                            (mapcar
                             (lambda (arg)
                               (cond ((symbolp arg)
                                      (symbol-value arg))
                                     (t arg)))
                             args)
                            :wait () :output t)
    #+sbcl
    (run-program program args :wait () :output t :search t)
    #+openmcl
    (run-program program args :wait () :output t)
    #+allegro
    (excl:run-shell-command (format () "~A ~{ ~A~}" program args)
                            :output *standard-output*)
    ))

(defmacro exec (program args)
  `(do-exec ,program ,args))


;; like exec, but wait and return the return code from the program
;; for when the exec is useful /for/ the return value
(defmacro exec-returning-value (program &rest args)
  `(lambda (messages) 
    (declare (ignore messages))
    #+cmu
    (extensions:process-exit-code (extensions:run-program ,program ',args :wait t :output t))
    #+sbcl
    (SB-EXT:process-exit-code (SB-EXT:run-program ,program ',args :search t :wait t :output t))
    #+allegro
    (excl:run-shell-command (format () "~A ~{ ~A~}" ,program ',args)
                            :output *standard-output*)
    #-(or sbcl cmu allegro)
    (error "unimplemented~%")
    ))

;; these allow us to abstract writing things to files.
;; this should simplify our rulesets considerably
(defgeneric write-to-file (filename message))

(defmethod write-to-file (filename (message message))
  (with-open-file
      (file filename
            :direction :output
            :if-exists :append
            :if-does-not-exist :create)
    (format file "~A~%" (message message))))

(defmethod write-to-file (filename (context context))
  (with-open-file
      (file filename
            :direction :output
            :if-exists :append
            :if-does-not-exist :create)
    (write-context context file)))

(defmethod write-to-file (filename (string string))
  (with-open-file
      (file filename
            :direction :output
            :if-exists :append
            :if-does-not-exist :create)
    (format file "~A~%" string)))

;; make a function to write the message to a file
(defmacro file-write (filename)
  `(lambda (thing)
     (write-to-file ,filename thing)))


(defmethod pipe ((message message) (program string) &rest args)
  #+cmu
  (extensions:run-program 
   program args 
   :wait t 
   :input (make-string-input-stream (message message)))
  #+sbcl
  (run-program 
   program args 
   :wait t 
   :input (make-string-input-stream (message message)))
  #+allegro
  (mapcar
   (lambda (output-line)
     (format t "~&~A~%" output-line))
   (excl.osi:command-output (format () "~A ~{ ~A~}" program args)
                           :input (message message))))

(defmethod pipe ((string string) (program string) &rest args)
  #+cmu
  (extensions:run-program 
   program args 
   :wait t 
   :input (make-string-input-stream string))
  #+sbcl
  (run-program 
   program args 
   :wait t 
   :input (make-string-input-stream string))
  #+allegro
    (excl.osi:command-output (format () "~A ~{ ~A~}" program args)
                             :input string))

(defmethod pipe ((context context) (program string) &rest args)
  (let ((output-stream (make-string-output-stream)))

    (format output-stream "Context: ~A~%" (name context))
    (write-context context output-stream)

    #+cmu
    (extensions:run-program
     program args
     :wait t
     :input (make-string-input-stream (get-output-stream-string output-stream)))
    #+sbcl
    (run-program
     program args
     :wait t
     :input (make-string-input-stream (get-output-stream-string output-stream)))
    #+allegro
    (excl.osi:command-output (format () "~A ~{ ~A~}" program args)
                             :input (get-output-stream-string output-stream))
    ))

(defmethod mail ((context context) (recipient string) &optional subject)
  (if subject
      (pipe context *mail-program* "-s" subject recipient)
      (pipe context *mail-program* recipient)))

(defmethod mail ((string string) (recipient string) &optional subject)
  (if subject
      (pipe string *mail-program* "-s" subject recipient)
      (pipe string *mail-program* recipient)))

(defmethod mail ((message message) (recipient string) &optional subject)
  (if subject
      (pipe message *mail-program* "-s" subject recipient)
      (pipe message *mail-program* recipient)))

