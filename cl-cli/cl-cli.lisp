; CL-CLI Command line processing utility
; Copyright (C) 2003-2007 James Earl Prewett

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

(defpackage :org.prewett.cl-cli
  (:nicknames :cl-cli)
  (:use :cl
	#+allegro :clos
	#+cmu :pcl
        #+sbcl :sb-mop
	#+lispworks :hcl
        :cl-user
        )
  (:export process-options cli-opt get-application-args))

(in-package :org.prewett.cl-cli)

;; current version of cl-cli
; this is now inside of the eval-when so that SBCL won't puke
(eval-when (:compile-toplevel)
  (defconstant +cl-cli-version+ "0.0.3-pre"))


#+cmu
(setf EXTENSIONS::*COMPLAIN-ABOUT-ILLEGAL-SWITCHES* ())

(defclass cli-opt ()
  ((name :initform () :initarg :name :accessor name)
   (arguments :initform () :initarg :arguments :accessor arguments)
   (action :initform () :initarg :action :accessor action)
   (description :initform () :initarg :description :accessor description)))

;; process all options given on the command line
(defun process-options (options-list args)
  (let ((default-options ()))
    (flet ((nextarg ()
             (loop with seenflag = 'nil
                as arg in args
                if (not seenflag)
                do (setq seenflag t)
                else if (optionp arg)
                do (loop-finish)
                collect (pop args))))
      (loop as cdr on args until (optionp (car cdr)) 
         do
           (push (car cdr) default-options)
         finally (setq args cdr))
      (loop as nextopt = (nextarg)
         while nextopt
         do 
           (process-switch nextopt options-list))
      (when
          (member :default
                  options-list
                  :test (lambda (x y) (equal x (name y))))
        (process-switch (cons :default (reverse default-options))
                      options-list))
      args)))

(defun optionp (string)
 "Is STRING a command line option?"
 (or (starts-with-p string "--") (starts-with-p string "-")))

(defun starts-with-p (string1 string2 &key (test #'string=))
 "Does STRING1 start with STRING2?"
 (and (>= (length string1) (length string2))
      (funcall test string1 string2 :end1 (length string2))))

(defmethod same-name-p ((string string) name)
 "Is NAME the same as STRING in terms of command line options?"
 (or 
  (equal name (subseq string 1)) 
  (equal name (subseq string 2))))

(defmethod same-name-p ((sym symbol) name)
  (equal name sym))

;; process an individual switch
(defun process-switch (list options-list)
  (let* ((switch (pop list))
        (opt (car (member switch options-list
                          :test #'(lambda (a b) 
                                    (same-name-p a (name b)))))))
   (when opt
     (let ((numargs (length list)))
       (multiple-value-bind (minargs maxargs)
           (option-lengths opt)
         (cond
           ((< numargs minargs)
            (error "too few arguments for flag: ~A expecting ~A to ~A, given ~A~%"
                   (name opt) minargs maxargs numargs))
           ((and (numberp maxargs) (> numargs maxargs))
            (error "too many arguments for flag: ~A expecting ~A to ~A, given ~A~%"
                   (name opt) minargs maxargs numargs))
           ((action opt)
            (apply (action opt) list))))))))

;; pull the next flag & its args off the args list
(defun option-lengths (option)
  (let ((min-args 0)                  ; minimum arguments to this flag
        (max-args 0)                  ; maximum arguments to this flag
        (seen-optional-arg nil)  ; has an optional argument been seen?
        (multiargs nil) ; are an infinite number of arguments possible?
        )
    (flet ((specialp (arg)
             (and (symbolp arg) (char= (elt (symbol-name arg) 0) #\&)))
           (special (arg)
             (let ((arg (intern (symbol-name arg))))
               (ecase arg
                 (&optional (setq seen-optional-arg t))
                 (&rest (setq multiargs t))
                 ((&body &key)
                  (error "Cannot support ~a for command line." arg))))))
      (loop as arg in (arguments option)
            if (specialp arg)
            do (special arg)
            else
            do (incf max-args)
            and unless (or seen-optional-arg multiargs)
            do (incf min-args)
            finally (return (if multiargs
                                (values min-args ())
                                (values min-args max-args)))))))

;; modified from James F. Amundson's code
;; CMU is working; I haven't checked others...
(defun get-application-args ()
    #+clisp (rest ext:*args*)

    #+sbcl (rest sb-ext:*posix-argv*)

    #+gcl  (let ((result  si::*command-args*))
	    (do ((removed-arg nil (pop result)))
		 ((or (equal removed-arg "--") (equal nil result)) result)))

    #+allegro
    (let ((args (system:command-line-arguments :application t)))
      ;; Skip the first arg, which is the full path to alisp.
      (rest args))

    #+cmu (cdr lisp::lisp-command-line-list)

    #+lispworks
    (cdr SYSTEM:*LINE-ARGUMENTS-LIST*)

    ;; FIXME: openmcl version missing
    #-(or lispworks cmu allegro gcl sbcl clisp)
       (error "unimplemented"))

;; display option help
(defun help (opts)
  (mapcar
   (lambda (option)
     (format t "~A~10T~{~T~A~}~25T~A~%"
             (name option)
             (or (arguments option) '(""))
             (or (description option) "")))
   opts))
