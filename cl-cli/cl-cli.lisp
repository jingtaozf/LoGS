; CL-CLI Command line processing utility
; Copyright (C) 2003-2006 James Earl Prewett

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
  (defconstant +cl-cli-version+ "0.0.2-pre"))
    

#+cmu
(setf EXTENSIONS::*COMPLAIN-ABOUT-ILLEGAL-SWITCHES* ())

(defclass cli-opt ()
  ((name :initform () :initarg :name :accessor name)
   (arguments :initform () :initarg :arguments :accessor arguments)
   (action :initform () :initarg :action :accessor action)
   (description :initform () :initarg :description :accessor description)))


;; process all options given on the command line
(defun process-options (options-list args)
  (macrolet ((nextarg (args)
               (let ((seenflag (gensym))
                     (arglist (gensym)))
                 `(let ((,seenflag ()) 
                        (,arglist ())) 
                   (dolist (x ,args ,arglist) 
                     (cond 
                       ((and (not ,seenflag) (equal "-" (subseq x 0 1)))
                        (progn
                          (setq ,arglist (append ,arglist (list (pop ,args))))
                          (setq ,seenflag t)))
                       ((equal "-" (subseq x 0 1)) (return ,arglist)) 
                       (t (setq ,arglist (append ,arglist (list (pop ,args)))))))))))
    
    (loop as nextopt = (nextarg args)
          when (not nextopt)
          do (return)
          
          else
          do 
          (process-switch nextopt options-list))))

;; process an individual switch
(defun process-switch (list options-list)
  (let* ((switch (pop list))
         (opt (car (member switch options-list 
                           :test #'(lambda (a b) (equal a (name b)))))))
    (when opt
      (let ((numargs (length list)))
        (multiple-value-bind (minargs maxargs)
            (option-lengths opt)
          
          (cond 
            ((< numargs minargs)
             (error "too few arguments for flag: ~A expecting ~A to ~A, given ~A~%"
                    (name opt) minargs maxargs numargs))
            ((not maxargs) t)
            ((> numargs maxargs)
             (error "too many arguments for flag: ~A expecting ~A to ~A, given ~A~%"
                    (name opt) minargs maxargs numargs))
            
            ((action opt)
             (apply (action opt) list))))))))
        
;; pull the next flag & its args off the args list
(defun option-lengths (option)
  (let ((min-args 0) ; minimum arguments to this flag
        (max-args 0) ; maximum arguments to this flag
        (seen-optional-arg nil) ; has an optional argument been seen?
        (multiargs nil) ; are an infinite number of arguments possible?
        )
    (loop as x in (arguments option)
          ;; handle a mandatory argument
          when (equal "<" (subseq x 0 1))
          do
          (when seen-optional-arg 
            (error "mandatory argument listed after optional argument!~%"))
          (incf min-args)
          (incf max-args)
          
          ;; handle rest arguments
          when (equal "..." x)
          do
          (setf multiargs t)
          (setf seen-optional-arg t)
          
          ;; handle optional arguments
          when (equal "[" (subseq x 0 1))
          do
          (incf max-args)
          (setf seen-optional-arg t))
    ;; return how many min/max args we take
    (if multiargs
        (values min-args ()) 
        (values min-args max-args))))

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
