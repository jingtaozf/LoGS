;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2005 James Earl Prewett

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

(in-package :LoGS)

(defun parse-continuing (list)
  (values
   t
   list))

(defun parse-rule-stuff (list)
  (apply #'make-instance 'rule
         (loop while list 
            nconcing
              (destructuring-bind (head &rest tail)
                  list
                (let ((parse-func (get-parse-func head)))
                  (if parse-func
                      (multiple-value-bind (result left)
                          (funcall parse-func tail)
                        (setf list left)
                        (list (car (get-affected-slots head)) result))
                      (error "unknown sub-statement ~A~%" head)))))))

;; bogus utility funcs
(defun true1 (message)
  (declare (ignore message))
  (format t "true1~%")
  t)

(defun true2 (message)
  (declare (ignore message))
  (format t "true2~%")
  t)

(defun false1 (message)
  (declare (ignore message))
  (format t "false1~%")
  ())

(defun false2 (message)
  (declare (ignore message))
  (format t "false2~%")
  ())

;; given a list of functions, produce a function
;; that returns the OR of calling the functions
;; with the given message
(defun or-funcs (list)
  "given a list of functions, produce a function that returns the logical OR of calling the functions with the given message"
  (lambda (message)
    (loop as function in list
         
       when (funcall function message)
       do
         (return t)
         
       finally 
         (return ()))))
  
(defun and-funcs (list)
  "given a list of functions, produce a function that returns the logical AND of calling the functions with the given message"
  (lambda (message)
    (loop as function in list
         
       when (not (funcall function message))
       do
         (return ())
         
       finally 
         (return t))))


(defun parse-and (list)
  (let ((remaining list)
        (match-funcs ()))
    (loop 
       while remaining
       do
         (multiple-value-bind (match-func left)
             (parse-match remaining)
           (setf match-funcs (append match-funcs (list match-func))
                 remaining left)))
    (values
     (and-funcs match-funcs)
     remaining)))

(defun parse-or (list)
  (let ((remaining list)
        (match-funcs ()))
    (loop 
       while remaining
       do
         (multiple-value-bind (match-func left)
             (parse-match remaining)
           (setf match-funcs (append match-funcs (list match-func))
                 remaining left)))
    (values
     (or-funcs match-funcs)
     remaining)))

(defun parse-not (list)
  (multiple-value-bind (un-match-func left)
      (parse-match list)
    (values
     (lambda (message)
       (not (funcall un-match-func message)))
     left)))

(defun parse-name (list)
  (destructuring-bind (head &rest tail)
      list
    (if head 
        (values
         head tail)
        (error "names cannot be NULL~%"))))

(defun parse-function (list)
  (values
   (car list)
   (cdr list)))

(defun parse-script (list)
  (values
   (lambda (message)
     (equal 0
            (if (listp (car list))
                (funcall (exec-returning-value (caar list) (cadr list)) message)
                (funcall (exec-returning-value (car list)) message))))
   (cdr list)))

(defun parse-regexp (list)
  (let ((regexp (car list))
        (left (cdr list))
        regexp-match-func)

    (if (listp regexp)
        ;; we need to bind
        (destructuring-bind (actual-regexp &rest bindings)
            regexp
          (setf
           regexp-match-func
           (match-regexp-binding-list actual-regexp bindings)))
        (setf
         regexp-match-func
         (match-regexp regexp)))
    (values
     regexp-match-func
     left)))
     
(defun parse-match-list (list)
  (let ((logic-type (car list)))
    (cond ((eql logic-type 'and)
           (parse-and (cdr list)))
          ((eql logic-type 'or)
           (parse-or (cdr list)))
          ((eql logic-type 'not)
           (parse-not (cdr list))))))

(defun parse-match (list)
  (let ((match-type (car list)))
    (cond ((eql match-type 'function)
           (parse-function (cdr list)))
          ((eql match-type 'regexp)
           (parse-regexp (cdr list)))
          ((eql match-type 'script)
           (parse-script (cdr list)))
          ((listp match-type)
           (let ((match-func (parse-match-list match-type)))
             (values
              match-func
              (cdr list))))
          (t
           (error "unknown match type: ~A~%" match-type)))))


;;; set the keyword in a table
;;; to a function, taking args evaluating body

(defparameter *LoGS-statements* (make-hash-table))
(defparameter *LoGS-statement-slot* (make-hash-table))

(defmacro get-statement-func (statement)
  `(gethash ,statement ,*LoGS-statements*))

(defmacro get-statement-slot (statement)
  `(gethash ,statement ,*LoGS-statement-slot*))

(defmacro defstatement (key args slot &body body)
  `(progn
     (setf
      (get-statement-slot ,key)
      ,slot)
     (setf
      (get-statement-func ,key)
      (lambda (,@args)
        ,@body))))

(defun parse-statement (name &rest rest)
  (let ((func (get-statement-func name)))
    (apply func rest)))

;; (defstatement 'named (name &rest rest) :name
;;   (values
;;    name
;;    rest))

;; (defstatement 'matching (match-type &rest rest) :match
;;   (if (listp match-type)
;;       (values
;;        (parse-match-list match-type)
;;        rest)
;;       (cond ((eql 'function match-type)
;;              (values 
;;               (car rest)
;;               (cdr rest)))
;;             (t
;;             (error "unimplemented match type: ~A~%" match-type)))))


;; (defstatement 'rule (&rest rest) ()
;;   (apply #'make-instance
;;          (nconc 
;;           '(rule)
;;           (loop while rest 
;;              nconcing
;;                (multiple-value-bind (statement-val remaining)
;;                    (apply #'parse-statement rest)
;;                  (let ((thing (car rest)))
;;                    (setf rest remaining)
;;                    (list (get-statement-slot thing)
;;                          statement-val)))))))