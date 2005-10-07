
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

(in-package :LoGS)

(defmacro defsyntax (name body)
  `(if (symbolp ,body)
       (setf (get ,name 'foo-fn)
             (get ,body 'foo-fn))
       (setf (get ,name 'foo-fn)
             ,body)))

(let ((ht (make-hash-table :test #'equal))
      (seen-vars ()))
  (defun set-rule-value (var value)
    (pushnew var seen-vars)
    (setf (gethash var ht) value))
  (defun get-rule-value (var)
    (gethash var ht))
  (defun reset-rule ()
    (setf seen-vars ())
    (setf ht (make-hash-table :test #'equal)))
  (defun seen-rule-vars ()
    seen-vars))

(defun parse-rule-statements (list)
  (do ((remaining list
                  (let ((what (car remaining))
                        (rest (cdr remaining)))
                    (let ((func (get what 'foo-fn)))
                      (if func
                          (funcall func rest)
                          (error "unknown keyword: ~A~%" what))))))
      ((null remaining)))
  t)

(defmacro build-rule (rule-parts)
  (let ((var (gensym))
        (parts (gensym)))
  `(let ((,parts ',(expand-rule-parts)))
     (format t "creating rule with args: ~A~%"
             ,parts)
     (eval `(make-instance 'rule ,@,parts)))))


(defmacro build-rule (rule-parts)
  `(progn 
    (format t "creating rule with args: ~A~%" ,rule-parts)
    (eval `(make-instance 'rule ,@,rule-parts))))

(defun expand-rule-parts ()
  (loop as var in (seen-rule-vars)
       nconcing (list var (get-rule-value var))))
  
(defmacro create (what &rest list)
  `(cond ((equal ',what 'rule)
          (progn
            (reset-rule)
            (parse-rule-statements ',list)
            (build-rule (expand-rule-parts))))))

(defun parse-name (list)
  (let ((name (car list))
        (rest (cdr list)))
    (cond (name
           (progn
             (set-rule-value :name name)
             (values rest name)))
          (t
           (error "no name specified~%")))))

(defsyntax 'named #'parse-name)

(defsyntax 'create
    (lambda (list)
      (let ((type (car list))
            (rest (cdr list)))
        (cond (type
               (progn
                 (set-rule-value :type type)
                 (values rest type)))
              (t
               (error "no type specified~%"))))))

(defun grab-vars (list)
  (let ((var (car list))
        (and? (equal 'and (cadr list))))
    (format t "var: ~A and? ~A~%" var and?)
    (if and?

        (multiple-value-bind (parsed-stuff rest)
            (grab-vars (cddr list))

          (values
           (cons var parsed-stuff)
           rest))
        (values
         (list var)
         (cdr list)))))

(defmacro match-regexp-binding-vars (regexp vars)
  (let ((message (gensym))
        (matches (gensym))
        (sub-matches (gensym))
        (var (gensym))
        (count (gensym)))
    `(lambda (,message)
       (multiple-value-bind (,matches ,sub-matches)
           (cl-ppcre::scan-to-strings ,regexp (message ,message))
         (when ,matches
           (values 
            t
            (let ((,count -1))
              (if (equal (length ,sub-matches) (length ,vars))
                  (mapcar
                   (lambda (,var) (list ,var (aref ,sub-matches (incf ,count))))
                   ,vars)
                  (error "different number of vars: ~A and vals ~A in ~A and ~A" (length ,sub-matches) (length ,vars) ,sub-matches ,vars)))))))))

(defmacro match-regexp-func (regexp)
  (let ((message (gensym)))
    `(lambda (,message)
       (format t "looking for regexp: ~A in message: ~A~%" 
               ,regexp (message ,message))
       (cl-ppcre::scan ,regexp (message ,message)))))

(defun parse-match-func (list)
  (let ((type (car list))
        (rest (cdr list)))
    (cond 
      ((and (equal type 'function) rest)
       (progn
         (set-rule-value :match (eval (car rest)))
         (values (cdr rest) (eval (car rest)))))
      
      ((and (equal type 'regexp) 
            (equal (cadr rest) 'binding))
       (let ((regexp (car rest)))
         (multiple-value-bind (vars left)
             (grab-vars (cddr rest))
           (format t "got vars: ~A and left: ~A~%" vars left)
           (let ((matchfunc (match-regexp-binding-vars regexp vars)))
             (set-rule-value :match matchfunc)
             (values 
              left matchfunc)))))

      ((and (equal type 'regexp) rest)
       (let* ((regexp (car rest))
              (matchfunc (match-regexp-func regexp)))
         (set-rule-value :match matchfunc)
         (values (cdr rest) matchfunc)))

      ((equal type 'regexp)
       (error "no regular expression specified~%"))
      ((equal type 'function)
       (error "no match function specified~%"))
      (t
       (error "unknown match specified: ~A ~%" type)))))
   
(defsyntax 'matching #'parse-match-func)

(defun parse-doing (list)
  (multiple-value-bind (funcs left)
      (grab-vars list)
    (set-rule-value :actions `(list ,@funcs))
    (values left funcs)))

(defsyntax 'doing #'parse-doing)