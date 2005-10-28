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

(defmacro defsyntax (name body)
  `(if (symbolp ,body)
       (setf (get ,name 'foo-fn)
             (get ,body 'foo-fn))
       (setf (get ,name 'foo-fn)
             ,body)))

(let ((ht (make-hash-table :test #'equal))
      (seen-vars ())
      (stack ()))
  (defun set-rule-value (var value)
    (pushnew var seen-vars)
    (setf (gethash var ht) value))
  (defun set-rule-attribute (var value)
    (setf (gethash var ht) value))
  (defun get-rule-value (var)
    (gethash var ht))
  (defun get-rule-attribute (var)
    (gethash var ht))
  (defun push-rule ()
    (push (list ht seen-vars) stack)
    (setf ht (make-hash-table :test #'equal)
          seen-vars ()))
  (defun pop-rule ()
    (destructuring-bind (hash seen)
        (pop stack)
      (setf ht hash
            seen-vars seen)))
  (defun reset-rule ()
    (setf seen-vars ())
    (setf ht (make-hash-table :test #'equal)))
  (defun seen-rule-vars ()
    seen-vars))

(defun parse-rule-statements (list)
  (do ((remaining list
                  (let ((what (car remaining))
                        (rest (cdr remaining)))
                    (let ((func (get what 'fo-fn)))
                      (if func
                          (funcall func rest)
                          (error "unknown keyword: ~A~%" what))))))
      ((null remaining)))
  t)

(defmacro build-rule (rule-parts)
  `(progn 
     (eval `(make-instance 'rule ,@,rule-parts))))

(defun expand-rule-parts ()
  (Loop as var in (seen-rule-vars)
       nconcing (list var (get-rule-value var))))
  
(defmacro create (what &rest list)
  (let ((where (gensym)))
    `(cond ((equal ',what 'rule)
            (progn
              (push-rule)
              (parse-rule-statements ',list)
              (let ((rule (build-rule (expand-rule-parts)))
                    (how (get-rule-value :insert-how))
                    (,where (get-rule-value :insert-where)))
                
                (cond ((and (equal how 'before)
                            ,where)
                       (progn
                         (dll-insert *ruleset* (get-rule *ruleset* `(name ,,where)) rule :direction :before)
                         )))
                (pop-rule)
                rule))))))

;; create rule ... 
;; (rule named ... )
(defun parse-rule (list)
  (values (cdr list) (car list)))

(defsyntax 'rule #'parse-rule)

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

(defmacro creating (&body body)
  `(lambda (message)
     (create ,@body)))

(defun grab-vars (list)
  (let ((var (car list))
        (and? (equal 'and (cadr list))))
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
                  (error "different number of vars: ~A and vals ~A in ~A and ~A" (length ,vars) (length ,sub-matches) ,vars ,sub-matches)))))))))

(defmacro match-regexp-func (regexp)
  (let ((message (gensym)))
    `(lambda (,message)
       (cl-ppcre::scan ,regexp (message ,message)))))


(defun parse-delete-func (list)
  (let ((type (car list))
        (rest (cdr list)))
    (cond 
      ((and (equal type 'function) rest)
       (progn
         (let ((deletefunc (eval (car rest))))
           (set-rule-value :delete-rule deletefunc)
           (values (cdr rest) deletefunc))))
      
      ((and (equal type 'regexp) 
            (equal (cadr rest) 'binding))
       (let ((regexp (eval (car rest))))
         (multiple-value-bind (vars left)
             (grab-vars (cddr rest))
           (let ((deletefunc (match-regexp-binding-vars regexp vars)))
             (set-rule-value :delete-rule deletefunc)
             (values 
              left deletefunc)))))

      ((and (equal type 'regexp) rest)
       (let* ((regexp (eval (car rest)))
              (deletefunc (match-regexp-func regexp)))
         (set-rule-value :delete-rule deletefunc)
         (values (cdr rest) deletefunc)))

      ((equal type 'regexp)
       (error "no regular expression specified~%"))
      ((equal type 'function)
       (error "no delete function specified~%"))
      (t
       (error "unknown delete specified: ~A ~%" type)))))

(defsyntax 'delete-when #'parse-delete-func)

(defun parse-no-delete-func (list)
  (let ((type (car list))
        (rest (cdr list)))
    (cond 
      ((and (equal type 'function) rest)
       (progn
         (let ((no-deletefunc (eval (car rest))))
           (set-rule-value :no-delete-rule no-deletefunc)
           (values (cdr rest) no-deletefunc))))
      
      ((and (equal type 'regexp) 
            (equal (cadr rest) 'binding))
       (let ((regexp (eval (car rest))))
         (multiple-value-bind (vars left)
             (grab-vars (cddr rest))
           (let ((no-deletefunc (match-regexp-binding-vars regexp vars)))
             (set-rule-value :no-delete-rule no-deletefunc)
             (values 
              left no-deletefunc)))))

      ((and (equal type 'regexp) rest)
       (let* ((regexp (eval (car rest)))
              (no-deletefunc (match-regexp-func regexp)))
         (set-rule-value :no-delete-rule no-deletefunc)
         (values (cdr rest) no-deletefunc)))

      ((equal type 'regexp)
       (error "no regular expression specified~%"))
      ((equal type 'function)
       (error "no no-delete function specified~%"))
      (t
       (error "unknown no-delete specified: ~A ~%" type)))))

(defsyntax 'no-delete-when #'parse-no-delete-func)

(defun parse-match-func (list)
  (let ((type (car list))
        (rest (cdr list)))
    (cond 
      ((and (equal type 'function) rest)
       (progn
         (let ((matchfunc (eval (car rest))))
           (set-rule-value :match matchfunc)
           (values (cdr rest) matchfunc))))
      
      ((and (equal type 'regexp) 
            (equal (cadr rest) 'binding))
       (let ((regexp (eval (car rest))))
         (multiple-value-bind (vars left)
             (grab-vars (cddr rest))
           (let ((matchfunc (match-regexp-binding-vars regexp vars)))
             (set-rule-value :match matchfunc)
             (values 
              left matchfunc)))))

      ((and (equal type 'regexp) rest)
       (let* ((regexp (eval (car rest)))
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

(defun parse-no-match-func (list)
  (let ((type (car list))
        (rest (cdr list)))
    (cond 
      ((and (equal type 'function) rest)
       (progn
         (let ((nomatchfunc (eval (car rest))))
           (set-rule-value :no-match nomatchfunc)
           (values (cdr rest) nomatchfunc))))
      
      ((and (equal type 'regexp) 
            (equal (cadr rest) 'binding))
       (let ((regexp (eval (car rest))))
         (multiple-value-bind (vars left)
             (grab-vars (cddr rest))
           (let ((nomatchfunc (match-regexp-binding-vars regexp vars)))
             (set-rule-value :no-match nomatchfunc)
             (values 
              left nomatchfunc)))))

      ((and (equal type 'regexp) rest)
       (let* ((regexp (eval (car rest)))
              (nomatchfunc (match-regexp-func regexp)))
         (set-rule-value :no-match nomatchfunc)
         (values (cdr rest) nomatchfunc)))

      ((equal type 'regexp)
       (error "no regular expression specified~%"))
      ((equal type 'function)
       (error "no match function specified~%"))
      (t
       (error "unknown match specified: ~A ~%" type)))))

(defsyntax 'not-matching #'parse-no-match-func)

(defun parse-doing (list)
  (multiple-value-bind (funcs left)
      (grab-vars list)
    (set-rule-value :actions `(list ,@funcs))
    (values left funcs)))

(defsyntax 'doing #'parse-doing)

(defun parse-continuing (list)
  (set-rule-value :continuep t)
  (values list t))

(defsyntax 'continuing #'parse-continuing)

;; before *current-rule*
(defun parse-insert (list)
  (destructuring-bind 
        (how where &rest rest)
      list
    (cond ((equal 'before how)
           (progn
             (set-rule-attribute :insert-how 'before)
             (set-rule-attribute :insert-where where)
             rest))
          (t
           (error "bad insert: ~A~%" how)))))

(defsyntax 'inserted #'parse-insert)

(defun parse-environment (list)
  (let ((environment (car list))
        (rest (cdr list)))
    (cond (environment
           (progn
             (set-rule-value :environment environment)
             (values rest environment)))
          (t
           (error "no environment specified~%")))))

(defsyntax 'with-environment #'parse-environment)
