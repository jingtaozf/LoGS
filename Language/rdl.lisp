;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2006 Vijay Lakshminarayanan

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.

;;;; This file is a part of LoGS.  The copyright will soon be
;;;; transferred to the author of LoGS, James Earl Prewett

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defpackage :org.prewett.LoGS.language
  (:use :cl)
  (:import-from :logs :rule :message))

(in-package #:org.prewett.LoGS.language)

;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

;;; an 'accessor' for the handle-fns
(defmacro handle-fn (keyword)
  `(get ,keyword 'handle-fn))

;;; To use synonyms easily
(defmacro alias (keyword)
  `(get ,keyword 'alias))

;;; Influenced by Peter Norvig's LOOP implementation
(defstruct rule-macro
  (name '())
  (match '())
  (actions '())
  (environment '())
  (timeout '())
  (relative-timeout '())
  (filter '())
  (continuep '()))

(defmacro rule (&rest exprs)
  (let ((r (make-rule-macro)))
    (parse-rule r exprs)
    (fill-rule-template r)))

(defun standardize (X)
  "This is how we look at all lisp forms that are part of the RDL."
  (intern (format () "~a" X) '#:keyword))

(defun samep (x symbol)
  "Is X the same as SYMBOL or one of its aliases?"
  ;; To be package independent we use the KEYWORD package
  (let ((x (standardize x))
        (symbol (standardize symbol)))
    (or (eq x symbol) (eq (alias x) symbol))))

(defgeneric get-rule-slot (rule slot)
  (:documentation "Returns the requested slot for RULE depending
on the required behaviour for SLOT."))

(defun fill-rule-template (rule)
  `(make-instance
    'rule
    ,@(loop as slot in '(:match :name :actions :environment :timeout
                         :relative-timeout :filter :continuep)
            as res = (get-rule-slot rule slot)
            if res append `(,slot ,res))))

(defun parse-rule (rule exprs)
  (unless (null exprs)
    (parse-rule rule (parse-keyword rule (car exprs) (cdr exprs)))))

(defun parse-keyword (rule keyword exprs)
  (let* ((keyword (standardize keyword))
         (keyword (or (alias keyword) keyword)))
    (aif (handle-fn keyword)
         (funcall it rule exprs)
         (error "Unknown keyword ~S" keyword))))

(defun handle-name (rule exprs)
  (destructuring-bind (name . rest) exprs
    (push name (rule-macro-name rule))
    rest))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :name)))
  (declare (ignore slot))
  (aif (rule-macro-name rule)
       (if (cdr it)
           (error "Each rule can have only one name.")
           (car it))))

;;; There is a macro hiding in all the HANDLE- functions!

(defun handle-match (rule exprs)
  ;; Syntax:
  ;; matching regexp "abc" and regexp "def" == matching regexp "abcdef"
  ;; matching "string literal" == matching regexp "string literal"
  ;; matching regexp "foo+" and #'function == ERROR
  (destructuring-bind (car . cdr) exprs
    (cond ((samep car :regexp)
           (destructuring-bind (string . cdr) cdr
             (push string (rule-macro-match rule))
             (if (samep (car cdr) :and)
                 (handle-match rule (cdr cdr))
                 cdr)))
          ((stringp car)
           ;; If just a string is specified, assume it is a regexp
           (handle-match rule `(:regexp ,car ,@cdr)))
          (t
           (aif (rule-macro-match rule)
                (error "Cannot have multiple matching functions.  ~S ~
                         already specified." it))
           ;; Assume it is a predefined function
           (setf (rule-macro-match rule) car)
           cdr))))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :match)))
  "Return the form for MATCH tag"
  (declare (ignore slot))
  (let ((match (rule-macro-match rule)))
    ;; if there is no match, we need not generate a match function
    ;; I believe the match function that matched no regexps was technically
    ;; correct, but this is slightly better in terms of speed and IMO clarity
    (if (and match (every #'stringp match))
        (let ((msg (gensym "MESSAGE"))
              (matches (gensym "MATCHES"))
              (sub-matches (gensym "SUB-MATCHES")))
          `(lambda (,msg)
            (multiple-value-bind (,matches ,sub-matches)
                (and ,@(loop as r in (nreverse match)
                             collecting `(cl-ppcre:scan-to-strings
                                          ,r
                                          (message ,msg))))
              (when ,matches (values t (list (list ',(intern "SUB-MATCHES")
                                                   ,sub-matches)))))))
        match)))

(defun handle-actions (rule exprs)
  ;; Valid syntactic choices:
  ;; doing foo => foo
  ;; doing foo on bar => (LAMBDA (#:GENSYM123) (DECLARE ...) (FOO BAR))
  ;; do foo with (bar baz quux) => (LAMBDA (BAR BAZ QUUX) FOO)
  ;; do foo with (a) on x => (LAMBDA (A) (FOO X))
  ;; do foo with (a) on (x y z) => (LAMBDA (A) (FOO X Y Z))
  ;; the next one follows from the previous, but just to make things clear
  ;; do foo with (a) on ((x) 'y (list a)) => (LAMBDA (A) (FOO (X) 'Y (LIST A)))
  (destructuring-bind (fn . rest) exprs
    (let ((next (car rest))
          body arg-list
          (sym (gensym)))
      (if (or (samep next :on) (samep next :with))
          (flet ((set-body ()
                   (when (samep next :on)
                     (destructuring-bind (on b &rest cdr) rest
                       (when (samep on :on)
                         (setq body (if (consp b) b (list b))
                               rest cdr)))))
                 (set-arg-list ()
                   (when (samep next :with)
                     (destructuring-bind (with a &rest cdr) rest
                       (when (samep with :with)
                         (unless (and (consp a) (every #'symbolp a))
                           (error "Cannot have ~S as a lambda-list" a))
                         (setq arg-list a
                               rest cdr))))))
            (set-body)
            (set-arg-list)
            (setq next (car rest))
            (set-body)
            (set-arg-list)
            (setq body (if (null body) `(,fn) `((,fn ,@body)))
                  arg-list (if (null arg-list) `(,sym) arg-list))
            (push `(lambda ,arg-list (declare (ignorable ,@arg-list)) ,@body)
                  (rule-macro-actions rule)))
          (push fn (rule-macro-actions rule)))
      rest)))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :actions)))
  (declare (ignore slot))
  (nreverse (rule-macro-actions rule)))

(defun handle-timeout (rule exprs)
  (destructuring-bind (preposition seconds &rest rest) exprs
    (cond ((samep preposition :in)
           (aif (rule-macro-relative-timeout rule)
                (error "Relative timeout was already specified as ~S" it))
           (setf (rule-macro-relative-timeout rule) seconds)
           rest)
          ((samep preposition :at)
           (aif (rule-macro-timeout rule)
                (error "Timeout was already specified as ~S" it))
           (setf (rule-macro-timeout rule) seconds)
           rest)
          (t (error "Unexpected keyword ~S" preposition)))))

(defmethod get-rule-slot ((r rule-macro) (slot (eql :timeout)))
  "By default just return the slot value"
  (rule-macro-timeout r))

(defmethod get-rule-slot ((r rule-macro) (slot (eql :relative-timeout)))
  "By default just return the slot value"
  (rule-macro-relative-timeout r))

(defun handle-setenv (rule exprs)
  (labels ((evalable (X) (if (consp X)
                             (and (fboundp (car X)) (every #'evalable (cdr X)))
                             (constantp X))))
    (destructuring-bind (var = val &rest rest) exprs
      (declare (ignore =))
      (unless (symbolp var) (error "Cannot assign value to ~S" var))
      (let ((val (if (evalable val) (eval val) val)))
        (pushnew `(,var ,val) (rule-macro-environment rule)
                 :test #'eql :key #'car))
      (if (samep (car rest) :and)
          (handle-setenv rule (cdr rest))
          rest))))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :environment)))
  (declare (ignore slot))
  `',(nreverse (rule-macro-environment rule)))

(defun handle-continuep (rule exprs)
  (setf (rule-macro-continuep rule) t)
  exprs)

(defmethod get-rule-slot ((r rule-macro) (slot (eql :continuep)))
  "By default just return the slot value"
  (rule-macro-continuep r))

(defun handle-filter (rule exprs)
  (setf (rule-macro-filter rule) t
        (rule-macro-continuep rule) t)
  (handle-match rule exprs))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :filter)))
  (declare (ignore slot))
  (when (and (rule-macro-filter rule) (rule-macro-actions rule))
    (error "Cannot specify actions when filtering rule.")))

(setf (handle-fn :match)        #'handle-match)
(setf (handle-fn :name)         #'handle-name)
(setf (handle-fn :actions)      #'handle-actions)
(setf (handle-fn :timeout)      #'handle-timeout)
(setf (handle-fn :filter)       #'handle-filter)
(setf (handle-fn :setenv)       #'handle-setenv)

;;; Aliases
(defmacro set-aliases (symbol (&rest aliases))
  `(let ,@(loop as alias in aliases
                as alias-sym = (gensym)
                with symbol-sym = (gensym)
                with symbol-let = `(,symbol-sym (standardize ',symbol))
                collecting `(,alias-sym (standardize ',alias)) into lets
                appending `((alias ,alias-sym) ,symbol-sym) into setfs
                finally (return `((,symbol-let ,@lets) (setf ,@setfs))))))

(set-aliases match (matching matches))
(set-aliases filter (filtering))
(set-aliases actions (doing do))
(set-aliases setenv (set with))
(set-aliases name (named))
(set-aliases timeout (timing-out))
