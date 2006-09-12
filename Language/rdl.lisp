;;; Logs extensible (common-lisp based) log/event analysis engine/language
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
  (:import-from :logs :rule :ruleset :message :exec)
  (:nicknames :language))

(in-package #:language)

;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body then-forms)
  `(let ((it ,test-form))
    (if it (progn ,@then-forms))))

;;; an 'accessor' for the handle-fns
(defgeneric handle-fn (keyword type)
  (:documentation "Returns the requested handler function depending on the 
TYPE of the object passed in"))

;;; To use synonyms easily
(defmacro alias (keyword)
  `(get ,keyword 'alias))

;;; Influenced by Peter Norvig's LOOP implementation

;;; changed to CLOS objects instead of structures
(defclass rule-macro ()
  ((name :initform '() :accessor MACRO-NAME)
   (match :initform '() :accessor MACRO-MATCH)
   (bind :initform '() :accessor MACRO-BIND)
   (actions :initform '() :accessor MACRO-ACTIONS)
   (environment :initform '() :accessor MACRO-ENVIRONMENT)
   (timeout :initform '() :accessor MACRO-TIMEOUT)
   (relative-timeout :initform '() :accessor MACRO-RELATIVE-TIMEOUT)
   (filter :initform '() :accessor MACRO-FILTER)
   (continuep :initform '() :accessor MACRO-CONTINUEP)
   (storing :initform () :accessor MACRO-STORING)
   (exec :initform () :accessor MACRO-EXEC)))

(defmacro rule (&rest exprs)
  (let ((r (make-instance 'rule-macro)))
    (parse-rule r exprs)
    (fill-rule-template r)))

(defclass ruleset-macro (rule-macro)
  ((elements :initform () :accessor MACRO-ELEMENTS :initarg :MACRO-ELEMENTS)))

(defmacro ruleset (&rest exprs)
  (let ((r (make-instance 'ruleset-macro)))
    (parse-rule r exprs)
    (fill-rule-template r)))

(defun standardize (X)
  "This is how we look at all lisp forms that are part of the RDL:
convert them into interned symbols in the keyword package so that :FOO
and 'FOO and \"foo\" are seen as equivalent."
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

(defmethod get-rule-slot (rule slot)
  (error "unknown slot: ~A in ~A" slot rule))

(defgeneric fill-rule-template (rule))

(defmethod fill-rule-template ((rule rule-macro))
  `(let ((rule-instance 
          (make-instance
           'rule
           ,@(loop as slot in '(:match :name :actions :environment :timeout
                                :relative-timeout :filter :continuep)
                   as res = (get-rule-slot rule slot)
                   if res append `(,slot ,res)))))
    (progn
      (setf (logs::actions rule-instance)
            (append (logs::actions rule-instance)
                    ;; storing actions
                    (list
                     ,@(mapcar
                        (lambda (context)
                          `(lambda (message)
                             (LoGS::add-to-context ,context message)))
                        (get-rule-slot rule :storing)))
                    )))
    rule-instance
  ))

(defmethod fill-rule-template ((ruleset ruleset-macro))
  `(let ((ruleset-instance 
          (make-instance
           'ruleset
           ,@(loop as slot in '(:match :name :actions :environment :timeout
                                :relative-timeout :filter :continuep)
                as slot-contents = (get-rule-slot ruleset slot)
                if slot-contents append `(,slot ,slot-contents)))))
     (progn
       ,@(mapcar
          (lambda (rule)
            `(logs::enqueue ruleset-instance ,rule))
          (get-rule-slot ruleset :elements)))
     ruleset-instance))

(defgeneric parse-rule (rule exprs))

(defmethod parse-rule ((rule rule-macro) exprs)
  (unless (null exprs)
    (parse-rule rule (parse-keyword rule (car exprs) (cdr exprs)))))

;; handle one keyword and its arguments, returning what remains in exprs
(defun parse-keyword (rule keyword exprs)
  (let* ((keyword (standardize keyword))
         (keyword (or (alias keyword) keyword)))
    (aif (handle-fn keyword rule)
         (funcall it rule exprs)
         (error "Unknown keyword ~S" keyword))))


(defun handle-name (rule exprs)
  (destructuring-bind (name . rest) exprs
    (aif (macro-name rule)
         (error "Each rule can have only one name.  Rule was already named ~
                 as ~a" it)
         (setf (macro-name rule) name))
    rest))

(defmethod handle-fn ((keyword (eql :name)) (type rule-macro))
  #'handle-name)


(defmethod get-rule-slot ((rule rule-macro) (slot (eql :name)))
  (declare (ignore slot))
  (macro-name rule))

;;; There is a macro hiding in all the HANDLE- functions!


;;; Here I attempt to explain the MATCH syntax.
;;;
;;; The combinations of match are quite straightforward.
;;;
;;; You may combine several regular expressions using AND and OR.
;;; They work as follows:

;;; (rule matching "regexp-one" AND "two") will
;;; succeed iff the message matches both the regular expressions given
;;; by "regexp-one" AND "two" (in this case).

;;; Similarly OR will match only those regular expressions which match
;;; at least one of the given strings.

;;; We must say REGEXP before giving a form that is a regular
;;; expression but this is not necessary for string literals.  For
;;; example, we must say
;;; (rule matching regexp (format () "a regexp~a" "?"))
;;; but we need not use regexp for (rule matching "string regexp")

;;; NOT is used when we want to say that lack of a given regular
;;; expression is necessary for the match to succeed.

;;; AND and OR have equal precedence and explicit order must be
;;; specified within parentheses.

(defun handle-match (rule exprs)
  (destructuring-bind (car . cdr) exprs
    (cond ((not (null (macro-match rule)))
           (error "Only one match expression may be specified per rule."))
          ((or (stringp car) (samep car :regexp))
           (destructuring-bind (separated rest) (match-separate exprs)
             (setf (macro-match rule) `(:regexp ,@separated)
                   cdr rest)))
          (t
           ;; Assume it is a predefined function
           (setf (macro-match rule) car)))
    cdr))

(defmethod handle-fn ((keyword (eql :match)) (type rule-macro))
  #'handle-match)

(defun match-separate (expr)
  (labels ((conjunctionp (X) (or (samep X :and) (samep X :or)))
           (manage-keyword (keyword list)
             (cond ((stringp keyword) (list keyword list))
                   ((samep keyword :regexp)
                    (list `(:regexp ,(pop list)) list))
                   ((samep keyword :not)
                    (destructuring-bind (first rest)
                        (manage-keyword (pop list) list)
                      (list `(:not ,first) rest)))
                   ((consp keyword)
                    (list (first (match-separate keyword)) '()))
                   (t (error "~s unexpected.  Perhaps you meant to ~
                              prefix it with :REGEXP?"
                             keyword)))))
    (loop
      with collect = nil
      as first = (first expr)
      as conjunction = nil then (not conjunction)
      as next-expr = (pop expr) then (pop expr)
      if conjunction
       if (conjunctionp first) 
       collect first into c
        else return (if next-expr
                        (list c (cons next-expr expr))
                        (list c expr))
        else do (destructuring-bind (keyword-part remaining-part)
                    (manage-keyword first expr)
                  (setq expr remaining-part collect keyword-part))
        and collect collect into c)))

;; like OR, but returns all of the values from the statement that returns t 
;; as its first value
;; 
;; this way it may be nested and still ensure that all of the values are
;; properly returned. 
(defmacro multiple-value-or (&rest or-exprs)
  (when (car or-exprs)
    (let ((return-values (gensym)))
      `(let ((,return-values (multiple-value-list ,(car or-exprs))))
         (cond ((car ,return-values)
                (values-list ,return-values))
               ((null ',(cdr or-exprs))
                ())
               (t 
                (multiple-value-or ,@(cdr or-exprs))
                ))))))

(defun parse-match (expr gensym)
  (let ((output '()) (stack '()))
    (labels ((symid (sym) (standardize sym))
             (actual-sym (sym)
               (ecase (symid sym) (:and 'cl:and) (:or 'multiple-value-or)))
             (stack-push (x) (push (if (consp x) (main-parse x) x) stack))
             (stack-pop ()
               (destructuring-bind (oprnd1 oprnd2 . rest) output
                 (let ((top (actual-sym (pop stack))))
                   (setq output `((,top ,oprnd1 ,oprnd2) ,@rest)))))
             (unstack () (loop while stack do (stack-pop)))
             (cl-ppcre-it (form)
               `(cl-ppcre:scan-to-strings ,form ,gensym))
             (manage-form (form)
               (when form
                 (cond ((stringp form) (cl-ppcre-it form))
                       ((eq (car form) :regexp)
                        (cl-ppcre-it (second form)))
                       ((eq (car form) :not)
                        `(cl:not ,(manage-form (cadr form))))
                       (t (main-parse form)))))
             (main-parse (list)
               (loop
                 as operator = nil then (not operator)
                 as x in (reverse list)
                 if operator do (stack-push x)
                 else        do (push (manage-form x) output)
                 finally (unstack) (return (car output)))))
      (main-parse expr))))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :match)))
  "Return the form for MATCH tag"
  (declare (ignore slot))
  (let ((match (macro-match rule)))
    ;; if there is no match, we need not generate a match function
    ;; I believe the match function that matched no regexps was technically
    ;; correct, but this is slightly better in terms of speed and IMO clarity
    (if (and (consp match) (eq (car match) :regexp))
        (let ((msg (gensym "MESSAGE"))
              (matches (gensym "MATCHES"))
              (sub-matches (gensym "SUB-MATCHES"))
              (mmesg (gensym "MSGMSG")))
          `(lambda (,msg)
             (multiple-value-bind (,matches ,sub-matches)
                 (let ((,mmesg (message ,msg)))
                   ,(parse-match (cdr match) mmesg))
               (when ,matches
                 (values
                  ,matches
                  (cons
                   (list ',(intern "SUB-MATCHES")
                         ,sub-matches)
                   (loop for count from 0 
                      below ,(length (get-rule-slot rule :bind))
                      for val in ',(get-rule-slot rule :bind)
                      while (> (length ,sub-matches) count)
                      collect
                        (list val (aref ,sub-matches count)))))))))
        match)))


(defun handle-bind (rule exprs)
  "vars to bind from match"
  (destructuring-bind (car . cdr) exprs
    (cond ((or
            (samep car :vars)
            (samep car :variables))
           (handle-bind rule cdr))
          ((listp car)
           (setf (macro-bind rule) car)
           cdr))))

(defmethod handle-fn ((keyword (eql :bind)) (type rule-macro))
  #'handle-bind)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :bind)))
  (declare (ignore slot))
  (macro-bind rule))


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
                  (macro-actions rule)))
          (push fn (macro-actions rule)))
      rest)))

(defmethod handle-fn ((keyword (eql :actions)) (type rule-macro))
  #'handle-actions)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :actions)))
  (declare (ignore slot))
  (aif (macro-actions rule)
       (cons 'list
             (nreverse it))))


(defun handle-timeout (rule exprs)
  (destructuring-bind (preposition seconds &rest rest) exprs
    (cond ((samep preposition :in)
           (aif (macro-relative-timeout rule)
                (error "Relative timeout was already specified as ~S" it))
           (setf (macro-relative-timeout rule) seconds)
           rest)
          ((samep preposition :at)
           (aif (macro-timeout rule)
                (error "Timeout was already specified as ~S" it))
           (setf (macro-timeout rule) seconds)
           rest)
          (t (error "Unexpected keyword ~S" preposition)))))

(defmethod handle-fn ((keyword (eql :timeout)) (type rule-macro))
  #'handle-timeout)

(defmethod get-rule-slot ((r rule-macro) (slot (eql :timeout)))
  "By default just return the slot value"
  (macro-timeout r))

(defmethod get-rule-slot ((r rule-macro) (slot (eql :relative-timeout)))
  "By default just return the slot value"
  (macro-relative-timeout r))


(defun handle-setenv (rule exprs)
  (destructuring-bind (var = val &rest rest) exprs
    (unless (symbolp var) (error "Cannot assign value to ~S" var))
    (unless (samep = :=) (error "Incorrect assignment symbol.  Expected ~
                                 = but got ~S" =))
    (pushnew `(list ',var ,val) (macro-environment rule)
             :test #'eq :key #'(lambda (x) (second (second x))))
    (if (samep (car rest) :and)
        (handle-setenv rule (cdr rest))
        rest)))

(defmethod handle-fn ((keyword (eql :setenv)) (type rule-macro))
  #'handle-setenv)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :environment)))
  (declare (ignore slot))
  (aif (macro-environment rule)
       `(list ,@(nreverse it))
       '()))


(defun handle-continuep (rule exprs)
  (setf (macro-continuep rule) t)
  exprs)

(defmethod handle-fn ((keyword (eql :continuep)) (type rule-macro))
  #'handle-continuep)

(defmethod get-rule-slot ((r rule-macro) (slot (eql :continuep)))
  "By default just return the slot value"
  (macro-continuep r))


(defun handle-filter (rule exprs)
  (setf (macro-filter rule) t
        (macro-continuep rule) t)
  (handle-match rule exprs))

(defmethod handle-fn ((keyword (eql :filter)) (type rule-macro))
  #'handle-filter)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :filter)))
  (declare (ignore slot))
  (when (and (macro-filter rule) (macro-actions rule))
    (error "Cannot specify actions when filtering rule.")))



;; (ruleset
;;  named 'foo
;;  matching regexp "foo"
;;  elements
;;  ((rule named 'bar)))

(defun handle-elements (ruleset exprs)
  (destructuring-bind ((&rest rule-list) . rest) exprs
    (setf (macro-elements ruleset) (append (macro-elements ruleset)
                                           rule-list))
    rest))

(defmethod handle-fn ((keyword (eql :elements)) (type ruleset-macro))
  #'handle-elements)


(defmethod get-rule-slot ((ruleset ruleset-macro) (slot (eql :elements)))
  (declare (ignore slot))
  (macro-elements ruleset))



(defmethod handle-storing (rule exprs)
  (if (samep (car exprs) :into)
      (handle-storing rule (cdr exprs))
      (destructuring-bind ((&rest contexts) . rest) exprs
        (setf (macro-storing rule)
              (append (macro-storing rule)
                      contexts))
        rest)))

(defmethod handle-fn ((keyword (eql :storing)) (type rule-macro))
  #'handle-storing)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :storing)))
  (declare (ignore slot))
  (macro-storing rule))

(defmethod get-rule-slot ((ruleset ruleset-macro) (slot (eql :storing)))
  (declare (ignore slot ruleset))
  (error "cannot use ruleset to store messages into a context!"))



(defmethod handle-fn ((keyword (eql :exec)) (type rule-macro))
  #'handle-exec)

(defun handle-exec (rule exprs)
  (destructuring-bind (prog args . rest) exprs
    (push 
     `(LoGS::do-exec ,prog ',args)
     (macro-actions rule))
    rest))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :exec)))
  (declare (ignore slot))
  (macro-exec rule))

(defmethod get-rule-slot ((ruleset ruleset-macro) (slot (eql :exec)))
  (declare (ignore slot))
  (error "cannot use ruleset to exec~%"))



(defmethod handle-fn ((keyword (eql :write)) (type rule-macro))
  #'handle-write)

(defun handle-write (rule exprs)
  (if (samep (car exprs) :to)
      (handle-write rule (cdr exprs))
      (destructuring-bind (filename . rest) exprs
        (push
         `(lambda (message)
             ',filename)
         (macro-actions rule))
        rest)))



;;; Aliases
(defmacro set-aliases (symbol (&rest aliases))
  `(let ,@(loop
             as alias in aliases
             as alias-sym = (gensym)
             with symbol-sym = (gensym)
             with symbol-let = `(,symbol-sym (standardize ',symbol))
             collecting `(,alias-sym (standardize ',alias)) into lets
             appending `((alias ,alias-sym) ,symbol-sym) into setfs
             finally (return `((,symbol-let ,@lets) (setf ,@setfs))))))

(set-aliases match      (matching matches))
(set-aliases filter     (filtering))
(set-aliases actions    (doing do))
(set-aliases setenv     (set with))
(set-aliases name       (named))
(set-aliases timeout    (timing-out))
(set-aliases continuep  (continue continuing))
(set-aliases bind       (binding))
(set-aliases elements   (contents containing))
