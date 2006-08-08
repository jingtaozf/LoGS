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
  (:import-from :logs :rule :message)
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
(defmacro handle-fn (keyword)
  `(get ,keyword 'handle-fn))

;;; To use synonyms easily
(defmacro alias (keyword)
  `(get ,keyword 'alias))

;;; Influenced by Peter Norvig's LOOP implementation
(defstruct rule-macro
  (name '())
  (match '())
  (bind '())
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
    (aif (rule-macro-name rule)
         (error "Each rule can have only one name.  Rule was already named ~
                 as ~a" it)
         (setf (rule-macro-name rule) name))
    rest))

(setf (handle-fn :name) #'handle-name)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :name)))
  (declare (ignore slot))
  (rule-macro-name rule))

;;; There is a macro hiding in all the HANDLE- functions!

(defun handle-match (rule exprs)
  ;; Syntax:
  ;; matching regexp "abc" and regexp "def" == matching regexp "abcdef"
  ;; matching "string literal" == matching regexp "string literal"
  ;; matching regexp "foo+" and #'function == ERROR
  ;;
  ;; matching regexp "abc" or regexp "def"
  (destructuring-bind (car . cdr) exprs
    (cond ((not (null (rule-macro-match rule)))
           (error "Only one match expression may be specified per rule."))
          ((or (stringp car) (samep car :regexp))
           (destructuring-bind (sanitized exprs) (match-sanitize exprs)
             (setf (rule-macro-match rule) `(:regexp ,@sanitized))
             exprs))
          (t
           ;; Assume it is a predefined function
           (setf (rule-macro-match rule) car)
           cdr))))

(setf (handle-fn :match) #'handle-match)

(defun match-sanitize (expr)
  "Removes the `REGEXP's from EXPR and finds where the match
expression ends.  Returns a (MATCH REST) list where MATCH is the
sanitized part of the match and REST is the remainder of the rule for
other settings."
  (loop
    with list = expr
    as car = (car list)
    and conjunction = nil then (not conjunction)
    if conjunction
      unless (or (samep car :and) (samep car :or) (samep car :>or))
        do (loop-finish)
      end
    else
      do (cond ((stringp car) t)
               ((samep car :regexp) (pop list) (setq car (car list)))
               (t (error "Expected keyword REGEXP or string.  Got ~s"
                         car)))
    collect car into collect
    do (pop list)
    finally (return (list collect list))))

(defun parse-and-or (expr)
  "Return EXPR in prefix notation.  EXPR is in infix expression of the
form (form1 conjunction form2 conjunction ...) where conjunction is
AND or OR.  AND has a higher priority than OR.  To allow those
situations where an OR of higher priority is desired, we use >OR.  >OR
has greater priority than AND."
  (let ((output '()) (stack '()))
    (labels ((symid (sym) (standardize sym))
             (actual-sym (sym)
               (ecase (symid sym) (:and 'cl:and) ((:or :>or) 'cl:or)))
             (priority (sym)
               (ecase (symid sym) (:or  0) (:and 1) (:>or 2)))
             (stack-push (x)
               (cond ((or (null stack)
                          (>= (priority x) (priority (car stack))))
                      (push x stack))
                     (t (stack-pop) (stack-push x))))
             (stack-pop ()
               (destructuring-bind (oprnd1 oprnd2 &rest rest) output
                 (let ((top (actual-sym (pop stack))))
                   (setq output `((,top ,oprnd1 ,oprnd2) ,@rest)))))
             (unstack () (loop while stack do (stack-pop))))
      (loop
        as toggle = t then (not toggle)
        as x in (reverse expr)
        if toggle do (push x output)
        else      do (stack-push x)
        finally (unstack) (return (car output))))))

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :match)))
  "Return the form for MATCH tag"
  (declare (ignore slot))
  (let ((match (rule-macro-match rule)))
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
                  ,(parse-and-or
                    (loop
                      as toggle = nil then (not toggle)
                      as e in (cdr match)
                      when toggle collect e
                      else collect `(cl-ppcre:scan-to-strings ,e ,mmesg))))
              (when ,matches 
                (values t
                        (aif ',(get-rule-slot rule :bind)
                             (cons
                              (list ',(intern "SUB-MATCHES")
                                    ,sub-matches)
                              (loop for count from 0 below (length ,sub-matches)
                                    collect
                                    (list (nth count it) (aref ,sub-matches count))))
                             (list (list ',(intern "SUB-MATCHES")
                                         ,sub-matches))))))))
        match)))

(defun handle-bind (rule exprs)
  "vars to bind from match"
  (destructuring-bind (car . cdr) exprs
    (cond ((samep car :vars)
           (handle-bind rule cdr))
          ((listp car)
           (format t "found list: ~A~%" car)
           (setf (rule-macro-bind rule) car)
           cdr))))

(setf (handle-fn :bind) #'handle-bind)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :bind)))
  (declare (ignore slot))
  (rule-macro-bind rule))

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

(setf (handle-fn :actions) #'handle-actions)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :actions)))
  (declare (ignore slot))
  (aif (rule-macro-actions rule)
       (cons 'list
             (nreverse it))))


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

(setf (handle-fn :timeout) #'handle-timeout)

(defmethod get-rule-slot ((r rule-macro) (slot (eql :timeout)))
  "By default just return the slot value"
  (rule-macro-timeout r))

(defmethod get-rule-slot ((r rule-macro) (slot (eql :relative-timeout)))
  "By default just return the slot value"
  (rule-macro-relative-timeout r))


(defun handle-setenv (rule exprs)
  (destructuring-bind (var = val &rest rest) exprs
    (unless (symbolp var) (error "Cannot assign value to ~S" var))
    (unless (samep = :=) (error "Incorrect assignment symbol.  Expected ~
                                 = but got ~S" =))
    (pushnew `(list ',var ,val) (rule-macro-environment rule)
             :test #'eq :key #'(lambda (x) (second (second x))))
    (if (samep (car rest) :and)
        (handle-setenv rule (cdr rest))
        rest)))

(setf (handle-fn :setenv) #'handle-setenv)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :environment)))
  (declare (ignore slot))
  (aif (rule-macro-environment rule)
       `(list ,@(nreverse it))
       '()))


(defun handle-continuep (rule exprs)
  (setf (rule-macro-continuep rule) t)
  exprs)

(setf (handle-fn :continuep) #'handle-continuep)

(defmethod get-rule-slot ((r rule-macro) (slot (eql :continuep)))
  "By default just return the slot value"
  (rule-macro-continuep r))


(defun handle-filter (rule exprs)
  (setf (rule-macro-filter rule) t
        (rule-macro-continuep rule) t)
  (handle-match rule exprs))

(setf (handle-fn :filter) #'handle-filter)

(defmethod get-rule-slot ((rule rule-macro) (slot (eql :filter)))
  (declare (ignore slot))
  (when (and (rule-macro-filter rule) (rule-macro-actions rule))
    (error "Cannot specify actions when filtering rule.")))


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
