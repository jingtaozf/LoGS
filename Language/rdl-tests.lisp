;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2006-2018 Vijay Lakshminarayanan

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

(in-package #:org.prewett.logs.test)

(use-package :org.prewett.logs)
(use-package :org.prewett.logs.language)
(use-package :lift)
(defun not-by-default (symbol)
  "Asserts that SYMBOL does not occur when it is not specified when
creating a rule."
  (let ((symbol (if (keywordp symbol)
                    symbol
                    (intern (symbol-name symbol) :keyword))))
   (not (member symbol (macroexpand-1 '(rule))))))

(deftestsuite Language (LoGS) ())
(deftestsuite basic-rule (Language) ())
(addtest (basic-rule)
  empty-rule-generates-no-errors
  (ensure-same (type-of (rule)) 'org.prewett.logs:rule))

(deftestsuite name-tests (Language) ())

(addtest (name-tests)
  unnamed-rule-is-not-named-by-default
  (not-by-default :name))

(addtest (name-tests)
  named-rule-takes-its-name-from-the-environment
  (let* ((name "string-name")
         (rule (rule named name)))
    (ensure-same (logs::name rule) name)))

(addtest (name-tests)
  named-rule-may-be-quoted
  (eq (logs::name (rule named 'some-name)) 'some-name))

(addtest (name-tests)
  rules-name-may-be-an-evaluated-value
  (let ((i 20))
    (= (logs::name (rule named (+ i 20))) 40)))

(deftestsuite continue-tests (Language) ())

(addtest (continue-tests)
  continue-is-not-set-by-default
  (not-by-default :continue))

(addtest (continue-tests)
  continue-works
  (logs::continuep (rule continuing)))

(deftestsuite timeout-tests (Language) ())

(addtest (timeout-tests)
  timeout-is-not-set-by-default
  (not-by-default :timeout))

(addtest (timeout-tests)
  relative-timeout-is-not-set-by-default
  (not-by-default :relative-timeout))

(addtest (timeout-tests)
  timeout-works
  (let ((rule (rule timeout at 12)))
    (and (logs::timeout rule)
         (= (logs::timeout rule) 12))))

(addtest (timeout-tests)
  relative-timeout-works
  (let ((rule (rule timeout in 12)))
    (and (logs::relative-timeout rule)
         (= (logs::relative-timeout rule) 12))))

(addtest (timeout-tests)
  context-timeout-works
  (let ((context (context timeout at 12)))
    (and (logs::timeout context)
         (= (logs::timeout context) 12))))

(addtest (timeout-tests)
  context-relative-timeout-works
  (let ((context (context timeout in 12)))
    (and (logs::relative-timeout context)
         (= (logs::relative-timeout context) 12))))

(deftestsuite evaluation-tests (Language) ())

(addtest (evaluation-tests)
  rules-name-may-be-specified-with-a-variable
  (let* ((rule-name "test rule name")
         (rule (rule named rule-name)))
    (equal rule-name (LoGS::name rule))))

(addtest (evaluation-tests)
  rules-match-may-be-specified-with-a-variable
      (let* ((rule-match #'LoGS::match-all)
             (rule (rule matching rule-match)))
        (equal rule-match (LoGS::match rule))))

(addtest (evaluation-tests)
  contexts-name-may-be-specified-with-a-variable
  (let* ((context-name "test context name")
         (context (context named context-name)))
    (equal context-name (LoGS::name context))))


(deftestsuite var-tests (Language) ())

(addtest (var-tests)
  name-can-be-simple-symbol
      (let ((rule (rule named 'jims-test-symbol)))
        (and
         (symbolp (LoGS::name rule))
         (equal 'jims-test-symbol (LoGS::name rule)))))

(addtest (var-tests)
  name-can-be-simple-string
      (let ((rule (rule named "jims test string")))
        (and
         (stringp (LoGS::name rule))
         (equal "jims test string" (LoGS::name rule)))))

(addtest (var-tests)
  name-can-be-lexical-variable
  (let ((jims-test-var "lexical variable name"))
    (let ((rule (rule named jims-test-var)))
      (equal (LoGS::name rule) jims-test-var))))

(addtest (var-tests)
  name-can-be-set-by-matching-rules-environment
  (let* ((new-rule NIL)
         (rule (rule matching 
                     (lambda (message environment)
                       (declare (ignore message))
                       (values t
                               (list (list 'jims-test-env-var "environment variable name"))
                               NIL
                               ))
                     doing 
                     (lambda (message environment)
                       (declare (ignore message))
                       ;; ()))))
                       (setf new-rule
                             (rule named
                                   (get-LoGS-env-var 
                                    'jims-test-env-var environment)))))))
    (LoGS::check-rule rule (make-instance 'message :message "some message") NIL)
    (equal (LoGS::name new-rule)
           "environment variable name")
    ))

(addtest (var-tests)
  regexp-can-be-simple-string
  (let ((rule (rule matching regexp "jims test string")))
    (stringp 
     (logs::check-rule rule (make-instance 'message :message "jims test string should match") NIL))))

(addtest (var-tests)
  regexp-can-be-lexical-variable
  (let ((jims-test-var "lexical variable regexp"))
    (let ((rule (rule matching regexp jims-test-var)))
      (stringp (logs::check-rule rule (make-instance 'message :message "lexical variable regexp should match") NIL)))))

(deftestsuite complex-var-tests (Language) ())

(addtest (complex-var-tests)
  regexp-can-be-set-by-matching-rules-environment
      (let* ((new-rule NIL)
             (rule (rule matching 
                         (lambda (message environment)
                           (declare (ignore message environment))
                           (values t
                                   '((test-env-var 
                                      "environment variable regexp")
                                     (foo 42))))
                         doing 
                         (lambda (message environment)
                           (declare (ignore message))
                           (setf new-rule
                                  (rule matching regexp 
                                        (get-LoGS-env-var
                                         'test-env-var environment)))))))
        (LoGS::check-rule rule 
                          (make-instance 'message :message "some message") NIL)
        (and new-rule
             (stringp
              (LoGS::check-rule 
               new-rule 
               (make-instance 
                'message 
                :message "environment variable regexp should match") 
               NIL)))))
