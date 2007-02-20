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

(in-package #:language)
(use-package :org.ancar.clunit)
(use-package :org.prewett.logs)

(defun not-by-default (symbol)
  "Asserts that SYMBOL does not occur when it is not specified when
creating a rule."
  (let ((symbol (if (keywordp symbol)
                    symbol
                    (intern (symbol-name symbol) :keyword))))
   (not (member symbol (macroexpand-1 '(rule))))))

(deftest "An empty rule generates no errors."
    :category 'basic-rule
    :test-fn (lambda ()
               (eq (type-of (rule)) 'org.prewett.logs:rule)))

(deftest "An unnamed rule is not named by default"
    :category 'name
    :test-fn (lambda () (not-by-default :name)))

(deftest "A named rule takes its name from the environment."
    :category 'name
    :test-fn (lambda ()
               (let* ((name "string-name")
                      (rule (rule named name)))
                 (string-equal (logs::name rule) name))))

(deftest "A named rule may be quoted."
    :category 'name
    :test-fn (lambda ()
               (eq (logs::name (rule named 'some-name)) 'some-name)))

(deftest "A rules name may be an evaluated value"
    :category 'name
    :test-fn (lambda ()
               (let ((i 20))
                 (= (logs::name (rule named (+ i 20))) 40))))

(deftest "Continue is not set by default"
    :category 'continue
    :test-fn (lambda () (not-by-default :continue)))

(deftest "Continue works"
    :category 'continue
    :test-fn (lambda ()
               (logs::continuep (rule continuing))))

(deftest "Timeout is not set by default"
    :category 'timeout
    :test-fn (lambda () (not-by-default :timeout)))

(deftest "Relative timeout is not set by default"
    :category 'timeout
    :test-fn (lambda () (not-by-default :relative-timeout)))

(deftest "Timeout works"
    :category 'timeout
    :test-fn (lambda ()
               (let ((rule (rule timeout at 12)))
                 (and (logs::timeout rule)
                      (= (logs::timeout rule) 12)))))

(deftest "Relative timeout works"
    :category 'timeout
    :test-fn (lambda ()
               (let ((rule (rule timeout in 12)))
                 (and (logs::relative-timeout rule)
                      (= (logs::relative-timeout rule) 12)))))

(deftest "context timeout works"
    :category 'timeout
    :test-fn 
    (lambda ()
      (let ((context (context timeout at 12)))
        (and (logs::timeout context)
             (= (logs::timeout context) 12)))))

(deftest "context relative timeout works"
    :category 'timeout
    :test-fn 
    (lambda ()
      (let ((context (context timeout in 12)))
        (and (logs::relative-timeout context)
             (= (logs::relative-timeout context) 12)))))

;; Rule evaluation tests
(deftest "a rule's name may be specified with a variable"
    :category 'evaluation-tests
    :test-fn
    (lambda ()
      (let* ((rule-name "test rule name")
             (rule (rule named rule-name)))
        (equal rule-name (LoGS::name rule)))))

(deftest "a rule's match may be specified with a variable"
    :category 'evaluation-tests
    :test-fn
    (lambda ()
      (let* ((rule-match #'LoGS::match-all)
             (rule (rule matching rule-match)))
        (equal rule-match (LoGS::match rule)))))

(deftest "a context's name may be specified with a variable"
    :category 'evaluation-tests
    :test-fn
    (lambda ()
      (let* ((context-name "test context name")
             (context (context named context-name)))
        (equal context-name (LoGS::name context)))))

;; var tests

(deftest "name can be simple symbol"
    :category 'var-tests
    :test-fn
    (lambda ()
      (let ((rule (rule named 'jims-test-symbol)))
        (and
         (symbolp (LoGS::name rule))
         (equal 'jims-test-symbol (LoGS::name rule))))))

(deftest "name can be simple string"
    :category 'var-tests
    :test-fn
    (lambda ()
      (let ((rule (rule named "jims test string")))
        (and
         (stringp (LoGS::name rule))
         (equal "jims test string" (LoGS::name rule))))))

(deftest "name can be lexical variable"
    :category 'var-tests
    :test-fn
    (lambda ()
      (let ((jims-test-var "lexical variable name"))
        (let ((rule (rule named jims-test-var)))
                  (equal (LoGS::name rule) jims-test-var)))))

(deftest "name can be set by matching rule's environment"
    :category 'complex-var-tests
    :test-fn
    (lambda ()
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
        )))

;; regexp var tests

(deftest "regexp can be simple string"
    :category 'var-tests
    :test-fn
    (lambda ()
      (let ((rule (rule matching regexp "jims test string")))
        (stringp 
         (logs::check-rule rule (make-instance 'message :message "jims test string should match") NIL)))))

(deftest "regexp can be lexical variable"
    :category 'var-tests
    :test-fn
    (lambda ()
      (let ((jims-test-var "lexical variable regexp"))
        (let ((rule (rule matching regexp jims-test-var)))
          (stringp (logs::check-rule rule (make-instance 'message :message "lexical variable regexp should match") NIL))))))

(deftest "regexp can be set by matching rule's environment"
    :category 'complex-var-tests
    :test-fn
    (lambda ()
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
               NIL))))))