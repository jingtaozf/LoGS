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


;(load "CLUnit.lisp")

(in-package :LoGS)
(use-package :ORG.ANCAR.CLUNIT)

(defvar *do-error* t)

(defun assert-equal (a b &key test)
       (not 
        (unless 
            (if test
                (funcall test a b)
                (equal a b))
          (or test 
              (setf test #'equal))
           (not
            (when *do-error* 
              (error "failed equal asseration with test: ~A and values: ~A and ~A~%" test a b))))))
       
(defun assert-nil (a)
  (not
   (unless (null a)
     (not
      (when *do-error*
        (error "Value: ~A is not null~%" a))))))

(defun assert-non-nil (a)
  (not
   (when (null a)
     (not
      (when *do-error*
        (error "Value: ~A is null~%" a))))))


(defun assert-not-equal (a b &key test)
  (not (unless
           (not 
            (if test
               (funcall test a b)
               (equal a b)))
         (not
          (when *do-error*
            (or test 
                (setf test #'equal))
            (error "failed not equal asseration with test: ~A and values: ~A and ~A~%" test a b))))))

(defun find-package-functions (package)
  (let ((lst ()))
    (do-all-symbols (s lst)     
      (when (and (eq (find-package package) (symbol-package s)) 
                 (fboundp s)) 
        (push s lst)))
    lst))

(defun match-all (message)
  (declare (ignore message))
  t)

(defun match-none (message)
  (declare (ignore message))
  ())

;; dummy test
(deftest "foo" :test-fn (lambda () t))


;; linked list tests

(deftest "item inserted into a ruleset with after and () neighbor is tail"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :after)
      (assert-equal
       (data (tail dll))
       42))))

(deftest "item inserted into a ruleset with before and () neighbor is head"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (assert-equal
       (data (head dll))
       42))))

(deftest "item inserted into linked list is inserted with item -> dlli"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (assert-non-nil (gethash 42 (list-entries dll)))
      (assert-equal 42 (data (gethash 42 (list-entries dll)))))))

(deftest "only item in linked list has no rlink or llink"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (and
       (assert-equal () (rlink (head dll)))
       (assert-equal () (llink (head dll)))))))

(deftest "only item in linked list is head and tail"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (assert-non-nil (head dll))
      (assert-equal (head dll) (tail dll)))))

(deftest "item removed from list is removed from list-entries"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (dll-delete dll 42)
      (assert-nil (gethash 42 (list-entries dll))))))
    
(deftest "rule that becomes head and tail is traversed"
    :test-fn
  (lambda () 
    (let* ((*root-ruleset* (make-instance 'ruleset))
           (ran-tail-rule ())
           (*messages* 
            (make-instance 
             'list-follower :message-list (list "the" "cat")))
           (dying-rule (make-instance 'rule
                                      :name 'dying-rule
                                      ;; it matches a message and dies
                                      :match #'match-all
                                      :delete-rule #'match-all))
           ;; becomes the head and tail of the ruleset
           (tail-rule (make-instance 'rule
                                     :name 'tail-rule
                                     :match #'match-all
                                     :actions
                                     (list
                                      (lambda (message)
                                        (declare (ignore message))
                                        (setf ran-tail-rule t))))))
      
      
      
      (enqueue *root-ruleset* dying-rule)
      (enqueue *root-ruleset* tail-rule)
      (main)
      (assert-non-nil ran-tail-rule))))

      
      
                                            

;; rule creation tests

(deftest "Rules always have names"
    :test-fn
  (lambda ()
    (assert-non-nil (name (make-instance 'rule)))))

(deftest "Rule name provided" 
    :test-fn 
  (lambda () 
    (let* ((name "asdf")
           (name-provided-rule (make-instance 'rule :name name)))
      (assert-equal name (name name-provided-rule)))))

(deftest "Rule name not provided"
    :test-fn
  (lambda () 
    (let ((nameless-rule (make-instance 'rule)))
      (assert-non-nil (name nameless-rule)))))

;; ruleset insertion tests
(deftest "ruleset insertion test"
    :test-fn
  (lambda ()
    (let* ((rule (make-instance 'rule))
           (ruleset (make-instance 'ruleset)))
      (progn
        (enqueue ruleset rule)
        (assert-equal rule (gethash (name rule) (elements ruleset)) 
                      :test #'eq)))))

(deftest "ruleset named rule insertion test"
    :test-fn
  (lambda ()
    (let* ((name "fooble")
           (rule (make-instance 'rule :name name))
           (ruleset (make-instance 'ruleset)))
      (progn
        (enqueue ruleset rule)
        (assert-equal rule (gethash name (elements ruleset)) 
                      :test #'eq)))))


(deftest "retrieving arbitrary rules from rulesets"
    :test-fn
  (lambda ()
    (let* ((rule1 (make-instance 'rule :name 'rule1))
           (rule2 (make-instance 'rule :name 'rule2))
           (ruleset (make-instance 'ruleset :name "ruleset")))
      (enqueue ruleset rule1)
      (enqueue ruleset rule2)
      (and
       (eq (gethash 'rule2 (elements ruleset)) rule2)
       (eq (gethash 'rule1 (elements ruleset)) rule1)))))

(deftest "retrieving arbitrary rulesets from rulesets"
    :test-fn
  (lambda ()
    (let* ((ruleset1 (make-instance 'ruleset :name 'ruleset1))
           (ruleset2 (make-instance 'ruleset :name 'ruleset2))
           (ruleset (make-instance 'ruleset :name "ruleset")))
      (enqueue ruleset ruleset1)
      (enqueue ruleset ruleset2)
      (and
       (eq (gethash 'ruleset2 (elements ruleset)) ruleset2)
       (eq (gethash 'ruleset1 (elements ruleset)) ruleset1)))))

(deftest "retrieving arbitrary unnamed rulesets from rulesets"
    :test-fn
  (lambda ()
    (let* ((ruleset1 (make-instance 'ruleset))
           (ruleset2 (make-instance 'ruleset))
           (ruleset (make-instance 'ruleset :name "ruleset")))
      (enqueue ruleset ruleset1)
      (enqueue ruleset ruleset2)
      (and
       (eq (gethash (name ruleset2) (elements ruleset)) ruleset2)
       (eq (gethash (name ruleset1) (elements ruleset)) ruleset1)))))

(deftest "two different named rules are not equal"
    :test-fn
  (lambda () 
    (let ((r1 (make-instance 'rule :name 'a))
          (r2 (make-instance 'rule :name 'b)))
      (not (eq r1 r2)))))

(deftest "two different unnamed rules are not equal"
    :test-fn
  (lambda () 
    (let ((r1 (make-instance 'rule))
          (r2 (make-instance 'rule)))
      (not (eq r1 r2)))))

(deftest "rule is entered into ruleset name table"
    :test-fn
  (lambda ()
    (let ((rule (make-instance 'rule :name 'r1))
          (ruleset (make-instance 'ruleset :name 'ruleset)))
      (enqueue ruleset rule)
      (assert-equal rule (gethash (name rule) (elements ruleset))))))

(deftest "unnamed rule is entered into ruleset name table"
    :test-fn
  (lambda ()
    (let ((rule (make-instance 'rule))
          (ruleset (make-instance 'ruleset :name 'ruleset)))
      (enqueue ruleset rule)
      (assert-equal rule (gethash (name rule) (elements ruleset))))))

(deftest "doubly linked list with an item has a head"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (enqueue dll 42)
      (assert-non-nil (head dll)))))

(deftest "rule-before is entered into ruleset name table"
    :test-fn
  (lambda () 
    (let ((r1 (make-instance 'rule :name "r1"))
          (r2 (make-instance 'rule :name "r2"))
          (*ruleset* (make-instance 'ruleset :name 'ruleset)))
      (enqueue *ruleset* r1)
      (let ((*current-rule* (head *ruleset*)))
        (rule-before r2)
        (assert-equal r2 (gethash (name r2) (elements *ruleset*)))))))


(deftest "unnamed rule-before is entered into ruleset name table"
    :test-fn
  (lambda ()
    (let ((r1 (make-instance 'rule :name 'current))
          (*ruleset* (make-instance 'ruleset :name 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue *ruleset* r1)
      (let ((*current-rule* (head *ruleset*)))
        (rule-before rule)
        (assert-equal rule (gethash (name rule) (elements *ruleset*)))))))

(deftest "rule-after is entered into ruleset name table"
    :Test-fn
  (lambda ()
    (let ((r1 (make-instance 'rule :name 'r1))
          (r2 (make-instance 'rule :name 'r2))
          (*ruleset* (make-instance 'ruleset)))
      (enqueue *ruleset* r1)
      (let ((*current-rule* (head *ruleset*)))
        (rule-after r2)
        (assert-equal r2 (gethash (name r2) (elements *ruleset*)))))))

(deftest "unnamed rule-after is entered into ruleset name table"
    :test-fn
  (lambda () 
    (let ((r1 (make-instance 'rule :name 'current))
          (*ruleset* (make-instance 'ruleset :name 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue *ruleset* r1)
      (let ((*current-rule* (head *ruleset*)))
        (rule-after rule)
        (assert-equal rule (gethash (name rule) (elements *ruleset*)))))))

(deftest "rule-tail is entered into ruleset name table"
    :test-fn
  (lambda () 
    (let ((*current-rule* (make-instance 'rule :name 'r1))
          (r2 (make-instance 'rule :name 'r2))
          (*ruleset* (make-instance 'ruleset)))
      (enqueue *ruleset* *current-rule*)
      (rule-tail r2)
      (assert-equal r2 (gethash (name r2) (elements *ruleset*))))))

(deftest "contexts have names"
    :test-fn
  (lambda ()
    (let ((c (ensure-context)))
      (assert-non-nil (name c)))))

(deftest "contexts are added to context hash"
    :test-fn
  (lambda ()
    (let* ((c (ensure-context))
           (c2 (get-context (name c))))
      (assert-equal c c2 :test #'equal))))
      
(deftest "only one context with a given name"
    :test-fn
  (lambda ()
    (let* ((c1 (ensure-context))
           (c2 (ensure-context :name (name c1))))
      (eq c1 c2))))

(Deftest "default style rule matches message"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message :message "this is a message"))
          (rule (make-instance 'rule :match #'match-all)))
      (multiple-value-bind (matches sub-matches)
          (check-rule rule message)
        (declare (ignore sub-matches))
        (assert-non-nil matches)))))

(deftest "no exception works"
    :test-fn
  (lambda () 
    (let ((message (make-instance 'message :message "asdf"))
          (rule (make-instance 'rule
                               :match #'match-all
                               :no-match #'match-none)))
      (multiple-value-bind (matches sub-matches)
          (check-rule rule message)
        (declare (ignore sub-matches))
        (assert-non-nil matches)))))


(deftest "ruleset rule insertion works"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue ruleset rule)
      (eq (data (head ruleset)) rule))))

(deftest "ruleset match works"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match #'match-all))
          (rule (make-instance 'rule :match #'match-all))
          (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule)
      (check-rule ruleset message))))

(deftest "empty ruleset does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match #'match-all))
          (message (make-instance 'message :message "test message")))
      (not (check-rule ruleset message)))))

(deftest "matching ruleset with non-matching rules does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match #'match-all))
          (message (make-instance 'message :message "test message"))
          (r1 (make-instance 'rule :match #'match-none)))
      (enqueue ruleset r1)
      (not (check-rule ruleset message)))))

(deftest "matching ruleset with matching rule does match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match #'match-all))
          (message (make-instance 'message :message "test message"))
          (r1 (make-instance 'rule :match #'match-all)))
      (enqueue ruleset r1)
      (check-rule ruleset message))))

(deftest "ruleset with matching no-match does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset 
                                  :match #'match-all
                                  :no-match #'match-all))
          (message (make-instance 'message))
          (r1 (make-instance 'rule :match #'match-all)))
      (enqueue ruleset r1)
      (not 
       (check-rule ruleset message)))))


(deftest "message is added to context"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message))
          (context (ensure-context)))
      (and
       (equal (Ecount context) 0)
       (add-item context message)
       (equal (Ecount context) 1)))))

(deftest "removing a rule from the ruleset removes it from elements"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue ruleset rule)
      (dll-delete ruleset rule)
      (assert-nil (gethash (name rule) (elements ruleset))))))

(deftest "removing a rule from the ruleset removes it from list-entries"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue ruleset rule)
      (dll-delete ruleset rule)
      (assert-nil (gethash rule (list-entries ruleset))))))

(deftest "check-rules finds simple match"
    :test-fn
  (lambda () 
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :match #'match-all))
          (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule)
      (check-rules message ruleset))))

(deftest "check-rules finds simple regexp match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :match (lambda (message) (cl-ppcre::scan "test.*" (message message)))))
          (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule)
      (check-rules message ruleset))))

(deftest "check-rules doesn't find simple non-match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :match #'match-none))
          (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule)
      (not (check-rules message ruleset)))))

(deftest "check-rules finds match in second rule"
    :test-fn
  (lambda ()
    (let* ((ruleset (make-instance 'ruleset))
           (xyzzy 0) ; a variable whose value we change in the action
           (rule (make-instance 'rule :match #'match-none))
           (rule2 (make-instance 'rule 
                                 :match #'match-all
                                 :actions
                                 (list
                                  (lambda (message)
                                    (declare (ignore message))
                                    (setf xyzzy 42)))))
           (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule)
      (enqueue ruleset rule2)
      (and (check-rules message ruleset)
           (assert-equal 42 xyzzy)))))

(deftest "check-rules doesn't find match in several non-matching rules"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule1 (make-instance 'rule :match #'match-none))
          (rule2 (make-instance 'rule :match #'match-none))
          (rule3 (make-instance 'rule :match #'match-none))
          (message (make-instance 'message :message "test message")))
      (enqueue ruleset rule1)
      (enqueue ruleset rule2)
      (enqueue ruleset rule3)
      (not (check-rules message ruleset)))))
          
          
(deftest "message is eq to same message added to context"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message))
          (context (ensure-context)))
      (add-item context message)
      (eq message (aref (data context) 0)))))

(deftest "dll-delete removes a rule from the ruleset"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule)))
      (enqueue ruleset rule)
      (dll-delete ruleset rule)
      (not (head ruleset)))))

(deftest "rule with timeout can be inserted"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :timeout 1)))
      (enqueue ruleset rule)
      (assert-equal rule (data (head ruleset))))))
          
     

(deftest "rule that has exceeded its limits is found to be so"
    :test-fn
  (lambda ()
    (let ((rule (make-instance 'rule :timeout 1)))
      (rule-exceeded-limit-p rule (get-universal-time)))))

(deftest "rule that has timed out is removed from the ruleset"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :timeout 1)))
      (setf *now* 42)
      (enqueue ruleset rule)
      (check-limits *timeout-object-timeout-queue*)
      (or (null (head ruleset))
          (dead-p (data (head ruleset)))))))

(deftest "timed out rule is removed, others are not"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
            (make-instance 'priority-queue 
                           :comparison-function 
                           (lambda (x y) 
                             (> (timeout x) 
                                (timeout y))))))
      (let ((ruleset (make-instance 'ruleset))
            (r1 (make-instance 'rule))
            (r2 (make-instance 'rule :timeout 1))
            (r3 (make-instance 'rule :timeout 5000)))
        (enqueue ruleset r1)
        (enqueue ruleset r2)
        (enqueue ruleset r3)
        (setf *now* 42)
        (check-limits *timeout-object-timeout-queue*) ; find and remove old rules
        ;; make sure old rule is gone and new is not
        (and
         (dead-p r2)
         (assert-non-nil 
          (and (gethash r1 (list-entries ruleset))
               (gethash r3 (list-entries ruleset)))))))))


;;; XXX FIXME XXX
(deftest "new file-follower gets a line"
    :test-fn
  (lambda ()
    (let* ((testfile "/tmp/testfile")
           (ff ()))
      ;; open the file the first time. overwrite it if it exists.
      (with-open-file (output-stream testfile :direction :output 
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
        ;; put a line into it.
        (format output-stream "this is a line~%"))

      ;; open the filefollower for input
      (setf ff (make-instance 'file-follower :filename testfile))

      (let ((line (get-line ff)))
        ;; kill of the temp file
        (unix:unix-unlink testfile)
        (assert-non-nil line)))))

(deftest "file follower returns nil with no input"
    :test-fn
  (lambda () 
    (let ((ff (make-instance 'file-follower :filename "/dev/null")))
      (assert-nil (get-line ff)))))

(deftest "file follower addition works"
    :test-fn
  (lambda ()
    (let* ((testfile "/tmp/testfile")
           (ff ()))
      ;; open the file the first time. overwrite it if it exists.
      (with-open-file (output-stream testfile :direction :output 
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
        ;; put a line into it.
        (format output-stream "this is a line~%"))

      ;; open the filefollower for input
      (setf ff (make-instance 'file-follower :filename testfile))
      ;; read the "this is a line line"
      (assert-non-nil (get-line ff))
      ;; put another line in the file
      (with-open-file (output-stream testfile :direction :output 
                                     :if-exists :append
                                     :if-does-not-exist :create)
        (not (format output-stream "here is another~%")))

      (let ((line (get-line ff)))
        (unix:unix-unlink testfile)
        (assert-non-nil line)))))

(deftest "file-follower inode rollover works"
    :test-fn
  (lambda ()
    (let* ((testfile "testfileaxxx"))
      (with-open-file (output-stream testfile :direction :output 
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
        (format output-stream "this is a line"))

      (let ((ff (make-instance 'file-follower :filename testfile)))
        (assert-non-nil
         (and
          (get-line ff)
          (not (get-line ff))))
        (with-open-file (output-stream testfile :direction :output 
                                       :if-exists :rename-and-delete
                                       :if-does-not-exist :create)
          (format output-stream "here is another~%"))
        (let ((line (get-line ff)))
          (unix:unix-unlink testfile)
          (assert-non-nil line)
           )))))

(deftest "file follower makes correct series of messages"
    :test-fn
  (lambda ()
    (let* ((testfile "/tmp/testfile2")
           (file-lines '("this is a line" "this is another" "this is the third" "yet another")))
      (with-open-file (output-stream testfile :direction :output 
                                     :if-exists :overwrite
                                     :if-does-not-exist :create)
        (mapcar (lambda (x) (format output-stream "~A~%" x)) file-lines))
      (let* ((ff (make-instance 'file-follower :filename testfile))
             (messages 
              (mapcar 
               (lambda (x) 
                 (declare (ignore x))
                 (get-logline ff)) 
               file-lines)))
        (let ((result
               (loop as q in 
                    (mapcar (lambda (x y) (equal X (message Y)))
                            file-lines messages)
                  do
                    (or q (return ()))
                  finally
                    (return t))))
          (unix:unix-unlink testfile)
          (assert-non-nil result))))))

(deftest "get-rule returns the rule"
    :test-fn
  (lambda ()
    (let ((rule (make-instance 'rule))
          (ruleset (make-instance 'ruleset)))
      (enqueue ruleset rule)
      (assert-equal rule (get-rule ruleset (name rule))))))
      

(deftest "contexts that have expired are shown to exceed limit"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :name 'test-context :timeout 47))
          (now 48))
      (context-exceeded-limit-p context now))))

(deftest "contexts that don't have timeouts don't exceed limits"
    :test-fn
  (lambda ()
    (let ((context (ensure-context))
          (now 48))
      (assert-nil (context-exceeded-limit-p context now)))))

(deftest "contexts that have expired are removed from hash with remove-context-if-stale"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :timeout 47))
          (now 48))
      (remove-context-if-stale context now)
      (assert-nil (get-context (name context))))))

(deftest "contexts that haven't expired are not removed from hash"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :timeout 49))
          (now 48))
      (remove-context-if-stale context now)
      (assert-non-nil (get-context (name context))))))
        
(deftest "get-context returns proper context"
    :test-fn
  (lambda ()
    (let ((context (ensure-context)))
      (assert-equal 
       context
       (get-context (name context))))))

(deftest "context is added to *contexts* list when created"
    :test-fn
  (lambda ()
    (let ((context (ensure-context)))
      (assert-non-nil (gethash context (list-entries *contexts*))))))

(deftest "context is removed from *contexts* list when deleted"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :timeout 47)))
      (and
       (gethash context (list-entries *contexts*)))
       (remove-context-if-stale context 1)
       t)))

(deftest "old context exceeds limits"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :timeout 47))
          (now 48))
      (context-exceeded-limit-p context now))))

(deftest "full context exceeds limits"
    :test-fn
  (lambda ()
    (let ((context (ensure-context :name 'context :max-lines 1)))
      (add-to-context 'context (make-instance 'message :message "this is a line"))
      (add-to-context 'context (make-instance 'message :message "this is a line"))
      (assert-non-nil (context-exceeded-limit-p context 42)))))
      
(deftest "full context runs actions"
    :test-fn
  (lambda ()
    (let* ((fooble ())
           (context (ensure-context
                                   :max-lines 1
                                   :actions
                                   (list
                                    (lambda (context)
                                      (declare (ignore context))
                                      (setf fooble t))))))
      (add-to-context (name context) (make-instance 'message))
      ;; context should exceed limit here and run actions
      (add-to-context (name context) (make-instance 'message))
      (assert-non-nil fooble))))

(deftest "deleted context is removed from *contexts* list"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout  x)
                               (timeout  y))))))
      (let ((context (ensure-context :timeout 0))
            (*now* 1))
        (declare (ignore context))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (check-limits *timeout-object-timeout-queue*)
        (assert-nil (head *timeout-object-timeout-queue*))))))

(deftest "deleted context is removed from *contexts* list when pq is searched"
    :test-fn
  (lambda ()
    (let ((*contexts* (make-instance 'doubly-linked-list))
          (*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let* ((*now* 42)
             (context (ensure-context :timeout (- *now* 1))))
        (declare (ignore context))
        (assert-non-nil (head *contexts*))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (check-limits *timeout-object-timeout-queue*)
        (assert-nil (head *contexts*))
        (assert-nil (head *timeout-object-timeout-queue*))))))

(deftest "deleted context runs actions"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let* ((*fooble* ())
             (*now* 2)
             (context (ensure-context :timeout 1
                                     :actions
                                     (list
                                      (lambda (context)
                                        (declare (ignore context))
                                        (setf *fooble* t))))))

        (assert-non-nil (context-exceeded-limit-p context *now*))
        (check-limits *timeout-object-timeout-queue*)
        (assert-non-nil *fooble*)))))

(deftest "nested rulesets are run"
    :test-fn
  (lambda ()
    
    (let* ((thing ())
           (ruleset1 (make-instance 'ruleset :match #'match-all))
           (ruleset2 (make-instance 'ruleset :match #'match-all))
           (rule (make-instance 'rule 
                                :match #'match-all
                                :actions 
                                (list
                                 (lambda (message)
                                   (declare (ignore message))
                                   (progn 
                                     (setf thing t))))))
           (message (make-instance 'message :message "test message")))
      (enqueue ruleset2 rule)
      (enqueue ruleset1 rule)
      (check-rule ruleset1 message)
      (assert-non-nil thing))))


(deftest "insert after inserts after in doubly-linked-list"
    :test-fn
  (lambda ()
    (let ((lst (make-instance 'doubly-linked-list))
          (item1 (make-instance 'rule))
          (item2 (make-instance 'rule)))
      (enqueue lst item1)
      (dll-insert lst (head lst) item2 :direction :after)
      (assert-equal item1 (data (head lst)))
      (assert-equal item2 (data (rlink (head lst)))))))

(deftest "insert before inserts before in doubly-linked-list"
    :test-fn
  (lambda ()
    (let ((lst (make-instance 'doubly-linked-list))
          (item1 (make-instance 'rule))
          (item2 (make-instance 'rule)))
      (enqueue lst item1)
      (dll-insert lst (head lst) item2 :direction :before)
      (assert-equal item2 (data (head lst)))
      (assert-equal item1 (data (rlink (head lst)))))))

(deftest "rule with timeout is inserted into priority queue"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
            (make-instance 'priority-queue 
                           :comparison-function 
                           (lambda (x y) 
                             (> (timeout x)
                                (timeout y))))))
      (let ((rule (make-instance 'rule :timeout 1)))
        (declare (ignore rule))
        (assert-non-nil (head *timeout-object-timeout-queue*))))))

(deftest "context with timeout is inserted into priority queue"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((context (ensure-context :timeout 1)))
        (declare (ignore context))
        (assert-non-nil (head *timeout-object-timeout-queue*))))))

(deftest "item is entered into priority queue's list-entries"
    :test-fn
  (lambda ()
    (let ((pq (make-instance 'priority-queue)))
      (enqueue pq 42)
      (assert-non-nil (gethash 42 (list-entries pq))))))

(deftest "timed-out rule is removed from priority queue"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((rule (make-instance 'rule :timeout 1)))
        (declare (ignore rule))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (check-limits *timeout-object-timeout-queue*)
        (assert-nil (head *timeout-object-timeout-queue*))))))

(deftest "timed-out-rule is removed from pq hash"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((rule (make-instance 'rule :timeout 1))
            (*now* 42))
        (check-limits *timeout-object-timeout-queue*)
        (assert-nil (gethash rule (list-entries *timeout-object-timeout-queue*)))))))

(deftest "contexts end up in right order in priority queue"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((c3 (ensure-context :timeout 46))
            (c1 (ensure-context :timeout 42))
            (c2 (ensure-context :timeout 43)))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      c1)
        (assert-equal (data (rlink (head *timeout-object-timeout-queue*)))
                      c2)
        (assert-equal (data (rlink (rlink (head *timeout-object-timeout-queue*))))
                      c3)))
))

(deftest "rules end up in right order in priority queue"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((r3 (make-instance 'rule :timeout 46))
            (r1 (make-instance 'rule :timeout 42))
            (r2 (make-instance 'rule :timeout 43)))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      r1)
        (assert-equal (data (rlink (head *timeout-object-timeout-queue*)))
                      r2)
        (assert-equal (data (rlink (rlink (head *timeout-object-timeout-queue*))))
                      r3)))))

(deftest "context priority queue is updated"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((c1 (ensure-context :timeout 42))
            (c2 (ensure-context :timeout 43)))
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      c1)
        (setf (timeout c2) 40)
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      c2)))))

(deftest "rule priority queue is updated"
    :test-fn
  (lambda ()
    (let ((*timeout-object-timeout-queue*
           (make-instance 'priority-queue
                          :comparison-function
                          (lambda (x y)
                            (> (timeout x)
                               (timeout y))))))
      (let ((r1 (make-instance 'rule :timeout 42))
            (r2 (make-instance 'rule :timeout 43)))
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      r1)
        (setf (timeout r2) 40)
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      r2)))))

(deftest "nomatch negates match in rule"
    :test-fn
  (lambda ()
    (let ((dummy ()))
      (let ((rule (make-instance 'rule
                                 :match #'match-all
                                 :no-match #'match-all
                                 :actions
                                 (list
                                  (lambda (message matches sub-matches)
                                    (declare 
                                     (ignore message matches sub-matches))
                                    (setf dummy t)))))
            (message (make-instance 'message)))
        (check-rule rule message)
        (assert-nil dummy)))))

(deftest "delete-rule causes a rule to be deleted"
    :test-fn
  (lambda () 
    (let ((rule (make-instance 'rule
                               :match #'match-all
                               :delete-rule #'match-all))
          (*ruleset* (make-instance 'ruleset))
          (message (make-instance 'message)))
      (assert-nil (head *ruleset*))
      (enqueue *ruleset* rule)
      (check-rule rule message)
      (head *ruleset*)
      (or (not (head *ruleset*))
          (dead-p (data (head *ruleset*)))))))

(deftest "rule inside ruleset inside ruleset"
    :category "ruleset"
    :test-fn
    (lambda ()
      (let* ((*ruleset* (make-instance 'ruleset :match #'match-all))
             (r2 (make-instance 'ruleset :match #'match-all))
             (xyzzy 0)
             (message (make-instance 'message :message "test message"))
             (rule (make-instance 'rule :match #'match-all
                                  :actions 
                                  (list
                                   (lambda (message)
                                     (declare (ignore message))
                                     (setf xyzzy 42))))))
        (enqueue  r2 rule)
        (enqueue *ruleset* r2)
        (check-rules message *ruleset*)
        (assert-equal 42 xyzzy))))

(deftest "non-matching regexp rule works"
    :test-fn
  (lambda ()
    (let ((message 
           (make-instance 'message :message "this is a different message"))
          (rule (make-instance 
                 'rule
                 :match (lambda (message)
                          (cl-ppcre::scan "this is a message" 
                                          (message message))))))
      (not (check-rule rule message)))))

(deftest "exception works"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message :message "this is a message"))
          (rule (make-instance 'rule
                               :match (lambda (message) 
                                        (declare (ignore message))
                                        t)
                               :no-match (lambda (message) 
                                           (declare (ignore message))
                                           t))))
      (not (check-rule rule message)))))

(deftest "regexp rule works"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message :message "this is a message"))
          (rule (make-instance 
                 'rule
                 :match (lambda (message)
                          (cl-ppcre::scan "this is a message" 
                                          (message message))))))
      (assert-non-nil (check-rule rule message)))))

(deftest "anti-default style rule doesn't match message"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message :message "this is a message"))
          (rule (make-instance 'rule :match (lambda (message) 
                                              (declare (ignore message))
                                              ()))))
      (not (check-rule rule message)))))

(deftest "get-context returns alias instead of orig context"
    :test-fn
  (lambda ()
    (let ((c1 (ensure-context :name 'c1))
          (c2 (ensure-context :name 'c2)))
      (declare (ignore c1))
      (alias-context c2 'c1)
      (assert-equal c2 (get-context 'c1)))))

(deftest "delete-context deletes alias"
    :test-fn
  (lambda ()
    (let* ((*contexts-hash* (make-hash-table :test #'equal))
           (*contexts-alias-hash* (make-hash-table :test #'equal))
           (c1 (ensure-context :name 'c1))
           (c2 (ensure-context :name 'c2)))
      (alias-context c2 'c1)
      (delete-context 'c1)
      (assert-equal c1 (get-context 'c1)))))

(defun test-filter ()
    (let ((*messages* 
           (make-instance 
            'list-follower
            :message-list
            (list 
             "the"
             "cat"
             "ran")))
          (seen-the ())
          (seen-ran ())
          (seen-cat ())
          (*root-ruleset* 
           (make-instance 'ruleset))
          (rule
           (filter
            (lambda (message)
              (equal (message message) "cat")))))
      (enqueue *root-ruleset* rule)
      (enqueue *root-ruleset*
               (Single
                (lambda (message)
                  (equal (message message) "the"))
                (list
                 (lambda (message)
                   (declare (ignore message))
                   (setf seen-the t)))))
      (enqueue *root-ruleset*
               (Single
                (lambda (message)
                  (equal (message message) "ran"))
                (list
                 (lambda (message)
                   (declare (ignore message))
                   (setf seen-ran t)))))
      (enqueue *root-ruleset*
               (Single
                (lambda (message)
                  (equal (message message) "cat"))
                (list
                 (lambda (message)
                   (declare (ignore message))
                   (setf seen-cat t)))))
      (main)
      (assert-non-nil (and seen-the (not seen-cat) seen-ran))))

 (deftest "filter function works"
     :test-fn #'test-filter)

(defun test-suppress ()
  (let* ((*now* (get-internal-real-time))
         (*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list 
            "the"
            "cat"
            "ran"
            "the")))
         (seen-messages ())
         (save-messages (Single 
                         (lambda (message)
                           (declare (ignore message))
                           t)
                         (list
                          (lambda (message)
                            (setf seen-messages 
                                  (cons (message message)
                                        seen-messages))))))
         (rule (Suppress 
                (lambda (message)
                  (equal (message message) "the"))
                1)))
    
    (enqueue *root-ruleset* rule)
    (enqueue *root-ruleset* save-messages)
    (main)
    (assert-equal 
     seen-messages
     '("ran" "cat"))))

(defun test-suppress2 ()
  (let* ((*now* (get-internal-real-time))
         (*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list 
            "the"
            "cat"
            "ran"
            "the")))
         (seen-messages ())
         (foo (single (lambda (message) 
                        (declare (ignore message))
                        t) ()))
         (save-messages (Single 
                         (lambda (message)
                           (declare (ignore message))
                           t)
                         (list
                          (lambda (message)
                            (setf seen-messages 
                                  (cons (message message)
                                        seen-messages))))))
         (rule (Suppress 
                (lambda (message)
                  (equal (message message) "the"))
                0
                :name 'foo
                )))
    
    (enqueue *root-ruleset* rule)
    (enqueue *root-ruleset* save-messages)
    (enqueue *root-ruleset* foo)

    (loop as x from 1 to 3 do
          (let ((*message* (get-logline *messages*)))
            (check-rules *message* *root-ruleset*)
            (check-limits *timeout-object-timeout-queue*)
            ))

    (setf *now* (* 2 *now*))
    (check-limits *timeout-object-timeout-queue*)

    (let ((*message* (get-logline *messages*)))
      (check-rules *message* *root-ruleset*)) 

    (assert-equal 
     seen-messages
     '("the" "ran" "cat"))))
    

 (deftest "suppress function works"
     :test-fn #'test-suppress)

(deftest "suppress function stops after timeout"
    :test-fn #'test-suppress2)


(defun test-suppress-until ()
  (let* ((*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list 
            "the"
            "cat"
            "the"
            "ran"
            "the")))

         (suppress-until-rule
          (suppress-until
           (lambda (message)
             (equal (message message) "the"))
           (lambda (message)
             (equal (message message) "ran"))
           :name 'suppress-until-rule))

         (match-count 0)
         (catch-all-rule
          (make-instance 'rule
                         :match #'match-all
                         :name 'xxx
                         :actions
                         (list
                          (lambda (message)
                            (declare (ignore message))
                            (incf match-count))))))
    
    (enqueue *root-ruleset* suppress-until-rule)
    (enqueue *root-ruleset* catch-all-rule)
    
    (main)
    
    (assert-equal match-count 2)))

(deftest "test suppress-until"
    :test-fn #'test-suppress-until)


(defun test-single ()
  (let* ((*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list 
            "the"
            "cat"
            "ran"
            "the")))
         (match-count 0)
         (catch-all-rule
          (Single
           (lambda (message)
             (declare (ignore message))
             t)
           (list 
            (lambda (message)
              (declare (ignore message))
              (incf match-count))))))
    
    (enqueue *root-ruleset* catch-all-rule)
    (main)
    (assert-equal match-count 4)))

(deftest "test single rule type"
    :test-fn #'test-single)

(defun test-single-with-suppress ()
  (let* ((*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list 
            "the"
            "cat"
            "ran"
            "the")))
         (*now* 1)
         (count 0)
         (rule
          (single-with-suppress
           #'match-all
           (list
            (lambda ()
              (incf count)))
           1)))
    (enqueue *root-ruleset* rule)
    ; XXX 
    
     (loop as x from 1 to 3 do
          (let ((*message* (get-logline *messages*)))
            (check-rules *message* *root-ruleset*)
            (check-limits *timeout-object-timeout-queue*)
            ))

;;     (setf *now* (* 2 *now*))
;;     (check-limits *timeout-object-timeout-queue*)

;;     (let ((*message* (get-logline *messages*)))
;;       (check-rules *message* *root-ruleset*))
    
    count))


(defun test-pair-finding-match2 ()
  (let* ((*root-ruleset* (make-instance 'ruleset))
        (*messages* 
         (make-instance 
          'list-follower
          :message-list
          (list 
           "the"
           "cat"
           "ran"
           "the")))
        (count 0)
        (rule
         (pair
          
          ;; match1
          (lambda (message)
            (equal (message message) "the"))
          
          ;; actions1
          ()
          
          ;; match2
          (lambda (message)
            (equal (message message) "cat"))

          ;; actions2
          (list
           (lambda (message)
             (declare (ignore message))
             (incf count))))))

    (enqueue *root-ruleset* rule)
    (main)
    
    (assert-equal count 1)))


(deftest "test pair rule type"
    :test-fn #'test-pair-finding-match2)



;; 5 inserts:
;;
;; insert empty list
;; insert head of list
;; insert tail of list
;; insert between 2 elements


(deftest "insert empty list"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll (head dll) 42 :direction :before)
      (assert-equal 42 (data (head dll)))
      (assert-equal 42 (data (tail dll))))))

(deftest "insert head of list"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll (head dll) 42 :direction :before)
      (dll-insert dll (head dll) 43 :direction :before)
      (assert-equal 43 (data (head dll)))
      (assert-equal 42 (data (tail dll))))))

(deftest "insert head of list, rlink & llinks are right"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll (head dll) 42 :direction :before)
      (dll-insert dll (head dll) 43 :direction :before)
      (assert-equal 42 (data (rlink (head dll))))
      (assert-equal 43 (data (llink (tail dll)))))))
      
(deftest "insert tail of list"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll (head dll) 42 :direction :before)
      (dll-insert dll (tail dll) 43 :direction :after)
      (assert-equal 42 (data (head dll)))
      (assert-equal 43 (data (tail dll))))))

(deftest "insert tail of list, rlinks & llinks are right"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll (head dll) 42 :direction :before)
      (dll-insert dll (tail dll) 43 :direction :after)
      (assert-equal 43 (data (rlink (head dll))))
      (assert-equal 42 (data (llink (tail dll)))))))

(deftest "insert after head (before tail) of list"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      ;; head is 42
      (dll-insert dll (head dll) 42 :direction :before)
      ;; tail is 43
      (dll-insert dll (tail dll) 43 :direction :after)
      ;; insert between head and tail, after head
      (dll-insert dll (head dll) 44 :direction :after)
      (assert-equal 42 (data (head dll)))
      (assert-equal 43 (data (tail dll)))
      (assert-equal 44 (data (rlink (head dll))))
      (assert-equal 44 (data (llink (tail dll))))) 
      ))

(deftest "insert before tail (after head) of list"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      ;; head is 42
      (dll-insert dll (head dll) 42 :direction :before)
      ;; tail is 43
      (dll-insert dll (tail dll) 43 :direction :after)
      ;; insert between head and tail, before tail
      (dll-insert dll (tail dll) 44 :direction :before)
      (assert-equal 42 (data (head dll)))
      (assert-equal 43 (data (tail dll)))
      (assert-equal 44 (data (llink (tail dll)))))))
    
(run-all-tests)
