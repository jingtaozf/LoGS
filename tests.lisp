; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2004 James Earl Prewett

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

(setf *do-error* t)

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

(deftest "only item in linked list is its own rlink & llink"
    :test-fn
  (lambda ()
    (let ((dll (make-instance 'doubly-linked-list)))
      (dll-insert dll () 42 :direction :before)
      (and
       (assert-equal (head dll) (rlink (head dll)))
       (assert-equal (head dll) (llink (head dll)))))))

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
;      (format t "Name R1: ~A -- ~A~%" r1 (name r1))
;      (format t "Name R2: ~A -- ~A~%" r2 (name r2))
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
        (format t "xxx: ~A~%"  (gethash (data *current-rule*) (list-entries *ruleset*)))
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
        (format t "blah: ~A~%" (gethash (name rule) (elements *ruleset*)))
        (format t "thing: ~A~%" (gethash (name rule) (elements *ruleset*)))
        (assert-equal rule (gethash (name rule) (elements *ruleset*)))))))

(deftest "rule-tail is entered into ruleset name table"
    :test-fn
  (lambda () 
    (let ((*current-rule (make-instance 'rule :name 'r1))
          (r2 (make-instance 'rule :name 'r2))
          (*ruleset* (make-instance 'ruleset)))
      (enqueue *ruleset* *current-rule*)
      (rule-tail r2)
      (assert-equal r2 (gethash (name r2) (elements *ruleset*))))))

(deftest "contexts have names"
    :test-fn
  (lambda ()
    (let ((c (make-instance 'context)))
      (assert-non-nil (name c)))))

(deftest "contexts are added to context hash"
    :test-fn
  (lambda ()
    (let* ((c (make-instance 'context))
           (c2 (get-context (name c))))
      (assert-equal c c2 :test #'equal))))
      
(deftest "only one context with a given name"
    :test-fn
  (lambda ()
    (let* ((c1 (make-instance 'context))
           (c2 (make-instance 'context :name (name c1))))
      (eq c1 c2))))

(deftest "default style rule matches message"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message :message "this is a message"))
          (rule (make-instance 'rule :match (lambda (message) t))))
      (multiple-value-bind (matches sub-matches)
          (check-rule rule message)
        (assert-non-nil matches)))))

(deftest "no exception works"
    :test-fn
  (lambda () 
    (let ((message (make-instance 'message :message "asdf"))
          (rule (make-instance 'rule
                               :match (lambda (message) t)
                               :no-match (lambda (message) ()))))
      (multiple-value-bind (matches sub-matches)
          (check-rule rule message)
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
    (let ((ruleset (make-instance 'ruleset :match (lambda (message) t)))
          (rule (make-instance 'rule :match (lambda (message) t)))
          (message (make-instance 'message)))
      (enqueue ruleset rule)
      (check-rule ruleset message))))

(deftest "empty ruleset does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match (lambda (message) t)))
          (message (make-instance 'message)))
      (not (check-rule ruleset message))))) 

(deftest "matching ruleset with non-matching rules does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match (lambda (message) t)))
          (message (make-instance 'message))
          (r1 (make-instance 'rule :match (lambda (message) ()))))
      (enqueue ruleset r1)
      (not (check-rule ruleset message)))))

(deftest "matching ruleset with matching rule does match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match (lambda (message) t)))
          (message (make-instance 'message))
          (r1 (make-instance 'rule :match (lambda (message) t))))
      (enqueue ruleset r1)
      (check-rule ruleset message))))

(deftest "ruleset with matching no-match does not match"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset :match (lambda (message) t)
                                  :no-match (lambda (message) t)))
          (message (make-instance 'message))
          (r1 (make-instance 'rule :match (lambda (message) t))))
      (enqueue ruleset r1)
      (not 
       (check-rule ruleset message)))))


(deftest "message is added to context"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message))
          (context (make-instance 'context)))
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
      (format t "~A~%" (list-entries ruleset))
      (assert-nil (gethash rule (list-entries ruleset))))))

(deftest "check-rules finds simple match"
    :test-fn
  (lambda () 
    (let ((ruleset (make-instance 'ruleset))
          (rule (make-instance 'rule :match (lambda (message) t)))
          (message (make-instance 'message)))
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
          (rule (make-instance 'rule :match (lambda (message) ())))
          (message (make-instance 'message)))
      (enqueue ruleset rule)
      (not (check-rules message ruleset)))))

(deftest "check-rules finds match in second rule"
    :test-fn
  (lambda ()
    (let* ((ruleset (make-instance 'ruleset))
           (xyzzy 0)
           (rule (make-instance 'rule :match (lambda (message) ())))
           (rule2 (make-instance 'rule 
                                 :match (lambda (message) t)
                                 :actions
                                 (list
                                  (lambda (message matches sub-matches)
                                    (progn
                                    (format t "running rule~%")
                                    (setf xyzzy 42))))))
           (message (make-instance 'message)))
      (enqueue ruleset rule)
      (enqueue ruleset rule2)
      (and (check-rules message ruleset)
           (assert-equal 42 xyzzy)))))

(deftest "check-rules doesn't find match in several non-matching rules"
    :test-fn
  (lambda ()
    (let ((ruleset (make-instance 'ruleset))
          (rule1 (make-instance 'rule :match (lambda (message) ())))
          (rule2 (make-instance 'rule :match (lambda (message) ())))
          (rule3 (make-instance 'rule :match (lambda (message) ())))
          (message (make-instance 'message)))
      (enqueue ruleset rule1)
      (enqueue ruleset rule2)
      (enqueue ruleset rule3)
      (not (check-rules message ruleset)))))
          
          
(deftest "message is eq to same message added to context"
    :test-fn
  (lambda ()
    (let ((message (make-instance 'message))
          (context (make-instance 'context)))
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


(deftest "new file-follower gets a line"
    :test-fn
  (lambda ()
    (let ((ff (make-instance 'file-follower :filename "messages")))
      (assert-non-nil (get-line ff)))))


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
      (assert-non-nil (get-line ff)))))

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
        (assert-non-nil
         (get-line ff))))))

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
             (messages (mapcar (lambda (x) (get-logline ff)) file-lines)))
        (loop as q in 
              (mapcar (lambda (x y) (equal X (message Y)))
                      file-lines messages)
              do
              (or q (return ()))
              finally
              (return t))))))

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
    (let ((context (make-instance 'context :name 'test-context :timeout 47))
          (now 48))
      (context-exceeded-limit-p context now))))

(deftest "contexts that don't have timeouts don't exceed limits"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context))
          (now 48))
      (assert-nil (context-exceeded-limit-p context now)))))

(deftest "contexts that have expired are removed from hash with remove-context-if-stale"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context :timeout 47))
          (now 48))
      (remove-context-if-stale context now)
      (assert-nil (get-context (name context))))))

(deftest "contexts that haven't expired are not removed from hash"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context :timeout 49))
          (now 48))
      (remove-context-if-stale context now)
      (assert-non-nil (get-context (name context))))))
        
(deftest "get-context returns proper context"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context)))
      (assert-equal 
       context
       (get-context (name context))))))

(deftest "context is added to *contexts* list when created"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context)))
      (assert-non-nil (gethash context (list-entries *contexts*))))))

(deftest "context is removed from *contexts* list when deleted"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context :timeout 47)))
      (and
       (gethash context (list-entries *contexts*)))
       (remove-context-if-stale context 1)
       t)))

(deftest "old context exceeds limits"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context :timeout 47))
          (now 48))
      (context-exceeded-limit-p context now))))

(deftest "full context exceeds limits"
    :test-fn
  (lambda ()
    (let ((context (make-instance 'context :name 'context :max-lines 1)))
      (add-to-context 'context (make-instance 'message :message "this is a line"))
      (add-to-context 'context (make-instance 'message :message "this is a line"))
      (assert-non-nil (context-exceeded-limit-p context 42)))))
      
(deftest "full context runs actions"
    :test-fn
  (lambda ()
    (let* ((fooble ())
           (context (make-instance 'context
                                   :max-lines 1
                                   :actions
                                   (list
                                    (lambda (context)
                                      (progn
                                        (setf fooble t)))))))
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
      (let ((context (make-instance 'context :timeout 0))
            (now 1))
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
             (context (make-instance 'context :timeout (- *now* 1))))
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
             (context (make-instance 'context :timeout 1
                                     :actions
                                     (list
                                      (lambda (context)
                                        (progn
                                          (setf *fooble* t)
                                          (format t "running context action fooble: ~A~%" *fooble*)))))))
        (assert-non-nil (context-exceeded-limit-p context *now*))
        (check-limits *timeout-object-timeout-queue*)
        (assert-non-nil *fooble*)))))

(deftest "nested rulesets are run"
    :test-fn
  (lambda ()
    
    (let* ((thing ())
           (ruleset1 (make-instance 'ruleset :match (lambda (message) t)))
           (ruleset2 (make-instance 'ruleset :match (lambda (message) t)))
           (rule (make-instance 'rule 
                                :match (lambda (message) t)
                                :actions 
                                (list
                                 (lambda (message matches sub-matches) 
                                   (declare (ignore message 
                                                    matches 
                                                    sub-matches))
                                   (progn 
                                     (setf thing t))))))
           (message (make-instance 'message)))
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
      (assert-non-nil (enqueue lst item1))
      (assert-non-nil (dll-insert lst (head lst) item2 :direction :after))
      (assert-equal item1 (data (head lst)))
      (assert-equal item2 (data (rlink (head lst)))))))

(deftest "insert before inserts before in doubly-linked-list"
    :test-fn
  (lambda ()
    (let ((lst (make-instance 'doubly-linked-list))
          (item1 (make-instance 'rule))
          (item2 (make-instance 'rule)))
      (assert-non-nil (enqueue lst item1))
      (assert-non-nil (dll-insert lst (head lst) item2 :direction :before))
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
      (let ((context (make-instance 'context :timeout 1)))
        (assert-non-nil (head *timeout-object-timeout-queue*))))))

(deftest "item is entered into priority queue's list-entries"
    :test-fn
  (lambda ()
    (let ((pq (make-instance 'priority-queue)))
      (enqueue pq 42)
      (assert-non-nil (gethash 42 (list-entries pq))))))

;; this was making a context as the head of the rule timeout queue  WTF?
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
        ;; call the whatever to cause removal
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
      (let ((c3 (make-instance 'context :timeout 46))
            (c1 (make-instance 'context :timeout 42))
            (c2 (make-instance 'context :timeout 43)))
        (assert-non-nil (head *timeout-object-timeout-queue*))
        (assert-equal (data (head *timeout-object-timeout-queue*))
                      c1)
        (assert-equal (data (rlink (head *timeout-object-timeout-queue*)))
                      c2)
        (assert-equal (data (rlink (rlink (head *timeout-object-timeout-queue*))))
                      c3)))))

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
      (let ((c1 (make-instance 'context :timeout 42))
            (c2 (make-instance 'context :timeout 43)))
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
                                 :match (lambda (message) t)
                                 :no-match (lambda (message) t)
                                 :actions
                                 (list
                                  (lambda (message matches sub-matches)
                                    (setf dummy t)))))
            (message (make-instance 'message)))
        (check-rule rule message)
        (assert-nil dummy)))))

(deftest "delete-rule causes a rule to be deleted"
    :test-fn
  (lambda () 
    (let ((rule (make-instance 'rule
                               :match (lambda (message) t)
                               :delete-rule (lambda (message) t)))
          (*ruleset* (make-instance 'ruleset))
          (message (make-instance 'message)))
      (assert-nil (head *ruleset*))
      (enqueue *ruleset* rule)
      (check-rule rule message)
      (head *ruleset*)
      (or (not (head *ruleset*))
          (dead-p (data (head *ruleset*)))))))


(deftest "doubly-linked-list-item insert-between"
    :category "dlli"
    :test-fn
    (lambda () 
      (let ((item1 (make-instance 'doubly-linked-list-item))
            (item2 (make-instance 'doubly-linked-list-item))
            (item3 (make-instance 'doubly-linked-list-item)))
        (insert-between item2 item1 item3)
        (assert-equal item2 (rlink item1))
        (assert-equal item3 (rlink item2))
        (assert-equal item2 (llink item3))
        (assert-equal item1 (llink item2)))))

(deftest "doubly-linked-list-item insert-after"
    :category "dlli"
    :test-fn
    (lambda ()
      (let ((item1 (make-instance 'doubly-linked-list-item))
            (item2 (make-instance 'doubly-linked-list-item)))
        (insert-after item2 item1) ; insert item2 after item1
        (assert-equal item2 (rlink item1))
        (assert-equal item1 (rlink item2))
        (assert-equal item2 (llink item1))
        (assert-equal item1 (llink item2)))))

(deftest "doubly-linked-list-item insert-before"
    :category "dlli"
    :test-fn
    (lambda ()
      (let ((item1 (make-instance 'doubly-linked-list-item))
            (item2 (make-instance 'doubly-linked-list-item)))
        (insert-before item2 item1)
        (assert-equal item2 (llink item1))
        (assert-equal item1 (llink item2))
        (assert-equal item2 (rlink item1))
        (assert-equal item1 (rlink item2)))))

(deftest "doubly-linked-list-item remove-item"
    :category "dlli"
    :test-fn
    (lambda () 
      (let ((item1 (make-instance 'doubly-linked-list-item))
            (item2 (make-instance 'doubly-linked-list-item)))
        (insert-after item2 item1)
        (remove-item item2)
        (assert-equal item1 (llink item1))
        (assert-equal item1 (rlink item1)))))


(deftest "rule inside ruleset inside ruleset"
    :category "ruleset"
    :test-fn
    (lambda ()
      (let* ((*ruleset* (make-instance 'ruleset :match (lambda (x) t)))
             (r2 (make-instance 'ruleset :match (lambda (x) t)))
             (xyzzy 0)
             (message (make-instance 'message))
             (rule (make-instance 'rule :match (lambda (x) t)
                                  :actions 
                                  (list
                                   (lambda (a b c)
                                     (progn
                                       (format t "setting xyzzy~%")
                                       (setf xyzzy 42)))))))
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
                               :match (lambda (message) t)
                               :no-match (lambda (message) t))))
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
          (rule (make-instance 'rule :match (lambda (message) ()))))
      (not (check-rule rule message)))))


(deftest "get-context returns alias instead of orig context"
    :test-fn
  (lambda ()
    (let ((c1 (make-instance 'context :name 'c1))
          (c2 (make-instance 'context :name 'c2)))
      (alias-context c2 'c1)
      (assert-equal c2 (get-context 'c1)))))

(deftest "delete-context deletes alias"
    :test-fn
  (lambda ()
    (let* ((*contexts-hash* (make-hash-table :test #'equal))
           (*contexts-alias-hash* (make-hash-table :test #'equal))
           (c1 (make-instance 'context :name 'c1))
           (c2 (make-instance 'context :name 'c2)))
      (alias-context c2 'c1)
      (delete-context 'c1)
      (assert-equal c1 (get-context 'c1)))))

(run-all-tests)
