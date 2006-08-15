;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2006 James Earl Prewett

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


;;(load "CLUnit.lisp")

(in-package :org.prewett.LoGS)
(use-package :ORG.ANCAR.CLUNIT)

;; UGH!
#+(or ecl allegro clisp lispworks)
(set-dispatch-macro-character #\# #\$
                              (lambda (s c n)
                                (let ((thing (read s nil (values) t))) ())))

#+(or ecl allegro clisp lispworks)
(set-dispatch-macro-character #\# #\_
                              (lambda (s c n)
                                (let ((thing (read s nil (values) t))) ())))

;; this is /brain-dead/ 
;; it does /not/ do the right thing when filename.1 already exists!
;; if it rolls over, the new filename is returned, else nil
(defun roll-over (filename)
  (let ((newname (format () "~A.1" filename)))
    (when
        #+cmu
      (unix:unix-rename filename newname)
      #+(or clisp sbcl allegro openmcl lispworks)
      (rename-file filename newname)
      #-(or cmu clisp sbcl allegro openmcl lispworks)
      (error "unimplemented with this Lisp~%")
      newname)))

(defvar *do-error* t)

(defun remove-file (filename)
  #+cmu
  (unix:unix-unlink filename)
  #+sbcl
  (sb-posix:unlink filename)
  #+openmcl
  (#_unlink (ccl::make-cstring filename))
  #+(or clisp lispworks)
  (delete-file filename)
  #+allegro
  (excl.osi:unlink filename))

(defmacro with-temporary-file (stream filename open-forms closed-forms)
  `(unwind-protect
        (progn
          (with-open-file (,stream ,filename :direction :output 
                                   :if-exists :overwrite
                                   :if-does-not-exist :create)
            (progn
              ,@open-forms))
          (progn
            ,@closed-forms))
     (remove-file ,filename)))

(defmacro print-to-file (filename format &rest args)
  `(with-open-file (,stream ,filename :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
     (format t "filename: ~A~%" ,FILENAME)
     (format t ,format ,@args)))



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
(deftest "foo"
    :category 'dummy-test
    :test-fn (lambda () t))
;; linked list tests

(deftest "item inserted into a linked list with after and () neighbor is tail"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :after)
        (assert-equal
         (data (tail dll))
         42))))

(deftest "item inserted into a linked-list with before and () neighbor is head"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (assert-equal
         (data (head dll))
         42))))

(deftest "item inserted into linked list is inserted with item -> dlli"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (assert-non-nil (gethash 42 (list-entries dll)))
        (assert-equal 42 (data (gethash 42 (list-entries dll)))))))

(deftest "only item in linked list has no rlink or llink"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (and
         (assert-equal () (rlink (head dll)))
         (assert-equal () (llink (head dll)))))))

(deftest "only item in linked list is head and tail"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (assert-non-nil (head dll))
        (assert-equal (head dll) (tail dll)))))

(deftest "item removed from list is removed from list-entries"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (dll-delete dll 42)
        (assert-nil (gethash 42 (list-entries dll))))))
    


      
      
                                            

;; rule creation tests

(deftest "Rules always have names"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (assert-non-nil (name (make-instance 'rule)))))

(deftest "Rule name provided" 
    :category 'rule-tests
    :test-fn 
    (lambda () 
      (let* ((name "asdf")
             (name-provided-rule (make-instance 'rule :name name)))
        (assert-equal name (name name-provided-rule)))))

(deftest "Rule name not provided"
    :category 'rule-tests
    :test-fn
    (lambda () 
      (let ((nameless-rule (make-instance 'rule)))
        (assert-non-nil (name nameless-rule)))))

;; ruleset insertion tests
(deftest "ruleset insertion test"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let* ((rule (make-instance 'rule))
             (ruleset (make-instance 'ruleset)))
        (progn
          (enqueue ruleset rule)
          (assert-equal rule (gethash (name rule) (elements ruleset)) 
                        :test #'eq)))))

(deftest "rule that becomes head and tail is traversed"
    :category 'ruleset-tests
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
        (process-files)
        (assert-non-nil ran-tail-rule))))

(deftest "ruleset named rule insertion test"
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'rule-tests
    :test-fn
    (lambda () 
      (let ((r1 (make-instance 'rule :name 'a))
            (r2 (make-instance 'rule :name 'b)))
        (not (eq r1 r2)))))

(deftest "two different unnamed rules are not equal"
    :category 'rule-tests
    :test-fn
    (lambda () 
      (let ((r1 (make-instance 'rule))
            (r2 (make-instance 'rule)))
        (not (eq r1 r2)))))

(deftest "rule is entered into ruleset name table"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((rule (make-instance 'rule :name 'r1))
            (ruleset (make-instance 'ruleset :name 'ruleset)))
        (enqueue ruleset rule)
        (assert-equal rule (gethash (name rule) (elements ruleset))))))

(deftest "unnamed rule is entered into ruleset name table"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((rule (make-instance 'rule))
            (ruleset (make-instance 'ruleset :name 'ruleset)))
        (enqueue ruleset rule)
        (assert-equal rule (gethash (name rule) (elements ruleset))))))

(deftest "doubly linked list with an item has a head"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (enqueue dll 42)
        (assert-non-nil (head dll)))))

(deftest "rule-before is entered into ruleset name table"
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'ruleset-tests
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
    :category 'ruleset-tests
    :test-fn
    (lambda () 
      (let ((*current-rule* (make-instance 'rule :name 'r1))
            (r2 (make-instance 'rule :name 'r2))
            (*ruleset* (make-instance 'ruleset)))
        (enqueue *ruleset* *current-rule*)
        (rule-tail r2)
        (assert-equal r2 (gethash (name r2) (elements *ruleset*))))))

(deftest "contexts have names"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((c (ensure-context)))
        (assert-non-nil (name c)))))

(deftest "contexts are added to context hash"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((c (ensure-context))
             (c2 (get-context (name c))))
        (assert-equal c c2 :test #'equal))))
      
(deftest "only one context with a given name"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((c1 (ensure-context))
             (c2 (ensure-context :name (name c1))))
        (eq c1 c2))))

(Deftest "default style rule matches message"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 'rule :match #'match-all)))
        (multiple-value-bind (matches sub-matches)
            (check-rule rule message)
          (declare (ignore sub-matches))
          (assert-non-nil matches)))))

(deftest "no exception works"
    :category 'rule-tests
    :test-fn
    (lambda () 
      (let ((message (make-instance 'message :message "asdf"))
            (rule (make-instance 'rule
                                 :match #'match-all)))
        (multiple-value-bind (matches sub-matches)
            (check-rule rule message)
          (declare (ignore sub-matches))
          (assert-non-nil matches)))))


(deftest "ruleset rule insertion works"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule)))
        (enqueue ruleset rule)
        (eq (data (head ruleset)) rule))))

(deftest "ruleset match works"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset :match #'match-all))
            (rule (make-instance 'rule :match #'match-all))
            (message (make-instance 'message :message "test message")))
        (enqueue ruleset rule)
        (check-rule ruleset message))))

(deftest "empty ruleset does not match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset :match #'match-all))
            (message (make-instance 'message :message "test message"))
            (retval ()))
        (setf retval (not (check-rule ruleset message)))
        retval)))

(deftest "matching ruleset with non-matching rules does not match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset :match #'match-all))
            (message (make-instance 'message :message "test message"))
            (r1 (make-instance 'rule :match #'match-none)))
        (enqueue ruleset r1)
        (not (check-rule ruleset message)))))

(deftest "matching ruleset with matching rule does match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset :match #'match-all))
            (message (make-instance 'message :message "test message"))
            (r1 (make-instance 'rule :match #'match-all)))
        (enqueue ruleset r1)
        (check-rule ruleset message))))

(deftest "ruleset with matching no-match does not match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset 
                                    :match (lambda (message)
					     (and (match-all message)
						  (not (match-all message))))))
            (message (make-instance 'message))
            (r1 (make-instance 'rule :match #'match-all)))
        (enqueue ruleset r1)
        (not 				; is this a fair test?
         (check-rule ruleset message)))))


(deftest "message is added to context"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((message (make-instance 'message))
            (context (ensure-context)))
        (and
         (equal (Ecount context) 0)
         (add-to-context context message)
         (equal (Ecount context) 1)))))

(deftest "removing a rule from the ruleset removes it from elements"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule)))
        (enqueue ruleset rule)
        (dll-delete ruleset rule)
        (assert-nil (gethash (name rule) (elements ruleset))))))

(deftest "removing a rule from the ruleset removes it from list-entries"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule)))
        (enqueue ruleset rule)
        (dll-delete ruleset rule)
        (assert-nil (gethash rule (list-entries ruleset))))))

(deftest "check-rules finds simple match"
    :category 'rule-tests
    :test-fn
    (lambda () 
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule :match #'match-all))
            (message (make-instance 'message :message "test message")))
        (enqueue ruleset rule)
        (check-rules message ruleset))))

(deftest "check-rules finds simple regexp match"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule :match (lambda (message) (cl-ppcre::scan "test.*" (message message)))))
            (message (make-instance 'message :message "test message")))
        (enqueue ruleset rule)
        (check-rules message ruleset))))

(deftest "check-rules doesn't find simple non-match"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule :match #'match-none))
            (message (make-instance 'message :message "test message")))
        (enqueue ruleset rule)
        (not (check-rules message ruleset)))))

(deftest "check-rules finds match in second rule"
    :category 'rule-tests
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
    :category 'rule-tests
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
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((message (make-instance 'message))
            (context (ensure-context)))
        (add-item context message)
        (eq message (aref (data context) 0)))))

(deftest "dll-delete removes a rule from the ruleset"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule)))
        (enqueue ruleset rule)
        (dll-delete ruleset rule)
        (not (head ruleset)))))

(deftest "rule with timeout can be inserted"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((ruleset (make-instance 'ruleset))
            (rule (make-instance 'rule :timeout 1)))
        (enqueue ruleset rule)
        (assert-equal rule (data (head ruleset))))))
          
     

(deftest "rule that has exceeded its limits is found to be so"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((rule (make-instance 'rule :timeout 1)))
        (rule-exceeded-limit-p rule (get-universal-time)))))

(deftest "rule that has timed out is removed from the ruleset"
    :category 'rule-tests
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
    :category 'rule-tests
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
    :category 'file-follower-tests
    :test-fn
    (lambda ()
      (let* ((testfile "testfile-new-ff")
             ff)
        (with-temporary-file output-stream testfile
          ((format output-stream "this is a line~%"))
          ((setf ff (make-instance 'file-follower :filename testfile))
           (let ((line (get-line ff)))
             (close (filestream ff))
             (assert-non-nil line)))))))

(deftest "file follower returns nil with no input"
    :category 'file-follower-tests
    :test-fn
    (lambda () 
      (let ((ff (make-instance 'file-follower :filename "/dev/null")))
        (assert-nil (get-line ff)))))

(deftest "file follower addition works"
    :category 'file-follower-tests
    :test-fn
    (lambda ()
      (let* ((testfile "testfile-ff-addition")
             (ff ()))
        ;; open the file the first time. overwrite it if it exists.
        (with-open-file (output-stream testfile :direction :output 
                                       :if-exists :SUPERSEDE
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
          (close (filestream ff))
          (remove-file testfile)
          (assert-non-nil line)))))

(deftest "file-follower inode rollover works"
    :category 'file-follower-tests
    :test-fn
    (lambda ()
      (let* ((testfile "testfile-inode-rollover")
             (testfile-rollover ()))
        (with-open-file (output-stream testfile :direction :output 
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
          (format output-stream "this is a line"))

        (let ((ff (make-instance 'file-follower :filename testfile)))
          (assert-non-nil
           (and
            (get-line ff)
            (not (get-line ff))))

          (close (filestream ff)) 

          ;; ensure the rollover
          (setq testfile-rollover (roll-over (filename ff)))

          (with-open-file (output-stream testfile :direction :output 
                                         :if-exists :error
                                         :if-does-not-exist :create)
            (format output-stream "here is another~%"))

          (let ((line (get-line ff)))
            (close (filestream ff))
            (remove-file testfile)
            (when testfile-rollover (remove-file testfile-rollover))
            (assert-non-nil line)
            )))))

(deftest "PBS File-follower file rollover works"
    :category 'file-follower-tests
    :test-fn
    (lambda ()
      (let* ((day1 "20050227")
             (day2 "20050228")
             (day3 "20050301")
             (counter 0)
             (*messages* (make-instance 'pbs-file-follower :filename day1))
             (*root-ruleset* (make-instance 'ruleset)))
        (with-open-file (output-stream day1 :direction :output 
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
          (format output-stream "this is a line from day1"))
        (with-open-file (output-stream day2 :direction :output 
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
          (format output-stream "this is a line from day2"))
        (with-open-file (output-stream day3 :direction :output 
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
          (format output-stream "this is a line from day3"))

        (enqueue *root-ruleset*
                 (make-instance 'rule 
                                :match (lambda (x) 
                                         (declare (ignore x))
                                         t)
                                :actions (list (lambda (x) 
                                                 (declare (ignore x))
                                                 (incf counter)))))
        (process-files)
        (close (filestream *messages*))
        (remove-file day1)
        (remove-file day2)
        (remove-file day3)
        (assert-equal 3 counter))))

      

(deftest "file follower makes correct series of messages"
    :category 'file-follower-tests
    :test-fn
    (lambda ()
      (let* ((testfile "testfile-correct-series")
             (file-lines '("this is a line" "this is another" "this is the third" "yet another")))
        (with-temporary-file 
            output-stream testfile 
            ((mapcar (lambda (x) (format output-stream "~A~%" x)) file-lines))
            ((let* ((ff (make-instance 'file-follower :filename testfile))
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
                 (close (filestream ff))
            (assert-non-nil result))))))))

(deftest "get-rule returns the rule"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((rule (make-instance 'rule))
            (ruleset (make-instance 'ruleset)))
        (enqueue ruleset rule)
        (assert-equal rule (get-rule ruleset (name rule))))))
      

(deftest "contexts that have expired are shown to exceed limit"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :name 'test-context :timeout 47))
            (now 48))
        (context-exceeded-limit-p context now))))

(deftest "contexts that have expired their relative timeout are shown to exceed limit"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((*now* 48)
             (context (ensure-context :relative-timeout 1)))
        (context-exceeded-limit-p 
         context 
         (+ *now* 1
            (* INTERNAL-TIME-UNITS-PER-SECOND 1))))))

(deftest "contexts that don't have timeouts don't exceed limits"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context))
            (now 48))
        (assert-nil (context-exceeded-limit-p context now)))))

(deftest "contexts that have expired are removed from hash with remove-context-if-stale"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :timeout 47))
            (now 48))
        (remove-context-if-stale context now)
        (assert-nil (get-context (name context))))))

(deftest "contexts that haven't expired are not removed from hash"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :timeout 49))
            (now 48))
        (remove-context-if-stale context now)
        (assert-non-nil (get-context (name context))))))
        
(deftest "get-context returns proper context"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context)))
        (assert-equal 
         context
         (get-context (name context))))))

(deftest "context is added to *contexts* list when created"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context)))
        (assert-non-nil (gethash context (list-entries *contexts*))))))

(deftest "context is removed from *contexts* list when deleted"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :timeout 47)))
        (and
         (gethash context (list-entries *contexts*)))
        (remove-context-if-stale context 1)
        t)))

(deftest "old context exceeds limits"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :timeout 47))
            (now 48))
        (context-exceeded-limit-p context now))))

(deftest "full context exceeds limits"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((context (ensure-context :name 'context :max-lines 1)))
        (add-to-context 'context (make-instance 'message :message "this is a line"))
        (add-to-context 'context (make-instance 'message :message "this is a line"))
        (assert-non-nil (context-exceeded-limit-p context 42)))))
      
(deftest "full context runs actions"
    :category 'context-tests
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
        (check-limits context)
        ;; context should exceed limit here and run actions
        (add-to-context (name context) (make-instance 'message))
        (check-limits context)
        (assert-non-nil fooble))))

(deftest "context runs actions exactly once"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((fooble 0)
             (context (ensure-context
                       :max-lines 1
                       :actions
                       (list
                        (lambda (context)
                          (declare (ignore context))
                          (incf fooble))))))
        (add-to-context (name context) (make-instance 'message))
        (check-limits context)

        ;; context should exceed limit here and run actions
        (add-to-context (name context) (make-instance 'message))
        (check-limits context)
        (assert-equal fooble 1))))

(deftest "un-killable context runs actions exactly once"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((fooble 0)
             (context (ensure-context
                       :max-lines 1
                       :lives-after-timeout t
                       :actions
                       (list
                        (lambda (context)
                          (declare (ignore context))
                          (incf fooble))))))
        (add-to-context (name context) (make-instance 'message))
        (check-limits context)

        ;; context should exceed limit here and run actions
        (add-to-context (name context) (make-instance 'message))
        ;; calling check-limits here wouldn't be right!
        (assert-equal fooble 1))))

(deftest "timed-out context runs actions"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((fooble ())
             (context (ensure-context
                       :timeout 1
                       :actions
                       (list
                        (lambda (context)
                          (declare (ignore context))
                          (setf fooble t))))))
        (declare (ignore context))      ; we use it, just not here
        (let ((*now* 2))
          (check-limits *timeout-object-timeout-queue*)
          (assert-non-nil fooble)))))

(deftest "relative timed-out context runs actions"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((fooble ())
             (context (ensure-context
                       :timeout (+ *now* (* INTERNAL-TIME-UNITS-PER-SECOND 100))
                       :relative-timeout 1
                       :actions
                       (list
                        (lambda (context)
                          (declare (ignore context))
                          (setf fooble t))))))
        (let ((*now* (+ (next-timeout context) 1)))
          (check-limits *RELATIVE-TIMEOUT-OBJECT-TIMEOUT-QUEUE*)
          (assert-non-nil fooble)))))

(deftest "deleted context is removed from *contexts* list"
    :category 'context-tests
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
    :category 'context-tests
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
    :category 'context-tests
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
    :category 'ruleset-tests
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
    :category 'linked-list-tests
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
    :category 'linked-list-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
    :test-fn
    (lambda ()
      (let ((pq (make-instance 'priority-queue)))
        (enqueue pq 42)
        (assert-non-nil (gethash 42 (list-entries pq))))))

(deftest "timed-out rule is removed from priority queue"
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'priority-queue-tests
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
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((dummy ()))
        (let ((rule (make-instance 'rule
                                   :match (lambda (msg)
					    (if (and (match-all msg)
						     (not (match-all msg)))
						t
  					        'nil))
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
    :category 'rule-tests
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
    :category 'ruleset-tests
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
    :category 'rule-tests
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
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 'rule
                                 :match (lambda (message) 
                                          (declare (ignore message))
                                          t))))
        (check-rule rule message))))

(deftest "regexp rule works"
    :category 'rule-tests
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
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 'rule :match (lambda (message) 
                                                (declare (ignore message))
                                                ()))))
        (not (check-rule rule message)))))

(deftest "get-context returns alias instead of orig context"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let ((c1 (ensure-context :name 'c1))
            (c2 (ensure-context :name 'c2)))
        (declare (ignore c1))
        (alias-context c2 'c1)
        (assert-equal c2 (get-context 'c1)))))

(deftest "delete-context deletes alias"
    :category 'context-tests
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
    (process-files)
    (assert-non-nil (and seen-the (not seen-cat) seen-ran))))

(deftest "filter function works"
    :category 'rule-tests
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
    (process-files)
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
    :category 'rule-tests
    :test-fn #'test-suppress)

(deftest "suppress function stops after timeout"
    :category 'rule-tests
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
    
    (process-files)
    
    (assert-equal match-count 2)))

(deftest "test suppress-until"
    :category 'rule-tests
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
    (process-files)
    (assert-equal match-count 4)))

(deftest "test single rule type"
    :category 'rule-tests
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
    (process-files)
    
    (assert-equal count 1)))


(deftest "test pair rule type"
    :category 'rule-tests
    :test-fn #'test-pair-finding-match2)

;; 5 inserts:
;;
;; insert empty list
;; insert head of list
;; insert tail of list
;; insert between 2 elements


(deftest "insert empty list"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (assert-equal 42 (data (head dll)))
        (assert-equal 42 (data (tail dll))))))

(deftest "insert head of list"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (head dll) 43 :direction :before)
        (assert-equal 43 (data (head dll)))
        (assert-equal 42 (data (tail dll))))))

(deftest "insert head of list, rlink & llinks are right"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (head dll) 43 :direction :before)
        (assert-equal 42 (data (rlink (head dll))))
        (assert-equal 43 (data (llink (tail dll)))))))
      
(deftest "insert tail of list"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (tail dll) 43 :direction :after)
        (assert-equal 42 (data (head dll)))
        (assert-equal 43 (data (tail dll))))))

(deftest "insert tail of list, rlinks & llinks are right"
    :category 'linked-list-tests
    :test-fn
    (lambda ()
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (tail dll) 43 :direction :after)
        (assert-equal 43 (data (rlink (head dll))))
        (assert-equal 42 (data (llink (tail dll)))))))

(deftest "insert after head (before tail) of list"
    :category 'linked-list-tests
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
    :category 'linked-list-tests
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

(deftest "rule that exceeds relative-timeout is shown to have exceeded limits"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let* ((*now* 1)
             (rule (make-instance 'rule :relative-timeout 1
                                  :match (lambda (message)
                                           (declare (ignore message))
                                           t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (assert-non-nil (check-limits rule)))))

(deftest "rule that exceeds relative-timeout is marked as dead"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let* ((*now* 1)
             (rule (make-instance 'rule :relative-timeout 1
                                  :match (lambda (message)
                                           (declare (ignore message))
                                           t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (check-limits rule)
        (assert-non-nil (dead-p rule)))))

(deftest "rule relative-timeouts are updated on match"
    :category 'rule-tests
    :test-fn
    (lambda ()
      (let* ((*now* 1)
             (rule (make-instance 'rule :relative-timeout 1
                                  :match (lambda (message)
                                           (declare (ignore message))
                                           t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (check-rule rule (make-instance 'message))
        (assert-nil (check-limits rule)))))

(deftest "ruleset with relative timeout is updated on match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
        (enqueue ruleset (make-instance 'rule :match (lambda (message)
                                                       (declare (ignore message))
                                                       t)))
        (setf *now* 456)
        (check-rules (make-instance 'message) ruleset)
        (assert-non-nil (> (next-timeout ruleset) 456)))))
                                      
(deftest "ruleset with relative timeout is NOT update without match"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
        (enqueue ruleset (make-instance 'rule :match (lambda (message)
                                                       (declare (ignore message))
                                                       NIL)))
        (setf *now* (+ (next-timeout ruleset) 1))
        (check-rules (make-instance 'message) ruleset)
        (assert-nil (> (next-timeout ruleset) *now*)))))

(deftest "ruleset that exceeds relative timeout is shown to exceed limits"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
      
        (setf *now* (+ (next-timeout ruleset) 1))
        (assert-non-nil (check-limits ruleset)))))

(deftest "ruleset that exceeds relative timeout is marked as dead"
    :category 'ruleset-tests
    :test-fn
    (lambda ()
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
      
        (setf *now* (+ (next-timeout ruleset) 1))
        (check-limits ruleset)
        (assert-non-nil (dead-p ruleset)))))


(deftest "context that should live after timeout isnt destroyed"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((*now* 123)
             (context (make-instance 'context
                                     :name 'foop
                                     :timeout 124
                                     :lives-after-timeout t)))
        (setf *now* 125)
        (check-limits context)
        (let ((retval (assert-non-nil (get-context (name context)))))
          (expire-context context)
          retval))))

(deftest "context that shouldnt live after timeout is destroyed"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((*now* 123)
             (context (make-instance 'context
                                     :name 'foop
                                     :timeout 124
                                     :lives-after-timeout ())))
        (setf *now* 125)
        (assert-non-nil (check-limits context))
        (assert-nil (get-context (name context))))))

(deftest "min-lines context test"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((*now* 123)
             (testvar t)
             (context (make-instance 'context
                                     :timeout 124
                                     :min-lines 5
                                     :actions
                                     (list
                                      (lambda (context)
                                        (declare (ignore context))
                                        (setf testvar ()))))))
        (add-to-context context (make-instance 'message :message "test"))
        (check-limits context)
        (assert-non-nil testvar))))

(deftest "min-lines exceeded context test"
    :category 'context-tests
    :test-fn
    (lambda ()
      (let* ((*now* 123)
             (testvar ())
             (context (make-instance 'context
                                     :timeout 124
                                     :min-lines 5
                                     :actions
                                     (list
                                      (lambda (context)
                                        (declare (ignore context))
                                        (setf testvar t))))))
        (loop as x from 0 to 6 do 
             (add-to-context context 
                             (make-instance 'message 
                                            :message
                                            (format () "test message ~A~%" x))))
        (setf *now* 125)
        (check-limits context)
        (assert-non-nil testvar))))


;; some option tests

(deftest "--no-internal-time option test"
    :category 'cli-tests
    :test-fn
    (lambda ()
      (progn
        (setq org.prewett.LoGS::*use-internal-real-time* t) ;; make sure its in true state
        (process-options *opts* '("--no-internal-time"))
        (assert-nil org.prewett.LoGS::*use-internal-real-time*))))

(deftest "--file option test"
    :category 'cli-tests
    :test-fn
    (lambda ()
      (let ((*file-list* ()))
        (declare (special *file-list*))
        (process-command-line *opts* '("--file" "foo.txt"))
        (typep *messages* 'file-follower))))

(deftest "--file option with position test"
    :category 'cli-tests
    :test-fn
    (lambda ()
      (let* ((*file-list* ())
             (testfile "testfile-position-test")
             (file-lines '("this is a line" "this is another" "this is the third" "yet another")))
        (declare (special *file-list*))
        (with-open-file (output-stream testfile :direction :output 
                                       :if-exists :SUPERSEDE
                                       :if-does-not-exist :create)
          (mapcar (lambda (x) (format output-stream "~A~%" x)) file-lines))
      
      
        (process-command-line *opts* `("--file" ,testfile "42"))
        (and 
         (typep *messages* 'file-follower)
         (let ((ret (assert-equal 42 (file-position (filestream *messages*)))))
           (close (filestream *messages*))
           (remove-file testfile)
           ret)))))

(deftest "spawn works"
    :category 'cli-tests
    :test-fn
    (lambda ()
      (let* ((testfile "testfile-spawn")
             (file-lines '("this is a line" "this is another" "this is the third" "yet another")))
      
        ;; write the file
        (with-temporary-file output-stream testfile 
                             ((mapcar 
                               (lambda (x) 
                                 (format output-stream "~A~%" x)) 
                               file-lines))
                             ((let ((*messages* 
                                     (make-instance 'spawn
                                                    :spawnprog "/bin/cat"
                                                    :spawnargs (list
                                                                testfile)))
                                    (testval t))
                                ;; check to see that the output of the cat 
                                ;; process matches the stuff we wrote to the 
                                ;; file
                                (mapcar 
                                 (lambda (x) 
                                   (let ((message (get-logline *messages*)))
                                     (or (equal (message message) x)
                                         (setq testval ()))))
                                 file-lines)
                                (assert-non-nil testval)))))))


(deftest "old items are removed from window"
    :category 'window-tests
    :test-fn
    (lambda ()
      (let ((window (make-instance 'window :window-length 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (setf *now* (+ *now* (* 10 INTERNAL-TIME-UNITS-PER-SECOND)))
        (check-limits window)
        (assert-nil (head (data window))))))

(deftest "non-old items are not removed from window"
    :category 'window-tests
    :test-fn
    (lambda ()
      (let ((window (make-instance 'window :window-length 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (setf *now* (+ *now* (* 10 INTERNAL-TIME-UNITS-PER-SECOND)))
        (let ((save-message (make-instance 'message)))
          (add-item window save-message)
          (check-limits window)
          (and 
           (assert-equal (cadr (data (head (data window)))) save-message)
           (assert-nil (rlink (head (data window)))))))))

(deftest "window with too many non-old entries is shown to exceed limits"
    :category 'window-tests
    :test-fn
    (lambda ()
      (let ((window (make-instance 'window :max-lines 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (assert-non-nil (check-limits window)))))

;; (deftest "parse-function returns function"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (lambda (message) 
;;                                         (declare (ignore message))
;;                                         t)))
;;              (fn (parse-function parse-list))
;;             (message (make-instance 'message :message "some message")))
;;         (and (functionp fn)
;;              (funcall fn message)))))

(deftest "empty rule has no match"
    :category 'language-tests
    :test-fn
    (lambda ()
      (let ((rule (org.prewett.LoGS.language::rule)))
        (assert-nil (match rule)))))



(deftest "rule can set environment"
    :category 'language-tests
    :test-fn
    (lambda ()
      (let ((rule (org.prewett.LoGS.language::rule with foo = "one")))
        (assert-equal
         "one"
         (cadar (member 'foo (environment rule) :key #'car))))))

;; (deftest "parse-function returns remaining"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let ((parse-list (list (lambda (message)
;;                                (declare (ignore message))
;;                                t)
;;                          'a 'b 'c)))
;;         (multiple-value-bind (fn rest)
;;             (parse-function parse-list)
;;           (declare (ignore fn))
;;           (equal '(a b c) rest)))))

;; (deftest "parse-regexp returns matching function"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list "the cat ran"))
;;              (fn (parse-regexp parse-list))
;;              (message (make-instance 'message :message "the cat ran")))
;;         (and (functionp fn)
;;              (and
;;               (funcall fn message)
;;               t)))))

;; (deftest "parse-script returns matching function"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list "true"))
;;              (fn (parse-script parse-list))
;;              (message (make-instance 'message :message "doesn't matter")))
;;         (and (functionp fn)
;;              (funcall fn message)
;;              t))))

;; (deftest "parse-script returns remaining"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let ((parse-list (list "true"
;;                          'a 'b 'c)))
;;         (multiple-value-bind (fn rest)
;;             (parse-script parse-list)
;;           (and (functionp fn)
;;                (equal '(a b c) rest))))))

;; (deftest "parse-regexp returns remaining"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let ((parse-list (list "the cat ran"
;;                          'a 'b 'c)))
;;         (multiple-value-bind (fn rest)
;;             (parse-regexp parse-list)
;;           (and (functionp fn)
;;                (equal '(a b c) rest))))))

;; (deftest "parse-match regexp returns matching function"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'regexp "the cat ran"))
;;              (match-fn (parse-match parse-list))
;;              (message (make-instance 'message :message "the cat ran")))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "parse-match function returns matching function"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((the-function (lambda (message)
;;                                            (declare (ignore message))
;;                                            t))
;;              (parse-list (list 'function the-function))
;;              (message  (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (eql match-fn the-function)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "parse-match script returns matching script"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'script "true"))
;;              (message  (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "false script returns nil"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'script "false"))
;;              (message  (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; (deftest "true script returns t"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'script "true"))
;;              (message  (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))


;; (defun true-func (message)
;;   (declare (ignore message))
;;   t)

;; (defun false-func (message)
;;   (declare (ignore message))
;;   ())

;; (deftest "true function returns t"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'function #'true-func))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "false function returns ()"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list 'function #'false-func))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; (deftest "not of true function returns NIL"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'not 'function #'true-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; (deftest "not of false function returns t"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'not 'function #'false-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "or of 2 falses is false"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'or 
;;                                      'function #'false-func 
;;                                      'function #'false-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; (deftest "or of 2 trues is true"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list `((or 
;;                             function ,#'true-func 
;;                             function ,#'true-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "or of false and true is true"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'or 
;;                                      'function #'false-func 
;;                                      'function #'true-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "and of 2 trues is true"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'and
;;                                      'function #'true-func 
;;                                      'function #'true-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (funcall match-fn message)
;;          t))))

;; (deftest "and of true and false is false"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list (list (list 'and
;;                                      'function #'true-func 
;;                                      'function #'false-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; (deftest "and of 2 falses is false"
;;     :category 'logic-tests
;;     :test-fn
;;     (lambda ()
;;       (let* ((parse-list 
;;               `((and 
;;                  function ,#'false-func
;;                  function ,#'false-func)))
;;              (message (make-instance 'message))
;;              (match-fn (parse-match parse-list)))
;;         (and
;;          (functionp match-fn)
;;          (not (funcall match-fn message))
;;          t))))

;; a little slicker way of running all of the tests
(defun run-categories (&rest rest)
  (let ((passcount 0)
         (failcount 0))
     (mapcar
      (lambda (category)
        (progn
          (format t "running test category: ~A~%" category)
          (multiple-value-bind (all-passed failed passed)
              (run-category category)
            (declare (ignore all-passed))
            (incf passcount passed)
            (incf failcount failed))))
      rest)
     (format t "TOTALS: ~A ~[tests~;test~:;tests~] run; ~A ~[tests~;test~:;tests~] passed; ~A ~[tests~;test~:;tests~] failed.~%"
             (+ passcount failcount) (+ passcount failcount) passcount passcount failcount failcount)))

(defun run-all-categories ()
  (apply #'run-categories (org.ancar.clunit::list-test-categories)))

;; load other tests
(load "Language/rdl-tests.lisp")

(run-all-categories)
