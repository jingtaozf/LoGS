;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2007 James Earl Prewett

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

(in-package :org.prewett.LoGS.test)
(require 'lift)

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


;; master LoGS test suite
(deftestsuite LoGS () ())

;; dummy test
(deftestsuite dummy (LoGS)
  ())

(addtest (dummy)
  true-is-true
  (ensure t))

;; linked list tests

(deftestsuite linked-list (LoGS) () ())

(addtest (linked-list)
  lone-item-neighbor-is-tail
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :after)
        (ensure-same
         (data (tail dll))
         42)))

(addtest (linked-list)
  lone-item-neighhbor-is-head
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (ensure-same
         (data (head dll))
         42)))

(addtest (linked-list)
  item-added-to-list-entries
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (ensure-different NIL (gethash 42 (list-entries dll)))
        (ensure-same 42 (data (gethash 42 (list-entries dll))))))

(addtest (linked-list)
  lone-item-has-no-rlink
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (and
         (ensure-null (rlink (head dll))))))

(addtest (linked-list)
  lone-item-has-no-llink
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (and
         (ensure-null (llink (head dll))))))

(addtest (linked-list)
  lone-item-is-head-and-tail
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (and         
         (ensure-different NIL (head dll))
         (ensure-same (data (head dll)) 42)
         (ensure-same (head dll) (tail dll)))))

(addtest (linked-list)
  item-removed-is-removed-from-list-entries
  (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll () 42 :direction :before)
        (dll-delete dll 42)
        (ensure-null (gethash 42 (list-entries dll)))))

;; rule creation tests
(deftestsuite rule-tests (LoGS) () ())

(addtest (rule-tests)
  rules-always-have-names
  (and
   (ensure-different NIL (name (make-instance 'rule :name "foo")))
   (ensure-different NIL (name (make-instance 'rule)))))

(addtest (rule-tests)
  rule-name-provided
  (let* ((name "asdf")
         (name-provided-rule (make-instance 'rule :name name)))
    (ensure-same name (name name-provided-rule))))


(addtest (rule-tests)
  rule-name-not-provided
  (let ((nameless-rule (make-instance 'rule)))
    (ensure-different NIL (name nameless-rule))))

;; ruleset insertion tests
(deftestsuite ruleset-tests (LoGS) () ())

(addtest (ruleset-tests)
  rule-insertion-test
  (let* ((rule (make-instance 'rule))
         (ruleset (make-instance 'ruleset)))
    (progn
      (enqueue ruleset rule)
      (ensure-same rule (gethash (name rule) (elements ruleset)) 
                   :test #'eq))))

(addtest (ruleset-tests)
  head-and-tail-is-traversed
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
                                        (lambda (message environment)
                                          (declare (ignore message environment))
                                          (setf ran-tail-rule t))))))
      
      
      
        (enqueue *root-ruleset* dying-rule)
        (enqueue *root-ruleset* tail-rule)
        (process-files)
        (ensure-different NIL ran-tail-rule))
)

(addtest (ruleset-tests) 
  ruleset-named-rule-insertion
  (let* ((name "fooble")
             (rule (make-instance 'rule :name name))
             (ruleset (make-instance 'ruleset)))
        (progn
          (enqueue ruleset rule)
          (ensure-same rule (gethash name (elements ruleset)) 
                       :test #'eq))))

(addtest (ruleset-tests)
  retrieving-arbitrary-rules-from-rulesets
  (let* ((rule1 (make-instance 'rule :name 'rule1))
         (rule2 (make-instance 'rule :name 'rule2))
         (ruleset (make-instance 'ruleset :name "ruleset")))
    (enqueue ruleset rule1)
    (enqueue ruleset rule2)
    (and
     (ensure-same (gethash 'rule2 (elements ruleset)) rule2)
     (ensure-same (gethash 'rule1 (elements ruleset)) rule1))))

(addtest (ruleset-tests)
  retrieving-arbitrary-rulesets-from-rulesets
  (let* ((ruleset1 (make-instance 'ruleset :name 'ruleset1))
             (ruleset2 (make-instance 'ruleset :name 'ruleset2))
             (ruleset (make-instance 'ruleset :name "ruleset")))
        (enqueue ruleset ruleset1)
        (enqueue ruleset ruleset2)
        (and
         (eq (gethash 'ruleset2 (elements ruleset)) ruleset2)
         (eq (gethash 'ruleset1 (elements ruleset)) ruleset1))))

(addtest (ruleset-tests)
  retreiving-arbitrary-unnamed-rulesets
      (let* ((ruleset1 (make-instance 'ruleset))
             (ruleset2 (make-instance 'ruleset))
             (ruleset (make-instance 'ruleset :name "ruleset")))
        (enqueue ruleset ruleset1)
        (enqueue ruleset ruleset2)
        (and
         (eq (gethash (name ruleset2) (elements ruleset)) ruleset2)
         (eq (gethash (name ruleset1) (elements ruleset)) ruleset1))))

(addtest (rule-tests)
  two-different-named-rules-are-not-equal
  (let ((r1 (make-instance 'rule :name 'a))
        (r2 (make-instance 'rule :name 'b)))
    (not (eq r1 r2))))

(addtest (rule-tests)
  two-different-unnamed-rules-are-not-equal
  (let ((r1 (make-instance 'rule))
        (r2 (make-instance 'rule)))
    (not (eq r1 r2))))

(addtest (ruleset-tests)
  rule-is-entered-into-ruleset-name-table
  (let ((rule (make-instance 'rule :name 'r1))
        (ruleset (make-instance 'ruleset :name 'ruleset)))
    (enqueue ruleset rule)
    (assert-equal rule (gethash (name rule) (elements ruleset)))))

(addtest (ruleset-tests)
  unnamed-rule-is-entered-into-ruleset-name-table
  (let ((rule (make-instance 'rule))
        (ruleset (make-instance 'ruleset :name 'ruleset)))
    (enqueue ruleset rule)
    (assert-equal rule (gethash (name rule) (elements ruleset)))))

(addtest (linked-list)
  dll-with-an-item-has-a-head
  (let ((dll (make-instance 'doubly-linked-list)))
    (enqueue dll 42)
    (ensure-different NIL (head dll))))

(addtest (ruleset-tests)
  rule-before-is-entered-into-ruleset-name-table
      (let ((r1 (make-instance 'rule :name "r1"))
            (r2 (make-instance 'rule :name "r2"))
            (*ruleset* (make-instance 'ruleset :name 'ruleset)))
        (enqueue *ruleset* r1)
        (let ((*current-rule* (head *ruleset*)))
          (rule-before r2)
          (ensure-same r2 (gethash (name r2) (elements *ruleset*))))))

(addtest (ruleset-tests)
  unnamed-rule-before-is-entered-into-ruleset-name-table
  (let ((r1 (make-instance 'rule :name 'current))
        (*ruleset* (make-instance 'ruleset :name 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue *ruleset* r1)
    (let ((*current-rule* (head *ruleset*)))
      (rule-before rule)
      (ensure-same rule (gethash (name rule) (elements *ruleset*))))))

(addtest (ruleset-tests)
  rule-after-is-entered-into-ruleset-name-table
  (let ((r1 (make-instance 'rule :name 'r1))
        (r2 (make-instance 'rule :name 'r2))
        (*ruleset* (make-instance 'ruleset)))
    (enqueue *ruleset* r1)
    (let ((*current-rule* (head *ruleset*)))
      (rule-after r2)
      (ensure-same r2 (gethash (name r2) (elements *ruleset*))))))

(addtest (ruleset-tests)
  unnamed-rule-after-is-entered-into-ruleset-name-table
  (let ((r1 (make-instance 'rule :name 'current))
        (*ruleset* (make-instance 'ruleset :name 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue *ruleset* r1)
    (let ((*current-rule* (head *ruleset*)))
      (rule-after rule)
      (ensure-same rule (gethash (name rule) (elements *ruleset*))))))

(addtest (ruleset-tests)
  rule-tail-is-entered-into-ruleset-name-table
  (let ((*current-rule* (make-instance 'rule :name 'r1))
        (r2 (make-instance 'rule :name 'r2))
        (*ruleset* (make-instance 'ruleset)))
    (enqueue *ruleset* *current-rule*)
    (rule-tail r2)
    (ensure-same r2 (gethash (name r2) (elements *ruleset*)))))

;; context tests
(deftestsuite context-tests (LoGS) () ())

(addtest (context-tests)
  contexts-have-names
  (let ((c (ensure-context)))
    (ensure-different NIL (name c))))

(addtest (context-tests)
  contexts-are-added-to-context-hash
  (let* ((c (ensure-context))
         (c2 (get-context (name c))))
    (ensure-same c c2 :test #'equal)))

(addtest (context-tests)
  only-one-context-with-a-given-name
      (let* ((c1 (ensure-context))
             (c2 (ensure-context :name (name c1))))
        (ensure-same c1 c2)))

(addtest (rule-tests)
  default-style-rule-matches-message
  (let ((message (make-instance 'message :message "this is a message"))
        (rule (make-instance 'rule :match #'match-all)))
    (multiple-value-bind (matches sub-matches)
        (check-rule rule message NIL)
      (declare (ignore sub-matches))
      (ensure-different NIL matches))))

(addtest (rule-tests)
  no-exception-works
  (let ((message (make-instance 'message :message "asdf"))
        (rule (make-instance 'rule
                             :match #'match-all)))
    (multiple-value-bind (matches sub-matches)
        (check-rule rule message NIL)
      (declare (ignore sub-matches))
      (ensure-different NIL matches))))

(addtest (ruleset-tests)
  ruleset-rule-insertion-works
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue ruleset rule)
    (ensure-same (data (head ruleset)) rule)))

(addtest (ruleset-tests)
  ruleset-match-works
  (let ((ruleset (make-instance 'ruleset :match #'match-all))
        (rule (make-instance 'rule :match #'match-all))
        (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule)
    (check-rule ruleset message NIL)))

(addtest (ruleset-tests)
  empty-ruleset-does-not-match
  (let ((ruleset (make-instance 'ruleset :match #'match-all))
        (message (make-instance 'message :message "test message"))
        (retval ()))
    (ensure-null (check-rule ruleset message NIL))))

(addtest (ruleset-tests)
  matching-ruleseet-with-non-matching-rules-does-not-match
  (let ((ruleset (make-instance 'ruleset :match #'match-all))
        (message (make-instance 'message :message "test message"))
        (r1 (make-instance 'rule :match #'match-none)))
    (enqueue ruleset r1)
    (not (check-rule ruleset message NIL))))

(addtest (ruleset-tests)
  matching-ruleset-with-matching-rule-does-match
  (let ((ruleset (make-instance 'ruleset :match #'match-all))
        (message (make-instance 'message :message "test message"))
        (r1 (make-instance 'rule :match #'match-all)))
    (enqueue ruleset r1)
    (check-rule ruleset message NIL)))

(addtest (ruleset-tests)
  ruleset-with-matching-no-match-does-not-match
  (let ((ruleset (make-instance 
                  'ruleset 
                  :match (lambda (message environment)
                           (and (match-all message environment)
                                (not (match-all message environment))))))
        (message (make-instance 'message))
        (r1 (make-instance 'rule :match #'match-all)))
    (enqueue ruleset r1)
    (not 				; is this a fair test?
     (check-rule ruleset message NIL))))

(addtest (context-tests) 
  message-is-added-to-context
  (let ((message (make-instance 'message))
        (context (ensure-context)))
    (and
     (equal (Ecount context) 0)
     (add-to-context context message)
     (equal (Ecount context) 1))))

(addtest (ruleset-tests)
  removing-a-rule-from-the-ruleset-removes-it-from-elements
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue ruleset rule)
    (dll-delete ruleset rule)
    (ensure-null (gethash (name rule) (elements ruleset)))))

(addtest (ruleset-tests)
  removing-a-rule-from-the-ruleset-removes-it-from-list-entries
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue ruleset rule)
    (dll-delete ruleset rule)
    (ensure-null (gethash rule (list-entries ruleset)))))

(addtest (rule-tests)
  check-rules-finds-simple-match
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule :match #'match-all))
        (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule)
    (check-rules message ruleset NIL)))

(addtest (rule-tests)
  check-rules-finds-simple-regexp-match
  (let ((ruleset 
         (make-instance 'ruleset :name 'simple-regexp-match-ruleset))
        (rule (make-instance 
               'rule
               :name 'simple-regexp-match-rule
               :match 
               (lambda (message environment)
                 (declare (ignore environment))
                 (cl-ppcre::scan "test.*" 
                                 (message message)))))
        (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule)
    (check-rules message ruleset NIL)))


(addtest (rule-tests)
  check-rules-doesnt-find-simple-non-match
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule :match #'match-none))
        (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule)
    (not (check-rules message ruleset NIL))))

(addtest (rule-tests)
  check-rules-finds-match-in-second-rule
  (let* ((ruleset (make-instance 'ruleset))
         (xyzzy 0)    ; a variable whose value we change in the action
         (rule (make-instance 'rule :match #'match-none))
         (rule2 (make-instance 'rule 
                               :match #'match-all
                               :actions
                               (list
                                (lambda (message environment)
                                  (declare (ignore message environment))
                                  (setf xyzzy 42)))))
         (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule)
    (enqueue ruleset rule2)
    (and (check-rules message ruleset NIL)
         (ensure-same 42 xyzzy))))

(addtest (rule-tests)
  check-rules-doesnt-find-match-in-several-non-matching-rules
  (let ((ruleset (make-instance 'ruleset))
        (rule1 (make-instance 'rule :match #'match-none))
        (rule2 (make-instance 'rule :match #'match-none))
        (rule3 (make-instance 'rule :match #'match-none))
        (message (make-instance 'message :message "test message")))
    (enqueue ruleset rule1)
    (enqueue ruleset rule2)
    (enqueue ruleset rule3)
    (not (check-rules message ruleset NIL))))

(addtest (context-tests)
  message-is-eq-to-same-message-added-to-context
  (let ((message (make-instance 'message))
        (context (ensure-context)))
    (add-item context message)
    (eq message (aref (data context) 0))))          

(addtest (ruleset-tests)
  dll-delete-removes-a-rule-from-the-ruleset
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule)))
    (enqueue ruleset rule)
    (dll-delete ruleset rule)
    (not (head ruleset))))

(addtest (ruleset-tests)
  rule-with-timeout-can-be-inserted
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule :timeout 1)))
    (enqueue ruleset rule)
    (ensure-same rule (data (head ruleset))))
  )

(addtest (ruleset-tests)
  rule-that-has-exceeded-its-limits-is-found-to-be-so
  (let ((rule (make-instance 'rule :timeout 1)))
    (rule-exceeded-limit-p rule (get-universal-time))))

(addtest (rule-tests)
  rule-that-has-timed-out-is-removed-from-the-ruleset
  (let ((ruleset (make-instance 'ruleset))
        (rule (make-instance 'rule :timeout 1)))
    (setf *now* 42)
    (enqueue ruleset rule)
    (check-limits *timeout-object-timeout-queue*)
    (or (null (head ruleset))
        (dead-p (data (head ruleset)))))
  )

(addtest (rule-tests)
  timed-out-rule-is-removed-others-are-not
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
       (ensure-different NIL 
                         (and (gethash r1 (list-entries ruleset))
                              (gethash r3 (list-entries ruleset))))))))

;; file-follower tests
(deftestsuite file-follower-tests (LoGS) () ())

(addtest (file-follower-tests)
  new-file-follower-gets-a-line
  (let* ((testfile "testfile-new-ff")
         ff)
    (with-temporary-file 
        output-stream testfile
        ((format output-stream "this is a line~%"))
        (
          (setf ff (make-instance 'file-follower :filename testfile))
          (let ((line (get-line ff)))
            (close (filestream ff))
            (ensure-different NIL line))))))

(addtest (file-follower-tests)
  file-follower-returns-nil-with-no-input
  (let ((ff (make-instance 'file-follower :filename "/dev/null")))
    (ensure-null (get-line ff))))

(addtest (file-follower-tests)
  file-follower-addition-works
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
    (ensure-different NIL (get-line ff))
    ;; put another line in the file
    (with-open-file (output-stream testfile :direction :output 
                                   :if-exists :append
                                   :if-does-not-exist :create)
      (not (format output-stream "here is another~%")))

    (let ((line (get-line ff)))
      (close (filestream ff))
      (remove-file testfile)
      (ensure-different NIL line))))

(addtest (file-follower-tests)
  file-follower-inode-rollover-works
  (let* ((testfile "testfile-inode-rollover")
         (testfile-rollover ()))
    (with-open-file (output-stream testfile :direction :output 
                                   :if-exists :overwrite
                                   :if-does-not-exist :create)
      (format output-stream "this is a line"))

    (let ((ff (make-instance 'file-follower :filename testfile)))
      (ensure-different NIL
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
        (ensure-different NIL line)
        ))))

(addtest (file-follower-tests)
  PBS-file-follower-file-rollover-works
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
                                :match 
                                (lambda (x y) 
                                  (declare (ignore x y))
                                  t)
                                :actions 
                                (list
                                 (lambda (message environment)
                                   (declare (ignore message environment))
                                   (incf counter)))))
        (process-files)
        (close (filestream *messages*))
        (remove-file day1)
        (remove-file day2)
        (remove-file day3)
        (ensure-same 3 counter)))

(addtest (file-follower-tests)
  file-follower-makes-correct-series-of-messages
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
             (ensure-different NIL result)))))))

(addtest (rule-tests)
  get-rule-returns-the-rule
  (let ((rule (make-instance 'rule))
        (ruleset (make-instance 'ruleset)))
    (enqueue ruleset rule)
    (ensure-same rule (get-rule ruleset (name rule)))))

(addtest (context-tests)
  contests-that-have-expired-are-shown-to-exceed-limit
  (let ((context (ensure-context :name 'test-context :timeout 47))
        (now 48))
    (context-exceeded-limit-p context now)))

(addtest (context-tests)
  contexts-that-have-expired-their-relative-timeout-are-shown-to-exceed-limit
  (let* ((*now* 48)
         (context (ensure-context :relative-timeout 1)))
    (context-exceeded-limit-p 
     context 
     (+ *now* 1
        (* INTERNAL-TIME-UNITS-PER-SECOND 1))))) 

(addtest (context-tests)
  contexts-that-dont-have-timeouts-dont-exceed-limits
  (let ((context (ensure-context))
        (now 48))
    (ensure-null (context-exceeded-limit-p context now))))

(addtest (context-tests)
  contexts-that-have-expired-are-removed-from-hash-with-remove-context-if-stale
  (let ((context (ensure-context :timeout 47))
        (now 48))
    (remove-context-if-stale context now)
    (ensure-null (get-context (name context)))))

(addtest (context-tests)
  contexts-that-havent-expired-are-not-removed-from-hash
  (let ((context (ensure-context :timeout 49))
        (now 48))
    (remove-context-if-stale context now)
    (ensure-different NIL (get-context (name context)))))

(addtest (context-tests)
  get-context-returns-proper-context
  (let ((context (ensure-context)))
    (ensure-same 
     context
     (get-context (name context)))))

(addtest (context-tests)
  context-is-added-to-contexts-list-when-created
  (let ((context (ensure-context)))
    (ensure-different NIL (gethash context (list-entries *contexts*)))))

(addtest (context-tests)
  context-is-removed-from-contexts-list-when-deleted
  (let ((context (ensure-context :timeout 47)))
    (and
     (gethash context (list-entries *contexts*)))
    (remove-context-if-stale context 1)
    t))

(addtest (context-tests)
  old-context-exceeds-limits
  (let ((context (ensure-context :timeout 47))
        (now 48))
    (context-exceeded-limit-p context now)))

(addtest (context-tests)
  full-context-exceeds-limits
  (let ((context (ensure-context :name 'context :max-lines 1)))
    (add-to-context 'context (make-instance 'message :message "this is a line"))
    (add-to-context 'context (make-instance 'message :message "this is a line"))
    (ensure-different NIL (context-exceeded-limit-p context 42))))

(addtest (context-tests)
  full-context-runs-actions
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
        (ensure-different NIL fooble)))

(addtest (context-tests)
  context-runs-actions-exactly-once
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
    (ensure-same fooble 1)))

(addtest (context-tests)
  unkillable-context-runs-actions-exactly-once
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
    (ensure-same fooble 1)))

(addtest (context-tests)
  timed-out-context-runs-actions
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
          (ensure-different NIL fooble))))

(addtest (context-tests)
  relative-timed-out-context-runs-actions
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
          (ensure-different NIL fooble))))

(addtest (context-tests)
  deleted-context-is-removed-from-contexts-list
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout  x)
                                 (timeout  y))))))
        (let ((context (ensure-context :timeout 0))
              (*now* 1))
          (declare (ignore context))
          (ensure-different NIL (head *timeout-object-timeout-queue*))
          (check-limits *timeout-object-timeout-queue*)
          (ensure-null (head *timeout-object-timeout-queue*)))))

(addtest (context-tests)
  deleted-context-is-removed-from-contexts-list-when-pq-is-searched
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
          (ensure-different NIL (head *contexts*))
          (ensure-different NIL (head *timeout-object-timeout-queue*))
          (check-limits *timeout-object-timeout-queue*)
          (ensure-null (head *contexts*))
          (ensure-null (head *timeout-object-timeout-queue*)))))

(addtest (context-tests)
  deleted-context-runs-actions
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

          (ensure-different NIL (context-exceeded-limit-p context *now*))
          (check-limits *timeout-object-timeout-queue*)
          (ensure-different NIL *fooble*))))

(addtest (ruleset-tests)
  nested-rulesets-are-run
    
      (let* ((thing ())
             (ruleset1 (make-instance 'ruleset :match #'match-all))
             (ruleset2 (make-instance 'ruleset :match #'match-all))
             (rule (make-instance 'rule 
                                  :match #'match-all
                                  :actions 
                                  (list
                                   (lambda (message &optional environment)
                                     (declare (ignore message environment))
                                     (progn 
                                       (setf thing t))))))
             (message (make-instance 'message :message "test message")))
        (enqueue ruleset2 rule)
        (enqueue ruleset1 rule)
        (check-rule ruleset1 message NIL)
        (ensure-different NIL thing)))

(addtest (linked-list)
  insert-after-inserts-after-in-dll
      (let ((lst (make-instance 'doubly-linked-list))
            (item1 (make-instance 'rule))
            (item2 (make-instance 'rule)))
        (enqueue lst item1)
        (dll-insert lst (head lst) item2 :direction :after)
        (ensure-same item1 (data (head lst)))
        (ensure-same item2 (data (rlink (head lst))))))

(addtest (linked-list)
  insert-before-inserts-before-in-dll
      (let ((lst (make-instance 'doubly-linked-list))
            (item1 (make-instance 'rule))
            (item2 (make-instance 'rule)))
        (enqueue lst item1)
        (dll-insert lst (head lst) item2 :direction :before)
        (ensure-same item2 (data (head lst)))
        (ensure-same item1 (data (rlink (head lst))))))

(deftestsuite pq-tests (LoGS) () ())

(addtest (pq-tests)
  rule-with-timeout-is-inserted-into-pq
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue 
                            :comparison-function 
                            (lambda (x y) 
                              (> (timeout x)
                                 (timeout y))))))
        (let ((rule (make-instance 'rule :timeout 1)))
          (declare (ignore rule))
          (ensure-different NIL (head *timeout-object-timeout-queue*)))))

(addtest (pq-tests)
  context-with-timeout-is-inserted-into-pq
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((context (ensure-context :timeout 1)))
          (declare (ignore context))
          (ensure-different NIL (head *timeout-object-timeout-queue*)))))

(addtest (pq-tests)
  item-is-entered-into-priority-queues-list-entries
      (let ((pq (make-instance 'priority-queue)))
        (enqueue pq 42)
        (ensure-different NIL (gethash 42 (list-entries pq)))))

(addtest (pq-tests)
  timed-out-rule-is-removed-from-pq
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((rule (make-instance 'rule :timeout 1)))
          (declare (ignore rule))
          (ensure-different NIL (head *timeout-object-timeout-queue*))
          (check-limits *timeout-object-timeout-queue*)
          (ensure-null (head *timeout-object-timeout-queue*)))))

(addtest (pq-tests)
  timed-out-rule-is-removed-from-pq-hash
        (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((rule (make-instance 'rule :timeout 1))
              (*now* 42))
          (check-limits *timeout-object-timeout-queue*)
          (ensure-null (gethash rule (list-entries *timeout-object-timeout-queue*))))))

(addtest (pq-tests)
  contexts-end-up-in-right-order-in-pq
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((c3 (ensure-context :timeout 46))
              (c1 (ensure-context :timeout 42))
              (c2 (ensure-context :timeout 43)))
          (ensure-different NIL (head *timeout-object-timeout-queue*))
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        c1)
          (ensure-same (data (rlink (head *timeout-object-timeout-queue*)))
                        c2)
          (ensure-same (data (rlink (rlink (head *timeout-object-timeout-queue*))))
                        c3))))

(addtest (pq-tests)
  rules-end-up-in-right-order-in-pq
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((r3 (make-instance 'rule :timeout 46))
              (r1 (make-instance 'rule :timeout 42))
              (r2 (make-instance 'rule :timeout 43)))
          (ensure-different NIL (head *timeout-object-timeout-queue*))
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        r1)
          (ensure-same (data (rlink (head *timeout-object-timeout-queue*)))
                        r2)
          (ensure-same (data (rlink (rlink (head *timeout-object-timeout-queue*))))
                        r3))))

(addtest (pq-tests)
  context-pq-is-updated
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((c1 (ensure-context :timeout 42))
              (c2 (ensure-context :timeout 43)))
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        c1)
          (setf (timeout c2) 40)
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        c2))))

(addtest (pq-tests)
  rule-pq-is-updated
      (let ((*timeout-object-timeout-queue*
             (make-instance 'priority-queue
                            :comparison-function
                            (lambda (x y)
                              (> (timeout x)
                                 (timeout y))))))
        (let ((r1 (make-instance 'rule :timeout 42))
              (r2 (make-instance 'rule :timeout 43)))
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        r1)
          (setf (timeout r2) 40)
          (ensure-same (data (head *timeout-object-timeout-queue*))
                        r2))))

(addtest (rule-tests)
  nomatch-negates-match-in-rule
      (let ((dummy ()))
        (let ((rule (make-instance 'rule
                                   :match (lambda (msg environment)
					    (if (and (match-all msg environment)
						     (not (match-all msg environment)))
						t
  					        'nil))
                                   :actions
                                   (list
                                    (lambda (message environment)
                                      (declare (ignore message environment))
                                      (setf dummy t)))))
              (message (make-instance 'message)))
          (check-rule rule message NIL)
          (ensure-null dummy))))

(addtest (rule-tests)
  delete-rule-causes-a-rule-to-be-deleted
      (let ((rule (make-instance 'rule
                                 :match #'match-all
                                 :delete-rule #'match-all))
            (*ruleset* (make-instance 'ruleset))
            (message (make-instance 'message)))
        (ensure-null (head *ruleset*))
        (enqueue *ruleset* rule)
        (check-rule rule message NIL)
        (head *ruleset*)
        (or (not (head *ruleset*))
            (dead-p (data (head *ruleset*))))))

(addtest (ruleset-tests)
  rule-inside-ruleset-inside-ruleset
      (let* ((*ruleset* (make-instance 'ruleset :match #'match-all))
             (r2 (make-instance 'ruleset :match #'match-all))
             (xyzzy 0)
             (message (make-instance 'message :message "test message"))
             (rule (make-instance 'rule :match #'match-all
                                  :actions 
                                  (list
                                   (lambda (message &optional environment)
                                     (declare (ignore message environment))
                                     (setf xyzzy 42))))))
        (enqueue  r2 rule)
        (enqueue *ruleset* r2)
        (check-rules message *ruleset* NIL)
        (ensure-same 42 xyzzy)))

(addtest (rule-tests)
  non-matching-regexp-rule-works
      (let ((message 
             (make-instance 'message :message "this is a different message"))
            (rule (make-instance 
                   'rule
                   :match (lambda (message environment)
                            (declare (ignore environment))
                            (cl-ppcre::scan "this is a message" 
                                            (message message))))))
        (not (check-rule rule message NIL))))

(addtest (rule-tests)
  exception-works
      (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 'rule
                                 :match 
                                 (lambda (message environment) 
                                   (declare (ignore message environment))
                                   t))))
        (ensure-different NIL (check-rule rule message NIL))))

(addtest (rule-tests)
  regexp-rule-works
      (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 
                   'rule
                   :match (lambda (message environment)
                            (declare (ignore environment))
                            (cl-ppcre::scan "this is a message" 
                                            (message message))))))
        (ensure-different NIL (check-rule rule message NIL))))

(addtest (rule-tests)
  anti-default-style-rule-doesnt-match-message
        (let ((message (make-instance 'message :message "this is a message"))
            (rule (make-instance 
                   'rule 
                   :match 
                   (lambda (message environment) 
                     (declare (ignore message environment))
                     ()))))
        (not (check-rule rule message NIL))))

(addtest (context-tests)
  get-context-returns-alias-instead-of-orig-context
      (let ((c1 (ensure-context :name 'c1))
            (c2 (ensure-context :name 'c2)))
        (declare (ignore c1))
        (alias-context c2 'c1)
        (ensure-same c2 (get-context 'c1))))

(addtest (context-tests)
  delete-context-deletes-alias
        (let* ((*contexts-hash* (make-hash-table :test #'equal))
             (*contexts-alias-hash* (make-hash-table :test #'equal))
             (c1 (ensure-context :name 'c1))
             (c2 (ensure-context :name 'c2)))
        (alias-context c2 'c1)
        (delete-context 'c1)
        (ensure-same c1 (get-context 'c1))))

(addtest (rule-tests)
  filter-function-works
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
          (lambda (message environment)
            (declare (ignore environment))
            (equal (message message) "cat")))))
    (enqueue *root-ruleset* rule)
    (enqueue *root-ruleset*
             (Single
              (lambda (message environment)
                (declare (ignore environment))
                (equal (message message) "the"))
              (list
               (lambda (message environment)
                 (declare (ignore message environment))
                 (setf seen-the t)))))
    (enqueue *root-ruleset*
             (Single
              (lambda (message environment)
                (declare (ignore environment))
                (equal (message message) "ran"))
              (list
               (lambda (message environment)
                 (declare (ignore message environment))
                 (setf seen-ran t)))))
    (enqueue *root-ruleset*
             (Single
              (lambda (message environment)
                (declare (ignore environment))
                (equal (message message) "cat"))
              (list
               (lambda (message environment)
                 (declare (ignore message environment))
                 (setf seen-cat t)))))
    (process-files)
    (ensure-different NIL (and seen-the (not seen-cat) seen-ran))))


    
(addtest (rule-tests)
  suppress-function-works
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
                         (lambda (message environment)
                           (declare (ignore message environment))
                           t)
                         (list
                          (lambda (message environment)
                            (declare (ignore environment))
                            (setf seen-messages 
                                  (cons (message message)
                                        seen-messages))))))
         (rule (Suppress 
                (lambda (message environment)
                  (declare (ignore environment))
                  (equal (message message) "the"))
                1)))
    
    (enqueue *root-ruleset* rule)
    (enqueue *root-ruleset* save-messages)
    (process-files)
    (ensure-same 
     seen-messages
     '("ran" "cat"))))

(addtest (rule-tests)
  suppress-function-stops-after-timeout
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
         (foo (single (lambda (message environment) 
                        (declare (ignore message environment))
                        t) ()))
         (save-messages (Single 
                         (lambda (message environment)
                           (declare (ignore message environment))
                           t)
                         (list
                          (lambda (message environment)
                            (declare (ignore environment))
                            (setf seen-messages 
                                  (cons (message message)
                                        seen-messages))))))
         (rule (Suppress 
                (lambda (message environment)
                  (declare (ignore environment))
                  (equal (message message) "the"))
                0
                :name 'foo
                )))
    
    (enqueue *root-ruleset* rule)
    (enqueue *root-ruleset* save-messages)
    (enqueue *root-ruleset* foo)

    (loop as x from 1 to 3 do
         (let ((*message* (get-logline *messages*)))
           (check-rules *message* *root-ruleset* NIL)
           (check-limits *timeout-object-timeout-queue*)
           ))

    (setf *now* (* 2 *now*))
    (check-limits *timeout-object-timeout-queue*)

    (let ((*message* (get-logline *messages*)))
      (check-rules *message* *root-ruleset* NIL)) 

    (ensure-same 
     seen-messages
     '("the" "ran" "cat"))))


(addtest (rule-tests)
  suppress-until
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
           (lambda (message environment)
             (declare (ignore environment))
             (equal (message message) "the"))
           (lambda (message environment)
             (declare (ignore environment))
             (equal (message message) "ran"))
           :name 'suppress-until-rule))

         (match-count 0)
         (catch-all-rule
          (make-instance 'rule
                         :match #'match-all
                         :name 'xxx
                         :actions
                         (list
                          (lambda (message environment)
                            (declare (ignore message environment))
                            (incf match-count))))))
    
    (enqueue *root-ruleset* suppress-until-rule)
    (enqueue *root-ruleset* catch-all-rule)
    
    (process-files)
    
    (ensure-same match-count 2)))

(addtest (rule-tests)
  test-single-rule-type
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
           (lambda (message environment)
             (declare (ignore message environment))
             t)
           (list 
            (lambda (message environment)
              (declare (ignore message environment))
              (incf match-count))))))
    
    (enqueue *root-ruleset* catch-all-rule)
    (process-files)
    (ensure-same match-count 4)))

(addtest (rule-tests)
  test-pair-rule-type
  (let* ((*root-ruleset* (make-instance 'ruleset))
         (*messages* 
          (make-instance 
           'list-follower
           :message-list
           (list "the" "cat" "ran" "the")))
         (count 0)
         (rule
          (pair
           ;; match1
           (lambda (message environment)
             (declare (ignore environment))
             (equal (message message) "the"))
           ;; actions1
           ()
           ;; match2
           (lambda (message environment)
             (declare (ignore environment))
             (equal (message message) "cat"))
           ;; actions2
           (list
            (lambda (message environment)
              (declare (ignore message environment))
              (incf count))))))
    (enqueue *root-ruleset* rule)
    (process-files)    
    (ensure-same count 1)))

;;; XXX 
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
           (check-rules *message* *root-ruleset* NIL)
           (check-limits *timeout-object-timeout-queue*)
           ))

    ;;     (setf *now* (* 2 *now*))
    ;;     (check-limits *timeout-object-timeout-queue*)

    ;;     (let ((*message* (get-logline *messages*)))
    ;;       (check-rules *message* *root-ruleset*))
    
    count))

;; 5 inserts:
;;
;; insert empty list
;; insert head of list
;; insert tail of list
;; insert between 2 elements

(addtest (linked-list)
  insert-empty-list
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (ensure-same 42 (data (head dll)))
        (ensure-same 42 (data (tail dll)))))

(addtest (linked-list)
  insert-head-of-list
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (head dll) 43 :direction :before)
        (and (ensure-same 43 (data (head dll)))
             (ensure-same 42 (data (tail dll))))))

(addtest (linked-list)
  insert-head-of-list-rlink-and-llink-are-right
  (let ((dll (make-instance 'doubly-linked-list)))
    (dll-insert dll (head dll) 42 :direction :before)
    (dll-insert dll (head dll) 43 :direction :before)
    (and (ensure-same 42 (data (rlink (head dll))))
         (ensure-same 43 (data (llink (tail dll)))))))

(addtest (linked-list)
  insert-tail-of-list
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (tail dll) 43 :direction :after)
        (ensure-same 42 (data (head dll)))
        (ensure-same 43 (data (tail dll)))))

(addtest (linked-list)
  insert-tail-of-list-rlink-and-llinks-are-right
      (let ((dll (make-instance 'doubly-linked-list)))
        (dll-insert dll (head dll) 42 :direction :before)
        (dll-insert dll (tail dll) 43 :direction :after)
        (ensure-same 43 (data (rlink (head dll))))
        (ensure-same 42 (data (llink (tail dll))))))

(addtest (linked-list)
  insert-after-head-before-tail-of-list
      (let ((dll (make-instance 'doubly-linked-list)))
        ;; head is 42
        (dll-insert dll (head dll) 42 :direction :before)
        ;; tail is 43
        (dll-insert dll (tail dll) 43 :direction :after)
        ;; insert between head and tail, after head
        (dll-insert dll (head dll) 44 :direction :after)
        (ensure-same 42 (data (head dll)))
        (ensure-same 43 (data (tail dll)))
        (ensure-same 44 (data (rlink (head dll))))
        (ensure-same 44 (data (llink (tail dll))))))

(addtest (linked-list)
  insert-before-tail-after-head-of-list
  (let ((dll (make-instance 'doubly-linked-list)))
        ;; head is 42
        (dll-insert dll (head dll) 42 :direction :before)
        ;; tail is 43
        (dll-insert dll (tail dll) 43 :direction :after)
        ;; insert between head and tail, before tail
        (dll-insert dll (tail dll) 44 :direction :before)
        (ensure-same 42 (data (head dll)))
        (ensure-same 43 (data (tail dll)))
        (ensure-same 44 (data (llink (tail dll))))))

(addtest (rule-tests)
  rule-that-exceeds-relative-timeout-is-shown-to-have-exceeded-limits
      (let* ((*now* 1)
             (rule (make-instance 'rule :relative-timeout 1
                                  :match 
                                  (lambda (message environment)
                                    (declare (ignore message environment))
                                    t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (ensure-different NIL (check-limits rule))))

(addtest (rule-tests)
  rule-that-exceeds-relative-timeout-is-marked-as-dead
      (let* ((*now* 1)
             (rule (make-instance 
                    'rule 
                    :relative-timeout 1
                    :match 
                    (lambda (message environment)
                      (declare (ignore message environment))
                      t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (check-limits rule)
        (ensure-different NIL (dead-p rule))))

(addtest (rule-tests)
  rule-relative-timeouts-are-updated-on-match
      (let* ((*now* 1)
             (rule (make-instance 'rule :relative-timeout 1
                                  :match (lambda (message environment)
                                           (declare (ignore message environment))
                                           t))))
        ;; make *now* larger than the timeout
        (setf *now* 
              (+ *now* 1
                 (* INTERNAL-TIME-UNITS-PER-SECOND 1)))
        (check-rule rule (make-instance 'message) NIL)
        (ensure-null (check-limits rule))))

(addtest (ruleset-tests)
  ruleset-with-relative-timeout-is-updated-on-match
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
        (enqueue ruleset (make-instance 
                          'rule 
                          :match (lambda (message environment)
                                   (declare (ignore message environment))
                                   t)))
        (setf *now* 456)
        (check-rules (make-instance 'message) ruleset NIL)
        (ensure-different NIL (> (next-timeout ruleset) 456))))

(addtest (ruleset-tests)
  ruleset-with-relative-timeout-is-not-updated-without-match
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
        (enqueue ruleset (make-instance 
                          'rule 
                          :match 
                          (lambda (message environment) 
                            (declare (ignore message environment))
                            NIL)))
        (setf *now* (+ (next-timeout ruleset) 1))
        (check-rules (make-instance 'message) ruleset NIL)
        (ensure-null (> (next-timeout ruleset) *now*))))

(addtest (ruleset-tests)
  ruleset-that-exceeds-relative-timeout-is-shown-to-exceed-limits
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
      
        (setf *now* (+ (next-timeout ruleset) 1))
        (ensure-different NIL (check-limits ruleset))))

(addtest (ruleset-tests)
  ruleset-that-exceeds-relative-timeout-is-marked-as-dead
      (let ((*now* 123)
            (ruleset (make-instance 'ruleset :relative-timeout 1)))
      
        (setf *now* (+ (next-timeout ruleset) 1))
        (check-limits ruleset)
        (ensure-different NIL (dead-p ruleset))))

(addtest (context-tests)
  context-that-should-live-after-timeout-isnt-destroyed
      (let* ((*now* 123)
             (context (make-instance 'context
                                     :name 'foop
                                     :timeout 124
                                     :lives-after-timeout t)))
        (setf *now* 125)
        (check-limits context)
        (let ((retval (ensure-different NIL (get-context (name context)))))
          (expire-context context)
          retval)))

(addtest (context-tests)
  context-that-shouldnt-live-after-timeout-is-destroyed
      (let* ((*now* 123)
             (context (make-instance 'context
                                     :name 'foop
                                     :timeout 124
                                     :lives-after-timeout ())))
        (setf *now* 125)
        (ensure-different NIL (check-limits context))
        (ensure-null (get-context (name context)))))

(addtest (context-tests)
  min-lines-context-test
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
        (ensure-different NIL testvar)))

(addtest (context-tests)
  min-lines-exceeded-context-test
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
        (ensure-different NIL testvar)))

;; some option tests

(deftestsuite cli-tests (LoGS) () ())

(addtest (cli-tests)
  no-internal-time-option-test
  (progn
    (setq org.prewett.LoGS::*use-internal-real-time* t) ;; make sure its in true state
    (process-options *opts* '("--no-internal-time"))
    (ensure-null org.prewett.LoGS::*use-internal-real-time*)))

(addtest (cli-tests)
  file-option-test
      (let ((*file-list* ()))
        (declare (special *file-list*))
        (process-command-line *opts* '("--file" "foo.txt"))
        (typep *messages* 'file-follower)))

(addtest (cli-tests)
  file-option-with-position-test
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
         (let ((ret (ensure-same 42 (file-position (filestream *messages*)))))
           (close (filestream *messages*))
           (remove-file testfile)
           ret))))

(addtest (cli-tests)
  spawn-works
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
                                (ensure-different NIL testval))))))

(deftestsuite window-tests (LoGS) () ())

(addtest (window-tests)
  old-items-are-removed-from-window
      (let ((window (make-instance 'window :window-length 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (setf *now* (+ *now* (* 10 INTERNAL-TIME-UNITS-PER-SECOND)))
        (check-limits window)
        (ensure-null (head (data window)))))

(addtest (window-tests)
  non-old-items-are-not-removed-from-window
      (let ((window (make-instance 'window :window-length 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (setf *now* (+ *now* (* 10 INTERNAL-TIME-UNITS-PER-SECOND)))
        (let ((save-message (make-instance 'message)))
          (add-item window save-message)
          (check-limits window)
          (and 
           (ensure-same (cadr (data (head (data window)))) save-message)
           (ensure-null (rlink (head (data window))))))))

(addtest (window-tests)
  window-with-too-many-non-old-entries-is-shown-to-exceed-limits
      (let ((window (make-instance 'window :max-lines 1)))
        (add-item window (make-instance 'message))
        (add-item window (make-instance 'message))
        (ensure-different NIL (check-limits window))))

(deftestsuite language-tests (LoGS) () ())

(addtest (language-tests)
  empty-rule-has-no-match
  (let ((rule (org.prewett.LoGS.language::rule)))
    (ensure-null (match rule))))

(deftestsuite environment-tests (LoGS) () ())

(addtest (environment-tests)
  get-logs-env-var-finds-match-in-simple-environment
      (let ((environment '((FOO 42)
                           (BAR 23)
                           (BAZ 17))))
        (ensure-same 
         23
         (get-LoGS-env-var 'bar environment))))


(addtest (environment-tests)
  get-logs-env-var-finds-NIL-valued-match-in-simple-environment
      (let ((environment '((FOO 42)
                           (BAR NIL)
                           (BAZ 17))))
        (multiple-value-bind (value present-p)
            (get-LoGS-env-var 'bar environment)
          (and
           (ensure-different NIL
            present-p)
           (ensure-null
            value)))))

(addtest (environment-tests)
  get-logs-env-var-doesnt-find-missing-match-in-simple-environment
      (let ((environment '((FOO 42)
                           (BAR NIL)
                           (BAZ 17))))
        (multiple-value-bind (value present-p)
            (get-LoGS-env-var 'quux environment)
          (and
           (ensure-null
            present-p)
           (ensure-null
            value)))))

(addtest (environment-tests)
  rule-can-set-environment
      (let ((rule (org.prewett.LoGS.language::rule with foo = "one")))
        (ensure-same
         "one"
         (cadar (member 'foo (environment rule) :key #'car)))))

(addtest (environment-tests)
  ruleset-can-set-environment
      (let ((ruleset (language::ruleset with var = "xxx")))
        (ensure-same
         "xxx"
         (get-LoGS-env-var 'var (logs::environment ruleset)))))

(addtest (environment-tests)
  rule-match-can-bind-variable
      (let* ((set-me NIL)
             (rule (language::rule 
                    matching regexp "(.*)" 
                    binding (variable)
                    doing
                    (lambda (message environment)
                      (declare (ignore message))
                      (setf set-me (get-LoGS-env-var 'variable environment)))))
             (message (make-instance 'message :message "test")))
        (logs::check-rule rule message NIL)
        (ensure-same "test" set-me)))

(addtest (environment-tests)
  ruleset-match-can-bind-variable
      (let* ((set-me NIL)
             (ruleset (language::ruleset
                       named 'ruleset
                       matching regexp "(.*)"
                       binding (variable)
                       containing
                       ((rule 
                         named 'rule
                         matching regexp ".*"
                         doing
                         (lambda (message environment)
                           (declare (ignore message))
                           (setf set-me (get-LoGS-env-var 'variable environment)))))))
             (message (make-instance 'message :message "test")))
        (logs::check-rule ruleset message NIL)
        (ensure-same "test" set-me)))

(addtest (environment-tests)
  rule-match-variable-overrides-ruleset-match-variable
      (let* ((set-me NIL)
             (ruleset (language::ruleset
                       matching regexp "(.*)" 
                       binding (variable)
                       containing
                       ((rule matching regexp "foo:(.*)"
                              binding (variable)
                              doing
                              (lambda (message environment)
                                (declare (ignore message))
                                (setf set-me (get-LoGS-env-var 'variable environment)))))))
             (message (make-instance 'message :message "foo:test")))
        (logs::check-rule ruleset message NIL)
        (ensure-same "test" set-me)))

(addtest (environment-tests)
  rule-environment-variable-overrides-ruleset-environment-variable
      (let* ((set-me NIL)
             (ruleset (language::ruleset
                       named 'ruleset
                       with variable = 'ruleset-var
                       matching regexp ".*" 
                       containing
                       ((rule 
                         named 'rule
                         matching regexp ".*"
                         with variable = 'rule-var
                         doing
                         (lambda (message environment)
                           (declare (ignore message))
                           (setf set-me 
                                 (get-LoGS-env-var 'variable environment)))))))
             (message (make-instance 'message :message "test")))
        (logs::check-rule ruleset message NIL)
        (ensure-same 'rule-var set-me)
        ))

(addtest (environment-tests)
  rulesets-environment-variable-is-propogated
      (let* ((set-me NIL)
             (ruleset 
              (language::ruleset
               matching regexp "(.*)"
               with variable = 42
               containing
               ((rule matching regexp ".*"
                      doing
                      (lambda (message environment)
                        (declare (ignore message))
                        (setf set-me (get-LoGS-env-var 'variable environment)))))))
             (message (make-instance 'message :message "foo:test")))
        (logs::check-rule ruleset message NIL)
        (ensure-same 42 set-me)))

(addtest (environment-tests)
  rule-variable-only-lives-for-rule-check
      (let* ((set-me NIL)
             (ruleset (language::ruleset
                       matching regexp "(.*)" 
                       binding (variable)
                       containing
                       ((rule matching regexp "foo:(.*)"
                              binding (variable)
                              continue
                              doing
                              (lambda (message environment)
                                (declare (ignore message))
                                (setf set-me 
                                      (get-LoGS-env-var 'variable environment))))
                        (rule matching regexp ".*"
                              doing
                              (lambda (message environment)
                                (declare (ignore message))
                                (setf set-me 
                                      (get-LoGS-env-var 'variable environment)))))))
             (message (make-instance 'message :message "foo:test")))
        (logs::check-rule ruleset message NIL)
        (ensure-same "foo:test" set-me)))

(addtest (environment-tests)
  all-rule-variable-bindings-happen
      (let* ((set-me NIL)
             (ruleset 
              (let ((var1 "var1")) ;; variable set lexically
                (language::ruleset
                 with var2 = "var2"     ;; variable set in ruleset's environment
                 matching regexp "(.*):.*" 
                 binding (var3)        ;; variable set by ruleset's match function
                 containing
                 ((rule 
                   with var4 = "var4"   ;; variable set by rule's environment
                         matching regexp "var3:(.*)"
                              binding (var5) ;; variable set by rule's match function
                              doing
                              (lambda (message environment)
                                (declare (ignore message))
                                (setf set-me 
                                      (format () "~A ~A ~A ~A ~A" 
                                               var1 
                                               (get-LoGS-env-var 
                                                'var2 environment)
                                               (get-LoGS-env-var
                                                'var3 environment)
                                               (get-LoGS-env-var
                                                'var4 environment)
                                               (get-LoGS-env-var
                                                'var5 environment)))))))))
             (message (make-instance 'message :message "var3:var5")))
        (logs::check-rule ruleset message NIL)
        (ensure-same "var1 var2 var3 var4 var5" set-me)))


;; make sure that variable bindings aren't hanging around
;; from a previous test
(addtest (environment-tests)
  unbound-variable-test
      (let* ((set-me NIL)
             (rule (rule 
                    matching regexp ".*"
                    doing
                    (lambda (message environment)
                      (declare (ignore message))
                      (setf set-me (and (not (boundp 'var1))
                                        (not (get-LoGS-env-var 'var1 environment)))))))
             (message (make-instance 'message :message "test")))
        (logs::check-rule rule message NIL)
        (ensure-different NIL set-me)))

;; load other tests
;; (load-logs-file "Language/rdl-tests")

(defun run-all-tests ()
  (let ((tests (run-tests :suite :logs)))
    (format t "~%failures: ~A~%" (failures tests)            )
    (if (or (errors tests)
            (failures tests))
        (progn
          (format t "failed 1 or more tests~%")
          #+sbcl
          (quit :unix-status 1))
        (progn         
          (format t "passed all tests~%")
          #+sbcl
          (quit :unix-status 0)))))