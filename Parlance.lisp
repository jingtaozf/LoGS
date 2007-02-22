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

(in-package :org.prewett.LoGS)

;;; Basically, this file is here to make things a little higher-level
;;; than the LoGS core provides by itself.

;;; ways to ignore things

;; filter out matching messages
(defun filter (match-func &key name environment continuep)
  "create a rule that causes messages that match to be ignored"
  (declare (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (make-instance
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match match-func))

(defmacro match-regexp (regexp)
  (let ((message (gensym "MESSAGE"))
        (environment (gensym "ENVIRONMENT")))
    `(lambda (,message ,environment)
      (declare (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)) (ignore ,environment))
      (cl-ppcre::scan ,regexp (message ,message)))))

;; filter out messages matching the given regexp
(defun filter-regexp (regexp &key name environment continuep)
  "create a rule that causes messages matching the given regexp to be ignored"
  (filter
   (match-regexp regexp)
   :name name
   :continuep continuep
   :environment environment
   ))

;; suppress messages matching match1 until timeout has been exceeded
;; how is this different from throttling?  Shouldn't this be called throttling?
(defun suppress (match-func timeout &key name environment continuep)
  "create a rule that causes messages that match to be ignore for a certain period of time from *now*"
  (make-instance
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match match-func
   :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))))

;; suppress messages matching match1 until match2 is seen
(defun suppress-until (match1 match2 &key name environment continuep)
  (make-instance
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match
   (lambda (message environment)
     (or
      (funcall match1 message environment)
      (funcall match2 message environment)))
   :delete-rule
   match2))

;; I DO NOT LIKE THIS NAME!
(defun single (match-func actions-list &key name environment continuep)
  "create a rule that triggers the functions in the actions list as a result of a match"
  (make-instance
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match match-func
   :actions actions-list))

;; single with throttle?
(defun single-with-suppress (match-func actions-list time &key name environment continuep)
  "run the actions in the actions list in response to a match, then add a rule before this one to ignore this message for a while"
  (make-instance
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match match-func
   :actions (append 
             actions-list
             (rule-before
              (suppress match-func time)))))

;; do something at a particular time
(defun timed-event (actions-list time &key name)
  "cause something to happen at a particular time"
  (make-instance 
   'context
   :name name
   :actions actions-list
   :timeout time))

;; do something every time seconds
(defun periodic-event (actions-list time &key name)
  "cause something to happen periodically"
  (make-instance 
   'context
   :name name
   :actions actions-list
   :relative-timeout time
   :lives-after-timeout t))

(defun pair (match1 actions1 match2 actions2 &key continuep environment)
  "like SEC pair rule"
  (make-instance 
   'rule
   :match match1
   :continuep continuep
   :environment environment
   :actions
   (append
    (list
     (lambda (message environment)
       (declare (ignore message))
       (let ((ign-match1 (make-instance 'rule :match match1))
             (find-match2 (make-instance 'rule
                                         :match
                                         match2
                                         :environment
                                         environment
                                         :actions actions2)))
         (rule-before find-match2)
         (rule-before ign-match1))))
    actions1)))

;;; XXX finish me!
(defun pair-with-window (match1 actions1 match2 actions2 missing-actions window 
                         &key name1 name2 environment continuep)
  "like SEC pair with window rule."
  (let* ((timeout (+ *now* (* window INTERNAL-TIME-UNITS-PER-SECOND)))
         (match1-rule-name (or name1 (gensym)))
         (match2-rule-name (or name2 (gensym)))
         (context (make-instance 
                   'context
                   :timeout timeout
                   :actions 
                   ;; XXX add stuff to delete rules to actions list
                   (append
                    (list
                     (lambda (context)
                       (declare (ignore context))
                       (kill (get-rule *ruleset* match2-rule-name))))
                    missing-actions))))
    ;; rule to match first message and create rule to match second message
    (make-instance
     'rule
     :name match1-rule-name ; does this need a name?
     :continuep continuep
     :environment environment
     :match match1
     :actions
     (append
      (list
       ;; make a rule to ignore match1 until match2 is seen
       (lambda (message environment)
         (declare (ignore message environment))
         (rule-before
          (suppress-until match1 match2)))
       ;; make a rule to trigger actions2 when match2 is seen
       (lambda (message environment)
         (declare (ignore message environment))
         (rule-before
          (make-instance 
           'rule
           :name match2-rule-name
           :match match2
           :timeout timeout
           :delete-rule (lambda (message environment) 
                          (declare (ignore message environment))
                          t)
           :actions actions2))))
      actions1))))

;; XXX don't like this!
(defun long-message (long actions-list &key name environment continuep)
  "create a rule that matches messages longer than long.  trigger the functions in the actions list as a result."
  (Single
   (lambda (message environment)
     (declare (ignore environment))
     (> (length (message message)) long))
   actions-list
   :name name
   :environment environment
   :continuep continuep))


(defun print-context (context)
  "print out the contents of a context"
  (when (> (ecount context) 0)
    (map-into 
     (make-array (list (ecount context)))
     (lambda (x) (format t "~A~%" (message x)))
     (data context))))


(defmethod gather-into (match (context context) &key name environment continuep)
  "gather messages matching match into the given context"
  (make-instance
   'rule
   :name name
   :match match
   ;; the rule should die when the context does
   :timeout (timeout context)
   :environment environment
   :continuep continuep
   :actions
   (list
    (lambda (message environment)
      (declare (ignore environment))
      (add-to-context context message)))))
                 
(defmethod gather-into (match context &key name environment continuep)
  "gather messages matching match into the named context"
  (gather-into match (get-context context) 
               :name name 
               :environment environment 
               :continuep continuep))

(defun gather (match timeout actions &key context-name name max-lines environment continuep)
  "gather messages matching match into a context.  When the timeout is exceeded, run the context actions provided."
  (let ((context (ensure-context
                  :max-lines max-lines
                  :name context-name
                  :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))
                  :actions actions)))
    (gather-into match context :name name :environment environment :continuep continuep)))

(defun gather-related (matchlist timeout actions &key names environment continuep)
  "gather messages matching one of the functions in the match list into a context.  When the timeout is exceeded, run the context actions provided."
  (let ((context (make-instance 
                  'context
                  :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))
                  :actions actions)))
    (if names
        (mapcar
         (lambda (match name)
           (rule-before
            (gather-into match context 
                         :name name 
                         :environment environment 
                         :continuep continuep)))
         matchlist
         names)
        (mapcar
         (lambda (match)
           (rule-before
            (gather-into match context 
                         :environment environment 
                         :continuep continuep)))
         matchlist))))

;; throttle action.
(defmacro throttle (timeout &key name continuep environment)
  "filter out messages that match the match function of this rule until the timeout is exceeded"
  (let ((message (gensym "MESSAGE"))
        (rule-environment (gensym "ENVIRONMENT")))
    `(lambda (,message ,rule-environment)
       (declare (ignore ,message ,rule-environment))
       (rule-before
        (make-instance
         'rule
         :name ,name
         :continuep ,continuep
         :environment ,environment
         :match (match
                 (data
                  *current-rule*))
         :timeout (+ *now* (* ,timeout INTERNAL-TIME-UNITS-PER-SECOND)))))))

;;;; action function creating functions

(defun print-log ()
  "print the message"
  (lambda (message environment)
    (declare (ignore environment))
    (format t "~A~%" (message message))))

(defun echo ()
  "an alias for print-log"
  (print-log))

(defun print-message (message environment)
  (declare (ignore environment))
  (format t "~A~%" (message message)))

;;;; match functions

(defun match-all (message ENVIRONMENT) 
  "match every message"
  (declare (ignore message environment)) 
  t)

(defun match-none (message environment)
  "do not match any message (useful for debugging)"
  (declare (ignore message environment))
  ())

(defun copy-array-add-zeroeth-element (array element)
  (let* ((dimensions (array-dimensions array))
         (element-type (array-element-type array))
         (adjustable (adjustable-array-p array))
         (fill-pointer (when (array-has-fill-pointer-p array)
                         (fill-pointer array)))
         (new-array (make-array (list (incf (car dimensions)))
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (loop for i from 0 below (length array)
         do
         (setf (aref new-array (+ 1 i)) (aref array i)))
    (setf (aref new-array 0) element)
    new-array))

(defun match-regexp-binding-list (regexp binding-list &key use-full-match)
  "create a function that will match the given regexp and return a match value and a list of bindings"
  (lambda (message environment)
    (declare (ignore environment))
    (multiple-value-bind (matches sub-matches)
        (cl-ppcre::scan-to-strings regexp (message message))
        (when matches
          (let ((sub-matches 
                 (if use-full-match
                     (copy-array-add-zeroeth-element sub-matches matches)
                     sub-matches)))
            (unless (eql (length sub-matches) (length binding-list))
              (error "binding and match length mis-match~%"))
            (let ((count -1))
              (values 
               t
               (mapcar
                (lambda (var)
                  (incf count)
                  (list var (aref sub-matches count)))
                binding-list))))))))

(defmacro match-regexp2 (regexp string &optional bind-list)
  (let ((thing (cons 'list 
                     (mapcar
                      (lambda (v)
                        `(list (quote ,v) ,v))
                        bind-list))))
    `(register-groups-bind 
      ,bind-list
      (,regexp ,string)
      (values
       t
       ,thing))))

(defun script-return-value (scriptname &rest args)
  (let ((fn (exec-returning-value scriptname args)))
    (lambda (message environment)
      (declare (ignore environment))
      (funcall fn message))))

(defmacro script-return-value-with-arglist (scriptname args)
  `(script-return-value ,scriptname ,@args))

(defmacro with-message (message &body body)
  (let ((environment (gensym)))
    `(lambda (,message ,environment) ,@body)))
