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


;;; i'm stealing lots of stuff from SEC here.
;;; Risto did a good job!

;;; Basically, this file is here to make things a little higher-level
;;; than the LoGS core provides by itself.

;; wrappers to call make-instance

(defmacro rule (&rest rest)
  `(make-instance 'rule ,@rest))

(defmacro context (&rest rest)
  `(ensure-context ,@rest))

(defmacro ruleset (&rest rest)
  `(make-instance 'ruleset ,@rest))

;;; ways to ignore things

;; filter out matching messages
(defun filter (match-func &key name environment continuep)
  "create a rule that causes messages that match to be ignored"
  (make-instance 
   'rule
   :name name
   :continuep continuep
   :environment environment
   :match match-func))

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
  (make-instance 'rule
                 :name name
                 :continuep continuep
                 :environment environment
                 :match
                 (lambda (message)
                   (or
                    (funcall match1 message)
                    (funcall match2 message)))
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


;; (defun pair (match1 actions1 match2 actions2 &key name name1 name2)
;;   "like SEC pair rule."
;;   (make-instance 'rule
;;                  :name name
;;                  :match match1
;;                  :actions
;;                  (append
;;                   (list
;;                    ;; make a rule to ignore match1 until match2 is seen
;;                    (lambda (message)
;;                      (rule-before
;;                       (suppress-until match1 match2 :name name1)))
;;                    ;; make a rule to trigger actions2 when match2 is seen
;;                    (lambda (message)
;;                      (rule-before
;;                       (make-instance 'rule
;;                                      :name name2
;;                                      :match match2
;;                                      :delete-rule (lambda (message) t)
;;                                      :actions actions2))))
;;                   actions1)))


(defun pair (match1 actions1 match2 actions2 &key continuep environment)
  (make-instance 
   'rule
   :match match1
   :continuep continuep
   :environment environment
   :actions
   (append
    (list
     (lambda (message)
       (let ((ign-match1 (make-instance 'rule
                                        :match match1))
             (find-match2 (make-instance 'rule
                                         :match
                                         match2
                                         :environment
                                         env
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
         (context (make-instance 'context
                                 :timeout timeout
                                 :actions 
                                 ;; XXX add stuff to delete rules to actions list
                                 (append
                                  (list
                                   (lambda (context)
                                     (declare (ignore context))
                                     (progn
                                       ;(kill (get-rule *ruleset* match1-rule-name))
                                       (kill (get-rule *ruleset* match2-rule-name)))))
                                  missing-actions))))
    ;; rule to match first message and create rule to match second message
  (make-instance 'rule
                 :name match1-rule-name ; does this need a name?
                 :continuep continuep
                 :environment environment
                 :match match1
                 ;:timeout timeout
                 :actions
                 (append
                  (with-sub-matches-as-dollar-things
                      (list
                       ;; make a rule to ignore match1 until match2 is seen
                       (lambda (message)
                         (rule-before
                          (suppress-until match1 match2)))
                       ;; make a rule to trigger actions2 when match2 is seen
                       (lambda (message)
                         (rule-before
                          (make-instance 'rule
                                         :name match2-rule-name
                                         :match match2
                                         :timeout timeout
                                         :delete-rule (lambda (message) t)
                                         :actions actions2))))
                    actions1)))))

;; XXX don't like this!
(defun long-message (long actions-list &key name environment continuep)
  "create a rule that matches messages longer than long.  trigger the functions in the actions list as a result."
  (Single
   (lambda (message)
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
  (make-instance 'rule
                 :name name
                 :match match
                 ;; the rule should die when the context does
                 :timeout (timeout context)
                 :environment environment
                 :continuep continuep
                 :actions
                 (list
                  (lambda (message)
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
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(lambda (,a ,b ,c)
      (rule-before
       (make-instance 'rule
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
  (lambda (message)
    (format t "~A~%" (message message))))

(defun echo ()
  "an alias for print-log"
  (print-log))

;;;; match functions

(defun match-all (message) 
  "match every message"
  (declare (ignore message)) 
  t)

(defun match-none (message)
  "do not match any message (useful for debugging)"
  (declare (ignore message))
  ())

(defun match-regexp (regexp)
  "create a function that will match the given regular expression"
  (lambda (message)
    (cl-ppcre::scan-to-strings
     regexp
     (message message))))
