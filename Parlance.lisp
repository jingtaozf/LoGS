
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


;;; i'm stealing lots of stuff from SEC here.
;;; Risto did a good job!

;;; Basically, this file is here to make things a little higher-level
;;; than the LoGS core provides by itself.

;;; ways to ignore things

;; filter out matching messages
(defun filter (match-func &key name)
  "create a rule that causes messages that match to be ignored"
  (make-instance 
   'rule
   :name name
   :match match-func))

;; make sure that I can filter out messages using filter
;; eg. they won't be seen by subsequent rules.


(defun suppress (match-func timeout &key name)
  "create a rule that causes messages that match to be ignore for a certain period of time from *now*"
  (make-instance
   'rule
   :name name
   :match match-func
   :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))))

;; suppress messages matching match1 until match2 is seen
(defun suppress-until (match1 match2 &key name)
  (make-instance 'rule
                 :name name
                 :match
                 (lambda (message)
                   (if (funcall match2 message)
                       (progn
                         (setf (dead-p (data *current-rule*)) t)
                         ())
                       (funcall match1 message)))))

(defun single (match-func actions-list &key name)
  "create a rule that triggers the functions in the actions list as a result of a match"
  (make-instance 
   'rule
   :name name
   :match match-func
   :actions actions-list))

(defun single-with-suppress (match-func actions-list time &key name)
  "run the actions in the actions list in response to a match, then add a rule before this one to ignore this message for a while"
  (make-instance 'rule
                 :name name
                 :match match-func
                 :actions (append 
                           actions-list
                           (rule-before
                            (suppress match-func time)))))

(defun pair (match1 actions1 match2 actions2 &key name name1 name2)
  "like SEC pair rule."
  (make-instance 'rule
                 :name name
                 :match match1
                 :actions
                 (append
                  (list
                   ;; make a rule to ignore match1 until match2 is seen
                   (lambda (message matches sub-matches)
                     (rule-before
                      (suppress-until match1 match2 :name name1)))
                   ;; make a rule to trigger actions2 when match2 is seen
                   (lambda (message matches sub-matches)
                     (rule-before
                      (make-instance 'rule
                                     :name name2
                                     :match match2
                                     :delete-rule (lambda (message) t)
                                     :actions actions2))))
                  actions1)))


;;; XXX finish me!
(defun pair-with-window (match1 actions1 match2 actions2 missing-actions window 
                         &key name1 name2)
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
                 :match match1
                 ;:timeout timeout
                 :actions
                 (append
                  (list
                   ;; make a rule to ignore match1 until match2 is seen
                   (lambda (message matches sub-matches)
                     (rule-before
                      (suppress-until match1 match2)))
                   ;; make a rule to trigger actions2 when match2 is seen
                   (lambda (message matches sub-matches)
                     (rule-before
                      (make-instance 'rule
                                     :name match2-rule-name
                                     :match match2
                                     :timeout timeout
                                     :delete-rule (lambda (message) t)
                                     :actions actions2))))
                  actions1))))


(defun long-message (long actions-list &key name)
  "create a rule that matches messages longer than long.  trigger the functions in the actions list as a result."
  (Single
   (lambda (message)
     (> (length (message message)) long))
   actions-list
   :name name))


(defun print-context (context)
  (when (> (ecount context) 0)
    (map-into 
     (make-array (list (ecount context)))
     (lambda (x) (format t "~A~%" (message x)))
     (data context))))


(defmethod gather-into (match (context context) &key name)
  (make-instance 'rule
                 :name name
                 :match match
                 ;; the rule should die when the context does
                 :timeout (timeout context)
                 :actions
                 (list
                  (lambda (message matches sub-matches)
                    (declare (ignore matches sub-matches))
                    (add-to-context context message)))))
                 
(defmethod gather-into (match context &key name)
  (gather-into match (get-context context) :name name))

(defun gather (match timeout actions &key name)
  (let ((context (make-instance 
                  'context 
                  :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))
                  :actions actions)))
    (gather-into match context :name name)))

(defun gather-related (matchlist timeout actions &key names)
  (let ((context (make-instance
                  'context
                  :timeout (+ *now* (* timeout INTERNAL-TIME-UNITS-PER-SECOND))
                  :actions actions)))
    (if names
        (mapcar
         (lambda (match name)
           (rule-before
            (gather-into match context :name name)))
         matchlist
         names)
        (mapcar
         (lambda (match)
           (rule-before
            (gather-into match context)))
         matchlist))))
                       



;; throttle action.
(defmacro throttle (timeout &key name)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(lambda (,a ,b ,c)
      (rule-before
       (make-instance 'rule
        :name ,name
        :match (match
                (data
                 *current-rule*))
        :timeout (+ *now* (* ,timeout INTERNAL-TIME-UNITS-PER-SECOND)))))))

;;;; action function creating functions

(defun print-log ()
  (lambda (message matches sub-matches)
    (format t "~A~%" (message message))))

;;;; match functions

(defun match-all (message) 
  (declare (ignore message)) 
  t)