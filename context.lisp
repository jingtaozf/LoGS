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

(defclass context (limited-collection timeout-object relative-timeout-object killable-item environment-object)
  ((actions 
    :initarg :actions
    :initform ()
    :accessor actions
    :documentation "A list of functions to call when the context dies")
   (max-lines 
    :initarg :max-lines
    :initform ()
    :accessor max-lines
    :documentation 
    "An artificial limit on the number of lines this context can hold")
   (min-lines 
    :initarg :min-lines
    :initform ()
    :accessor min-lines
    :documentation 
    "The minumum number of lines in the context for it to run its actions.")
   (lives-after-timeout 
    :initarg :lives-after-timeout
    :initform ()
    :accessor lives-after-timeout))
  (:documentation "A data structure that stores messages."))

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (name) obj
      (format stream "~A" name))))

;;; context name/alias related stuff
(defvar *contexts-hash* (make-hash-table :test #'equal)
  "a hash to hold all of their contexts so that we can find them by name.")

(defvar *contexts-alias-hash* (make-hash-table :test #'equal))

;; context name hash manipulation stuff
(defmethod context-hash-name ((context context))
  (gethash (name context) *contexts-hash*))

(defmethod context-hash-name (context)
  (gethash context *contexts-hash*))

(defmethod set-context-hash-name ((context1 context) (context2 context))
  (setf (gethash (name context1) *contexts-hash*) context2))

(defmethod set-context-hash-name (context1 (context2 context))
  (setf (gethash context1 *contexts-hash*) context2))
   
(defsetf context-hash-name set-context-hash-name)

;; context alias hash manipulation stuff
(defmethod context-hash-alias ((context context))
  (gethash (name context) *contexts-alias-hash*))

(defmethod context-hash-alias (context)
  (gethash context *contexts-alias-hash*))

(defmethod set-context-hash-alias ((context1 context) (context2 context))
  (setf (gethash (name context1) *contexts-alias-hash*) context2))

(defmethod set-context-hash-alias (context1 (context2 context))
  (setf (gethash context1 *contexts-alias-hash*) context2))

(defsetf context-hash-alias set-context-hash-alias)

(defgeneric alias-context (context alias)
  (:documentation "give a context an alias"))

(defmethod alias-context ((context context) alias)
  (setf (gethash alias *contexts-alias-hash*) context))

(defmethod alias-context (context alias)
  (setf (gethash alias *contexts-alias-hash*) 
        (get-context context)))

(defvar *contexts* (make-instance 'doubly-linked-list)
  "The current set of contexts.")

;; reset things

(defun reset-contexts-hash () (setf *contexts-hash* (make-hash-table :test #'equal)))

(defun reset-contexts-alias-hash () (setf *contexts-alias-hash* (make-hash-table :test #'equal)))

(defun reset-contexts () (setf *contexts* (make-instance 'doubly-linked-list)))

(defun clear-contexts ()
  "remove all contexts"
  (progn
    (reset-contexts-hash)
    (reset-contexts-alias-hash)
    (reset-contexts)))

;; after we initialize the context, put it into the contexts collection
(defmethod initialize-instance :after ((instance context) &rest rest)
  (declare (ignore rest))
  (register-context instance))

(defmethod register-context ((context context))
  (enqueue *contexts* context)
  (setf (context-hash-name context) context))

(defmethod unregister-context ((context context))
  (dll-delete *contexts* context)
  (remhash (name context) *contexts-hash*))

(defgeneric delete-context (context)
  (:documentation "remove a context from the namehash."))

(defmethod delete-context ((context context))
  (delete-context (name context)))

(defmethod delete-real-context ((context t))
  (when context
    (LoGS-debug "deleting context: ~A~%" context)
    (let ((c (context-hash-name context)))
      (when c
        (kill c)
        (unregister-context c)
        (dll-delete *timeout-object-timeout-queue* c)
        (dll-delete *relative-timeout-object-timeout-queue* c)))))

; delete a context by name
;; if its in the alias, remove that
;; else, remove the actual context (assuming that exists)
(defmethod delete-context ((context t))
  (when context
    (if (context-hash-alias context)
        (remhash context *contexts-alias-hash*)
        (delete-real-context context))))

(defgeneric remove-context-if-stale (context time)
  (:documentation "remove a context if it is stale."))

(defmethod remove-context-if-stale ((context context) time)
  (and
   (context-exceeded-limit-p context time)
   (delete-context context)))

(defgeneric get-context (name)
  (:documentation "Return the context named name if it exists."))

(defmethod get-context (name)
  (or
   (context-hash-alias name)
   (context-hash-name name)))

(defgeneric add-to-context (name message)
  (:documentation "Add a message to a context with the given name."))

(defmethod add-to-context (name (message message))
  (let ((context (get-context name)))
    (if context
        (add-to-context context message)
        (error "couldn't find context: ~A~%" name))))

(defmethod add-to-context ((context context) (message message))
  (add-item context message))

(defmethod add-to-context ((context context) (add-context context))
  (progn
    (LoGS-debug "adding context: ~A to context: ~A~%" add-context context)
    (loop for i from 0 below (ecount add-context) 
       do
         (add-to-context context (aref (data add-context) i)))))

(defmethod add-item :after ((context context) item &rest rest)
  (declare (ignore rest))
  (with-slots (max-lines ecount) context
    (when (and max-lines (> ecount max-lines))
      (expire-context context))))

;; run a context's actions then delete it.
(defgeneric expire-context (context)
  (:documentation "cause a context to run its actions, then be deleted"))

(defmethod expire-context ((context context))
  (with-slots (min-lines ecount) context
    
    (LoGS-debug "expiring context: ~A named: ~A~%" context (name context))
  
    (when (or 
           (not min-lines)
           (> ecount min-lines))
      (run-context-actions context))
           
    (if
     (lives-after-timeout context)
     (update-relative-timeout context)
     (progn
       (LoGS-debug "deleteing context: ~A" context)
       (delete-context context)))))

(defgeneric run-context-actions (context)
  (:documentation
   "Run the actions associated with a context."))

(defmethod run-context-actions ((context context))
  "Run the actions associated with a context."
  (LoGS-debug "running context actions~%")
  (with-slots (actions) context
    (when actions
      (mapcar 
       (lambda (x) 
         (declare (function x))
         (funcall x context))
       actions))))

 (defmethod check-limits :around ((context context))
   (or (dead-p context)
       (let ((ret (call-next-method)))
         (when ret 
           (expire-context context))
         ret)))

(defmethod destroy-context-if-exceeded-limit ((context context))
  (check-limits context))

(defgeneric context-exceeded-limit-p (context time)
  (:documentation "check to see if a context has exceeded one of its limits"))

(defmethod context-exceeded-limit-p ((context context) time)
  (let ((*now* time))
    (check-limits  context)))

(defmethod write-context ((context context) stream)
  (loop for i from 0 below (ecount context) 
        do
        (format stream "~A~%" 
                (message 
                 (aref
                  (data context)
                  i)))))

;; ensure-context is the new replacement for the fooky make-instance
;; apparently the standard frowns upon not returning a *NEW* instance from
;; make-instance.  Sometimes compilers are too smart and cause a new instance
;; to always be returned. 

(defmacro ensure-context (&rest rest &key name &allow-other-keys)
  (let ((context-name (gensym)))
    `(let ((,context-name (get-context ,name)))
      (if ,context-name
          (progn
            (LoGS-debug "a context named ~A already exists at~%" 
                        ,context-name)
            ,context-name)
          (make-instance 'context ,@rest)))))

;; expire all remaining contexts

(defun expire-all-contexts ()
  (loop
     for context = (head *contexts*) then next
     as next = (when context (rlink context))
     when context
     do
       (expire-context (data context))
       (setq context next)
     else
     do
       (return t)))

