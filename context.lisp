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

(in-package :LoGS)

(defvar *contexts-hash* (make-hash-table :test #'equal)
  "a hash to hold all of their contexts so that we can find them by name.")

(defvar *contexts-alias-hash* (make-hash-table :test #'equal))

(defvar *contexts* (make-instance 'doubly-linked-list)
  "The current set of contexts.")

(defclass context (limited-collection timeout-object relative-timeout-object killable-item)
  ((actions :initarg :actions
                  :initform ()
                  :accessor actions
                  :documentation "A list of functions to call when the context dies")
   (max-lines :initarg :max-lines
              :initform ()
              :accessor max-lines
              :documentation "An artificial limit on the number of lines this context can hold")

   (min-lines :initarg :min-lines
              :initform ()
              :accessor min-lines
              :documentation "The minumum number of lines in the context for it to run its actions.")

   (lives-after-timeout :initarg :lives-after-timeout
                        :initform ()
                        :accessor lives-after-timeout))
  (:documentation "A data structure that stores messages."))

;; when createing an instance of a context,
;; if a context with the same name already exists, return it
;; else, return a new instance with that name
(defmethod make-instance :around ((instance (eql 'CONTEXT)) &rest rest &key name &allow-other-keys)
  (declare (ignore rest))
  (progn
    (when +debug+ 
      (if (and name (get-context name))
          (format t "a context named ~A already exists~%" name)))
    (or
     (when name
       (get-context name))
     (let ((inst (call-next-method)))
       (setf (gethash (name inst) *contexts-hash*) inst)))))

;; after we initialize the context, put it into the contexts collection
(defmethod initialize-instance :after ((instance context) &rest rest)
  (declare (ignore rest))
  (progn
    (enqueue *contexts* instance)
    (setf (gethash (name instance) *contexts-hash*) instance)))

(defgeneric delete-context (context)
  (:documentation "remove a context from the namehash."))

(defmethod delete-context ((context context))
  (delete-context (name context)))

(defmethod delete-real-context ((context t))
  (when context
    (when +debug+ (format t "deleting context: ~A~%" context))
    (let ((c (gethash context *contexts-hash*)))
      (when c
        (kill c)
        (dll-delete *contexts* c)
        (remhash context *contexts-hash*)
        (dll-delete *timeout-object-timeout-queue* c)
        (dll-delete *relative-timeout-object-timeout-queue* c)))))

; delete a context by name
;; if its in the alias, remove that
;; else, remove the actual context (assuming that exists)
(defmethod delete-context ((context t))
  (when context
    (if (gethash context *contexts-alias-hash*)
        (remhash context *contexts-alias-hash*)
        (delete-real-context context))))

(defgeneric alias-context (context alias)
  (:documentation "give a context an alias"))

(defmethod alias-context ((context context) alias)
  (setf (gethash alias *contexts-alias-hash*) context))

(defmethod alias-context (context alias)
  (setf (gethash alias *contexts-alias-hash*) 
        (get-context context)))

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
   (gethash name *contexts-alias-hash*)
   (gethash name *contexts-hash*)))

(defgeneric add-to-context (name message)
  (:documentation "Add a message to a context with the given name."))

(defmethod add-to-context (name (message message))
  (let ((context (get-context name)))
    (if context
        (add-to-context context message)
        (error "couldn't find context: ~A~%" name))))

(defmethod add-to-context ((context context) (message message))
  (add-item context message))

;; what about adding a context to a context?

(defmethod add-to-context ((context context) (add-context context))
  (progn
    (when +debug+
      (format t "adding context: ~A to context: ~A~%" add-context context))
    (loop for i from 0 below (ecount add-context) 
       do
         (add-to-context context (aref (data add-context) i)))))

(defmethod add-item :after ((context context) item &rest rest)
  (declare (ignore rest))
  (let ((max-lines (max-lines context)))
    (when (and max-lines (> (ecount context) max-lines))
      (expire-context context))))

;; run a context's actions then delete it.
(defgeneric expire-context (context)
  (:documentation "cause a context to run its actions, then be deleted"))

(defmethod expire-context ((context context))
  (let ((min-lines (min-lines context)))
  
    (when +debug+ (format t "expiring context: ~A named: ~A~%" 
                          context (name context)))
  
    (when (or 
           (not min-lines)
           (> (ecount context) min-lines))
      (run-context-actions context))
           
    (if
     (lives-after-timeout context)
     (update-relative-timeout context)
     (progn
       (when +debug+
         (format t "deleteing context: ~A" context))
       (delete-context context)))))

(defmethod initialize-instance
    :around
    ((c context)
     &rest args
     &key name
     &allow-other-keys)
    (declare (ignore args))
    (if (gethash name *contexts-hash*) ; name exits
        (call-next-method)
        (progn
          (enqueue *contexts* c)
          (call-next-method)
          (setf (gethash name *contexts-hash*)
                c))))

(defgeneric run-context-actions (context)
  (:documentation
   "Run the actions associated with a context."))

(defmethod run-context-actions ((context context))
  "Run the actions associated with a context."
  (progn
    (when +debug+
      (format t "running context actions~%"))
    (when (actions context)
      (mapcar 
       (lambda (x) 
         (declare (function x))
         (funcall x context))
       (actions context)))))

 (defmethod check-limits :around ((context context))
   (or (dead-p context)
       (let ((ret (call-next-method)))
         (when ret 
           (expire-context context)
           (dll-delete *contexts* context))
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
  `(progn
    (when +debug+ 
      (if (and ,name (get-context ,name))
          (format t "a context named ~A already exists at~%" ,name (get-context ,name))))
    (or
     (when ,name
       (get-context ,name))
     (make-instance 'context
      ,@rest))))

;; expire all remaining contexts

(defun expire-all-contexts ()
    (let ((context (head *contexts*)))
      (loop 

         when context
         do
           (let ((next (rlink context)))
             (expire-context (data context))
             (setq context next))
                        
         when (not context)
         do
           (return t))))

(defun reset-contexts-hash () (setf *contexts-hash* (make-hash-table :test #'equal)))

(defun reset-contexts-alias-hash () (setf *contexts-alias-hash* (make-hash-table :test #'equal)))

(defun reset-contexts () (setf *contexts* (make-instance 'doubly-linked-list)))

(defun clear-contexts ()
  "remove all contexts"
  (progn
    (reset-contexts-hash)
    (reset-contexts-alias-hash)
    (reset-contexts)))

