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

;; (defvar *context-timeout-queue*
;;   (make-instance 'priority-queue 
;;                  :comparison-function 
;;                  (lambda (x y)
;;                    (let ((t-x (timeout x))
;;                          (t-y (timeout y)))
;;                      (declare (fixnum t-x t-y))
;;                      (> t-x t-y))))
;;   "A priority queue to hold contexts that can time out.")

(defvar *contexts-hash* (make-hash-table :test #'equal)
  "a hash to hold all of their contexts so that we can find them by name.")

(defun reset-contexts-hash () (setf *contexts-hash* (make-hash-table :test #'equal)))

(defvar *contexts-alias-hash* (make-hash-table :test #'equal))

(defun reset-contexts-alias-hash () (setf *contexts-alias-hash* (make-hash-table :test #'equal)))

(defvar *contexts* (make-instance 'doubly-linked-list)
  "The current set of contexts.")

(defun reset-contexts () (setf *contexts* (make-instance 'doubly-linked-list)))

(defun clear-contexts ()
  "remove all contexts"
  (progn
    (reset-contexts-hash)
    (reset-contexts-alias-hash)
    (reset-contexts)))

(defclass context (collection timeout-object killable-item)
  ((actions :initarg :actions
                  :initform ()
                  :accessor actions
                  :documentation "A list of functions to call when the context dies")
   (max-lines :initarg :max-lines
              :initform ()
              :accessor max-lines
              :documentation "An artificial limit on the number of lines this context can hold"))
  (:documentation "A data structure that stores messages."))

;; (defmethod (setf timeout) :after (new-value (context context))
;;   (declare (ignore new-value))
;;   (progn
;;     (dll-delete *context-timeout-queue* context)
;;     (enqueue *context-timeout-queue* context)))

;; when createing an instance of a context,
;; if a context with the same name already exists, return it
;; else, return a new instance with that name
(defmethod make-instance :around ((instance (eql 'CONTEXT)) &rest rest &key name &allow-other-keys)
  (declare (ignore rest))
  (or
   (and name
        (get-context name))
   (let ((inst (call-next-method)))
     (setf (gethash (name inst) *contexts-hash*) inst))))

;; after we initialize the context, put it into the contexts collection
(defmethod initialize-instance :after ((instance context) &rest rest)
  (declare (ignore rest))
  (enqueue *contexts* instance))


(defgeneric delete-context (context)
  (:documentation "remove a context from the namehash."))

(defmethod delete-context ((context context))
  (delete-context (name context)))

(defmethod delete-real-context ((context t))
  (when context
    (let ((c (gethash context *contexts-hash*)))
      (dll-delete *contexts* c)
      (remhash context *contexts-hash*)
      (dll-delete *timeout-object-timeout-queue* c))))

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
    (add-to-context context message)))

(defmethod add-to-context ((context context) (message message))
  (add-item context message))

;; if this context is full, run actions and kill it!
(defmethod add-to-context :after ((context context) (message message))
  (when (context-exceeded-limit-p context *now*)
    (expire-context context)))

;; run a context's actions then delete it.
(defgeneric expire-context (context)
  (:documentation "cause a context to run its actions, then be deleted"))

(defmethod expire-context ((context context))
  (progn
    (run-context-actions context)
    (delete-context context)))

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

(defgeneric context-exceeded-limit-p (context time)
  (:documentation "check to see if a context has exceeded one of its limits"))

(defmethod context-exceeded-limit-p ((context context) time)
  (or
   ; context is old
   (when (timeout context)
     (> time (timeout context)))
   ; context is full
   (when (max-lines context)
     (> (Ecount context) (max-lines context)))))
