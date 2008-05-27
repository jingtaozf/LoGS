;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2008 James Earl Prewett

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

(defclass Spawn (Data-Source)
  ((SpawnStream :accessor SpawnStream :initform ())
   (spawnprog :accessor spawnprog :initform () :initarg :spawnprog)
   (spawnargs :accessor spawnargs :initform () :initarg :spawnargs)))

(defmethod cleanup ((FF Spawn))
  (CLOSE (SpawnStream FF)))

;; generic way to launch or program
;; returns a stream we can do stuff with
(defun spawn-prog (program args)
  #+cmu
  (extensions:process-output
   (run-program program args :output :stream :wait ()))
  #+sbcl
  (sb-ext:process-output
   (run-program program args :output :stream :wait ()))

  #+openmcl
  (ccl:external-process-output-stream 
   (ccl:run-program program args :output :stream :wait ()))
  ;; XXX finish me for Allegro

  #+allegro
  (excl:run-shell-command 
   (format () "~A ~{ ~A~}" program args)
   :output :stream :wait ())

  #+clisp
  (ext:run-program program :arguments args :output :stream :wait ())

  #+lispworks
  (sys::open-pipe 
   (format () "~A ~{ ~A~}" program args)
   :direction :input)
  )

(defgeneric start-spawn (spawn)
  (:documentation "get a spawn all set up"))

(defmethod start-spawn ((spawn spawn))
  (let ((stream (spawn-prog (spawnprog Spawn) (spawnargs Spawn))))
    (setf (spawnstream spawn)
          stream)))


;; make sure that the new spawn has a stream associated
(defmethod initialize-instance :after ((spawn spawn) &rest rest)
  (declare (ignore rest))
  (start-spawn spawn))

(defgeneric get-line (spawn)
  (:documentation "get the next line of input from the spawn"))

(defmethod get-line ((spawn spawn))
  (read-line (spawnstream spawn) () ()))

(defmethod get-logline ((spawn spawn))
  "wrap the next line of output from the spawn in a message."
  (let ((line (get-line spawn)))
    (when line
      (if *remember-file*
          (make-instance 'from-message :message line 
                         :from-file (spawnprog spawn))
          (make-instance 'message :message line)))))
                         
                         
       
