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

;; this is very simple, better implementations will come... seems to work for now!

(defmacro exec (program &rest args)
  `(lambda (messages) 
    (declare (ignore messages))
    #+cmu
    (extensions:run-program ,program (quote ,args) :wait () :output t)
    #+sbcl
    (run-program ,program (quote ,args) :wait () :output t)
    #+openmcl
    (run-program ,program (quote ,args) :wait () :output t)
    ))

(defmacro file-write (filename)
  `(lambda (message)
    (with-open-file 
        (file ,filename 
         :direction :output 
         :if-exists :append 
         :if-does-not-exist :create)
      (format file "~A~%" (message message)))))

(defmethod pipe ((message message) (program string) &rest args)
  #+cmu
  (extensions:run-program 
   program args 
   :wait t 
   :input (make-string-input-stream (message message)))
  #+sbcl
  (run-program 
   program args 
   :wait t 
   :input (make-string-input-stream (message message))))

(defmethod pipe ((context context) (program string) &rest args)
  (let ((output-stream (make-string-output-stream)))

    (format output-stream "Context: ~A~%" (name context))
    (write-context context output-stream)

    #+cmu
    (extensions:run-program
     program args
     :wait t
     :input (make-string-input-stream (get-output-stream-string output-stream)))
    #+sbcl
    (run-program
     program args
     :wait t
     :input (make-string-input-stream (get-output-stream-string output-stream)))))

(defmethod mail ((context context) (recipient string) &optional subject)
  (if subject
      (pipe context "/bin/mail" "-s" subject recipient)
      (pipe context "/bin/mail" recipient)))


(defmethod mail ((message message) (recipient string) &optional subject)
  (if subject
      (pipe message "/usr/bin/mail" "-s" subject recipient)
      (pipe message "/usr/bin/mail" recipient)))