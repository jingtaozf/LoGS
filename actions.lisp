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
  `(lambda (messages matches sub-matches) 
    (declare (ignore messages matches sub-matches))
    #+cmu
    (extensions:run-program ,program (quote ,args) :wait () :output t)
    #+sbcl
    (extensions:run-program ,program (quote ,args) :wait () :output t)
    #+openmcl
    (run-program ,program (quote ,args) :wait () :output t)
    ))

(defmacro pipe (program &rest args)
  (let ((stream (gensym))
        (args (mapcar (lambda (x) (eval x)) args)))
    
    `(lambda (message matches sub-matches)
      (let ((,stream (make-string-input-stream (message message))))
        #+cmu
        (extensions:run-program ,program (quote ,args) :wait t :input ,stream :output t)
        #+sbcl
        (extensions:run-program ,program (quote ,args) :wait t :input ,stream :output t)
        #+openmcl
        (run-program ,program (quote ,args) :wait t :input ,stream :output t)
        ))))

; send mail to the given recipient with the given subject
(defmacro mail (recipient &optional subject)
  `(if ,subject
    (pipe "/usr/bin/mail" "-s" ,subject ,recipient)
    (pipe "/usr/bin/mail" ,recipient)))

(defmacro file-write (filename)
  `(lambda (message)
    (with-open-file 
        (file ,filename 
         :direction :output 
         :if-exists :append 
         :if-does-not-exist :create)
      (format file "~A~%" (message message)))))
