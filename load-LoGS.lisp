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

;; borrowed heavily from Edi's load.lisp

(defun load-LoGS-file (filename)
  (let ((compiled-filename (compile-file-pathname filename)))
    (when (or 
           (not (probe-file compiled-filename)) ; no compiled file
           (> (file-write-date filename)
              (file-write-date compiled-filename))) ; old compiled file

      (compile-file filename))
    (load compiled-filename)))

(with-compilation-unit ()
  
  (let ((LoGS-files 
         (list
          '(()
            ("LoGS"))

          '("data_structures"
            ("doubly-linked-list"
             "priority-queue"))
          
          '(()
            ("message" "named-object" "timeout-object" "relative-timeout-object"
           "killable-item" "collection" "limited-collection" "context"
             "window"))
          
          (list 
           "Data_Sources"
           (list
            #+cmu 
            "File-Follower_CMUCL.low"
            #+sbcl
            "File-Follower_SBCL.low"
            #+openmcl
            "File-Follower_OpenMCL.low"
            #+allegro
            "File-Follower_Allegro.low"
            #+clisp
            "File-Follower_CLISP.low"
            #+lispworks
            "File-Follower_LispWorks.low"
            
            "Data-Source" "List-Follower" "File-Follower" "PBS-File-Follower"
            "Spawn" "STDIN-Follower" "Multi-Follower"
            "Buffered-SQL-Follower"))
          '(()
            ("rule" "ruleset" "actions" "Parlance" "LoGS-command-line"))
          ))
        (LoGS-home (make-pathname :name () :type () :defaults (parse-namestring *load-truename*))))
    (mapcar 
     (lambda (x)
       (let ((dir (car x)))
         (mapcar 
          (lambda (file)
            (when file
              (let* ((load-filename (merge-pathnames (make-pathname :name file :type "lisp" :directory (if dir `(:relative ,dir))) LoGS-home)))
                (load-LoGS-file load-filename))))
          (cadr x))
         ))
     LoGS-files)))
