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

;; don't use internal time?
#+cmu
(ext:defswitch "-no-internal-time" #'(lambda (switch) 
                                       (setq
                                        *use-internal-real-time* ())))


;; the name of the file to process
#+cmu
(ext:defswitch "-file" #'(lambda (switch)
                           (let ((filename (car (ext:cmd-switch-words switch)))
                                 (position (cadr (ext:cmd-switch-words switch))))
                             (setq *messages* 
                                   (make-instance 'File-Follower 
                                                  :FileName 
                                                  filename))
                             ;; if position is specified, start there
                             (when position
                               (set-file-follower-position
                                *messages*
                                position)))))




;; the names of multiple files
#+cmu
(ext:defswitch 
    "-files" 
    #'(lambda (switch)
        (progn
          (setf *messages*
                (make-instance 'multi-follower))
          (mapcar
           (lambda (filename)
             (let* ((split  (cl-ppcre::split ":" filename))
                    (name (car split))
                    (position-str (cadr split))
                    (position (when position-str 
                                (read-from-string position-str)))
                    (follower (make-instance 
                               'file-follower
                               :filename name)))
               (when position
                 (set-file-follower-position follower position))
               (add-item *messages* follower)))
           (ext:cmd-switch-words switch)))))
          
;; the ruleset
#+cmu
(ext:defswitch "-ruleset" 
    #'(lambda (switch)
        (let ((filename (car (ext:cmd-switch-words switch))))
          (load (compile-file filename)))))

;; run forever?
#+cmu
(ext:defswitch "-run-forever"
    #'(lambda (switch)
        (declare (ignore switch))
        (setq *run-forever* t)))

;; remember which file the message came from?
#+cmu
(ext:defswitch "-remember-file"
    #'(lambda (switch)
        (declare (ignore switch))
        (setq *remember-file* t)))
