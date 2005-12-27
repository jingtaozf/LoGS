;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2005 James Earl Prewett

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

(in-package :LoGS)

(defvar *file-list* ())

(defun process-command-line (opts args)
  (progn
    (LoGS-debug "processing options~%")
    (PROCESS-OPTIONS opts args)
    (let ((len (length *file-list*)))
      (cond ((eq 1 len) (setf *messages* (car *file-list*)))
            ((eq 0 len) (setf *messages* (make-instance 'STDIN-follower)))
            (t (progn
                 (setf *messages* (make-instance 'LoGS::multi-follower))
                 (mapcar (lambda (ff) (add-item *messages* ff)) *file-list*))))))))

(setf *opts*
      (list
       (make-instance 'cli-opt
                      :name "--no-internal-time"
                      :arguments ()
                      :action #'(lambda () 
                                  (setq
                                   LoGS::*use-internal-real-time* ()))
                      :description "do not use internal time")
   
       (make-instance 'cli-opt
                      :name "--file"
                      :arguments '("<filename>" "[position]")
                      ;; XXX needs to change, get rid of *messages*
                      :action #'(lambda (filename &optional position)
                                  (let ((ff (make-instance 'LoGS::File-Follower 
                                                           :FileName 
                                                           filename)))
                                    ;; if position is specified, start there
                                    (when position
                                      (when (not ff) (error "no ff~%"))
                                      (LoGS::set-file-follower-position
                                       ff
                                       (read-from-string position)))
                                    (push ff *file-list*)))
                      :description "name of the file to process and optional position")
       (make-instance 'cli-opt
                      :name "--PBS-file"
                      :arguments '("<filename>")
                      :action #'(lambda (filename &optional position)
                                  (let ((ff (make-instance 'LoGS::PBS-File-Follower
                                                           :FileName
                                                           filename)))
                                    ;; if position is specified, start there
                                    (when position
                                      (LoGS::set-file-follower-position
                                       LoGS::*messages*
                                       (read-from-string position)))
                                    (push ff *file-list*)))
                      :description "follow a sequence of PBS log files starting with the named file")
       
       (make-instance 'cli-opt
                      :name "--mysql-follower"
                      :arguments '("<host>" "<database>" "<username>" "<password>" "<query>" "[buffer-size]")
                      :action 
                  
                      (if +use-sql+
                          #'(lambda (host database username password query &optional buffer-size)
                              (if +use-sql+
                                  (let ((ff 
                                         (if (and buffer-size (integerp (read-from-string buffer-size)))
                                             (make-instance 
                                              'LoGS::buffered-sql-Follower
                                              :username username
                                              :password password
                                              :host host
                                              :database database
                                              :thequery query
                                              :buffer-size buffer-size
                                              )
                                             (make-instance 
                                              'LoGS::buffered-sql-Follower
                                              :username username
                                              :password password
                                              :host host
                                              :database database
                                              :thequery query
                                              ))))
                                    (push ff *file-list*))))
                          #'(lambda (host database username password query &optional buffer-size)
                              (progn
                                (format t "SQL support does not exist in this build of LoGS... exiting~%")
                                (quit-LoGS))))
                      :description 
                      (if +use-sql+ 
                          "follow the result of a database query"
                          "follow the result of a database query *DISABLED*"
                          ))

       (make-instance 'cli-opt
                      :name "--files"
                      :arguments '("<filename>" "...")
                      :action 
                      #'(lambda (&rest filenames)
                          (progn
                            (setf LoGS::*messages*
                                  (make-instance 'LoGS::multi-follower))
                            
                            (mapcar
                             (lambda (filename)
                               (let* ((split  (cl-ppcre::split ":" filename))
                                      (name (car split))
                                      (position-str (cadr split))
                                      (position 
                                       (when position-str 
                                         (read-from-string position-str)))
                                      (follower (make-instance 
                                                 'LoGS::file-follower
                                                 :filename name)))
                                 (when position
                                   (LoGS::set-file-follower-position 
                                    follower position))
                                 (push follower *file-list*)))
                             filenames)))
                      :description "names of files to process and optional position (separated by colons eg. logfile:42)")

       (make-instance 'cli-opt
                      :name "--spawn" 
                      :arguments '("<command>" "...")
                      :action
                      #'(lambda (command &rest args)
                          (let ((spawn (make-instance 'spawn 
                                                      :spawnprog command
                                                      :spawnargs args)))
                           
                            (push spawn *file-list*)))
                      :description
                      "spawn the named command (with optional arguments) and use its output as an input source for LoGS")
                      
       (make-instance 'cli-opt
                      :name "--ruleset"
                      :arguments '("<ruleset>")
                      :action
                      #'(lambda (ruleset)
                          (in-package :LoGS)
                          (load (compile-file ruleset)))
                      :description "name of the ruleset to load")

       (make-instance 'cli-opt
                      :name "--run-forever"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*run-forever* t))
                      :description "don't exit when there is no more immediately available input")
       (make-instance 'cli-opt
                      :name "--tail"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*run-forever* t))
                      :description "an alias for --run-forever")
       (make-instance 'cli-opt
                      :name "--remember-file"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*remember-file* t))
                      :description "store the name of the file that a given message came from in the from-file slot of the message if set")

       (make-instance 'cli-opt
                      :name "--tag-messages"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*tag-messages* t))
                      :description
                      "allow messages to be given tags")

       (make-instance 'cli-opt
                      :name "--count-rules"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*count-rules* t))
                      :description
                      "count the number of times a rule is matched/attempted")
       (make-instance 'cli-opt
                      :name "--expire-contexts-on-exit"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq LoGS::*run-before-exit*
                                (cons
                                 #'expire-all-contexts
                                 LoGS::*run-before-exit*)))
                      :description
                      "expire all remaining contexts before exiting LoGS")

       (make-instance 'cli-opt
                      :name "--sleep-time"
                      :arguments '("<seconds>")
                      :action
                      #'(lambda (seconds)
                          (setf *LoGS-sleep-time* (read-from-string seconds)))
                      :description
                      "how long to sleep when there are no messages to process")
   
       (make-instance 'cli-opt
                      :name "--pid"
                      :arguments '("<pidfile>")
                      :action 
                      #'(lambda (pidfile)
                          (setf *write-pid-to-file* pidfile))
                      :description
                      "the name of the file to write LoGS' PID to")

       (make-instance 'cli-opt
                      :name "--LoGS-version"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (progn
                            (format t "This is LoGS version ~A~%" +LoGS-version+)
                            (format t "Copyright (C) 2004 James E. Prewett~%This is free software; see the source for copying conditions.  There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~%")
                            (quit-LoGS)))
                      :description
                      "print version and Copyright information for the program")
                            
       (make-instance 'cli-opt
                      :name "--help"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (progn
                            (cl-cli::help LoGS::*opts*)
                            (quit-LoGS)))
                      :description "display this help text")
       ))
