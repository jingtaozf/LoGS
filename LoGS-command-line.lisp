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

(defvar *file-list* ())
(defvar *ruleset-list* ())

(defun reload-all-rulesets ()
  (warn "reloading rulesets")
  (setf *root-ruleset* (make-instance 'ruleset))
  (load-all-rulesets))

(defgeneric load-ruleset (ruleset))


(defvar *fasl-string*
  #-cmucl "fasl"
  #+cmucl "x86f"
  )

(defmethod load-ruleset ((ruleset string))
  (let* ((c-f-p (compile-file-pathname ruleset))
         (c-f-t (and (probe-file c-f-p)
                     (sb-posix:stat-mtime
                      (sb-posix:stat c-f-p))))
         (r-t (sb-posix:stat-mtime
               (sb-posix:stat ruleset))))
    (if (and c-f-t (> c-f-t r-t))
        (PROGN
          (format t "loading compiled ruleset~%")
          (load c-f-p))
        (progn
          (format t "compiling ruleset~%")
          (load (compile-file ruleset))))))


(defmethod load-ruleset ((ruleset pathname))
  (load-ruleset (namestring ruleset)))

(defun load-all-rulesets ()
  (when *ruleset-list*
      (mapcar
       (lambda (ruleset-file)
         (format t "loading ruleset file: ~A~%" ruleset-file)
         (if ruleset-file
             (load-ruleset ruleset-file)))
       *ruleset-list*)))

(defun process-command-line (opts args)
  (progn
    (LoGS-debug "processing options~%")
    (PROCESS-OPTIONS opts args)
    (let ((len (length *file-list*)))
      (cond ((eq 1 len) (setf *messages* (car *file-list*)))
            ((eq 0 len) (setf *messages* (make-instance 'STDIN-follower)))
            (t 
             (progn
               (setf *messages* 
                     (make-instance 'org.prewett.LoGS::multi-follower))
               (mapcar (lambda (ff) (add-item *messages* ff))
                       *file-list*)))))
    ;; SET INITIAL TIME HERE ??? 
    (cond (*use-internal-real-time*
           (setq *now* (get-internal-real-time)))
          (t
           (warn "unknown initial time value~%")
           ))
    (load-all-rulesets)))

;;; some helper functions

;; turn on offline mode
(defun set-no-internal-time ()
  (setq
   org.prewett.LoGS::*use-internal-real-time* ()
   org.prewett.LoGS::*LoGS-internal-time-units-per-second* 1))

;; 

;; The options list

(setf *opts*
      (list
       (make-instance 'cli-opt
                      :name "no-internal-time"
                      :arguments ()
                      :action #'set-no-internal-time
                      :description "do not use internal time")
       (make-instance 'cli-opt
                      :name "timestamp-start"
                      :arguments '(start-value)
                      :action 
                      (lambda (start-value)
                        (setf LoGS::*timestamp-start* 
                              (parse-integer start-value :junk-allowed t)))
                      :description "the start of the timestamp for offline mode")
       (make-instance 'cli-opt
                      :name "timestamp-end"
                      :arguments '(end-value)
                      :action 
                      (lambda (end-value)
                        (setf LoGS::*timestamp-end* 
                              (parse-integer end-value :junk-allowed t)))
                      :description "the end of the timestamp for offline mode")
       (make-instance 'cli-opt
                      :name "parse-timestamp"
                      :arguments NIL
                      :action
                      (lambda ()
                        (set-no-internal-time)
                        (setf LoGS::*parse-timestamp* t))
                      :description "set *NOW* using the timestamp on each line")
       (make-instance 'cli-opt
                      :name "file"
                      :arguments '(filename &optional position)
                      ;; XXX needs to change, get rid of *messages*
                      :action #'(lambda (filename &optional position)
                                  (let ((ff (make-instance 'org.prewett.LoGS::File-Follower 
                                                           :FileName 
                                                           filename)))
                                    ;; if position is specified, start there
                                    (when (or position *start-from-end*)
                                      (cond ((not ff) (error "no ff~%"))
                                            ((or (equal position "end")
                                                 (equal position "END")
                                                 *start-from-end*)
                                             (org.prewett.LoGS::set-file-follower-position
                                              ff
                                              (get-file-length-from-filename (filename ff))))
                                            (t (org.prewett.LoGS::set-file-follower-position
                                                ff
                                                (read-from-string position)))))
                                    (push ff *file-list*)))
                      :description "name of the file to process and optional position")
       (make-instance 'cli-opt
                      :name "PBS-file"
                      :arguments '(filename &optional position)
                      :action #'(lambda (filename &optional position)
                                  (let ((ff (make-instance 'org.prewett.LoGS::PBS-File-Follower
                                                           :FileName
                                                           filename)))
                                    ;; if position is specified, start there
                                    (when position
                                      (org.prewett.LoGS::set-file-follower-position
                                       org.prewett.LoGS::*messages*
                                       (read-from-string position)))
                                    (push ff *file-list*)))
                      :description "follow a sequence of PBS log files starting with the named file")
       
       (make-instance 'cli-opt
                      :name "mysql-follower"
                      :arguments '(host database username password query &optional buffer-size)
                      :action 
                  
                      (if +use-sql+
                          #'(lambda (host database username password query &optional buffer-size)
                              (if +use-sql+
                                  (let ((ff 
                                         (if (and buffer-size (integerp (read-from-string buffer-size)))
                                             (make-instance 
                                              'org.prewett.LoGS::buffered-sql-Follower
                                              :username username
                                              :password password
                                              :host host
                                              :database database
                                              :thequery query
                                              :buffer-size buffer-size
                                              )
                                             (make-instance 
                                              'org.prewett.LoGS::buffered-sql-Follower
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
                      :name "files"
                      :arguments '(&rest filenames)
                      :action 
                      #'(lambda (&rest filenames)
                          (progn
                            (setf org.prewett.LoGS::*messages*
                                  (make-instance 'org.prewett.LoGS::multi-follower))
                            
                            (mapcar
                             (lambda (filename)
                               (let* ((split  (cl-ppcre::split ":" filename))
                                      (name (car split))
                                      (position-str (cadr split))
                                      (position 
                                       (when position-str 
                                         (read-from-string position-str)))
                                      (follower (make-instance 
                                                 'org.prewett.LoGS::file-follower
                                                 :filename name)))
                                 (when position
                                   (org.prewett.LoGS::set-file-follower-position 
                                    follower position))
                                 (push follower *file-list*)))
                             filenames)))
                      :description "names of files to process and optional position (separated by colons eg. logfile:42)")

       (make-instance 'cli-opt
                      :name "spawn" 
                      :arguments '(command &rest args)
                      :action
                      #'(lambda (command &rest args)
                          (let ((spawn (make-instance 'spawn 
                                                      :spawnprog command
                                                      :spawnargs args)))
                           
                            (push spawn *file-list*)))
                      :description
                      "spawn the named command (with optional arguments) and use its output as an input source for LoGS")
                      
       (make-instance 'cli-opt
                      :name "ruleset"
                      :arguments '(ruleset)
                      :action
                      #'(lambda (ruleset)
                          (push ruleset *ruleset-list*))
                      :description "name of the ruleset to load")

       (make-instance 'cli-opt
                      :name "run-forever"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*run-forever* t))
                      :description "don't exit when there is no more immediately available input")
       (make-instance 'cli-opt
                      :name "tail"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*run-forever* t))
                      :description "an alias for --run-forever")
       (make-instance 'cli-opt
                      :name "show-profile"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*show-profile* t))
                      :description "store the name of the file that a given message came from in the from-file slot of the message if set")
       (make-instance 'cli-opt
                      :name "remember-file"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*remember-file* t))
                      :description "store the name of the file that a given message came from in the from-file slot of the message if set")
       (make-instance 'cli-opt
                      :name "start-from-end"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*start-from-end* t))
                      :description "store the name of the file that a given message came from in the from-file slot of the message if set")
       (make-instance 'cli-opt
                      :name "tag-messages"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*tag-messages* t))
                      :description
                      "allow messages to be given tags")

       (make-instance 'cli-opt
                      :name "count-rules"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*count-rules* t))
                      :description
                      "count the number of times a rule is matched/attempted")
       (make-instance 'cli-opt
                      :name "expire-contexts-on-exit"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (setq org.prewett.LoGS::*run-before-exit*
                                (cons
                                 #'expire-all-contexts
                                 org.prewett.LoGS::*run-before-exit*)))
                      :description
                      "expire all remaining contexts before exiting LoGS")

       (make-instance 'cli-opt
                      :name "sleep-time"
                      :arguments '(seconds)
                      :action
                      #'(lambda (seconds)
                          (setf *LoGS-sleep-time* (read-from-string seconds)))
                      :description
                      "how long to sleep when there are no messages to process")
   
       (make-instance 'cli-opt
                      :name "pid"
                      :arguments '(pidfile)
                      :action 
                      #'(lambda (pidfile)
                          (setf *write-pid-to-file* pidfile))
                      :description
                      "the name of the file to write LoGS' PID to")

       (make-instance 'cli-opt
                      :name "LoGS-version"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (progn
                            (format t "This is LoGS version ~A~%" +LoGS-version+)
                            (format t "Copyright (C) 2003-2007 James E. Prewett~%This is free software; see the source for copying conditions.  There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~%")
                            (quit-LoGS)))
                      :description
                      "print version and Copyright information for the program")
                            
       (make-instance 'cli-opt
                      :name "help"
                      :arguments ()
                      :action
                      #'(lambda ()
                          (progn
                            (org.prewett.cl-cli::help org.prewett.LoGS::*opts*)
                            (quit-LoGS)))
                      :description "display this help text")
       (make-instance 'cli-opt
                      :name "run-before-exit"
                      :arguments '(func)
                      :action
                      #'(lambda (func)
                          (let ((split-func
                                 (cl-ppcre::split
                                  ":{1,2}"
                                  func)))
                            (setf LoGS::*run-before-exit* 
                                  (cons 
                                   (if (cdr split-func)
                                       (intern 
                                        (cadr split-func)
                                        (car split-func))
                                       (intern func))
                                   LoGS::*run-before-exit*)))))
       (make-instance 'cli-opt
                      :name "dont-quit-lisp"
                      :arguments ()
                      :action
                      (lambda ()
                        (setf LoGS::*quit-lisp-when-done* NIL))
                      :description "do not terminate the Lisp process when LoGS is done")
       (make-instance 'cli-opt
                      :name "compile-only"
                      :arguments ()
                      :action
                      (lambda ()
                        (setf LoGS::*compile-only* t)))
       (make-instance 'cli-opt
                      :name "repl"
                      :arguments ()
                      
                      :action
                      (lambda ()
                         (setf LoGS::*do-repl* t)))))
