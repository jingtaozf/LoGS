;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2018 James Earl Prewett

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
                     #+sbcl
                     (sb-posix:stat-mtime
                      (sb-posix:stat c-f-p))
                     #-sbcl
                     (error "FIXME")))
         (r-t 
          #+sbcl
           (sb-posix:stat-mtime
            (sb-posix:stat ruleset))
           #-sbcl
           (error "FIXME")
           ))
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
           (setq *now* (get-internal-real-time))))
    (load-all-rulesets)))

;;; some helper functions

;; turn on offline mode
(defun set-no-internal-time ()
  (setq
   org.prewett.LoGS::*use-internal-real-time* ()
   org.prewett.LoGS::*LoGS-internal-time-units-per-second* 1))

;; The options list
'(setf *opts*
      (list
       (defopt ("no-internal-time") () () "do not use internal time"
           (setq
            org.prewett.LoGS::*use-internal-real-time* ()
            org.prewett.LoGS::*LoGS-internal-time-units-per-second* 1))
       (defopt ("timestamp-start") (start-value) () 
           "the start of the timestamp for offline mode"
         (setf LoGS::*timestamp-start* 
               (parse-integer start-value :junk-allowed t)))
       (defopt ("timestamp-end") (end-value) () 
           "the end of the timestamp for offline mode"
         (setf LoGS::*timestamp-end* 
               (parse-integer end-value :junk-allowed t)))
       (defopt ("parse-timestamp") () ()
           "set *NOW* using the timestamp on each line"
         (set-no-internal-time)
         (setf LoGS::*parse-timestamp* t))
       (defopt ("file") (filename &optional position) ()
           "name of the file to process and optional position"
           ;; XXX needs to change, get rid of *messages*
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
       (defopt ("PBS-file") (filename &optional position) ()
            "follow a sequence of PBS log files starting with the named file"
           (let ((ff (make-instance 'org.prewett.LoGS::PBS-File-Follower
                                    :FileName
                                    filename)))
             ;; if position is specified, start there
             (when position
               (org.prewett.LoGS::set-file-follower-position
                org.prewett.LoGS::*messages*
                (read-from-string position)))
             (push ff *file-list*))
         )
       
       (if +use-sql+
           (defopt ("mysql-follower") (host database username password query &optional buffer-size) ()
               "follow the result of a database query"
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
                         :thequery query))))
               (push ff *file-list*)))
           (defopt  ("mysql-follower") (host database username password query &optional buffer-size) ()   
               "follow the result of a database query *DISABLED*"
             (progn
               (format t "SQL support does not exist in this build of LoGS... exiting~%")
               (quit-LoGS))))
       

       (defopt ("files") (&rest filenames) ()
           "names of files to process and optional position (separated by colons eg. logfile:42)" 
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
          filenames))
       (defopt ("spawn" ) (command &rest args) ()
           "spawn the named command (with optional arguments) and use its output as an input source for LoGS"
         (let ((spawn (make-instance 'spawn 
                                     :spawnprog command
                                     :spawnargs args)))
           (push spawn *file-list*)))
       (defopt ("ruleset") (ruleset) ()
           "name of the ruleset to load"
         (push ruleset *ruleset-list*))
       (defopt ("run-forever" "tail") () ()
           "don't exit when there is no more immediately available input"
         (setq org.prewett.LoGS::*run-forever* t))
       (defopt ("show-profile") () ()
           "profile the run and display the profile information"
         (setq org.prewett.LoGS::*show-profile* t))
       (defopt ("remember-file") () ()
           "remember which file a message came from"
         (setq org.prewett.LoGS::*remember-file* t))
       (defopt ("start-from-end") () ()
           "start file-followers from the end of the file"
         (setq org.prewett.LoGS::*start-from-end* t))
       (defopt ("tag-messages") () ()
           "allow messages to be given tags"
           (setq org.prewett.LoGS::*tag-messages* t))
       (defopt ("count-rules") () ()
           "count the number of times a rule is matched/attempted"
           (setq org.prewett.LoGS::*count-rules* t))
       (defopt ("expire-contexts-on-exit") () ()
           "expire all remaining contexts before exiting LoGS"
           (setq org.prewett.LoGS::*run-before-exit*
                 (cons
                  #'expire-all-contexts
                  org.prewett.LoGS::*run-before-exit*)))
       (defopt ("sleep-time") (seconds) ()
           "how long to sleep when there are no messages to process"
         (setf *LoGS-sleep-time* (read-from-string seconds)))
       (defopt ("pid") (pidfile) ()
           "the name of the file to write LoGS' PID to"
         (setf *write-pid-to-file* pidfile))
       (defopt ("LoGS-version") () ()
           "print version and Copyright information for the program"
           (progn
             (format t "This is LoGS version ~A~%" *LoGS-version*)
             (format t "Copyright (C) 2003-2008 James E. Prewett~%This is free software; see the source for copying conditions.  There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~%")
             (quit-LoGS)))
       (defopt ("help") () ()
           "display this help text"
         (progn
           (cl-cli::help org.prewett.LoGS::*opts*)
           (quit-LoGS)))
       (defopt ("run-before-exit") (func) ()
           "run this function before exiting LoGS"
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
                    LoGS::*run-before-exit*))))
       (defopt ("dont-quit-lisp") () ()
           "do not terminate the Lisp process when LoGS is done"
         (lambda ()
           (setf LoGS::*quit-lisp-when-done* NIL)))
       (defopt ("compile-only") () ()
           "only compile rulesets, don't process files"
         (setf LoGS::*compile-only* t))
       (defopt ("repl") () ()
            "run a REPL instead of processing files"
         (setf LoGS::*do-repl* t))))

;; (setf *opts*
;;       (list
;;        (make-instance 'cli-opt
;;                       ("no-internal-time")
;;                       :arguments ()
;;                       :action #'set-no-internal-time
;;                       :description "do not use internal time")
;;        (make-instance 'cli-opt
;;                       ("timestamp-start")
;;                       :arguments '(start-value)
;;                       :action 
;;                       (lambda (start-value)
;;                         (setf LoGS::*timestamp-start* 
;;                               (parse-integer start-value :junk-allowed t)))
;;                       :description "the start of the timestamp for offline mode")
;;        (make-instance 'cli-opt
;;                       ("timestamp-end")
;;                       :arguments '(end-value)
;;                       :action 
;;                       (lambda (end-value)
;;                         (setf LoGS::*timestamp-end* 
;;                               (parse-integer end-value :junk-allowed t)))
;;                       :description "the end of the timestamp for offline mode")
;;        (make-instance 'cli-opt
;;                       ("parse-timestamp")
;;                       :arguments NIL
;;                       :action
;;                       (lambda ()
;;                         (set-no-internal-time)
;;                         (setf LoGS::*parse-timestamp* t))
;;                       :description "set *NOW* using the timestamp on each line")
;;        (make-instance 'cli-opt
;;                       ("file")
;;                       :arguments '(filename &optional position)
;;                       ;; XXX needs to change, get rid of *messages*
;;                       :action #'(lambda (filename &optional position)
;;                                   (let ((ff (make-instance 'org.prewett.LoGS::File-Follower 
;;                                                            :FileName 
;;                                                            filename)))
;;                                     ;; if position is specified, start there
;;                                     (when (or position *start-from-end*)
;;                                       (cond ((not ff) (error "no ff~%"))
;;                                             ((or (equal position "end")
;;                                                  (equal position "END")
;;                                                  *start-from-end*)
;;                                              (org.prewett.LoGS::set-file-follower-position
;;                                               ff
;;                                               (get-file-length-from-filename (filename ff))))
;;                                             (t (org.prewett.LoGS::set-file-follower-position
;;                                                 ff
;;                                                 (read-from-string position)))))
;;                                     (push ff *file-list*)))
;;                       :description "name of the file to process and optional position")
;;        (make-instance 'cli-opt
;;                       ("PBS-file")
;;                       :arguments '(filename &optional position)
;;                       :action #'(lambda (filename &optional position)
;;                                   (let ((ff (make-instance 'org.prewett.LoGS::PBS-File-Follower
;;                                                            :FileName
;;                                                            filename)))
;;                                     ;; if position is specified, start there
;;                                     (when position
;;                                       (org.prewett.LoGS::set-file-follower-position
;;                                        org.prewett.LoGS::*messages*
;;                                        (read-from-string position)))
;;                                     (push ff *file-list*)))
;;                       :description "follow a sequence of PBS log files starting with the named file")
       
;;        (make-instance 'cli-opt
;;                       ("mysql-follower")
;;                       :arguments '(host database username password query &optional buffer-size)
;;                       :action 
                  
;;                       (if +use-sql+
;;                           #'(lambda (host database username password query &optional buffer-size)
;;                               (if +use-sql+
;;                                   (let ((ff 
;;                                          (if (and buffer-size (integerp (read-from-string buffer-size)))
;;                                              (make-instance 
;;                                               'org.prewett.LoGS::buffered-sql-Follower
;;                                               :username username
;;                                               :password password
;;                                               :host host
;;                                               :database database
;;                                               :thequery query
;;                                               :buffer-size buffer-size
;;                                               )
;;                                              (make-instance 
;;                                               'org.prewett.LoGS::buffered-sql-Follower
;;                                               :username username
;;                                               :password password
;;                                               :host host
;;                                               :database database
;;                                               :thequery query
;;                                               ))))
;;                                     (push ff *file-list*))))
;;                           #'(lambda (host database username password query &optional buffer-size)
;;                               (progn
;;                                 (format t "SQL support does not exist in this build of LoGS... exiting~%")
;;                                 (quit-LoGS))))
;;                       :description 
;;                       (if +use-sql+ 
;;                           "follow the result of a database query"
;;                           "follow the result of a database query *DISABLED*"
;;                           ))

;;        (make-instance 'cli-opt
;;                       ("files")
;;                       :arguments '(&rest filenames)
;;                       :action 
;;                       #'(lambda (&rest filenames)
;;                           (progn
;;                             (setf org.prewett.LoGS::*messages*
;;                                   (make-instance 'org.prewett.LoGS::multi-follower))
                            
;;                             (mapcar
;;                              (lambda (filename)
;;                                (let* ((split  (cl-ppcre::split ":" filename))
;;                                       (name (car split))
;;                                       (position-str (cadr split))
;;                                       (position 
;;                                        (when position-str 
;;                                          (read-from-string position-str)))
;;                                       (follower (make-instance 
;;                                                  'org.prewett.LoGS::file-follower
;;                                                  :filename name)))
;;                                  (when position
;;                                    (org.prewett.LoGS::set-file-follower-position 
;;                                     follower position))
;;                                  (push follower *file-list*)))
;;                              filenames)))
;;                       :description "names of files to process and optional position (separated by colons eg. logfile:42)")

;;        (make-instance 'cli-opt
;;                       ("spawn" )
;;                       :arguments '(command &rest args)
;;                       :action
;;                       #'(lambda (command &rest args)
;;                           (let ((spawn (make-instance 'spawn 
;;                                                       :spawnprog command
;;                                                       :spawnargs args)))
                           
;;                             (push spawn *file-list*)))
;;                       :description
;;                       "spawn the named command (with optional arguments) and use its output as an input source for LoGS")
                      
;;        (make-instance 'cli-opt
;;                       ("ruleset")
;;                       :arguments '(ruleset)
;;                       :action
;;                       #'(lambda (ruleset)
;;                           (push ruleset *ruleset-list*))
;;                       :description "name of the ruleset to load")

;;        (make-instance 'cli-opt
;;                       ("run-forever")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*run-forever* t))
;;                       :description "don't exit when there is no more immediately available input")
;;        (make-instance 'cli-opt
;;                       ("tail")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*run-forever* t))
;;                       :description "an alias for --run-forever")
;;        (make-instance 'cli-opt
;;                       ("show-profile")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*show-profile* t))
;;                       :description "profile the run and display the profile information")
;;        (make-instance 'cli-opt
;;                       ("remember-file")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*remember-file* t))
;;                       :description "remember which file a message came from")
;;        (make-instance 'cli-opt
;;                       ("start-from-end")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*start-from-end* t))
;;                       :description "start file-followers from the end of the file")
;;        (make-instance 'cli-opt
;;                       ("tag-messages")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*tag-messages* t))
;;                       :description
;;                       "allow messages to be given tags")

;;        (make-instance 'cli-opt
;;                       ("count-rules")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*count-rules* t))
;;                       :description
;;                       "count the number of times a rule is matched/attempted")
;;        (make-instance 'cli-opt
;;                       ("expire-contexts-on-exit")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (setq org.prewett.LoGS::*run-before-exit*
;;                                 (cons
;;                                  #'expire-all-contexts
;;                                  org.prewett.LoGS::*run-before-exit*)))
;;                       :description
;;                       "expire all remaining contexts before exiting LoGS")

;;        (make-instance 'cli-opt
;;                       ("sleep-time")
;;                       :arguments '(seconds)
;;                       :action
;;                       #'(lambda (seconds)
;;                           (setf *LoGS-sleep-time* (read-from-string seconds)))
;;                       :description
;;                       "how long to sleep when there are no messages to process")
   
;;        (make-instance 'cli-opt
;;                       ("pid")
;;                       :arguments '(pidfile)
;;                       :action 
;;                       #'(lambda (pidfile)
;;                           (setf *write-pid-to-file* pidfile))
;;                       :description
;;                       "the name of the file to write LoGS' PID to")

;;        (make-instance 'cli-opt
;;                       ("LoGS-version")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (progn
;;                             (format t "This is LoGS version ~A~%" *LoGS-version*)
;;                             (format t "Copyright (C) 2003-2008 James E. Prewett~%This is free software; see the source for copying conditions.  There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~%")
;;                             (quit-LoGS)))
;;                       :description
;;                       "print version and Copyright information for the program")
                            
;;        (make-instance 'cli-opt
;;                       ("help")
;;                       :arguments ()
;;                       :action
;;                       #'(lambda ()
;;                           (progn
;;                             (cl-cli::help org.prewett.LoGS::*opts*)
;;                             (quit-LoGS)))
;;                       :description "display this help text")
;;        (make-instance 'cli-opt
;;                       ("run-before-exit")
;;                       :arguments '(func)
;;                       :description "run this function before exiting LoGS"
;;                       :action
;;                       #'(lambda (func)
;;                           (let ((split-func
;;                                  (cl-ppcre::split
;;                                   ":{1,2}"
;;                                   func)))
;;                             (setf LoGS::*run-before-exit* 
;;                                   (cons 
;;                                    (if (cdr split-func)
;;                                        (intern 
;;                                         (cadr split-func)
;;                                         (car split-func))
;;                                        (intern func))
;;                                    LoGS::*run-before-exit*)))))
;;        (make-instance 'cli-opt
;;                       ("dont-quit-lisp")
;;                       :arguments ()
;;                       :action
;;                       (lambda ()
;;                         (setf LoGS::*quit-lisp-when-done* NIL))
;;                       :description "do not terminate the Lisp process when LoGS is done")
;;        (make-instance 'cli-opt
;;                       ("compile-only")
;;                       :description "only compile rulesets, don't process files"
;;                       :arguments ()
;;                       :action
;;                       (lambda ()
;;                         (setf LoGS::*compile-only* t)))
;;        (make-instance 'cli-opt
;;                       ("repl")
;;                       :arguments ()
;;                       :description "run a REPL instead of processing files"
;;                       :action
;;                       (lambda ()
;;                          (setf LoGS::*do-repl* t)))))
