
(defvar *opts*
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
                                (push ff *file-list*))))
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
                  :name "--remember-file"
                  :arguments ()
                  :action
                  #'(lambda ()
                      (setq LoGS::*remember-file* t)))))