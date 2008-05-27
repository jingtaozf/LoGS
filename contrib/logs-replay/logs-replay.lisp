
(defvar *time-diff* NIL)
(setf *parse-timestamp* t
      *use-internal-real-time* NIL)

(setf *opts*
      (list
       (make-instance 'cli-opt
                      :name :default
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
                      :description "name of the file to process and optional position")))

(enqueue *root-ruleset*
         (rule matching #'match-all
               doing
               (lambda (message env)
                 (let ((real-time (get-universal-time)))
                   (unless *time-diff*
                     (setf *time-diff* (- real-time *now*)))
                   (when (> (+ *now* *time-diff*) real-time)
                     (sleep (- (+ *now* *time-diff*) real-time)))
                   (format t "~A~%" (message message))))))