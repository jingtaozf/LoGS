
;; insert GPL notice here

;; Copyright 2006 James E. Prewett

(in-package :org.prewett.LoGS)

(defun main ()
  (progn
    ;; process any command line options
    (LoGS-debug "processing options~%")
    (let ((args (get-application-args)))
      (process-command-line *opts* args))
    
    ;; write out PID if necessary
    (if *write-pid-to-file*
        (progn
          (LoGS-debug "writing PID to file: ~A~%" *write-pid-to-file*)
          (let ((PID 
                 #+cmu
                  (unix:unix-getpid)))
            (with-open-file
                (file *write-pid-to-file*
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
              (format file "~A~%" PID))))
        )
    
    ;; process any files
    (process-files)
    ;; call any exit functions
    (mapcar
     (lambda (function)
       (funcall function))
     *run-before-exit*)
    ;; exit LoGS
    (quit-LoGS)
    ))




;; pretty much the former mainline
;; adding the option processing to the mainline made testing more difficult
;; so I broke the processing out to a separate function.
(defun process-files ()    
  (declare (OPTIMIZE (SPEED 0) (DEBUG 3) (SAFETY 3)))  
  (loop named processing as *message* = (get-logline *messages*)
       
     when +debug+
       do (format t "processing message: ~A~%" (if *message* (message *message*)))
       
     ;; update the internal time
     when *use-internal-real-time*
     do
       (setq *now* (get-internal-real-time))
              
     ;; check the message against the ruleset if it exists
     ;; and check the timeout objects

     if *message*
     do
       (LoGS-debug "got message: ~A~%" (IF *MESSAGE* (message *message*)))
       (check-rules *message* *root-ruleset*)

     else
     do
       (LoGS-debug "no message~%")
       (if *run-forever*
           ;; sleep a bit
           (sleep *LoGS-sleep-time*)
           ;; exit if there is no message and we're not running forever
           (return-from processing))

     when t
     do
       (check-limits *timeout-object-timeout-queue*)
       (check-limits *relative-timeout-object-timeout-queue*)))
