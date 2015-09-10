;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2008 James Earl Prewett

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


#+cmu
(use-package :system)
#+cmu
(use-package :unix)

#+sbcl
(use-package :sb-sys)
#+sbcl
(use-package :sb-unix)

#+sbcl
(require :sb-sprof)

#+sbcl 
(sb-profile:PROFILE)

;; this is starting to become a bit of a mess!
(defmacro with-LoGS-interrupts (interrupts &body body)
  #+sbcl
  `(progn ,@body)
  #+cmu
  `(with-enabled-interrupts ,interrupts ,@body))

(defun main ()
  (with-LoGS-interrupts ((SIGINT #'handle-ctrl-c))
    (PROGN
      #+sbcl
      (progn       
        (sb-sys:enable-interrupt 
         sb-unix:sigint 
         (lambda (sig code scp)
           (declare (ignore sig code scp))
           (LoGS::handle-ctrl-c)))
        (sb-sys:enable-interrupt 
         SB-POSIX:SIGHUP 
         (lambda (sig code scp)
           (declare (ignore sig code scp))
           (LoGS::reload-all-rulesets))))

    ;; process any command line options
    (LoGS-debug "processing options~%")
    (let ((args (get-application-args)))
      (process-command-line *opts* args))

    (if *compile-only* (quit-LoGS))

    ;; write out PID if necessary
    (when *write-pid-to-file*
      (progn
        (LoGS-debug "writing PID to file: ~A~%" *write-pid-to-file*)
        (let ((PID 
               #+cmu
                (unix:unix-getpid)
                #+sbcl
                (sb-unix:unix-getpid)))
          (with-open-file
              (file *write-pid-to-file*
                    :direction :output
                    :if-exists :overwrite
                    :if-does-not-exist :create)
            (format file "~A~%" PID)))))
  
    ;; thread for watching timeouts
    #+sb-thread
    (sb-thread:make-thread #'watch-timeouts-thread-func)
    
    ;; process any files
    (if *do-repl*
        (progn
          #+sbcl
          (SB-IMPL::TOPLEVEL-INIT)
          #+cmu
          (LISP::%TOP-LEVEL)
          #+allegro
          (TPL:TOP-LEVEL-READ-EVAL-PRINT-LOOP))
        (if *show-profile*
        (process-files) ;; XXX
        (Process-files)))

    ;; call any exit functions
    (mapcar
     (lambda (function)
       (funcall function))
     *run-before-exit*)
    ;; exit LoGS
    (when *quit-lisp-when-done*
      (quit-LoGS)))))


(defun watch-timeouts-thread-func ()
  (loop
     (check-limits *timeout-object-timeout-queue*)
     (check-limits *relative-timeout-object-timeout-queue*)
     ;; sleep a bit
     (sleep *LoGS-sleep-time*)
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
     if *use-internal-real-time*
     do
       (setq *now* (get-internal-real-time))
     else if (and *message* *parse-timestamp*)
     do
       ;; what to do if the time doesn't parse?
       ;; for now, just keep the last timestamp and move on
       (aif (cybertiggyr-time:parse-time
             (subseq 
              (message *message*)
              *timestamp-start*
              *timestamp-end*)
             (list (cybertiggyr-time::make-fmt-recognizer 
                    *timestamp-format*)))
            (setq *now* it))
     end
     end
              
     ;; check the message against the ruleset if it exists
     ;; and check the timeout objects

     if *message*
     do
       (LoGS-debug "got message: ~A~%" (IF *MESSAGE* (message *message*)))
       (check-rules *message* *root-ruleset* NIL)

     when t
     do
       (check-limits *timeout-object-timeout-queue*)
       (check-limits *relative-timeout-object-timeout-queue*)  

     when (not *message*)
     do
       (LoGS-debug "no message~%")
       (if *run-forever*
           ;; sleep a bit
           (progn
             (sleep *LoGS-sleep-time*)
             (LoGS-debug "sleeping~%"))
           ;; exit if there is no message and we're not running forever
           (return-from processing))))
