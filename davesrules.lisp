;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2006 James Earl Prewett

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

(load "apachehelp")

;; Match important apache log information from access_log format
;; Really I would love to replace the nasty let statement below with a destructuring statement...
(enqueue *ruleset*
         (make-instance 'rule
                        :match
                        (lambda (message)
                          (cl-ppcre::scan-to-strings
                           (concatenate 'string
                                        "(" *ip-address-regexp* ")"
                                        "\\s+\\-\\s+\\-\\s+\\[(.+?)\\]\\s+\\\"GET\\s+(.+?)\\s+(HTTP/\\d.\\d)\\\"\\s+\(\\d+)\\s+(\\d+)\\s+\\\"(.+?)\\\"\\s+\\\"(.+?)\\\"\\s*$") 
                           (message message)))

                        :actions
                        (list
                         (lambda (message matches sub-matches)
                           ; name all of the vector elements... should be done with a destructuring method
                           (let* ((ip (svref sub-matches 0))
                                 (time (svref sub-matches 1))
                                 (date (car (cl-ppcre:split ":" time :limit 2)))
                                 (URL (svref sub-matches 2))
                                 (HTTP-ver (svref sub-matches 3))
                                 (status-code (svref sub-matches 4))
                                 (bytes-sent (svref sub-matches 5))
                                 (referrer (svref sub-matches 6))
                                 (browser (svref sub-matches 7)))
                             (progn
                               ;; just print something for the kids...
                               ;(format t "~A fetched ~A on ~A~%" ip URL time)

                               ;; record this tresspass against our weak and innocent web server
                               (ApacheHelp:add-dayhit time)
                               
                               (let ((worm-list '(("system32\/cmd.exe\\\?\/c\\\+dir" 'nimda)
                                                  ("\/default.ida\\\?N{10,}"         'codered)
                                                  )))
                                 
                                 (labels ((worm-finder (ls)
                                                       (if ls (progn
                                                                (format t "Now checking for ~A in ~A~%" (caar ls) URL)
                                                                (if (cl-ppcre:scan (caar ls) URL)
                                                                    (progn
                                                                      (apachehelp:add-worm-hit ip (cadar ls) time)
                                                                      ;;(format t "Matched ~A~%" (cadar ls))
                                                                      )
                                                                (worm-finder (cdr ls)))))))
                                         (worm-finder worm-list)))
                               )
                             
                             )
                           )
                         )
                        )
         )

