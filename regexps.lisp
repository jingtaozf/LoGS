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

;;;; Useful regular expressions for use in pattern matching rules
;;;; Please do not add any regexps that contain matches 
;;;; *ip-address-regexp* shows how to do this and still use grouping

(defconstant *ip-address-regexp* 
  "(?imsx-imsx:(?imsx-imsx:[01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.(?imsx-imsx:[01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.(?imsx-imsx:[01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.(?imsx-imsx:[01]?\\d\\d?|2[0-4]\\d|25[0-5]))" 
  "This regular expression matches an IP address.")

(defconstant *interface-regexp* 
  "(?imsx-imsx:[a-z]+\\d+)"
  "This regular expression matches an interface (eg. wi0, eth0, fxp0, ... )"
)


(defconstant *Month-regexp*  
  "(?imsx-imsx:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Nov|Dec)"
  "This regular expression matches a month as displayed by syslogd.")

(defconstant *Hour-regexp*
  "[0-2][0-9]"
  "This regular expression matches an hour field.")

(defconstant *Second-regexp*
  "[0-5][0-9]"
  "This regular expression matches a second field.")

(defconstant *Minute-regexp*
  "[0-5][0-9]"
  "This regular expression matches a Mintue field.")

(defconstant *Time-regexp*
  (concatenate
   'string
   *Hour-regexp* ":"
   *Minute-regexp* ":"
   *Second-regexp*))

(defconstant *Day-regexp*
  "(?imsx-imsx:[0-3]| )[0-9]"
  "This regular expression matches a (numeric) Day field")
   

(defconstant *date-time-regexp* 
  (concatenate 
   'string
   *Month-regexp* " "
   *Day-regexp* " "
   *Time-regexp*)
  "This regular expression matches the date and time fields of a syslog message.")

(defconstant *hostname-regexp*
  "(?imsx-imsx:\\w|\\.)+"
  "This regular expression matches a hostname field.")



;; get-inode-from-filename is used because i'm lazy!
;; bad! XXX 
(and (get-inode-from-filename "regexps-local.lsp")
     (not (format t "loading local regexps~%"))
     (load "regexps-local.lsp"))
