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

(defconstant *syslog-hosts-regexp*
  "(?imsx-imsx:aloe|sage)")

(defconstant *SYSLOG-VERSION-REGEXP*
  "(?imsx-imsx:1.4.1|1.3-3)")

(defconstant *USER-CRON-HOST*
  "ll0[2-4]")

(defconstant *NUMBER-OF-GM-BOARDS*
  "1")

(defconstant *valid-username-regexp*
  "[a-z][a-z0-9-_]{1,7}")

(defconstant *administrator-account*
  "(?imsx-imsx:download|tensile|royh|chimaera|cychan|jatencio|root)")

(defconstant *passwd-changing-host-regexp*
  "(?imsx-imsx:ll0[2-4]|aloe|sage|agave|yucca)")

(defconstant *sudoers*
  "(?imsx-imsx:download|tensile|royh|chimaera|cychan|jatencio|root)")

(defconstant *master-nis-host*
  "agave.alliance.unm.edu")

; local network is: 129.24.240.0 / 21 (129.24.240.0 - 129.24.244.255)
(defconstant *local-network*
  "(?imsx-imsx:129\\.24\\.24[0-4]\\.(?imsx-imsx:[01]?\\d\\d?|2[0-4]\\d|25[0-5]))")
