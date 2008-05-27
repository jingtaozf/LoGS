;;;; LoGS extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2006-2008 Vijay Lakshminarayanan

;;;; This file is a part of LoGS.  LoGS is free software; you can
;;;; redistribute it and/or modify it under the terms of the GNU
;;;; General Public License as published by the Free Software
;;;; Foundation; either version 2 of the License, or (at your option)
;;;; any later version.

;;;; This file is a part of LoGS.  The copyright will soon be
;;;; transferred to the author of LoGS, James Earl Prewett.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defpackage :org.prewett.LoGS.language
  (:use #:cl #:cybertiggyr-time #:logs)
  ; (:import-from #:logs #:rule #:ruleset #:message #:exec #:context)
  (:import-from #:cybertiggyr-time #:make-broken-time)
  (:nicknames #:language)
  (:export timeout))

;;; *eof*
