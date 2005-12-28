;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2005 James Earl Prewett

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


;; a multi-follower follows multiple data sources.
;; all of the nifty functionality should be in the underlying data source.  
;; eg. a file-follower handles the inode rotation stuff.

(in-package :LoGS)

(defclass Multi-Follower (collection Data-Source)
  ((current-follower :initform 0 :accessor current-follower)))

(defmethod get-logline ((Mf Multi-Follower))
  (when (> (Ecount Mf) 0)
    (loop
       with starting-follower = (current-follower Mf)
       for current-follower = starting-follower then (mod (1+ current-follower) 
                                                           (Ecount Mf))
       for line = (get-logline (aref (data Mf) current-follower))
       do
         (setf (current-follower Mf)
               current-follower)
       when line
       do
         (return line)
       when ;; if we've already tried them all, return NIL
         (equal starting-follower
                (mod (current-follower Mf)
                     (ecount Mf)))
       do
         (return NIL)
         )))

    
    
