; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2004 James Earl Prewett

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


;; a multi-follower follows multiple data sources.
;; all of the nifty functionality should be in the underlying data source.  
;; eg. a file-follower handles the inode rotation stuff.

(defclass Multi-Follower (collection)
  ((current-follower :initform 0 :accessor current-follower)))

(defmethod get-logline ((Mf Multi-Follower))
  (when (> (Ecount Mf) 0)
    (let ((starting-follower (current-follower Mf)))
      (loop with current-follower = starting-follower
         do
           (let ((line (get-logline (aref (data Mf) current-follower))))
             (setf (current-follower Mf)
                   (mod (1+ (current-follower Mf)) 
                        (Ecount Mf)))
             (when line
               (return line)))

         when ;; if we've already tried them all, return NIL
           (equal starting-follower
                  (mod (1+ (current-follower Mf))
                       (ecount Mf)))
         do
           (return NIL)

         when t
         do
           (setf current-follower 
                 (mod (1+ current-follower) 
                      (Ecount Mf)))))))
    
    
    