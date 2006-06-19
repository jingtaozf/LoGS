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

(in-package :org.prewett.LoGS)

(defun fifo-p (filename)
  (< 0 
     (logand 4096 
             (excl.osi:stat-mode (excl.osi:stat filename)))))

(defun get-file-length-from-filename (filename)
  "Given a filename, return the number of bytes currently in the file."
  (excl.osi::stat-size (excl.osi::stat filename)))

(defun open-fifo (filename)
  (let ((fifofd 
         (unix-open
          filename
          (logior EXCL::*O-RDONLY*
                  EXCL::*O-NONBLOCK*)
          #o444)))
    (make-fd-stream fifofd :input t)))

(defun get-inode-from-filename (Filename)
  "Given a filename, return the inode associated with that filename."
  (when (probe-file filename)
    (excl.osi:stat-ino (excl.osi:stat filename))))
