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

(defun nstr (string n)
    (let ((str ""))
      (loop as x from 0 below n do 
            (setf str (concatenate 'string str string)))
      str))

(defun zero-pad-left (str len)
  (concatenate 'string
               (nstr "0" (- len (length str)))
               str))

(defun make-beginning-zeroes-optional (str)
  (let ((zeroes ""))
    (loop for x from 0 to (length str)
          until (not (char= #\0 (aref str x)))
          do
          (setf zeroes (format () "~A~A" zeroes "0?"))
          finally
          (return (values zeroes (subseq str x (length str)))))))

(defun range1 (start-string end-string &optional seen)
  (let ((ret "")
        (found seen))
    (loop for y from 0 to (- (length start-string) 1)
          do
          (setf found seen)
          (loop for x from 0 to (- (length start-string) 1)
                do
                (cond ((eq x 0)
                       (progn
                         ;(format t "setting ret~%")
                         (setf ret (format () "~A~A" 
                                           ret 
                                           (if (and (not found) (eq #\0 (aref start-string x)))
                                               "0?"
                                               (aref start-string x))))))
                      ((< x y)
                       (setf ret 
                             (format () "~A~A" ret
                                     (if (and (not found) (eq #\0 (aref start-string x)))
                                         "0?"
                                         (progn
                                           (setf found t)
                                           (aref start-string x)))))
                       )
                      (t (setf ret (format () "~A~A" 
                                           ret 
                                           (cond ((> x y)
                                                  (format () "[~A-9]~A|" 
                                                          (+ 1 (read-from-string (format () "~A" (aref start-string x))))
                                                          (nstr "[0-9]" (- (length start-string) y 2))))
                                                 (t (if (and (not found) (eq #\0 (aref start-string x)))
                                                        "0?"  
                                                        (progn
                                                          (setf found t)
                                                          (aref start-string x)))
                                                    ))))))
                
                ))
    ret))


(defun range2 (start-string end-string)
  (let ((end-digit ( - (read-from-string 
                    (format () "~A" 
                            (aref end-string 0))) 1))
        (start-digit 
         (read-from-string 
          (format () "~A" 
                  (aref start-string 0)))))
    
    (if (<= start-digit end-digit)
        (format () "[~A-~A]~A" 
                (aref start-string 0)
                end-digit
                (nstr "[0-9]" (- (length start-string) 1))))))
  
(defun range3 (start-string end-string)
  (if (equal start-string end-string)
      start-string
      (let ((end-len (length end-string))
            (ret ""))
        (loop as x from 0 below (- end-len 1)
              do 
              (loop as y from 0 below (- end-len x 1)
                    do
                    (setf ret (format () "~A~A" ret (aref end-string y)))
                    finally
                    (loop as z from y below end-len
                          do
                          (if (> z y)
                              (if (char= #\0 (aref end-string z))
                                  (setf ret (format () "~A0" ret))
                                  (setf ret (format () "~A[0-9]" ret)))
                              (setf ret (format () "~A[0-~A]" ret (let ((upper (- (read-from-string (format () "~A" (aref end-string z))) 1)))
                                                                    (if (<= 0 upper)
                                                                        upper
                                                                        0)))))
                          (if (and (>= z (- (length start-string) 1))
                                   (not (eq x (- (length start-string) 2))))
                              (setf ret (format () "~A|" ret))))))
                                        ;(format t "ret: ~A~%" ret)
        (concatenate 'string end-string (if (not (equal ret "")) "|") ret))))

(defun range-to-regexp (start-string end-string)
  (let* ((start-len (length start-string))
         (end-len (length end-string))
         (start-int (read-from-string start-string))
         (end-int (read-from-string end-string))
         (padded-start-string (zero-pad-left start-string end-len))
         (len-diff (- end-len start-len))
         (shared "")
         (ret ""))
    ;;shared digits
    (loop as q from 0 below (- end-len 1)
          until (char-not-equal (aref padded-start-string q) (aref end-string q))
          do
          ;(format t "adding: ~A~%" (aref padded-start-string q))
          (setf shared (format () "~A~A" shared (aref padded-start-string q)))
          finally
          ;(format t "q is: ~A~%shared is: ~A" q shared)
          ;(format t "~A not eq ~A in ~A and ~A~%" (aref padded-start-string q) (aref end-string q) padded-start-string end-string)
          (let ((r1 (range1 (subseq padded-start-string q end-len) 
                            (format () "~A~A" 
                                    (+ 1 (read-from-string 
                                          (format () "~A" 
                                                  (aref padded-start-string q))))  
                                    (nstr "0" (- end-len q 1)))
                            (not (eq shared ""))))
                (r2 (range2 
                     (format () "~A~A" 
                             (+ 1 (read-from-string 
                                   (format () "~A" 
                                           (aref padded-start-string q))))  
                             (nstr "0" (- end-len q 1)))
                     (format () "~A~A"
                             (aref end-string q)
                             (nstr "0" (- end-len q 1)))))
                (r3 (range3 
                     (format () "~A~A"
                             (aref end-string q)
                             (nstr "0" (- end-len q 1)))
                     (subseq end-string q end-len))))
            (return
              (if r2
                  (progn 
                    ;(format t "found r2~%")
                    (format () "~A(?imsx-imsx:~A|~A|~A)"
                          shared
                          r1
                          r2
                          r3
                          ))
                  (format () "~A(?imsx-imsx:~A|~A)"
                          shared
                          r1
                          r3
                          )))))))

(defun stringdiff (start end)
  (let ((same "")
        (startdiff "")
        (enddiff ""))
        (loop as x from 0 to (- (length start) 1)
              until (char-not-equal (aref start x) (aref end x))
              do
              (setf same (format () "~A~A" same (aref start x)))
              finally
              (setf startdiff (subseq start x (length start)))
              (setf enddiff  (subseq end x (length start))))
        (values same startdiff enddiff)))
         

(defun range-to-regexp (start-string end-string)
  (let* ((start-len (length start-string))
         (end-len (length end-string))
         (start-int (read-from-string start-string))
         (end-int (read-from-string end-string))
         (padded-start-string (zero-pad-left start-string end-len))
         (len-diff (- end-len start-len))
         ;(shared "")
         (ret ""))
    ;;shared digits
    (multiple-value-bind
          (shared start end)
        (stringdiff padded-start-string end-string)
      (if (and (equal start "") (equal end ""))
          shared
          (let ((r1 (range1 start
                            (format () "~A~A" 
                                    (+ 1 (read-from-string 
                                          (format () "~A" 
                                                  (aref start 0))))
                                    (nstr "0" (- (length start) 1)))
                            (not (equal shared ""))))
                (r2 (range2 
                     (format () "~A~A" 
                             (+ 1 (read-from-string 
                                   (format () "~A" 
                                           (aref start 0))))  
                             (nstr "0" (- (length start) 1)))
                     (format () "~A~A"
                             (aref end 0)
                             (nstr "0" (- (length start) 1)))))
                (r3 (range3 
                     (format () "~A~A"
                             (aref end 0)
                             (nstr "0" (- (length start) 1)))
                     end)))
            
            (if r2
                (format () "~A(?imsx-imsx:~A|~A|~A)"
                        shared
                        r1
                        r2
                        r3
                        )
                (format () "~A(?imsx-imsx:~A|~A)"
                        shared
                        r1
                        r3
                        )))))))


;; code stolen from Dave... 

(defun verify-ip (ip)
  (let ((ips (cl-ppcre::split "\\." ip)))
    (if (or
         (not (equal (length ips) 
                     4))     ; not four subs
         (not
          (let ((first (read-from-string (car ips)))
                (second (read-from-string (cadr ips)))
                (third (read-from-string (caddr ips)))
                (fourth (read-from-string (cadddr ips))))
            (and (not (> first 255))  ; sub > 255
                 (not (> second 255))
                 (not (> third 255))
                 (not (> fourth 255))
                 (not (< first 0))    ; sub < 255
                 (not (< second 0))
                 (not (< third 0))
                 (not (< fourth 0))))))
        ()
        ip)))

(defun verify-cidr (cidr)
  (multiple-value-bind (matches sub-matches)
      (cl-ppcre::scan-to-strings
       "((\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+))/(\\d+)"
       cidr)
    (declare (ignore matches))
    (and sub-matches
         (let ((mask (aref sub-matches 5))
               (ip (aref sub-matches 0)))
           (and
            (verify-ip ip)
            (< (read-from-string mask) 31)
            (> (read-from-string mask) 0))))))

;;; 0K
(defun int-to-bitvec (int &optional length)
  (let ((arr (make-array (if length
                             (if (>= length (integer-length int))
                                 length
                                 (error "length is not large enough to hold integer"))
                             (integer-length int))
                         :element-type 'bit)))
    (loop for x from 0 below (length arr)
          do
          (setf (aref arr x)
                (ldb (byte 1 x) int)))
    arr))

;;; OK
(defun bitvec-to-int (bitvec)
  (let ((int 0))
    (loop for x from 0 below (length bitvec)
          do
          (setf int (+ int (ash (aref bitvec x) x))))
    int))

;; works!
(defun ip-to-bitvec (ip)
  (let ((ips (cl-ppcre::split "\\." ip))
        (arr (make-array '(0) :element-type 'bit)))
    (mapcar (lambda (x) (setf arr 
                              (concatenate '(vector bit) 
                                           (int-to-bitvec (read-from-string x) 8) arr))) 
            ips)
    arr))

;; works!
(defun bitvec-to-ip (bitvec)
  (let ((ip ""))
    (loop as x from 0 below (length bitvec) by 8
          do 
          (setf ip 
                (concatenate 
                 'string
                 ;ip
                 (format () "~A~A" (bitvec-to-int (subseq bitvec x (+ 8 x))) 
                         (if (> x 0) "." ""))
                 ip)))
    ip))

;; works!
(defun build-bitmask (bits)
  (let ((arr (make-array '(32) :element-type 'bit)))
    (loop for x from 0 below bits
          do
          (setf (aref arr (- 31 x)) 1))
          
    arr))

;; broken!
(defun cidr-to-regexp (cidr)
  (if (verify-cidr cidr)
      (destructuring-bind (ip sig-bits)
          (cl-ppcre::split "\/" cidr)
        (let* ((ip-vec (ip-to-bitvec ip))
               (mask-vec (bit-not (build-bitmask (read-from-string sig-bits))))
               (broadcast (bit-ior mask-vec ip-vec))
               (ip-parts (cl-ppcre::split "\\." ip))
               (broadcast-parts (cl-ppcre::split "\\." (bitvec-to-ip broadcast)))
               (ret ""))
          (loop for x from 0 to 3 do
                (setf ret (format () "~A~A" ret (range-to-regexp (nth x ip-parts) (nth x broadcast-parts))))
                (if (< x 3)
                    (setf ret (format () "~A." ret))))
          ret))
      (error "CIDR block: ~A is invalid!~%" cidr)))
