(defclass priority-queue-item (doubly-linked-list-item)
  ())

(defclass priority-queue (doubly-linked-list)
  ((comparison-function :initform (lambda (x y) 
                                    (declare (fixnum x y))
                                    (> x y))
                        :initarg :comparison-function 
                        :accessor comparison-function)
   (elements :initform (make-hash-table :test #'equal) :accessor elements)))

;; add an item to the priority queue
(defmethod enqueue ((pq priority-queue) item)
  (let ((x ()))

    (when +debug+
      (format t "inserting item: ~A into priority queue ~A~%" item pq))
    ;; find the right place to insert the item
    (loop initially (setf x (head pq))
          do
          (cond ((or (not x)
                     (not
                      (funcall (comparison-function pq) item (data x))))
                 (progn
                   (when +debug+ (format t "inserting before item: ~A~%" (when x (data x))))
                   (return (dll-insert pq x item :direction :before))))
                ((eq x (tail pq))
                 (progn
                   (when +debug+ (format t "inserting after item: ~A~%" (when x (data x))))
                   (return (dll-insert pq x item :direction :after))))
                (t (setf x (rlink x)))))))

(defmethod check-limits ((pq priority-queue))
  (loop as dlli = (head pq)
        when +debug+
        do (format t "checking pq: ~A dlli: ~A~%" pq dlli)
        when (not dlli)
        do
        (return)
        when (not (check-limits (data dlli)))
        do 
        (return)
        when t
        do
        (dll-delete pq dlli)
        (setf dlli (rlink dlli))))