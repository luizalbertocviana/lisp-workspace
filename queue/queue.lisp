(defpackage :queue
  (:use :common-lisp)
  (:export
     :queue :new :empty? :enqueue :dequeue))

(in-package :queue)

(defstruct queue
  "represents a linked list queue data structure"
  data last-cell)

(defun new ()
  "returns an empty queue"
  (make-queue :data nil
              :last-cell nil))

(defun empty? (queue)
  "determines whether queue is empty"
  (declare (type queue queue))
  (declare (optimize (speed 3) (debug 0)))
  (the boolean (null (queue-data queue))))

(defun dequeue (queue)
  "removes element at the beginning of queue"
  (declare (type queue queue))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((element (car (queue-data queue))))
    (pop (queue-data queue))
    (when (null (queue-data queue))
      (setf (queue-last-cell queue) nil))
    element))

(defun enqueue (queue element)
  "inserts element at the end of queue"
  (declare (type queue queue))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((cell (cons element nil)))
    (if (null (queue-data queue))
        (setf (queue-data queue) cell)
        (setf (cdr (queue-last-cell queue)) cell))
    (setf (queue-last-cell queue) cell)
    (the queue queue)))
