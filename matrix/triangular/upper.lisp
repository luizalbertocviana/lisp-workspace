(defpackage :triangular-matrix/upper
  (:nicknames :tri-upper)
  (:use :common-lisp :macros)
  (:import-from :matrix :add :multiply)
  (:shadow :aref :identity)
  (:export
     :matrix :new-matrix :matrix-type :matrix-dimension
     :aref
     :identity
     :reduce-two-matrices :reduce-matrices
     :incf-product))

(in-package :triangular-matrix/upper)

(defstruct (matrix)
  "represents an upper triangular matrix"
  (data      nil :type matrix:matrix)
  (type)
  (dimension nil :type (integer 0)))

(defun new-matrix (&key (type 'number) dimension (initial-element 0))
  "creates an upper triangular matrix with elements typed to
  type (defaults to number) and dimension. All positions (except the
  lower triangle) are initialized as initial-element (defaults to 0)"
  (make-matrix :data (matrix:new-matrix :type type
                                        :number-rows (ceiling dimension 2)
                                        :number-cols (1+ dimension)
                                        :initial-element initial-element)
               :type type
               :dimension dimension))

(defun new-matrix-like (matrix)
  "creates an upper triangular matrix with same dimension and type of matrix"
  (new-matrix :type (matrix-type matrix)
              :dimension (matrix-dimension matrix)))

(defun aref (matrix row col)
  "element of matrix at position (row col)"
  (if (> row col)
      (coerce 0 (matrix-type matrix))
      (let* ((mat      (matrix-data matrix))
             (dim      (matrix-dimension matrix))
             (mat-rows (matrix:matrix-number-rows mat)))
        (if (< row mat-rows)
            (matrix:aref mat row (+ 1 col))
            (matrix:aref mat (- dim row 1) (- dim col 1))))))

(defun (setf aref) (value matrix row col)
  "sets (row col) position of matrix to value"
  (if (> row col)
      0
      (let* ((mat      (matrix-data matrix))
             (dim      (matrix-dimension matrix))
             (mat-rows (matrix:matrix-number-rows mat)))
        (if (< row mat-rows)
            (setf (matrix:aref mat row (+ 1 col)) value)
            (setf (matrix:aref mat (- dim row 1) (- dim col 1)) value)))))

(defun identity (&key (type 'number) dimension)
  "creates an identity matrix"
  (let ((id (new-matrix :type      type
                        :dimension dimension)))
    (dotimes (i dimension)
      (setf (aref id i i) (coerce 1 type)))
    id))

(defmacro reduce-with-accessors (op matrix-a matrix-b aref-a aref-b result)
  "reduces matrix-a and matrix-b applying op position-wise. Result is
stored in result. Access to matrices is performed using aref-a and aref-b"
  (with-gensyms (dim i j)
    `(let ((,dim (matrix-dimension ,matrix-a)))
       (dotimes (,i ,dim)
         (loop for ,j from ,i below ,dim
               do (setf (aref ,result ,i ,j)
                        (funcall ,op
                                 (,aref-a ,matrix-a ,i ,j)
                                 (,aref-b ,matrix-b ,i ,j))))))))

(defun reduce-two-matrices (op matrix-a matrix-b &key (result nil))
  "reduces matrix-a and matrix-b applying op position-wise. Result is
stored in result. If result is nil, a new matrix is allocated"
  (unless result
    (setf result (new-matrix-like matrix-a)))
  (reduce-with-accessors op matrix-a matrix-b aref aref result))

(defun reduce-matrices (op matrices &key (result nil))
  "reduces matrices applying op position-wise. If result is nil, a new
matrix is allocated"
  (funcall #'matrix::reduce-matrices-with-reductor op #'reduce-two-matrices matrices
           :result result
           :allocator-like #'new-matrix-like))

(defun sum (matrices &key (result nil))
  "sum matrices. If result is nil, a new matrix is allocated"
  (funcall #'reduce-matrices #'+ matrices :result result))

(defun incf-product (result matrix-a matrix-b)
  "sums to result the product of matrix-a and matrix-b"
  (let ((dim (matrix-dimension matrix-a)))
    (dotimes (i dim)
      (loop for j from i below dim
            do (loop for k from i to j
                     do (incf (aref result i j)
                              (* (aref matrix-a i k)
                                 (aref matrix-b k j))))))))

(defmethod add ((matrix-a matrix) (matrix-b matrix))
  (reduce-two-matrices #'+ (copy-matrix matrix-a) matrix-b))

(defmethod add ((matrix-a matrix:matrix) (matrix-b matrix))
  (let ((result (matrix:copy-matrix matrix-a)))
    (reduce-with-accessors #'+ result matrix-b (matrix-dimension matrix-b) matrix:aref aref)))

(defmethod add ((matrix-a matrix) (matrix-b matrix:matrix))
  (add matrix-b matrix-a))

(defmethod multiply ((matrix-a matrix) (matrix-b matrix))
  (let ((result (new-matrix :type      (matrix-type matrix-a)
                            :dimension (matrix-dimension matrix-a))))
    (incf-product result matrix-a matrix-b)
    result))

(defmethod multiply ((matrix-a matrix:matrix) (matrix-b matrix))
  (let ((num-rows (matrix:matrix-number-rows matrix-a))
        (num-cols (matrix-dimension matrix-b)))
    (let ((result (matrix:new-matrix :type        (matrix:matrix-type matrix-a)
                                     :number-rows num-rows
                                     :number-cols num-cols)))
      (dotimes (i num-rows)
        (dotimes (j num-cols)
          (loop for k from 0 to j
                do (incf (matrix:aref result i j)
                         (* (matrix:aref matrix-a i k)
                            (aref        matrix-b k j))))))
      result)))

(defmethod multiply ((matrix-a matrix) (matrix-b matrix:matrix))
  (let ((num-rows (matrix-dimension matrix-a))
        (num-cols (matrix:matrix-number-cols matrix-b)))
    (let ((result (matrix:new-matrix :type (matrix-type matrix-a)
                                     :number-rows num-rows
                                     :number-cols num-cols)))
      (dotimes (i num-rows)
        (dotimes (j num-cols)
          (loop for k from i below (matrix-dimension matrix-a)
                do (incf (matrix:aref result i j)
                         (* (aref        matrix-a i k)
                            (matrix:aref matrix-b k j))))))
      result)))
