(defpackage :triangular-matrix/diagonal
  (:nicknames :mat-diagonal)
  (:use :common-lisp :macros)
  (:shadow :aref :identity)
  (:export
     :matrix :new-matrix))

(in-package :triangular-matrix/diagonal)

(defstruct (matrix)
  "represents a diagonal matrix"
  (data nil :type (simple-array))
  (type)
  (dimension nil :type (integer 0)))

(defun new-matrix (&key (type 'number) dimension (initial-element 0))
  "creates a diagonal matrix with elements typed to type (defaults to
  number) and dimension. All diagonal positions are initialized as
  initial-element (defaults to 0)"
  (make-matrix :data (make-array dimension
                                 :element-type type
                                 :initial-element (coerce initial-element type))
               :type type
               :dimension dimension))

(defun new-matrix-like (matrix)
  "creates a diagonal matrix with same dimension and type of matrix"
  (new-matrix :type (matrix-type matrix)
              :dimension (matrix-dimension matrix)))

(defun aref (matrix row col)
  "element of matrix at position (row col)"
  (if (= row col)
      (cl:aref (matrix-data matrix) row)
      (coerce 0 (matrix-type matrix))))

(defun (setf aref) (value matrix row col)
  "sets (row col) position of matrix to value"
  (if (= row col)
      (setf (cl:aref (matrix-data matrix) row) value)
      0))

(defun identity (&key (type 'number) dimension)
  "creates an identity matrix"
  (new-matrix :type            type
              :dimension       dimension
              :initial-element 1))

(defun reduce-two-matrices (op matrix-a matrix-b &key (result nil))
  "reduces matrix-a and matrix-b applying op position-wise. If result
is nil, a new matrix is allocated"
  (unless result
    (setf result (new-matrix-like matrix-a)))
  (dotimes (i (matrix-dimension matrix-a))
    (setf (aref result i i)
          (funcall op
                   (aref matrix-a i i)
                   (aref matrix-b i i))))
  matrix-a)

(defun reduce-matrices (op matrices &key (result nil))
  "reduces matrices applying op position-wise. If result is nil, a new
matrix is allocated"
  (matrix::reduce-matrices-with-reductor op #'reduce-two-matrices matrices
                                         :result result
                                         :allocator-like #'new-matrix-like))

(defun sum (matrices &key (result nil))
  "sums matrix and matrices, storing result in matrix"
  (reduce-matrices #'+ matrices :result result))

(defun product (matrix-a matrix-b &key (result nil))
  "sums to result the product of matrix-a and matrix-b. If result is
nil, a new matrix is allocated"
  (dotimes (i (matrix-dimension matrix-a))
    (incf (aref result i i)
          (* (aref matrix-a i i)
             (aref matrix-b i i)))))
