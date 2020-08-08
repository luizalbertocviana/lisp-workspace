(modules:using "macros")

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

(defun reduce-two-matrices (op matrix-a matrix-b)
  "reduces matrix-a and matrix-b applying op position-wise. Result is
  stored in matrix-a"
  (dotimes (i (matrix-dimension matrix-a))
    (setf (aref matrix-a i i)
          (funcall op
                   (aref matrix-a i i)
                   (aref matrix-b i i))))
  matrix-a)

(defun reduce-matrices (op matrix &rest matrices)
  "reduces matrix and matrices applying op position-wise. Result is
  stored in matrix"
  (when matrices
    (reduce (fn2 (funcall #'reduce-two-matrices op _1 _2)) matrices
            :initial-value matrix))
  matrix)

(defun sum (matrix &rest matrices)
  "sums matrix and matrices, storing result in matrix"
  (apply #'reduce-matrices #'+ matrix matrices))

(defun incf-product (result matrix-a matrix-b)
  "sums to result the product of matrix-a and matrix-b"
  (dotimes (i (matrix-dimension matrix-a))
    (incf (aref result i i)
          (* (aref matrix-a i i)
             (aref matrix-b i i)))))

(modules:module "triangular-matrix/diagonal")
