(modules:using "matrix" "triangular-matrix/upper")

(defpackage :triangular-matrix/lower
  (:nicknames :tri-lower)
  (:use :common-lisp)
  (:import-from :matrix :add :multiply)
  (:shadow :aref :identity)
  (:export
     :matrix :new-matrix :matrix-type :matrix-dimension
     :aref
     :identity
     :reduce-two-matrices :reduce-matrices
     :incf-product))

(in-package :triangular-matrix/lower)

(defstruct (matrix)
  "represents a lower triangular matrix"
  (data nil :type triangular-matrix/upper:matrix)
  (type)
  (dimension nil :type (integer 0)))

(defun new-matrix (&key (type 'number) dimension (initial-element 0))
  "creates an lower triangular matrix with elements typed to
  type (defaults to number) and dimension. All positions (except the
  upper triangle) are initialized as initial-element (defaults to 0)"
  (make-matrix :data (tri-upper:new-matrix :type            type
                                           :dimension       dimension
                                           :initial-element initial-element)
               :type type
               :dimension dimension))

(defun aref (matrix row col)
  "element of matrix at position (row col)"
  (tri-upper:aref (matrix-data matrix) col row))

(defun (setf aref) (value matrix row col)
  "sets (row col) position of matrix to value"
  (setf (tri-upper:aref (matrix-data matrix) col row) value))

(defun identity (&key (type 'number) dimension)
  "creates an identity matrix"
  (make-matrix :data (tri-upper:identity :type      type
                                         :dimension dimension)
               :type type
               :dimension dimension))

(defun reduce-two-matrices (op matrix-a matrix-b)
  "reduces matrix-a and matrix-b applying op position-wise. Result is
stored in matrix-a"
  (tri-upper:reduce-two-matrices op
                                 (matrix-data matrix-a)
                                 (matrix-data matrix-b))
  matrix-a)

(defun reduce-matrices (op matrix &rest matrices)
  "reduces matrix and matrices applying op position-wise. Result is
  stored in matrix"
  (tri-upper:reduce-matrices op
                             (matrix-data matrix)
                             (mapcar #'matrix-data matrices)))

(defun sum (matrix &rest matrices)
  "sum matrix and matrices, storing result in matrix"
  (apply #'reduce-matrices #'+ matrix matrices))

(defun incf-product (result matrix-a matrix-b)
  "sums to result the product of matrix-a and matrix-b"
  (tri-upper:incf-product (matrix-data result)
                          (matrix-data matrix-a)
                          (matrix-data matrix-b)))

(defmethod add ((matrix-a matrix) (matrix-b matrix))
  (reduce-two-matrices #'+ (copy-matrix matrix-a) matrix-b))

(defmethod add ((matrix-a tri-upper:matrix) (matrix-b matrix))
  (let* ((dim    (tri-upper:matrix-dimension matrix-a))
         (result (matrix:square-matrix :type      (tri-upper:matrix-type matrix-a)
                                       :dimension dim)))
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (matrix:aref result i j)
              (cond ((= i j) (+ (tri-upper:aref matrix-a i j)
                                (aref           matrix-b i j)))
                    ((< i j) (tri-upper:aref matrix-a i j))
                    ((> i j) (aref matrix-b i j))))))
    result))

(defmethod add ((matrix-a matrix) (matrix-b tri-upper:matrix))
  (add matrix-b matrix-a))

(defmethod add ((matrix-a matrix:matrix) (matrix-b matrix))
  (let ((dim    (matrix:matrix-number-rows matrix-a))
        (result (matrix:copy-matrix matrix-a)))
    (dotimes (i dim)
      (loop for j from 0 to i
            do (incf (matrix:aref result i j)
                     (aref matrix-b i j))))
    result))

(defmethod add ((matrix-a matrix) (matrix-b matrix:matrix))
  (add matrix-b matrix-a))

(modules:module "triangular-matrix/lower")
