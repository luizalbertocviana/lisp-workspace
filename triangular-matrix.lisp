(modules:using "matrix")

(defpackage :triangular-matrix
  (:use :common-lisp)
  (:shadow :aref))

(in-package :triangular-matrix)

(defstruct (upper-triangular-matrix)
  "represents an upper triangular matrix"
  (data      nil :type matrix:matrix)
  (type)
  (dimension nil :type (integer 0)))

(defun new-upper-triangular-matrix (&key (type 'number) dimension (initial-element 0))
  "creates an upper triangular matrix with elements typed to
  type (defaults to number) and dimension. All positions (except the
  lower triangle) are initialized as initial-element (defaults to 0)"
  (make-upper-triangular-matrix :data (matrix:new-matrix :type type
                                                         :number-rows (ceiling dimension 2)
                                                         :number-cols (1+ dimension)
                                                         :initial-element initial-element)
                                :type type
                                :dimension dimension))

(defun aref (matrix row col)
  (if (> row col)
      0
      (let* ((mat      (upper-triangular-matrix-data matrix))
             (dim      (upper-triangular-matrix-dimension matrix))
             (mat-rows (matrix:matrix-number-rows mat)))
        (if (< row mat-rows)
            (matrix:aref mat row (+ 1 col))
            (matrix:aref mat (- dim row 1) (- dim col 1))))))

(defun (setf aref) (value matrix row col)
  (if (> row col)
      0
      (let* ((mat      (upper-triangular-matrix-data matrix))
             (dim      (upper-triangular-matrix-dimension matrix))
             (mat-rows (matrix:matrix-number-rows mat)))
        (if (< row mat-rows)
            (setf (matrix:aref mat row (+ 1 col)) value)
            (setf (matrix:aref mat (- dim row 1) (- dim col 1)) value)))))

(modules:module "triangular-matrix")
