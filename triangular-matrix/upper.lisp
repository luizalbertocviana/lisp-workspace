(modules:using "matrix" "macros")

(defpackage :triangular-matrix/upper
  (:use :common-lisp :macros)
  (:import-from :matrix :add :multiply)
  (:shadow :aref :identity))

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

(defun aref (matrix row col)
  "element of matrix at position (row col)"
  (if (> row col)
      0
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

(defmacro reduce-with-accessors (op matrix-a matrix-b dimension aref-a aref-b)
  "reduces matrix-a and matrix-b applying op position-wise. Result is
stored in matrix-a. Access to matrices is performed using aref-a and aref-b"
  (with-gensyms (dim i j)
    `(let ((,dim ,dimension))
       (dotimes (,i ,dim)
         (loop for ,j from ,i below ,dim
               do (setf (,aref-a ,matrix-a ,i ,j)
                        (funcall ,op
                                 (,aref-a ,matrix-a ,i ,j)
                                 (,aref-b ,matrix-b ,i ,j)))))
       ,matrix-a)))

(defun reduce-two-matrices (op matrix-a matrix-b)
  "reduces matrix-a and matrix-b applying op position-wise. Result is
stored in matrix-a"
  (reduce-with-accessors op matrix-a matrix-b (matrix-dimension matrix-a) aref aref))

(defun reduce-matrices (op matrix &rest matrices)
  "reduces matrix and matrices applying op position-wise. Result is
  stored in matrix"
  (apply #'matrix::reduce-matrices-with-reductor op #'reduce-two-matrices matrix matrices))

(defun sum (matrix &rest matrices)
  "sum matrix and matrices, storing result in matrix"
  (apply #'reduce-matrices #'+ matrix matrices))

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
                            (matrix:aref matrix-b k j)))))))))

(modules:module "triangular-matrix/upper")
