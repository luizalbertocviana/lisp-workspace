(defpackage :array
  (:use :common-lisp :macros))

(in-package :array)

(defun build-static-traversal-code (arr n body &key (unroll 1))
  "returns the code necessary to run body on each of the n first
positions of arr. Notice that n has to be a constant integer. Loop
unrolling is made for each of unroll iterations"
  (multiple-value-bind (quot rem) (truncate n unroll)
    (let ((remaining-base (- n rem)))
      (with-gensyms (i base)
        `(let ((,base 0))
           (dotimes (,i ,quot)
             ,@(loop for offset from 0 to (1- unroll)
                     collect `(symbol-macrolet ((idx (+ ,base ,offset))
                                                (elt (aref ,arr
                                                           (+ ,offset ,base))))
                                ,@body))
             (incf ,base ,unroll))
           ,@(loop for r from 0 to (1- rem)
                   collect `(symbol-macrolet ((idx ,(+ remaining-base r))
                                              (elt (aref ,arr
                                                         ,(+ remaining-base r))))
                              ,@body)))))))

(defun build-dynamic-traversal-code (arr n body &key (unroll 1))
  "returns the code necessary to run body on each of the first n
positions of arr. Loop unrolling is made for each of unroll
iterations"
  (let ((exec-lambda-body `(funcall (lambda () ,@body))))
   (with-gensyms (once-n quot rem base remaining-base i r)
    `(let ((,once-n ,n))
       (multiple-value-bind (,quot ,rem) (truncate ,once-n ,unroll)
         (let ((,base 0)
               (,remaining-base (- ,once-n ,rem)))
           (dotimes (,i ,quot)
             ,@(loop for offset from 0 to (1- unroll)
                     collect `(symbol-macrolet ((idx (+ ,base ,offset))
                                                (elt (aref ,arr
                                                           (+ ,base ,offset))))
                                ,exec-lambda-body))
             (incf ,base ,unroll))
           (dotimes (,r ,rem)
             (symbol-macrolet ((idx (+ ,remaining-base ,r))
                               (elt (aref ,arr
                                          (+ ,remaining-base ,r))))
               ,exec-lambda-body))))))))

(defmacro traversal ((arr &key (length `(length ,arr)) (unroll 2 unroll-p)) &body body)
  "performs a traversal on the first length elements of arr, loop
unrolling each of unroll iterations. Body is executed in each
iteration, and its expressions may refer to current elemnt and index
in terms of symbols elt and idx, respectively"
  (if (integerp length)
      (build-static-traversal-code arr length body
                                   :unroll (if unroll-p
                                               unroll
                                               (if (<= length 11) ; generates nice asm code
                                                   (+ length 1)
                                                   2)))
      (build-dynamic-traversal-code arr length body :unroll unroll)))
