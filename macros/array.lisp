(defpackage :array
  (:use :common-lisp :macros))

(in-package :array)

(defmacro traversal ((arr n &key (unroll 1)) &body body)
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
