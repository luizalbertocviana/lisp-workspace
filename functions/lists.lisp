(defpackage :lists
  (:use :common-lisp)
  (:export
    :pairs :maptree :map-sexp
    :make-circular))

(in-package :lists)

(defun pairs (a b &rest others)
  "arranges its arguments into a list of consed pairs"
  (let ((pair (cons a b)))
    (if (null others)
        (list pair)
        (cons pair (apply #'pairs others)))))

(defun maptree (f tree)
  "returns a copy of tree with each of its non-nil elements
transformed by f"
  (cond ((null tree) nil)
        ((atom tree) (funcall f tree))
        ((consp tree)
         (let ((first (car tree)) (second (cdr tree)))
           (cons (maptree f first) (maptree f second))))))

(defun map-sexp (f sexp)
  "returns a sexp whose sub-expressions (including sexp) are modified by f"
  (let ((modified (funcall f sexp)))
    (when (consp modified)
      (setf (car modified)
            (map-sexp f (car modified)))
      (setf (cdr modified)
            (map-sexp f (cdr modified))))
    modified))

(defun make-circular (list)
  "destructively turns list into a circular list. If list is already
circular, this function never returns"
  (let ((last-cons (last list)))
    (setf (cdr last-cons) list)
    list))
