(defpackage :lists
  (:use :common-lisp)
  (:export
    :pairs :maptree :map-sexp
    :make-circular
    :take :drop))

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

(defun map-sexp (f sexp &key (copy nil))
  "returns a sexp whose sub-expressions (including sexp) are modified by f"
  (let ((modified (funcall f sexp)))
    (if (consp modified)
        (let ((modified-car (map-sexp f (car modified) :copy copy))
              (modified-cdr (map-sexp f (cdr modified) :copy copy)))
          (if copy
              (cons modified-car modified-cdr)
              (progn
                (setf (car modified) modified-car)
                (setf (cdr modified) modified-cdr)
                modified)))
        modified)))

(defun make-circular (list)
  "destructively turns list into a circular list. If list is already
circular, this function never returns"
  (let ((last-cons (last list)))
    (setf (cdr last-cons) list)
    list))

(defun take (n list)
  "takes the first n elements of list"
  (if (= n 0)
      nil
      (cons (car list)
            (take (1- n) (cdr list)))))

(defun drop (n list)
  "drops the first n elements of list"
  (if (= n 0)
      list
      (drop (1- n) (cdr list))))
