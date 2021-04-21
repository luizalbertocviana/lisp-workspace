(defpackage :disjoint-set
  (:use :common-lisp)
  (:export
     :disjoint-set :new :representant :join))

(in-package :disjoint-set)

(defstruct disjoint-set
  (n 0 :type fixnum)
  (parent #(0) :type (simple-array fixnum))
  (rankss #(0) :type (simple-array fixnum)))

(defun new (n)
  "creates a new disjoint-set instance holding n elements, each of
which represented by itself"
  (let ((ds (make-disjoint-set :n n
                               :parent (make-array n :element-type 'fixnum)
                               :rankss (make-array n
                                                 :element-type 'fixnum
                                                 :initial-element 0))))
    (loop for i from 0 to (1- n)
          do (setf (aref (disjoint-set-parent ds) i) i))
    ds))

(defun representant (ds elt)
  "returns the representant of elt in disjoint-set ds"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
           (type fixnum elt)
           (type disjoint-set ds))
  (symbol-macrolet ((parent (aref (disjoint-set-parent ds) elt)))
    (if (= elt parent)
        elt
        (let ((repr (representant ds parent)))
          (setf parent repr)
          repr))))

(defun join (ds elt-a elt-b)
  "alters disjoint-set ds such that elt-a and elt-b (and all their
relatives) have the same representant"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
           (type fixnum elt-a elt-b)
           (type disjoint-set ds))
  (let ((repr-a (representant ds elt-a))
        (repr-b (representant ds elt-b)))
    (declare (type fixnum repr-a repr-b))
    (unless (= repr-a repr-b)
      (symbol-macrolet ((parent-a (aref (disjoint-set-parent ds) repr-a))
                        (parent-b (aref (disjoint-set-parent ds) repr-b))
                        (rankss-a (aref (disjoint-set-rankss ds) repr-a))
                        (rankss-b (aref (disjoint-set-rankss ds) repr-b)))
        (cond ((< rankss-a rankss-b) (setf parent-a parent-b))
              ((> rankss-a rankss-b) (setf parent-b parent-a))
              ((= rankss-a rankss-b) (progn
                                       (setf parent-a parent-b)
                                       (setf rankss-b (1+ rankss-a)))))))
    ds))
