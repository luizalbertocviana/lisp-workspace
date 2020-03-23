(modules:using "aliases")

(defpackage :bstree
  (:use :common-lisp :aliases)
  (:export
     :compare :lookup :insert))

(in-package :bstree)

(defgeneric compare (a b)
  (:documentation "compares a and b, returning one of :less, :equal or :greater accordingly"))

(defmethod compare ((a fixnum) (b fixnum))
  (cond ((< a b) :less)
        ((= a b) :equal)
        ((> a b) :greater)))

(defun make-bstree (key val left right)
  "creates a bstree node"
  `(:key ,key :val ,val :left ,left :right ,right))

(defun make-leaf (key val)
  "returns a bstree node containing key and val, with empty left and right subtrees"
  (make-bstree key val nil nil))

(defmacro with-node (node (key val left right) &body body)
  "provides accessors for fields of a bstree node"
  `(with-expressions
       ((,key   (getf ,node :key))
        (,val   (getf ,node :val))
        (,left  (getf ,node :left))
        (,right (getf ,node :right)))
     ,@body))

(defun lookup (node key)
  "searches for key in bstree rooted at node, returning corresponding val (or nil, in case key is not present)"
  (when node
    (with-node node
        (node-key node-val node-left node-right)
      (case (compare key node-key)
        (:less    (lookup node-left key))
        (:equal   node-val)
        (:greater (lookup node-right key))))))

(defun insert (node key val)
  "inserts key val pair in bstree rooted at node in case key is not present yet"
  (if node
      (with-node node
          (node-key node-val left right)
        (case (compare key node-key)
          (:less    (make-bstree node-key node-val (insert left key val) right))
          (:equal   node)
          (:greater (make-bstree node-key node-val left (insert right key val)))))
      (make-leaf key val)))

(modules:module "bstree")
