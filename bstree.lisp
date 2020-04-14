(modules:using "aliases")

(defpackage :bstree
  (:use :common-lisp :aliases)
  (:export
     :make-leaf
     :compare
     :lookup :max-key :min-key
     :insert :update))

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

(defmacro recurse-into (func child)
  "makes func recurse into node child.  If child is nil, returns node key val pair"
  `(when node
     (with-node node
         (key val left right)
       (if ,child
           (,func ,child)
           (cons key val)))))

(defun max-key (node)
  "returns key val pair whose key is maximum"
  (recurse-into max-key right))

(defun min-key (node)
  "returns key val pair whose key is minimum"
  (recurse-into min-key left))

(defun lookup (node key)
  "searches for key in bstree rooted at node, returning corresponding
val (or nil, in case key is not present)"
  (when node
    (with-node node
        (node-key val left right)
      (case (compare key node-key)
        (:less    (lookup left key))
        (:equal   val)
        (:greater (lookup right key))))))

(defun insert (node key val)
  "inserts key val pair in bstree rooted at node in case key is not
present yet"
  (if node
      (with-node node
          (node-key node-val left right)
        (case (compare key node-key)
          (:less    (make-bstree node-key node-val (insert left key val) right))
          (:equal   node)
          (:greater (make-bstree node-key node-val left (insert right key val)))))
      (make-leaf key val)))

(defun update (node key new-val)
  "updates key to be attached to new-val, in case key is present in
node"
  (when node
    (with-node node
        (node-key node-val left right)
      (case (compare key node-key)
        (:less    (make-bstree node-key node-val (update left key new-val) right))
        (:equal   (make-bstree node-key new-val left right))
        (:greater (make-bstree node-key node-val left (update right key new-val)))))))

(modules:module "bstree")
