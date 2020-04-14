(modules:using "aliases")

(defpackage :bstree
  (:use :common-lisp :aliases)
  (:shadow :remove)
  (:export
     :make-leaf
     :compare
     :lookup :max-key :min-key
     :insert :update :remove
     :insertf :updatef :removef))

(in-package :bstree)

(defgeneric compare (a b)
  (:documentation "compares a and b, returning one of :less, :equal or :greater accordingly"))

(defmethod compare ((a fixnum) (b fixnum))
  (cond ((< a b) :less)
        ((= a b) :equal)
        ((> a b) :greater)))

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct (bstree)
    "represents a binary search tree node"
    key val left right))

(defun make-leaf (key val)
  "returns a bstree node containing key and val, with empty left and right subtrees"
  (make-bstree :key   key
               :val   val
               :left  nil
               :right nil))

(defmacro with-node (node (key val left right) &body body)
  "provides accessors for fields of a bstree node"
  `(with-expressions
       ((,key   (bstree-key   ,node))
        (,val   (bstree-val   ,node))
        (,left  (bstree-left  ,node))
        (,right (bstree-right ,node)))
     ,@body))

(defmacro new-node-from (node &key (key nil) (val nil) (left nil) (right nil))
  "creates a new bstree node based on node. Replaces node fields with
the provided ones"
  `(make-bstree :key   ,(if key
                            key
                            `(bstree-key ,node))
                :val   ,(if val
                            val
                            `(bstree-val ,node))
                :left  ,(if left
                            left
                            `(bstree-left ,node))
                :right ,(if right
                            right
                            `(bstree-right ,node))))

(defmacro recurse-into (func child)
  "makes func recurse into node child.  If child is nil, returns node key val pair"
  `(when (bstree-p node)
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
  (when (bstree-p node)
    (with-node node
        (node-key val left right)
      (case (compare key node-key)
        (:less    (lookup left key))
        (:equal   val)
        (:greater (lookup right key))))))

(defun insert (node key val)
  "inserts key val pair in bstree rooted at node in case key is not
present yet"
  (if (bstree-p node)
      (with-node node
          (node-key node-val left right)
        (case (compare key node-key)
          (:less    (new-node-from node :left (insert left key val)))
          (:equal   node)
          (:greater (new-node-from node :right (insert right key val)))))
      (make-leaf key val)))

(defun update (node key new-val)
  "updates key to be attached to new-val, in case key is present in
node"
  (when (bstree-p node)
    (with-node node
        (node-key node-val left right)
      (case (compare key node-key)
        (:less    (new-node-from node :left  (update left key new-val)))
        (:equal   (new-node-from node :val   new-val))
        (:greater (new-node-from node :right (update right key new-val)))))))

(modules:module "bstree")
