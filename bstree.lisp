(modules:using "aliases")

(defpackage :bstree
  (:use :common-lisp :aliases)
  (:shadow :remove)
  (:export
     :bstree :bstree-p :make-leaf :empty
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

(defconstant empty nil
  "representation of empty bstree")

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
  "makes func recurse into node child.  If child is nil, returns node key val pair as values"
  `(when (bstree-p node)
     (with-node node
         (key val left right)
       (if ,child
           (,func ,child)
           (values key val)))))

(defun max-key (node)
  "returns values of key val pair whose key is maximum"
  (recurse-into max-key right))

(defun min-key (node)
  "returns values of key val pair whose key is minimum"
  (recurse-into min-key left))

(defun lookup (node key)
  "searches for key in bstree rooted at node, returning corresponding
key and val as values (or nil, in case key is not present)"
  (when (bstree-p node)
    (with-node node
        (node-key val left right)
      (case (compare key node-key)
        (:less    (lookup left key))
        (:equal   (values key val))
        (:greater (lookup right key))))))

(defun insert (node key val)
  "inserts key val pair in bstree rooted at node in case key is not
present yet"
  (if node
      (if (bstree-p node)
          (with-node node
              (node-key node-val left right)
            (case (compare key node-key)
              (:less    (new-node-from node :left (insert left key val)))
              (:equal   node)
              (:greater (new-node-from node :right (insert right key val)))))
          node)
      (make-leaf key val)))

(defun update (node key new-val)
  "updates key to be attached to new-val, in case key is present in
node"
  (if (bstree-p node)
      (with-node node
          (node-key node-val left right)
        (case (compare key node-key)
          (:less    (new-node-from node :left  (update left key new-val)))
          (:equal   (new-node-from node :val   new-val))
          (:greater (new-node-from node :right (update right key new-val)))))
      node))

(defun remove (node key)
  "removes key val pair from bstree rooted at node, returning values
of modified node and key of parent who had a child removed"
  (if (bstree-p node)
      (with-node node
          (node-key val left right)
        (case (compare key node-key)
          ;; this function may make a recursive call. If it does, such
          ;; a call returns exactly one value iff it deletes its
          ;; node. This way, if subtree-key is nil, we know that node
          ;; had a child deleted; otherwise, removal may have taken
          ;; place deep down in the tree. Thus, (or subtree-key
          ;; node-key) evaluates to the key of a parent who had a
          ;; child removed
          (:less    (multiple-value-bind (modified-left subtree-key) (remove left key)
                      (values (new-node-from node :left modified-left)
                              (or subtree-key node-key))))
          (:greater (multiple-value-bind (modified-right subtree-key) (remove right key)
                      (values (new-node-from node :right modified-right)
                              (or subtree-key node-key))))
          (:equal   (if (and left right)
                        (multiple-value-bind (max-left-key max-left-val) (max-key left)
                          (multiple-value-bind (modified-left subtree-key) (remove left max-left-key)
                            (values (new-node-from node :key  max-left-key
                                                        :val  max-left-val
                                                        :left modified-left)
                                    (or subtree-key node-key))))
                        (or left right)))))
      node))

(defmacro insertf (place key val)
  "updates place with (insert place key val)"
  `(setf ,place (insert ,place ,key ,val)))

(defmacro updatef (place key new-val)
  "updates place with (update place key new-val)"
  `(setf ,place (update ,place ,key ,new-val)))

(defmacro removef (place key)
  "updates place with (remove place key)"
  `(setf ,place (remove ,place ,key)))

(modules:module "bstree")
(modules:used-by "avltree")
