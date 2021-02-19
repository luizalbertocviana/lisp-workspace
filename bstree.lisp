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
          (do ((previous nil current)
               (current node (case (compare key (bstree-key current))
                               (:less (bstree-left current))
                               (:equal nil)
                               (:greater (bstree-right current)))))
              ((null current) (let ((cmp (compare key (bstree-key previous))))
                                (unless (eq cmp :equal)
                                  (let ((new-leaf (make-leaf key val)))
                                    (case cmp
                                      (:less (setf (bstree-left previous) new-leaf))
                                      (:greater (setf (bstree-right previous) new-leaf)))))
                                node)))
          node)
      (make-leaf key val)))

(defun update (node key new-val)
  "updates key to be attached to new-val, in case key is present in
node"
  (if (bstree-p node)
      (with-node current
          (current-key current-val left right)
        (do ((current node (case (compare key current-key)
                             (:less left)
                             (:equal (progn
                                       (setf current-val new-val)
                                       nil))
                             (:greater right))))
            ((null current) node)))
      node))

(defun remove (node key)
  "removes key val pair from bstree rooted at node, returning values
of modified node and key of parent who had a child removed"
  (if (bstree-p node)
      (with-node current
          (c-key c-val c-left c-right)
        (with-node previous
            (p-key p-val p-left p-right)
          (do ((parent nil previous)
               (previous nil current)
               (current node (case (compare key c-key)
                               (:less c-left)
                               (:equal nil)
                               (:greater c-right))))
              ((null current)
               (if (eq (compare key p-key) :equal)
                   (if (and p-left p-right)
                       (multiple-value-bind (max-key max-val) (max-key p-left)
                         (setf p-key max-key)
                         (setf p-val max-val)
                         (setf p-left (remove p-left max-key))
                         node)
                       (let ((nonempty-child (or p-left p-right)))
                         (if parent
                             (progn
                               (case (compare p-key (bstree-key parent))
                                 (:less (setf (bstree-left parent) nonempty-child))
                                 (:greater (setf (bstree-right parent) nonempty-child)))
                               node)
                             nonempty-child)))
                   node)))))
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
