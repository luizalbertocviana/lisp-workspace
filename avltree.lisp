(modules:using "bstree")

(defpackage :avltree
  (:use :common-lisp)
  (:import-from :bstree :lookup :empty)
  (:export
     :avltree :avltree-p :make-leaf :empty
     :lookup))

(in-package :avltree)

(defstruct (avltree (:include bstree:bstree))
  "represents a avl tree node"
  (height 0 :type (unsigned-byte 64)))

(defun height (node)
  "returns height of node"
  (if (avltree-p node)
      (avltree-height node)
      -1))

(defun transform (node)
  "recursively transforms a bstree node into an avltree node"
  (if (avltree-p node)
      node
      (when (bstree:bstree-p node)
        (bstree::with-node node
            (key val node-left node-right)
          (let* ((left   (transform node-left))
                 (right  (transform node-right))
                 (height (1+ (max (height left) (height right)))))
            (make-avltree :key    key
                          :val    val
                          :left   left
                          :right  right
                          :height height))))))

(defun make-leaf (key val)
  "returns a avltree node containing key and val, with empty left and right subtrees"
  (transform (bstree:make-leaf key val)))

(modules:module "avltree")
