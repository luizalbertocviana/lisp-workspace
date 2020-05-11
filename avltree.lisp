(modules:using "aliases" "bstree")

(defpackage :avltree
  (:use :common-lisp :aliases)
  (:import-from :bstree
     :lookup :empty :compare :max-key :min-key)
  (:export
     :avltree :avltree-p :make-leaf :empty
     :compare
     :lookup :max-key :min-key
     :insert :update
     :insertf :updatef))

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
  "returns a avltree node containing key and val, with empty left and
right subtrees"
  (transform (bstree:make-leaf key val)))

(defun path-to-key (node key)
  "returns a list containing references that form a path (in reversed
order) from node to the node containing key"
  (do ((current-node node (bstree::with-node current-node
                              (node-key val left right)
                            (case (compare key node-key)
                              (:less    left)
                              (:equal   nil)
                              (:greater right))))
       (path nil (cons current-node path)))
      ((null current-node) path)))

(defun insert (node key val)
  "inserts key val pair in avltree rooted at node in case key is not
present yet"
  (if node
      (if (and (avltree-p node)
               (not (lookup node key)))
          (let* ((modified-node (bstree:insert node key val))
                 (path          (path-to-key modified-node key))
                 ;; as node is not nil, path has at least two nodes
                 (parent-node   (second path)))
            (case (compare key (avltree-key parent-node))
              (:less    (setf (avltree-left  parent-node) (transform (avltree-left  parent-node))))
              (:greater (setf (avltree-right parent-node) (transform (avltree-right parent-node)))))
            modified-node)
          node)
      (make-leaf key val)))

(modules:module "avltree")
