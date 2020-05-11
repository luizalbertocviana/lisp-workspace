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

(defmacro with-node (node (key val left right height) &body body)
  "provides accessors for fields of a avltree node"
  `(bstree::with-node ,node
       (,key ,val ,left ,right)
     (with-expressions
         ((,height (avltree-height ,node)))
       ,@body)))

(defun height (node)
  "returns height of node"
  (if (avltree-p node)
      (avltree-height node)
      -1))

(defun calculate-height (node)
  "calculates height of node"
  (when (avltree-p node)
    (with-node node
        (key val left right node-height)
      (1+ (max (height left) (height right))))))

(defun update-height (node)
  "updates height of node"
  (when (avltree-p node)
    (with-node node
        (key val left right height)
      (setf height (calculate-height node)))))

(defun balance-factor (node)
  "calculates balance factor of node, returning one of:
:left-heavy,
:left-pending,
:balanced,
:right-epnding,
:right-heavy"
  (when (avltree-p node)
    (bstree::with-node node
        (key val left right)
      (case (- (height right) (height left))
        (-2 :left-heavy)
        (-1 :left-pending)
        (0  :balanced)
        (1  :right-pending)
        (2  :right-heavy)))))

(defun rotate-r (node)
  "returns a right rotated version of node"
  (when (avltree-p node)
    (with-node node
        (node-key node-val left right node-height)
      (with-node left
          (left-key left-val left-left left-right left-height)
        (rotatef left left-right node)
        (update-height left)
        (update-height right)
        (update-height node)
        node))))

(defun rebalance (node)
  "returns a balanced version of node"
  (when (avltree-p node)
    (with-node node
        (key val left right height)
      (case (balance-factor node)
        (:left-heavy (case (balance-factor left)
                       (:left-pending (rotate-r node))
                       ((:right-pending :balanced) node)))
        (:right-heavy (case (balance-factor right)
                        (:left-pending node)
                        ((:right-pending :balanced) node)))
        (otherwise node)))))

(defun rebalance-path-to-key (node key)
  "recursively rebalances nodes in the path from node to key"
  (when (avltree-p node)
    (with-node node
        (node-key val left right height)
      (case (compare key node-key)
        (:less
         (setf left (rebalance-path-to-key left key)))
        (:greater
         (setf right (rebalance-path-to-key right key))))
      (update-height node)
      (rebalance node))))

(defun transform (node)
  "recursively transforms a bstree node into an (nonbalanced) avltree
node"
  (if (avltree-p node)
      node
      (when (bstree:bstree-p node)
        (bstree::with-node node
          (key val node-left node-right)
          (let* ((left     (transform node-left))
                 (right    (transform node-right))
                 (avl-node (make-avltree :key   key
                                         :val   val
                                         :left  left
                                         :right right)))
            (update-height avl-node)
            avl-node)))))

(defun make-leaf (key val)
  "returns a avltree node containing key and val, with empty left and
right subtrees"
  (transform (bstree:make-leaf key val)))

(defun insert (node key val)
  "inserts key val pair in avltree rooted at node in case key is not
present yet"
  (if node
      (if (and (avltree-p node)
               (not (lookup node key)))
          (let* ((modified-node (bstree:insert node key val))
                 (avl-node      (transform modified-node)))
            (rebalance-path-to-key avl-node key))
          node)
      (make-leaf key val)))

(defun update (node key new-val)
  "updates key to be attached to new-val, in case key is present in
node"
  (transform (bstree:update node key new-val)))

(defmacro insertf (place key val)
  "updates place with (insert place key val)"
  `(setf ,place (insert ,place ,key ,val)))

(defmacro updatef (place key val)
  "updates place with (insert place key val)"
  `(setf ,place (update ,place ,key ,val)))

(modules:module "avltree")
