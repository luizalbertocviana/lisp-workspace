(defpackage :digraph
  (:use :common-lisp)
  (:export
    :digraph :empty :complete
    :num-verts :num-edges
    :has-edge :add-edge :remove-edge
    :edge-iterator))

(in-package :digraph)

(defstruct digraph
  "representes a directed graph"
  (num-verts 0 :type fixnum)
  (num-edges 0 :type fixnum)
  (adj #(0) :type (simple-array bit)))

(defun empty (num-verts)
  "creates a new empty digraph with num-verts vertices"
  (make-digraph :num-verts num-verts
                :num-edges 0
                :adj (make-array (* num-verts num-verts)
                                 :element-type 'bit)))

(defun num-verts (digraph)
  "returns the number of vertices of digraph"
  (digraph-num-verts digraph))

(defun num-edges (digraph)
  "returns the number of edges of digraph"
  (digraph-num-edges digraph))

(defmacro edge (digraph u v)
  "creates code to access the position in the digraph adj bit-vector
correspondiong to edge (u, v)"
  `(aref (digraph-adj ,digraph)
         (the fixnum (+ (the fixnum (* ,u (digraph-num-verts ,digraph))) ,v))))

(defun has-edge (digraph u v)
  "determines whether digraph has edge (u, v)"
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0))
           (type fixnum u v)
           (type digraph digraph))
  (= 1 (edge digraph u v)))

(defun set-edge (digraph u v value)
  "sets the position in digraph adj bit-vector corresponding to
edge (u, v) to value"
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0))
           (type fixnum u v)
           (type bit value)
           (type digraph digraph))
  (setf (edge digraph u v) value))

(defun add-edge (digraph u v)
  "adds edge (u, v) to digraph"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (type fixnum u v)
           (type digraph digraph))
  (unless (has-edge digraph u v)
    (incf (digraph-num-edges digraph))
    (set-edge digraph u v 1)))

(defun remove-edge (digraph u v)
  "removes edge (u, v) from digraph"
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0))
           (type fixnum u v)
           (type digraph digraph))
  (when (has-edge digraph u v)
    (decf (digraph-num-edges digraph))
    (set-edge digraph u v 0)))

(defun complete (num-verts &key (loops nil))
  "creates a new complete digraph with num-verts vertices. Unless
otherwise stated, the created digraph will not contain loops"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
           (type fixnum num-verts))
  (let ((digraph (make-digraph :num-verts num-verts
                               :num-edges (the fixnum (* num-verts num-verts))
                               :adj (make-array (the fixnum (* num-verts num-verts))
                                                :element-type 'bit
                                                :initial-element 1))))
    (unless loops
      (dotimes (i num-verts)
        (remove-edge digraph i i))
      (decf (digraph-num-edges digraph) num-verts))
    digraph))

(defun edge-iterator (digraph &key (ending-symbol :done))
  "returns an iterator over the edges of digraph"
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (let ((i 0) (j 0) (n (digraph-num-verts digraph)))
    (declare (type fixnum i j n))
    (lambda ()
      (labels ((next ()
                 (incf j)
                 (when (= j n)
                   (setf j 0)
                   (incf i)))
               (end ()
                 (and (= i n)
                      (= j 0)))
               (return-edge ()
                 (let ((ret-i i) (ret-j j))
                   (next)
                   (values ret-i ret-j))))
        (cond ((end) ending-symbol)
              ((has-edge digraph i j) (return-edge))
              (t (do ()
                     ((or (end) (has-edge digraph i j))
                      (if (end)
                          ending-symbol
                          (return-edge)))
                   (next))))))))
