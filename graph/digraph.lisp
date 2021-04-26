(defpackage :digraph
  (:use :common-lisp)
  (:export
    :digraph :new :has-edge :add-edge :remove-edge))

(in-package :digraph)

(defstruct digraph
  (num-verts 0 :type fixnum)
  (num-edges 0 :type fixnum)
  (adj #(0) :type (simple-array bit)))

(defun new (num-verts)
  "creates a new empty digraph with num-verts vertices"
  (make-digraph :num-verts num-verts
                :num-edges 0
                :adj (make-array (* num-verts num-verts)
                                 :element-type 'bit)))

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
