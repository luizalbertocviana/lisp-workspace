(defpackage :graph
  (:use :common-lisp)
  (:export
    :graph :empty :has-edge :add-edge :remove-edge))

(in-package :graph)

(defstruct graph
  "represents an undirected graph"
  (digraph nil :type digraph:digraph))

(defun empty (num-verts)
  "creates a new empty graph with num-verts vertices"
  (make-graph :digraph (digraph:empty num-verts)))

(defmacro adapt-from-digraph (function)
  "generates code to use function with graph"
  `(progn
     (unless (<= u v) (rotatef u v))
     (,function (graph-digraph graph) u v)))

(defun has-edge (graph u v)
  "determines whether graph has edge {u, v}"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type fixnum u v)
           (type graph graph))
  (adapt-from-digraph digraph:has-edge))

(defun add-edge (graph u v)
  "adds edge {u, v} to graph"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type fixnum u v)
           (type graph graph))
  (adapt-from-digraph digraph:add-edge))

(defun remove-edge (graph u v)
  "removes edge {u, v} from graph"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type fixnum u v)
           (type graph graph))
  (adapt-from-digraph digraph:remove-edge))
