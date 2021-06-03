(defpackage :graph-algorithms
  (:use :common-lisp)
  (:export
     :depth-first-search))

(in-package :graph-algorithms)

(defun depth-first-search (neighbor-fn initial-vertex visitor-fn)
  "based on the neighbor structure described by neighbor-fn, a
function that, given a vertex, returns a list of its neighbors,
performs a depth-first-search starting at initial-vertex, calling
visitor-fn at the end of each vertex visit"
  (let ((color-ht (make-hash-table)))
    (macrolet ((color (vertex)
                 `(gethash ,vertex color-ht :white)))
      (labels ((visit (vertex)
                 (setf (color vertex) :gray)
                 (visit-neighbors vertex)
                 (finish-visit vertex))
               (visit-neighbors (vertex)
                 (let ((neighbors (funcall neighbor-fn vertex)))
                   (loop for neighbor in neighbors
                         do (when (eq (color neighbor) :white)
                              (visit neighbor)))))
               (finish-visit (vertex)
                 (funcall visitor-fn vertex)
                 (setf (color vertex) :black)))
        (visit initial-vertex)))))
