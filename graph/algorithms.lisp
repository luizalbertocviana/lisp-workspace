(defpackage :graph-algorithms
  (:use :common-lisp)
  (:export
     :depth-first-search :breadth-first-search))

(in-package :graph-algorithms)

(defun transpose-adjacency-closure (digraph)
  (lambda (u v)
    (has-edge digraph v u)))

(defun neighbors (adjacency-fn vertex)
  "returns the list of vertices adjacent to vertex according to
adjacency-fn"
  (do ((i 0 (1+ i))
       (neighbors nil (if (funcall adjacency-fn vertex i)
                          (cons i neighbors)
                          neighbors)))
      ((= i (num-verts digraph)) neighbors)))

(defun neighbor-closure (adjacency-fn)
  "returns a closure that, given a vertex, returns a list
  of its neighbors according to adjacency-fn"
  (lambda (vertex)
    (neighbors adjacency-fn vertex)))

(defun depth-first-search (neighbor-fn initial-vertex &key (early-visitor-fn nil) (late-visitor-fn nil))
  "based on the neighbor structure described by neighbor-fn, a
function that, given a vertex, returns a list of its neighbors,
performs a depth-first-search starting at initial-vertex, calling
visitor-fn at the end of each vertex visit"
  (let ((color-ht (make-hash-table)))
    (macrolet ((color (vertex)
                 `(gethash ,vertex color-ht :white)))
      (labels ((visit (vertex)
                 (and early-visitor-fn
                      (funcall early-visitor-fn vertex))
                 (setf (color vertex) :gray)
                 (visit-neighbors vertex)
                 (finish-visit vertex))
               (visit-neighbors (vertex)
                 (let ((neighbors (funcall neighbor-fn vertex)))
                   (loop for neighbor in neighbors
                         do (when (eq (color neighbor) :white)
                              (visit neighbor)))))
               (finish-visit (vertex)
                 (and late-visitor-fn
                      (funcall late-visitor-fn vertex))
                 (setf (color vertex) :black)))
        (visit initial-vertex)))))

(defun breadth-first-search (neighbor-fn initial-vertex visitor-fn)
  "based on the neighbor structure described by neighbor-fn, a
function that, given a vertex, returns a list of its neighbors,
performs a breadth-first-search starting at initial-vertex, calling
visitor-fn at the end of each vertex visit"
  (let ((vertex-queue (queue:new))
        (color-ht (make-hash-table)))
    (macrolet ((color (vertex)
                 `(gethash ,vertex color-ht :white)))
      (labels ((touch (vertex)
                 (queue:enqueue vertex-queue vertex)
                 (setf (color vertex) :gray)))
        (touch initial-vertex)
        (do ()
            ((queue:empty? vertex-queue) nil)
          (let* ((current-vertex (queue:dequeue vertex-queue))
                 (neighbors (funcall neighbor-fn current-vertex)))
            (loop for neighbor in neighbors
                  do (when (eq (color neighbor) :white)
                       (touch neighbor)))
            (funcall visitor-fn current-vertex)
            (setf (color current-vertex) :black)))))))
