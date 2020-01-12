(defun pairs (a b &rest others)
  "arranges its arguments into a list of consed pairs"
  (let ((pair (cons a b)))
    (if (null others)
        (list pair)
        (cons pair (apply #'pairs others)))))

(defun maptree (f tree)
  "returns a copy of tree with each of its non-nil elements transformed by f"
  (cond ((null tree) nil)
        ((atom tree) (funcall f tree))
        ((consp tree)
         (let ((first (car tree)) (second (cdr tree)))
           (cons (maptree f first) (maptree f second))))))
