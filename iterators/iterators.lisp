;; an iterator is a closure which returns elemtents until an eq
;; comparable special symbol is returned, indicating that no more
;; elements will be returned but that special symbol. We convention
;; that :done is the default special symbol

;; here we provide some functions that create useful iterators

(defpackage :iterators
  (:use :common-lisp :lists)
  (:export
    :repeat :cycle
    :to-list :from-list
    :take :drop
    :enumerate
    :consume))

(in-package :iterators)

(defun repeat (element)
  "creates an iterator which always returns element"
  (lambda () element))

(defun cycle (iterator &key (ending-symbol :done))
  "creates an iterator that returns the elements returned by iterator
in a loopy manner"
  (let ((reversed-elements nil)
        (elements nil))
    (lambda ()
      (let ((current (and iterator
                          (funcall iterator))))
        (if (or (not current)
                (eq current ending-symbol))
            (progn
              (unless elements
                (setf iterator nil)
                (setf elements (nreverse reversed-elements))
                (setf reversed-elements nil)
                (make-circular elements))
              (let ((element (car elements)))
                (setf elements (cdr elements))
                element))
            (progn
              (push current reversed-elements)
              current))))))

(defun to-list (iterator &key (ending-symbol :done))
  "returns a list containing the elements returned by iterator"
  (loop for element = (funcall iterator)
        while (not (eq element ending-symbol))
        collect element))

(defun from-list (list &key (ending-symbol :done))
  "creates an iterator that returns each element of list"
  (lambda ()
    (if list
        (let ((element (first list)))
          (pop list)
          element)
        ending-symbol)))

(defun take (n iterator &key (ending-symbol :done))
  "creates an iterator that returns the first n elements of iterator"
  (lambda ()
    (if (> n 0)
        (progn
          (decf n)
          (funcall iterator))
        ending-symbol)))

(defun drop (n iterator)
  "discards the first n elements of iterator, then returns it"
  (loop repeat n
        do (funcall iterator))
  iterator)

(defun enumerate (iterator &key (starting-index 0) (ending-symbol :done))
  "creates an iterator that returns, at each call, two values: an
enumerating index and the corresponding element of iterator"
  (let ((current-index starting-index))
    (lambda ()
      (if iterator
          (let ((element (funcall iterator)))
            (if (eq element ending-symbol)
                (progn
                  (setf iterator nil)
                  ending-symbol)
                (values (incf current-index) element)))
          ending-symbol))))

(defun consume (iterator function &key (ending-symbol :done))
  "consumes each element of iterator, calling function on each of
them"
  (do ((element (funcall iterator) (funcall iterator)))
      ((eq element ending-symbol) nil)
    (funcall function element)))
