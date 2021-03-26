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
    :take :drop :take-while :drop-while
    :enumerate :chain
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
      (block :cycle-closure
        (when iterator
          (let ((current (funcall iterator)))
            (if (eq current ending-symbol)
                (progn
                  (setf iterator nil)
                  (setf elements (nreverse reversed-elements))
                  (setf reversed-elements nil)
                  (make-circular elements))
                (progn
                  (push current reversed-elements)
                  (return-from :cycle-closure current)))))
        (let ((element (car elements)))
          (setf elements (cdr elements))
          element)))))

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

(defun take-while (predicate iterator &key (ending-symbol :done))
  "creates an iterator that returns the first elements of iterator
that satisfy predicate"
  (lambda ()
    (if iterator
        (let ((element (funcall iterator)))
          (if (funcall predicate element)
              element
              (progn
                (setf iterator nil)
                ending-symbol)))
        ending-symbol)))

(defun drop-while (predicate iterator)
  "discards the first elements of iterator that do not satisfy
predicate, then returns it"
  (let ((first-element (loop for element = (funcall iterator)
                             while (funcall predicate element)
                             finally (return element)))
        (first-call t))
    (lambda ()
      (if first-call
          (progn
            (setf first-call nil)
            first-element)
          (funcall iterator)))))

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

(defun chain (iterator-1 iterator-2 &key (ending-symbol :done))
  "creates an iterator that returns the elememts of iterator-1, and
when this has ended, returns the elements of iterator-2"
  (let ((current-iterator iterator-1))
    (lambda ()
      (if current-iterator
          (progn
            (let ((element (funcall current-iterator)))
              (when (eq element ending-symbol)
                (setf current-iterator
                      (if (eq current-iterator iterator-1)
                          iterator-2
                          nil))
                (setf element
                      (if current-iterator
                          (funcall current-iterator)
                          ending-symbol)))
              element))
          ending-symbol))))

(defun consume (iterator function &key (ending-symbol :done))
  "consumes each element of iterator, calling function on each of
them"
  (do ((element (funcall iterator) (funcall iterator)))
      ((eq element ending-symbol) nil)
    (funcall function element)))
