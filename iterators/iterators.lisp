;; an iterator is a closure which returns elemtents until an eq
;; comparable special symbol is returned, indicating that no more
;; elements will be returned but that special symbol. We convention
;; that :done is the default special symbol

;; here we provide some functions that create useful iterators

(defpackage :iterators
  (:use :common-lisp)
  (:shadow :merge)
  (:export
    :repeat :iterate :cycle
    :to-list :from-list
    :take :drop :take-while :drop-while
    :filter :partition
    :split :merge
    :enumerate :chain
    :consume))

(in-package :iterators)

(defun repeat (element)
  "creates an iterator which always returns element"
  (lambda () element))

(defun iterate (function element)
  "returns element and the results of successively applying function to it"
  (let ((previous nil)
        (current element))
    (lambda ()
      (setf previous current)
      (setf current (funcall function current))
      previous)))

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

(defun drop-while (predicate iterator &key (ending-symbol :done))
  "discards the first elements of iterator that do not satisfy
predicate, then returns it"
  (let ((first-element
          (do ((element (funcall iterator) (funcall iterator)))
              ((or (eq element ending-symbol)
                   (not (funcall predicate element)))
               element)))
        (first-call t))
    (if (eq first-element ending-symbol)
        (repeat ending-symbol)
        (lambda ()
          (if first-call
              (progn
                (setf first-call nil)
                first-element)
              (funcall iterator))))))

(defun filter (predicate iterator &key (ending-symbol :done))
  "creates an iterator that consumes iterator, returning the elements
satisfying predicate"
  (lambda ()
    (if iterator
        (do ((element (funcall iterator) (funcall iterator)))
            ((or (eq element ending-symbol)
                 (funcall predicate element))
             (progn
               (when (eq element ending-symbol)
                 (setf iterator nil))
               element)))
        ending-symbol)))

(defun partition (predicate iterator &key (ending-symbol :done))
  "creates two iterators: the first one returns the elements of
iterator that satisfy predicate, whereas the second one returns the
elements of iterator that do not satisfy predicate"
  (let ((queue-yes (queue:new))
        (queue-no (queue:new)))
    (macrolet ((do-loop (target-queue stop-condition)
                 `(do ((element (funcall iterator) (funcall iterator)))
                     ((or (eq element ending-symbol)
                          ,stop-condition)
                      (progn (when (eq element ending-symbol)
                               (setf iterator nil))
                             element))
                   (queue:enqueue ,target-queue element))))
      (values (lambda ()
                (if (queue:empty? queue-yes)
                    (if iterator
                        (do-loop queue-no (funcall predicate element))
                        ending-symbol)
                    (queue:dequeue queue-yes)))
              (lambda ()
                (if (queue:empty? queue-no)
                    (if iterator
                        (do-loop queue-yes (not (funcall predicate element)))
                        ending-symbol)
                    (queue:dequeue queue-no)))))))

(defun split (iterator &key (ending-symbol :done))
  "creates two iterators, which return the elements of iterator in an
interleaved way"
  (let ((queue-even (queue:new))
        (queue-odd (queue:new)))
    (values (lambda ()
              (if (queue:empty? queue-even)
                  (if iterator
                      (let ((even (funcall iterator)))
                        (if (eq even ending-symbol)
                            (setf iterator nil)
                            (let ((odd (funcall iterator)))
                              (if (eq odd ending-symbol)
                                  (setf iterator nil)
                                  (queue:enqueue queue-odd odd))))
                        even)
                      ending-symbol)
                  (queue:dequeue queue-even)))
            (lambda ()
              (if (queue:empty? queue-odd)
                  (if iterator
                      (let ((even (funcall iterator)))
                        (if (eq even ending-symbol)
                            (progn (setf iterator nil)
                                   ending-symbol)
                            (let ((odd (funcall iterator)))
                              (queue:enqueue queue-even even)
                              (when (eq odd ending-symbol)
                                (setf iterator nil))
                              odd)))
                      ending-symbol)
                  (queue:dequeue queue-odd))))))

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
