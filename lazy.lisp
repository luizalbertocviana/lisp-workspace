(modules:using "aliases" "macros" "lists")

(defpackage :lazy
  (:use :common-lisp :aliases :macros :lists)
  (:shadow
     :let :cons :car :cdr :list
     :mapcar :keep-if
     :length)
  (:export
     :thunk :delay :force :forcef
     :let
     :cons :car :cdr :list
     :length :take :drop
     :take-while :drop-while :keep-if
     :force-cons :force-list
     :repeat :mapcar :iterate))

(in-package :lazy)

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct (thunk)
    "represents a possibly suspended computation"
    code result))

;; pretty-printing thunks
(defmethod print-object ((thk thunk) out)
  (print-unreadable-object (thk out :type t)
    (format out "~a " (if (null (thunk-code thk))
                          (thunk-result thk)
                          "_"))))

(defmacro delay (&body body)
  "creates a struct containing the code to be evaluated and a place to
put its evaluation"
  (cl:let ((fn `(fn0 ,@body)))
    `(make-thunk :code ,fn)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun force (thing)
    "forces the evaluation of thing. If already evaluated, just
returns the result"
    (if (thunk-p thing)
        (with-expressions ((code   (thunk-code   thing))
                           (result (thunk-result thing)))
          (when code
            (setf result (funcall code))
            (setf code nil))
          result)
        thing)))

(defmacro forcef (place)
  "if place contains a thunk, sets place with thunk result"
  `(setf ,place (force ,place)))

(defun car (c)
  "thunk-aware version of car"
  (forcef (cl:car c)))

(defun cdr (c)
  "thunk-aware version of cdr"
  (forcef (cl:cdr c)))

(defun force-cons (c)
  "calls car and cdr on c, thus forcing its evaluation"
  (car c) (cdr c) c)

(defun force-list (list)
  "forces evaluation of entire list"
  (when (consp list)
    (force-cons list)
    (force-list (cdr list))))

(defmacro lazily (func &rest args)
  "calls func with each arg inside of a thunk"
  `(,func ,@(loop for arg in args
                  collect `(delay ,arg))))

(defmacro cons (a b)
  "creates a cons with both components inside thunks"
  `(lazily cl:cons ,a ,b))

(defmacro list (&rest elements)
  "anaphoric macro to create a list. In the description of last
element, symbol it refers to the entire list"
  (cl:let ((it   (intern (symbol-name 'it)))
           (init (butlast elements))
           (tail (cl:car (last elements))))
    (with-gensyms (list-name)
      (cl:let ((new-tail (maptree (lambda (x) (if (eq x it)
                                                  list-name
                                                  x))
                                  tail)))
        `(cl:let ((,list-name (cl:list ,@init)))
           (setf (cl:cdr (last ,list-name)) ,new-tail)
           ,list-name)))))

(defun length (list)
  "returns number of elements in list"
  (labels ((f (lst acc)
             (if (consp lst)
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f list 0)))

(defun repeat (a)
  "creates an infinite list with each element being a"
  (cons a (repeat a)))

(defun iterate (f a)
  "returns successive applications of f over a"
  (cons a (iterate f (funcall f a))))

(defun take (n list)
  "returns prefix of list containing the first n elements"
  (when (and (plusp n)
             (consp list))
    (cons (car list) (take (1- n) (cdr list)))))

(defun drop (n list)
  "returns suffix of list containing all but the first n elements"
  (if (and (plusp n)
           (consp list))
      (drop (1- n) (cdr list))
      list))

(defun take-while (p list)
  "takes elements from list while p is true"
  (cl:let ((head (car list)))
    (if (funcall p head)
        (cons head (take-while p (cdr list)))
        nil)))

(defun drop-while (p list)
  "drops elements from list while p is true"
  (if (funcall p (car list))
      (drop-while p (cdr list))
      list))

(defun keep-if (p list)
  "keeps elements of list that satisfy p"
  (with-expressions ((head (car list))
                     (tail (keep-if p (cdr list))))
    (if (funcall p head)
        (cons head tail)
        tail)))

(defmacro let ((&rest bindings) &body body)
  "creates bindings using implicit thunks"
  (cl:let ((variables (cl:mapcar #'first bindings))
           (values    (cl:mapcar #'second bindings)))
    (cl:let ((gvariables (loop for var in variables
                               collect (gensym))))
      `(cl:let (,@(loop for gvar in gvariables and val in values
                        collect `(,gvar (delay ,val))))
         (with-expressions (,@(loop for var in variables and gvar in gvariables
                                    collect `(,var (force ,gvar))))
           ,@body)))))

(defun mapcar (fn list &rest lists)
  "applies fn to corresponding elements of list and lists, returning
results as a list"
  (let ((head  (car list))
        (heads (cl:mapcar #'car lists))
        (tail  (cdr list))
        (tails (cl:mapcar #'cdr lists)))
    (cons (apply fn head heads) (apply #'mapcar fn tail tails))))

(modules:module "lazy")
