(defpackage :anaphoric-macros
  (:use :common-lisp :macros)
  (:export
   :pipeline
   :aif :if-let :when-let
   :alambda))

(in-package :anaphoric-macros)

(defmacro pipeline (expr &rest exprs)
  "anaphoric macro to create a sequence of expressions where it refers
to the result of the previous expression"
  (with-interned-symbols (it)
    (if (null exprs)
        expr
        `(let ((,it ,expr))
           (pipeline ,@exprs)))))

(defmacro aif (condition then &optional else)
  "anaphoric macro that binds condition value to variable it, which
can be referenced in both then and else expressions"
  (with-interned-symbols (it)
    `(let ((,it ,condition))
       (if ,it ,then ,else))))

(defmacro if-let (binding then &optional else)
  "introduces binding (var test), then executes then in case test is
not nil, otherwise executes else"
  (destructuring-bind (var test) binding
    `(let ((,var ,test))
       (if ,var
           ,then
           ,else))))

(defmacro when-let (binding &body body)
  "introduces binding (var test), then executes body in case test is
not nil"
  `(if-let ,binding (progn ,@body)))

(defmacro alambda (args &body body)
  "anaphoric macro that returns a lambda which can refer to itself as
self"
  (with-interned-symbols (self)
    `(labels ((,self ,args ,@body))
       #',self)))
