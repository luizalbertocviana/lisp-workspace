(defpackage :anaphoric-macros
  (:use :common-lisp :macros)
  (:export
   :pipeline
   :aif :alambda))

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

(defmacro alambda (args &body body)
  "anaphoric macro that returns a lambda which can refer to itself as
self"
  (with-interned-symbols (self)
    `(labels ((,self ,args ,@body))
       #',self)))
