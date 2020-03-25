(defpackage :tests
  (:use :common-lisp)
  (:export
     :test :test-suite))

(in-package :tests)

(defmacro test (expr)
  "when expr evals to nil, prints fail message"
  `(if ,expr
       t
       (format t "test ~a failed" ',expr)))

(defmacro test-suite (&rest exprs)
  "tests exprs in sequence, stopping at the first fail"
  `(eval-when (:compile-toplevel)
     (and ,@(loop for expr in exprs
                  collect `(test ,expr)))))

(modules:module "tests")
