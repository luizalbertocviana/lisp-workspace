(defpackage :anaphoric-macros-tests
  (:use :common-lisp :anaphoric-macros :fiveam))

(in-package :anaphoric-macros-tests)

(test pipeline-test
  "test the pipeline macro"
  (is (= 36
         (pipeline 2
                   (* 3 it)
                   (+ it 6)
                   (* 3 it))))
  (is (= 14
         (pipeline '(1 2 3)
                   (mapcar (lambda (n) (* n n)) it)
                   (reduce #'+ it)))))
