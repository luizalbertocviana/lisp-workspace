(defpackage :compiling
  (:use :common-lisp)
  (:export
     :compiling-for
     :disassemble-for
     :with-types))

(in-package :compiling)

(defmacro compiling-for (target &body body)
  (let ((speed-declaration
          `((compilation-speed 0)
            (debug             0)
            (safety            0)
            (space             0)
            (speed             3)))
        (safety-declaration
          `((compilation-speed 0)
            (debug             3)
            (safety            3)
            (space             0)
            (speed             0))))
    `(locally
         (declare (optimize ,@(case target
                                (:speed  speed-declaration)
                                (:safety safety-declaration))))
       ,@body)))

(defmacro disassemble-for (target &body body)
  `(disassemble (lambda ()
                  (compiling-for ,target ,@body))))

(defmacro with-types ((&rest type-vars) &body body)
  `(locally
       (declare ,@(loop for type-var in type-vars
                        collect `(type ,@type-var)))
     ,@body))

(modules:module "compiling")
