(modules:using "macros")

(defpackage :compiling
  (:use :common-lisp :macros)
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

(defmacro type-let ((&rest type-bindings) &body body)
  (let* ((types      (mapcar #'first type-bindings))
         (vars       (mapcar #'second type-bindings))
         (vals       (mapcar #'third type-bindings))
         (typed-vars (mapcar #'list types vars))
         (typed-vals (mapcar (fn2 `(the ,_1 ,_2)) types vals))
         (bindings   (mapcar #'list vars typed-vals)))
    `(let ,bindings
       (with-types ,typed-vars
         ,@body))))

(defmacro type-defun (name (returns &rest type-vars) &optional (doc "") &body body)
  (let ((vars (mapcar #'second type-vars)))
    `(defun ,name ,vars ,doc
       (with-types ,type-vars
         (the ,returns (progn ,@body))))))

(modules:module "compiling")
