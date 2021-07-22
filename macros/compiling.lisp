(defpackage :compiling
  (:use :common-lisp :macros)
  (:export
     :compiling-for :disassemble-for
     :with-types :type-let :type-defun))

(in-package :compiling)

(defmacro compiling-for (target &body body)
  "compiles body according to target, which can be one of :speed of
:safety"
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
  "disassemble body as the body of a lambda with no arguments compiled
according to target"
  `(disassemble (lambda ()
                  (compiling-for ,target ,@body))))

(defmacro with-types ((&rest type-vars) &body body)
  "given a list of pairs associating types and variables, creates a
context for body in which variables are declared to be of the
associated type"
  `(locally
       (declare ,@(loop for type-var in type-vars
                        collect `(type ,@type-var)))
     ,@body))

(defmacro type-let ((&rest type-bindings) &body body)
  "given a list of triples associating types, variables and values,
creates a context for body in which both variable and value are
declared to be of the associated type"
  (let* ((types      (mapcar #'first type-bindings))
         (vars       (mapcar #'second type-bindings))
         (vals       (mapcar #'third type-bindings))
         (typed-vars (mapcar #'list types vars))
         (typed-vals (mapcar (lambda (tp v) `(the ,tp ,v)) types vals))
         (bindings   (mapcar #'list vars typed-vals)))
    `(let ,bindings
       (with-types ,typed-vars
         ,@body))))

(defmacro type-defun (name (returns &rest type-vars) &optional (doc "") &body body)
  "creates a function named name with signature and body. signature is
a list whose first element is the return type of name and the
remaining ones are pairs of types and input variables. &rest,
&optional, &key are not supported"
  (let ((vars (mapcar #'second type-vars)))
    `(defun ,name ,vars ,doc
       (with-types ,type-vars
         (the ,returns (progn ,@body))))))
