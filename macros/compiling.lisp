(defpackage :compiling
  (:use :common-lisp :macros)
  (:export
     :defun-spec :compiling-for :disassemble-for
     :with-types :type-let :type-defun))

(in-package :compiling)

(defmacro defun-spec (name lambda-list (&rest specs) &body body)
  "defines a function named name with lambda-list as signature.
Each spec has the form (return-type ((type sig-var) ...)
:locals ((type local-var) ...)).  For each spec, the types of argument
values are checked at runtime, and execution is directed to a
corresponding optimized version of body. In case no spec is met, an
unoptimized versoin of body is used. For instance, 
(defun-spec add-one (x)
    ((fixnum ((fixnum x)) :locals ((fixnum a))))
  (let ((a 1))
    (+ x a)))
expands to
(defun add-one (x)
  (cond
    ((and (typep x 'fixnum))
     (locally
         (declare (optimize (speed 3) (safety 0) (debug 0))
                  (type fixnum x))
       (the fixnum
            (progn
              (let ((a 1))
                (declare (type fixnum a))
                (+ x a))))))
    (t
     (let ((a 1))
       (+ x a)))))
"
  (labels ((let? (form)
             (and (listp form)
                  (>= (length form) 2)
                  (destructuring-bind (name bindings &body body) form
                    (declare (ignore body))
                    (and (or (eq name 'let)
                             (eq name 'let*))
                         (every (lambda (binding)
                                  (and (listp binding)
                                       (= (length binding) 2))) bindings)))))
           (treat-let (let-form declaration)
             (destructuring-bind (let-type bindings &body body) let-form
               `(,let-type ,bindings ,declaration ,@body)))
           (treat-spec (spec body)
             (destructuring-bind (return-type typed-vars &key (locals nil)) spec
               (labels ((modify-let (let-form)
                          (let* ((bindings (second let-form))
                                 (let-variables (mapcar #'first bindings))
                                 (bound-locals (remove-if-not (lambda (type-var)
                                                                (member (second type-var)
                                                                        let-variables))
                                                              locals))
                                 (declaration `(declare ,@(loop for (type var) in bound-locals
                                                                collect `(type ,type ,var)))))
                            (treat-let let-form declaration))))
                 (let ((modified-body (lists:map-sexp (lambda (sexp)
                                                        (if (let? sexp)
                                                            (modify-let sexp)
                                                            sexp))
                                                      body)))
                   `((and ,@(loop for (type var) in typed-vars
                                  collect `(typep ,var ',type)))
                     (locally
                         (declare (optimize (speed             3)
                                            (safety            0)
                                            (debug             0)
                                            (space             0)
                                            (compilation-speed 0))
                                  ,@(loop for (type var) in typed-vars
                                          collect `(type ,type ,var)))
                       (the ,return-type (progn ,@modified-body)))))))))
    (if (stringp (first body))
        (let ((expanded-body (mapcar #'sb-cltl2:macroexpand-all (rest body))))
          `(defun ,name ,lambda-list
             ,(first body)
             (cond ,@(loop for spec in specs
                           collect (treat-spec spec expanded-body))
                   (t ,@(rest body)))))
        (let ((expanded-body (mapcar #'sb-cltl2:macroexpand-all body)))
          `(defun ,name ,lambda-list
             (cond ,@(loop for spec in specs
                           collect (treat-spec spec expanded-body))
                   (t ,@body)))))))

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
         (typed-vals (mapcar (fn2 `(the ,_1 ,_2)) types vals))
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
