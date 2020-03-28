(modules:using "aliases" "macros")

(defpackage :lazy
  (:use :common-lisp :aliases :macros)
  (:shadow :let :cons :car :cdr)
  (:export
     :thunk :force :let :cons :car :cdr))

(in-package :lazy)

(defmacro thunk (&body body)
  "creates a plist containing the code to be evaluated and the result of its evaluation"
  ``(:result nil :code ,(fn0 ,@body)))

(defun force (thunk)
  "force the evaluation of thunk. If already evaluated, just return the result"
  (with-expressions ((code   (getf thunk :code))
                     (result (getf thunk :result)))
    (when code
      (setf result (funcall code))
      (setf code nil))
    result))

(defmacro let ((&rest bindings) &body body)
  "creates bindings using implicit thunks"
  (cl:let ((variables (mapcar #'first bindings))
        (values    (mapcar #'second bindings)))
    (cl:let ((gvariables (loop for var in variables
                            collect (gensym))))
      `(cl:let (,@(loop for gvar in gvariables and val in values
                     collect `(,gvar (thunk ,val))))
         (with-expressions (,@(loop for var in variables and gvar in gvariables
                                    collect `(,var (force ,gvar))))
           ,@body)))))

(defmacro lazily (func &rest args)
  "calls func with each arg inside of a thunk"
  `(,func ,@(loop for arg in args
                  collect `(thunk ,arg))))

(defmacro cons (a b)
  "creates a cons with both components inside thunks"
  `(lazily cl:cons ,a ,b))

(defun repeat (a)
  "creates an infinite list with each element being a"
  (cons a (repeat a)))

(defmacro defforce (force-fn fn)
  "creates a thunk-aware version of fn"
  (let ((doc-string (documentation fn 'function)))
    `(defun ,force-fn (&rest args)
       ,doc-string
       (force (apply #',fn args)))))

(defforce car cl:car)
(defforce cdr cl:cdr)

(modules:module "lazy")
