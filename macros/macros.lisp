(defpackage :macros
  (:use :common-lisp :lists)
  (:export
     :with-gensyms :with-interned-symbols
     :alias :aliases
     :partial
     :for-each
     :compose-predicates
     :not-p :and-p :or-p
     :let-values*))

(in-package :macros)

(defmacro with-gensyms ((&rest symbols) &body body)
  "assigns a gensym for each arg in a let environment"
  `(let (,@(loop for symbol in symbols
                 collect `(,symbol (gensym))))
     ,@body))

(defmacro with-interned-symbols ((&rest symbols) &body body)
  "ensures each one of symbols is interned in current package. Useful
for anaphoric macros"
  `(let (,@(loop for symbol in symbols
                 collect `(,symbol (intern (symbol-name ',symbol)))))
     ,@body))

(defmacro alias (new-name old-name)
  "creates an alias to an already defined function or macro"
  (let ((alias-doc
          (format nil "~a is an alias for ~a. " new-name old-name))
        (doc-string (documentation old-name 'function)))
    `(defmacro ,new-name (&rest args)
       ,(concatenate 'string alias-doc doc-string)
       `(,',old-name ,@args))))

(defmacro aliases (&rest args)
  "creates an alias for each pair of arguments"
  `(progn ,@(loop for pair in (apply #'pairs args)
                  collect `(alias ,(car pair) ,(cdr pair)))))

(defmacro partial (f &rest args)
  "returns a function representing a partial application of f over
args"
  (with-gensyms (more-args)
    `(lambda (&rest ,more-args) (apply ,f ,@args ,more-args))))

(defmacro for-each (sym (&rest exprs) &body body)
  "for each expr in exprs, runs body one time, replacing every
occurrence of var with expr"
  (let* ((bodies (loop for expr in exprs
                       collect (maptree (lambda (e)
                                          (if (eq e sym)
                                              expr
                                              e))
                                        body)))
         (conc-bodies (apply #'nconc bodies)))
    `(progn ,@conc-bodies)))

(defmacro not-p (p)
  "creates a lambda that negates p"
  (with-interned-symbols (_)
    `(fn1 (not (funcall ,p ,_)))))

(defmacro and-p (&rest preds)
  "creates a lambda representing the conjunction of each predicate in
preds"
  (with-interned-symbols (_)
    `(fn1 (and ,@(loop for pred in preds
                       collect `(funcall ,pred ,_))))))

(defmacro or-p (&rest preds)
  "creates a lambda representing the disjunction of each predicate in
preds"
  (with-interned-symbols (_)
    `(fn1 (or ,@(loop for pred in preds
                       collect `(funcall ,pred ,_))))))

(defmacro compose-predicates (expr)
  "builds a function where every symbol in expr other than 'and, 'or
and 'not is turned into a proper function call"
  (let ((logical-operators '(and or not)))
    (with-gensyms (x)
      (labels ((transform (p)
                 (if (member p logical-operators)
                     p
                     `(,p ,x))))
        `(lambda (,x) ,(maptree #'transform expr))))))

(defmacro let-values* (bindings &body body)
  "binding is an expression (var ... values-form). Performs body in a
context where, for each binding in bindings, var ... is bound to the
values returned from values-form"
  (labels ((modify-body (binding)
             (let ((vars (butlast binding))
                   (values-form (car (last binding))))
               (setf body
                     `(multiple-value-bind ,vars ,values-form
                        ,body)))))
    (setf body `(progn ,@body))
    (loop for binding in (reverse bindings)
          do (modify-body binding))
    body))
