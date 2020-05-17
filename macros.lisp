(modules:using "lists")

(defpackage :macros
  (:use :common-lisp :lists)
  (:export
     :with-gensyms :with-interned-symbols
     :pipeline
     :alias :aliases
     :fn0 :fn1 :fn2 :partial
     :for-each
     :compose-predicates
     :not-p :and-p :or-p
     :aif))

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

(defmacro pipeline (expr &rest exprs)
  "anaphoric macro to create a sequence of expressions where it refers
to the result of the previous expression"
  (with-interned-symbols (it)
    (if (null exprs)
        expr
        `(let ((,it ,expr))
           (pipeline ,@exprs)))))

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

(defmacro fn0 (&body body)
  "macro to create a lambda with no arguments"
  `(lambda () ,@body))

(defmacro fn1 (&body body)
  "anaphoric macro to create a lambda with one _ argument"
  (with-interned-symbols (_)
    `(lambda (,_) ,@body)))

(defmacro fn2 (&body body)
  "anaphoric macro to create a lambda with arguments _1 and _2"
  (with-interned-symbols (_1 _2)
    `(lambda (,_1 ,_2) ,@body)))

(defmacro partial (f &rest args)
  "returns a function representing a partial application of f over
args"
  (with-gensyms (more-args)
    `(lambda (&rest ,more-args) (apply ,f ,@args ,more-args))))

(defmacro for-each (sym (&rest exprs) &body body)
  "for each expr in exprs, runs body one time, replacing every
occurrence of var with expr"
  (let* ((bodies (loop for expr in exprs
                       collect (maptree (fn1 (if (eq _ sym)
                                                 expr
                                                 _))
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

(defmacro aif (condition then &optional else)
  "anaphoric macro that binds condition value to variable it, which
can be referenced in both then and else expressions"
  (with-interned-symbols (it)
    `(let ((,it ,condition))
       (if ,it ,then ,else))))

(modules:module "macros")
(modules:used-by "aliases" "lazy")
