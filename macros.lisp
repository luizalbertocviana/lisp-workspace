(using "lists")

(defmacro with-gensyms ((&rest symbols) &body body)
  "assigns a gensym for each arg in a let environment"
  `(let (,@(loop for symbol in symbols
                 collect `(,symbol (gensym))))
     ,@body))

(defmacro pipeline (expr &rest exprs)
  "anaphoric macro to create a sequence of expressions where it refers to the result of the previous expression"
  (if (null exprs)
      expr
      `(let ((it ,expr))
         (pipeline ,@exprs))))

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
  `(lambda (_) ,@body))

(defmacro fn2 (&body body)
  "anaphoric macro to create a lambda with arguments _1 and _2"
  `(lambda (_1 _2) ,@body))

(defmacro compose-predicates (expr)
  "builds a function where every symbol in expr other than 'and and 'or is turned into a proper function call"
  (let ((logical-operators '(and or)))
    (with-gensyms (x)
      (labels ((transform (p)
                 (if (member p logical-operators)
                     p
                     `(,p ,x))))
        `(lambda (,x) ,(maptree #'transform expr))))))

(defmacro aif (condition then &optional else)
  "anaphoric macro that binds condition value to variable it, which
   can be referenced in both then and else expressions"
  `(let ((it ,condition))
     (if it ,then ,else)))
