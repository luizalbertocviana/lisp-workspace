(defpackage threading-macros
  (:use :common-lisp
   :lists)
  (:export
   :-> :->>))

(in-package threading-macros)

(defmacro -> (expr &rest exprs)
  "clojure style -> macro"
  (if (null exprs)
      expr
      (let ((outer-expr (car exprs)))
        (when (atom outer-expr)
          (setf outer-expr `(,outer-expr)))
        `(-> ,(put-before-nth-cdr outer-expr
                                  1
                                  expr)
             ,@(cdr exprs)))))

(defmacro ->> (expr &rest exprs)
  "clojure style ->> macro"
  (if (null exprs)
      expr
      (let ((outer-expr (car exprs)))
        (when (atom outer-expr)
          (setf outer-expr `(,outer-expr)))
        `(->> ,(put-before-nth-cdr outer-expr
                                   (length outer-expr)
                                   expr)
              ,@(cdr exprs)))))

(defmacro as-> (expr name &rest exprs)
  "clojure style as-> macro"
  (if (null exprs)
      expr
      `(let ((,name ,expr))
         (as-> ,(car exprs) ,name ,@(cdr exprs)))))
