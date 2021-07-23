(defpackage threading-macros
  (:use :common-lisp :lists :macros :anaphoric-macros)
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

(defmacro cond-> (expr &rest forms)
  "clojure style cond-> macro"
  (if (null forms)
      expr
      (destructuring-bind ((cdt outer-expr) &rest other-forms) forms
        (when (atom outer-expr)
          (setf outer-expr `(,outer-expr)))
        `(cond-> (if ,cdt
                     ,(put-before-nth-cdr outer-expr
                                          1
                                          expr)
                     ,expr)
                 ,@other-forms))))

(defmacro cond->> (expr &rest forms)
  "clojure style cond->> macro"
  (if (null forms)
      expr
      (destructuring-bind ((cdt outer-expr) &rest other-forms) forms
        (when (atom outer-expr)
          (setf outer-expr `(,outer-expr)))
        `(cond->> (if ,cdt
                      ,(put-before-nth-cdr outer-expr
                                           (length outer-expr)
                                           expr)
                      ,expr)
                  ,@other-forms))))

(defmacro some-> (expr &rest forms)
  "clojure style some-> macro"
  (if (null forms)
      expr
      (with-gensyms (test)
        `(some-> (when-let (,test ,expr)
                   ,(put-before-nth-cdr (car forms)
                                        1
                                        test))
                 ,@(cdr forms)))))

(defmacro some->> (expr &rest forms)
  "clojure style some->> macro"
  (if (null forms)
      expr
      (destructuring-bind (form &rest other-forms) forms
        (with-gensyms (test)
          `(some->> (when-let (,test ,expr)
                      ,(put-before-nth-cdr form
                                           (length form)
                                           test))
                    ,@other-forms)))))
