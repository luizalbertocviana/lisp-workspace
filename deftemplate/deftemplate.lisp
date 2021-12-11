(defpackage :deftemplate
  (:use :common-lisp :lists)
  (:export :deftemplate))

(in-package :deftemplate)

(defvar *templates* (make-hash-table :test #'eq))

(defstruct template
  parameters
  body)

(defmacro deftemplate (name param-list &body body)
  "registers body as a template identified by name whose parameters
are those in param-list"
  (let ((template (make-template :parameters param-list
                                 :body body)))
    (if (gethash name *templates*)
        (error "attempt to redefine template ~a" name)
        (setf (gethash name *templates*) template))
    name))

(defun param-value (param param-bindings)
  "retrieves the value associated with param in param-bindings. In
case no binding refers to param, returns nil. In case more than one
binding refers to param, returns the value in the first binding"
  (let ((binding (find param param-bindings :key #'first)))
    (when binding
      (second binding))))

(defun instantiate-body (body params param-bindings)
  "transforms body according to params and param-bindings. Each
element of params has its occurrences in body replaced with its
corresponding value from param-bindings. In case a parameter has no
corresponding value in param-bindings, all of its occurrences are
removed from body instead"
  (dolist (param params)
    (let ((value (param-value param param-bindings)))
      (setf body (if value
                     (replace-atom body param value)
                     (remove-atom body param)))))
  body)

(defmacro instantiate-template (name inst-name param-bindings)
  "retrieves the template registered under name (a keyword) and apply
parameter subistitution according to param-bindings. In case a
template param has no value in param-bindings, all of its occurrences
are removed instead. Also, if name occurs in its template body, it is
replaced by inst-name."
  (let* ((template (gethash name *templates*))
         (extended-params (cons name (template-parameters template)))
         (extended-bindings (cons `(,name ,inst-name) param-bindings)))
    `(progn ,@(instantiate-body (template-body template)
                                extended-params
                                extended-bindings))))
