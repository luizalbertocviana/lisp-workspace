(defpackage :deftemplate
  (:use :common-lisp :lists)
  (:export :deftemplate))

(in-package :deftemplate)

(defvar *templates* (make-hash-table :test #'eq))

(defstruct template
  parameters
  body)

(defmacro deftemplate (name param-list &body body)
  (let ((template (make-template :parameters param-list
                                 :body body)))
    (if (gethash name *templates*)
        (error "attempt to redefine template ~a" name)
        (setf (gethash name *templates*) template))
    name))

(defun param-value (param param-bindings)
  (let ((binding (find param param-bindings :key #'first)))
    (when binding
      (second binding))))

(defun instantiate-body (body params param-bindings)
  (dolist (param params)
    (let ((value (param-value param param-bindings)))
      (setf body (if value
                     (replace-atom body param value)
                     (remove-atom body param)))))
  body)

(defmacro instantiate-template (name inst-name param-bindings)
  (let* ((template (gethash name *templates*))
         (extended-params (cons name (template-parameters template)))
         (extended-bindings (cons `(,name ,inst-name) param-bindings)))
    `(progn ,@(instantiate-body (template-body template)
                                extended-params
                                extended-bindings))))
