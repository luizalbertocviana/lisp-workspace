(defmodule ("lazy"
            :using ("aliases" "macros"))

  (defmacro thunk (&body body)
    "creates a plist containing the code to be evaluated and the
     result of its evaluation"
    ``(:result nil :code ,(fn0 ,@body)))

  (defun force (thunk)
    "force the evaluation of thunk. If already evaluated, just return
     the result"
    (with-expressions ((code   (getf thunk :code))
                       (result (getf thunk :result)))
      (when code
        (setf result (funcall code))
        (setf code nil))
      result))

  (defmacro lazy-let ((&rest bindings) &body body)
    "creates bindings using implicit thunks"
    (let ((variables (mapcar #'first bindings))
          (values    (mapcar #'second bindings)))
      (let ((gvariables (loop for var in variables
                              collect (gensym))))
        `(let (,@(loop for gvar in gvariables and val in values
                       collect `(,gvar (thunk ,@val))))
           (with-expressions (,@(loop for var in variables and gvar in gvariables
                                      collect `(,var (force ,gvar))))
             ,@body))))))
