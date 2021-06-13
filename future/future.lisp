(defpackage :future
  (:use :common-lisp)
  (:export
     :future :wait))

(defmacro future (&body body)
  `(sb-thread:make-thread (lambda ()
                            (sb-thread:return-from-thread (progn ,@body)))))

(defmacro wait (future)
  `(sb-thread:join-thread ,future))
