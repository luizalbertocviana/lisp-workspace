(defmacro future (&body body)
  `(sb-thread:make-thread (lambda ()
                            (sb-thread:return-from-thread
                             (progn ,@body)))))

(defmacro join (future)
  `(sb-thread:join-thread ,future))
