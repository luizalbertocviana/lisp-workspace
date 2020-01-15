(defvar *user-modules* nil
  "list of currently loaded user modules. This variable identifies
   modules by absolute path")

(defparameter *base-path* #.(or *compile-file-truename* *load-truename*)
  "absolute path of modules.lisp file. This is used as the root of the
   project. This way, you should put modules.lisp at the root of your
   project so you can properly use these macros")

(defmacro using-when (situation &rest file-names)
  "loads a fasl for each of the corresponding file-name files, if not
   already loaded or outdated, during situation time. This also
   updates fasl files as needed. Situation must be :compiling or
   :loading"
  ;; chooses values according to situation
  `(eval-when (,situation)
     (progn ,@(loop for fn in file-names
                    collect (let ((fn-fasl              (format nil "~a.fasl" fn))
                                  (fn-lisp              (format nil "~a.lisp" fn))
                                  (abs-pathname-fasl    (gensym))
                                  (abs-pathname-lisp    (gensym))
                                  (fasl-membership-test (gensym))
                                  (fasl-updated-test    (gensym)))
                              ;; creates absolute paths for both fasl
                              ;; and lisp files, based on current
                              ;; file being compiled/loaded
                              `(let* ((,abs-pathname-fasl    (merge-pathnames ,fn-fasl *base-path*))
                                      (,abs-pathname-lisp    (merge-pathnames ,fn-lisp *base-path*))
                                      (,fasl-membership-test (member ,abs-pathname-fasl *user-modules* :test #'equal))
                                      (,fasl-updated-test    (and (probe-file ,abs-pathname-fasl)
                                                                  (>= (file-write-date ,abs-pathname-fasl)
                                                                      (file-write-date ,abs-pathname-lisp)))))
                                 ;; if module is not loaded or is
                                 ;; outdated
                                 (unless (and ,fasl-membership-test ,fasl-updated-test)
                                   ;; update module in case it is
                                   ;; outdated
                                   (unless ,fasl-updated-test
                                     (compile-file ,abs-pathname-lisp))
                                   ;; load module
                                   (load ,abs-pathname-fasl :verbose t)
                                   ;; register module is loaded
                                   (unless ,fasl-membership-test
                                     (push ,abs-pathname-fasl *user-modules*)))))))))

(defmacro using (&rest file-names)
  "loads a fasl for each of the corresponding file-names files, if not
   already loaded, during both loading and compilation time. This also
   updates fasl files as needed"
  `(progn
     ;; prepares to load modules during both compilation and loading
     ;; times
     (using-when :compile-toplevel ,@file-names)
     (using-when :load-toplevel    ,@file-names)))
