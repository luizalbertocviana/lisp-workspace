(defpackage :spec
  (:use :common-lisp)
  (:export :defun-tag))

(in-package :spec)

(defparameter +agressive-optimize-tuple+
  '(optimize (speed             3)
             (safety            0)
             (debug             0)
             (space             0)
             (compilation-speed 0))
  "tuple to be used in (declare ...) forms to indicate extreme
optimization settings")

(defun let? (form)
  "determines whether form is a let-form"
  (and (listp form)
       (>= (length form) 2)
       (destructuring-bind (name bindings &body body) form
         (declare (ignore body))
         (and (or (eq name 'let)
                  (eq name 'let*))
              (every (lambda (binding)
                       (and (listp binding)
                            (= (length binding) 2))) bindings)))))

(defun change-binding (binding new-val)
  "changes initial value of binding to new-val"
  `(,(car binding) ,new-val))

(defun find-binding (bindings var)
  "returns binding of variable var present in bindings. In case no
such binding exists, returns nil"
  (find-if (lambda (binding)
             (eq var (first binding)))
           bindings))

(cl:defun bindings-single-change (bindings var new-val)
  "changes bindings so var is initilized with new-val. In case
bindings does not contain a binding for var, itis returned
unmodified"
  (let ((target-binding (find-binding bindings var)))
    (when target-binding
      (setf (second target-binding) new-val))
    bindings))

(defun bindings-multiple-changes (bindings vars-vals)
  "changes bindings according to vars-vals"
  (loop for (var new-val) in vars-vals
        do (setf bindings
                 (bindings-single-change bindings var new-val)))
  bindings)

(defun update-let-bindings (let-form vars-vals)
  "creates a new let form based on let-form with bindings updated
according to vars-vals"
  (destructuring-bind (let-type bindings &body body) let-form
    `(,let-type ,(bindings-multiple-changes bindings vars-vals) ,@body)))

(defun put-decl-let (let-form declaration)
  "inserts declaration in let-form"
  (destructuring-bind (let-type bindings &body body) let-form
    `(,let-type ,bindings ,declaration ,@body)))

(defun put-type-decl-let (let-form typed-vars)
  "inserts a (declare (type ...)) form into let-form according to
typed-vars, verifying which of those are bound by let-form"
  (let* ((bindings (second let-form))
         (let-variables (mapcar #'first bindings))
         (bound-locals (remove-if-not (lambda (type-var)
                                        (member (second type-var)
                                                let-variables))
                                      typed-vars)))
    (put-decl-let let-form (create-type-declaration bound-locals))))

(defun modify-all-lets (body typed-bindings)
  "inserts a (declare (type ...)) form into every let form of body
according to typed-vars, verifying which of those are bound by each
let form"
  (let ((typed-vars (mapcar (lambda (tb)
                              (lists:take 2 tb))
                            typed-bindings))
        (new-bindings (mapcar (lambda (tb)
                                (lists:drop 1 tb))
                              typed-bindings)))
    (lists:map-sexp (lambda (sexp)
                      (if (let? sexp)
                          (progn
                            (setf sexp (update-let-bindings sexp new-bindings))
                            (put-type-decl-let sexp typed-vars))
                          sexp))
                    body
                    :copy t)))

(defun create-type-tuples (typed-vars)
  "creates (type ...) tuples to a (declare ...) form according to typed-vars"
  (loop for (type var) in typed-vars
        collect `(type ,type ,var)))

(defun create-type-declaration (typed-vars)
  "creates a declaration specifying the type of some variables according to typed-vars"
  `(declare ,@(create-type-tuples typed-vars)))

(defun create-optimized-body (body return-type typed-vars typed-locals)
  "creates a (locally ...) form equivalent to body, optimized
according to typed-vars, typed-locals and return-type"
  (let ((modified-body (modify-all-lets body typed-locals)))
    `(locally
         (declare ,+agressive-optimize-tuple+
                  ,@(create-type-tuples typed-vars))
       (the ,return-type (progn ,@modified-body)))))

(defun treat-spec (spec body)
  "creates a cond clause responsible for the execution of an optimized
version of body built according to spec"
  (destructuring-bind (return-type typed-vars &key (locals nil)) spec
    `((and ,@(loop for (type var) in typed-vars
                   collect `(typep ,var ',type)))
      ,(create-optimized-body body return-type typed-vars locals))))

(defun treat-tagged-spec (tagged-spec body)
  "creates a case clause responsible for the execution of an optimized
version of body built according to tagged-spec"
  (destructuring-bind (tag (return-type typed-vars &key (locals nil))) tagged-spec
    `(,tag ,(create-optimized-body body return-type typed-vars locals))))

(defmacro defun-tag (name lambda-list (&rest tagged-specs) &body body)
  "defines a function named name with lambda-list as signature. An
additional &key argument (tag :default) is providded. Each tagged-spec
has the form (tag (return-type ((type sig-var) ...)  :locals ((type
local-var init-val) ...))).  For each tagged-spec, an optimized
version of body is created according to the type information
provided. Execution is directed to the corresponding version of body,
according to the tag argument received. In case :default or no tag is
passed, an unoptimized version of body is executed. For instance,
(spec:defun-tag add-one (x)
    ((:fixnum (fixnum ((fixnum x))
              :locals ((fixnum a 1))))
     (:single-float (single-float ((single-float x))
                    :locals ((single-float a 1.0)))))
  (let ((a 1))
    (+ x a)))
expands to
(defun add-one (x &key (tag :default))
  (case tag
    (:fixnum
     (locally
         (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)
                            (compilation-speed 0))
                  (type fixnum x))
       (the fixnum
            (progn (let ((a 1))
                     (declare (type fixnum a))
                     (+ x a))))))
    (:single-float
     (locally
         (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)
                            (compilation-speed 0))
                  (type single-float x))
       (the single-float
            (progn (let ((a 1.0))
                     (declare (type single-float a))
                     (+ x a))))))
    (:default
     (let ((a 1))
       (+ x a)))))
"
  (let ((modified-lambda-list (append lambda-list
                                      (if (member '&key lambda-list)
                                          '((tag :default))
                                           '(&key (tag :default))))))
    (if (stringp (first body))
        (let ((expanded-body (mapcar #'sb-cltl2:macroexpand-all (rest body))))
          `(defun ,name ,modified-lambda-list
             ,(first body)
             (case tag
               ,@(loop for tagged-spec in tagged-specs
                       collect (treat-tagged-spec tagged-spec expanded-body))
               (:default ,@(rest body)))))
        (let ((expanded-body (mapcar #'sb-cltl2:macroexpand-all body)))
          `(defun ,name ,modified-lambda-list
             (case tag
               ,@(loop for tagged-spec in tagged-specs
                       collect (treat-tagged-spec tagged-spec expanded-body))
               (:default ,@body)))))))
