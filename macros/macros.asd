(defsystem :macros
  :description "some simple macros"
  :author "Luiz Alberto do Carmo Viana <luizalbertocviana@gmail.com>"
  :depends-on (:functions)
  :components ((:file "macros")
               (:file "aliases" :depends-on ("macros"))
               (:file "compiling" :depends-on ("macros"))
               (:file "spec")))
