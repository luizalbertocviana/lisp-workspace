(defsystem :matrix
  :description "matrix representations and operations"
  :author "Luiz Alberto do Carmo Viana <luizalbertocviana@gmail.com>"
  :depends-on ("macros")
  :components ((:file "matrix")
               (:module "triangular"
                :depends-on ("matrix")
                :components ((:file "upper")
                             (:file "lower" :depends-on ("upper"))
                             (:file "diagonal")))))
