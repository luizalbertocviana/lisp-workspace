(defsystem :graph
  :description "implementations of graph representations"
  :author "Luiz Alberto do Carmo Viana <luizalbertocviana@gmail.com>"
  :depends-on (:queue)
  :components ((:file "digraph")
               (:file "graph" :depends-on ("digraph"))
               (:file "algorithms")))
