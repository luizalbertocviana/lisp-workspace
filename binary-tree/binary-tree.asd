(defsystem :binary-tree
  :description "implementations of binary search trees"
  :author "Luiz Alberto do Carmo Viana <luizalbertocviana@gmail.com>"
  :depends-on (:macros)
  :components ((:file "bstree")
               (:file "avltree" :depends-on ("bstree"))))
