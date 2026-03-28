
(defsystem "named-languages"
  :author "Héctor Galbis Sanchis"
  :description "Named languages for Common Lisp"
  :depends-on ("named-readtables" "alexandria")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "language")))))
