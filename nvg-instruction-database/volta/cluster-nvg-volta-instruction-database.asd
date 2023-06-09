(cl:in-package #:asdf-user)

(defsystem :cluster-nvg-volta-instruction-database
  :depends-on (:cluster)
  :serial t
  :components
  ((:file "nvg-volta-set")
   (:file "mov")
   (:file "add")))
