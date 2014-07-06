(defsystem cim-test
  :author "keens"
  :license "LLGPL"
  :depends-on (:cim :fiveam :cl-ppcre :osicat)
  :components ((:module "test"
                :serial t
                :components
                ((:file :package)
                 (:file :cim)
                 (:file :option-parser)
                 (:file :main)
                 (:file :main2))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :cim)"))
		    (asdf:clear-system c)))
