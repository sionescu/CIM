(defsystem cim-test
  :author "keens"
  :license "LLGPL"
  :depends-on (:cim :fiveam :cl-ppcre :osicat)
  :components ((:module "test"
                :serial t
                :components
                ((:file :package)
                 (:file :option-parser)
                 (:file :main))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :cim)"))
		    (asdf:clear-system c)))
