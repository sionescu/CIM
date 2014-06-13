(defsystem cim
  :version "1.0.0"
  :author "keens"
  :license "LLGPL"
  :components ((:module "lib"
			:serial t
			:components
			((:file :packages)
                         (:file :cim)
                         (:file :option-parser)
                         (:file :repl)
                         (:file :process-args)
                         (:file :mains))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cim-test))))
