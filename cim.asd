(defsystem cim
  :version
  (with-open-file (stream (merge-pathnames
                           #p"VERSION"
                           (or *load-pathname* *compile-file-pathname*))
                          :if-does-not-exist nil
                          :direction :input)
    (when stream
      (let ((seq (make-array (file-length stream)
                             :element-type 'character
                             :fill-pointer t)))
        (setf (fill-pointer seq) (read-sequence seq stream))
        seq)))
  :author "keens"
  :license "LLGPL"
  :components ((:module "lib"
			:serial t
			:components
			((:file :packages)
                         (:file :debug)
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
