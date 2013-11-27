
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (my-getenv "CIM_HOME"))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

