(in-package :cl-user)

(defpackage cim
  (:use :cl)
  (:export :*argv*
	   :getenv
	   :parse-options
	   :opt))

(defpackage cim.repl
  (:use :CL :cim)
  (:export :*history*
	   :print-prompt
	   :with-handle-conditions
	   :strf
           :repl))

