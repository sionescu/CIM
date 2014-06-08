(in-package :cl-user)

;; implementation package -- used by testing script
(defpackage cim.impl
  (:use :cl)
  (:export :*argv*
	   :getenv
	   :parse-options
	   :opt
           :short-opt-p
           :long-opt-p))

(defpackage cim.repl
  (:use :CL :cim.impl)
  (:export :*history*
	   :print-prompt
	   :with-handle-conditions
	   :strf
           :repl))

;; public package -- only a limited number of symbols are exported
(defpackage cim
  (:use :cl :cim.impl)
  (:export :*argv*
	   :getenv
	   :parse-options
	   :opt))
