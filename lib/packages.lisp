(in-package :cl-user)

;; implementation package -- used by testing script
(defpackage cim.impl
  (:use :cl)
  (:export :*argv*
	   :getenv
	   :opt
           :exit

           :short-opt-p
           :long-opt-p
           :combined-opt-p
           :make-option
           :explode-combined-opts

           :parse-clause
           :clause-options
           :clause-short-options
           :clause-long-options
           :clause-aux-options
           :clause-lambda-list
           :clause-doc
           :clause-body
           :clause-help-title
           :clause-flag-match-condition

           :make-dispatcher-function
           :generate-help-message
           :make-parse-options
	   :parse-options

           :main
           :main-core
           :process-in-place

           :process-args
))

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
	   :opt
           :main))
