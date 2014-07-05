(in-package :cl-user)

;; implementation package -- used by testing script
(defpackage cim.impl
  (:use :cl)
  (:export :*argv*
	   :getenv
           :get-raw-argv
	   :opt
           :*options*
           :exit
           :cim_home
           :ql_home
           :remove-shebang
           :shebang-load
           :shebang-p
           :pathname-as-directory

           :short-opt-p
           :long-opt-p
           :combined-opt-p
           :make-option
           :explode-combined-opts
           :long-opt-value-p

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
           :read-from-stdin

           :process-args

           ;; repl
           :*history*
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
           :main
           :exit
           :pathname-as-directory

           :cim_home
           :ql_home

           :*history*
	   :print-prompt
	   :with-handle-conditions
	   :strf
           :repl))

(defpackage cim.repl
  (:use :cl)
  (:import-from :cim.impl
                :*history*
                :print-prompt
                :with-handle-conditions
                :strf
                :repl))
