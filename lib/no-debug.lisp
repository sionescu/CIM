

(in-package :cim.impl)

;; This file is loaded only when the system is compiled by built.sh, in other words
;; only loaded during the production environment.
;; It affects the compilation of option-parser.

;; see also : debug.lisp

(defvar *cim-option-match-verbose* nil)
