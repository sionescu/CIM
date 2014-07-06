

(in-package :cim.impl)

;; This file is loaded only when the system is compiled by asdf, in other words
;; only loaded during the debugging environment.

;; On the other hand, script.lisp is built without this file,
;; and with no-debug.lisp instead.
;; It affects the compilation of option-parser.

(pushnew :cim-option-match-verbose *features*)
