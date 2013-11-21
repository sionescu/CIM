(defpackage cim
  (:use :cl)
  (:export :args))
(in-package :cim)
(defun args ()
  #+allegro  (cdr (system:command-line-arguments))
  #+sbcl (do  ((list sb-ext:*posix-argv* (cdr sb-ext:*posix-argv*)))
	      ((string= (car list) "--") (return (cdr list))))
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+abcl extensions:*command-line-argument-list*
  #+gcl (si::command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*unprocessed-command-line-argument-list*
  #+lispworks system:*line-arguments-list*)
