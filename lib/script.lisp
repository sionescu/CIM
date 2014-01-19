;;
(defpackage cim
  (:use :cl)
  (:export :*argv*
	   :repl
	   :getenv))
(in-package :cim)
(defvar *argv*
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

(defvar *argf* *standard-input*)
(defconstant +VERSION+ "0.0.1")
(defvar *help*
"Usage: cl [switchs] [--] [programfile] [argumensts]
 -C DIR			set *default-pathname-defaults* DIR, before executing your script
 -d, --debug		set debugging flags (push :debug into *features*)
 -e, --eval SEXP	one line of script. Several -e's allowed. Omit [programfile]
 -f, --load file	load the file
 -h, --help		print this help
 -i[extxntion]		edit *argv* files in place and make backup with the extension .EXT
 -l library		quickload the library
 -L library 		quickload and use-package the library
 -r, --repl		run repl
 -q, --no-init		do not load ~/.lisprc
     --no-rl		do not use rlwrap
     --no-right		do not display right prompt. This is effective only --repl is specified
     --no-color         do not use color. This is effective only --repl is specified
 -v, --version		print the version
if neither programfile, -e(--eval) nor -r(--repl) are specified, cl reads scripts from the standard input and then eval them.
")
(defvar help nil)
(defvar extension nil)
(defvar no-init nil)
(defvar no-right nil)
(defvar no-color nil)
(defvar version nil)
(defvar sexps ())
(defvar replgiven nil)
(defvar sexpgiven nil)

(defun remove-shebang (in)
  (let* ((first-char (read-char in))
	 (second-char (read-char in)))
    (cond
      ((and (char= first-char #\#) (char= second-char #\!))
       (read-line in))
      (t (unread-char second-char in)
	 (unread-char first-char in)))
    in))

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun open-files (files)
  (apply #'make-concatenated-stream
   (mapcar (lambda (file) (open file :if-does-not-exist :error))
	   files)))

(defun without-ext (stream)
  (let ((stream (if (typep stream 'concatenated-stream)
		    (car (concatenated-stream-streams stream))
		    stream)))
    (make-pathname :name (pathname-name stream) :directory (pathname-directory stream))))

(defun cim_home (path)
  (concatenate 'string (getenv "CIM_HOME") path))
(defun ql_home (path)
  (concatenate 'string (getenv "HOME") path))

(load (cim_home "/lib/option-parser.lisp") )

(parse-options *argv*
	       (("-C") "set *default-pathname-defaults* DIR, before executing your script"
		((dir)
		 (let ((dir (if (char= #\/ (elt dir 1 ))
				(pathname dir)
				(merge-pathnames (pathname dir)))))
		   (push `(setf *default-pathname-defaults* ,dir) sexps))))
	       (("-d" "--debug") "set debugging flags (push :debug into *features*)"
		(() (push '(push :debug *features*) sexps)))
	       (("-e" "--eval") "one line of script. Several -e's allowed. Omit [programfile]"
		((sexp)
		 (push `(eval (read (make-string-input-stream ,sexp))) sexps)
		 (setf sexpgiven t)))
	       (("-f" "--load") "load the file"
		((file)))
	       (("i") "edit *argv* files in place and make backup with the extension .EXT"
		((ext) (setf extension ext)))
	       (("-l") "quickload the library"
		((library) (push `(#+nil ql:quickload ,(intern library :keyword)) sexps)))
	       (("-L") "quickload and use-package the library"
		((library)
		 (let ((sys (intern library :keyword)))
		   (push `(#+nil ql:quickload ,sys) sexps)
		   (push `(use-package  ,sys) sexps))))
	       (("-r" "--repl") "run repl"
		(() (setf replgiven t)))
	       (("-q" "--no-init") "do not load ~/.lisprc"
		(() (setf no-init t)))
	       (("--no-rl") "do not use rlwrap"
		(()))
	       (("--no-right") "do not display right prompt. This is effective only --repl is specified"
		(() (setf no-right t)))
	       (("--no-color") "do not use color. This is effective only --repl is specified"
		(() (setf no-color t)))
	       (("-v" "--version") "print the version"
		(() (setf version t))))


(defun script (programfile)
  "Execute a file as script ignoring shebang."
  (let ((in (remove-shebang (open programfile :if-does-not-exist :error))))
    (load in)
    (values)))

(if (equal *default-pathname-defaults* #p"")
    (setf *default-pathname-defaults*
	  (pathname (getenv "PWD"))))

(cond
  (version (format t "~A~%" +VERSION+))
  (help    (format t "~A~%" *help*))
  (t
   (unless no-init (load (cim_home "/init.lisp")))
   (dolist (sexp (nreverse sexps)) (eval sexp))
   (cond
     (replgiven (load (cim_home "/lib/repl.lisp")))
     ((and (not sexpgiven) *argv*) (script (pop *argv*)))
     ((not *argv*) (loop (handler-case
			     (eval (read))
			   (condition () (return 1))))))))
#-(or sbcl allegro) (cl-user::quit)
#+sbcl (sb-ext::exit)
#+allegro (cl-user::exit)
