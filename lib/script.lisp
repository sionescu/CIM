;;
(defpackage cim
  (:use :cl)
  (:export :*argv*
	   :repl))
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
 -r --repl		run repl
 -q, --no-init		do not load ~/.lisprc
 -v, --version		print the version
if neither programfile, -e(--eval) nor -r(--repl) are specified, cl reads scripts from the standard input and then eval them.
")
(defvar help nil)
(defvar extension nil)
(defvar no-init nil)
(defvar version nil)
(defvar sexps ())
(defvar sexpgiven nil)
(defvar replgiven nil)

(defun remove-shebang (in)
  (let* ((first-char (read-char in))
	 (second-char (read-char in)))
    (cond
      ((and (char= first-char #\#) (char= second-char #\!))
       (read-line in))
      (t (unread-char second-char in)
	 (unread-char first-char in)))
    in))

(defun open-files (files)
  (apply #'make-concatenated-stream
   (mapcar (lambda (file) (open file :if-does-not-exist :error))
	   files)))

(defun without-ext (stream)
  (let ((stream (if (typep stream 'concatenated-stream)
		    (car (concatenated-stream-streams stream))
		    stream)))
    (make-pathname :name (pathname-name stream) :directory (pathname-directory stream))))

(setf (symbol-function ' mv)  #'rename-file)

(defun parse-options ()
  (flet ((push-cd (dir)
	   (let ((dir (if (char= #\/ (elt dir 1 ))
			  (pathname dir)
			  (merge-pathnames (pathname dir)))))
	     (push `(setf *default-pathname-defaults* ,dir) sexps)))
	 (push-debug () (push '(push :debug *features*) sexps))
	 (push-eval (sexp)
	   (push `(eval (read (make-string-input-stream ,sexp))) sexps)
	   (setf sexpgiven t))
	 (set-help () (setf help t))
	 (set-extension (ext) (setf extension ext))
	 (push-load (file) (push `(load ,file) sexps))
	 (push-quickload (sys) (push `(#+nil ql:quickload ,(intern sys :keyword)) sexps))
	 (push-quickload-and-use (sys)
	   (let ((sys (intern sys :keyword)))
	     (push `(#+nil ql:quickload ,sys) sexps)
	     (push `(use-package  ,sys) sexps)))
	 (set-no-init () (setf no-init t))
	 (set-version () (setf version t))
	 (set-repl () (setf replgiven t)))
    (do ((opt (pop *argv*) (pop *argv*))) (nil)
      (cond
	((string= "--"  opt) (push `(script ,(pop *argv*)) sexps) (return-from parse-options))
	((and (> (length opt) 2) (string= "--" (subseq opt 0 2)))
	 (let ((opt (subseq opt 2)))
	   (cond
	     ((string= opt "debug") (push-debug))
	     ((string= opt "eval") (push-eval (pop *argv*)))
	     ((string= opt "help") (set-help))
	     ((string= opt "load") (push-load (pop *argv*)))
	     ((string= opt "no-init") (set-no-init))
	     ((string= opt "version") (set-version))
	     ((string= opt "repl") (set-repl)))))
	((and (> (length opt) 1) (char= #\- (elt opt 0)))
	 (let ((flags (cdr (coerce opt 'list))))
	   (do ((flag (pop flags) (pop flags)))
	       (nil)
	     (case flag
	       ((#\C) (if (cdr flags)
			  (push-cd (coerce (cdr flags) 'string))
			  (push-cd (pop *argv*)))
		(return-from nil))
	       ((#\d) (push-debug))
	       ((#\e) (if (cdr flags)
			  (push-eval (coerce (cdr flags) 'string))
			  (push-eval (pop *argv*)))
		(return-from nil))
	       ((#\f) (if (cdr flags)
			  (push-load (coerce (cdr flags) 'string))
			  (push-load (pop *argv*)))
		(return-from nil))
	       ((#\h)  (set-help)
		(return-from nil))
	       ((#\i) (set-extension (coerce (cdr flags) 'string))
		(return-from nil))
	       ((#\l) (if (cdr flags)
			  (push-quickload (coerce (cdr flags) 'string))
			  (push-quickload (pop *argv*)))
		(return-from nil))
	       ((#\L) (if (cdr flags)
			  (push-quickload-and-use (coerce (cdr flags) 'string))
			  (push-quickload-and-use (pop *argv*)))
		(return-from nil))
	       ((#\q) (set-no-init))
	       ((#\v) (set-version)))
	     (unless (cdr flags) (return-from nil)))))
	(t (push opt *argv*) (return-from parse-options))))))

(defun script (programfile)
  "Execute a file as script ignoring shebang."
  (let ((in (remove-shebang (open programfile :if-does-not-exist :error))))
    (load in)
    (values)))
(and *argv* (parse-options))
(cond
  (version (format t "~A~%" +VERSION+))
  (help    (format t "~A~%" *help*))
  (t (cond
       (sexpgiven (dolist (sexp sexps) (eval sexp)))
       (replgiven (load "/home/kim/.cim/lib/repl.lisp"))
       (*argv* (script (pop *argv*)))
       (t (loop (handler-case
		    (eval (read))
		  (condition () (return 1))))))))
#-(or sbcl allegro) (cl-user::quit)
#+sbcl (sb-ext::exit)
#+allegro (cl-user::exit)
