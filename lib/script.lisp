;;

(defpackage cim
  (:use :CL)
  (:export :*argv*
	   :repl
	   :getenv
	   :parse-options))
(in-package :cim)
(defvar *argv*
  #+allegro  (cdr (system:command-line-arguments))
  #+sbcl (do*  ((var sb-ext:*posix-argv* (cdr list))
		(list var var))
	       ((string= (car list) "--") (return (cdr list))))
  #+clisp ext:*args*
  #+ecl (do*  ((var (si:command-args) (cdr list))
	       (list var var))
	      ((string= (car list) "--") (return (cdr list))))
  #+abcl extensions:*command-line-argument-list*
  #+gcl (do*  ((var si::*command-args* (cdr list))
	       (list var var))
	      ((string= (car list) "--") (return (cdr list))))
  #+cmu ext:*command-line-words*
  #+ccl ccl:*unprocessed-command-line-argument-list*
  #+lispworks system:*line-arguments-list*)

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
   #+GCL (si::getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   #+ABCL (java:jstatic "getenv" "java.lang.System" name)
   default))

(defun cim_home (path)
  (concatenate 'string (getenv "CIM_HOME") path))
(defun ql_home (path)
  (concatenate 'string (cim_home "/quicklisp") path))

(defvar *help*)
(defvar *options* (make-hash-table))
(defvar sexps ())
(setf *load-verbose* nil)

(let ((v))
  (defun version ()
    (or v
	(setf v
	      (with-open-file (in (cim_home "/VERSION"))
		(read-line in))))))

(defun opt (key)
  (gethash key *options*))
(defun (setf opt) (obj key)
  (setf (gethash key *options*) obj))

(defun remove-shebang (in)
  (let ((line (read-line in nil "#!")))
    (cond
      ((and (> (length line) 1) (string= line "#!" :end1 2))
       in)
      (t
       (make-concatenated-stream (make-string-input-stream line) in)))))

(defun exit ()
  #-(or sbcl allegro) (cl-user::quit)
  #+sbcl (sb-ext::exit)
  #+allegro (cl-user::exit))

(defun ensure-quicklisp ()
  #-quicklisp
  (let ((quicklisp-init (ql_home "/setup.lisp")))
    (when (probe-file quicklisp-init)
      (load quicklisp-init :verbose nil))))

(load (cim_home "/lib/option-parser.lisp")  :verbose nil :print nil)
(setf *argv*
      (parse-options *argv*
		     (("-C") (dir)
		      "set *default-pathname-defaults* DIR."
		      (let ((dir (if (char= #\/ (elt dir 1 ))
				     (pathname dir)
				     (merge-pathnames (pathname dir)))))
			(push `(setf *default-pathname-defaults* ,dir) sexps)))
		     (("-d" "--debug") ()
		      "set debugging flags (push :debug into *features*)"
		      (push '(push :debug *features*) sexps))
		     (("-e" "--eval") (sexp)
		      "one line of script. Several -e's are allowed. Omit [programfile]"
		      (setf (opt :sexp) (concatenate 'string (opt :sexp) sexp)))
		     (("-f" "--load") (file)
		      "load the FILE"
		      (push `(load ,file) sexps))
		     (("-i") (ext)
		      "edit *argv* files in place and make backup with the extension .EXT"
		      (push `(setf (opt :extension) ,ext) sexps))
		     (("-l") (library)
		      "quickload the LIBRARY"
		      (push `(progn
			       (ensure-quicklisp)
			       (funcall (intern "QUICKLOAD" :ql) ,(intern (format nil "~:@(~A~)" library) :keyword))) sexps))
		     (("-L") (library)
		      "quickload and use-package the LIBRARY"
		      (let ((sys (intern (format nil "~:@(~A~)" library) :keyword)))
			(push `(progn
				 (ensure-quicklisp)
				 (funcall (intern "QUICKLOAD" :ql) ,sys)
				 (use-package  ,sys))
			      sexps)))
		     (("-r" "--repl") ()
		      "run repl"
		      (setf (opt :repl) t))
		     (("-q" "--no-init") ()
		      "do not load $CIM_HOME/init.lisp"
		      (setf (opt :no-init) t))
		     (("--no-rl") ()
		      "do not use rlwrap. This is effective only when --repl is specified"
		      ())
		     (("--no-right") ()
		      "do not display right prompt. This is effective only when --repl is specified"
		      (setf (opt :no-right) t))
		     (("--no-color") ()
		      "do not use color. This is effective only when --repl is specified"
		      (setf (opt :no-color) t))
		     (("-h" "--help") ()
		      "print this help"
		      (setf *help* generated-help)
		      (setf (opt :help) t))
		     (("-v" "--version") ()
		      "print the version" (setf (opt :version) t))))

(if (equal *default-pathname-defaults* #p"")
    (setf *default-pathname-defaults*
	  (pathname (getenv "PWD"))))
(cond
  ((opt :version)
   (format t "~A~%" (version)))
  ((opt :help)
   (format t "~A~2%" "Usage: cl [switchs] [--] [programfile] [argumensts]")
   (format t "~A~%" *help*)
   (format t "~A~%" "If neither programfile, -e (--eval) nor -r (--repl) are specified, cl reads scripts from the standard input and then eval them."))
  (t
   (unless (opt :no-init) (load (cim_home "/init.lisp") :verbose nil :print nil))
   (dolist (sexp (nreverse sexps)) (eval sexp))
   (macrolet ((main ()
		'(cond
		  ((opt :repl)
		   (load (cim_home "/lib/repl.lisp") :verbose nil :print nil))
		  (*argv*
		   (let ((*load-print* nil))
		     (load (if (opt :sexp)
			       (make-string-input-stream (opt :sexp))
			       (remove-shebang (open (pop *argv*) :if-does-not-exist :error)))
			   :verbose nil :print nil)))
		  ((not *argv*)
		   (loop (handler-case
			     (eval (read))
			   (condition () (return 1))))))))
     (if (opt :extension)
	 (let ((files (if (and (not (opt :sexp)) *argv*)
			  (cdr *argv*)
			  *argv*)))
	   (dolist (file files)
	     (setf *standard-input* (open file :if-does-not-exist :error))
	     (if (zerop (length (opt :extension)))
		 (delete-file file)
		 (rename-file file (concatenate 'string file (opt :extension))))
	     (setf *standard-output* (open file :direction :output :if-exists :supersede))
	     (main)))
	 (main)))))
(exit)
