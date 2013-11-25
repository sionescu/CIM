;;
(defpackage cim
  (:use :cl)
  (:export :*argv*))
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
(defvar extention nil)
(defvar no-init nil)
(defvar version nil)
(defvar sexps ())

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
#+nil
(defun parse-options ()
  (flet ((push-cd (dir)
	   (let ((dir (if (char= #\/ (elt dir 1 ))
			  (pathname dir)
			  (merge-pathnames (pathname dir)))))
	     (push `(setf *default-pathname-defaults* ,dir) sexps)))
	 (push-debug () (push '(push :debug *features*) sexps))
	 (push-eval (sexp) (push `(eval (read (make-string-input-stream ,sexp))) sexps))
	 (set-help () (setf help t))
	 (set-extention (ext) (setf extention ext))
	 (push-load (file) (push `(load ,file) sexps))
	 (push-quickload (sys) (push `(ql:quickload ,(intern sys :keyword)) sexps))
	 (push-quickload-and-use (sys)
	   (let ((sys (intern sys :keyword)))
	     (push `(ql:quickload ,sys) sexps)
	     (push `(use-package  ,sys) sexps)))
	 (set-no-init () (setf no-init t))
	 (set-version () (setf version t)))
    (do ((opt (pop *argv*) (pop *argv*))) ((null *argv*))
      (declare (type string opt))
      (cond
	((string= "--"  opt) (push `(script ,(pop *argv*)) sexps) (return-from parse-options))
	((string= "--" (subseq opt 0 2))
	 (let ((opt (subseq opt 2)))
	   (cond
	     ((string= opt "debug") (push-debug))
	     ((string= opt "eval") (push-eval (pop *argv*)))
	     ((string= opt "help") (set-help))
	     ((string= opt "load") (push-load (pop *argv*)))
	     ((string= opt "no-init") (set-no-init))
	     ((string= opt "version") (set-version)))))
	((char= #\- (elt opt 0))
	 (let ((flags (cdr (coerce opt 'list))))
	   (do ((flag (pop flags) (pop flags)))
	       ((null flags))
	     (case flag
	       ((#\C) (if (cdr flags)
			  (push-cd (coerce (cdr flags) 'string))
			  (push-cd (pop *argv*))))
	       ((#\d) (push-debug))
	       ((#\e) (if (cdr flags)
			  (push-eval (coerce (cdr flags) 'string))
			  (push-eval (pop *argv*)))
		(return-from nil))
	       ((#\f) (if (cdr flags)
			  (push-load (coerce (cdr flags) 'string))
			  (push-load (pop *argv*)))
		(return-from nil))
	       ((#\h) (set-help))
	       ((#\i) (set-extention (coerce (cdr flags) 'string)))
	       ((#\l) (if (cdr flags)
			  (push-quickload (coerce (cdr flags) 'string))
			  (push-quickload (pop *argv*))))
	       ((#\L) (if (cdr flags)
			  (push-quickload-and-use (coerce (cdr flags) 'string))
			  (push-quickload-and-use (pop *argv*))))
	       ((#\q) (set-no-init))
	       ((#\v) (set-version))))))))))
#+nil
(defun script (&rest args)
  "Execute a file as script ignoring shebang."
  (let ((scriptfile (unless sexp (if scriptpath (which (shift)) (shift)))))
    (symbol-macrolet ((in (if sexp
			      (make-string-input-stream sexp)
			      (remove-shebang (open scriptfile :if-does-not-exist :error)))))
      (when debug (pushnew :debug *features*))
      (when load-path (setf ($ |:|) (nconc (mapcar #'truename (cl-ppcre:split ":" load-path)) ($ |:|))))
      (when cd (setf *default-pathname-defaults* (truename cd)))
      (if (or splitline printline)
	  (loop while (setf ($ _) (read-line ($ <) nil nil t)) do
	       (progn
		 (if autosplit (setf ($ F) (cl-ppcre:split (or F "\\s+") ($ _))))
		 (unless ending (setf ($ _) (format nil "~a~%" ($ _))))
		 (load in)
		 (if ending (setf ($ _) (format nil "~a~%" ($ _))))
		 (if printline (princ ($ _) ($ >)))))
	  (load in))))
  (values))
(load "/home/kim/.cim/lib/repl.lisp")
