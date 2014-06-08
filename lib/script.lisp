;;

;; (load (cim_home "/lib/option-parser.lisp")  :verbose nil :print nil)

(require :cim)
(in-package :cim)

(let ((sexps))
  (setf *argv*
	(parse-options *raw-argv*
		       (("-c" "--compile") (file)
			"compile FILE."
			(push `(compile-file ,file) sexps))
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
		       (("-Q" "--quit") ()
			"quit "
			(push '(exit) sexps))
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
			(setf *help* (format nil "~A~2%~A~%~A~%"
					     "Usage: cl [switchs] [--] [programfile] [argumensts]"
					     generated-help
					     "If neither programfile, -e (--eval) nor -r (--repl) are specified, cl reads scripts from the standard input and then eval them."))
			(setf (opt :help) t))
		       (("-v" "--version") ()
			"print the version" (setf (opt :version) t))))

  (if (equal *default-pathname-defaults* #p"")
      (setf *default-pathname-defaults*
	    (pathname (getenv "PWD"))))
  (cond
    ((opt :version)
     (write-line (version)))
    ((opt :help)
     (write-line *help* ))
    (t
     (unless (opt :no-init) (load (cim_home "/init.lisp") :verbose nil :print nil))
     (in-package :cl)
     (dolist (sexp (nreverse cim::sexps)) 
       (eval sexp))
     (in-package :cim)
     (flet ((main ()
              (cond
                ((opt :repl) (repl))
                ((opt :sexp)
                 (let ((+eof+ (gensym "eof")))
                   (with-input-from-string (in (opt :sexp))
                     (loop :for sexp := (read in nil +eof+)
                        :until (eq sexp +eof+) :do
                        (eval sexp)))))
                ((car *argv*)
                 (let ((*load-print* nil))
                   (load (remove-shebang (open (pop *argv*) :if-does-not-exist :error))
                         :verbose nil :print nil)))
                (t
                 (loop (eval (read)))))))
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
	   (main))))))
(exit)
