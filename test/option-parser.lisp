
(in-package :cim.test)
(def-suite :cim)
(in-suite :cim)

(test short-opt-p
  (is (short-opt-p "-h"))
  (is (not (short-opt-p "--help")))
  (is (short-opt-p "-a"))
  (is (not (short-opt-p "-avl")))
  (is (not (short-opt-p "-"))))


(test long-opt-p
  (is (long-opt-p "--help"))
  (is (not (long-opt-p "-a")))
  (is (not (long-opt-p "-avl")))
  (is (not (long-opt-p "--"))))

(test parse-clause
  (finishes
    (parse-clause
     '(("-h" "--help") ()
       (return))))
  (let ((help (parse-clause
                 '(("-h" "--help") ()
                   (return)))))
    (is (equal (clause-doc help) ""))
    (is (null (clause-lambda-list help)))
    (is (member "--help" (clause-long-options help)))
    (is (member "-h" (clause-short-options help)))
    (is (equal (clause-aux-options help) nil))
    (is (eval `(let ((flag "-h"))
                 ,(clause-flag-match-condition 'flag help))))
    (is (not (eval `(let ((flag "-a"))
                      ,(clause-flag-match-condition 'flag help)))))))

(defvar *sample*
  '((("-c" "--compile") (file)
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

(test help-message
  (finishes
    (terpri)
    (dolist (c *sample*)
      (princ
       (clause-help-title
        (parse-clause c)))
      (terpri)))
  (finishes
    (terpri)
    (princ
     (generate-help-message (mapcar #'parse-clause *sample*)))))





