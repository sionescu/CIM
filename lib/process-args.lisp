
(in-package :cim.impl)

;;;; implementation strategy --
;; 
;; *raw-argv* is parsed sequentially.
;; Each time, the *raw-argv* is chopped off its head, and the
;; body of matching clause (e.g. `(compile-file file)') are
;; stored in *hooks* as a zero-arg closure
;; (e.g. `(lambda () (compile-file file))').

;; the stored closures should be manually executed afterwards.

(defmacro with-protected-package ((&key) &body body)
  ;; the package is protected and do not interfere the later evaluation
  `(let ((*package* (or (opt :package)
                        #.(find-package :common-lisp-user))))
     ,@body))

(defparameter *eval-stream* (make-string-input-stream ""))
(defun make-eval-closure (string)
  (lambda ()
    (block closure
      (with-protected-package ()
        (with-input-from-string (s string)
          (let ((in (make-concatenated-stream *eval-stream* s)))
            (loop
               (eval
                (let* ((read (make-string-output-stream))
                       (echo (make-echo-stream in read)))
                  ;; The characters which are read from `in' are
                  ;; automatically copied to the stream `read'
                  (handler-bind ((end-of-file
                                  (lambda (c)
                                    (declare (ignore c))
                                    (setf *eval-stream*
                                          (make-string-input-stream
                                           (get-output-stream-string read)))
                                    (return-from closure))))
                    (read echo)))))))))))

(defun process-args (argv)
  "Default argument processor in CIM."
  (parse-options argv
    ;;
    (("-c" "--compile") (file)
     "compile FILE = `filename.lisp' and save it into `filename.$LISP_IMPL' ."
     (add-hook
      (lambda ()
        (let ((p (pathname file)))
          (let ((fasl (merge-pathnames (fasl-pathname p))))
            (compile-file p :output-file fasl))))))

    (("-C") (dir)
     "set *default-pathname-defaults* to DIR."
     (add-hook
      (lambda ()
        (setf *default-pathname-defaults*
              (if (char= #\/ (elt dir 0))
                  (pathname dir)
                  (merge-pathnames (pathname dir)))))))

    (("-d" "--debug") ()
     "set debugging flags (push :debug into *features*)"
     (push :debug *features*))

    (("-e" "--eval") (sexp)
     "Evaluates a one-line script.
Once -e option is specified, [programfile] is ignored.
Several -e's are evaluated individually, and in the given order as specified.
For each time the evaluation is performed,
the package is reset to the default package `cl-user'
i.e. changes to the package is not saved among the processing.
The default package can be modified via -p option.
"
     (setf (opt :eval) t)
     (add-hook
      (make-eval-closure sexp)))

    (("-p" "--package") (package)
     "Modifies the default package, initially cl-user.
All -e commands are affected after this option.
Multiple -p overwrites the previous invocation of -p,
i.e. only the last -p is processed."
     (setf (opt :package) (find-package (string-upcase package))))

    (("-f" "--load") (file)
     "load the FILE. The file is searched under the effect of -C flag.
The file is loaded under the default package specified by -p, defaulted to `cl-user'."
     (add-hook
      (lambda ()
        (with-protected-package ()
          (load (merge-pathnames file))))))

    (("-i") (ext)
     "Edit the files specified in the remainder of *argv* in place,
 that is, take the file as input and write the output to the same file.
Using this option assumes the command takes filenames as arguments,
and the same processing is performed over those files.
There are two such cases:

  cl <flags> -- <script>.lisp [inputfile]...
  cl <flags> -e '(do-something *argv*)' -- [inputfile]...

The old input file will be backed up with the extension EXT.
For exammple, 'cl ... -i .old ... x.data' results in two files named
'x.data.old' and the modified file 'x.data'."
     (setf (opt :in-place-backup-extention) ext))

    (("-l") (library)
     "quickload the LIBRARY"
     (ensure-quicklisp)
     ;; speed does not matter.
     ;; ideally use of (intern ...) should be avoided.
     (add-hook
      (lambda ()
        (funcall (symbol-function
                  (read-from-string "ql:quickload"))
                 library))))

    (("-L") (library)
     "quickload and use-package the LIBRARY.
use-package is affected by -p,
so it is called in the same environment as -e option does."
     ;; use-package accepts string designator
     (add-hook
      (lambda ()
        (ensure-quicklisp)
        (funcall (symbol-function
                  (read-from-string "ql:quickload"))
                 library)
        (with-protected-package ()
          (use-package (string-upcase library))))))

    (("-r" "--repl") ()
     "Run the REPL. The default package is affected by -p option."
     (setf (opt :repl) t))

    (("-q" "--no-init") ()
     "do not load $CIM_HOME/init.lisp"
     (setf (opt :no-init) t))

    (("-Q" "--quit") ()
     "quit after processing all supplied commands"
     (setf (opt :quit) t))

    (("--no-rl") ()
     "do not use rlwrap. This is effective only when --repl is specified"
     (setf (opt :no-rl) t))

    (("--no-right") ()
     "do not display right prompt. This is effective only when --repl is specified"
     (setf (opt :no-right) t))

    (("--no-color") ()
     "do not use color. This is effective only when --repl is specified"
     (setf (opt :no-color) t))

    (("--version" "-v") ()
     "print the version of cim"
     (write-line (version))
     (exit))

    (("-V") ()
     "Specify the verbosity while loading files.
A verbosity is a fixnum if specified. Otherwise it is NIL. It is stored in (opt :verbosity).
If it is specified once, the value is 1.
Using duplicate options (e.g. -vvv) increases the verbosity value."
     (if (opt :verbosity)
         (incf (opt :verbosity))
         (setf (opt :verbosity) 1)))

    (("--verbose") (n)
     "Specify the verbosity while loading files. Unlike -V, it specifies the verbosity directly."
     (setf (opt :verbosity) n))))


