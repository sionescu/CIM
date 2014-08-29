;;
(defpackage cim
  (:use :CL)
  (:export :*argv*
           :*interrupt-condition*
           :getenv
           :parse-options
           :opt
           :exit))
(in-package :cim)
(defvar *raw-argv*
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
  #+ccl ccl:*unprocessed-command-line-arguments*
  #+lispworks system:*line-arguments-list*)
(defvar *argv*)

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+abcl (java:jstatic "getenv" "java.lang.System" name)
   #+allegro (sys:getenv name)
   #+clisp (ext:getenv name)
   #+ccl (ccl:getenv name)
   #+ECL (si:getenv name)
   #+gcl (si::getenv name)
   #+sbcl (sb-unix::posix-getenv name)
   #+lispworks (lispworks:environment-variable name)
   default))

(defun cim_home (path)
  (concatenate 'string (getenv "CIM_HOME") path))
(defun ql_home (path)
  (concatenate 'string (cim_home "/quicklisp") path))

(defvar *help*)
(defvar *options* (make-hash-table))

(defvar *interrupt-condition*
  ;; It seems abcl does not raise any conditions
  #+allegro 'excl:interrupt-signal
  #+ccl 'ccl:interrupt-signal-condition
  #+clisp 'system::simple-interrupt-condition
  #+ecl 'ext:interactive-interrupt
  #+sbcl 'sb-sys:interactive-interrupt
  #-(or allegro ccl clisp ecl sbcl) 'no-conditon-known)
(setf *load-verbose* nil)

(let ((v))
  (defun version ()
    (or v
        (setf v
              (with-open-file (in (cim_home "/VERSION"))
                (read-line in))))))

(defun opt (key)
  (gethash key *options*))
(defsetf opt (key) (obj)
  `(setf (gethash ,key *options*) ,obj))

(defun remove-shebang (in)
  (let ((line (read-line in nil "#!")))
    (cond
      ((and (> (length line) 1) (string= line "#!" :end1 2))
       in)
      (t
       (make-concatenated-stream (make-string-input-stream line) in)))))

(defun read-stream-into-string (stream)
  (let* ((buffer-size 4096)
         (buffer (make-array buffer-size :element-type 'character)))
    (with-output-to-string (str)
     (loop
        :for bytes-read = (read-sequence buffer stream)
        :do (write-sequence buffer str :start 0 :end bytes-read)
        :while (= bytes-read buffer-size)))))

(defun exit ()
  #-(or sbcl allegro) (cl-user::quit)
  #+sbcl (sb-ext::exit)
  #+allegro (cl-user::exit))

(defun canonicalize-path (path)
  (if (char= #\/ (aref path (1- (length path))))
      path
      (concatenate 'string path "/")))

(defun ensure-quicklisp ()
  #-quicklisp
  (let ((quicklisp-init (ql_home "/setup.lisp")))
    (when (probe-file quicklisp-init)
      (load quicklisp-init :verbose nil))))

(load (cim_home "/lib/option-parser.lisp")  :verbose nil :print nil)

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
                       (("--core") (file)
                        "use the specified core file"
                        (declare (ignore file))
                        nil)
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
            (pathname (canonicalize-path (getenv "PWD")))))
  (cond
    ((opt :version)
     (write-line (version)))
    ((opt :help)
     (write-line *help* ))
    (t
     (unless (opt :no-init) (load (cim_home "/init.lisp") :verbose nil :print nil))
     (in-package :cl)
     (handler-case
      (dolist (sexp (nreverse cim::sexps)) 
        (eval sexp))
       (#.*interrupt-condition* () (exit)))
     (in-package :cim)
     (macrolet ((main ()
                  '(handler-case
                    (cond
                      ((opt :repl)
                       (load (cim_home "/lib/repl.lisp") :verbose nil :print nil))
                      ((opt :sexp)
                       (let ((+eof+ (gensym "eof"))
                             (*package* (find-package :cl)))
                         (with-input-from-string (in (opt :sexp))
                           (loop :for sexp := (read in nil +eof+)
                              :until (eq sexp +eof+) :do
                              (eval sexp)))))
                      ((car *argv*)
                       (let ((*load-print* nil)
                             (stream (remove-shebang (open (pop *argv*) :if-does-not-exist :error))))
                         #-ccl(load  stream
                                    :verbose nil :print nil)
                         #+ccl(let ((str (read-stream-into-string stream)))
                                (load (make-string-input-stream str)
                                      :verbose nil :print nil))))
                      (t
                       (let ((+eof+ (gensym "eof"))
                             (*package* (find-package :cl)))
                         (loop
                            :for sexp := (read *standard-input* nil +eof+)
                            :until (eq sexp +eof+) :do
                            (eval sexp)))))
                    (#.*interrupt-condition* () (exit)))))
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
