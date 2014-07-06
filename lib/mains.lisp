
(in-package :cim.impl)

(define-condition read-from-stdin (simple-warning)
  ()
  (:documentation "Signalled in order to tell the user that
the implementation is not just freezed, rather waiting for some input.
It might also be used by the testing scripts.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "~&Reading from stdin...~&"))))

(defun main-core (hooks)
  ;; run the stored hooks if any
  (mapc #'funcall hooks)
  (cond
    ((opt :repl)
     (with-protected-package ()
       (repl)))
    ((opt :eval)
     ;; then the command is:
     ;; cl ... -e "(dosomething)" -- [args]...
     ;; then the options are already treated
     ;; by (mapc #'funcall hooks)
     :already-treated)
    ((opt :quit)
     (return-from main-core))
    ((consp *argv*)
     ;; then the command is:
     ;; cl ... -- X.lisp [args]...
     ;; and X.lisp is a shebang
     (destructuring-bind (filename . *argv*) *argv*
       (with-protected-package ()
         (shebang-load filename))))
    (t
     (warn 'read-from-stdin)
     (with-protected-package ()
       (handler-case
           (loop (eval (read)))
         (end-of-file (c)
           (declare (ignore c))))))))

(defun process-in-place (ext hooks)
  (dolist (file (if (opt :eval) *argv* (cdr *argv*)))
    (with-open-file (*standard-input* file :if-does-not-exist :error)
      (if (zerop (length ext))
          (delete-file file)
          (rename-file file (concatenate 'string file ext)))
      (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
        (main-core hooks)))))

(defun main (&optional (exit-status 0) (raw-argv (get-raw-argv)))
  ;; read the *raw-argv* and store the hooks
  (multiple-value-bind (*argv* hooks)
      (process-args raw-argv)

    ;; defaulting the current directory
    (if (equal *default-pathname-defaults* #p"")
        (setf *default-pathname-defaults*
              (pathname-as-directory (pathname (getenv "PWD")))))

    ;; modify the verbosity
    (let ((*load-print* (>= (or (opt :verbosity) 0) 1))
          (*load-verbose* (>= (or (opt :verbosity) 0) 2)))

      ;; load the init script
      (unless (opt :no-init)
        (with-protected-package ()
          (load (cim_home "/init.lisp"))))

      (let ((ext (opt :in-place-backup-extention)))
        (if ext (process-in-place ext hooks) (main-core hooks))))
    (exit exit-status)))
