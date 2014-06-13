
(in-package :cim.impl)

(define-condition repl-entered (condition)
  ())

(defun main-core ()
  (cond
    ((opt :repl) (repl))
    ((opt :eval)
     ;; then the command is:
     ;; cl ... -e "(dosomething)" -- [args]...
     ;; then the options are already treated.
     :already-treated)
    ((and (not (opt :eval)) (consp *argv*))
     ;; then the command is:
     ;; cl ... -- X.lisp [args]...
     (load (remove-shebang (open (car *argv*) :if-does-not-exist :error))))
    (t
     (signal 'repl-entered)
     (let ((*package* #.(find-package :common-lisp-user)))
       (loop (eval (read)))))))

(defun process-in-place (ext)
  (dolist (file (if (opt :eval) *argv* (cdr *argv*)))
    (with-open-file (*standard-input* file :if-does-not-exist :error)
      (if (zerop (length ext))
          (delete-file file)
          (rename-file file (concatenate 'string file ext)))
      (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
        (main-core)))))


(defun main (&optional (exit-status 0) (raw-argv (get-raw-argv)))
  ;; read the *raw-argv* and store the hooks
  (multiple-value-bind (*argv* hooks)
      (process-args raw-argv)

    ;; defaulting the current directory
    (if (equal *default-pathname-defaults* #p"")
        (setf *default-pathname-defaults*
              (pathname (getenv "PWD"))))

    ;; modify the verbosity
    (let ((*load-print* (>= (or (opt :verbosity) 0) 1))
          (*load-verbose* (>= (or (opt :verbosity) 0) 2)))

      ;; load the init script
      (unless (opt :no-init)
        (load (cim_home "/init.lisp")))

      ;; run the stored hooks if any
      (mapc #'funcall hooks))

    (let ((ext (opt :in-place-backup-extention)))
      (if ext (process-in-place ext) (main-core)))
    (exit exit-status)))
