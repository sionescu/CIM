
(in-package :cim)

(defun main-core ()
  (cond
    ((opt :repl) (repl))
    ((listp *argv*)
     (load (remove-shebang (open (pop *argv*) :if-does-not-exist :error))))
    (t
     (let ((*package* #.(find-package :common-lisp-user)))
       (loop (eval (read)))))))

(defun process-in-place (ext)
  (dolist (file (if (opt :eval) *argv* (cdr argv)))
    (with-open-file (*standard-input* file :if-does-not-exist :error)
      (if (zerop (length ext))
          (delete-file file)
          (rename-file file (concatenate 'string file ext)))
      (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
        (main-core)))))


(defun main ()
  ;; read the *raw-argv* and store the hooks
  (process-args)

  ;; defaulting the current directory
  (if (equal *default-pathname-defaults* #p"")
      (setf *default-pathname-defaults*
            (pathname (getenv "PWD"))))

  ;; do not be verbose
  (let ((*load-print* nil)
        (*load-verbose* nil))

    ;; load the init script
    (unless (opt :no-init)
      (load (cim_home "/init.lisp")))

    ;; run the stored hooks if any
    (mapc #'funcall *hooks*))

  (let ((ext (opt :in-place-backup-extention)))
    (if ext (process-in-place ext) (main)))

  (exit))

