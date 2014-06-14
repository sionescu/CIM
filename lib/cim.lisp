
(in-package :cim.impl)

(defun get-raw-argv ()
  #+allegro  (cdr (system:command-line-arguments))
  #+sbcl (do*  ((var sb-ext:*posix-argv* (cdr list))
                (list var var))
               ((or (null list)
                    (string= (car list) "--"))
                (return (cdr list))))
  #+clisp ext:*args*
  #+ecl (do*  ((var (si:command-args) (cdr list))
               (list var var))
              ((or (null list)
                   (string= (car list) "--")) (return (cdr list))))
  #+abcl extensions:*command-line-argument-list*
  #+gcl (do*  ((var si::*command-args* (cdr list))
               (list var var))
              ((or (null list)
                   (string= (car list) "--")) (return (cdr list))))
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
   #+allegro (sys:getenv name)
   #+clisp (ext:getenv name)
   #+ccl (ccl:getenv name)
   #+ECL (si:getenv name)
   #+gcl (si::getenv name)
   #+sbcl (sb-unix::posix-getenv name)
   #+lispworks (lispworks:environment-variable name)
   #+abcl (java:jstatic "getenv" "java.lang.System" name)
   default))

(defun cim_home (path)
  "string -> string"
  (assert (stringp path))
  (let ((raw-home (getenv "CIM_HOME" #+asdf (princ-to-string (asdf:system-source-directory :cim)))))
    (if (char= #\/ (aref raw-home (1- (length raw-home)))) ; trailing slash
        (if (char= #\/ (aref path 0))                      ; slash in the beginning
            (concatenate 'string raw-home (subseq path 1)) ; xxx..xx/ + /yyy/yy...
            (concatenate 'string raw-home path))           ; xxx..xx/ + ttt/tt...
        (if (char= #\/ (aref path 0))                      ; slash in the beginning
            (concatenate 'string raw-home path)            ; xxx..xx + /yyy/yy...
            (concatenate 'string raw-home "/" path)))))    ; xxx..xx + ttt/tt...

(defun ql_home (path)
  (concatenate 'string (cim_home "/quicklisp") path))

(defvar *help*)
(defvar *options* (make-hash-table))
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

(defun shebang-load (path)
  (with-open-file (in path :if-does-not-exist :error)
    (load (remove-shebang in))))

;; partly copied from @fukamachi 's shelly.util:terminate
(defun exit (&optional (status 0))
  #+ccl (ccl:quit status)
  ;; http://www.sbcl.org/manual/#Exit
  #+sbcl (sb-ext:exit :code status)
  ;; http://franz.com/support/documentation/9.0/doc/operators/excl/exit.htm
  #+allegro (excl:exit status :quiet t)
  #+clisp (ext:quit status)
  #+cmucl (unix:unix-exit status)
  #+ecl (ext:quit status)
  #-(or ccl sbcl allegro clisp cmucl ecl) (cl-user::quit))

(defun ensure-quicklisp ()
  #-quicklisp
  (let ((quicklisp-init (ql_home "/setup.lisp")))
    (when (probe-file quicklisp-init)
      (load quicklisp-init :verbose nil))))
