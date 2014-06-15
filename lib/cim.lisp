
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

(defun pathname-as-directory (pathname)
  (assert (null (pathname-type pathname)) ()
          "This pathname is not suitable for converting into a directory pathname")
  (if (pathname-name pathname)
      (make-pathname :name nil
                     :directory (append (pathname-directory pathname)
                                        (list (pathname-name pathname))))
      pathname))

(defun cim_home (&optional (path "/"))
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

(defun shebang-p (line)
  (and (> (length line) 1)
       (string= line "#!" :end1 2)))

(defun remove-shebang (in)
  (let ((line (read-line in nil "#!")))
    (cond
      ((shebang-p line) in)
      (t
       (make-concatenated-stream (make-string-input-stream line) in)))))

#+ccl
(defun copy-without-shebang (path)
  (print (merge-pathnames path) *error-output*)
  (with-open-file (in path :if-does-not-exist :error)
    (let ((tmp-path (merge-pathnames
                     (concatenate 'string (princ-to-string path) "__tmp"))))

      (with-open-file (tmp tmp-path
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
        (handler-case
            (tagbody
             start
               (let ((line (read-line in)))
                 (if (shebang-p line)
                     (terpri tmp)
                     (write-line line tmp)))
               (go start))
          (end-of-file (c)
            (declare (ignore c))
            ;; copy finished
            tmp-path))))))

(defun shebang-load (path)
  #-ccl
  (with-open-file (in path :if-does-not-exist :error)
    (load (remove-shebang in)))
  #+ccl
  (let ((new-path (copy-without-shebang path)))
    (assert (probe-file (merge-pathnames new-path)) ()
            "path ~a does not exist" (merge-pathnames new-path))
    (load new-path)))

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
