(in-package :cim.test)
(in-suite :cim)

(defparameter *test-root*
  (load-time-value
   (princ-to-string
    (merge-pathnames
     "test/"
     (asdf:system-source-directory :cim-test)))))
(print *test-root*)

(defun fresh-main (argv)
  (let ((*options* (make-hash-table)))
    (let ((fdefinition (symbol-function 'exit)))
      (unwind-protect
           (progn
             (setf (symbol-function 'exit)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (warn "exited!")))
             (main 0 argv))
        (setf (symbol-function 'exit) fdefinition)))))

(test eval
  (is (string=
       "ABABAP"
       (with-output-to-string (*standard-output*)
         (fresh-main (list "-e" "(princ :ababap)"))))))

(test directory
  (is (string=
       *test-root*
       (with-output-to-string (*standard-output*)
         (fresh-main (list "-C" *test-root*
                           "-e" "(princ *default-pathname-defaults*)"))))))

(test load
  (finishes
    ;; load successful no matter what value
    ;; *default-pathname-defaults* is bound to.
    (let ((*default-pathname-defaults* (pathname "/")))
      (fresh-main (list "-C" *test-root*
                        "-f" "scripts/load.lisp"
                        "-e" "nil"))))
  ;; file not found
  (signals file-error
    (fresh-main (list "-f" "no-such-file.lisp"
                      "-e" "nil") ;; specify the nonexistent value
                )))

(test verbose
  ;; test the verbosity
  ;; verbosity = 1
  (finishes
    (fresh-main (list "-C" *test-root*
                      "-f" "scripts/verbose.lisp"
                      "-v"
                      "-e" "nil")))
  ;; check the verbosity
  (is (string= "1"
               (with-output-to-string (*error-output*)
                 (fresh-main (list "-C" *test-root*
                                   "-f" "scripts/verbose.lisp"
                                   "-v"
                                   "-p" "cim"
                                   "-e" "(princ (opt :verbosity) *error-output*)")))))

  ;; verbosity = 2
  (finishes
    (fresh-main (list "-C" *test-root*
                      "-f" "scripts/verbose.lisp"
                      "-v" "-v"
                      "-p" "cim"
                      "-e" "(princ (opt :verbosity))")))
  ;; check the verbosity
  (is (string= "2"
               (with-output-to-string (*error-output*)
                 (fresh-main (list "-C" *test-root*
                                   "-f" "scripts/verbose.lisp"
                                   "-v" "-v"
                                   "-p" "cim"
                                   "-e" "(princ (opt :verbosity) *error-output*)")))))

  ;; use the combined args, v = 2
  (finishes
    (fresh-main (list "-C" *test-root*
                      "-f" "scripts/verbose.lisp"
                      "-vv"
                      "-e" "nil")))
  ;; check the verbosity
  (finishes
    (is (string= "2"
                 (with-output-to-string (*error-output*)
                   (fresh-main (list "-C" *test-root*
                                     "-f" "scripts/verbose.lisp"
                                     "-vv"
                                     "-p" "cim"
                                     "-e" "(princ (opt :verbosity) *error-output*)")))))))


(test package
  (is (string= "1COMMON-LISP-USER"
               (with-output-to-string (*standard-output*)
                 (fresh-main (list "-C" *test-root*
                                   "-e" "(princ 1)"
                                   "-e" "(princ (package-name *package*))")))))
  (is (string= "CIM"
               (with-output-to-string (*standard-output*)
                 (fresh-main (list "-C" *test-root*
                                   "-p" "cim"
                                   "-e" "(princ (package-name *package*))"))))))

(test main1
  (is (string=
       "HELLO!"
       (with-output-to-string (*standard-output*)
         (fresh-main (list "-C" *test-root*
                           "-f" "scripts/main1.lisp"
                           "-e" "(main1)"))))))

(test version
  (is (string=
       (with-open-file (stream (asdf:system-relative-pathname
                                :cim
                                #p"VERSION"))
         (when stream
           (let ((seq (make-array (file-length stream)
                                  :element-type 'character
                                  :fill-pointer t)))
             (setf (fill-pointer seq) (read-sequence seq stream))
             seq)))
       (with-output-to-string (*standard-output*)
         (handler-case 
             (fresh-main (list "--version"))
           (warning (c)))))))
