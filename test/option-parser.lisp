
(in-package :cim.test)
(in-suite :cim)

(test short-opt-p
  (is (short-opt-p "-h"))
  (is (not (short-opt-p "--help")))
  (is (not (short-opt-p "-avl")))
  (is (not (short-opt-p "--")))
  (is (not (short-opt-p "-")))
  (is (not (short-opt-p "a-"))))

(test long-opt-p
  (is (long-opt-p "--help"))
  (is (not (long-opt-p "-a")))
  (is (not (long-opt-p "-avl")))
  (is (not (long-opt-p "--")))
  (is (not (long-opt-p "-")))
  (is (not (long-opt-p "a-"))))

(test combined-opt
  (is (combined-opt-p "-avl"))
  (is (not (combined-opt-p "-a")))
  (is (not (combined-opt-p "--help")))
  (is (not (combined-opt-p "--")))
  (is (not (combined-opt-p "-")))
  (is (not (combined-opt-p "a-"))))

(test make-option
  (is (string= (make-option "c") "-c"))
  (is (string= (make-option "c" t) "--c"))
  (signals error (make-option "cim"))
  (is (string= (make-option "cim" t) "--cim"))
  (is (string= (make-option #\c) "-c"))
  (is (string= (make-option #\c t) "--c")))

(test explode-combined-opts
  (is (every #'string= (explode-combined-opts "c") '("-c")))
  (is (every #'string= (explode-combined-opts "cim") '("-c" "-i" "-m")))
  (is (every #'string= (explode-combined-opts "-c") '("-c")))
  (is (every #'string= (explode-combined-opts "-cim") '("-c" "-i" "-m"))))

(test long-opt-value-p
  (multiple-value-bind (value value-p) (long-opt-value-p "--help=XXX")
    (is-true value)
    (is (string= "XXX" value))
    (is-true value-p))
  (multiple-value-bind (value value-p) (long-opt-value-p "--help")
    (is-false value)
    (is-false value-p))
  (multiple-value-bind (value value-p) (long-opt-value-p "--help=")
    (is-false value)
    (is-true value-p))
  (is-false (long-opt-value-p "-a"))
  (is-false (long-opt-value-p "-avl"))
  (is-false (long-opt-value-p "--"))
  (is-false (long-opt-value-p "-"))
  (is-false (long-opt-value-p "a-")))

(test parse-clause
  ;; basic structure
  (finishes
    (parse-clause '(("-h" "--help") () (return))))

  ;; docstring on
  (finishes
    (parse-clause '(("-h" "--help") () "giving a help" (return))))

  ;; no combined options in the option definition
  (signals error
    (parse-clause '(("-cim") () (return))))



  (let ((help (parse-clause '(("-h" "--help") () (return)))))
    (is (equal (clause-doc help) ""))
    (is (null (clause-lambda-list help)))
    (is (member "--help" (clause-long-options help) :test #'string=))
    (is (member "-h" (clause-short-options help) :test #'string=))
    (is (null (clause-aux-options help)))

    (let ((condition (clause-flag-match-condition 'flag help)))
      ;; a form that checks if the variable `flag' matches the definition in help
      (is (eval `(let ((flag "-h")) ,condition)))
      (is (not (eval `(let ((flag "-a")) ,condition)))))))

(defparameter *sample*
  '((("-a") ()
     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.")
    (("-b" "--bb") ()
     "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.")

    ;; handling of arguments
    (("-c" "--cc") (file)
     "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.")

    ;; see the effect of evaluation
    (("-d" "--dd" "--double-down") (file file2)
     "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd."
     (error "hi!"))
    (("-e") (number)
     "set a number to x."
     (setf x (parse-integer number)))))

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

(defun print-and-eval (x)
  (assert (or (symbolp x) (listp x)))
  (print x)
  (eval x))

(defun split-args (str)
  `(list ,@(split " " str)))

(test make-parse-options
  (finishes
    (print
     (make-parse-options 'x *sample*)))

  (dolist (argv (mapcar #'split-args
                        (list "-a"
                              "-b"
                              "-b -bb"
                              "-a -bb"
                              "-a -c c-value -bb")))
    (finishes
      (eval (make-parse-options argv *sample*))))

  (is (equal (eval (split-args "-c c-value"))
             (eval (make-parse-options (split-args "-a -- -c c-value") *sample*))))

  ;; ensure -d clauses are evaluated
  (signals error
    (eval
     (make-parse-options (split-args "-d 1 2") *sample*)))
  (signals error
    (eval
     (make-parse-options (split-args "--double-down 1 2") *sample*)))
  (signals error
    (eval
     (make-parse-options (split-args "--dd 1 2") *sample*)))

  (is (= 1
         (eval
          `(let ((x nil))
             ,(make-parse-options (split-args "-e 1") *sample*)
             x))))


  ;; ensure "--" is removed
  (is (= 2
         (eval
          `(parse-integer
            (first
             ,(make-parse-options (split-args "-a --bb -- 2 -a 3") *sample*)))))))



