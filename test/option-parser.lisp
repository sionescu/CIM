
(in-package :cim.test)
(def-suite :cim)
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
             x)))))



