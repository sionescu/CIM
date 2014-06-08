
(in-package :cim.test)
(def-suite :cim)
(in-suite :cim)

(test short-opt-p
  (is (short-opt-p "-h"))
  (is (not (short-opt-p "--help")))
  (is (short-opt-p "-a"))
  (is (not (short-opt-p "-avl")))
  (is (not (short-opt-p "-"))))

(test long-opt-p
  (is (long-opt-p "--help"))
  (is (not (long-opt-p "-a")))
  (is (not (long-opt-p "-avl"))) ;; TODO.
  (is (not (long-opt-p "--"))))

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

(defvar *sample*
  '((("-a") ()
     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.")
    (("-b" "--bb") ()
     "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.")
    (("-c" "--cc") (file)
     "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.")
    (("-d" "--dd" "--double-down") (file file2)
     "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd."
     (signal "hi!"))))

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

(test make-parse-options
  (finishes
    (make-parse-options 'x *sample*))

  (dolist (argv (mapcar (lambda (str)
                          `(list ,@(split " " str)))
                        (list "-a"
                              "-b"
                              "-b -bb"
                              "-a -bb"
                              "-a -c c-value -bb")))
    (finishes
      (let ((compiled (make-parse-options argv *sample*)))
        (print compiled)
        (eval compiled)))))



