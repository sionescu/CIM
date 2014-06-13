(in-package :cim.impl)

(defun short-opt-p (opt-string)
  "input: \"-c\" result: t"
  (and (= (length opt-string) 2)
       (char= (aref opt-string 0) #\-)
       (char/= (aref opt-string 1) #\-)))

(defun long-opt-p (opt-string)
  "input: \"--cim\" result: t"
  (and (> (length opt-string) 2)
       (string= opt-string "--" :end1 2)))

(defun combined-opt-p (opt-string)
  "input: \"-cim\" result: t"
  (and (> (length opt-string) 2)
       (char= (aref opt-string 0) #\-)
       (char/= (aref opt-string 1) #\-)))

(defun make-option (string-or-char &optional long)
  "returns a new option string such as \"-c\" from the input #\c or \"c\"."
  (symbol-macrolet ((sc string-or-char))
    (if long
        (concatenate 'string "--" (string sc))
        (typecase sc
          (string
           (assert (= (length sc) 1))
           (concatenate 'string "-" sc))
          (character
           (let ((s (make-string 2)))
             (setf (aref s 0) #\-)
             (setf (aref s 1) sc)
             s))))))

(defun explode-combined-opts (opt-string)
  "returns a list of short options like '(\"-c\" \"-i\" \"-m\") from \"-cim\" or \"cim\""
  (map 'list #'make-option
       (if (char= #\- (aref opt-string 0))
           (subseq opt-string 1)
           opt-string)))

(defstruct (clause (:constructor clause))
  (long-options nil :type list)
  (short-options nil :type list)
  (aux-options nil :type list)
  (lambda-list nil :type list)
  (doc "" :type string)
  (body nil :type list))

(defun parse-clause (clause)
  (destructuring-bind (options lambda-list . body) clause
    (dolist (designator '(&rest &optional &key &allow-other-keys
                          &aux &body &whole &environment))
      (assert (not (member designator lambda-list)) (lambda-list)
              "option lambda-list should not contain ~a." designator))
    (assert (notany #'combined-opt-p options) (options)
            "option specification in the definition do not accept combined short options. ~% ~a." options)
    (clause :long-options (remove-if-not #'long-opt-p options)
            :short-options (reduce (lambda (o prev)
                                     (cond ((short-opt-p o) (cons o prev))
                                           ((combined-opt-p o)
                                            ;; here, prev is always a fresh value, so it is safe to use nconc
                                            (nconc (explode-combined-opts o) prev))
                                           (t prev)))
                                   options :from-end t :initial-value nil)
            :aux-options (remove-if (lambda (o) (or (long-opt-p o)
                                                    (short-opt-p o)
                                                    (combined-opt-p o)))
                                    options)
            :lambda-list lambda-list
            :doc (if (stringp (car body)) (car body) "")
            :body (if (stringp (car body)) (cdr body) body))))

(defun clause-options (clause)
  (append (clause-long-options clause)
          (clause-short-options clause)
          (clause-aux-options clause)))

(defun clause-help-title (clause)
  (format nil "~{~A~^, ~} ~{~A~^ ~}"
          (clause-options clause)
          (clause-lambda-list clause)))

;; TODO: align the table correctly
;; consider the given help message

;; dispatcher compilation

(defun clause-flag-match-condition (flag-var clause)
  (assert (symbolp flag-var) nil)
  `(or ,@(mapcar (lambda (opt) `(string= ,opt ,flag-var)) (clause-options clause))))

(defun make-dispatcher-function (clauses)
  (let ((head (gensym "HEAD"))
        (rest (gensym "REST"))
        (crest (gensym "CREST")))
  `(lambda (,head &rest ,rest)
     (block nil
       (cond
         ,@(mapcar
            (lambda (c)
              `(,(clause-flag-match-condition head c)
                 ,(if (clause-lambda-list c)
                      `(destructuring-bind (,@(clause-lambda-list c) . ,crest) ,rest
                         ,@(clause-body c)
                         (values ,crest t))
                      `(progn
                         ,@(clause-body c)
                         (values ,rest t)))))
            clauses)
         ((string= ,head "--") (values ,rest nil)))))))

;; help message aggregation

(defun generate-help-message (clauses)
  (let* ((titles (mapcar #'clause-help-title clauses))
         (max (reduce #'max titles :key #'length))
         (docs (mapcar #'clause-doc clauses)))
    (with-output-to-string (s)
      (loop
         for title in titles
         for doc in docs
         do (format s "~VA ~A~%" max title doc)))))

;; runtime dispatch

(defun %parse-options-rec (argv dispatcher)
  (multiple-value-bind (result dispatched-p) (apply dispatcher argv)
    (if (and result dispatched-p)
        (%parse-options-rec result dispatcher)
        result)))

(defun make-parse-options (argv clauses)
  (let ((parsed-clauses (mapcar #'parse-clause clauses)))
    `(%parse-options-rec
      ,argv
      ,(make-dispatcher-function
        (append parsed-clauses
                (list
                 (parse-clause
                  `(("-h" "--help") ()
                    ,(generate-help-message parsed-clauses) ;; help of help
                    (write-string ,(generate-help-message parsed-clauses))
                    (return)))))))))

(defmacro parse-options (argv &rest clauses)
  "Parse `ARGV' follwoing `CLAUSES'. The clauses should be
 ((options) (parameters)
   \"docstring \"
    body)
where
`OPTIONS'    are either strings which start with \"-\" followed by a single
             character (short option) or \"--\" followed by string (long option).
`PARAMETERS' are symbols which will be bound given arguments.
`DOCSTRING'  is string which explain the option. This is used for `--help'.
`BODY'       is evaluated as an implicit progn under the environment where
             `PARAMETERS' are bound to the values given in the command arguments.

If \"--\" is found, immidiately exit from this macro.
The return value is the rest of `ARGV'.
You can access to the variable `GENERATED-HELP' which contains help string.
Example:
 (defvar foo)
 (parse-options *argv*
  ((\"--foo\" \"-f\") ()
   \"set foo\"
   (setf foo t))
  ((\"--bar\") (arg)
   \"do something with `ARG'\"
   (do-something-with arg)))

The predefined option is
 ((\"-h\" \"--help\") ()
  (return)).
You can override \"-h\" and \"--help\" to controll help printing.
 "
  (make-parse-options argv clauses))


