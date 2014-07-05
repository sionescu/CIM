(in-package :cim.impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; predicates and options

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

(defun long-opt-value-p (opt-string)
  "input: \"--cim\" result: NIL,NIL
input: \"--cim=value\" result: \"value\",T
input: \"--cim=\" result: NIL,T"
  (and (long-opt-p opt-string)
       (let ((pos (position #\= opt-string :test #'char=)))
         (when pos
           (let ((value (subseq opt-string (1+ pos))))
             (if (plusp (length value))
                 (values value t)
                 (values nil t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clauses

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
                                            (nconc (remove-duplicates
                                                    (explode-combined-opts o)
                                                    :test #'string=)
                                                   prev))
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

;; dispatcher compilation

(defun clause-flag-match-condition (flag-var clause)
  (assert (symbolp flag-var) nil)
  `(or ,@(mapcar (lambda (opt) `(string= ,opt ,flag-var)) (clause-options clause))))

(defvar *hooks* nil)
(defun add-hook (fn) (push fn *hooks*))
(defun make-dispatcher-function (clauses)
  (let ((head (gensym "HEAD"))
        (rest (gensym "REST"))
        (crest (gensym "CREST")))
  `(lambda (,head &rest ,rest)
     (block nil
       (cond
         ((combined-opt-p ,head)
          ,(when *cim-option-match-verbose*
                 `(format *trace-output*
                          "~&In make-dispatcher-function: Found a combined option ~a. ~
                             Explode it into ~a and continue.~&"
                          ,head
                          (explode-combined-opts ,head)))
          (values (nconc (explode-combined-opts ,head) ,rest) t))
         ((string= ,head "--")
          ,(when *cim-option-match-verbose*
                 `(format *trace-output*
                          "~&In make-dispatcher-function: Found a option terminater --. ~
                             All options are processed. remaining: ~a~&"
                          ,rest))
          (values ,rest nil))
         ,@(mapcar
            (lambda (c)
              `(,(clause-flag-match-condition head c)
                 ,(if (clause-lambda-list c)
                      `(destructuring-bind (,@(clause-lambda-list c) . ,crest) ,rest
                         ,(when *cim-option-match-verbose*
                                `(format *trace-output*
                                         "~&In make-dispatcher-function: Option ~a : ~:@{~a=~a~^, ~}~&"
                                         ',(clause-options c)
                                         ,@(mapcar (lambda (sym) `(list ',sym ,sym))
                                                   (clause-lambda-list c))))
                         ,@(clause-body c)
                         (values ,crest t))
                      `(progn
                         ,@(clause-body c)
                         (values ,rest t)))))
            clauses)
         (t
          ,(when *cim-option-match-verbose*
                 `(format *trace-output*
                          "~&In make-dispatcher-function: All options processed. remaining: ~a~&"
                          (cons ,head ,rest)))
          (values (cons ,head ,rest) nil)))))))

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
  (if (null argv)
      nil
      (multiple-value-bind (result dispatched-p) (apply dispatcher argv)
        (if dispatched-p
            (%parse-options-rec result dispatcher)
            result))))

(defun make-parse-options (argv clauses)
  (let ((parsed-clauses (mapcar #'parse-clause clauses)))
    `(let ((*hooks* nil))
       (values
        (%parse-options-rec
         ,argv
         ,(make-dispatcher-function
           (append parsed-clauses
                   (list
                    ;; default help format
                    (clause :short-options '("-h")
                            :long-options '("--help")
                            :doc "Print this help"
                            :body `((format nil "~A~2%~A~%~A~%"
                                            "
Usage: cl [switchs-sans-e] [--] [programfile] [arguments]
Usage: cl [switchs] -e form [--] [arguments]
Usage: cl [switchs] -i OLD-EXT [--] [programfile] [files]
Usage: cl [switchs] -i OLD-EXT -e form [--] [files]
"
                                            ,(generate-help-message parsed-clauses)
                                            "
If neither programfile, -e (--eval) or -r (--repl) are specified,
cl reads scripts from the standard input and then evaluate them.")
                                    (setf (opt :help) t)
                                    (exit)))))))
        (nreverse *hooks*)))))

(defmacro parse-options (argv &body clauses)
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

It returns two values, one is the rest of `ARGV' sans `--'.
The secondary value is a list of hooks to be run later.
Each hook is a zero-arg closure of `BODY' which stores the information
of `PARAMETER'. Hooks are stored in the order the given options are specified.
To actually process the arguments, users should call each closure.

You can override \"-h\" and \"--help\" to controll help message."
  (make-parse-options argv clauses))


