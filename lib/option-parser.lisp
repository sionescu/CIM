
(defun make-parse-options (argv clauses)
  (let ((long-cond ()) (short-case ())
	(helpgiven nil) (help ()) (help-max 0)
	(generated-help "")
	(argv-sym (gensym "argv"))
	(param (gensym "param"))
	(flags (gensym "flags"))
	(flag (gensym "flag")))
    (flet ((short-opt-p (opt)
	     (and (= (length opt) 2) (char= (aref opt 0) #\-) (char/= (aref opt 1) #\-)))
	   (long-opt-p (opt)
	     (and (> (length opt) 2) (string= opt "--" :end1 2))))
      (loop :for (opts lambda-list doc . proc) :in clauses :do
	 (let ((short ())
	       (lprocess `(let* ,(loop :for p :in lambda-list :collect `(,p (pop ,argv-sym)))
			    ,@proc))
	       (sprocess `(let* ,(loop :for p :in lambda-list :collect `(,p (if ,flags
										(prog1
										    (coerce ,flags 'string)
										  (setf ,flags nil))
										(pop ,argv-sym))))
			    ,@proc)))
	   (push (list (format nil "~{~A~^, ~} ~{~A~^ ~}" opts lambda-list) doc) help)
	   (setf help-max (max help-max (length (caar help))))
	   (loop :for opt :in opts :do
	      (cond
		((short-opt-p opt)
		 (push (aref opt 1) short)
		 (when (string= opt "-h") (setf helpgiven t)))
		((long-opt-p opt)
		 (push `((string= ,param ,opt) ,lprocess ) long-cond)
		 (when (string= opt "--help")  (setf helpgiven t)))))
	   (when short (push (list short sprocess) short-case))))
      (setf generated-help
	    (with-output-to-string (str)
	      (loop :for (opt doc) :in (nreverse help) :do
		 (format str "~VA ~A~%" help-max opt doc))))
      `(let ((,argv-sym ,argv)
	     (generated-help ,generated-help))
	 (do ((,param (pop ,argv-sym) (pop ,argv-sym))) (nil)
	   (cond
	     ((string= "--" ,param) (return ,argv-sym))
	     ,@(nreverse long-cond)
	     ,(if helpgiven
		  '(nil nil)
		  `((string= "--help" ,param)
		    (write-string generated-help)
		    (return)))
	     ,(if
	       short-case
	       `((and (>= (length ,param) 2) (char= (aref ,param 0) #\-) (char/= (aref ,param 1) #\-))
		 (let ((,flags (cdr (coerce ,param 'list))))
		   (do ((,flag (pop ,flags) (pop ,flags))) (nil)
		     (case ,flag
		       ,@(nreverse short-case)
		       ,(if helpgiven
			    '(nil nil)
			    `((#\h)
			      (write-string generated-help)
			      (return))))
		     (unless ,flags (return-from nil)))))
	       '(nil nil))
	     (t (push ,param ,argv-sym) (return ,argv-sym))))))))


(defmacro parse-options (argv &rest clauses)
  "Parse `ARGV' follwoing `CLAUSES'. The clauses should be
((options) (parameters)
  \"docstring \"
   body)
where
`OPTIONS' are strings which start with \"-\" followed by single char (shot option) or \"--\" followed by string (long option).
`PARAMETERS' are symbols which will be bound given arguments.
`DOCSTRING' is string which explain the option. This is used for `--help'.
`BODY' is forms doing with `PARAMETERS'.
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
\"\"
 (write-string generated-help)
 (return)).
You can override \"-h\" and \"--help\" to controll help printing.
 "
  (make-parse-options argv clauses))

