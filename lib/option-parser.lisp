(defmacro parse-options (argv &rest clauses)
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
