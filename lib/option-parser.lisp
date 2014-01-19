(defmacro parse-options (argv &rest clauses)
  (let ((long-cond ()) (short-case ())
	(helpgiven nil) (help ()) (help-max 0)
	(argv-sym (gensym "argv"))
	(param (gensym "opt"))
	(flags (gensym "flags"))
	(flag (gensym "flag")))
    (flet ((short-opt-p (opt)
	     (and (= (length opt) 2) (char= (aref opt 0) #\-) (char/= (aref opt 1) #\-)))
	   (long-opt-p (opt)
	     (and (> (length opt) 2) (string= opt "--" :end1 2))))
      (loop :for (opts doc (lambda-list . proc)) :in clauses :do
	 (let ((short ())
	       (process `(let* ,(loop :for p :in lambda-list :collect `(,p (pop ,argv-sym)))
			    ,@proc)))
	   (push (list (format nil "~{~A~^, ~} ~{~A~^ ~}" opts lambda-list) doc) help)
	   (setf help-max (max help-max (length (caar help))))
	   (loop :for opt :in opts :do
	      (cond
		((short-opt-p opt)
		 (push (aref opt 1) short)
		 (when (string= opt "-h") (setf helpgiven t)))
		((long-opt-p opt)
		 (push `((string= ,param ,opt) ,process ) long-cond)
		 (when (string= opt "--help")  (setf helpgiven t)))))
	   (when short (push (list short process) short-case))))
      (symbol-macrolet ((print-help
			 `(progn
			    ,@(loop :for (opt doc) :in help :collect
				 `(format t "~VA ~A~%" ,help-max ,opt ,doc))
			    (return))))
	`(let ((,argv-sym ,argv))
	   (do ((,param (pop ,argv-sym) (pop ,argv-sym))) (nil)
	     (cond
	       ((string= "--" ,param) (return ,argv-sym))
	       ,@(nreverse long-cond)
	       ,(if helpgiven
		    '(nil nil)
		    `((string= "--help" ,param) ,print-help))
	       ,(if
		 short-case
		 `((and (>= (length ,param) 2) (char= (aref ,param 0) #\-) (char/= (aref ,param 1) #\-))
		   (let ((,flags (cdr (coerce ,param 'list))))
		     (do ((,flag (pop ,flags) (pop ,flags))) (nil)
		       (case ,flag
			 ,@(nreverse short-case)
			 ,(if helpgiven
			      '(nil nil)
			      `((#\h)  ,print-help)))
		       (unless ,flags (return-from nil)))))
		 '(nil nil))
	       (t (push ,param ,argv-sym) (return ,argv-sym)))))))))
