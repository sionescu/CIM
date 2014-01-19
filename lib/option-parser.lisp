(defmacro parse-options (argv &rest clauses)

  (let ((long-cond ())
	(short-case ())
	(help ())
	(help-max 0)
	(argv-sym (gensym "argv"))
	(param (gensym "opt"))
	(flags (gensym "flags"))
	(flag (gensym "flag"))
	(proc-list (gensym "list"))
	(proc-sym (gensym "var")))
    (flet ((short-opt-p (opt)
	     (and (= (length opt) 2) (char= (aref opt 0) #\-) (char/= (aref opt 1) #\-)))
	   (long-opt-p (opt)
	     (and (> (length opt) 2) (string= opt "--" :end1 2))))
      (loop :for (opts doc (lambda-list . proc)) :in clauses :do
	 (let ((short ())
	       (process ``(let* ,(loop :for p :in (quote ,lambda-list) :collect `(,p ,(pop ,argv-sym)))
			   ,@(quote ,proc))))
	   (push (list (format nil "~{~A~^, ~} ~{~A~^ ~}" opts lambda-list) doc) help)
	   (setf help-max (max help-max (length (caar help))))
	   (loop :for opt :in opts :do
	      (cond
		((short-opt-p opt) (push (aref opt 1) short))
		((long-opt-p opt) (push `((string= ,param ,opt) (push ,process ,proc-list)) long-cond))))
	   (when short (push `(,short (push ,process ,proc-list)) short-case))))
      (symbol-macrolet ((print-help
			 `(progn
			    ,@(loop :for (opt doc) :in help :collect
				`(format t "~VA ~A~%" ,help-max ,opt ,doc))
			    (return))))
	`(let ((,argv-sym ,argv)
	       (,proc-list ()))
	   (do ((,param (pop ,argv-sym) (pop ,argv-sym))) (nil)
	     (cond
	       ((string= "--" ,param) (return))
	       ((string= "--help" ,param) ,print-help)
	       ,@(nreverse long-cond)
	       ,(if short-case
		    `((and (>= (length ,param) 2) (char= (aref ,param 0) #\-) (char/= (aref ,param 1) #\-))
		      (let ((,flags (cdr (coerce ,param 'list))))
			(do ((,flag (pop ,flags) (pop ,flags))) (nil)
			  (case ,flag
			    ((#\h)  ,print-help)
			    ,@(nreverse short-case))
			  (unless ,flags (return-from nil)))))
		    '(nil nil))
	       (t (push ,param ,argv-sym) (loop :for ,proc-sym :in (nreverse ,proc-list) :do (eval ,proc-sym)) (return))
	       )))))))
