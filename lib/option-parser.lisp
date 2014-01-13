(defmacro parse-options (argv &rest clauses)

  (let ((long-cond ())
	(short-case ())
	(argv-sym (gensym "argv"))
	(param (gensym "opt"))
	(flags (gensym "flags"))
	(flag (gensym "flag")))
    (flet ((short-opt-p (opt)
	     (and (= (length opt) 2) (char= (aref opt 0) #\-) (char/= (aref opt 1) #\-)))
	   (long-opt-p (opt)
	     (and (> (length opt) 2) (string= opt "--" :end1 2))))
      (loop for (opts doc process) in clauses do
	   (let ((short ()))
	     (loop for opt in opts do
		  (cond
		    ((short-opt-p opt) (push (aref opt 1) short))
		    ((long-opt-p opt) (push `((string= ,param ,opt) ,process) long-cond))))
	     (when short (push `(,short ,process) short-case))))
      `(let ((,argv-sym ,argv))
	 (do ((,param (pop ,argv-sym) (pop ,argv-sym))) (nil)
	   (cond
	     ((string= "--" ,param) (return))
	     ,@(nreverse long-cond)
	     ,(if short-case
		    `((and (>= (length ,param) 2) (char= (aref ,param 0) #\-) (char/= (aref ,param 1) #\-))
		      (let ((,flags (cdr (coerce ,param 'list))))
			(do ((,flag (pop ,flags) (pop ,flags))) (nil)
			  (case ,flag
			    ,@(nreverse short-case))
			  (unless (cdr ,flags) (return-from nil)))))
		    '(nil nil))
	     (t (push ,param ,argv-sym) (return))
	     ))))))

