

(defun get-current-package-name ()
  (or (car (package-nicknames *package*))
      (package-name *package*)))

(defmacro cyan (&rest strings)
  `(concatenate 'string "[36m" ,@strings "[39m"))
(defmacro magenta (&rest strings)
  `(concatenate 'string "[35m" ,@strings "[39m"))
(defmacro red (&rest strings)
  `(concatenate 'string "[31m" ,@strings "[39m"))
(defmacro green (&rest strings)
  `(concatenate 'string "[32m" ,@strings "[39m"))
(defmacro yellow (&rest strings)
  `(concatenate 'string "[33m" ,@strings "[39m"))
(defmacro bright (&rest strings)
  `(concatenate 'string "[1m" ,@strings "[0m"))
(defmacro str (&rest strings)
  `(concatenate 'string ,@strings))

(defun handle-simple-condition (err)
  (format *error-output* "~% *~A*~%~A"
	  (red (bright (format nil "~A" (class-name (class-of err)))))
	  (red (bright (apply (function format) nil
			      (simple-condition-format-control err)
			      (simple-condition-format-arguments err)))))
  (force-output *error-output*))

(defun handle-condition (err)
  (format *error-output*
	  (str "~% *~A*~%~A~%")
	  (red (bright (format nil "~A" (class-name (class-of err)))))
	  (red (bright (format nil "~A" err))))
  (if (string= (class-name (class-of err)) "INTERACTIVE-INTERRUPT")
      (format *error-output* "~A~%" (red "If you want to quit, type 'exit', 'quit', '(exit)', '(quit)', or Ctrl-D")))
  (force-output *error-output*))

(defmacro handling-errors (&body body)
  `(handler-case
       (progn ,@body)
     (SIMPLE-CONDITION (e)
       (handle-simple-condition e))
     (CONDITION (e)
       (handle-condition e))))

(defun repl ()
  (do ((+eof+ (gensym))
       (+eol+ (gensym))
       (input "")
       (form "")
       (hist 1 (1+ hist)))
      (nil)
    (block iter
	(format *query-io* (str "~&" (cyan "~A[" (magenta "~D")) (cyan  "]> ")) (get-current-package-name) hist)
      (force-output *query-io*)
      (handling-errors
       (loop
	  (setf input (read-line *standard-input* nil +eof+))
	  (when (equal input "") (decf hist) (return-from iter))
	  (when (eq input +eof+) (return-from repl))
	  (setf form (concatenate 'string form " " input))
	  (handler-case
	      (progn (setf - (read (make-string-input-stream form)))
		     (setf form "")
		     (return))
	    (END-OF-FILE ()
	      (format t (cyan "~a> " )
		      (coerce (make-array
			       (length (format nil "~a[~d]" (get-current-package-name) hist))
			       :initial-element #\-) 'string))
	      (force-output))))
       (setf +++ ++   ++ +   + -)
       (when (member - '((quit) quit (exit) exit (bye)) :test (function equal))
	 (return-from repl))
       (format *standard-output* "~A"
	       (yellow
		(with-output-to-string (s)
		  (let ((*standard-output* s))
		    (setf /// //   // /   / (multiple-value-list (eval -)))))))
       (setf *** **   ** *   * (first /))
       (force-output)
       (format *query-io* (green "~&~{~#[; No value~%~:;;=> ~@{~S~%~^;   ~}~]~:}") / )
       (force-output *query-io*)))))
(repl)
#-(or sbcl allegro) (cl-user::quit)
#+sbcl (sb-ext::exit)
#+allegro (cl-user::exit)
