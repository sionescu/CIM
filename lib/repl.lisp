

(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err) 
       (format *error-output* "~&[31m[1m*~A*[0m[39m~%"
	         (class-name (class-of err)))
       (format *error-output* "[31m[1m")
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "[39m[0m")
       (force-output *error-output*))
     (condition (err) 
       (format *error-output* "~&[31m  [1m~A[0m:[39m~%[31m~A[39m~%"
                 (class-name (class-of err)) err)
       (force-output *error-output*))))

(defun repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format *query-io* "~&[36m~A[[35m~D[36m]>[39m " (package-name *package*) hist)
    (force-output *query-io*)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit) (exit) (bye)) :test (function equal)))
       (return-from repl))
     (format *standard-output* "[33m")
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (format *standard-output* "[39m")
     (setf *** **   ** *   * (first /))
     (force-output)
     (format *query-io* "~&[32m~{~#[; No value~%~:;;=> ~@{~S~%~^;   ~}~]~:}[39m" / )
     (force-output *query-io*))))
(repl)
#-sbcl (cl-user::quit)
#+sbcl (sb-ext::exit)
