(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err) 
       (format *error-output* "~&~c[31m~c[1m*~A*~c[0m~c[39m~%"
	       #\Esc #\Esc (class-name (class-of err)) #\Esc #\Esc)
       (format *error-output* "~c[31m~c[1m" #\Esc #\Esc)
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~c[39m~c[0m" #\Esc #\Esc)
       (force-output *error-output*))
     (condition (err) 
       (format *error-output* "~&~c[31m  ~c[1m~A~c[0m:~c[39m~%~c[31m~A~c[39m~%"
               #\Esc #\Esc (class-name (class-of err)) #\Esc #\Esc #\Esc err #\Esc)
       (force-output *error-output*))))

(defun repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format *query-io* "~&~c[36m~A[~c[35m~D~c[36m]>~c[39m " #\Esc (package-name *package*) #\Esc hist #\Esc #\Esc)
    (force-output *query-io*)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit) (exit) (bye)) :test (function equal)))
       (return-from repl))
     (format *standard-output* "~c[33m" #\Esc)
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (format *standard-output* "~c[39m" #\Esc)
     (setf *** **   ** *   * (first /))
     (force-output)
     (format *query-io* "~&~c[32m~{~#[; No value~%~:;;=> ~@{~S~%~^;   ~}~]~:}~c[39m"  #\Esc /  #\Esc)
     (force-output *query-io*))))
(repl)
#-sbcl (cl-user::quit)
#+sbcl (sb-ext::exit)
