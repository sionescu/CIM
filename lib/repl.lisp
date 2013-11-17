(loop
   (format t "~&~a> " (package-name *package*))
   (force-output)
   (handler-case
       (format t "~s" (eval (read)))
     (condition (error)
       (print error)
       nil))
   (force-output))
