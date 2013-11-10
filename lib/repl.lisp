(loop
   (format t "~&~a> " (package-name *package*))
   (force-output)
   (format t "~s" (eval (read)))
   (force-output))
