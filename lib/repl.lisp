;; ECL ignores the first line, so dose not write here any code.

(defun get-current-package-name ()
  (or (car (package-nicknames *package*))
      (package-name *package*)))

(defmacro make-wrapper (name before after)
  (let ((str (gensym "strings")))
    `(defmacro ,name (&rest ,str)
       `(concatenate 'string ,,before ,@,str ,,after))))

(defmacro make-wrappers ((name before after) &rest clauses)
  `(progn
     ,@(cons `(make-wrapper ,name ,before ,after)
	     (when (not (null clauses))
	       (loop for (n b a) in clauses
		  collect (list 'make-wrapper n b a))))))

(make-wrappers
 (red     "[31m" "[39m")
 (green   "[32m" "[39m")
 (yellow  "[33m" "[39m")
 ; blue is not used
 (magenta "[35m" "[39m")
 (cyan    "[36m" "[39m")
 (bright  "[1m"  "[0m")
 (str     ""       ""))

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
		  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun filter-escapes (string)
  (let ((list (coerce string 'list)))
    (do ((char (pop list) (pop list))
	 (prev #\Space char)
	 (in-escape-p) (result))
	(nil)
      (cond
	((and (char= prev #\) (char= char #\[))
	 (pop result)
	 (setf in-escape-p t))
	(t (if in-escape-p
	       (when (member char '(#\m #\H #\D #\C #\N #\s #\u))
		 (setf in-escape-p nil))
	       (push char result))))
      (if (null list)
	  (return (coerce (nreverse result) 'string))))))

(defun handle-simple-condition (err)
  (format *error-output* " *~A*~%~A~%"
	  (red (bright (format nil "~A" (class-name (class-of err)))))
	  (red (bright (apply (function format) nil
			      (simple-condition-format-control err)
			      (simple-condition-format-arguments err)))))
  (force-output *error-output*))

(defun handle-condition (err)
  (format *error-output*
	  (str " *~A*~%~A~%")
	  (red (bright (format nil "~A" (class-name (class-of err)))))
	  (red (bright (format nil "~A" err))))
  (if (string= (class-name (class-of err)) "INTERACTIVE-INTERRUPT")
      (format *error-output* "~&~A~%" (red "If you want to quit, type 'exit', 'quit', '(exit)', '(quit)', or Ctrl-D")))
  (force-output *error-output*))

(defmacro handling-errors (&body body)
  `(handler-case
       (progn ,@body)
     (SIMPLE-CONDITION (e)
       (handle-simple-condition e))
     (CONDITION (e)
       (handle-condition e))))

(defvar *history* 0)
(defvar *prompt-before* "")
(defvar *left-prompt*
  '(format nil (str (cyan "~A[" (magenta "~D")) (cyan  "]> "))
    (get-current-package-name) *history*))
(defvar *continue-prompt*
  '(format nil (cyan "~A> ")
    (coerce (make-array
	     (- (length (filter-escapes *prompt-before*)) 2)
	     :initial-element #\-) 'string)))
(defvar *right-prompt*
  '(format nil "~A" *default-pathname-defaults*))

(defun print-prompt (stream &optional continuep)
  (let* ((left (eval (if continuep *continue-prompt* *left-prompt*)))
	 (right (eval *right-prompt*))
	 (col 170 #+nil(parse-integer (my-getenv "COLUMNS"))))
    (setf *prompt-before* left)
    (write-string left stream)
    (format stream "[~DC"
	    (if continuep
		(- col (+ (length (filter-escapes left))
				(length (filter-escapes right))))	
		(- col (+ (length (filter-escapes left))
			  (length (filter-escapes right))))))
    (write-string right stream)
    (format stream "[~DD" (- col (length (filter-escapes left))))
    (force-output stream)))

(defun repl ()
  (do ((+eof+ (gensym))
       (+eol+ (gensym))
       (read-done-p nil nil)
       (-) (--) (---) (+) (++) (+++)
       (/) (//) (///) (*) (**) (***))
      (nil)
    (block iter
      (incf *history*)
      (print-prompt *query-io*)
      ;; read part
      (handling-errors
       ;; repeat until sexp complete
       (do ((input) (form))
	   (read-done-p)
	 (setf input (read-line *standard-input* nil +eof+))
	 (when (equal input "")
	   (return-from iter))
	 (when (eq input +eof+) (return-from repl))
	 (setf form (concatenate 'string form " " input))
	 (handler-case
	     ;; repeat over the input sexps
	     (do* ((in (make-string-input-stream form))
		   (sexp (read in nil +eol+) (read in nil +eol+))
		   (count 0) (sexps))
		  ((eql sexp +eol+)
		   (setf read-done-p t)
		   (setf -- (if (= count 1) (car sexps) (cons 'values (nreverse sexps)))))
	       (push sexp sexps)
	       (incf count))
	   (END-OF-FILE ()
	     (print-prompt *error-output* t))))
       (setf +++ ++   ++ +   + - - --)
       ;; eval and print part
       (when (member - '((quit) quit (exit) exit (bye)) :test (function equal))
	 (return-from repl))
       (format *error-output* "~A"
	       (yellow
		(with-output-to-string (s)
		  (let ((*standard-output* s))
		    (setf /// //   // /   / (multiple-value-list (eval -)))))))
       (setf *** **   ** *   * (first /))
       (force-output)
       (format *error-output* (green "~{~#[; No value~:;;=> ~@{~S~^~&;   ~}~]~:}~%") / )
       (force-output *error-output*)))))

(repl)
#-(or sbcl allegro) (cl-user::quit)
#+sbcl (sb-ext::exit)
#+allegro (cl-user::exit)
