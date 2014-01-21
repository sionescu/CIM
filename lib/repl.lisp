;; ECL ignores the first line, so dose not write any code here.
(in-package :cim)
(defun get-current-package-name ()
  (or (car (package-nicknames *package*))
      (package-name *package*)))

(defmacro make-color (name before after)
  (let ((str (gensym "strings")))
    `(defmacro ,name (&rest ,str)
       `(concatenate 'string
		     (if (opt :no-color) "" ,,before) ,@,str (if (opt :no-color) "" ,,after)))))

(defmacro make-colors ((name before after) &rest clauses)
  `(progn
     ,@(cons `(make-color ,name ,before ,after)
	     (when (not (null clauses))
	       (loop for (n b a) in clauses
		  collect (list 'make-color n b a))))))

(make-colors
 (red     "[31m" "[39m")
 (green   "[32m" "[39m")
 (yellow  "[33m" "[39m")
 (blue    "[34m" "[39m")
 (magenta "[35m" "[39m")
 (cyan    "[36m" "[39m")
 (bright  "[1m"  "[0m")
 (str     ""       ""))


(defun filter-escapes (string)
  (if (string= string "")
      ""
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
	      (return (coerce (nreverse result) 'string)))))))

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
  '(let* ((dir (namestring *default-pathname-defaults*))
	  (impl (getenv "LISP_IMPL")))
    (let* ((it (string<  (getenv "HOME") dir))
	   (subdirp (= it (length (getenv "HOME")))))
      (when subdirp (setf dir (concatenate 'string "~" (subseq dir it (1- (length dir)))))))
    (format nil "~a ~a" (blue "(" impl ")")  (cyan dir))))

(defun print-prompt (stream &optional continuep)
  (let* ((left (eval (if continuep *continue-prompt* *left-prompt*)))
	 (col (or (parse-integer (or (getenv "COLUMNS") "") :junk-allowed t) 75)))
    (unless (opt :no-right)
      (let ((right (eval *right-prompt*)))
	(format stream "[~DC" (- col (length (filter-escapes right))))
	(write-string right stream)
	(format stream "[~DD" col)))
    (setf *prompt-before* left)
    (write-string left stream)
    (force-output stream)))

(defun repl ()
    (do ((+eof+ (gensym))
	 (+eol+ (gensym))
	 (read-done-p nil nil)
	 (-) (--) (+) (++) (+++)
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
