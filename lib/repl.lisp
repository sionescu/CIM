(in-package :cl-user)
(defpackage cim.repl
  (:use :CL :cim)
  (:export :*history*
	   :print-prompt
	   :with-handle-conditions
	   :strf))
(in-package :cim.repl)

(defun get-current-package-name ()
  (or (car (package-nicknames *package*))
      (package-name *package*)))

(defmacro make-color (name before after)
  (let ((str (gensym "strings")))
    `(defun ,name (&rest ,str)
       (apply #'concatenate
              (append '(string)
                      (list ,(if (opt :no-color) "" before)) ,str (list ,(if (opt :no-color) "" after)))))))

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

(defmacro strf (sym &rest strings)
  (let ((strs (gensym "strs")))
    `(let ((,strs ,(cons 'list (cons sym strings))))
       (setf ,sym
	     (apply #'str ,strs)))))

(defun filter-escapes (string)
  (if (string= string "")
      ""
      (let ((list (coerce string 'list)))
	(do ((char (pop list) (pop list))
	     (in-escape-p) (result))
	    (nil)
	  (cond
	    ((and (char= char #\) (char= (car list) #\[))
	     (pop list)
	     (setf in-escape-p t))
	    (t (if in-escape-p
		   (when (member char '(#\m #\H #\D #\C #\N #\s #\u))
		     (setf in-escape-p nil))
		   (push char result))))
	  (if (null list)
	      (return (coerce (nreverse result) 'string)))))))

(defun handle-interrupt (condition)
  (declare (ignore condition))
  (format *error-output* "~%~A" (red "Interrupt from keybord"))
  (format *error-output* "~&~A~%" (red "If you want to quit, type 'exit', 'quit', '(exit)', '(quit)', or Ctrl-D"))
  (force-output *error-output*))

(defun handle-condition (condition)
  (format *error-output*
	  (str "~& *~A*~%~A~%")
	  (red (bright (format nil "~A" (class-name (class-of condition)))))
	  (red (bright (format nil "~A" condition))))
  (force-output *error-output*))

(defmacro with-handle-conditions (&body body)
  `(handler-case
       #-ccl(progn
              ,@body)
       #+ccl (let ((ccl:*break-hook* (lambda (condition hook)
                                       (declare (ignore hook))
                                       (error condition))))
               ,@body)
       #+(or allegro ccl clisp ecl sbcl)
       (#.*interrupt-condition* (c)
         (handle-interrupt c))
       (condition (c)
         (handle-condition c))))

(defvar *history* 0)
(defvar *prompt-before* "")
(defvar *left-prompt*
  '(format nil (str (cyan "~A[" (magenta "~D")) (cyan  "]> "))
    (get-current-package-name) *history*))
(defvar *continue-prompt*
  '(format nil (cyan "~A> ")
    (make-string
     (- (length (filter-escapes *prompt-before*)) 2)
     :initial-element #\-)))
(defvar *right-prompt*
  '(let* ((dir (namestring *default-pathname-defaults*))
	  (impl (getenv "LISP_IMPL")))
    (when (char/= (aref dir (1- (length dir))) #\/)
      (strf dir "/"))
    (let* ((len (string<  (getenv "HOME") dir))
	   (subdirp (and len (= len (length (getenv "HOME"))))))
      (when subdirp (setf dir (str "~" (subseq dir len (1- (length dir)))))))
    (format nil "~a ~a" (blue "(" impl ")")  (cyan dir))))

(defun print-prompt (stream &key continuep)
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

(in-package :cl-user)
(defun repl ()
  (let ((+eof+ (gensym "eof"))
	(+bol+ (gensym "bol"))
	(+sexps+)
	;; previous multiple forms
	(-) (--) (---)
	;; previous foms
	(+) (++) (+++)
	;; previous multiple values
	(/) (//) (///)
	;; previous values
	(*) (**) (***))
    (loop :for continue := nil :do
       (block iter
	 (incf cim.repl:*history*)
	 (cim.repl:print-prompt *query-io*)
	 ;; read part
	 (cim.repl:with-handle-conditions
	     ;; repeat until sexps complete. 
	     (loop :named reader :with form
                :for input := (read-line *standard-input* nil +eof+) :do
                (cond
                  ;; if input is null, go next prompt
                  ((and (equal input "")  (not continue)) (return-from iter))
                  ;; if ^D is signaled, exit
                  ((eq input +eof+) (return-from repl))
                  (t
                   (cim.repl:strf form (format nil "~%") input)
                   (handler-case
                       ;; repeat over the input sexps
                       (loop :with in := (make-string-input-stream form)
                          :for sexp := (read in nil +bol+)
                          :until (eq sexp +bol+) :collect sexp :into sexps
                          :finally (return-from reader (setf +sexps+ sexps)))
                     ;; input was incomplete
                     (END-OF-FILE ()
                       (setf continue t)
                       (cim.repl:print-prompt *query-io* :continuep t))))))
           ;; eval part
           (when (member + '((quit) quit (exit) exit (bye)) :test (function equal))
             (return-from repl))
           (format *standard-output* "~A"
                   (cim.repl::yellow
                    (with-output-to-string (*standard-output*)
                      (setf /// //  // /  / (loop :for sexp :in +sexps+ :append
                                               (multiple-value-list (eval sexp))))
                      (setf *** **  ** *  * (first /))
                      (setf --- --  -- -  - +sexps+)
                      (setf +++ ++  ++ +  + (first -)))))
           (force-output)
           ;; print part
           (format *error-output* (cim.repl::green "~{~#[; No value~:;;=> ~@{~S~^~&;   ~}~]~:}~%") / )
	   (force-output *error-output*))))))
(repl)
