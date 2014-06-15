(in-package :cim.test)
(in-suite :cim)

;; TODO: -c, -d, -i, -l, -L, -r, -q, -Q, --no-rl, --no-right, --no-color, --help

(test ccl-ql-dep-error
  (block nil
    (handler-bind ((file-error (lambda (c)
                                 (is (string= "quicklisp"
                                              (pathname-name
                                               (file-error-pathname c))))
                                 (return))))
      (fresh-main `("--no-init"
                    "-C" ,(cim_home)
                    "--"
                    "scripts/ql_cmd_deps"
                    "--path" "quicklisp/")))))
