(in-package :cim.test)
(in-suite :cim)

;; TODO: -c, -d, -i, -l, -L, -r, -q, -Q, --no-rl, --no-right, --no-color, --help

(test ccl-ql-dep-error
  (finishes
    (fresh-main `("--no-init"
                  "-C" ,(cim_home "shell-test/")
                  "--"
                  "cimtest/scripts/ql_cmd_deps"
                  "--path" "cimtest/quicklisp/"))))
