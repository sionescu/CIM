#!/bin/sh

sources="packages.lisp cim.lisp option-parser.lisp repl.lisp process-args.lisp mains.lisp run.lisp"

run(){
    cat > script.lisp <<EOF
;; This is an automatically generated lisp file.
;; Modification to this file has no effect on the main source
;; and may be overwritten by the later call to the build.sh .
EOF
    cat $sources >> script.lisp
    exit 0
}

if [ ! -e script.lisp ]
then
    run
fi

for lisp in $sources
do
    if [ $lisp -nt script.lisp ]
    then
        run
    fi
done

