#!/bin/sh

sources=$(echo packages.lisp cim.lisp option-parser.lisp repl.lisp process-args.lisp mains.lisp run.lisp)

if [ ! -e script.lisp ]
then
    cat $sources > script.lisp
    exit 0
fi

for lisp in $sources
do
    if [ $lisp -nt script.lisp ]
    then
        cat $sources > script.lisp
        exit 0
    fi
done

