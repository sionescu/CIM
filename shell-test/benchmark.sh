#!/bin/bash

source .bashrc
export HOME=$(readlink -f ./)
export CIM_HOME=cimtest/

echorun(){
    echo $@
    $@
}

echorun rm -f cimtest/lib/script*
echorun rm -f ../lib/script*
echorun make all

echo total runtime of 5 runs:
echo w/ quicklisp
time {
for i in {1..5}
do
    cl -e "(print $i)"
done
}

echo w/o quicklisp with --no-init option
time {
for i in {1..5}
do
    cl --no-init -e "(print $i)"
done
}

echo w/o quicklisp, script.lisp compiled
echorun cl --no-init -c "cimtest/lib/script.lisp" -Q

time {
for i in {1..5}
do
    cl --no-init -e "(print $i)"
done
}
