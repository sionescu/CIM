#!/bin/bash

rm ../lib/script.lisp
( cd ../lib ; ./build.sh )

rm -rf cimtest/lib cimtest/scripts
cp -rf ../lib cimtest/
cp -rf ../scripts cimtest/

