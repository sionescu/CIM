#!/bin/bash

benchmark (){
    cim use $1
    ./benchmark.sh
}

benchmark ccl
benchmark sbcl
benchmark ecl
benchmark abcl
benchmark clisp
benchmark alisp

