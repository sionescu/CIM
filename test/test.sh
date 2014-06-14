#!/bin/bash

source ~/.bashrc
./easy-update.sh # ensures the latest lib/ is used

testExit(){

    cl <<EOF
(exit)
EOF
}

testPrint(){
    # use ${_ASSERT_EQUALS_} instead of assertEquals
    ${_ASSERT_EQUALS_} ':A' $(cl -e "(print :a) (exit)")
}

. loader
