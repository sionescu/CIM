#!/bin/bash

./benchmark.sh ccl

./benchmark.sh sbcl

# ./benchmark.sh ecl
# on ecl, fasl does not work

# ./benchmark.sh abcl 
# too slow

./benchmark.sh clisp

# ./benchmark.sh gcl
# on gcl, fasl does not work

# ./benchmark.sh alisp
# not publicly available?

