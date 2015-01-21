#!/bin/sh

DOC_ROOT="$(cd $(dirname $0); pwd)"
MAN_DIR="${DOC_ROOT}/man/man1"

txt2man(){
    if [ ! -e "${MAN_DIR}/$(basename $1 .txt).1" ] || [ "$1" -nt "${MAN_DIR}/$(basename $1 .txt).1" ]; then
        echo "Building $1"
        a2x -v  --doctype manpage --format manpage -D "${MAN_DIR}" "$1" > "log/$(basename $1 .txt).log" 2>&1 ||
            echo "Bulid failed: $1. See log/$(basename $1 .txt).log"
    fi
}

cd "${DOC_ROOT}"
for f in *.txt; do
    txt2man "$f" &
done

wait
