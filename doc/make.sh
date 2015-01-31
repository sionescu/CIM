#!/bin/sh

: ${DOC_ROOT:="$(cd $(dirname $0); pwd)"}
: ${MAN_DIR:="${DOC_ROOT}/man/man1"}
: ${AUTO_POLL:=false}
: ${POLL_INTERVAL:=5}

txt2man(){
    if [ ! -e "${MAN_DIR}/$(basename $1 .txt).1" ] || [ "$1" -nt "${MAN_DIR}/$(basename $1 .txt).1" ]; then
        echo "Building $1"
        if a2x -v  --doctype manpage --format manpage -D "${MAN_DIR}" "$1" > "log/$(basename $1 .txt).log" 2>&1
        then
            echo "O Built $1"
        else
            echo "X Bulid failed: $1. See log/$(basename $1 .txt).log"
            return 1
        fi
    fi
}

cd "${DOC_ROOT}"
while "${AUTO_POLL}"; do
      for f in *.txt; do
          txt2man "$f" &
      done

      wait
      if  ${AUTO_POLL}; then
          sleep "${POLL_INTERVAL}"
      fi
done
