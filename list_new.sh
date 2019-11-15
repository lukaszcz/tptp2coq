#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "usage: list_new.sh first.log second.log"
    exit 1
fi

diff -u $1 $2 | grep '^\+.*.log$' | sed s/^\+//
