#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "usage: run.sh num_cores time_limit_in_s directory [email]"
    exit 1
fi

N=$1
LIMIT=$2
DIR=$3

rm problems
ln -s $DIR problems
rm problems/*.vo
rm -rf logs
mkdir logs

make -f Makefile.coq -j $N LIMIT=$LIMIT

echo -n "Successes: "
grep "Tactic call ran for .* (success)" logs/*.log | wc -l
echo -n "Failures: "
grep "Tactic call ran for .* (failure)" logs/*.log | wc -l
echo -n "Timeouts: "
grep -L "Tactic call ran for" logs/*.log | wc -l

if [ -n "$4" ]; then
    echo "" | mail -s "Evaluation finished" "$4"
fi
