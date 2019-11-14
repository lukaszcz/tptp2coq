#!/bin/bash

if [ "$1" = "-v" ]; then
    VERBOSE=1
    shift
fi

if [ "$#" -ne 3 ] && [ "$#" -ne 4 ]; then
    echo "usage: run.sh [-v] num_cores time_limit_in_s directory [email]"
    exit 1
fi

N=$1
LIMIT=$2
DIR=$3

rm problems 2> /dev/null
ln -s $DIR problems
rm problems/*.vo 2> /dev/null
rm -rf logs 2> /dev/null
mkdir logs

if [ "$VERBOSE" = "1" ]; then
    make -f Makefile.coq -j $N LIMIT=$LIMIT VERBOSE=1
else
    make -f Makefile.coq -j $N LIMIT=$LIMIT
fi

rm problems/*.{vo,glob}

echo -n "Successes: "
grep "Tactic call ran for .* (success)" logs/*.log | wc -l
echo -n "Failures: "
grep "Tactic call ran for .* (failure)" logs/*.log | wc -l
echo -n "Timeouts: "
grep -L "Tactic call ran for" logs/*.log | wc -l
echo
echo -n "Successes (0.00): "
./count_rating.sh "0.00"
echo -n "Successes (0.25): "
./count_rating.sh "0.25"
echo -n "Successes (0.50): "
./count_rating.sh "0.50"
echo -n "Successes (0.75): "
./count_rating.sh "0.75"
echo -n "Successes (1.00): "
./count_rating.sh "1.00"
echo
echo -n "Successes (< 1s): "
./count_time.sh 1
echo -n "Successes (< 5s): "
./count_time.sh 5
echo -n "Successes (< 10s): "
./count_time.sh 10
echo -n "Successes (< 30s): "
./count_time.sh 30
echo -n "Successes (< 60s): "
./count_time.sh 60

if [ -n "$4" ]; then
    echo "" | mail -s "Evaluation finished" "$4"
fi
