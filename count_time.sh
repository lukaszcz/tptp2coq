#!/bin/bash

COUNT=0
for f in `grep -l "Tactic call ran for .* (success)" logs/*.log`
do
    TIME=`cat $f | grep "Tactic call ran for .* (success)" | sed 's/.* for \(.*\) secs .*/\1/'`
    if (( $(echo "$TIME < $1" | bc -l) )); then
        COUNT=$((COUNT + 1))
    fi
done
echo $COUNT
