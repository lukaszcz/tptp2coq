#!/bin/bash

for f in `grep -l "Rating (intuit.) : $1" problems/*.v`
do
    basename $f
done | sed 's/v$/log/' | awk '{print "logs/"$1}' | xargs grep "Tactic call ran for .* (success)" | wc -l | xargs -I {} echo {} "/" `grep -l "Rating (intuit.) : $1" problems/*.v | wc -l`
