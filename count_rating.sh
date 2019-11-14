#!/bin/bash
grep -l "Rating (intuit.) : $1" problems/*.v | xargs basename -a 2>/dev/null | sed 's/v$/log/' | awk '{print "logs/"$1}' | xargs grep "Tactic call ran for .* (success)" | wc -l
