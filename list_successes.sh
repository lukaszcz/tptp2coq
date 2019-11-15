#!/bin/bash

for f in `grep -l "Tactic call ran for .* (success)" logs$1/*.log`; do basename $f; done > successes$1.log
