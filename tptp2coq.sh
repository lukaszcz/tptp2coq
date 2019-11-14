#!/bin/bash

rm -rf ILTP-coq*

# translate the problems from FOF TPTP to Coq format
mkdir ILTP-coq
cd ILTP
for f in `find -name "*.p" -print`
do
    out=../ILTP-coq/`basename $f .p`.v
    tmp=$f.tmp
    iconv -c -t UTF-8 $f -o $tmp
    echo '(*' > $out
    grep '% ' $tmp >> $out
    echo '*)' >> $out
    echo >> $out
    ../tptp2coq $1 $tmp >> $out
    rm $tmp
done
cd ..

# remove non-theorems
for f in `grep -l "Status (intuit.) : Non-Theorem" ILTP-coq/*.v`
do
    rm $f
done

# rename files
for f in ILTP-coq/*.v
do
    f2=ILTP-coq/`basename $f .v | tr '+' '_' | tr '.' '_'`.v
    mv $f $f2
done

# distribute problems to different directories based on rating
for d in 0.00 0.25 0.50 0.75 1.00
do
    mkdir ILTP-coq-$d/
    grep -l "Rating (intuit.) : $d" ILTP-coq/*.v | xargs -I{} cp {} ILTP-coq-$d/
done
