#!/bin/bash

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
    ../tptp2coq $tmp >> $out
    rm $tmp
done
cd ..
