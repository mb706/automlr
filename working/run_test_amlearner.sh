#!/bin/sh
if [ -z "$1" ] ; then echo need test number ; exit 1 ; fi
R_LIBS=~/lmu/master/library ./memusg timeout -k 10 600 ./test_amlearner.R $1 2>&1
exit $?

