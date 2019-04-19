#!/bin/bash
FILE1=/tmp/`basename $1`
FILE2=/tmp/`basename $2`

xmllint --exc-c14n $1 > $FILE1
xmllint --exc-c14n $2 > $FILE2

diff -u --ignore-space-change -U9999999 --ignore-blank-lines $FILE1 $FILE2
