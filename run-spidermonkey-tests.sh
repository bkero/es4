#!/bin/sh

LIMIT=$1
ROOT=tests/spidermonkey
LOG=spidermonkey-test.log
rm -f $LOG

for ECMA in $ROOT/ecma*
  do
  for SECTION in $(find $ECMA -mindepth 1 -maxdepth 1 -type d)
    do
    for FILE in $(find $SECTION -type f -name \*.js)
      do
      if [ \( -z "$LIMIT" \) -o \( "$LIMIT" == $( dirname $FILE ) \) ]
	  then 
	  echo $FILE
	  if [ -s $SECTION/shell.js ]
	      then 
	      make run FILE="$SECTION/shell.js $FILE" >>$LOG 2>&1
	  else
	      make run FILE="$ECMA/shell.js $FILE" >>$LOG 2>&1
	  fi
      fi
    done
  done
done

