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
	      make run FILE="$ECMA/shell.js $SECTION/shell.js $FILE" >>$LOG 2>&1
	  else
	      make run FILE="$ECMA/shell.js $FILE" >>$LOG 2>&1
	  fi
      fi
    done
  done
done


echo "----------------------------------------------"
echo "TOTALS"
if [ ! -z $LIMIT ]
then
    echo " (under $LIMIT)"
fi
echo "----------------------------------------------"

function count {
    grep -F -c "$1" "$LOG"
}

function percent {
    echo "scale=2; 100 * ($1 / ($1 + $2))" | bc
}

function report { 
    printf "%10.10s=%-.f, %10.10s=%-.f (%.2f%% %s)\n" $1 $2 $3 $4 $(percent $2 $4) $1
}

FINISHED=$(count 'evaluated!')
CRASHED=$(( $(count '**ERROR**') + $(count 'Alarm clock') ))

PASSED=$(count 'PASSED!')
FAILED=$(count 'FAILED!')

echo -n "tests in suite:"
report "FINISHED" $FINISHED "CRASHED" $CRASHED
echo -n "cases in tests:"
report "PASSED" $PASSED "FAILED" $FAILED

echo "----------------------------------------------"