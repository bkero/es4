#!/bin/sh
#
# Usage:
#	-v causes output to go to stdout instead of the logfile (used for autobuilds)
#    run-spidermonkey-tests.sh [-p prefix] [-v] [limit]

LIMIT=""
PREFIX=""
VERBOSE=0
ROOT=tests/spidermonkey
LOG=spidermonkey-test.log
rm -f $LOG

#Open LOG with file descriptor 6
exec 6<> $LOG

while getopts "p:v" OPTIONS
do
	case $OPTIONS in
		p)	PREFIX=$OPTARG;;
		v)	exec 6>&1; VERBOSE=1 ;; #redirect fd 6 to stdout instead of logfile
	esac
done

shift $(($OPTIND -1))

LIMIT=$1

make dump-heap-for-running 1>&6 2>&6
for ECMA in $ROOT/ecma*
  do
  for SECTION in $(find $ECMA -mindepth 1 -maxdepth 1 -type d)
    do
    for FILE in $(find $SECTION -type f -name $PREFIX\*.js)
      do
      # -z:string is null, -o:logical OR
      if [ \( -z "$LIMIT" \) -o \( "$LIMIT" == $( dirname $FILE ) \) ]
	  then 
	  echo $FILE
	  if [ -s $SECTION/shell.js ]
	      then 
	      make run-dumped FILE="$ECMA/shell.js $SECTION/shell.js $FILE" 1>&6 2>&6
	  else
	      make run-dumped FILE="$ECMA/shell.js $FILE" 1>&6 2>&6
	  fi
      fi
    done
  done
done

if [[ $VERBOSE == 0 ]]
then
	#close fd 6
	exec 6>&-
fi

perl analyze-spidermonkey-log.pl $LOG
