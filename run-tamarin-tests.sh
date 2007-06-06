#!/bin/bash
#
# Usage:
#       run-spidermonkey-tests.sh [-p prefix] [-d dirname] [-v] [limit]
#
# Options:
#	-v causes output to go to stdout instead of the logfile (used for autobuilds)
#       -p selects a test file prefix (eg 15.10 for regexp tests)
#       -s selects a section name (eg Array for array tests)
#       -n dry run
#
# Observe that -p and -d are not redundant because test files in some
# directories do not have predictable prefixes (eg many regexp tests start
# with the prefix "regress-").

LIMIT=""
PREFIX=""
SECNAME=""
VERBOSE=0
NOEXEC=0
ROOT="tamarin/test"
STARTDIR=$ROOT
LOG="tamarin-test.log"
EXT="as"
rm -f $LOG

# grab the tamarin tests
if [ -d "tamarin" ]; then
  hg -R tamarin/ pull http://10.132.64.101:8000
else
  hg clone http://10.132.64.101:8000 tamarin
fi




#Open LOG with file descriptor 6
exec 6<> $LOG

while getopts "p:s:d:vn" OPTIONS
do
	case $OPTIONS in
		p)	PREFIX=$OPTARG ;;
	        s)      SECNAME=$OPTARG ;;
		d)	STARTDIR=$OPTARG ;;
		v)	exec 6>&1; VERBOSE=1 ;; #redirect fd 6 to stdout instead of logfile
	        n)      NOEXEC=1 ;;
	esac
done

shift $(($OPTIND -1))

LIMIT=$1

if [ $NOEXEC = 0 ]; then
  make dump-heap-for-running 1>&6 2>&6
fi

runTest() {
    SECTION=$1
    echo "SECTION: $SECTION"
    #Get a list of subdirectories
    SUB_DIRS=$(find $SECTION -maxdepth 1 -mindepth 1 -type d)
    if [[ $SECNAME == "" || $(basename $SECTION) == $SECNAME ]]; then
      for FILE in $(find $SECTION -maxdepth 1 -type f -name $PREFIX\*.$EXT)
        do
        # -z:string is null, -o:logical OR
        if [ \( -z "$LIMIT" \) -o \( "$LIMIT" == $( dirname $FILE ) \) ]; then 
          echo "FILE: $FILE"
	  # If there is a dir with the same name as .as file, include all files in that dir
	  SUBDIR=$(dirname $FILE)/$(basename $FILE .$EXT)
	  if [ -d $SUBDIR ]; then
	    #remove subdir from SUB_DIRS list
	    SUB_DIRS=${SUB_DIRS/$SUBDIR/}
	    for INC_FILE in $(find $SUBDIR -type f -name *.$EXT)
	      do
		FILE="$INC_FILE $FILE"
	    done
	    echo "INC: $FILE"
	  fi 
  	  if [ $NOEXEC = 0 ]; then
  	    if [ -s $SECTION/shell.$EXT ]; then
	      make run-dumped FILE="$ROOT/shell.$EXT $SECTION/shell.$EXT $FILE" 1>&6 2>&6
	    else
	      make run-dumped FILE="$ROOT/shell.$EXT $FILE" 1>&6 2>&6
	    fi
          fi
        fi
      done
      #Go through child dirs (that weren't include dirs)
      for DIR in $SUB_DIRS
      do
	runTest $DIR
      done
    fi
}

runTest $STARTDIR

if [[ $VERBOSE == 0 ]]
then
	#close fd 6
	exec 6>&-
fi

perl analyze-spidermonkey-log.pl $LOG
