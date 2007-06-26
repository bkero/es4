#!/bin/bash

# The following licensing terms and conditions apply and must be
# accepted in order to use the Reference Implementation:
# 
#    1. This Reference Implementation is made available to all
# interested persons on the same terms as Ecma makes available its
# standards and technical reports, as set forth at
# http://www.ecma-international.org/publications/.
# 
#    2. All liability and responsibility for any use of this Reference
# Implementation rests with the user, and not with any of the parties
# who contribute to, or who own or hold any copyright in, this Reference
# Implementation.
# 
#    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
# HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# End of Terms and Conditions
# 
# Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
# Software ASA, and others.

#
# Usage:
#       run-spidermonkey-tests.sh [-p prefix] [-d dirname] [-v] [limit]
#
# Options:
#	-v causes output to go to stdout instead of the logfile (used for autobuilds)
#       -p selects a test file prefix (eg 15.10 for regexp tests)
#       -s selects a section name (eg Array for array tests)
#       -n dry run
#	-d test directory to run
#	-l logfile name
#
# Observe that -p and -d are not redundant because test files in some
# directories do not have predictable prefixes (eg many regexp tests start
# with the prefix "regress-").

LIMIT=""
PREFIX=""
SECNAME=""
VERBOSE=0
NOEXEC=0
ROOT="tests/as"
STARTDIR=$ROOT
LOG="as-test.log"
EXT="as"



while getopts "p:s:d:l:vn" OPTIONS
do
	case $OPTIONS in
		p)	PREFIX=$OPTARG ;;
	        s)      SECNAME=$OPTARG ;;
		d)	STARTDIR=$OPTARG ;;
		l)	LOG=$OPTARG ;;
		v)	exec 6>&1; VERBOSE=1 ;; #redirect fd 6 to stdout instead of logfile
	        n)      NOEXEC=1 ;;
	esac
done

shift $(($OPTIND -1))
LIMIT=$1

rm -f $LOG
#Open LOG with file descriptor 6
exec 6<> $LOG


if [ $NOEXEC = 0 ]; then
  make dump 1>&6 2>&6
fi

runTest() {
    SECTION=$1
    #echo "SECTION: $SECTION"
    #Get a list of subdirectories
    SUB_DIRS=$(find $SECTION -maxdepth 1 -mindepth 1 -type d)
    if [[ $SECNAME == "" || $(basename $SECTION) == $SECNAME ]]; then
      for FILE in $(find $SECTION -maxdepth 1 -type f -name $PREFIX\*.$EXT)
        do
        # -z:string is null, -o:logical OR
        if [ \( -z "$LIMIT" \) -o \( "$LIMIT" == $( dirname $FILE ) \) ]; then 
          echo "$FILE"
	  # If there is a dir with the same name as .as file, include all files in that dir
	  SUBDIR=$(dirname $FILE)/$(basename $FILE .$EXT)
	  if [ -d $SUBDIR ]; then
	    #remove subdir from SUB_DIRS list
	    SUB_DIRS=${SUB_DIRS/$SUBDIR/}
	    for INC_FILE in $(find $SUBDIR -type f -name *.$EXT)
	      do
		FILE="$INC_FILE $FILE"
	    done
	    #echo "INC: $FILE"
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
grep -c '**ERRORS**' $LOG
grep -c 'SMLload' $LOG
