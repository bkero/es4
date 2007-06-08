#!/bin/bash

# The following licensing terms and conditions apply and must be
# accepted in order to use the Reference Implementation:
# 
#    1. This Reference Implementation is made available to all
# interested persons on the same terms as Ecma makes available its
# standards and technical reports, as set forth at
# http://www.ecma-international.org/publications/.
# 
#    2. All liability and responsibility for the implementation or other
# use of this Reference Implementation rests with the implementor, and
# not with any of the parties who contribute to, or who own or hold any
# copyright in, this Reference Implementation.
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
#
# Observe that -p and -d are not redundant because test files in some
# directories do not have predictable prefixes (eg many regexp tests start
# with the prefix "regress-").

LIMIT=""
PREFIX=""
SECNAME=""
VERBOSE=0
NOEXEC=0
ROOT=tests/spidermonkey
LOG=spidermonkey-test.log
rm -f $LOG

#Open LOG with file descriptor 6
exec 6<> $LOG

while getopts "p:s:vn" OPTIONS
do
	case $OPTIONS in
		p)	PREFIX=$OPTARG ;;
	        s)      SECNAME=$OPTARG ;;
		v)	exec 6>&1; VERBOSE=1 ;; #redirect fd 6 to stdout instead of logfile
	        n)      NOEXEC=1 ;;
	esac
done

shift $(($OPTIND -1))

LIMIT=$1

if [ $NOEXEC = 0 ]; then
  make dump-heap 1>&6 2>&6
fi
for ECMA in $ROOT/ecma*
  do
  for SECTION in $(find $ECMA -mindepth 1 -maxdepth 1 -type d)
    do
    if [[ $SECNAME == "" || $(basename $SECTION) == $SECNAME ]]; then
    for FILE in $(find $SECTION -type f -name $PREFIX\*.js)
      do
      # -z:string is null, -o:logical OR
      if [ \( -z "$LIMIT" \) -o \( "$LIMIT" == $( dirname $FILE ) \) ]; then 
        echo $FILE
	if [ $NOEXEC = 0 ]; then
  	  if [ -s $SECTION/shell.js ]; then
	    make run-dumped FILE="$ECMA/shell.js $SECTION/shell.js $FILE" 1>&6 2>&6
	  else
	    make run-dumped FILE="$ECMA/shell.js $FILE" 1>&6 2>&6
	  fi
        fi
      fi
    done
    fi
  done
done

if [[ $VERBOSE == 0 ]]
then
	#close fd 6
	exec 6>&-
fi

perl analyze-spidermonkey-log.pl $LOG
