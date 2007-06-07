#!/bin/sh

if ! which sml >/dev/null 2>&1 ; then
    echo "The Cygwin release of the ECMAScript Edition 4 Reference implementation"
    echo "currently requires an installation of SML/NJ."
    echo
    echo "Please install SML/NJ version 110.64 or later."
    echo
    echo "You can download the latest version of SML/NJ from http://www.smlnj.org."
    exit 1
fi

_here=`dirname $0`

# help the evaluator find the "decimal" executable
export PATH="$PATH":$_here

exec sml @SMLload=$_here/es4.heap $*
