#!/bin/sh

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

# install-smlnj.sh
#
# Downloads, builds, and installs the latest svn sources of SML/NJ to
# the provided installation directory.
#
#     usage: install-smlnj.sh dir
#         dir   installation directory
#
# NOTE: It should be safe to rerun this script multiple times over the
# same directory without deleting the contents of the directory. It
# should NOT be run over directories that were not originally created
# by this script.


print_usage()
{
    echo 'usage: install-smlnj.sh dir'
    echo '    dir   installation directory'
    echo
    echo 'NOTE: It should be safe to rerun this script multiple times over the'
    echo 'same directory without deleting the contents of the directory. It'
    echo 'should NOT be run over directories that were not originally created'
    echo 'by this script.'
}

die()
{
    if [ $# -eq 1 ]; then
        echo
        echo "ERROR: $1"
    fi
    exit 1
}

abs_path()
{
    (mkdir -p "$1"; cd "$1"; pwd)
}

update_targets()
{
    FILE=$1
    shift

    cmd=sed
    for i in $@; do
        if echo $i | grep '^-' >/dev/null 2>&1; then
            i=`echo $i | sed -e s/^-//`
            cmd="$cmd -e 's/^request[ ]$i/#request $i/'"
        else
            i=`echo $i | sed -e s/^+//`
            cmd="$cmd -e 's/^#request[ ]$i/request $i/'"
        fi
    done

    cmd="$cmd $FILE"
    eval $cmd
}

progress()
{
    echo -n $1 1>&2
}

complete()
{
    echo 'done.' 1>&2
}

# Check command-line arguments.
if [ $# -ne 1 ]; then
    print_usage
    die
fi

LOG_FILE=`abs_path \`dirname $0\``/install-smlnj.log
TARGET_DIR=`abs_path $1`
TARGETS='-src-smlnj -eXene +ml-lex +ml-yacc +ml-ulex +ml-antlr +ml-lpt-lib +smlnj-lib +tdp-util +cml +cml-lib +mlrisc +ml-nlffi-lib +ml-nlffigen +mlrisc-tools +heap2asm'

echo 'Installing SML/NJ from subversion sources. The installation log lives at'
echo
echo "    $LOG_FILE"
echo
echo 'You can monitor the progress of the installation by running'
echo
echo "    tail -f '$LOG_FILE'"
echo

touch "$LOG_FILE"
echo -n '*** INSTALLATION OF ' >> "$LOG_FILE"
date >> "$LOG_FILE"

# If we're installing in cygwin, use the cygwin runtime.
case `uname` in
CYGWIN*)
    export SMLNJ_CYGWIN_RUNTIME=1
esac

# Download the administrative scripts from SVN.
progress '1. Downloading administrative scripts...'
mkdir -p $TARGET_DIR/admin
cd $TARGET_DIR/admin
(echo $PATH && which svn && svn --version&& svn co svn://smlnj-gforge.cs.uchicago.edu/smlnj/admin >>"$LOG_FILE" 2>&1)
complete

# Checkout the main SML/NJ source code from SVN.
progress '2. Checking out SML/NJ source code from subversion...'
(./admin/checkout-all.sh $TARGET_DIR >>"$LOG_FILE" 2>&1) || die 'failed to check out svn sources'
complete
cd $TARGET_DIR

# Select just a minimal subset of packages for bootstrapping.
cp ./config/targets ./config/targets.initial
cat >./config/targets <<EOF
request ml-lex
request ml-yacc
request smlnj-lib
request tdp-util
request mlrisc
EOF

# Bootstrap the system.
progress '3. Bootstrapping SML/NJ...'
(./config/install.sh >>"$LOG_FILE" 2>&1) || die 'failed to install initial bootstrap environment'
progress '.'
cd ./base/system
(./fixpt >>"$LOG_FILE" 2>&1) || die 'failed to compile bootstrapped SML/NJ compiler'
progress '.'
(./makeml >>"$LOG_FILE" 2>&1) || die 'failed to make bootstrapped SML/NJ'
progress '.'
(./installml >>"$LOG_FILE" 2>&1) || die 'failed to install bootstrapped SML compiler'
complete

# Now that we've bootstrapped, request all the packages we want.
progress '4. Installing SML/NJ packages...'
cd $TARGET_DIR
update_targets config/targets.initial $TARGETS > ./config/targets

# Reinstall, forcing the new packages to be installed.
(./config/install.sh >>"$LOG_FILE" 2>&1) || die 'failed to install full package set'
complete

echo
echo 'Installation successful!'
echo
echo "The executables live in $TARGET_DIR/bin. You should add these to your PATH."

case `uname` in
CYGWIN*)
    echo
    echo 'Make sure whenever running SML that the SMLNJ_CYGWIN_RUNTIME environment'
    echo 'variable is set to 1. You should probably put this in your login scripts or '
    echo 'Windows environment.'
esac

echo
