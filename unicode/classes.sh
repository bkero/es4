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

# Generate all the class tables into one file, 'classes.es'

(echo "/* See UnicodeTbl.es for instructions */"
echo "use default namespace Unicode; "
./class.sh Lu
./class.sh Ll
./class.sh Lt
./class.sh Lm
./class.sh Lo
./class.sh Mn
./class.sh Mc
./class.sh Me
./class.sh Nd
./class.sh Nl
./class.sh No
./class.sh Pc
./class.sh Pd
./class.sh Ps
./class.sh Pe
./class.sh Pi
./class.sh Pf
./class.sh Po
./class.sh Sm
./class.sh Sc
./class.sh Sk
./class.sh So
./class.sh Zs
./class.sh Zl
./class.sh Zp
./class.sh Cc
./class.sh Cf
./class.sh Cs
./class.sh Co
./class.sh Cn
echo "}" ) > ../builtins/UnicodeClasses.es

