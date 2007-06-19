/* -*- mode: java; indent-tabs-mode: nil -*-
 * 
 * E262-4 draft proposals:bytearray 
 *
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 * 
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * End of Terms and Conditions
 * 
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *
 *
 * The system needs to know to construct ByteArray instances that have
 * some facility for storing byte data; those data are accessed by
 * magic::getByteArrayByte and magic::setByteArrayByte.
 *
 *
 * Lars and Brendan agreed (2007-01-26) that this class is final,
 * non-dynamic, and nullable.
 *
 * Lars and Brendan agreed (2007-01-26) that the arguments and return
 * value of get* can't be constrained by types (because arbitrary
 * properties can be added to the prototype object).  Thus the
 * strict-mode typechecking behavior spelled out in the proposal must
 * be special-cased in the type checker.
 *
 * In the case of set* the arguments can't be constrained because
 * value conversion (from arbitrary things to uint) would be more
 * permissive than we wish to be.
 */

package 
{
    use default namespace public;
    use namespace intrinsic;

    intrinsic final class ByteArray
    {
        use strict;

        function ByteArray(n : uint = 0) {
            length = n;
        }

        meta static function convert(a : Array!) : ByteArray! {
            let n  : uint = a.length;
            let ba : ByteArray = new ByteArray(n);
            for ( let i : uint = 0 ; i < n ; i++ )
                ba[i] = a[i];
            return ba;
        }

        static function fromString(s : string) : ByteArray! {
            let n  : uint = s.length;
            let ba : ByteArray! = new ByteArray(n);
            
            for ( let i : uint = 0; i < n; i++ )
                ba[i] = s.charCodeAt(i);
            return a;
        }

        static function fromArray(a:Array) : ByteArray!
            a to ByteArray;

        function get length() : uint
            _length;

        function set length(n : uint) : void {
            for ( let i : uint = length ; i < n ; i++ )
                magic::setByteArrayByte(this, n, 0);
            _length = n;
        }

        meta function get(k) {
            if (k is Numeric && intrinsic::isIntegral(k) && k >= 0 && k <= 0xFFFFFFFE) {
                let (k : uint = k) 
                    {
                        if (k < length)
                            return magic::getByteArrayByte(this, k);
                        else
                            return 0;
                    }
            }
            else
                return intrinsic::get(this, k);
        }

        meta function set(k, v) : void {
            if (k is Numeric && intrinsic::isIntegral(k) && k >= 0 && k <= 0xFFFFFFFE) {
                let (k : uint = k,
                     v : uint = v)
                    {
                        if (k >= length)
                            length = k+1;
                        magic::setByteArrayByte(this, k, v);
                    }
            }
            else
                intrinsic::set(this, k, v);
        }

        prototype function toString(this: ByteArray) 
            this.intrinsic::toString();

        override intrinsic function toString() : string {
            let n : uint = length;
            let s : string = "";
            for ( let i:int = 0; i < n; i++ )
                s += string.fromCharCode(this[i]);
            return s;
        }

        private var _length : uint = 0;
    }
}
