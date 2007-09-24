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

        static const length = 1;

        function ByteArray(length: uint = 0)
            : length = length
        {
        }

        meta static function invoke(value) : ByteArray! {
            if (value is ByteArray)
                return value;
            let ba = new ByteArray();
            if (value is Strings) {
                value = string(value);
                for ( let i=0, limit=value.length ; i < limit ; i++ )
                    ba[i] = value.intrinsic::charCodeAt(i) & 255;
            }
            else {
                for ( let i=0, limit=Number(value.length) ; i < limit ; i++ )
                    ba[i] = Number(value[i]) & 255;
            }
            return ba;
        }

        function get length()
            _length;

        function set length(n) : void {
            if (!(n is Numeric &&
                  n >= 0 && n <= 0xFFFFFFFF &&
                  helper::isIntegral(n)))
                throw new RangeError("Invalid ByteArray length");
            n = uint(n);
            for ( let i=length ; i < n ; i++ )
                magic::setByteArrayByte(this, n, 0);
            _length = n;
        }

        meta function get(k) {
            if (helper::isIndex(k)) {
                k = uint(k);
                if (k < length)
                    return magic::getByteArrayByte(this, k);
                else
                    return 0;
            }
            else
                return intrinsic::get(this, k);
        }

        meta function set(k, v) : void {
            if (helper::isIndex(k)) {
                k = uint(k);
                v = uint(v);
                if (k >= length)
                    length = k+1;
                magic::setByteArrayByte(this, k, v);
            }
            else
                intrinsic::set(this, k, v);
        }

        override intrinsic function toString() : string {
            let s = "";
            for ( let i=0, limit=length ; i < limit ; i++ )
                s += string.fromCharCode(this[i]);
            return s;
        }

        prototype function toString(this: ByteArray)
            this.intrinsic::toString();

        private var _length : uint = 0;
    }
}
