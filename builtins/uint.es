/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "uint" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code
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
 * Status: Complete; not reviewed; not tested.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use strict;

    intrinsic final class uint! extends Number
    {
        static const MAX_VALUE : uint = 0xFFFFFFFFu;
        static const MIN_VALUE : uint = 0;

        /* E262-4 draft */
        meta static function convert(x)
            uint(x);

        /* E262-4 draft: The uint Constructor Called as a Function */
        meta static function invoke(x=0u)
            x is uint ? x : magic::newUInt(x);

        /* E262-4 draft: The uint constructor */
        function uint(x=0u) : super(x)
            magic::bindUInt(this, x);

        /* E262-4 draft: uint.prototype.toString */
        prototype function toString(this:uint, radix)
            this.toString(radix);

        override intrinsic function toString(radix=10) {
            if (radix === 10 || radix === undefined)
                return ToString(this);
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                radix = int(radix);
                let v = this;
                var q = "";
                while (v != 0) {
                    q = "0123456789abcdefABCDEFGHIJKLMNOPQRSTUVWXYZ"[v % radix] + q;
                    v = (v - (v % radix)) / radix;
                }
                if (q == "")
                    q = "0";
                return q;
            }
            else
                throw new TypeError("Invalid radix argument to uint.toString");
        }

        /* E262-4 draft: uint.prototype.toLocaleString() */
        prototype function toLocaleString(this:uint)
            this.toLocaleString();

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            this.toString();  /* FIXME: "this." should not be necessary */

        /* E262-4 draft: uint.prototype.valueOf */
        prototype function valueOf(this:uint)
            this.valueOf();

        override intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 uint.prototype.toFixed */
        prototype function toFixed(this : uint, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        override intrinsic function toFixed(fractionDigits : double) : string
	    ToDouble(this).toFixed(fractionDigits);

        /* E262-4 draft: uint.prototype.toExponential */
        prototype function toExponential(this : uint, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        override intrinsic function toExponential(fractionDigits : double) : string
	    ToDouble(this).toExponential(fractionDigits);

        /* E262-4 draft: uint.prototype.toPrecision */
        prototype function toPrecision(this : uint, precision)
            this.toPrecision(ToDouble(precision));

        override intrinsic function toPrecision(precision : double) : string
	    ToDouble(this).toPrecision(precision);

        /* The E262-3 number primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
}
