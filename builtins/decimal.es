/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "decimal" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code.
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
 * Status: Incomplete (toExponential, toPrecision, toFixed; constants); not reviewed; not tested.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use strict;

    intrinsic final class decimal! extends Number
    {       
	// FIXME
        static const MAX_VALUE         = 1.7976931348623157e+308m;
        static const MIN_VALUE         = 5e-324m;
        static const NaN               = 0.0m / 0.0m;  // ???
        static const NEGATIVE_INFINITY = -1.0m / 0.0m; // ???
        static const POSITIVE_INFINITY = 1.0m / 0.0m;  // ???

        /* E262-4 draft */
        meta static function convert(x)
            decimal(x);

        /* E262-3 15.7.1.1: The decimal Constructor Called as a Function */
        meta static function invoke(x=0m)
            x is decimal ? x : new decimal(x);

        /* E262-3 15.7.2.1: The decimal constructor */
        function decimal(x=0m)
            magic::bindDecimal(this, x);

        /* E262-3 15.7.4.2: decimal.prototype.toString */
        prototype function toString(this:decimal, radix)
            this.toString(radix);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return ToString(this);
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            }
            else
                throw new TypeError("Invalid radix argument to decimal.toString");
        }
        
        /* E262-3 15.7.4.3: decimal.prototype.toLocaleString() */
        prototype function toLocaleString(this:decimal)
            this.toLocaleString();

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            toString();

        /* E262-3 15.7.4.4: decimal.prototype.valueOf */
        prototype function valueOf(this:decimal)
            this.valueOf();

        override intrinsic function valueOf() : decimal
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:decimal, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:decimal, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:decimal, precision)
            this.toPrecision(ToDouble(precision));

        // FIXME these are supposed to be native, but the parser has trouble
        // parsing "override intrinsic function native". No idea why.
        override intrinsic function toFixed(fractionDigits:double) : string "";
        override intrinsic function toExponential(fractionDigits:double) : string ""; 
        override intrinsic function toPrecision(precision:double) : string "";
    }
}
