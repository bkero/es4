/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "int" object
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
    import ECMAScript4_Internal.*;

    // The [[Prototype]] of "int" is Number.[[Prototype]]
    // Don't add prototype methods or properties here!

    intrinsic final class int!
    {
        static const MAX_VALUE : int = 0x7FFFFFFFi;
        static const MIN_VALUE : int = -0x80000000i;

        /* Obsolete, needed for the moment because the RI does not yet handle
           interconversion of numbers */
        meta static function convert(x)
            int(x);

        /* E262-4 draft: The int Constructor Called as a Function */
        meta static function invoke(x=0)
            x is int ? x : magic::newInt(x);

        /* E262-4 draft: The int constructor */
        function int(x=0i)
            magic::bindInt(this, x);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return string(this);
            if (typeof radix === "number" &&
                     radix >= 2 &&
                     radix <= 36 && helper::isIntegral(radix)) {
                /* INFORMATIVE */
                radix = int(radix);
                let v = this;
                let s = "";
                var q = "";
                if (v < 0) {
                    s = "-";
                    v = -v;
                }
                while (v != 0) {
                    q = "0123456789abcdefABCDEFGHIJKLMNOPQRSTUVWXYZ"[v % radix] + q;
                    v = (v - (v % radix)) / radix;
                }
                if (q == "")
                    q = "0";
                return s + q;
            }
            throw new TypeError("Invalid radix argument to int.toString");
        }

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            intrinsic::toString();

        override intrinsic function valueOf(): int
            this;

        intrinsic function toFixed(fractionDigits=0) : string
            double(this).intrinsic::toFixed(fractionDigits);

        intrinsic function toExponential(fractionDigits=undefined) : string
            double(this).intrinsic::toExponential(fractionDigits);

        intrinsic function toPrecision(precision=undefined) : string
            double(this).intrinsic::toPrecision(precision);

        /* The E262-3 number primitive consumes all additional [[set]] operations. */
        // FIXME: why is this here?
        meta function set(n,v) : void
        {
        }
    }
}
