/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "double" object
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
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code.
 *
 * Status: Incomplete.
 *
 * TO DO:
 *  - toPrecision and toExponential currently punt to native code
 *    but are mostly expressible in ECMAScript.  toFixed shows how this
 *    might be done.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    import ECMAScript4_Internal.*;
    import JSON.*;

    // The [[Prototype]] of "double" is Number.[[Prototype]]
    // Don't add prototype methods or properties here!

    __ES4__ final class double!
    {
        static const length = 1;

        static const MAX_VALUE : double         = 1.7976931348623157e+308;  /* INFORMATIVE */
        static const MIN_VALUE : double         = 5e-324;                   /* INFORMATIVE */
        static const NaN : double               = 0.0 / 0.0;
        static const NEGATIVE_INFINITY : double = -1.0 / 0.0;
        static const POSITIVE_INFINITY : double = 1.0 / 0.0;
        // 15.8.1 Value Properties of the Math Object.  These are {DD,DE,RO}.
        static const E: double = 2.7182818284590452354;   /* Approximately */
        static const LN10: double = 2.302585092994046;    /* Approximately */
        static const LN2: double = 0.6931471805599453;    /* Approximately */
        static const LOG2E: double = 1.4426950408889634;  /* Approximately */
        static const LOG10E: double = 0.4342944819032518; /* Approximately */
        static const PI: double = 3.1415926535897932;     /* Approximately */
        static const SQRT1_2: double = 0.7071067811865476;/* Approximately */
        static const SQRT2: double = 1.4142135623730951;  /* Approximately */

        /* E262-3 15.7.1.1: The double Constructor Called as a Function */
        meta static function invoke(x=0d)
            (x is double) ? x : new double(x);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return string(this);
            if (radix is AnyNumber && 
                radix >= 2 && radix <= 36 && helper::isIntegral(radix))
                return informative::toString(int(radix));
            throw new TypeError("Invalid radix argument to double.toString");
        }

        informative function toString(radix) {
            // FIXME
            throw new Error("Unimplemented: non-decimal radix");
        }

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            toString();

        override intrinsic function toJSONString(pretty: boolean=false) : string
            JSON.formatNumber(this, pretty);

        override intrinsic function valueOf() : double
            this;

        intrinsic function toFixed(fractionDigits=0) : string {
            let x = this;
            let f = helper::toInteger(fractionDigits);
            if (f < 0 || f > 20)
                throw new RangeError();

            if (isNaN(x))
                return "NaN";
            let s = "";
            if (x < 0) {
                s = "-";
                x = -x;
            }

            if (x >= Math.pow(10,21))
                return s + string(m);

            let n = toFixedStep10(x, f);
            let m = n == 0 ? "0" : string(n);
            if (f == 0)
                return s + m;
            let k = m.length;
            if (k <= f) {
                m = "00000000000000000000".substring(0,f+1-k) + m;
                k = f+1;
            }
            return s + m.substring(0,k-f) + "." + m.substring(k-f);
        }

        /* Step 10 of the toFixed algorithm in E262-3 15.7.4.5: return
           an integer n such that n / 10^f - x is as close to zero as
           possible.  If there are two such n, pick the larger.

           x must be positive, f is in the range [0,20]. */

        // FIXME: really informative, not intrinsic
        native intrinsic function toFixedStep10(x : (double|decimal), f : int) : int;

        intrinsic function toExponential(fractionDigits=undefined) : string {
            return "**toExponential: FIXME**";
        }            

        intrinsic function toPrecision(precision=undefined) : string {
            return "**toPrecision: FIXME**";
        }

        /* The E262-3 number primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
}
