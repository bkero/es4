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
    use namespace __ES4__;
    use strict;

    import ECMAScript4_Internal.*;
    import JSON.*;

    // The [[Prototype]] of "decimal" is Number.[[Prototype]]
    // Don't add prototype methods or properties here!

    __ES4__ final class decimal!
    {
        static const length = 1;

        static const MAX_VALUE         = 999999999999999999999999999999999999999e+6111m;
        static const MIN_VALUE         = 10e-6143m;
        static const NaN               = 0.0m / 0.0m;
        static const NEGATIVE_INFINITY = -1.0m / 0.0m;
        static const POSITIVE_INFINITY = 1.0m / 0.0m;

        // 15.8.1 Value Properties of the Math Object.  These are {DD,DE,RO}.

        static const E: decimal = 2.718281828459045235360287471352662m;   /* Approximately */
        static const LN10: decimal = 2.302585092994045684017991454684364m;    /* Approximately */
        static const LN2: decimal = 0.6931471805599453094172321214581766m;    /* Approximately */
        static const LOG2E: decimal = 1.442695040888963407359924681001892m;  /* Approximately */
        static const LOG10E: decimal = 0.4342944819032518276511289189166051m; /* Approximately */
        static const PI: decimal = 3.141592653589793238462643383279503m;     /* Approximately */
        static const SQRT1_2: decimal = 0.7071067811865475244008443621048490m;/* Approximately */
        static const SQRT2: decimal = 1.414213562373095048801688724209698m;  /* Approximately */

        /* E262-3 15.7.1.1: The decimal Constructor Called as a Function */
        meta static function invoke(x=0m)
            (x is decimal) ? x : new decimal(x);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return string(this);
            if (radix is AnyNumber && 
                radix >= 2 && radix <= 36 && helper::isIntegral(radix))
                return informative::toString(int(radix));
            throw new TypeError("Invalid radix argument to decimal.toString");
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

        override intrinsic function valueOf() : decimal
            this;

        // FIXME: wrong to convert to double here
        intrinsic function toFixed(fractionDigits=0) : string
            double(this).intrinsic::toFixed(fractionDigits);

        // FIXME: wrong to convert to double here
        intrinsic function toExponential(fractionDigits=undefined) : string
            double(this).intrinsic::toExponential(fractionDigits);

        // FIXME: wrong to convert to double here
        intrinsic function toPrecision(precision=undefined) : string
            double(this).intrinsic::toPrecision(precision);

        /* The E262-3 number primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
}
