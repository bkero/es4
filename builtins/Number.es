/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Number" wrapper object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
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
 * The committee decided at the January 2007 meeting at Mozilla that
 * "Number" is a heavyweight wrapper (non-final, dynamic) for a
 * "double" value.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use namespace __ES4__;

    import ECMAScript4_Internal.*;

    __ES4__ type AnyNumber = (double|decimal|Number!);

    dynamic class Number
    {
        private var val : double;
        
        // IMPLEMENTATION ARTIFACT: A getter because Number is loaded before int.
        static function get length() { return 1 }

        // IMPLEMENTATION ARTIFACT:  Static getters because Number is loaded before double.
        static function get MAX_VALUE() : double         1.7976931348623157e+308;  /* INFORMATIVE */
        static function get MIN_VALUE() : double         5e-324;                   /* INFORMATIVE */
        static function get NaN() : double               0d / 0d;
        static function get NEGATIVE_INFINITY() : double -1d / 0d;
        static function get POSITIVE_INFINITY() : double 1d / 0d;

        /* E262-3 15.7.1.1: The Number Constructor Called as a Function */
        meta static function invoke(value=0d) {
            if (value is AnyNumber)
                return value;
            return double(value);
        }

        /* E262-3 15.7.2.1: The Number constructor */
        function Number(value=0d) : val = double(value) {}

        override intrinsic function toString(radix = 10) : string
            intrinsic::valueOf().intrinsic::toString(radix);

        override intrinsic function toLocaleString() : string
            intrinsic::valueOf().intrinsic::toLocaleString();

        override intrinsic function toJSONString(pretty: boolean=false) : string
            intrinsic::valueOf().intrinsic::toJSONString(pretty);

        override intrinsic function valueOf(): (double|decimal)
            val;

        intrinsic function toFixed(fractionDigits=0): string
            intrinsic::valueOf().intrinsic::toFixed(fractionDigits);

        intrinsic function toExponential(fractionDigits=undefined) : string
            intrinsic::valueOf().intrinsic::toExponential(fractionDigits);

        intrinsic function toPrecision(precision=undefined) : string
            intrinsic::valueOf().intrinsic::toPrecision(precision);

        /* The prototype is shared with double, and decimal, and none
         * of these functions may assume they operate on a "Number".
         */

        prototype function toString(this: AnyNumber, radix=10)
            this.intrinsic::toString(radix);

        prototype function toLocaleString(this: AnyNumber)
            this.intrinsic::toLocaleString();

        prototype function toJSONString(this: AnyNumber, pretty=false)
            this.intrinsic::toJSONString(pretty);

        prototype function valueOf(this: AnyNumber)
            this.intrinsic::valueOf();

        prototype function toFixed(this:AnyNumber, fractionDigits)
            this.intrinsic::toFixed(fractionDigits);

        prototype function toExponential(this: AnyNumber, fractionDigits)
            this.intrinsic::toExponential(fractionDigits);

        prototype function toPrecision(this: AnyNumber, precision)
            this.intrinsic::toPrecision(precision);
    }
}
