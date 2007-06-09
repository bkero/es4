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
    use strict;

    dynamic class Number
    {
        // These need to be static getters rather than static consts,
        // since 'Number' initializes before 'double'.

        static function get MAX_VALUE() : double         1.7976931348623157e+308;  /* INFORMATIVE */
        static function get MIN_VALUE() : double         5e-324;                   /* INFORMATIVE */
        static function get NaN() : Number               0.0 / 0.0;
        static function get NEGATIVE_INFINITY() : Number -1.0 / 0.0;
        static function get POSITIVE_INFINITY() : Number 1.0 / 0.0;

        /* E262-4 draft */
        meta static function convert(x) : Number
            Number(x);

        /* E262-3 15.7.1.1: The Number Constructor Called as a Function */
        meta static function invoke(x=0.0d) 
            x is double ? x : double(x)

        /* E262-3 15.7.2.1: The Number constructor */
        function Number(x=0.0d) 
            magic::bindDouble(this, x);

        prototype function toString(radix = 10)
            this.toString(radix);

        override intrinsic function toString(radix = 10) : string
	    private::toString(radix);

	private function toString(radix = 10) : string
            ToDouble(this).toString(radix);
        
        prototype function toLocaleString()
            this.toLocaleString();

        override intrinsic function toLocaleString() : string
            value.toLocaleString();

	private function toLocaleString() : string
	    ToDouble(this).toLocaleString();

        prototype function valueOf()
            this.valueOf();

        override intrinsic function valueOf() : Numeric
            private::valueOf();

	private function valueOf() : Numeric
	    ToNumeric(this);

        prototype function toFixed(fractionDigits)
            this.toFixed(fractionDigits);

        intrinsic function toFixed(fractionDigits:double) : string 
            private::toFixed(fracitonDigits);

        private function toFixed(fractionDigits:double) : string 
            ToDouble(this).toFixed(fractionDigits);

        prototype function toExponential(fractionDigits)
            this.toExponential(fractionDigits);

        intrinsic function toExponential(fractionDigits:double) : string
            private::toExponential(fractionDigits);

        private function toExponential(fractionDigits:double) : string
            ToDouble(this).toExponential(fractionDigits);

        prototype function toPrecision(precision)
            this.toPrecision(precision);

        intrinsic function toPrecision(precision:double) : string
            private::toPrecision(precision);

        private function toPrecision(precision:double) : string
            ToDouble(this).toPrecision(precision);
    }
}
