/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Number" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 *
 * The committee decided at the January 2007 meeting at Mozilla that
 * "Number" is a heavyweight wrapper (non-final, dynamic) for a
 * "double" value.
 */

package
{
    use namespace intrinsic;
    use strict;

    dynamic class Number!
    {
        public static const MAX_VALUE : double         = double.MAX_VALUE;
        public static const MIN_VALUE : double         = double.MIN_VALUE;
        public static const NaN : double               = double.NaN;
        public static const NEGATIVE_INFINITY : double = double.NEGATIVE_INFINITY;
        public static const POSITIVE_INFINITY : double = double.POSITIVE_INFINITY;

        const value : double;

        /* E262-4 draft */
        public static function to(x : Numeric) : Number
            x is Number ? x : new Number(ToDouble(x));

        /* E262-3 15.7.1.1: The Number Constructor Called as a Function */
        intrinsic static function invoke(value) 
            value === undefined ? 0.0 : new Number(value);

        /* E262-3 15.7.2.1: The Number constructor */
        public function Number(value) 
            value = ToDouble(value);

        prototype function toString(radix = 10)
            this.toString(radix);

        intrinsic function toString(radix = 10) : String!
            value.toString(radix);
        
        prototype function toLocaleString()
            this.toLocaleString();

        intrinsic function toLocaleString() : String!
            value.toLocaleString();

        prototype function valueOf()
            this.valueOf();

        intrinsic function valueOf() : Object!
            value.valueOf();

        prototype function toFixed(fractionDigits)
            this.toFixed(fractionDigits);

        intrinsic function toFixed(fractionDigits:double) : String! 
            value.toFixed(fractionDigits);

        prototype function toExponential(fractionDigits)
            this.toExponential(fractionDigits);

        intrinsic function toExponential(fractionDigits:double) : String
            value.toExponential(fractionDigits);

        prototype function toPrecision(precision)
            this.toPrecision(precision);

        intrinsic function toPrecision(precision:double) : String!
            value.toPrecision(precision);
    }
}
