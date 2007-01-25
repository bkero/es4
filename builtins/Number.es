/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Number" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 *
 * "Number" is for backwards compatibility, most of the functionality
 * is in "double".
 */

package
{
    use namespace intrinsic;
    use strict;

    type Numeric = (int, uint, double, decimal);

    final class Number!
    {
        public static const MAX_VALUE : double        = 1.7976931348623157e+308;   
        public static const MIN_VALUE : double        = 5e-324;    
        public static const NaN : double              = 0.0/0.0;   
        public static const NEGATIVE_INFINITY : double = -1.0/0.0;
        public static const POSITIVE_INFINITY : double = 1.0/0.0;

        prototype = double.prototype;

        /* E262-3 15.7.1.1: The Number Constructor Called as a Function */
        intrinsic static function call(value) 
            value === undefined ? 0 : ToDouble(value);

        /* E262-3 15.7.2.1: The Number constructor */
        public function Number(value)
            new double(value);

        intrinsic function toString(radix:Number = 10) : String!
            magic::getValue(this).toRadix(radix);
        
        intrinsic function toLocaleString() : String!
            magic::getValue(this).toLocaleString();

        intrinsic function valueOf() : Object!
            magic::getValue(this).valueOf();

        intrinsic function toFixed(fractionDigits:Number) : String! 
            magic::getValue(this).toFixed(fractionDigits);

        intrinsic function toExponential(fractionDigits:Number) : String
            magic::getValue(this).toExponential(fractionDigits);

        intrinsic function toPrecision(precision:Number) : String!
            magic::getValue(this).toPrecision(precision);
    }
}
