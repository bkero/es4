/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "int" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code
 *
 * Status: Complete; not reviewed; not tested.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use strict;

    intrinsic final class int! extends Number
    {       
        static const MAX_VALUE : int = 0x7FFFFFFFi;
        static const MIN_VALUE : int = -0x80000000i;

        /* E262-4 draft */
        meta static function convert(x : Numeric)
            int(x);

        /* E262-4 draft: The int Constructor Called as a Function */
        meta static function invoke(x)
            x is int ? x : new int(x);

        /* E262-4 draft: The int constructor */
        function int(x) : super(x)
            magic::bindInt(this, x);

        /* E262-4 draft: int.prototype.toString */
        prototype function toString(this:int, radix)
            this.toString(radix);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return ToString(this);
            else if (typeof radix === "number" && 
		     radix >= 2 && 
		     radix <= 36 && isIntegral(radix))
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            else
                throw new TypeError("Invalid radix argument to int.toString");
        }
        
        /* E262-4 draft: int.prototype.toLocaleString() */
        prototype function toLocaleString(this:int)
            this.toLocaleString();

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            toString();

        /* E262-4 draft: int.prototype.valueOf */
        prototype function valueOf(this:int)
            this.valueOf();

        override intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 int.prototype.toFixed */
        prototype function toFixed(this:int, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        override intrinsic function toFixed(fractionDigits:double) : string 
	    ToDouble(this).toFixed(fractionDigits);

        /* E262-4 draft: int.prototype.toExponential */
        prototype function toExponential(this:int, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        override intrinsic function toExponential(fractionDigits:double) : string
	    ToDouble(this).toExponential(fractionDigits);

        /* E262-4 draft: int.prototype.toPrecision */
        prototype function toPrecision(this:int, precision)
            this.toPrecision(ToDouble(precision));

        override intrinsic function toPrecision(precision:double) : string
	    ToDouble(this).toPrecision(precision);
    }
}
