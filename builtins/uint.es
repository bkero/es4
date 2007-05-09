/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "uint" object
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

    intrinsic final class uint! extends Number
    {       
        static const MAX_VALUE : uint = 0xFFFFFFFFu;
        static const MIN_VALUE : uint = 0;

        /* E262-4 draft */
        meta static function convert(x : Numeric)
            uint(x);

        /* E262-4 draft: The uint Constructor Called as a Function */
        meta static function invoke(x)
            x is uint ? x : new uint(x);

        /* E262-4 draft: The uint constructor */
        function uint(x) : super(x)
            magic::bindUInt(this, x);

        /* E262-4 draft: uint.prototype.toString */
        prototype function toString(this:uint, radix)
            this.toString(radix);

        override intrinsic function toString() {
            var radix = 10  /* FIXME */
            if (radix === 10 || radix === undefined)
                return ToString(this);
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
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
    }
}
