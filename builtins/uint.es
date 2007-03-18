/* -*- indent-tabs-mode: nil -*- 
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
    use namespace intrinsic;
    use strict;

    namespace core;

    final class uint!
    {       
        static const MAX_VALUE : uint = 0xFFFFFFFFu;
        static const MIN_VALUE : uint = 0;

        /* E262-4 draft */
        core static function convert(x : Numeric)
            x is uint ? x : ToUint(x);

        /* E262-4 draft: The uint Constructor Called as a Function */
        core static function invoke(value)
            value === undefined ? 0u : ToUint(value);

        /* E262-4 draft: The uint constructor */
        public function uint(value) {
            magic::setValue(this, ToUint(value));
	}

        /* E262-4 draft: uint.prototype.toString */
        prototype function toString(this:uint, radix)
            this.toString(radix);

        override intrinsic function toString() {
            var radix = 10  /* FIXME */
            if (radix === 10 || radix === undefined)
                return ToString(magic::getValue(this));
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
            toString();

        /* E262-4 draft: uint.prototype.valueOf */
        prototype function valueOf(this:uint)
            this.valueOf();

        override intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 uint.prototype.toFixed */
        prototype function toFixed(this : uint, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        intrinsic function toFixed(fractionDigits : double) : string 
	    ToDouble(this).toFixed(fractionDigits);

        /* E262-4 draft: uint.prototype.toExponential */
        prototype function toExponential(this : uint, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        intrinsic function toExponential(fractionDigits : double) : string
	    ToDouble(this).toExponential(fractionDigits);

        /* E262-4 draft: uint.prototype.toPrecision */
        prototype function toPrecision(this : uint, precision)
            this.toPrecision(ToDouble(precision));

        intrinsic function toPrecision(precision : double) : string
	    ToDouble(this).toPrecision(precision);
    }
}
