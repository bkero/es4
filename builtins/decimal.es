/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "decimal" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code.
 *
 * Status: Incomplete (toExponential, toPrecision, toFixed; constants); not reviewed; not tested.
 */

package
{
    use namespace intrinsic;

    namespace core;

    final class decimal!
    {       
	// FIXME
        public static const MAX_VALUE         = 1.7976931348623157e+308m;
        public static const MIN_VALUE         = 5e-324m;
        public static const NaN               = 0.0m / 0.0m;  // ???
        public static const NEGATIVE_INFINITY = -1.0m / 0.0m; // ???
        public static const POSITIVE_INFINITY = 1.0m / 0.0m;  // ???

        /* E262-4 draft */
        core static function convert(x : Numeric)
            x is decimal ? x : ToDecimal(x);

        /* E262-3 15.7.1.1: The decimal Constructor Called as a Function */
        core static function invoke(value)
            value === undefined ? 0m : ToDecimal(value);

        /* E262-3 15.7.2.1: The decimal constructor */
        public function decimal(value) {
            magic::setValue(this, ToDecimal(value));
        }

        /* E262-3 15.7.4.2: decimal.prototype.toString */
        prototype function toString(this:decimal, radix)
            this.toString(radix);

        override intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return ToString(magic::getValue(this));
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            }
            else
                throw new TypeError("Invalid radix argument to decimal.toString");
        }
        
        /* E262-3 15.7.4.3: decimal.prototype.toLocaleString() */
        prototype function toLocaleString(this:decimal)
            this.toLocaleString();

        /* INFORMATIVE */
        intrinsic function toLocaleString() : string
            toString();

        /* E262-3 15.7.4.4: decimal.prototype.valueOf */
        prototype function valueOf(this:decimal)
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:decimal, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        intrinsic native function toFixed(fractionDigits:double) : string; // FIXME

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:decimal, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        intrinsic native function toExponential(fractionDigits:double) : string; // FIXME

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:decimal, precision)
            this.toPrecision(ToDouble(precision));

        intrinsic native function toPrecision(precision:double) : string; // FIXME
    }
}
