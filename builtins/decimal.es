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

    final class decimal!
    {       
	// FIXME
        public static const MAX_VALUE         = 1.7976931348623157e+308m;
        public static const MIN_VALUE         = 5e-324m;
        public static const NaN               = 0.0m / 0.0m;  // ???
        public static const NEGATIVE_INFINITY = -1.0m / 0.0m; // ???
        public static const POSITIVE_INFINITY = 1.0m / 0.0m;  // ???

        /* E262-4 draft */
        static function to(x : Numeric) : decimal
            x is decimal ? x : ToDecimal(x);

        /* E262-3 15.7.1.1: The decimal Constructor Called as a Function */
        intrinsic static function call(value)
            value === undefined ? 0m : ToDecimal(value);

        /* E262-3 15.7.2.1: The decimal constructor */
        public function decimal(value) {
            magic::setValue(this, ToDecimal(value));
        }

        /* E262-3 15.7.4.2: decimal.prototype.toString */
        prototype function toString(this:decimal, radix:Number)
            this.toString(radix);

        intrinsic function toString(radix:Number = 10) : String! {
            if (radix === 10 || radix === undefined)
                return ToString(magic::getValue(this));
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            }
            else
                throw new TypeError("Invalid radix argument to Number.toString");
        }
        
        /* E262-3 15.7.4.3: decimal.prototype.toLocaleString() */
        prototype function toLocaleString(this:decimal)
            this.toLocaleString();

        /* INFORMATIVE */
        intrinsic function toLocaleString() : String!
            toString();

        /* E262-3 15.7.4.4: decimal.prototype.valueOf */
        prototype function valueOf(this:decimal)
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:decimal, fractionDigits)
            this.toFixed(ToNumber(fractionDigits));

        intrinsic native function toFixed(fractionDigits:Number) : String!; // FIXME

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:decimal, fractionDigits)
            this.toExponential(ToDecimal(fractionDigits));

        intrinsic native function toExponential(fractionDigits:Number):String; // FIXME

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:decimal, precision)
            this.toPrecision(ToDecimal(precision));

        intrinsic native function toPrecision(precision:Number) : String!; // FIXME
    }
}
