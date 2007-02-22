/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "double" object
 *
 * E262-3 15.7
 * E262-4 proposals:numbers
 * Tamarin code.
 *
 * Status: Incomplete.  
 *
 * TO DO:
 *  - toPrecision and toExponential currently punt to native code
 *    but are mostly expressible in ECMAScript.  toFixed shows how this
 *    might be done.
 */

package
{
    use namespace intrinsic;

    final class double!
    {       
        public static const MAX_VALUE : double         = 1.7976931348623157e+308;  /* INFORMATIVE */
        public static const MIN_VALUE : double         = 5e-324;                   /* INFORMATIVE */
        public static const NaN : double               = 0.0 / 0.0;
        public static const NEGATIVE_INFINITY : double = -1.0 / 0.0;
        public static const POSITIVE_INFINITY : double = 1.0 / 0.0;

        /* E262-4 draft */
        function to double(x : Numeric)
            x is double ? x : ToDouble(x);

        /* E262-3 15.7.1.1: The double Constructor Called as a Function */
        function call double(value)
            value === undefined ? 0d : ToDouble(value);

        /* E262-3 15.7.2.1: The double constructor */
        public function double(value) {
            magic::setValue(this, ToDouble(value));
        }

        /* E262-3 15.7.4.2: double.prototype.toString */
        prototype function toString(this:double, radix)
            this.toString(radix);

        intrinsic function toString(radix = 10) : string {
            if (radix === 10 || radix === undefined)
                return ToString(magic::getValue(this));
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            }
            else
                throw new TypeError("Invalid radix argument to double.toString");
        }
        
        /* E262-3 15.7.4.3: double.prototype.toLocaleString() */
        prototype function toLocaleString(this:double)
            this.toLocaleString();

        /* INFORMATIVE */
        intrinsic function toLocaleString() : string
            toString();

        /* E262-3 15.7.4.4: double.prototype.valueOf */
        prototype function valueOf(this:double)
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:double, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        intrinsic function toFixed(fractionDigits:double) : string {
            let f : double = ToInteger(fractionDigits);
            if (f < 0 || f > 20)
                throw new RangeError("fractionDigits out of range");
            let x : double = magic::getValue(this);
            if (isNaN(x))
                return "NaN";
            let s : string = "";
            if (x < 0) {
                s = "-";
                x = -x;
            }

            if (x >= Math.pow(10,21)) 
                return s + ToString(m);
            
            let n : double = toFixedStep10(x, f);
            let m : string = n == 0 ? "0" : ToString(n);
            if (f == 0)
                return s + m;
            let k : int = m.length;
            if (k <= f) {
                m = "00000000000000000000".substring(0,f+1-k) + m;
                k = f+1;
            }
            return "-" + m.substring(0,k-f) + "." + m.substring(k-f);
        }

        /* Step 10 of the toFixed algorithm in E262-3 15.7.4.5: return
           an integer n such that n / 10^f - x is as close to zero as
           possible.  If there are two such n, pick the larger. 

           x must be positive, f is in the range [0,20]. */

        native function toFixedStep10(x : double, f : int) : int;

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:double, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        intrinsic native function toExponential(fractionDigits:double) : string; // FIXME

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:double, precision)
            this.toPrecision(ToDouble(precision));

        intrinsic native function toPrecision(precision:double) : string; // FIXME
    }
}
