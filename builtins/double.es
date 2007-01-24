/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "double" object
 * E262-3 15.7
 * E262-4 proposals:numbers
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
        static const MAX_VALUE         = 1.7976931348623157e+308;   /* INFORMATIVE */
        static const MIN_VALUE         = 5e-324;                    /* INFORMATIVE */
        static const NaN               = 0.0/0.0;
        static const NEGATIVE_INFINITY = -1.0/0.0;
        static const POSITIVE_INFINITY = 1.0/0.0;

        /* E262-3 15.7.1.1: The double Constructor Called as a Function */
        static intrinsic function call(value)
            value === undefined ? 0 ToDouble(value);

        /* E262-3 15.7.2.1: The double constructor */
        function double(value) {
            magic::setValue(this, ToDouble(value));
        }

        /* E262-3 15.7.4.2: double.prototype.toString */
        prototype function toString(this:double, radix:Number)
            this.toString(radix);

        intrinsic function toString(radix:Number = 10) : String! {
            if (radix === 10 || radix === undefined)
                return ToString(magic::getValue(this));
            else if (typeof radix === "number" && radix >= 2 && radix <= 36 && intrinsic::isIntegral(radix)) {
                // FIXME
                throw new Error("Unimplemented: non-decimal radix");
            }
            else
                throw new TypeError("Invalid radix argument to Number.toString");
        }
        
        /* E262-3 15.7.4.3: double.prototype.toLocaleString() */
        prototype function toLocaleString(this:double)
            this.toLocaleString();

        /* INFORMATIVE */
        intrinsic function toLocaleString() : String!
            toString();

        /* E262-3 15.7.4.4: double.prototype.valueOf */
        prototype function valueOf(this:double)
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:double, fractionDigits)
            this.toFixed(ToNumber(fractionDigits));

        intrinsic function toFixed(fractionDigits:Number) : String! {
            let f : Number = ToInteger(fractionDigits);
            if (f < 0 || f > 20)
                throw new RangeError("fractionDigits out of range");
            let x : Number = magic::getValue(this);
            if (isNaN(x))
                return "NaN";
            let s : String! = "";
            if (x < 0) {
                s = "-";
                x = -x;
            }

            if (x >= Math.pow(10,21)) 
                return s + ToString(m);
            
            let n : Number = toFixedStep10(x, f);
            let m : String! = n == 0 ? "0" : ToString(n);
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

        private native function toFixedStep10(x : double, f : int) : int;

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:double, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        public native intrinsic function toExponential(fractionDigits:Number):String; // FIXME

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:double, precision)
            this.toPrecision(ToDouble(precision));

        public intrinsic native function toPrecision(precision:Number) : String!; // FIXME
    }
}
