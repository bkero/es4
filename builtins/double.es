/* -*- mode: java; indent-tabs-mode: nil -*- 
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
    use default namespace public;
    use namespace intrinsic;
    use strict;

    intrinsic final class double! extends Number
    {       
        static const MAX_VALUE : double         = 1.7976931348623157e+308;  /* INFORMATIVE */
        static const MIN_VALUE : double         = 5e-324;                   /* INFORMATIVE */
        static const NaN : double               = 0.0 / 0.0;
        static const NEGATIVE_INFINITY : double = -1.0 / 0.0;
        static const POSITIVE_INFINITY : double = 1.0 / 0.0;

        /* E262-4 draft */
        meta static function convert(x)
            double(x);

        /* E262-3 15.7.1.1: The double Constructor Called as a Function */
        meta static function invoke(x=0.0d)
            x is double ? x : magic::newDouble(x); 

        /* E262-3 15.7.2.1: The double constructor */
        function double(x=0.0d) : super(x)
        {
            // No need to magic::bindDouble a second time, 
            // since our super(x) call did it for us.
        }


        /* E262-3 15.7.4.2: double.prototype.toString */
        prototype function toString(radix = 10)
            this.toString(radix);

        override intrinsic function toString(radix = 10) : string 
            private::toString(radix);

        private function toString(radix) : string {
            if (radix === 10 || radix === undefined)
                return ToString(this);
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
        override intrinsic function toLocaleString() : string
            toString();

        /* E262-3 15.7.4.4: double.prototype.valueOf */
        prototype function valueOf(this:double)
            this.valueOf();

        override intrinsic function valueOf() : double
            this;

        /* E262-3 15.7.4.5 Number.prototype.toFixed */
        prototype function toFixed(this:double, fractionDigits)
            this.toFixed(ToDouble(fractionDigits));

        override intrinsic function toFixed(fractionDigits:double) : string {
            let f : double = ToInteger(fractionDigits);
            if (f < 0 || f > 20)
                throw new RangeError("fractionDigits out of range");
            let x : double = this;
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

        native intrinsic function toFixedStep10(x : double, f : int) : int;

        /* E262-3 15.7.4.6: Number.prototype.toExponential */
        prototype function toExponential(this:double, fractionDigits)
            this.toExponential(ToDouble(fractionDigits));

        /* E262-3 15.7.4.7: Number.prototype.toPrecision */
        prototype function toPrecision(this:double, precision)
            this.toPrecision(ToDouble(precision));

        // FIXME these are supposed to be native, but the parser has trouble
        // parsing "override intrinsic function native". No idea why.
        override intrinsic function toExponential(fractionDigits:double) : string ""; 
        override intrinsic function toPrecision(precision:double) : string "";
    }
}
