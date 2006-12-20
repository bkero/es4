/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Number" object
 * E262-3 15.7
 *
 * "Number" is for backwards compatibility, but its semantics are not
 * yet entirely pinned down.
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
	final class Number! extends Object
	{		
		static const MAX_VALUE         = 1.7976931348623157e+308;	
		static const MIN_VALUE         = 5e-324;	
		static const NaN               = 0.0/0.0;	
		static const NEGATIVE_INFINITY = -1.0/0.0;
		static const POSITIVE_INFINITY = 1.0/0.0;

		/* E262-3 15.7.1.1: The Number Constructor Called as a Function */
		static intrinsic function call(value) {
			if (value === intrinsic::undefined)
				return 0;
			return intrinsic::ToNumber(value);
		}

		/* E262-3 15.7.2.1: The Number constructor */
		function Number(value) {
			magic::setValue(this, intrinsic::ToNumber(value));
		}

		/* E262-3 15.7.4.2: Number.prototype.toString */
		prototype function toString(this:Number, radix:Number) {
			return this.intrinsic::toString(radix);
		}

		intrinsic function toString(radix:Number = 10) : String! {
			if (radix === 10 || radix === intrinsic::undefined)
				return intrinsic::ToString(magic::getValue(this));
			else if (typeof radix === "number" && radix >= 2 && radix <= 36 && intrinsic::isIntegral(radix)) {
				// FIXME
				throw new Error("Unimplemented: non-decimal radix");
			}
			else
				throw new TypeError("Invalid radix argument to Number.toString");
		}
		
		/* E262-3 15.7.4.3: Number.prototype.toLocaleString() */
		prototype function toLocaleString(this:Number) {
			return this.intrinsic::toLocaleString();
		}

		intrinsic function toLocaleString() : String! {
			return this.intrinsic::toString();
		}

		/* E262-3 15.7.4.4: Number.prototype.valueOf */
		prototype function valueOf(this:Number) {
			return this.intrinsic::valueOf();
		}

		intrinsic function valueOf() : Object! {
			return this;
		}

		/* E262-3 15.7.4.5 Number.prototype.toFixed */
		prototype function toFixed(this:Number, fractionDigits) {
			return this.intrinsic::toFixed(Number(fractionDigits));
		}

		intrinsic function toFixed(fractionDigits:Number) : String! {
			let f : Number = intrinsic::ToInteger(fractionDigits);
			if (f < 0 || f > 20)
				throw new RangeError("fractionDigits out of range");
			let x : Number = magic::getValue(this);
			if (intrinsic::isNaN(x))
				return "NaN";
			let s : String! = "";
			if (x < 0) {
				s = "-";
				x = -x;
			}

			if (x >= Math.intrinsic::pow(10,21)) 
				return s + intrinsic::ToString(m);
			
			let n : Number = toFixedStep10(x, f);
			let m : String! = n == 0 ? "0" : intrinsic::ToString(n);
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
		prototype function toExponential(this:Number, fractionDigits) {
			return this.intrinsic::toExponential(Number(fractionDigits));
		}

		intrinsic native function toExponential(fractionDigits:Number):String; // FIXME

		/* E262-3 15.7.4.7: Number.prototype.toPrecision */
		prototype function toPrecision(this:Number, precision) {
			return this.intrinsic::toPrecision(Number(precision));
		}

		intrinsic native function toPrecision(precision:Number) : String!; // FIXME
	}
}
