/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Number" object
 * E262-3 15.7
 *
 * Status: toPrecision, toFixed, and toExponential currently punt to native
 * code but are mostly expressible in ECMAScript.
 */
package
{
	dynamic class Number extends Object
	{		
		/* E262-3 15.7.1.1: The Number Constructor Called as a Function */
		static intrinsic function call(value)
		{
			if (value === intrinsic::undefined)
				return 0;
			return intrinsic::ToNumber(value);
		}

		/* E262-3 15.7.2.1: The Number constructor */
		function Number(value)
		{
			magic::setValue(this, intrinsic::ToNumber(value));
		}

		/* Note: these are const in E3, even though global.undefined,
		   etc, are vars  */
		static const MAX_VALUE = 1.7976931348623157e+308;	
		static const MIN_VALUE = 5e-324;	
		static const NaN = 0.0/0.0;	
		static const NEGATIVE_INFINITY = -1.0/0.0;
		static const POSITIVE_INFINITY = 1.0/0.0;

		/* E262-3 15.7.4.2: Number.prototype.toString */
		Number.prototype.toString = function(this:Number, radix:Number)
		{
			return this.intrinsic::toString(radix);
		}

		public intrinsic function toString(radix:Number = 10):String
		{
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
		Number.prototype.toLocaleString = function(this:Number)
		{
			return this.intrinsic::toLocaleString();
		}

		public intrinsic function toLocaleString():String
		{
			return this.intrinsic::toString();
		}

		/* E262-3 15.7.4.4: Number.prototype.valueOf */
		Number.prototype.valueOf = function(this:Number)
		{
			return this;
		}

		public intrinsic function valueOf():Object
		{
			return this;
		}

		/* E262-3 15.7.4.5 Number.prototype.toFixed */
		Number.prototype.toFixed = function(this:Number, fractionDigits)
		{
			return this.intrinsic::toFixed(fractionDigits);
		}

		public intrinsic function toFixed(fractionDigits:Number):String {
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

		native function toFixedStep10(x : double, f : int) : int;

		/* E262-3 15.7.4.6: Number.prototype.toExponential */
		Number.prototype.toExponential = function(this:Number, fractionDigits)
		{
			return this.intrinsic::toExponential(fractionDigits);
		}

		public native intrinsic function toExponential(fractionDigits:Number):String;

		/* E262-3 15.7.4.7: Number.prototype.toPrecision */
		Number.prototype.toPrecision = function(this:Number, precision)
		{
			return this.intrinsic::toPrecision(precision);
		}

		public intrinsic native function toPrecision(precision:Number) : String!;

		Intrinsic.setPropertyIsEnumerable(Number.prototype, "constructor", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "toString", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "toLocaleString", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "valueOf", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "toFixed", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "toExponential", false);
		Intrinsic.setPropertyIsEnumerable(Number.prototype, "toPrecision", false);
	}
}
