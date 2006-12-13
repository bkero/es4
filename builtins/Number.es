package
{
	dynamic class Number extends Object
	{		
		// 15.7.1 The Number Constructor Called as a Function
		static intrinsic function call(value)
		{
			if (value === void(0))
				return 0;
			return @todo;
		}

		// 15.7.2.1 new Number ( [ value ] )
		function Number(value)
		{
			// @todo
			// conceptually:
			// this.[[Value]] = Number(value);
		}

		// 15.7.3 Properties of the Number Constructor
		static const length = 1;
		
		// 15.7.3.1 Number.prototype
		static const prototype = { };
	
		// note: these are const in E3, even though global.undefined, etc, are vars.
		static const MAX_VALUE = @todo;	
		static const MIN_VALUE = @todo;	
		static const NaN = NaN;	
		static const NEGATIVE_INFINITY = -Infinity;	
		static const POSITIVE_INFINITY = Infinity;	

		// 15.7.4 Properties of the Number Prototype Object
   		prototype.[[Prototype]] = prototype;
   		prototype.[[Class]] = "Number";

		// 15.7.4.1 Number.prototype.constructor
		prototype.constructor = Number;

		// 15.7.4.2 Number.prototype.toString (radix)
		// @todo, ES4 currently doesn't allow overrides to add parameters....
		private static function _toString(o:Number, radix:Number):String
		{
			return @todo;
		}
		prototype.toString = function(this:Number, radix:Number)
		{
			if (radix == void(0))
				radix = 10;
			return _toString(this, radix);
		}
		ECMA4 function toString(radix:Number = 10):String
		{
			return _toString(this, radix);
		}
		
		// 15.7.4.3 Number.prototype.toLocaleString()
		prototype.toLocaleString = function(this:Number)
		{
			return this.toString();
		}
		ECMA4 function toLocaleString():String
		{
			return this.toString();
		}

		// 15.7.4.4 Number.prototype.valueOf ( )
		prototype.valueOf = function(this:Number)
		{
			return this;
		}
		ECMA4 function valueOf():Object
		{
			return this;
		}

		// 15.7.4.5 Number.prototype.toFixed (fractionDigits)
		prototype.toFixed = function(this:Number, fractionDigits)
		{
			return _toFixed(this, fractionDigits);
		}
		prototype.toFixed.length = 1;
		ECMA4 function toFixed(fractionDigits:Number):String
		{
			return _toFixed(this, fractionDigits);
		}
		private static function _toFixed(o:Number, fractionDigits):String
		{
			return @todo;
		}

		// 15.7.4.6 Number.prototype.toExponential (fractionDigits)
		prototype.toExponential = function(this:Number, fractionDigits)
		{
			return _toExponential(this, fractionDigits);
		}
		prototype.toExponential.length = 1;
		ECMA4 function toExponential(fractionDigits:Number):String
		{
			return _toExponential(this, fractionDigits);
		}
		private static function _toExponential(o:Number, fractionDigits):String
		{
			return @todo;
		}

		// 15.7.4.7 Number.prototype.toPrecision (precision)
		prototype.toPrecision = function(this:Number, precision)
		{
			return _toPrecision(this, precision);
		}
		prototype.toPrecision.length = 1;
		ECMA4 function toPrecision(precision:Number):String
		{
			return _toPrecision(this, precision);
		}
		private static function _toPrecision(o:Number, precision):String
		{
			return @todo;
		}

		// mark all prototype functions as {DE}
		_dontEnum(prototype);

	} // class
} // package
