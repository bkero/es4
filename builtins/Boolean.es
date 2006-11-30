package
{
	dynamic class Boolean extends Object
	{		
		// 15.3.1 The Boolean Constructor Called as a Function
		static intrinsic function call(value)
		{
			return value ? true : false;
		}

		// 15.6.2.1 new Boolean (value)
		function Boolean(value)
		{
			// @todo
			// conceptually:
			// this.[[Value]] = Boolean(value);
		}

		// 15.6.3 Properties of the Boolean Constructor
		static const length = 1;
		
		// 15.6.3.1 Boolean.prototype
		static const prototype = { };

		// 15.6.4 Properties of the Boolean Prototype Object
   		prototype.[[Prototype]] = prototype;
   		prototype.[[Class]] = "Boolean";
		
		// 15.6.4.1 Boolean.prototype.constructor
		prototype.constructor = Boolean;

		// 15.6.4.2 Boolean.prototype.toString ( )
		private static function _toString(f:Boolean):String
		{
			return this.[[Value]] ? "true" : "false";
		}
		prototype.toString = function(this:Boolean)
		{
			return _toString(this);
		}
		ECMA4 function toString():String
		{
			return _toString(this);
		}
		
		// 15.6.4.3 Boolean.prototype.valueOf ( )
		prototype.valueOf = function(this:Boolean)
		{
			return this;
		}
		ECMA4 function valueOf():Object
		{
			return this;
		}

		// mark all prototype functions as {DE}
		_dontEnum(prototype);

	} // class
} // package
