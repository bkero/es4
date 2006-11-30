package
{
	dynamic class Function extends Object
	{		
		// 15.3.1 The Function Constructor Called as a Function
		static intrinsic function call(...args)
		{
			return _construct(args);
		}

		// 15.3.1.1 Function (p1, p2, … , pn, body)
		private static function _construct(args:Array):Function
		{
			return @todo;
		}
		function Function(...args)
		{
			return _construct(args);
		}

		// 15.3.3 Properties of the Function Constructor
		static const length = 1;
		
		// 15.3.3.1 Function.prototype
		static const prototype = { };

		// 15.3.4 Properties of the Function Prototype Object
   		prototype.[[Prototype]] = prototype;
   		prototype.[[Class]] = "Function";
		
		// 15.3.4.1 Function.prototype.constructor
		prototype.constructor = Function;

		// 15.3.4.2 Function.prototype.toString ( )
		private static function _toString(f:Function):String
		{
			// implementation dependent, so this is acceptable
			return "function Function() {}";
		}
		prototype.toString = function(this:Function):String
		{
			return _toString(this);
		}
		ECMA4 function toString():String
		{
			return _toString(this);
		}
		
		// 15.3.4.3 Function.prototype.apply (thisArg, argArray)
		private static function _apply(o, thisArg, argArray)
		{
			if (thisArg == null)
				thisArg = intrinsic::global;
			// @todo how to convert args into arg1, arg2.... etc
			if (argArray == null)
				return o.intrinsic::call(thisArg);
			else
				return o.intrinsic::call(thisArg, argArray...);
		}
		prototype.apply = function(thisArg, argArray)
		{
			return _apply(this, thisArg, argArray);
		}
		ECMA4 function apply(thisArg:Function, argArray:Array):*
		{
			return _apply(this, thisArg, argArray);
		}

		// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
		private static function _call(o, args:Array):*
		{
			return _apply(o, args[0], args.slice(1));
		}
		prototype.call = function(...args)
		{
			return _call(this, args);
		}
		prototype.call.length = 1;			// ECMA-262 says so
		ECMA4 function call(...args:Array):*
		{
			return _call(this, args);
		}
		
		// 15.3.5 Properties of Function Instances
		// 15.3.5.1 length
		const length;	// @todo -- how to set initial value

		// 15.3.5.2 prototype
		// { DontDelete } 
		var prototype;	// @todo -- how to set initial value

		// 15.3.5.3 [[HasInstance]] (V)
		function [[HasInstance]](V)		
		{
			// implements instanceof
			if (!(V is Object))
				return false;
			var O:Object = this.prototype;	// throws TypeError if not Object
			V = V.prototype;
			while (V != null)
			{
				if (O == V)
					return true;
				V = V.prototype;
			}
			return false;
		}

		// mark all prototype functions as {DE}
		_dontEnum(prototype);

	} // class
} // package
