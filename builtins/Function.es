/*
	Attempt at modeling ECMA-262 builtin classes using the new ECMA4 language.
	
	Note that these are intended to reflect ECMA-262 behavior, which may omit
	common-but-nonstandard extensions used in various implementations (e.g., SpiderMonkey).
	
	This also makes no attempt at efficiency of implementation, preferring clarity and
	simplicity instead.
*/

package
{
	public dynamic class Function extends Object
	{		
		// 15.3.1 The Function Constructor Called as a Function
		public static intrinsic function call(...args)
		{
			return _construct(args);
		}

		// 15.3.1.1 Function (p1, p2, … , pn, body)
		private static function _construct(args:Array):Function
		{
			return @todo, requires eval
		}
		public static intrinsic function construct(...args)
		{
			return _construct(args);
		}

		// 15.3.3.1 Function.prototype
		// { DontEnum, DontDelete, ReadOnly } 
		// @todo how to set DontEnum?
		public static function get prototype()
		{
			return intrinsic::get("prototype");
		}
		intrinsic::set("prototype", { prototype: Object.prototype });

		// 15.3.4.1 Function.prototype.constructor
		prototype.constructor = construct;

		// 15.3.4.2 Function.prototype.toString ( )
		private static function _toString(o):String
		{
			// implementation dependent, so this is acceptable
			return "[function]";
		}
		prototype.toString = function():String
		{
			var _this:Function = this;	// throw TypeError if not a Function
			return _toString(_this);
		}
		public function toString():String
		{
			return _toString(this);
		}
		
		// 15.3.4.3 Function.prototype.apply (thisArg, argArray)
		private static function _apply(o, thisArg:Function, argArray:Array)
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
		public function apply(thisArg:Function, argArray:Array):*
		{
			return _apply(this, thisArg, argArray);
		}

		// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
		private static function _call(o, args:Array)
		{
			return _apply(o, args[0], args.slice(1));
		}
		prototype.call = function(...args)
		{
			return _call(this, args);
		}
		prototype.call.length = 1;			// ECMA-262 says so
		public function call(...args:Array):*
		{
			return _call(this, args);
		}
		
		// 15.3.5 Properties of Function Instances
		// 15.3.5.1 length
		// { DontEnum, DontDelete, ReadOnly } 
		// @todo how to set DontEnum?
		// @todo how to set initial value?
		public function get length()
		{
			return intrinsic::get("length");
		}

		// 15.3.5.2 prototype
		// { DontDelete } 
		// @todo how to set initial value?
		public function get prototype()
		{
			return intrinsic::get("prototype");
		}
		public function set prototype(p)
		{
			intrinsic::set("prototype", p);
		}


	} // class
} // package
