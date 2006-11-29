/*
	Attempt at modeling ECMA-262 builtin classes using the new ECMA4 language.
	
	Note that these are intended to reflect ECMA-262 behavior, which may omit
	common-but-nonstandard extensions used in various implementations (e.g., SpiderMonkey).
	
	This also makes no attempt at efficiency of implementation, preferring clarity and
	simplicity instead.
*/

package
{
		// @todo -- class name? oops
	public dynamic class _Object extends Object
	{		
		// 15.2.1.1 Object ( [ value ] )
		// the Object constructor called as a function
		public static intrinsic function call(value)
		{
			if (value === null || value === void 0)
			{
				return new _Object;
			}
			return _Object(value);
		}

		// 15.2.2.1 new Object ( [ value ] )
		public static intrinsic function construct(value)
		{
			if (value === null || value === void 0)
			{
				return new _Object;
			}
			return _Object(value);
		}
		
		// 15.2.3.1 Object.prototype
		// { DontEnum, DontDelete, ReadOnly } 
		// @todo how to set DontEnum?
		public static function get prototype()
		{
			return intrinsic::get("prototype");
		}

		// 15.2.4.1 Object.prototype.constructor
		intrinsic::set("prototype", { constructor: _Object.construct });

		// 15.2.4.2 Object.prototype.toString ( )
		private static function _toString(o):String
		{
			// @todo 
			return "[object " + o._getClassName() + "]";
		}
		prototype.toString = function():String
		{
			return _toString(this);
		}
		public function toString():String
		{
			return _toString(this);
		}

		// 15.2.4.3 Object.prototype.toLocaleString ( )
		prototype.toLocaleString = function():String
		{
			return toString();
		}
		public function toLocaleString():String
		{
			return toString();
		}

		// 15.2.4.4 Object.prototype.valueOf ( )
		prototype.valueOf = function()
		{
			return this;
		}
		public function valueOf()
		{
			return this;
		}

		// 15.2.4.5 Object.prototype.hasOwnProperty (V)
		private static function _hasOwnProperty(o, V):Boolean
		{
			// @todo 
			return @todo;
		}
		prototype.hasOwnProperty = function(V)
		{
			return _hasOwnProperty(this, V);
		}
		public function hasOwnProperty(V)
		{
			return _hasOwnProperty(this, V);
		}
		
		// 15.2.4.6 Object.prototype.isPrototypeOf (V)
		private static function _isPrototypeOf(o, V):Boolean
		{
			if (!(V is _Object))
				return false;
			V = V.intrinsic::get("prototype");
			while (V != null)
			{
				if (o == V)
					return true;
				V = V.intrinsic::get("prototype");
			}
			return false;
		}
		prototype.isPrototypeOf = function(V)
		{
			return _isPrototypeOf(this, V);
		}
		public function isPrototypeOf(V)
		{
			return _isPrototypeOf(this, V);
		}
		
		// 15.2.4.7 Object.prototype.propertyIsEnumerable (V)
		private static function _propertyIsEnumerable(o, V):Boolean
		{
			return @todo;
		}
		prototype.propertyIsEnumerable = function(V)
		{
			return _propertyIsEnumerable(this, V);
		}
		public function propertyIsEnumerable(V)
		{
			return _propertyIsEnumerable(this, V);
		}

	} // class
} // package
