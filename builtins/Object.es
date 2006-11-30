package
{
	dynamic class Object 
	{		
		// 15.2.1.1 Object ( [ value ] )
		// the Object constructor called as a function
		static intrinsic function call(value)
		{
			return @todo;
		}

		// 15.2.2.1 new Object ( [ value ] )
		function Object(value)
		{
			@todo;
			// conceptually:
			// this.[[Prototype]] = myClass.prototype;
			// this.[[Class]] = myClass.prototype.[[Class]];
		}
		
		// 15.2.3.1 Object.prototype
		static const prototype = { };
		
		// 15.2.4 Properties of the Object Prototype Object
   		prototype.[[Prototype]] = null;
   		prototype.[[Class]] = "Object";
		
		// 15.2.4.1 Object.prototype.constructor
   		prototype.constructor = Object;

		// 15.2.4.2 Object.prototype.toString ( )
		private static function _toString(o):String
		{
			return "[object " + o.[[Class]] + "]";
		}
		prototype.toString = function()
		{
			return _toString(this);
		}
		public function toString():String
		{
			return _toString(this);
		}

		// 15.2.4.3 Object.prototype.toLocaleString ( )
		prototype.toLocaleString = function()
		{
			return toString();
		}
		function toLocaleString():String
		{
			return toString();
		}

		// 15.2.4.4 Object.prototype.valueOf ( )
		prototype.valueOf = function()
		{
			return this;
		}
		function valueOf()
		{
			return this;
		}

		// 15.2.4.5 Object.prototype.hasOwnProperty (V)
		private static function _hasOwnProperty(o, V):Boolean
		{
			return @todo;
		}
		prototype.hasOwnProperty = function(V)
		{
			return _hasOwnProperty(this, V);
		}
		function hasOwnProperty(V)
		{
			return _hasOwnProperty(this, V);
		}
		
		// 15.2.4.6 Object.prototype.isPrototypeOf (V)
		private static function _isPrototypeOf(o, V):Boolean
		{
			if (!(V is Object))
				return false;
			V = V.prototype;
			while (V != null)
			{
				if (o == V)
					return true;
				V = V.prototype;
			}
			return false;
		}
		prototype.isPrototypeOf = function(V)
		{
			return _isPrototypeOf(this, V);
		}
		function isPrototypeOf(V)
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
		function propertyIsEnumerable(V)
		{
			return _propertyIsEnumerable(this, V);
		}

       // ECMA-262 doesn't expose __proto__ and it's internally readonly
		private const [[Prototype]];
		private const [[Class]];

       // bound {DE,DD} accessors
		SpiderMonkey function get __proto__() { return [[Prototype]]; }
		SpiderMonkey function set __proto__(o) { [[Prototype]] = o; }   // check for a cycle, throw Error if so
		
		// private helper function, used to mark all prototype functions as {DE}
		protected static native function _setPropertyIsEnumerable(o:Object, V:String, enumerable:Boolean):void;
		protected static function _dontEnumPrototype(proto:Object):void
		{
			for (var name:String in proto)
			{
				_setPropertyIsEnumerable(proto, name, false);
			}
		}

		// mark all prototype functions as {DE}
		_dontEnumPrototype(prototype);

	} // class
} // package
