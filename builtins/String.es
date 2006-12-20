package
{
	// @todo: String is final in AS3 but dynamic in E262. 
	// Making it final offers useful runtime optimizations.
	dynamic class String extends Object
	{		
		// 15.5.1 The String Constructor Called as a Function
		// 15.5.1.1 String ( [ value ] )
		static intrinsic function call(value)
		{
			if (value == void(0))
				return "";
			return value.toString();
		}

		// 15.5.2 The String Constructor
		// 15.5.2.1 new String ( [ value ] )
		function String(value)
		{
			// @todo
			// conceptually:
			// this.[[Value]] = (value == void(0) ? "" : value.toString());
			// this.length = this.[[Value]].length;
		}
		
		static const length:int = 1;	// ECMA-262 says length is 1, but doesn't specify attribs

		// 15.5.3 Properties of the String Constructor
		// 15.5.3.1 String.prototype
		static const prototype = new String;

		// 15.5.3.2 String.fromCharCode ( [ char0 [ , char1 [ , … ] ] ] )
		static function fromCharCode(...args:Array):Number
		{
			return _fromCharCode(args);
		}
		fromCharCode.length = 1;	// ECMA-262 says length is 1
		ECMA4 static function fromCharCode(...args:Array):Number
		{
			return _fromCharCode(args);
		}

		private static function _fromCharCode(args:Array):Number
		{
			var s:String = "";
			for (var i:int = 0; i < args.length; ++i)
				s += _fromOneCharCode(args[i]);
			return s;
		}

		// 15.5.4 Properties of the String Prototype Object
   		prototype.magic::proto = prototype;
   		prototype.magic::className = "String";
		prototype.length = 0;
		
		// 15.5.4.1 String.prototype.constructor
		prototype.constructor = String;
		
		// 15.5.4.2 String.prototype.toString ( )
		prototype.toString = function(this:String)
		{
			return this;
		}
		ECMA4 function toString():String
		{
			return this;
		}
		
		// 15.5.4.3 String.prototype.valueOf ( )
		prototype.valueOf = function(this:String)
		{
			return this;
		}
		ECMA4 function valueOf():Object
		{
			return this;
		}

		// 15.5.4.4 String.prototype.charAt (pos)
		prototype.charAt = function(pos)
		{
			return _charAt(this, pos);
		}
		ECMA4 function charAt(pos:Number = 0):String
		{
			return _charAt(this, pos);
		}
		private static function _charAt(o, pos):String
		{
			var s:String = String(o);
			var ipos:int = int(pos);
			if (ipos < 0 || ipos >= s.length)
				return "";
			return _fromOneCharCode(_getOneCharCodeAt(s, ipos));
		}

		// 15.5.4.5 String.prototype.charCodeAt (pos)
		prototype.charCodeAt = function(pos)
		{
			return _charCodeAt(this, pos);
		}
		ECMA4 function charCodeAt(pos:Number = 0):String
		{
			return _charCodeAt(this, pos);
		}
		private static function _charCodeAt(o, pos):Number
		{
			var s:String = String(o);
			var ipos:int = int(pos);
			if (ipos < 0 || ipos >= s.length)
				return NaN;
			return _getOneCharCodeAt(s, ipos);
		}

		// 15.5.4.6 String.prototype.concat ( [ string1 [ , string2 [ , … ] ] ] )
		prototype.concat = function(...args)
		{
			return _concat(this, args);
		}
		prototype.concat.length = 1;	// ECMA-262 says length is 1
		ECMA4 function concat(...args:Array):String
		{
			return _concat(this, args);
		}
		private static function _concat(o, args:Array):String
		{
			var s:String = String(o);
			var argslen:uint = uint(args.length);
			for (var i:uint = 0; i < olen; i++)
			{
				s = s + String(args[i]);
			}
			return s;
		}

		// 15.5.4.7 String.prototype.indexOf (searchString, position)
		prototype.indexOf = function(searchString, position)
		{
			return _indexOf(this, searchString, position);
		}
		prototype.indexOf.length = 1;	// ECMA-262 says length is 1
		ECMA4 function indexOf(searchString:String = void(0), position:Number = 0):Number
		{
			return _indexOf(this, searchString, position);
		}
		private static function _indexOf(o, searchString, position):Number
		{
			var s:String = String(o);
			var ss:String = String(searchString);	// undefined -> ""
			var p:int = int(position);				// undefined -> 0
			var slen:int = int(s.length);
			var tmp:int = Math.min(Math.max(p, 0), slen);
			var sslen:int = int(ss.length);
			return @todo;
		}

		// 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
		prototype.lastIndexOf = function(searchString, position)
		{
			return _lastIndexOf(this, searchString, position);
		}
		prototype.lastIndexOf.length = 1;	// ECMA-262 says length is 1
		ECMA4 function lastIndexOf(searchString:String = void(0), position:Number = Infinity):Number
		{
			return _lastIndexOf(this, searchString, position);
		}
		private static function _lastIndexOf(o, searchString, position):Number
		{
			var s:String = String(o);
			var ss:String = String(searchString);	// undefined -> ""
			var p:Number = Number(position);		// undefined -> NaN
			if (p == NaN)
				p = Infinity;
			else
				p = int(p);
			var slen:int = int(s.length);
			var tmp:int = Math.min(Math.max(p, 0), slen);
			var sslen:int = int(ss.length);
			return @todo;
		}

		// 15.5.4.9 String.prototype.localeCompare (that)
		prototype.localeCompare = function(that)
		{
			return _localeCompare(this, that);
		}
		ECMA4 function localeCompare(that = void(0)):Number
		{
			return _localeCompare(this, that);  
		}
		private static function _localeCompare(o, that):Number
		{
			var sthis:String = String(o);		// undefined -> ""
			var sthat:String = String(that);	// undefined -> ""
			// this is implementation-dependent, thus this is an acceptable
			// (but hardly ideal) implementation.
			var i:Number = 0;
			var j:Number = 0;
			var ic:Number = 0;
			var jc:Number = 0;
			while (ic == jc && (i < Number(sthis.length) || j < Number(sthat.length)))
			{
				ic = sthis.charCodeAt(i++);
				if (ic == NaN) 
				{
					ic = 0;
				}
				jc = sthat.charCodeAt(j++);
				if (jc == NaN) 
				{
					jc = 0;
				}
			}
			return (ic - jc);
		}

		// 15.5.4.10 String.prototype.match (regexp)
		prototype.match = function(regexp)
		{
			return _match(this, regexp);
		}
		ECMA4 function match(regexp = void(0)):Array
		{
			return _match(this, regexp);  
		}
		// regexp can be a RegEx or is coerced to a string (and then RegEx constructor is called)
		private static function _match(o, regexp):Array
		{
			var sthis:String = String(o);
			var re:RegExp;
			if (regexp is RegExp)
			{
				re = RegExp(re);
			}
			else
			{
				re = new RegExp(String(regexp));
			}
			if (re.global)
			{
				var result:Array = new Array();
				var oldLastIndex = re.lastIndex;
				re.lastIndex = 0;
				var matchArray;
				while ((matchArray = re.exec(sthis)) != null)
				{
					result.push(matchArray[0]);
				}
				if (re.lastIndex == oldLastIndex)
					++re.lastIndex;
				return result;
			}
			else
			{
				return re.exec(sthis);
			}
		}

		// 15.5.4.11 String.prototype.replace (searchValue, replaceValue)
		prototype.replace = function(searchValue, replaceValue)
		{
			return _replace(this, searchValue, replaceValue);
		}
		ECMA4 function replace(searchValue:String, replaceValue:String):String
		{
			return _replace(this, searchValue, replaceValue);  
		}
		private static function _replace(o, searchValue, replaceValue):String
		{
			return @todo;
		}
		
		// 15.5.4.12 String.prototype.search (regexp)
		prototype.search = function(regexp)
		{
			return _search(this, regexp);
		}
		ECMA4 function search(regexp:Object):int
		{
			return _search(this, regexp);  
		}
		private static function _search(o, regexp):int
		{
			return @todo;
		}

		// 15.5.4.13 String.prototype.slice (start, end)
		prototype.slice = function(A, B)
		{
			if (A == void(0))
				A = 0;
			if (B == void(0))
				B = Infinity;
			return _slice(this, Number(A), Number(B))
		}
		prototype.slice.length = 2;
		ECMA4 function slice(A:Number = 0, B:Number = Infinity):Array
		{
			return _slice(this, Number(A), Number(B))
		}
		private static function _slice(o, A:Number, B:Number):Array
		{
			return @todo;
		}
		
		// 15.5.4.14 String.prototype.split (separator, limit)
		prototype.split = function(separator, limit)
		{
			return _split(this, separator, limit)
		}
		// separator may be String or RegExp (or undefined)
		ECMA4 function split(separator:*, limit:Number):Array
		{
			return _split(this, separator, limit)
		}
		private static function _split(o, separator:*, limit:Number):Array
		{
			return @todo;
		}

		// 15.5.4.15 String.prototype.substring (start, end)
		prototype.substring = function(start, end)
		{
			return _substring(this, Number(start), Number(end));
		}
		ECMA4 function substring(start:Number = 0, end:Number = 0):String
		{
			return _substring(this, start, end);
		}
		private static function _substring(o, start:Number, end:Number):String
		{
			return @todo;
		}

		// 15.5.4.16 String.prototype.toLowerCase ( )
		prototype.toLowerCase = function()
		{
			return _toLowerCase(this);
		}
		ECMA4 function toLowerCase():String
		{
			return _toLowerCase(this);
		}
		private static function _toLowerCase(o):String
		{
			return @todo;
		}

		// 15.5.4.17 String.prototype.toLocaleLowerCase ( )
		prototype.toLocaleLowerCase = function()
		{
			return _toLocaleLowerCase(this);
		}
		ECMA4 function toLocaleLowerCase():String
		{
			return _toLocaleLowerCase(this);
		}
		private static function _toLocaleLowerCase(o):String
		{
			return @todo;
		}

		// 15.5.4.18 String.prototype.toUpperCase ( )
		prototype.toUpperCase = function()
		{
			return _toUpperCase(this);
		}
		ECMA4 function toUpperCase():String
		{
			return _toUpperCase(this);
		}
		private static function _toUpperCase(o):String
		{
			return @todo;
		}

		// 15.5.4.18 String.prototype.toUpperCase ( )
		prototype.toUpperCase = function()
		{
			return _toUpperCase(this);
		}
		ECMA4 function toUpperCase():String
		{
			return _toUpperCase(this);
		}
		private static function _toUpperCase(o):String
		{
			return @todo;
		}

		// 15.5.4.18 String.prototype.toLocaleUpperCase ( )
		prototype.toLocaleUpperCase = function()
		{
			return _toLocaleUpperCase(this);
		}
		ECMA4 function toLocaleUpperCase():String
		{
			return _toLocaleUpperCase(this);
		}
		private static function _toLocaleUpperCase(o):String
		{
			return @todo;
		}

		// 15.5.5 Properties of String Instances
		magic const value;

		// 15.5.5.1 length
		const length;	

		// native helpers
		private static native function _fromOneCharCode(code:Number):String;				// @todo
		private static native function _getOneCharCodeAt(s:String, index:Number):Number;	// @todo

		// mark all prototype functions as {DE}
		_dontEnum(prototype);

	} // class
} // package
