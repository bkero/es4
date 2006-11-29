/*
	Attempt at modeling ECMA-262 builtin classes using the new ECMA4 language.
	
	Note that these are intended to reflect ECMA-262 behavior, which may omit
	common-but-nonstandard extensions used in various implementations (e.g., SpiderMonkey).
	
	This also makes no attempt at efficiency of implementation, preferring clarity and
	simplicity instead.
*/

package
{
	public class Global extends Object
	{		
		// 15.1.1.1 NaN
		// { DontEnum, DontDelete}
		public static function get NaN():*
		{
			return @todo;
		}
		public static function set NaN(value:*):void
		{
			@todo;
		}

		// 15.1.1.2 Infinity
		// { DontEnum, DontDelete}
		public static function get Infinity():*
		{
			return @todo;
		}
		public static function set Infinity(value:*):void
		{
			@todo;
		}

		// 15.1.1.3 undefined
		// { DontEnum, DontDelete}
		public static function get undefined():*
		{
			return @todo;
		}
		public static function set undefined(value:*):void
		{
			@todo;
		}

		// 15.1.2.1 eval (x)
		public static function eval(x)
		{
			return @todo;
		}

		// 15.1.2.2 parseInt (string , radix)
		public static function parseInt(string, radix)
		{
			return @todo;
		}

		// 15.1.2.3 parseFloat (string)
		public static function parseFloat(string)
		{
			return @todo;
		}

		// 15.1.2.4 isNaN (number)
		public static function isNaN(number):Boolean
		{
			return Number(number) === NaN;
		}

		// 15.1.2.4 isFinite (number)
		public static function isFinite(number):Boolean
		{
			var theNumber:Number = Number(number);
			return !(theNumber === NaN && 
					theNumber === Infinity &&
					theNumber === -Infinity);
					
		}
		
		// 15.1.3.1 decodeURI (encodedURI)
		public static function decodeURI(encodedURI)
		{
			return @todo;
		}

		// 15.1.3.2 decodeURIComponent (encodedURIComponent)
		public static function decodeURIComponent(encodedURIComponent)
		{
			return @todo;
		}
		
		// 15.1.3.3 encodeURI (uri)
		public static function encodeURI(uri)
		{
			return @todo;
		}
		
		// 15.1.3.4 encodeURIComponent (uriComponent)
		public static function encodeURIComponent(uriComponent)
		{
			return @todo;
		}

		// 15.1.4 Constructor Properties of the Global Object
		public static function get Object() { return @todo; }
		public static function get Function() { return @todo; }
		public static function get Array() { return @todo; }
		public static function get String() { return @todo; }
		public static function get Boolean() { return @todo; }
		public static function get Number() { return @todo; }
		public static function get Date() { return @todo; }
		public static function get RegExp() { return @todo; }
		public static function get EvalError() { return @todo; }
		public static function get RangeError() { return @todo; }
		public static function get ReferenceError() { return @todo; }
		public static function get SyntaxError() { return @todo; }
		public static function get TypeError() { return @todo; }
		public static function get URIError() { return @todo; }
		
		// 15.1.5.1 Math
		public static function get Math() { return @todo; }
	} // class
} // package
