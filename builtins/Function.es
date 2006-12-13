/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Function" object
 * E262-3 15.3
 * E262-4 draft
 *
 * Status: not reviewed against specs.
 *
 * The model here is that a function definition is compiled to a
 * Function object whose private "env" property is something
 * arbitrary; when a function is closed in an environment the object
 * is cloned and the clone is given a new value for "env".
 * Implementations don't have to do it that way but as a model it's
 * just fine.
 */

package
{
	dynamic class Function extends Object
	{		
		/* E262-3 15.3.1.1: The Function constructor */
		static intrinsic function construct(...args)
		{
			return createFunction(args);
		}

		/* E262-3 15.3.1: The Function Constructor Called as a Function */
		static intrinsic function call(...args)
		{
			return createFunction(args);
		}

		/* E262-3 15.3.3: Properties of the Function Constructor */
		static const length = 1;
		
		/* E262-3 10.X / 13.X: function invocation */
		intrinsic function call(...args)
		{
			return magic::invoke(code, env, args);
		}

		Function.prototype.toString = function()
		{
			return this.intrinsic::toString();
		}

		intrinsic function toString() : String!
		{
			return source;
		}
		
		/* E262-3 15.3.4.3: Function.prototype.apply */
		Function.prototype.apply = function(thisArg, argArray)
		{
			return this.intrinsic::apply(thisArg, argArray);
		}

		intrinsic function apply(thisArg, argArray) : *
		{
			return Function.apply(this, thisArg, argArray);
		}

		/* E262-4 draft: "apply" and "call" are static methods on the
		   Function object, and everyone eventually ends up in
		   Function.apply().

		   Note ES4 bug fix: the arguments object is an 'Array', so the test
		   for applicability of argArray is simpler than in ES3 */
		static function apply(fn : Function!, thisArg, argArray)
		{
			if (thisArg === void 0 || thisArg === null)
				thisArg = intrinsic::global;
			if (argArray === void 0 || argArray === null)
				argArray = [];
			else if (!(argArray is Array))
				throw new TypeError("argument array to 'apply' must be Array");
			return magic::apply(fn, thisArg, argArray);
		}

		/* E262-3 15.3.4.4: Function.prototype.call.

		   Assuming a rest argument does not contribute to the
		   "length" of the function, so the length of
		   Function.prototype.call is 1, which is what we want. */
		Function.prototype.call = function(thisObj, ...args)
		{
			return Function.apply(this, thisObj, args);
		}

		intrinsic function call(thisObj, ...args:Array):*
		{
			return Function.apply(this, thisObj, args);
		}

		/* E262-4 draft: "apply" and "call" are static methods on the
		   Function object */
		static function call(thisObj, ...args:Array):*
		{
			return Function.apply(this, thisObj, args);
		}
		
		/* E262-3 15.3.5.3: [[HasInstance]] */
		intrinsic function HasInstance(V)
		{
			if (!(V is Object))
				return false;

			var O : Object = this.prototype;
			if (!(O is Object))
				throw new TypeError("[[HasInstance]]: prototype is not object");

			for (;;) {
				V = magic::getPrototype(V);
				if (V === null)
					return false;
				if (O == V)
					return true;
			}
		}

		magic::setPropertyIsDontEnum(Function.prototype, "toString", true);
		magic::setPropertyIsDontEnum(Function.prototype, "apply", true);
		magic::setPropertyIsDontEnum(Function.prototype, "call", true);

		/*** Function public data ***/

		const length : Number;     // Initialized by createFunction, below
		var prototype : *;         // ditto

		/*** Function private data ***/

		private code : *;          // Opaque representation of compiled code
		private env : *;           // Environment in which this function is closed
		private source : String!;  // Source code for decompilation

		/*** Function construction ***/

		/* Given an array of values as passed to the function
		   constructor, create a new function object. */
		static private createFunction(args : Array!) : Function!
		{
			var [code, source, length] : [*, String!, Number] = compileFunction(args);
			var fn : Function = super.intrinsic::construct(Function);
			var x : * = fn.Function();
			if (x is Object)
				return x to Object!;
			fn.length = length;
			fn.prototype = new Object;
			fn.source = source;
			fn.code = code;
			fn.env = intrinsic::global;
			return fn;
		}

		/* Given an array of values as passed to the function
		   constructor, compile the function and return the compiled
		   code, a representation of the source code suitable for
		   Function.prototype.toString(), and the function's
		   "length".  */
		static private compileFunction(...args) : [*, String!, Number]
		{
			var formals = args[0:args.length-1].join(",");
			var body = args[args.length-1];
			var [code, length] = magic::compile(formals, body);
			var source = "function (" + formals + ") {" + body + "}";
			return [code, source, length];
		}
	}
}
