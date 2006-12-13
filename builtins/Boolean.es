/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Boolean" object
 * ES-262-3 15.6
 */

package
{
	dynamic class Boolean extends Object
	{		
		/* ES-262-3 15.6.1: The Boolean Constructor Called as a Function.

		   ES-262-4 draft: There are no Boolean "values", only Boolean
		   objects, so this will always return type Boolean. */
		static intrinsic function call(value)
		{
			return value ? true : false;                      // value is forced through ToBoolean
		}

		/* ES-262-3 15.6.2.1: The Boolean Constructor. */
		function Boolean(value)
		{
			intrinsic::setPrototype(this, Boolean.prototype);
			intrinsic::setClass(this, "Boolean");
			intrinsic::setValue(this, value ? true : false);  // value is forced through ToBoolean
		}

		/* ES-262-3 15.6.3:  Properties of the Boolean Constructor */
		static const length = 1;

		/* ES-262-3 15.6.3.1: Boolean.prototype */
		static const prototype = { };

		/* ES-262-3 requires that the class name of the prototype
		   object is "Boolean".  */
   		intrinsic::setClass(Boolean.prototype, "Boolean");

		/* ES-262-3 15.6.4.1: Boolean.prototype.constructor is the
		   original Boolean constructor */
		Boolean.prototype.constructor = Boolean;
		
		/* ES-262-3 15.6.4.2: Boolean.prototype.toString.  

		   The type constraint enforces non-transportability of the
		   toString method.  */
		Boolean.prototype.toString = function toString(this : Boolean) 
		{
			return this.intrinsic::toString();
		}

		/* ES-262-4 draft ch 19 */
		intrinsic function toString() : String
		{
			return intrinsic::getValue(this) ? "true" : "false";
		}
		
		/* ES-262-3 15.6.4.3: Boolean.prototype.valueOf.

		   The type constraint enforces non-transportability of the
		   toString method.  */
		Boolean.prototype.valueOf = function valueOf(this : Boolean)
		{
			return this;
		}

		/* ES-262-4 draft ch 19 */
		ECMA4 function valueOf() : Object
		{
			return this;
		}

		/* ES-262-3 FIXME-cite: properties on prototype are DontEnum */
		intrinsic::setPropertyIsDontEnum(Boolean.prototype, "constructor", true);
		intrinsic::setPropertyIsDontEnum(Boolean.prototype, "toString", true);
		intrinsic::setPropertyIsDontEnum(Boolean.prototype, "valueOf", true);
	}
}
