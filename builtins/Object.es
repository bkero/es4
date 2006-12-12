/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Object" object
 * ES-262-3 15.2
 * ES-262-4 draft
 *
 * Status: not reviewed against specs.
 */

package
{
	dynamic class Object 
	{		
		/* ES-262-3 15.2.1.1: The Object constructor called as a function */
		static intrinsic function call(value)
		{
			return intrinsic::ToObject(value);
		}

		/* ES-262-3 15.2.2.1: The Object constructor */
		static intrinsic function construct(value) : Object!
		{
			if (value !== void 0 && value !== null)
				return value;

			const o : Object! = super.intrinsic::construct(Object);  // see Class.es
			const x : * = o.Object();
			return (x is Object) ? x to Object! : o;
		}

		/* ES-262-3 15.2.4.2: Object.prototype.toString */
		Object.prototype.toString = function toString()
		{
			return this.intrinsic::toString();
		}

		intrinsic function toString() : String!
		{
			return "[object " + magic::getClassName() + "]";
		}

		/* ES-262-3 15.2.4.3: Object.prototype.toLocaleString */
		Object.prototype.toLocaleString = function toLocaleString()
		{
			return this.intrinsic::toLocaleString();
		}

		intrinsic function toLocaleString() : String!
		{
			return "[object " + magic::getClassName() + "]";
		}

		/* ES-262-3 15.2.4.4:  Object.prototype.valueOf */
		Object.prototype.valueOf = function valueOf()
		{
			return this.intrinsic::valueOf();
		}

		intrinsic function valueOf() : Object!
		{
			return this;
		}

		/* ES-262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
		Object.prototype.hasOwnProperty = function hasOwnProperty(V)
		{
			return this.intrinsic::hasOwnProperty(V);
		}

		intrinsic function hasOwnProperty(V) : Boolean!
		{
			return magic::hasOwnProperty(this, intrinsic::ToString(V));
		}
		
		/* ES-262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
		Object.prototype.isPrototypeOf = function(V)
		{
			return this.intrinsic::isPrototypeOf(V);
		}

		intrinsic function isPrototypeOf(V) : Boolean!
		{
			var O : Object! = this;

			if (!(V is Object))
				return false;

			for (;;) {
				V = magic::getPrototype(V);
				if (V === null)
					return false;
				if (O === V)
					return true;
			}
		}

		/* ES-262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
		prototype.propertyIsEnumerable = function propertyIsEnumerable(V, ...args)
		{
			if (args.length <= 1)
			    return magic::getPropertyIsEnumerable(this, intrinsic::ToString(V));
			else
			    magic::setPropertyIsEnumerable(this, intrinsic::ToString(V), intrinsic::ToBoolean(args[0]));
		}

		intrinsic function propertyIsEnumerable(V, ...args)
		{
			if (args.length <= 1)
			    return magic::getPropertyIsEnumerable(this, intrinsic::ToString(V));
			else
			    magic::setPropertyIsEnumerable(this, intrinsic::ToString(V), intrinsic::ToBoolean(args[0]));
		}

		magic::setPropertyIsEnumerable(prototype, "toString", false);
		magic::setPropertyIsEnumerable(prototype, "toLocaleString", false);
		magic::setPropertyIsEnumerable(prototype, "valueOf", false);
		magic::setPropertyIsEnumerable(prototype, "hasOwnProperty", false);
		magic::setPropertyIsEnumerable(prototype, "isPrototypeOf", false);
		magic::setPropertyIsEnumerable(prototype, "propertyIsEnumerable", false);
	}
}
