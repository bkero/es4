/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Global" object
 * ES-262-3 15.1
 * ES-262-4 draft
 *
 * Status: not reviewed against specs.
 */

package
{
	// 15.1.1.1 NaN
	// 15.1.1.2 Infinity
	// 15.1.1.3 undefined
	// {DE,DD,RO} -- change from ECMA-262, which has them as {DE,DD}
	ECMA4 const NaN = 0.0/0.0;
	ECMA4 const Infinity = 1.0/0.0;
	ECMA4 const undefined = void(0);
	
	// @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
	
	// 15.1.2.1 eval (x)
	ECMA4 native function eval(x);

	// 15.1.2.2 parseInt (string , radix)
	ECMA4 native function parseInt(string:String, radix:Number);

	// 15.1.2.3 parseFloat (string)
	ECMA4 native function parseFloat(string:String);

	// 15.1.2.4 isNaN (number)
	ECMA4 native function isNaN(number:*):Boolean;

	// 15.1.2.4 isFinite (number)
	ECMA4 native function isFinite(number:*):Boolean;
	
	// 15.1.3.1 decodeURI (encodedURI)
	ECMA4 native function decodeURI(encodedURI);

	// 15.1.3.2 decodeURIComponent (encodedURIComponent)
	ECMA4 native function decodeURIComponent(encodedURIComponent);
	
	// 15.1.3.3 encodeURI (uri)
	ECMA4 native function encodeURI(uri);
	
	// 15.1.3.4 encodeURIComponent (uriComponent)
	ECMA4 native function encodeURIComponent(uriComponent);


	var NaN = ECMA4::NaN;
	var Infinity = ECMA4::Infinity;
	var undefined = ECMA4::undefined;

	var eval = ECMA4::eval;
	var parseInt = ECMA4::parseInt;
	var parseFloat = ECMA4::parseFloat;
	var isNaN = ECMA4::isNaN;
	var isFinite = ECMA4::isFinite;
	var decodeURI = ECMA4::decodeURI;
	var decodeURIComponent = ECMA4::decodeURIComponent;
	var encodeURI = ECMA4::encodeURI;
	var encodeURIComponent = ECMA4::encodeURIComponent;

	/* ... */
	intrinsic function ToPrimitive(value, preferredType)	{
		if (value === void 0 || value === null || value is String || value is Boolean || value is Number)
			return value;
		return value.intrinsic::DefaultValue(preferredType);
	}

	/* ES-262-3 9.2: The ToBoolean operation */
	intrinsic function ToBoolean(value) : Boolean {
		if (value is Boolean)
			return value;

		if (value === void 0 || value === null)
			return false;
		if (value is String)
			return value !== "";
		if (value is Number)
			return value !== 0.0 && value === value;
		return true;
	}
		
	/* ES-262-3 9.3: ToNumber */
	intrinsic function ToNumber(value) : Number {
		if (value is Number)
			return value;
		if (value === void 0)
			return 0.0/0.0;
		if (value === null)
			return 0;
		if (value is Boolean)
			return value ? 1 : 0;
		if (value is String)
			return parseFloat(value);
		return ToNumber(ToPrimitive(value, "Number"));
	}

	intrinsic function ToInteger(value) : Number {
		value = ToNumber(value);
		if (value !== value)
			return 0;
		if (value === 0 || !intrinsic::isFinite(value))
			return value;
		var sign = value < 0 ? -1 : 1;
		return sign * Math.floor(Math.abs(value));
	}

	intrinsic function ToString(value) : String {
		if (value is String)
			return value;
		if (value === void 0)
			return "undefined";
		if (value === null)
			return "null";
		if (value is Boolean)
			return value ? "true" : "false";
		if (value is Number)
			intrinsic::NumberToString(value, 10);  // Not quite
		return ToString(ToPrimitive(value, "String"));
	}

	/* ES-262-3 9.9: ToObject.

	   ES-262-4 draft: All values except undefined and null are
  	   already objects, no conversion is necessary.  */

	intrinsic function ToObject(value) : Object {
		if (value === void 0 || value === null)
			throw new TypeError("Can't convert undefined or null to Object");
		return value;
	}
}
