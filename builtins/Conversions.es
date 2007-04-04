/* -*- mode: java; indent-tabs-mode: nil -*- */

/* Primitive conversion functions.  These are all "intrinsic"-only, in
   E262-3 they are hidden in the implementation but there's no real
   reason not to expose them, though we could certainly hide them.
*/

package
{
    use namespace intrinsic;
    use strict;

    /* ... */
    intrinsic function ToPrimitive(value, preferredType)    {
        if (value === undefined || value === null || value is String || value is Boolean || value is Numeric)
            return value;
        return value.intrinsic::DefaultValue(preferredType);
    }

    /* ES-262-3 9.2: The ToBoolean operation */
    intrinsic function ToBoolean(value) : Boolean {
        if (value is Boolean)
            return value;

        if (value === undefined || value === null)
            return false;
        if (value is String)
            return value !== "";
        if (value is Numeric)
            return value !== 0 && value === value;
        return true;
    }
        
    intrinsic function ToInteger(value) : Number {
        value = ToDouble(value);
        if (value !== value)
            return 0;
        if (value === 0 || !isFinite(value))
            return value;
        var sign:double = value < 0d ? -1d : 1d;
        return sign * Math.floor(Math.abs(value));
    }

    // FIXME: these can probably be decomposed into smaller steps with
    // less magic. For now we just want it to start running.
    //
    // NB. be careful to check if the parameter matches the desired
    // type already and bottom out, since the converters call back
    // into the constructors, which call back *here*.

    intrinsic function ToInt(value) : int
	(value is int) ? value : magic::toInt(value);

    intrinsic function ToUint(value) : uint 
	(value is uint) ? value : magic::toUint(value);

    intrinsic function ToDouble(value) : double
	(value is double) ? value : magic::toDouble(value);

    intrinsic function ToDecimal(value) : decimal
	(value is decimal) ? value : magic::toDecimal(value);

    intrinsic function ToString(value) : string {
        if (value is string)
            return value;
        if (value is String)
            return value.valueOf();
        if (value === undefined)
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

    intrinsic function ToObject(value) : Object! {
        if (value === undefined || value === null)
            throw new TypeError("Can't convert undefined or null to Object");
        return value;
    }
}
