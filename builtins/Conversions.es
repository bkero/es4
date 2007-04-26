/* -*- mode: java; indent-tabs-mode: nil -*- */

/* Primitive conversion functions.  These are all "intrinsic"-only, in
   E262-3 they are hidden in the implementation but there's no real
   reason not to expose them, though we could certainly hide them.
*/

package
{
    use namespace intrinsic;
    use strict;

    intrinsic function IsPrimitive(value)
        (value === undefined || 
         value === null ||
         value is String || 
         value is Boolean || 
         value is Numeric);

    intrinsic function DefaultValue(obj, preferredType) {
        var x = undefined;
        if (preferredType == "String") {
            if ("toString" in obj)
                x = obj.public::toString();
            if (x === undefined && "valueOf" in obj)
                x = obj.public::valueOf();
        } else {
            if ("valueOf" in obj)
                x = obj.public::valueOf();
            if (x === undefined && "toString" in obj)
                x = obj.public::toString();
        }

        if (IsPrimitive(x))
            return x;

        throw new TypeError();
    }

    /* ... */
    intrinsic function ToPrimitive(value, preferredType) {
        if (IsPrimitive(value))
            return value;
        return DefaultValue(value, preferredType);
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

    intrinsic function ToInt(v) : int
        int(value)

    intrinsic function ToUint(x) : uint 
        uint(x);

    intrinsic function ToDouble(x) : double
        double(x);

    intrinsic function ToDecimal(x) : decimal
        decimal(x);

    intrinsic function ToString(x) : string
        string(x);

    /* ES-262-3 9.9: ToObject.

       ES-262-4 draft: All values except undefined and null are
       already objects, no conversion is necessary.  */

    intrinsic function ToObject(value) : Object! {
        if (value === undefined || value === null)
            throw new TypeError("Can't convert undefined or null to Object");
        return value;
    }
}
