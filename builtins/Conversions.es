/* -*- indent-tabs-mode: nil -*- */

/* Primitive conversion functions.  These are all "intrinsic"-only, in
   E262-3 they are hidden in the implementation but there's no real
   reason not to expose them, though we could certainly hide them.
*/

package
{
    use namespace intrinsic;

    /* ... */
    intrinsic function ToPrimitive(value, preferredType)    {
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
        if (value === undefined)
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

    intrinsic function ToInt(value) : int {
        // FIXME
    }

    intrinsic function ToUint(value) : uint {
        // FIXME
    }

    intrinsic function ToDouble(value) : double {
        // FIXME
    }

    intrinsic function ToDecimal(value) : decimal {
        // FIXME
    }

    intrinsic function ToString(value) : String! {
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

    intrinsic function ToObject(value) : Object! {
        if (value === void 0 || value === null)
            throw new TypeError("Can't convert undefined or null to Object");
        return value;
    }
