/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Error" object
 *
 * E262-3 15.11
 * E262-4 draft (intrinsic functions)
 *
 * Status: Incomplete; Not reviewed against spec; Not tested.
 *
 * To check:
 *
 * Can we use "prototype" with "var" like I do here?  It makes sense,
 * and is desirable, but probably not necessary.
 *
 * It's my belief that making eg EvalError extend Error makes
 * EvalError.prototype be an Error object, though that needs to be
 * verified.
 */

package
{
    use namespace intrinsic;

    /* E262-3 15.11 */
    dynamic class Error 
    {
        static intrinsic function call(... args)
            Error.construct.apply(null, args);

        /* E262-3 15.11.4.2: "name" property on prototype */
        prototype var name = "Error";

        /* E262-3 15.11.4.3: "message" property on prototype */
        /* INFORMATIVE */
        prototype var message = "Generic error";

        /* E262-3 15.11.4.4: toString */
        prototype function toString()
            this.toString();

        /* INFORMATIVE */
        intrinsic function toString()
            this.name + ": " + this.message;   /* "this" qualification in case they've been deleted */
    }

    /* E262-3 15.11.6.1; 15.11.7 */
    dynamic class EvalError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "EvalError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Illegal use of 'eval'";
    }

    /* E262-3 15.11.6.2; 15.11.7 */
    dynamic class RangeError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "RangeError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Numeric value out of range";
    }

    /* E262-3 15.11.6.3; 15.11.7 */
    dynamic class ReferenceError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "ReferenceError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Invalid reference value";
    }

    /* E262-3 15.11.6.4; 15.11.7 */
    dynamic class SyntaxError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "SyntaxError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Parsing error";
    }

    /* E262-3 15.11.6.5; 15.11.7 */
    dynamic class TypeError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "TypeError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */ 
        /* INFORMATIVE */
        prototype var message = "Operand does not have the expected type";
    }

    /* E262-3 15.11.6.6; 15.11.7 */
    dynamic class URIError extends Error
    {
        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "URIError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Abuse of URI method";
    }
}
