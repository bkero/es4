/* -*- mode: java; indent-tabs-mode: nil -*- 
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
    use default namespace public;

    /* E262-3 15.11 */
    public dynamic class Error 
    {
        static intrinsic function call(... args)
            Error.construct.apply(null, args);

        function Error(msg)
        {
            if (msg !== undefined)
                this.message = ToString(msg);
        }

        /* E262-3 15.11.4.2: "name" property on prototype */
        prototype var name = "Error";

        /* E262-3 15.11.4.3: "message" property on prototype */
        /* INFORMATIVE */
        prototype var message = "Generic error";

        /* E262-3 15.11.4.4: toString */
        prototype function toString()
            this.toString();

        /* INFORMATIVE */
        override intrinsic function toString() {
            if (this.message)        
                return this.name + ": " + this.message;   /* "this" qualification in case they've been deleted */
            else
                return this.name;
        }
    }

    /* E262-3 15.11.6.1; 15.11.7 */
    public dynamic class EvalError extends Error
    {
        function EvalError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "EvalError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Illegal use of 'eval'";
    }

    /* E262-3 15.11.6.2; 15.11.7 */
    public dynamic class RangeError extends Error
    {
        function RangeError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "RangeError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Numeric value out of range";
    }

    /* E262-3 15.11.6.3; 15.11.7 */
    public dynamic class ReferenceError extends Error
    {
        function ReferenceError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "ReferenceError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Invalid reference value";
    }

    /* E262-3 15.11.6.4; 15.11.7 */
    public dynamic class SyntaxError extends Error
    {
        function SyntaxError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "SyntaxError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Parsing error";
    }

    /* E262-3 15.11.6.5; 15.11.7 */
    public dynamic class TypeError extends Error
    {
        function TypeError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "TypeError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */ 
        /* INFORMATIVE */
        prototype var message = "Operand does not have the expected type";
    }

    /* E262-3 15.11.6.6; 15.11.7 */
    public dynamic class URIError extends Error
    {
        function URIError(msg) : super(msg) {}

        /* E262-3 15.11.7.9: "name" property on NativeError prototype */
        prototype var name = "URIError";

        /* E262-3 15.11.7.10: "message" property on NativeError prototype */
        /* INFORMATIVE */
        prototype var message = "Abuse of URI method";
    }
}
