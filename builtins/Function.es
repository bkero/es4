/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Function" object
 * E262-3 15.3
 * E262-4 draft
 *
 * Status: complete; not reviewed; not tested.
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
    dynamic class Function
    {       
        use namespace intrinsic;
        use strict;

        /* E262-3 15.3.1.1: The Function constructor */
        intrinsic static function construct(...args)
            createFunction(args);

        /* E262-3 15.3.1: The Function Constructor Called as a Function */
        intrinsic static function call(...args)
            createFunction(args);

        /* E262-3 10.X / 13.X: function invocation */
        intrinsic function call(...args)
            magic::invoke(code, env, args);

        prototype function toString()
            this.toString();

        intrinsic function toString() : String!
            source;
        
        /* E262-3 15.3.4.3: Function.prototype.apply */
        prototype function apply(thisArg, argArray)
            this.apply(thisArg, argArray);

        intrinsic function apply(thisArg, argArray) : *
            Function.apply(this, thisArg, argArray);

        /* E262-4 draft: "apply" and "call" are static methods on the
           Function object, and everyone eventually ends up in
           Function.apply().

           Note ES4 bug fix: the arguments object is an 'Array', so the test
           for applicability of argArray is simpler than in ES3.
        */
        public static function apply(fn : Function!, thisArg, argArray)
        {
            if (thisArg === void 0 || thisArg === null)
                thisArg = global;
            if (argArray === void 0 || argArray === null)
                argArray = [];
            else if (!(argArray is Array))
                throw new TypeError("argument array to 'apply' must be Array");
            return magic::apply(fn, thisArg, argArray);
        }

        /* E262-3 15.3.4.4: Function.prototype.call.

           Assuming a rest argument does not contribute to the
           "length" of the function, so the length of
           Function.prototype.call is 1, which is what we want. 
        */
        prototype function call(thisObj, ...args)
            Function.apply(this, thisObj, args);

        intrinsic function call(thisObj, ...args:Array):*
            Function.apply(this, thisObj, args);

        /* E262-4 draft: "apply" and "call" are static methods on the
           Function object. */
        public static function call(thisObj, ...args:Array):*
            Function.apply(this, thisObj, args);
        
        /* E262-3 15.3.5.3: [[HasInstance]] */
        intrinsic function HasInstance(V)
        {
            if (!(V is Object))
                return false;

            let O : Object = this.prototype;
            if (!(O is Object))
                throw new TypeError("[[HasInstance]]: prototype is not object");

            while (true) {
                V = magic::getPrototype(V);
                if (V === null)
                    return false;
                if (O == V)
                    return true;
            }
        }


        var code : *;          // Opaque representation of compiled code
        var env : *;           // Environment in which this function is closed
        var source : String!;  // Source code for decompilation

        /* Given an array of values as passed to the function
           constructor, create a new function object. 
        */
        static function createFunction(args : Array!) : Function!
        {
            let [code, source, length] : [*, String!, Number] = compileFunction(args);
            let fn : Function = super.intrinsic::construct(Function);
            let x : * = fn.Function();

            if (x is Object)
                return x to Object;

            fn.length = length;
            fn.prototype = new Object;
            fn.source = source;
            fn.code = code;
            fn.env = global;
            return fn;
        }

        /* Given an array of values as passed to the function
           constructor, compile the function and return the compiled
           code, a representation of the source code suitable for
           Function.prototype.toString(), and the function's
           "length".  
        */
        static function compileFunction(...args) : [*, String!, Number]
        {
            let formals = args[0:args.length-1].join(",");
            let body = args[args.length-1];
            let [code, length] = magic::compile(formals, body);
            let source = "function (" + formals + ") {" + body + "}";
            return [code, source, length];
        }
    }
}
