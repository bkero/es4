/* -*- mode: java; indent-tabs-mode: nil -*- 
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
    use default namespace public;
    use namespace intrinsic;

    dynamic class Function
    {
        use namespace intrinsic;
        use strict;

        /* E262-3 15.3.1: The Function Constructor Called as a Function */
        meta static function invoke(...args) 
            magic::construct(Function, args);

        // FIXME: there seems to be little possibility of writing the
        // Function ctor in ES4: it has to bottom out in a no-arg
        // magic-setting variant in a very early bootstrap environment
        // that has no ints, arrays, strings, or other amusements to
        // supply a sensible definition with. So for the time being we
        // will special-case anyone calling "new Function(...)" in 
        // the runtime.


        /* E262-3 10.X / 13.X: function invocation.

           This method is never called.  The Function constructor
           marks instances of Function specially, and recognizes these
           instances in the implementation of function calling.  The
           instance meta::invoke method is defined here to prevent
           subclasses of Function from overriding it.

           Other parts of the class hierarchy may however create
           meta::invoke methods that will be considered by the
           function calling machinery. 
        */        
        meta final function invoke(...args) {
            throw new Error("Implementation error");
        }

        /* XXX: The prototype object behaves like a function (and
           system magic makes its classname be "Function", though it's
           not an instance of the Function class).  So here we install
           some things in the prototype that ensures that the object
           behaves like a function in some trivial ways.
         */
        meta prototype function invoke()
            undefined;

        prototype var source : string = "function () { }";

        prototype var length : uint = 0;

        /* XXX: Function.prototype.toString */
        prototype function toString()
            this.source;

        override intrinsic function toString() : string
            source;
        
        /* E262-3 15.3.4.3: Function.prototype.apply */
        prototype function apply(thisArg, argArray)
            Function.apply(this, thisArg, argArray);

        intrinsic function apply(thisArg, argArray) : *
            Function.apply(this, thisArg, argArray);

        /* E262-4 draft: "apply" and "call" are static methods on the
           Function object, and everyone eventually ends up in
           Function.apply().

           Note ES4 bug fix: the arguments object is an 'Array', so the test
           for applicability of argArray is simpler than in ES3.
        */
        static function apply(fn : Function!, thisArg, argArray) {
            if (thisArg === undefined || thisArg === null)
                thisArg = global;
            if (argArray === undefined || argArray === null)
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
        static function call(thisObj, ...args:Array):*
            Function.apply(this, thisObj, args);
        
        /* E262-3 15.3.5.3: [[HasInstance]] */
        intrinsic function HasInstance(V) {
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

        /* Source code for decompilation, installed by the constructor */
        var source : string;

        prototype function toSource() 
            this.source;

        // This is a getter because 'uint' is not defined by the time we
        // start constructing Functions.
        function get length() 
            magic::fnLength(this);

        // 'length' is logically a read-only property, so setting it should fail silently
        function set length(x) {
            // ignore it
        }
    }
}
