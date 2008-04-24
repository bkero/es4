/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Function" object
 * E262-3 15.3
 * E262-4 draft
 *
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *
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

// Vile hack.  See Object.es for documentation
package org.ecmascript.vilehack.Function {
    public namespace Private = "Function private";
}

package
{
    import org.ecmascript.vilehack.Function.*;

    use namespace __ES4__;

    public dynamic class Function
    {
        // IMPLEMENTATION ARTIFACT: A getter because Function is loaded before int.
        static function get length() { return 1 }

        /* E262-3 15.3.1: The Function Constructor Called as a Function */
        static meta function invoke(...args)
            magic::construct(Function, args);

        /* The following is for the benefit of the specification, don't remove it.

        public function Function(...args)
            helper::createFunction(args);

        helper function createFunction(args) {
            let parameters = "";
            let body = "";
            if (args.length > 0) {
                body = args[args.length-1];
                args.length = args.length-1;
                parameters = args.join(",");
            }
            body = string(body);
            magic::initializeFunction(this, __ES4__::global, parameters, body);
        }

        */

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
        public prototype meta function invoke(...args)
            undefined;

        public prototype var length : double = 0;

        /* XXX: Function.prototype.toString */
        public prototype function toString(this: Function)
            this.Private::toString();

        override intrinsic function toString() : string
            Private::toString();

        Private function toString(): string
            source || "function ( ) { }";


        /* E262-3 15.3.4.3: Function.prototype.apply */
        public prototype function apply(/*this: Callable,*/ thisArg=undefined, argArray=undefined)
            Function.apply(this,
                           thisArg === undefined ? null : thisArg, 
                           argArray === undefined ? null : argArray);

        intrinsic function apply(thisArg: Object=null, argArray: Object=null)
            Function.apply(this, thisArg, argArray);

        /* E262-4 draft: "apply" and "call" are static methods on the
           Function object, and everyone eventually ends up in
           Function.apply().
        */
        static public function apply(fn/*: Callable*/, thisArg: Object=null, argArray: Object=null) {
            if (thisArg === null)
                thisArg = global;
            if (argArray === null)
                argArray = [];
            return magic::apply(fn, thisArg, argArray);
        }

        /* E262-3 15.3.4.4: Function.prototype.call.

           Assuming a rest argument does not contribute to the
           "length" of the function, so the length of
           Function.prototype.call is 1, which is what we want.
        */
        public prototype function call(/*this: Callable,*/ thisObj=undefined, ...args)
            Function.apply(this, 
                           thisObj === undefined ? null : thisObj, 
                           args);

        intrinsic function call(thisObj: Object=null, ...args)
            Function.apply(this, thisObj, args);

        /* E262-4 draft: "apply" and "call" are static methods on the
           Function object. */
        static public function call(fn/*: Callable*/, thisObj: Object=null, ...args)
            Function.apply(fn, thisObj, args);


        /* E262-4 draft: "bind" is a static method on the Function object and
           also a method on function objects.
        */
        public prototype function bind(/*this: Callable,*/ thisObj, ...args)
            Function.helper::bind(this, thisObj, args);

        intrinsic function bind(thisObj: Object, ...args)
            Function.helper::bind(this, thisObj, args);

        static public function bind(method/*: Callable*/, thisObj: Object, ...args)
            helper::bind(method, thisObj, args);

        static helper function bind(method, thisObj, args) {
            return function (...moreargs) 
                       method.apply(thisObj, args.concat(moreargs));
        }


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
        var source : string?;

        public prototype function toSource()
            this.source;

        // IMPLEMENTATION ARTIFACT

        // This is a getter because 'double' is not defined by the time we
        // start constructing Functions.
        public function get length()
            magic::fnLength(this);

        // 'length' is logically a read-only property, so setting it should fail silently
        public function set length(x) {
            // ignore it
        }
    }
}
