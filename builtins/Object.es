/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Object" object
 *
 * E262-3 15.2
 * E262-4 proposals:builtin_classes
 * E262-4 draft proposals:json_encoding_and_decoding
 *
 * Status: Complete; Not reviewed against spec.
 */
package
{
    namespace core;

    dynamic class Object 
    {       
        use namespace intrinsic;
        use strict;

        /* E262-3 15.2.1.1: The Object constructor called as a function */
        core static function invoke(value)
            ToObject(value);

        /* E262-3 15.2.2.1: The Object constructor. */
        function Object() {}

        /* E262-3 15.2.4.2: Object.prototype.toString */
        prototype function toString()
            this.toString();

        intrinsic function toString() : string
            "[object " + magic::getClassName(this) + "]";

        /* E262-3 15.2.4.3: Object.prototype.toLocaleString */
        prototype function toLocaleString()
            this.toLocaleString();

        intrinsic function toLocaleString() : string
            "[object " + magic::getClassName(this) + "]";

        /* E262-3 15.2.4.4:  Object.prototype.valueOf */
        prototype function valueOf()
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
        prototype function hasOwnProperty(V)
            this.hasOwnProperty(V);

        intrinsic function hasOwnProperty(V : string) : Boolean 
            magic::hasOwnProperty(this, V);
        
        /* E262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
        prototype function isPrototypeOf(V)
            this.isPrototypeOf(V);

        intrinsic function isPrototypeOf(V) : Boolean {
            if (!(V is Object))
                return false;

            let VO : Object = V to Object;
            while (true) {
                VO = magic::getPrototype(VO);
                if (VO === null)
                    return false;
                if (VO === this)
                    return true;
            }
        }

        /* E262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
        prototype function propertyIsEnumerable(V, E=undefined)
            this.propertyIsEnumerable(V, E);

        /* E262-4 draft proposals:enumerability */
        intrinsic function propertyIsEnumerable(V : string, E=undefined) : Boolean {
            let O : Object = this;
            while (O !== null) {
                if (O.hasOwnProperty(V)) {
                    let old : Boolean = !magic::getPropertyIsDontEnum(O, V);
                    if (!magic::getPropertyIsDontDelete(O, V))
                        if (E is Boolean)
                            magic::setPropertyIsDontEnum(O, V, !E);
                    return old;
                }
                O = magic::getPrototype(O);
            }
        }

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function toJSONString() 
            this.toJSONString();

        intrinsic function toJSONString(...args) : string
            JSON.emit.apply(null, args.unshift(this));
    }
}
