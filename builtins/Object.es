/* -*- mode: java; mode: font-lock; tab-width: 4; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Object" object
 *
 * E262-3 15.2
 * E262-4 proposals:builtin_classes
 *
 * Status: Complete; Not reviewed against spec.
 */
package
{
    dynamic class Object 
    {       
        /* E262-3 15.2.1.1: The Object constructor called as a function */
        static intrinsic function call(value) {
            return intrinsic::ToObject(value);
        }

        /* E262-3 15.2.2.1: The Object constructor */
        static intrinsic function construct(value) : Object! {
            if (value !== intrinsic::undefined && value !== null)
                return value;

            const o : Object! = super.intrinsic::construct(Object);  // see Class.es
            const x : * = o.Object();
            return (x is Object) ? x to Object! : o;
        }

        /* E262-3 15.2.4.2: Object.prototype.toString */
        prototype function toString() {
            return this.intrinsic::toString();
        }

        intrinsic function toString() : String! {
            return "[object " + magic::getClassName(this) + "]";
        }

        /* E262-3 15.2.4.3: Object.prototype.toLocaleString */
        prototype function toLocaleString() {
            return this.intrinsic::toLocaleString();
        }

        intrinsic function toLocaleString() : String! {
            return "[object " + magic::getClassName(this) + "]";
        }

        /* E262-3 15.2.4.4:  Object.prototype.valueOf */
        prototype function valueOf() {
            return this.intrinsic::valueOf();
        }

        intrinsic function valueOf() : Object! {
            return this;
        }

        /* E262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
        prototype function hasOwnProperty(V) {
            return this.intrinsic::hasOwnProperty(V);
        }

        intrinsic function hasOwnProperty(V : String!) : Boolean {
            return magic::hasOwnProperty(this, V);
        }
        
        /* E262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
        prototype function isPrototypeOf(V) {
            return this.intrinsic::isPrototypeOf(V);
        }

        intrinsic function isPrototypeOf(V) : Boolean {
            if (!(V is Object))
                return false;

            var VO : Object = V to Object;
            for (;;) {
                VO = magic::getPrototype(VO);
                if (VO === null)
                    return false;
                if (VO === this)
                    return true;
            }
        }

        /* E262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
        prototype function propertyIsEnumerable(V, E=intrinsic::undefined) {
            return this.intrinsic::propertyIsEnumerable(V, E);
        }

        /* E262-4 draft proposals:enumerability */
        intrinsic function propertyIsEnumerable(V : String!, E=intrinsic::undefined) : Boolean {
            var O : Object = this;
            while (O !== null) {
                if (O.intrinsic::hasOwnProperty(V)) {
                    let old : Boolean = !magic::getPropertyIsDontEnum(O, V);
                    if (!magic::getPropertyIsDontDelete(O, V))
                        if (E is Boolean)
                            magic::setPropertyIsDontEnum(O, V, !E);
                    return old;
                }
                O = magic::getPrototype(O);
            }
        }
    }
}
