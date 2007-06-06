/* -*- mode: java; indent-tabs-mode: nil -*- 
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
    use default namespace public;

    // FIXME: this needs to be here to open the intrinsic namespace in the class.
    // There is a bug in the definer.
    use namespace intrinsic;

    dynamic class Object 
    {       
        use namespace intrinsic;
        use strict;

        /* E262-3 15.2.1.1: The Object constructor called as a function */
        meta static function invoke(value) {
            if (value === null ||
                value === undefined)
                return new Object();
            return ToObject(value);
        }

        /* E262-3 15.2.2.1: The Object constructor. */
        function Object() {}

        /* E262-3 15.2.4.2: Object.prototype.toString */
        prototype function toString()
            Object.private::toString(this);

        intrinsic function toString() : string
            Object.private::toString(this);

        private static function toString(obj) : string
            "[object " + magic::getClassName(obj) + "]";

        /* E262-3 15.2.4.3: Object.prototype.toLocaleString */
        prototype function toLocaleString()
            private::toLocaleString(this);

        intrinsic function toLocaleString() : string
            private::toLocaleString(this);

        private static function toLocaleString(obj)
            "[object " + magic::getClassName(obj) + "]";

        /* E262-3 15.2.4.4:  Object.prototype.valueOf */
        prototype function valueOf()
            this.valueOf();

        intrinsic function valueOf() : Object!
            this;

        /* E262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
        prototype function hasOwnProperty(V)
            Object.private::hasOwnProperty(this, V);  // FIXME: "Object." should not be necessary

        intrinsic function hasOwnProperty(V : (Name,string)) : boolean 
            Object.private::hasOwnProperty(this, V);  // FIXME: "Object." should not be necessary

        private static function hasOwnProperty(obj, V : (Name,string)) : boolean
            magic::hasOwnProperty(obj, V);
        
        /* E262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
        prototype function isPrototypeOf(V)
            private::isPrototypeOf(this, V);
        
        intrinsic function isPrototypeOf(V)
            private::isPrototypeOf(this, V);

        private static function isPrototypeOf(target, v) : boolean {
            if (!(v is Object))
                return false;

            let vo : Object = v to Object;
            while (true) {
                vo = magic::getPrototype(VO);
                if (vo === null || vo === undefined)
                    return false;
                if (vo === target)
                    return true;
            }
        }

        /* E262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
        prototype function propertyIsEnumerable(prop, e=undefined)
            private::propertyIsEnumerable(this, prop, e);

        intrinsic function propertyIsEnumerable(prop : (Name,string), e=undefined) : boolean
            private::propertyIsEnumerable(this, prop, e);

        /* E262-4 draft proposals:enumerability */
        private static function propertyIsEnumerable(obj : Object, prop : (Name,string),
                                                     e = undefined) : boolean {
            while (obj !== null) {
                if (obj.hasOwnProperty(prop)) {
                    let old : boolean = !magic::getPropertyIsDontEnum(obj, prop);
                    if (!magic::getPropertyIsDontDelete(obj, prop))
                        if (e is Boolean)
                            magic::setPropertyIsDontEnum(obj, prop, !e);
                    return old;
                }
                obj = magic::getPrototype(obj);
            }
        }

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function toJSONString() 
            private::toJSONString(this);

        intrinsic function toJSONString(...args) : string
            private::toJSONString.apply(obj, args);

        private static function toJSONString(obj, args) : string
            JSON.emit.apply(null, args.unshift(obj));
    }
}
