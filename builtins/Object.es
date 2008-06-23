/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Object" object
 *
 * E262-3 15.2
 * E262-4 proposals:builtin_classes
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
 * Status: Complete; Not reviewed against spec.
 */

    public dynamic class Object
    {
        // IMPLEMENTATION ARTIFACT: A getter because Object is loaded before int.
        public static function get length() { return 1 }

        /* Do not remove this, it's used by the spec
        public function Object(value=undefined) {
        }
        */

        /* E262-3 15.2.1.1: The Object constructor called as a function */
        static meta function invoke(value=undefined)
            new Object(value);

        /* E262-3 15.2.4.2: Object.prototype.toString */
        public prototype function toString()
            this.private::toString();

        intrinsic function toString() : string
            private::toString();

        private function toString(): string
            "[object " + helper::getClassName() + "]";

        helper function getClassName()
            informative::getClassName(this);

        /* E262-3 15.2.4.3: Object.prototype.toLocaleString */
        public prototype function toLocaleString()
            this.private::toLocaleString();

        intrinsic function toLocaleString() : string
            private::toLocaleString();

        private function toLocaleString()
            this.toString();


        /* E262-3 15.2.4.4:  Object.prototype.valueOf */
        public prototype function valueOf()
            this.private::valueOf();

        intrinsic function valueOf() : Object
            private::valueOf();

        private function valueOf(): Object
            this;


        /* E262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
        public prototype function hasOwnProperty(name)
            this.private::hasOwnProperty(helper::toEnumerableId(name));

        // Bootstrapping barfs if this does not go directly to the helper,
        // though I don't know why.  Could be that Object is not fully
        // set up yet when it's called.
        intrinsic function hasOwnProperty(name: EnumerableId): boolean
            helper::hasOwnProperty(this, name);

        private function hasOwnProperty(name: EnumerableId): boolean
            helper::hasOwnProperty(this, name);


        /* E262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
        public prototype function isPrototypeOf(value)
            this.private::isPrototypeOf(value);

        intrinsic function isPrototypeOf(value): boolean
            private::isPrototypeOf(value);

        private function isPrototypeOf(value): boolean {
            if (!(value is Object))
                return false;

            let obj = value;
            while (true) {
                obj = helper::getPrototype(obj);
                if (obj === null || obj === undefined)
                    return false;
                if (obj === this)
                    return true;
            }
        }

        /* E262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
        /* E262-4 draft proposals:enumerability */

        public prototype function propertyIsEnumerable(name)
            this.private::propertyIsEnumerable(helper::toEnumerableId(name));

        intrinsic function propertyIsEnumerable(name: EnumerableId): boolean 
            private::propertyIsEnumerable(name);

        private function propertyIsEnumerable(name) {
            if (!helper::hasOwnProperty(this, name))
                return false;
            return !helper::getPropertyIsEnumerable(this, name);
        }

        /* Old code
        prototype function propertyIsEnumerable(name, flag=undefined)
            this.private::propertyIsEnumerable(helper::toEnumerableId(name), 
                                               flag === undefined ? flag : boolean(flag));

        intrinsic function propertyIsEnumerable(name: EnumerableId, flag: (boolean|undefined) = undefined): boolean 
            private::propertyIsEnumerable(name, flag);

        private function propertyIsEnumerable(name, flag) {
            if (!helper::hasOwnProperty(this, name))
                return false;

            let oldval = !helper::getPropertyIsEnumerable(this, name);
            if (!helper::getPropertyIsDontDelete(this, name))
                if (flag !== undefined) 
                    helper::setPropertyIsEnumerable(this, name, !flag);
            return oldval;
        }
        */

        public prototype function __defineProperty__(name, value, enumerable=undefined, removable=undefined, writable=undefined)
            this.private::__defineProperty__(helper::toEnumerableId(name),
                                             value,
                                             enumerable === undefined ? true : boolean(enumerable),
                                             removable === undefined ? true : boolean(removable),
                                             writable === undefined ? true : boolean(writable));

        intrinsic function __defineProperty__(name: EnumerableId, value, enumerable:boolean=true, removable:boolean=true, writable:boolean=true): void
            private::__defineProperty__(name, value, enumerable, removable, writable);

        private function __defineProperty__(name, value, enumerable, removable, writable) {
            if (helper::hasOwnProperty(this, name))
                throw new TypeError(/* Property exists */);

            let obj = helper::getPrototype(this);
            while (obj != null) {
                if (helper::hasOwnProperty(obj, name) && !helper::getPropertyIsWritable(obj, name))
                    throw new TypeError(/* non-Writable property in prototype chain */);
                obj = helper::getPrototype(obj);
            }

            this[name] = value;
            helper::setPropertyIsEnumerable(this, name, enumerable);
            helper::setPropertyIsRemovable(this, name, removable);
            helper::setPropertyIsWritable(this, name, writable);
        }
    }
