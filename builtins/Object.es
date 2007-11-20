/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Object" object
 *
 * E262-3 15.2
 * E262-4 proposals:builtin_classes
 * E262-4 draft proposals:json_encoding_and_decoding
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

package
{
    use default namespace public;
    // FIXME: this needs to be here to open the intrinsic namespace in the class.
    // There is a bug in the definer.
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    import JSON.*;
    
    type EnumerableId = (int, uint, string /*, Name*/);  // FIXME: circularity
    
    dynamic class Object
    {
        // IMPLEMENTATION ARTIFACT: A getter because Object is loaded before int.
        static function get length() { return 1 }

        /* E262-3 15.2.1.1: The Object constructor called as a function */
        meta static function invoke(value=undefined) {
            if (value === null || value === undefined)
                return new Object();
            return new Object(value);
        }

        /* E262-3 15.2.4.2: Object.prototype.toString */
        prototype function toString()
            "[object " + magic::getClassName(this) + "]";

        intrinsic function toString() : string
            "[object " + magic::getClassName(this) + "]";


        /* E262-3 15.2.4.3: Object.prototype.toLocaleString */
        prototype function toLocaleString()
            this.public::toString();

        intrinsic function toLocaleString() : string
            this.public::toString();


        /* E262-3 15.2.4.4:  Object.prototype.valueOf */
        prototype function valueOf()
            this;

        intrinsic function valueOf() : Object!
            this;


        /* E262-3 15.2.4.5:  Object.prototype.hasOwnProperty */
        prototype function hasOwnProperty(V)
            magic::hasOwnProperty(this, V is EnumerableId ? V : string(V));

        intrinsic function hasOwnProperty(V: EnumerableId): boolean
            magic::hasOwnProperty(this, V);


        /* E262-3 15.2.4.6:  Object.prototype.isPrototypeOf */
        prototype function isPrototypeOf(V)
            private::isPrototypeOf(this,V);

        intrinsic function isPrototypeOf(V): boolean
            private::isPrototypeOf(this,V);

        private function isPrototypeOf(self, V): boolean {
            if (!(V is Object))
                return false;

            while (true) {
                V = magic::getPrototype(V);
                if (V === null || V === undefined)
                    return false;
                if (V === self)
                    return true;
            }
        }


        /* E262-3 15.2.4.7: Object.prototype.propertyIsEnumerable (V) */
        /* E262-4 draft proposals:enumerability */
        prototype function propertyIsEnumerable(prop, e=undefined)
            private::propertyIsEnumerable(this, 
                                         prop is EnumerableId ? prop : string(prop), 
                                         e is (boolean,undefined) ? e : boolean(e));

        intrinsic function propertyIsEnumerable(prop: EnumerableId,
                                                e:(boolean,undefined) = undefined): boolean 
            private::propertyIsEnumerable(this, prop, e);

        private function propertyIsEnumerable(self, prop, e) {
            if (!magic::hasOwnProperty(self,prop))
                return false;

            let oldval = !magic::getPropertyIsDontEnum(self, prop);
            if (!magic::getPropertyIsDontDelete(self, prop))
                if (e is boolean)
                    magic::setPropertyIsDontEnum(self, prop, !e);
            return oldval;
        }


        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function toJSONString(pretty=false)
            JSON.formatObject(this, pretty);

        intrinsic function toJSONString(pretty: boolean=false) : string
            JSON.formatObject(this, pretty);
    }
}
