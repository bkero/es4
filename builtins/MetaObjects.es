/* -*- mode: java; indent-tabs-mode: nil -*-
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
 */

package
{
    import ECMAScript4_Internal.*;

    {
        use namespace intrinsic;
        use namespace helper;

        helper function numberconversion(t)
            numbertypes.indexOf(t) != -1;

        helper function stringconversion(t)
            stringtypes.indexOf(t) != -1;

        helper function anyconversion(t)
            true;

        helper function noconversion(t)
            false;

        helper function registerMetaObject(cls, metaobj) {
            classtypes.push({cls: cls, metaobj: metaobj});
        }

        helper function getMetaObject(cls) {
            for ( let i=0, limit=classtypes.length ; i < limit ; i++ )
                if (classtypes[i].cls === cls)
                    return classtypes[i].metaobj;
            // FIXME: different code for interfaces... interfaces result here
            // from the hierarchy walk...
            let metaobj = new ClassTypeImpl(cls, noconversion);
            registerMetaObject(cls, metaobj);
            return metaobj;
        }

        type ClassTypeIterator = *;   // FIXME: iterator::IteratorType.<ClassType>
        type NominalTypeIterator = *; // FIXME: iterator::IteratorType.<NominalType>
        type FieldIterator = *;       // FIXME: iterator::IteratorType.<Field>
        type TypeIterator = *;        // FIXME: iterator::IteratorType.<Type>;
        type ValueIterator = *;       // FIXME: iterator::IteratorType.<*>;

        intrinsic interface Type 
        {
            function canConvertTo(t: Type): boolean;
            function isSubtypeOf(t: Type): boolean;
        }

        intrinsic function typeOf(v): Type {
            if (v is null)
                return nulltype;
            if (v is undefined)
                return undefinedtype;
            return getMetaObject(magic::getClassOfObject(v));
        }

        intrinsic interface NullType extends Type
        {
        }

        helper class NullTypeImpl implements NullType
        {
            public function canConvertTo(t: Type): boolean
                t !== undefinedtype;

            public function isSubtypeOf(t: Type): boolean
                t === nulltype;
        }

        intrinsic interface UndefinedType extends Type
        {
        }

        helper class UndefinedTypeImpl implements UndefinedType
        {
            public function canConvertTo(t: Type): boolean
                false;

            public function isSubtypeOf(t: Type): boolean {
                return t === undefinedtype;
            }
        }

        intrinsic interface NominalType extends Type
        {
            function name(): Name;
            function superTypes(): NominalTypeIterator;
            function publicMembers(): FieldIterator;
            function publicStaticMembers(): FieldIterator;
        }

        intrinsic interface ClassType extends NominalType
        {
            //function construct(typeArgs: TypeIterator, valArgs: ValueIterator): Object;
        }

        helper class ClassTypeImpl implements ClassType {

            // FIXME: It's a security problem that this is public
            function ClassTypeImpl(cls, convertsTo)
                : cls = cls
                , convertsTo = convertsTo
            {
            }

            private var cls;
            private var convertsTo;
            private var supers = [];
            private var supersComputed = false;

            // Compute the complete list of superclasses and superinterfaces
            private function computeSupers() {

                function superClass(cls) {
                    if (cls !== null) {
                        supers.push(getMetaObject(cls));
                        superClass(magic::getSuperClass(cls));
                    }
                }

                function superInterface(iface) {
                    supers.push(getMetaObject(iface));
                    let i = 0;
                    while (true) {
                        let iface2 = magic::getSuperInterface(iface, uint(i));
                        if (iface2 == null)
                            break;
                        superInterface(iface2);
                        i++;
                    }
                }

                if (supersComputed)
                    return;

                superClass(magic::getSuperClass(cls));

                let i = 0;
                while (true) {
                    let iface = magic::getImplementedInterface(cls, uint(i));
                    if (iface == null)
                        break;
                    superInterface(iface);
                    i++;
                }

                supersComputed = true;
            }

            public function name(): Name
                new Name("unknown");

            public function superTypes(): NominalTypeIterator {
                computeSupers();
                let i = 0;
                return { 
                    next: 
                    function (): NominalType {
                        if (i == supers.length)
                            throw iterator::StopIteration;
                        return supers[i++];
                    }
                };
            }

            public function publicMembers(): FieldIterator {
                return {
                    next:
                    function (): Field {
                        throw iterator::StopIteration;
                    }
                };
            }

            public function publicStaticMembers(): FieldIterator {
                return {
                    next:
                    function (): Field {
                        throw iterator::StopIteration;
                    }
                };
            }

            public function canConvertTo(t: Type): boolean {
                if (isSubtypeOf(t))
                    return true;
                return convertsTo(t);
            }

            public function isSubtypeOf(t: Type): boolean {
                if (t === this)
                    return true;
                computeSupers();
                return supers.indexOf(t) != -1;
            }
        }

        intrinsic interface InterfaceType extends NominalType
        {
            // Another security leak
            //function implementedBy(): ClassTypeIterator
        }

        intrinsic interface Field
        {
            function name(): Name;
            function type(): Type;
        }

        // These must come after the classes.  That seems like a bug.
        // Class definitions should be done "early" along with function
        // definitions.

        helper const nulltype = new NullTypeImpl;
        helper const undefinedtype = new UndefinedTypeImpl;
        helper const inttype = new ClassTypeImpl(int, numberconversion);
        helper const uinttype = new ClassTypeImpl(uint, numberconversion);
        helper const doubletype = new ClassTypeImpl(double, numberconversion);
        helper const decimaltype = new ClassTypeImpl(decimal, numberconversion);
        helper const Numbertype = new ClassTypeImpl(Number, numberconversion);
        helper const stringtype = new ClassTypeImpl(string, stringconversion);
        helper const Stringtype = new ClassTypeImpl(String, stringconversion);
        helper const booleantype = new ClassTypeImpl(boolean, anyconversion);
        helper const Booleantype = new ClassTypeImpl(Boolean, anyconversion);

        helper const numbertypes = [inttype, uinttype, doubletype, decimaltype, Numbertype];
        helper const stringtypes = [stringtype, Stringtype];

        helper const classtypes = [];

        registerMetaObject(int, inttype);
        registerMetaObject(uint, uinttype);
        registerMetaObject(double, doubletype);
        registerMetaObject(decimal, decimaltype);
        registerMetaObject(Number, Numbertype);
        registerMetaObject(string, stringtype);
        registerMetaObject(String, Stringtype);
        registerMetaObject(boolean, booleantype);
        registerMetaObject(Boolean, Booleantype);
    }
}

    /*

    interface UnionType extends Type
    {
        //function members(); //: TypeIterator;
        //function construct(typeArgs: TypeIterator, valArgs: ValueIterator): *;
    }

    interface FieldValue
    {
        //function namespace() : string;
        //function name(): string;
        //function value(): *;
    }

    //type FieldValueIterator = iterator::IteratorType.<FieldValue>;

    interface RecordType extends Type
    {
        //function fields(): FieldIterator;
        //function construct(typeArgs: TypeIterator, valArgs: FieldValueIterator): Object;
    }

    interface FunctionType extends Type
    {
        //function hasBoundThis(): boolean; // but what about the bound type?
        //function returnType(): Type;
        //function argTypes(): TypeIterator;
        //function construct(typeArgs: TypeIterator, valArgs: ValueIterator): *;
        //function apply(typeArgs: TypeIterator, thisArg: Object?, valArgs: ValueIterator): *;
    }
    */
