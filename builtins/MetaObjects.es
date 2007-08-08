package
{
    import ECMAScript4_Internal.*;

    //    use default namespace intrinsic;

    /* This must return an array of the direct superclass followed by
     * the direct superinterfaces, in declaration order.  Right now we
     * depend on undocumented behavior of the RI: the interfaces are
     * actually in order in the internal data structures.
     */
    informative function directSuperClassAndInterfaces(clsOrInterface: (Class,Interface)) {
        let supers = [];
        switch type (clsOrInterface) {
        case (cls:Class) {
            let probe = magic::getSuperClass(cls);
            if (probe !== null)
                supers.push(probe);
            for ( let i=0 ; ; i++ ) {
                let probe = magic::getImplementedInterface(cls, uint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        case (iface:Interface) {
            for ( let i=0 ; ; i++ ) {
                let probe = magic::getSuperInterface(iface, uint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        }
        return supers;
    }

    /*
    interface Type 
    {
        //function canConvertTo(t: Type): boolean;
        //function isSubtypeOf(t: Type): boolean;
    }

    interface AnyType extends Type { }

    interface NullType extends Type { }

    interface UndefinedType extends Type { }

    //    native function typeOf(e): Type;

    interface Field
    {
        //function namespace() : string;
        //function name(): string;
        //function type(): Type;
    }

    //type FieldIterator = iterator::IteratorType.<Field>;

    interface NominalType extends Type
    {
        //function name(): string;
        //function namespace(): string;
        //function superTypes();  //: IteratorType.<NominalType>;  // Classes and interfaces
        //function publicMembers(): FieldIterator;
        //function publicStaticMembers(): FieldIterator;
    }

    interface InterfaceType extends NominalType
    {
        //function implementedBy():IteratorType.<ClassType>
    }

    //type TypeIterator = iterator::IteratorType.<Type>;
    //type ValueIterator = iterator::IteratorType.<*>;

    interface ClassType extends NominalType
    {
        //function construct(typeArgs: TypeIterator, valArgs: ValueIterator): Object;
    }

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
}
