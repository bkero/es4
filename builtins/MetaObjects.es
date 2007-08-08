package
{
    use default namespace intrinsic;

    class Type {}

    class UndefinedType extends Type {}
    class NullType extends Type {}
    class AnyType extends Type { }

    /*
    interface Type 
    {
        //function canConvertTo(t: Type): boolean;
        //function isSubtypeOf(t: Type): boolean;
    }


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
