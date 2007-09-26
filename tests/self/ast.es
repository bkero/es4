/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is [Open Source Virtual Machine.].
 *
 * The Initial Developer of the Original Code is
 * Adobe System Incorporated.
 * Portions created by the Initial Developer are Copyright (C) 2004-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Adobe AS3 Team
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/* ast.es */

public namespace Ast

{
    use default namespace Ast;
    //    use namespace intrinsic;

    // POS

    type POS =
       { file: String
       , span: int //StreamPos.span
       , sm: int // StreamPos.sourcemap
       , post_newline: Boolean }

    // BASIC TYPES

    type IDENT = String;   // unicode string
    type IDENTS = [IDENT];

    type HEAD = Head;

    class Head {
        use default namespace public;  // TRAC should default namespace nest
        const fixtures: FIXTURES;  
        const exprs // : EXPRS;
        function Head (fixtures,exprs)
            : fixtures = fixtures
            , exprs = exprs { }
    }

    type FIXTURE_NAME =
       ( TempName
       , PropName )

    class TempName {
        const index : int;
        function TempName (index)
            : index = index {}
    }

    class PropName {
        const name /*: NAME*/;
        function PropName(name) 
            : name=name { }
    }

    type FIXTURE_BINDING = [FIXTURE_NAME,FIXTURE];
    type FIXTURES = [FIXTURE_BINDING];

    type INIT_BINDING = [FIXTURE_NAME,EXPR]
    type INITS = [INIT_BINDING];

    type NAMES = [NAME];
    type NAME =
       { ns: NAMESPACE
       , id: IDENT }

    type MULTINAME =
       { nss: [[NAMESPACE]]
       , id: IDENT }

    // NAMESPACE

    type NAMESPACES = [NAMESPACE];

    type NAMESPACE =
       ( IntrinsicNamespace
       , PrivateNamespace
       , ProtectedNamespace
       , PublicNamespace
       , InternalNamespace
       , UserNamespace
       , AnonymousNamespace
       , ImportNamespace );

    type RESERVED_NAMESPACE =
       ( IntrinsicNamespace
       , PrivateNamespace
       , ProtectedNamespace
       , PublicNamespace
       , InternalNamespace );

    class IntrinsicNamespace {
        function hash () { return "intrinsic"; }
    }

    class OperatorNamespace {
        function hash () { return "operator"; }
    }

    class PrivateNamespace {
        const name : IDENT
        function PrivateNamespace (name)
            : name = name { }
        function hash () { return "private " + name; }
    }

    class ProtectedNamespace {
        const name : IDENT
        function ProtectedNamespace (name)
            : name = name { }
        function hash () { return "protected " + name; }
    }

    class PublicNamespace {
        const name : IDENT;
        function PublicNamespace (name)
            : name = name { }
        function hash () { return "public " + name; }
    }

    class InternalNamespace {
        const name : IDENT;
        function InternalNamespace (name)
            : name = name { }
        function hash () { return "internal " + name; }
    }

    class UserNamespace {
        const name : IDENT;
        function UserNamespace (name)
            : name = name { }
        function hash () { return "use " + name; }
    }

    class AnonymousNamespace {
        const name : IDENT;
        function AnonymousNamespace (name)
            : name = name { }
        function hash () { return "anon " + name; }
    }

    class ImportNamespace {
        const ident : IDENT
        const ns : PublicNamespace
        function hash () { return "import " + ns.hash; }
    }

    var noNS = new PublicNamespace ("");   // FIXME find better way to express

    // NUMBERS

    type NUMERIC_MODE =
       { numberType : NUMBER_TYPE
       , roundingMode: ROUNDING_MODE
       , precision: int }

    type NUMBER_TYPE =
       ( DecimalType
       , DoubleType
       , IntType
       , UIntType
       , NumberType )

    class DecimalType {}
    class DoubleType {}
    class IntType {}
    class UIntType {}
    class NumberType {}

    type ROUNDING_MODE =
       ( Ceiling
       , Floor
       , Up
       , Down
       , HalfUp
       , HalfDown
       , HalfEven )

    class Ceiling {}
    class Floor {}
    class Up {}
    class Down {}
    class HalfUp {}
    class HalfDown {}
    class HalfEven {}

    // OPERATORS

    // Binary type operators

    type BINTYOP =
       ( CastOp
       , IsOp
       , ToOp )

    class CastOp {}
    class IsOp {}
    class ToOp {}

    const castOp = new CastOp;
    const isOp = new IsOp;
    const toOp = new ToOp;

    // Binary operators

    type BINOP =
       ( Plus
       , Minus
       , Times
       , Divide
       , Remainder
       , LeftShift
       , RightShift
       , RightShiftUnsigned
       , BitwiseAnd
       , BitwiseOr
       , BitwiseXor
       , LogicalAnd
       , LogicalOr
       , InstanceOf
       , In
       , Equal
       , NotEqual
       , StrictEqual
       , StrictNotEqual
       , Less
       , LessOrEqual
       , Greater
       , GreaterOrEqual )

    class Plus {}
    class Minus {}
    class Times {}
    class Divide {}
    class Remainder {}
    class LeftShift {}
    class RightShift {}
    class RightShiftUnsigned {}
    class BitwiseAnd {}
    class BitwiseOr {}
    class BitwiseXor {}
    class LogicalAnd {}
    class LogicalOr {}
    class InstanceOf {}
    class In {}
    class Equal {}
    class NotEqual {}
    class StrictEqual {}
    class StrictNotEqual {}
    class Less {}
    class LessOrEqual {}
    class Greater {}
    class GreaterOrEqual {}

    var plusOp = new Plus;
    var minusOp = new Minus;
    var timesOp = new Times;
    var divideOp = new Divide;
    var remainderOp = new Remainder;
    var leftShiftOp = new LeftShift;
    var rightShiftOp = new RightShift;
    var rightShiftUnsignedOp = new RightShiftUnsigned;
    var bitwiseAndOp = new BitwiseAnd;
    var bitwiseOrOp = new BitwiseOr;
    var bitwiseXorOp = new BitwiseXor;
    var logicalAndOp = new LogicalAnd;
    var logicalOrOp = new LogicalOr;
    var instanceOfOp = new InstanceOf;
    var inOp = new In;
    var equalOp = new Equal;
    var notEqualOp = new NotEqual;
    var strictEqualOp = new StrictEqual;
    var strictNotEqualOp = new StrictNotEqual;
    var lessOp = new Less;
    var lessOrEqualOp = new LessOrEqual;
    var greaterOp = new Greater;
    var greaterOrEqualOp = new GreaterOrEqual;

    /*
        ASSIGNOP
    */

    type ASSIGNOP =
       ( Assign
       , AssignPlus
       , AssignMinus
       , AssignTimes
       , AssignDivide
       , AssignRemainder
       , AssignLeftShift
       , AssignRightShift
       , AssignRightShiftUnsigned
       , AssignBitwiseAnd
       , AssignBitwiseOr
       , AssignBitwiseXor
       , AssignLogicalAnd
       , AssignLogicalOr )

    class Assign {}
    class AssignPlus {}
    class AssignMinus {}
    class AssignTimes {}
    class AssignDivide {}
    class AssignRemainder {}
    class AssignLeftShift {}
    class AssignRightShift {}
    class AssignRightShiftUnsigned {}
    class AssignBitwiseAnd {}
    class AssignBitwiseOr {}
    class AssignBitwiseXor {}
    class AssignLogicalAnd {}
    class AssignLogicalOr {}

    const assignOp = new Assign;

    // UNOP

    type UNOP =
       ( Delete
       , Void
       , Typeof
       , PreIncr
       , PreDecr
       , PostIncr
       , PostDecr
       , UnaryPlus
       , UnaryMinus
       , BitwiseNot
       , LogicalNot
       , Type )

    class Delete {}
    class Void {}
    class Typeof {}
    class PreIncr {}
    class PreDecr {}
    class PostIncr {}
    class PostDecr {}
    class UnaryPlus{}
    class UnaryMinus {}
    class BitwiseNot {}
    class LogicalNot {}
    class Type {}

    var deleteOp = new Delete;
    var voidOp = new Void;
    var typeOfOp = new Typeof;
    var preIncrOp = new PreIncr;
    var preDecrOp = new PreDecr;
    var postIncrOp = new PostIncr;
    var postDecrOp = new PostDecr;
    var unaryPlusOp = new UnaryPlus;
    var unaryMinusOp = new UnaryMinus;
    var bitwiseNotOp = new BitwiseNot;
    var logicalNotOp = new LogicalNot;
    var typeOp = new Type;

    // EXPR

    type EXPRS = [EXPR];

    type EXPR =
       ( TernaryExpr
       , BinaryExpr
       , BinaryTypeExpr
       , UnaryExpr
       , TypeExpr
       , ThisExpr
       , YieldExpr
       , SuperExpr
       , LiteralExpr
       , CallExpr
       , ApplyTypeExpr
       , LetExpr
       , NewExpr
       , ObjectRef
       , LexicalRef
       , SetExpr
       , ListExpr
       , InitExpr
       , SliceExpr
       , GetTemp
       , GetParam )

	class TernaryExpr {
        const e1 : EXPR
        const e2 : EXPR
        const e3 : EXPR
    	function TernaryExpr (e1,e2,e3)
            : e1=e1, e2=e2, e3=e3 {}
    }

    class BinaryExpr {
        const op : BINOP
        const e1 : EXPR
        const e2 : EXPR
    	function BinaryExpr (op,e1,e2)
	        : op=op, e1=e1, e2=e2 {}
    }

    /*
    class BinaryNumberExpr extends BinaryExpr {
        const mode : NUMERIC_MODE;
        function BinaryNumberExpr (op,e1,e2,mode)
            : mode = mode, super (op,e1,e2) {}
    }
    */

    class BinaryTypeExpr {
        const op : BINTYOP
        const e1 : EXPR
        const e2 : TYPE_EXPR
	    function BinaryTypeExpr (op,e1,e2)
	        : op=op, e1=e1, e2=e2 {}
	}

    class UnaryExpr {
        const op : UNOP;
        const e1 : EXPR;
	    function UnaryExpr (op,e1)
            : op=op, e1=e1 {}
    }

    /*
    class UnaryNumberExpr extends UnaryExpr {
        const mode : NUMERIC_MODE;
        function UnaryNumberExpr (op,ex,mode)
            : mode = mode, super (op,ex) {}
    }
    */

    class TypeExpr {
        const ex : TYPE_EXPR;
        function TypeExpr (ex)
            : ex=ex {}
    }

    class ThisExpr {
    }

    class YieldExpr {
        const ex : EXPR?;
        function YieldExpr (ex=null)
            : ex=ex {}
    }

    class SuperExpr {
        const ex : EXPR?;
        function SuperExpr (ex=null)
            : ex=ex {}
    }

    class LiteralExpr {
        const literal : LITERAL;
        function LiteralExpr (literal)
            : literal = literal {}
    }

    class CallExpr {
        const expr : EXPR;
        const args : EXPRS;
        function CallExpr (expr,args)
            : expr = expr
            , args = args {}
    }

    class ApplyTypeExpr {
        const expr : EXPR;
        const args : [TYPE_EXPR];
        function ApplyTypeExpr (expr,args)
            : expr = expr
            , args = args {}
    }

    class LetExpr {
        const head : HEAD;
        const expr : EXPR;
        function LetExpr (head,expr)
            : head = head
            , expr = expr {}
    }

    class NewExpr {
        const expr : EXPR;
        const args : EXPRS;
        function NewExpr (expr,args)
            : expr = expr
            , args = args {}
    }

    class ObjectRef {
        const base : EXPR;
        const ident : IDENT_EXPR;
        const pos : POS?;
        function ObjectRef (base,ident,pos=null)
            : base = base
            , ident = ident
            , pos = pos { }
    }

    class LexicalRef {
        const ident : IDENT_EXPR;
        function LexicalRef (ident)
            : ident = ident { }
    }

    class SetExpr {
        const op : ASSIGNOP;
        const le : EXPR;
        const re : EXPR;
        function SetExpr (op,le,re)
            : op=op, le=le, re=re {}
    }

    /*
    class SetNumberExpr extends SetExpr {
        const mode : NUMERIC_MODE;
        function SetNumberExpr (op,le,re,mode)
            : mode=mode, super (op,le,re) {}
    }
    */

    class ListExpr {
        const exprs : EXPRS;
        function ListExpr (exprs)
            : exprs=exprs {}
    }

    type INIT_TARGET =
       ( VarInit
       , LetInit
       , PrototypeInit
       , InstanceInit )

    class VarInit {}
    class LetInit {}
    class PrototypeInit {}
    class InstanceInit {}

    const varInit = new VarInit;
    const letInit = new LetInit;
    const prototypeInit = new PrototypeInit;
    const instanceInit = new InstanceInit;

	class InitExpr {
        const target : INIT_TARGET;
        const head : HEAD;               // for desugaring temporaries
        const inits  //: INITS;
        function InitExpr (target, head, inits)
            : target = target
            , head = head
            , inits = inits {}
    }

    class SliceExpr {
        const e1 : EXPR;
        const e2 : EXPR;
        const e3 : EXPR;
    }

    class GetTemp {
        const n : int;
        function GetTemp (n)
            : n = n {}
    }

    class GetParam {
        const n : int;
        function GetParam (n) 
            : n = n {}
    }

    // IDENT_EXPR

    type IDENT_EXPR =
       ( Identifier
       , QualifiedExpression
       , AttributeIdentifier
       , ExpressionIdentifier
       , QualifiedIdentifier
       , TypeIdentifier
       , UnresolvedPath
       , WildcardIdentifier
       , ReservedNamespace )

    class Identifier {
        const ident : IDENT;
        const nss //: NAMESPACES;
        function Identifier (ident,nss)
            : ident = ident
            , nss = nss {}
    }

    class QualifiedExpression {
        const qual : EXPR;
        const expr : EXPR;
        function QualifiedExpression (qual,expr)
            : qual=qual
            , expr=expr {}
    }

    class AttributeIdentifier {
        const ident : IDENT_EXPR;
        function AttributeIdentifier (ident)
            : ident=ident {}
    }

    class ReservedNamespace {
        const ns: NAMESPACE;
        function ReservedNamespace (ns)
            : ns=ns {}
    }

    class ExpressionIdentifier {
        const expr: EXPR;
        const nss //: [NAMESPACE];
        function ExpressionIdentifier (expr,nss)
            : expr=expr
            , nss = nss { }
    }

    class QualifiedIdentifier {
        const qual : EXPR;
        const ident : IDENT;
        function QualifiedIdentifier (qual,ident)
            : qual=qual
            , ident=ident {}
    }

    class TypeIdentifier {
        const ident : IDENT_EXPR;
        const typeArgs : [TYPE_EXPR];
        function TypeIdentifier (ident,typeArgs)
            : ident=ident
            , typeArgs=typeArgs {}
    }

    class UnresolvedPath {
        const path /*: [IDENT] */;
        const ident : IDENT_EXPR;
        function UnresolvedPath (path,ident)
            : path=path
            , ident=ident {}
    }

    class WildcardIdentifier {}

    // LITERAL

    type LITERAL = (
        LiteralNull,
        LiteralUndefined,
        LiteralContextDecimal,
        LiteralContextDecimalInteger,
        LiteralContextHexInteger,
        LiteralDouble,
        LiteralDecimal,
        LiteralInt,
        LiteralUInt,
        LiteralBoolean,
        LiteralString,
        LiteralArray,
        LiteralXML,
        LiteralNamespace,
        LiteralObject,
        LiteralFunction,
        LiteralRegExp
    )

    class LiteralNull {}

    class LiteralUndefined {}

    class LiteralContextDecimal {
        const strValue : String;
        function LiteralContextDecimal (strValue)
            : strValue=strValue {}
    }

    class LiteralContextDecimalInteger {
        const strValue : String;
        function LiteralContextDecimalInteger (strValue)
            : strValue=strValue {}
    }

    class LiteralContextHexInteger {
        const strValue : String;
        function LiteralContextHexInteger (strValue)
            : strValue=strValue {}
    }

    class LiteralDouble {
        const doubleValue : Number;
        function LiteralDouble (doubleValue)
            : doubleValue=doubleValue { }
    }

    class LiteralDecimal {
        const decimalValue : String;
        function LiteralDecimal (str : String)
            : decimalValue = str { }  // FIXME: convert from string to decimal
    }

    class LiteralInt {
        const intValue : int;
        function LiteralInt(intValue) 
            : intValue=intValue {}
    }

    class LiteralUInt {
        const uintValue : uint;
    }

    class LiteralBoolean {
        const booleanValue : Boolean;
        function LiteralBoolean(booleanValue) 
            : booleanValue=booleanValue {}
    }

    class LiteralString {
        const strValue : String;
        function LiteralString (strValue)
            : strValue = strValue {}
    }

    class LiteralArray {
        const exprs //: [EXPR];
        const type : TYPE_EXPR;
        function LiteralArray (exprs,ty)
            : exprs = exprs
            , type = ty { }
    }

    class LiteralXML {
        const exprs : [EXPR];
    }

    class LiteralNamespace {
        const namespaceValue : NAMESPACE;
        function LiteralNamespace (namespaceValue)
            : namespaceValue = namespaceValue { }
    }

    class LiteralObject {
        const fields : LITERAL_FIELDS;
        const type : TYPE_EXPR;
        function LiteralObject (fields, ty)
            : fields = fields
            , type = ty { }}
    
    type LITERAL_FIELD = LiteralField;
    type LITERAL_FIELDS = [LiteralField];

    class LiteralField {
        const kind: VAR_DEFN_TAG;
        const ident: IDENT_EXPR;
        const expr: EXPR;
        function LiteralField (kind,ident,expr)
            : kind = kind
            , ident = ident
            , expr = expr {}
    }

    type FIELD_TYPE = FieldType;
    type FIELD_TYPES = [FIELD_TYPE];

    class LiteralFunction {
        const func : FUNC;
    }

	class LiteralRegExp {
        const src : String;
    }

    type VAR_DEFN_TAG =
        ( Const
        , Var
        , LetVar
        , LetConst )

    class Const {}
    class Var {}
    class LetVar {}
    class LetConst {}

    const constTag = new Const;
    const varTag = new Var;
    const letVarTag = new LetVar;
    const letConstTag = new LetConst;


    class VariableDefn {
        const ns: NAMESPACE;
        const isStatic: Boolean;
        const isPrototype: Boolean;
        const kind: VAR_DEFN_TAG;
        const bindings: BINDING_INITS;
        function VariableDefn (ns,isStatic,isPrototype,kind,bindings)
            : ns = ns
            , isStatic = isStatic
            , isPrototype = isPrototype
            , kind = kind
            , bindings = bindings {}
    }

    // CLS

    type CLS = Cls;

    class Cls {
        const name //: NAME;
        const baseName; //: NAME?;
        const interfaceNames; //: [NAME];
        const constructor : CTOR;
        const classHead: HEAD;
        const instanceHead: HEAD;
        const classType; //: ObjectType;
        const instanceType; //: InstanceType;
        function Cls (name,baseName,interfaceNames,constructor,classHead,instanceHead
                     ,classType,instanceType)
            : name = name
            , baseName = baseName
            , interfaceNames = interfaceNames
            , constructor = constructor
            , classHead = classHead
            , instanceHead = instanceHead
            , classType = classType
            , instanceType = instanceType {}
    }

    // FUNC

    type FUNC = Func;

    type FUNC_NAME =
       { kind : FUNC_NAME_KIND
       , ident : IDENT }

    type FUNC_NAME_KIND =
       ( Ordinary
       , Operator
       , Get
       , Set )

    class Ordinary {}
    class Operator {}
    class Get {}
    class Set {}

    class Func {
        const name //: FUNC_NAME;
        const isNative: Boolean;
        const block: BLOCK;
        const params: HEAD;
        const vars: HEAD;
        const defaults: [EXPR];
        const type /*: FUNC_TYPE*/;    // FIXME: should be able to use 'type' here
        function Func (name,isNative,block,
                       params,vars,defaults,ty)
            : name = name
            , isNative = isNative
            , block = block
            , params = params
            , vars = vars
            , defaults = defaults
            , type = ty {}
    }

    // CTOR

    type CTOR = Ctor;

    class Ctor {
        const settings : [EXPR];
        const superArgs : [EXPR];
        const func : FUNC;
        function Ctor (settings,superArgs,func)
            : settings = settings
            , superArgs = superArgs
            , func = func {}
    }

    // BINDING_INIT

    type BINDING_INITS = [[BINDING],[INIT_STEP]];

    type BINDING = Binding;

    class Binding {
        const ident : BINDING_IDENT;
        const type : TYPE_EXPR?;
        function Binding (ident,ty)  // FIXME 'type' not allowed as param name in the RI
            : ident = ident
            , type = ty { }
    }

    type BINDING_IDENT =
       ( TempIdent
       , ParamIdent
       , PropIdent )

    class TempIdent {
        const index : int;
        function TempIdent (index)
            : index = index {}}
    

    class ParamIdent {
        const index : int;
        function ParamIdent (index)
            : index = index {}
    }

    class PropIdent {
        const ident : IDENT;
        function PropIdent (ident)
            : ident = ident { }
    }

    type INIT_STEP =
       ( InitStep
       , AssignStep )

    class InitStep {
        const ident : BINDING_IDENT;
        const expr : EXPR;
        function InitStep (ident,expr)
            : ident = ident
            , expr = expr { }
    }

    class AssignStep {
        const le : EXPR;
        const re : EXPR;
        function AssignStep (le,re)
            : le = le
            , re = re {}
    }

    // FIXTURE

    type FIXTURE = (
        NamespaceFixture,
        ClassFixture,
        InterfaceFixture,
        TypeVarFixture,
        TypeFixture,
        MethodFixture,
        ValFixture,
        VirtualValFixture)
        

    class NamespaceFixture {
        const ns : NAMESPACE;
        function NamespaceFixture (ns)
            : ns = ns {}
    }

    class ClassFixture {
        const cls : CLS;
        function ClassFixture (cls)
            : cls = cls {}
    }

    class InterfaceFixture { }

    class TypeVarFixture {}

    class TypeFixture {
        const type: TYPE_EXPR;
        function TypeFixture (ty)
            : type = ty {}
    }

    class MethodFixture {
        const func : FUNC;
        const type : TYPE_EXPR;
        const isReadOnly : Boolean;
        const isOverride : Boolean;
        const isFinal : Boolean;
        function MethodFixture(func, ty, isReadOnly, isOverride, isFinal) :
            func = func,
            type = ty,
            isReadOnly = isReadOnly,
            isOverride = isOverride,
            isFinal = isFinal
        {
        }
    }

    class ValFixture {
        const type : TYPE_EXPR;
        const isReadOnly : Boolean;
        function ValFixture(ty, isReadOnly) : type=ty, isReadOnly=isReadOnly {}}
    

    class VirtualValFixture {
        const type : TYPE_EXPR;
        const getter : FUNC?;
        const setter : FUNC?;
    }

    // TYPE_EXPR

    type TYPE_EXPRS = [TYPE_EXPR];
    type TYPE_EXPR = (
        SpecialType,
        UnionType,
        ArrayType,
        TypeName,
        ElementTypeRef,
        FieldTypeRef,
        FunctionType,
        ObjectType,
        AppType,
        NullableType,
        InstanceType,
        NominalType
    )

    class SpecialType {
        const kind : SPECIAL_TYPE_KIND;
        function SpecialType(kind) : kind=kind {}}
    

    type SPECIAL_TYPE_KIND =
        ( AnyType
        , NullType
        , UndefinedType
        , VoidType )

    class AnyType { function toString() "Any" }
    class NullType { function toString() "Null" }
    class UndefinedType { function toString() "Undefined" }
    class VoidType { function toString() "Void" }

    const anyType = new SpecialType (new AnyType);
    const nullType = new SpecialType (new NullType);
    const undefinedType = new SpecialType (new UndefinedType);
    const voidType = new SpecialType (new VoidType);

    class UnionType {
        const types : [TYPE_EXPR];
        function UnionType (types)
            : types = types { }
    }

    class ArrayType {
        const types : TYPE_EXPRS;
        function ArrayType (types)
            : types = types { }
    }

    class TypeName {
        const ident : IDENT_EXPR;
        function TypeName (ident)
            : ident = ident {}
    }

    class ElementTypeRef {
        const base : TYPE_EXPR;
        const index : int;
        function ElementTypeRef (base,index)
            : base = base
            , index = index { }
    }

    class FieldTypeRef {
        const base : TYPE_EXPR;
        const ident : IDENT_EXPR;
        function FieldTypeRef (base,ident)
            : base = base
            , ident = ident { }
    }

    class FunctionType {
        const ftype : FUNC_TYPE;
    }

    type FUNC_TYPE = {
        typeParams : [IDENT],
        params: [TYPE_EXPR],
        result: TYPE_EXPR,
        thisType: TYPE_EXPR?,
        hasRest: Boolean,
        minArgs: int
    }

    class ObjectType {
        const fields : [FIELD_TYPE];
        function ObjectType (fields)
            : fields = fields { }
    }

    class FieldType {
        const ident: IDENT;
        const type: TYPE_EXPR;
        function FieldType (ident,ty)
            : ident = ident
            , type = ty {}
    }

    class AppType {
        const base : TYPE_EXPR;
        const args : [TYPE_EXPR];
        function AppType (base,args)
            : base = base
            , args = args { }
    }

    class NullableType {
        const type : TYPE_EXPR;
        const isNullable : Boolean;
        function NullableType (ty,isNullable)
            : type = ty
            , isNullable = isNullable { }
    }

    class InstanceType {
        const name : NAME;
        const typeParams : [IDENT];
        const type : TYPE_EXPR;
        const isDynamic : Boolean;
    }

    class NominalType {
        const name : NAME;
    }

    // STMTs

    type STMTS = [STMT];

    type STMT =
       ( EmptyStmt
       , ExprStmt
       , ClassBlock
       , ForInStmt
       , ThrowStmt
       , ReturnStmt
       , BreakStmt
       , ContinueStmt
       , BlockStmt
       , LabeledStmt
       , LetStmt
       , WhileStmt
       , DoWhileStmt
       , ForStmt
       , IfStmt
       , WithStmt
       , TryStmt
       , SwitchStmt
       , SwitchTypeStmt
       , DXNStmt )

    class EmptyStmt { }

    class ExprStmt {
        const expr : EXPR;
        function ExprStmt (expr)
            : expr = expr {}
    }

    class ClassBlock {
        const name //: NAME;
        const block : BLOCK;
        function ClassBlock (name,block)
            : name = name
            , block = block {}
    }

    class ForInStmt {
    }

    class ThrowStmt {
        const expr : EXPR;
        function ThrowStmt (expr)
            : expr = expr { }
    }

    class ReturnStmt {
        const expr : EXPR?;
        function ReturnStmt(expr) 
            : expr = expr { }
    }

    class BreakStmt {
        const ident : IDENT?;
        function BreakStmt (ident)
            : ident = ident { }
    }

    class ContinueStmt {
        const ident : IDENT?;
        function ContinueStmt (ident)
            : ident = ident { }
    }

    class BlockStmt {
        const block : BLOCK;
        function BlockStmt (block)
            : block = block {}
    }

    class LabeledStmt {
        const ident : IDENT;
        const stmt : STMT;
        function LabeledStmt (label,stmt)
            : ident = ident
            , stmt = stmt { }
    }

    class LetStmt {
        const block : BLOCK;
        function LetStmt (block)
            : block = block {}
    }

    class WhileStmt {
        const expr : EXPR;
        const stmt : STMT;
        const labels : [IDENT];
        function WhileStmt (expr,stmt,labels)
            : expr = expr
            , stmt = stmt
            , labels = labels {}
    }

    class DoWhileStmt {
        const expr : EXPR;
        const stmt : STMT;
        const labels : [IDENT];
        function DoWhileStmt (expr,stmt,labels)
            : expr = expr
            , stmt = stmt
            , labels = labels {}
    }

    class ForStmt {
        const vars : HEAD;
        const init : EXPR?;
        const cond : EXPR?;
        const incr : EXPR?;
        const stmt : STMT;
        const labels : [IDENT];
        function ForStmt (vars,init,cond,incr,stmt,labels)
            : vars = vars
            , init = init
            , cond = cond
            , incr = incr
            , stmt = stmt
            , labels = labels {}
    }

    class IfStmt {
        const expr : EXPR;
        const then : STMT;
        const elseOpt : STMT?;
        function IfStmt (expr,then,elseOpt)
            : expr = expr
            , then = then
            , elseOpt = elseOpt { }
    }

    class SwitchStmt {
        const expr : EXPR;
        const cases : CASES;
        const labels : [IDENT];
        function SwitchStmt (expr, cases, labels)
            : expr = expr
            , cases = cases
            , labels = labels { }
    }

    type CASE = Case;
    type CASES = [CASE];

    class Case {
        const expr : EXPR?;  // null for default
        const stmts : STMTS;
        function Case (expr,stmts)
            : expr = expr
            , stmts = stmts { }
    }

    class WithStmt {
        const expr : EXPR;
        const stmt : STMT;
        function WithStmt (expr,stmt)
            : expr = expr
            , stmt = stmt { }
    }

    class TryStmt {
        const block : BLOCK;
        const catches: CATCHES;
        const finallyBlock: BLOCK?;
        function TryStmt (block,catches,finallyBlock)
            : block = block
            , catches = catches
            , finallyBlock = finallyBlock { }
    }

    class SwitchTypeStmt {
        const expr: EXPR;
        const type: TYPE_EXPR;
        const cases: CATCHES;
        function SwitchTypeStmt (expr,ty,cases)
            : expr = expr
            , type = ty
            , cases = cases { }
    }

    type CATCH = Catch;
    type CATCHES = [CATCH];

    class Catch {
        const param: HEAD;
        const block: BLOCK;
        function Catch (param,block)
            : param = param
            , block = block { }
    }

    class DXNStmt {
    }

    /*
        BLOCK
    */

    type BLOCK = Block;

    class Block {
        const head: HEAD?;
        const stmts : STMTS;
        function Block (head,stmts)
            : head = head
            , stmts = stmts { }
    }

    type PRAGMAS = [PRAGMA];

    type PRAGMA =
        ( UseNamespace
        , UseDefaultNamespace
        , UseNumber
        , UseRounding
        , UsePrecision
        , UseStrict
        , UseStandard
        , Import )

    class UseNamespace {}
    class UseDefaultNamespace {}
    class UseNumber {}
    class UseRounding {}
    class UsePrecision {}
    class UseStrict {}
    class UseStandard {}
    class Import {}

    /*
        PACKAGE
    */


    type PACKAGE = Package;
    type PACKAGES = [PACKAGE];

    class Package {
        var name: [IDENT];
        var block: BLOCK;
        function Package (name, block)
            : name = name
            , block = block {}
    }

    /*
        PROGRAM
    */

    type DIRECTIVES =
        { pragmas: [PRAGMA]
        , stmts: STMTS }

    type PROGRAM = Program

    class Program {
        var packages: PACKAGES;
        var block: BLOCK;
        var head: HEAD;
        function Program (packages, block, head)
            : packages = packages
            , block = block
            , head = head {}
    }

    function test () {
        print ("testing ast.es");
        print (new EmptyStmt);
    }

    test();
}
