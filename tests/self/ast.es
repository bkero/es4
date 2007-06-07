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

namespace Ast

{
    use default namespace Ast;
    use namespace intrinsic;

    // POS

    type POS = 
       { file: String
       , span: int //StreamPos.span
       , sm: int // StreamPos.sourcemap
       , post_newline: Boolean }

    // BASIC TYPES

    type IDENT = String   // unicode string

    type HEAD =
       { fixtures : FIXTURES
       , inits : INITS }

    type FIXTURE_NAME = 
       ( TempName
       , PropName )

    class TempName {}
    class PropName {}

    type FIXTURES = [[FIXTURE_NAME,FIXTURE]...]

    type INITS = [[FIXTURE_NAME,EXPR]...]
	
    type NAME =
       { ns: NAMESPACE
       , id: IDENT }
	
    type MULTINAME =
       { nss: [[NAMESPACE...]...]
       , id: IDENT }
	
    // NAMESPACE

    type NAMESPACE =
	   ( IntrinsicNamespace
	   , OperatorNamespace
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

    class IntrinsicNamespace {}

    class OperatorNamespace {}

    class PrivateNamespace { 
        const name : IDENT
    }

    class ProtectedNamespace { 
        const name : IDENT 
    }

    class PublicNamespace { 
        const name : IDENT 
    }

    class InternalNamespace { 
        const name : IDENT
    }

    class UserNamespace { 
        const name : IDENT 
    }

    class AnonymousNamespace { 
        const id : int 
    }
	
    class ImportNamespace { 
        const ident : IDENT
        const ns : PublicNamespace 
    }
	
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

    /*

    Pattern for using operator expression nodes

    switch type (e:EXPR) 
    {
        case (expr:UnaryExpr) {
            let {op,ex} = expr
            switch type (op:UNOP) {
                case (_:Delete) { ...delete... }
                case (_:Void) { ...void... }
            }
        }
        case (expr:BinaryExpr) {
            ...binary expr...
        }
        case (expr:TernaryExpr) {
            ...ternary expr...
        }
        default {
            throw "unimplemented expression type"
        }
    }

    */

    // Binary type operators

    type BINTYOP =
       ( CastOp
       , IsOp
       , ToOp )

    class CastOp {}
    class IsOp {}
    class ToOp {}
    
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
       , Equals
       , NotEquals
       , StrictEquals
       , StrictNotEquals
       , Less
       , LessOrEqual
       , Greater
       , GreaterOrEqual
       , Comma )

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
    class Equals {}
    class NotEquals {}
    class StrictEquals {}
    class StrictNotEquals {}
    class Less {}
    class LessOrEqual {}
    class Greater {}
    class GreaterOrEqual {}
    class Comma {}

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

    // EXPR
    
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

    class BinaryNumberExpr extends BinaryExpr {
        const mode : NUMERIC_MODE;
        function BinaryNumberExpr (op,e1,e2,mode)
            : mode = mode, super (op,e1,e2) {}
    }
	
    class BinaryTypeExpr {
        const op : BINTYOP
        const e1 : EXPR
        const e2 : TYPE_EXPR
	    function BinaryTypeExpr (op,e1,e2) 
	        : op=op, e1=e1, e2=e2 {}
	}
	
    class UnaryExpr {
        const op : UNOP;
        const ex : EXPR;
	    function UnaryExpr (op,ex)
            : op=op, ex=ex {}
    }

    class UnaryNumberExpr extends UnaryExpr {
        const mode : NUMERIC_MODE;
        function UnaryNumberExpr (op,ex,mode)
            : mode = mode, super (op,ex) {}
    }
    
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
        const func : EXPR;
        const args : [EXPR...];
        function CallExpr (func,args)
            : func = func
            , args = args {}
    }

    class ApplyTypeExpr {
        const expr : EXPR;
        const args : [TYPE_EXPR...];
        function ApplyTypeExpr (expr,args)
            : expr = expr
            , args = args {}
    }

    class LetExpr {
        const binds : BINDING_INITS;
        const head : HEAD;
        const body : EXPR;
        function LetExpr (binds,head,body)
            : binds = binds
            , head = head
            , body = body {}
    }

    class NewExpr {
        const ctor : EXPR;
        const args : [EXPR...];
    }

    class ObjectRef {
        const base : EXPR;
        const ident : IDENT_EXPR;
        const pos : POS;
    }

    class LexicalRef {
        const ident : IDENT_EXPR;
        const pos : POS?;
        function LexicalRef (ident,pos=null) 
            : ident = ident
            , pos = pos {}
    }
    

    class SetExpr {
        const op : ASSIGNOP;
        const le : EXPR;
        const re : EXPR;
        function SetExpr (op,le,re) 
            : op=op, le=le, re=re {}
    }

    class SetNumberExpr extends SetExpr {
        const mode : NUMERIC_MODE;
        function SetNumberExpr (op,le,re,mode) 
            : mode=mode, super (op,le,re) {}
    }

    class ListExpr {
        const exprs : [EXPR...];
    }

    type INIT_TARGET =
       ( Hoisted
       , Local
       , Prototype )

    class Hoisted {}
    class Local {}
    class Prototype {}

	class InitExpr {
        const target : INIT_TARGET;
        const head   : HEAD;               // for desugaring temporaries
        const inits  : INITS;
    }

    class SliceExpr {
        const e1 : EXPR;
        const e2 : EXPR;
        const e3 : EXPR;
    }

    class GetTemp {
        const n : int;
    }

    class GetParam {
        const n : int;
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
       , WildcardIdentifier )
	    
    class Identifier {
        const ident : IDENT;
        private const nss : [[NAMESPACE...]...];
        function Identifier (ident) 
            : ident = ident {}
        function set opennss (nss) { 
            this.nss = nss 
        }
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
	
    class ExpressionIdentifier { 
        const expr: EXPR;
        private const nss : [[NAMESPACE...]...];
        function ExpressionIdentifier (expr)
            : expr=expr {}
        function set opennss (nss) {
            this.nss = nss;
        }
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
        const typeArgs : [TYPE_EXPR...];
        function TypeIdentifier (ident,typeArgs)
            : ident=ident
            , typeArgs=typeArgs {}
    }
    
    class UnresolvedPath {
        const path : [IDENT...];
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
        const doubleValue : double;
        function LiteralDouble (doubleValue)
            : doubleValue=doubleValue {}
    }
	
    class LiteralDecimal {
        const decimalValue : decimal;
    }
	
    class LiteralInt {
        const intValue : int;
    }
	
    class LiteralUInt {
        const uintValue : uint;
    }
	
    class LiteralBoolean {
        const booleanValue : Boolean;
    }
	
    class LiteralString {
        const strValue : String;
    }
	
    class LiteralArray {
        const exprs : [EXPR...];
        const type : TYPE_EXPR;
    }

    class LiteralXML {
        const exprs : [EXPR...];
    }

    class LiteralNamespace {
        const namespaceValue : NAMESPACE;
    }

    class LiteralObject {
        const fields : [FIELD];
        const type : TYPE_EXPR;
    }

    type FIELD =
       { kind : VAR_DEFN_TAG
       , name : IDENT_EXPR
       , init : EXPR }

    type FIELD_TYPE =
       { name : IDENT
       , type : TYPE_EXPR
    }

    class LiteralFunction {
        const func : FUNC;
    }

	class LiteralRegExp {
        const src : String;
    }

    // CLS

    type CLS = Cls;

    class Cls {
        const name : NAME;
        const baseName : NAME?;
        const interfaceNames : [NAME...];
        const constructor : CTOR?;
        const classFixtures : FIXTURES;
        const instanceFixtures : FIXTURES;
        const instanceInits : HEAD;
        const classType : ObjectType;
        const instanceType : InstanceType;
        function Cls (name,baseName,interfaceNames,constructor)
            : name = name
            , baseName = baseName
            , interfaceNames = interfaceNames
            , constructor = constructor
            , classFixtures = []
            , instanceFixtures = []
            , instanceInits = []
            , classType = null
            , instanceType = null {}
    }

    // FUNCS

    type FUNC = Func;

    type FUNC_NAME =
       { kind : FUNC_NAME_KIND
       , ident : IDENT }

    type FUNC_NAME_KIND =
       ( Ordinary
       , Operator
       , Get
       , Set
       , Call
       , Has )

    class Ordinary {}
    class Operator {}
    class Get {}
    class Set {}
    class Call {}
    class Has {}

    class Func { 
        const name: FUNC_NAME;
        const fsig: FUNC_SIG;
        const isNative: Boolean;
        const block: BLOCK;
        const params: HEAD;
        const defaults: [EXPR...];
        const type: FUNC_TYPE;    // FIXME: should be able to use 'type' here
        function Func (name,fsig,isNative,block,
                       params,defaults,ty)
            : name = name
            , fsig = fsig
            , isNative = isNative
            , block = block
            , params = params
            , defaults = defaults
            , type = ty {}
    }

    type FUNC_SIG = FunctionSignature;

    class FunctionSignature {
        const typeParams : [IDENT...];
        const params : BINDING_INITS;
        const paramTypes : [TYPE_EXPR...];
        const defaults : [EXPR];
        const ctorInits : [BINDING_INITS,[EXPR...]]?;  /* [settings, super args] */
        const returnType : TYPE_EXPR;
        const thisType : TYPE_EXPR?;
        const hasRest : Boolean;
    }

    // CTORS

    type CTOR = Ctor;

    class Ctor {
        const settings : HEAD;
        const superArgs : [EXPR...];
        const func : FUNC;
    }

    // BINDINGS

    type BINDING_INITS = [[BINDING...],[INIT_STEP...]];

    type BINDING = Binding;

    class Binding {
        const ident : BINDING_IDENT;
        const type : TYPE_EXPR;
    }

    type BINDING_IDENT =
       ( TempIdent
       , ParamIdent
       , PropIdent )

    class TempIdent {
        const n : int;
    }

    class ParamIdent {
        const n : int;
    }

    class PropIdent {
        const ident : IDENT;
    }

    type INIT_STEP =
       ( InitStep
       , AssignStep )

    class InitStep {
        const ident : BINDING_IDENT;
        const expr : EXPR;
    }

    class AssignStep {
        const le : EXPR;
        const re : EXPR;
    }

    // FIXTURES

    type FIXTURE = (
        NamespaceFixture,
        ClassFixture,
        InterfaceFixture,
        TypeVarFixture,
        TypeFixture,
        MethodFixture,
        ValFixture,
        VirtualValFixture
    )

    class NamespaceFixture {
        const ns : NAMESPACE
    }

    class ClassFixture {
        const cls : CLS;
    }

    class InterfaceFixture { }

    class TypeVarFixture {}

    class TypeFixture {}

    class MethodFixture {
        const func : FUNC;
        const type : TYPE_EXPR;
        const isReadOnly : Boolean;
        const isOverride : Boolean;
        const isFinal : Boolean;
    }

    class ValFixture {
        const type : TYPE_EXPR;
        const isReadOnly : Boolean;
    }
	
    class VirtualValFixture {
        const type : TYPE_EXPR;
        const getter : FUNC_DEFN?;
        const setter : FUNC_DEFN?;
    }

    // TYPES

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
    }

    type SPECIAL_TYPE_KIND =
        ( AnyType
        , NullType
        , UndefinedType
        , VoidType )

    class AnyType {}
    class NullType {}
    class UndefinedType {}
    class VoidType {}

    class UnionType {
        const types : [TYPE_EXPR...];
    }

    class ArrayType {
        const types : [TYPE_EXPR...];
    }

    class TypeName {
        const ident : IDENT_EXPR;
    }

    class ElementTypeRef {
        const base : IDENT_EXPR;
        const index : int;
    }

    class FieldTypeRef {
        const base : IDENT_EXPR;
        const ident : IDENT;
    }

    class FunctionType {
        const ftype : FUNC_TYPE;
    }

    type FUNC_TYPE = { 
        typeParams : [IDENT...],
        params: [TYPE_EXPR...],
        result: TYPE_EXPR,
        thisType: TYPE_EXPR?,
        hasRest: Boolean,
        minArgs: int 
    }

    class ObjectType {
        const types : [FIELD_TYPE...];
    }

    class AppType {
        const base : TYPE_EXPR;
        const args : [TYPE_EXPR...];
    }

    class NullableType {
        const type : TYPE_EXPR;
        const isNullable : Boolean;
    }

    class InstanceType {
        const name : NAME;
        const typeParams : [IDENT...];
        const type : TYPE_EXPR;
        const isDynamic : Boolean;
    }

    class NominalType {
        const name : NAME;
    }

    // STMTs

    type STMT =
       ( EmptyStmt
       , ExprStmt
       , InitStmt
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

    class InitStmt {
    }

    class ClassBlock {
        const ns : EXPR?;
        const ident : IDENT;
        const name : NAME?;
        const block : BLOCK;
    }

    class ForInStmt {
    }

    class ThrowStmt {
        const expr : EXPR;
    }

    class ReturnStmt {
        const expr : EXPR?;
    }

    class BreakStmt {
        const ident : IDENT?;
    }

    class ContinueStmt {
        const ident : IDENT?;
    }

    class BlockStmt {
        const block : BLOCK;
    }

    class LabeledStmt {
        const label : IDENT;
        const stmt : STMT;
    }

    class LetStmt {
        const block : BLOCK;
    }

    class WhileStmt {
        const expr : EXPR;
        const body : STMT;
        const labels : [IDENT...];
        const fixtures : FIXTURES?
    }
    
    class DoWhileStmt {
        const expr : EXPR;
        const body : STMT;
        const labels : [IDENT...];
        const fixtures : FIXTURES?
    }

    class ForStmt {
    }


    class IfStmt {
    }

    class WithStmt {
    }

    class TryStmt {
    }

    class SwitchStmt {
    }

    class SwitchTypeStmt {
    }

    class DXNStmt {
    }

    // DEFN

    type DEFN = (
        ClassDefn,
        VariableDefn,
        FunctionDefn,
        ConstructorDefn,
        InterfaceDefn,
        NamespaceDefn,
        TypeDefn )

    type CLASS_DEFN =
        { ident: IDENT
        , ns: EXPR?
        , isNonnullable: Boolean
        , isDynamic: Boolean
        , isFinal: Boolean
        , params: [IDENT...]
        , base: IDENT_EXPR
        , implements: [IDENT_EXPR...]
        , classDefns: [DEFN...]
        , instanceDefns: [DEFN...]
        , instanceStmts: [STMT...]
        , ctorDefn: CTOR? }

    class ClassDefn {
        const ident: IDENT;
        const ns: EXPR?;
        const isNonnullable: Boolean;
        const isDynamic: Boolean;
        const isFinal: Boolean;
        const params: [IDENT...];
        const baseIdent: IDENT_EXPR?;  /* STATIC_IDENT_EXPR */
        const interfaceIdents: [IDENT_EXPR...]; /* STATIC_IDENT_EXPR list */
        const classDefns: [DEFN...];
        const instanceDefns: [DEFN...];
        const instanceStmts: [STMT...];
        const ctorDefn: CTOR?;
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

    type VAR_DEFN = VariableDefn

    class VariableDefn {
        const kind: VAR_DEFN_TAG;
        const ns: EXPR?;
        const isStatic: Boolean;
        const isPrototype: Boolean;
        const bindings: BINDING_INITS;
    }

    type FUNC_DEFN = FunctionDefn

    class FunctionDefn {
    }

    class ConstructorDefn {
    }

    class InterfaceDefn {
    }

    class NamespaceDefn {
    }

    class TypeDefn {
    }

    /*
        BLOCK
    */

    type BLOCK = Block

    class Block {
        const pragmas: [PRAGMA...];
        const defns: [DEFN...];
        const head: HEAD?;
        const body: [STMT...];
        const pos: POS?
    }

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

    type PACKAGE = Package

    class Package {
        var name: [IDENT...];
        var block: BLOCK;
        function Package (name, block)
            : name = name
            , block = block {}
    }

    /*
        PROGRAM
    */

    type PROGRAM = Program

    class Program {
        var packages: [PACKAG...];
        var fixtures: FIXTURES?;
        var block: BLOCK;
        function Program (packages, fixtures, block)
            : packages = packages
            , fixtures = fixtures
            , block = block { }
    }

    public function test () {
        intrinsic::print (new EmptyStmt)
    }

    test()
}
