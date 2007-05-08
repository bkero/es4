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

package Ast
{

    // BASIC TYPES

    type IDENT = string   // unicode string

    type HEAD = {
        fixtures : FIXTURES,
        inits : INITS
    }

    type FIXTURES = [[FIXTURE_NAME,FIXTURE]];
    type INITS = [[FIXTURE_NAME,EXPR]];
	
    // NAMESPACE

    type NAMESPACE = (
	    Intrinsic,
	    OperatorNamespace,
	    Private,
	    Protected,
	    Public,
	    Internal,
	    UserNamespace,
	    AnonUserNamespace,
	    LimitedNamespace 
    )

    class Intrinsic {}

    class OperatorNamespace {}

    class Private { 
        const name : IDENT
    }

    class Protected { 
        const name : IDENT 
    }

    class Public { 
        const name : IDENT 
    }

    class Internal { 
        const name : IDENT
    }

    class UserNamespace { 
        const name : IDENT 
    }

    class AnonUserNamespace { 
        const id : int 
    }
	
    class LimitedNamespace { 
        const ident : IDENT
        const ns : Public 
    }
	
    // NAME

    type NAME = {
        ns: NAMESPACE,
        id: IDENT
    }
	
    class MULTINAME {
        const nss: [[NAMESPACE]];
	    const id: IDENT;
    }
	
    // NUMBERS
	
    class DecimalType {}
    class DoubleType {}
    class IntType {}
    class UIntType {}
    class NumberType {}

    type NUMBER_TYPE = (
        DecimalType,
        DoubleType,
        IntType,
        UIntType,
        NumberType
    )
                
    type NUMERIC_MODE = {
        numberType : int,
        roundingMode: int,
        precision: int
    }

    // OPERATORS

    // Binary type operators

    type BINTYPEOP = (
        Cast,
        Is,
        To
    )
    
	class Cast {}
    
	class To {}
    
	class Is {}

    // Binary operators

    type BINOP = ( 
        Plus,
        Minus,
        Times,
    	Divide,
        Remainder,
        LeftShift,
        RightShift,
        RightShiftUnsigned,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        LogicalAnd,
        LogicalOr,
        InstanceOf,
        In,
        Equals,
        NotEquals,
        StrictEquals,
        StrictNotEquals,
        Less,
        LessOrEqual,
        Greater,
        GreaterOrEqual,
        Comma
    )
    
    class Plus { 
        const mode : NUMERIC_MODE;
        function Plus (mode) : mode = mode {}
    }
    
    class Minus { 
	    const mode : NUMERIC_MODE
        function Minus (mode) : mode = mode {}
    }
	
    class Times {
        const mode : NUMERIC_MODE
        function Times (mode) : mode = mode {}
    }

    class Divide {
        const mode : NUMERIC_MODE
        function Divide (mode) : mode = mode {}
    }
	
    class Remainder {
        const mode : NUMERIC_MODE
        function Remainder (mode) : mode = mode {}
    }
	
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
    
    class Equals {
        const mode : NUMERIC_MODE
    }
	
    class NotEquals {
        const mode : NUMERIC_MODE
    }
	
    class StrictEquals {
        const mode : NUMERIC_MODE
    }
	
    class StrictNotEquals {
        const mode : NUMERIC_MODE
    }
	
    class Less {
        const mode : NUMERIC_MODE
    }
	
    class LessOrEquals {
        const mode : NUMERIC_MODE
    }
	
    class Greater {
        const mode : NUMERIC_MODE
    }
	
    class GreaterOrEquals {
        const mode : NUMERIC_MODE
    }
	
    class Comma {}

    // Assignment operators
    
    type ASSIGNOP = (
        Assign,
        AssignPlus,
        AssignMinus,
        AssignTimes,
        AssignDivide,
        AssignRemainder,
        AssignLeftShift,
        AssignRightShift,
        AssignRightShiftUnsigned,
        AssignBitwiseAnd,
        AssignBitwiseOr,
        AssignBitwiseXor,
        AssignLogicalAnd,
        AssignLogicalOr
    )

    class Assign {}
    
    class AssignPlus {
        const mode : NUMERIC_MODE
    }
	
    class AssignMinus {
        const mode : NUMERIC_MODE
    }
	
    class AssignTimes {
        const mode : NUMERIC_MODE
    }
	
    class AssignDivide {
        const mode : NUMERIC_MODE
    }
	
    class AssignRemainder {
        const mode : NUMERIC_MODE
    }
	
    class AssignLeftShift {}

    class AssignRightShift {}

    class AssignRightShiftUnsigned {}

    class AssignBitwiseAnd {}

    class AssignBitwiseOr {}

    class AssignBitwiseXor {}

    class AssignLogicalAnd {}

    class AssignLogicalOr {}

    // Unary operators

    type UNOP = (
        Delete,
        Void,
        Typeof,
        PreIncrement,
        PreDecrement,
        PostIncrement,
        PostDecrement,
        UnaryPlus,
        UnaryMinus,
        BitwiseNot,
        LogicalNot,
        Type
    )

    class Delete {}

    class Void {}

    class Typeof {}

    class PreIncrement {
        const mode : NUMERIC_MODE
    }
	
    class PreDecrement {
        const mode : NUMERIC_MODE
    }
	
    class PostIncrement {
        const mode : NUMERIC_MODE
    }
	
    class PostDecrement {
        const mode : NUMERIC_MODE
    }
	
    class UnaryPlus {
        const mode : NUMERIC_MODE
    }
	
    class UnaryMinus {
        const mode : NUMERIC_MODE
    }

    class BitwiseNot {}

    class LogicalNot {}

    class Type {}

    // EXPR
    
    type EXPR = (
        TernaryExpr,
        BinaryExpr,
        BinaryTypeExpr,
        UnaryExpr,
        TypeExpr,
        ThisExpr,
        YieldExpr,
        SuperExpr,
        LiteralExpr,
        CallExpr,
        ApplyTypeExpr,
        LetExpr,
        NewExpr,
        ObjectRef,
        LexicalRef,
        SetExpr,
        ListExpr,
        InitExpr,
        SliceExpr,
        GetTemp,
        GetParam
    )
    
    type INIT_TARGET = (
        Hoisted,
        Local,
        Prototype
    )

    type FIXTURE_NAME = (
        TempName,
        PropName
    )

	class TernaryExpr {
    	const op : TRIOP
        const e1 : EXPR
        const e2 : EXPR
        const e3 : EXPR
    	function TernaryExpr (op,e1,e2,e3) 
            : op=op, e1=e1, e2=e2, e3=e3 {}
    }
    
    class BinaryExpr {
        const op : BINOP
        const e1 : EXPR
        const e2 : EXPR
    	function BinaryExpr (op,e1,e2) 
	        : op=op, e1=e1, e2=e2 {}
    }
	
    class BinaryTypeExpr {
        const op : BINTYOP
        const e1 : EXPR
        const e2 : TYPE_EXPR
	function BinaryTypeExpr (op,e1,e2) 
	        : op=op, e1=e1, e2=e2 {}
	}
	
    class UnaryExpr {
        const op : UNOP
        const ex : EXPR
	function UnaryExpr (op,ex)
            : op=op, ex=ex {}
    }
    
    class TypeExpr {
	const ex : TYPE_EXPR
    }
    
    class ThisExpr {}
	
    class YieldExpr {
        const ex : EXPR?
    }
    
    class SuperExpr {
        const ex : EXPR?
    }

    class LiteralExpr {
        const literal : LITERAL
    }

    class CallExpr {
        const func : EXPR;
        const args : [EXPR];
    }

    class ApplyTypeExpr {
        const expr : EXPR;
        const args : [TYPEEXPR];
    }

    class LetExpr {
        const binds : DEFNS;
        const head : HEAD;
        const body : EXPR;
    }

    class NewExpr {
        const ctor : EXPR;
        const args : [EXPR];
    }

    class ObjectRef {
        const base : EXPR;
        const ident : IDENT_EXPR;
        const pos : POS;
    }

    class LexicalRef {
        const ident : IDENT_EXPR;
        const pos : POS;
    }

    class SetExpr {
        const op : ASSIGNOP;
        const le : EXPR;
        const re : EXPR;
    }

    class ListExpr {
        const exprs : [EXPR];
    }

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

    type IDENT_EXPR = (
        Identifier,
        QualifierExpression,
        AttributeIdentifier,
        ExpressionIdentifier,
        QualifiedIdentifier,
        TypeIdentifier,
        UnresolvedPath,
        WildcardIdentifier 
    )
	    
    class Identifier {
        const ident : IDENT;
        const openNamespaces : [[NAMESPACE]];
        function Identifier (ident) : ident = ident {}
        function set opennss (v) { openNamespaces = v }
    }
	
    class QualifiedExpression {
        const qual : EXPR
        const expr : EXPR 
    }
    
    class AttributeIdentifier {
        const ident : IDENT_EXPR
    }
	
    class ExpressionIdentifier { 
        const expr: EXPR;
        const openNamespaces : [[NAMESPACE]];
    }
	
    class QualifiedIdentifier { 
        const qual : EXPR;
        const ident : IDENT;
    }
    
    class TypeIdentifier { 
        const ident : IDENT_EXPR
        const typeArgs : [TYPE_EXPR]
    }
    
    class UnresolvedPath {
        const path : [IDENT];
        const ident : IDENT_EXPR;
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
        const strValue : string;
    }

    class LiteralContextDecimalInteger {
        const strValue : string;
    }
	
    class LiteralContextHexInteger {
        const strValue : string;
    }
	
    class LiteralDouble {
        const doubleValue : double;
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
        const booleanValue : boolean;
    }
	
    class LiteralString {
        const strValue : string;
    }
	
    class LiteralArray {
        const exprs : [EXPR];
        const type : TYPE_EXPR;
    }

    class LiteralXML {
        const exprs : [EXPR];
    }

    class LiteralNamespace {
        const namespaceValue : NAMESPACE;
    }

    class LiteralObject {
        const fields : [FIELD];
        const type : TYPE_EXPR;
    }

    type FIELD = {
        kind : VAR_DEFN_TAG,
        name : IDENT_EXPR,
        init : EXPR
    }

    type FIELD_TYPE = {
        name : IDENT,
        type : TYPE_EXPR
    }

    class LiteralFunction {
        const func : FUNC;
    }

	class LiteralRegExp {
        const src : string;
    }

    // CLS

    type CLS = Cls;

    class Cls {
        const name : NAME;
        const extnds : NAME?;  // fixme: extends should work
        const impls : [NAME];  // fixme: implements should work
        const classFixtures : FIXTURES;
        const instanceFixutres : FIXTURES;
        const instanceInits : HEAD;
        const constructor : CTOR?;
        const classType : OBJECT_TYPE;
        const instanceType : INSTANCE_TYPE;
    }

    // FUNC

    type FUNC = Func;

    class Func { 
        const name: FUNC_NAME;
        const fsig: FUNC_SIG;
        const isNative: bool;
        const block: BLOCK;
        const param: HEAD;
        const defaults: [EXPR];
        const ty: FUNC_TYPE;
        function Func () {}
    }

    type FUNC_SIG = FunctionSignature;

    class FunctionSignature {
        const typeParams : [IDENT];
        const params : DEFNS;
        const paramTypes : [TYPE_EXPR];
        const defaults : [EXPR];
        const ctorInits : CTOR_INITS;
        const returnType : TYPE_EXPR;
        const thisType : TYPE_EXPR?;
        const hasRest : bool;
    }

    // CTOR

    type CTOR = Ctor;

    class Ctor {
        const settings : HEAD;
        const superArgs : [EXPR];
        const func : FUNC;
    }

    // BINDING

    type BINDING_INITS = [[BINDING],[INIT_STEP]];

    type BINDING = Binding;

    class Binding {
        const ident : BINDING_IDENT;
        const type : TYPE_EXPR;
    }

    type BINDING_IDENT = (
        TempIdent,
        ParamIdent,
        PropIdent
    )

    class TempIdent {
        const n : int;
    }

    class ParamIdent {
        const n : int;
    }

    class PropIdent {
        const ident : IDENT;
    }

    type INIT_STEP = (
        InitStep,
        AssignStep
    )

    class InitStep {
        const ident : BINDING_IDENT;
        const expr : EXPR;
    }

    class AssignStep {
        const le : EXPR;
        const re : EXPR;
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
        VirtualValFixture
    )

    class ClassFixture {
        const cls : CLS;
    }

    class InterfaceFixture { }

    class TypeVarFixture {}

    class TypeFixture {}

    class MethodFixture {
        const func : FUNC;
        const type : TYPE_EXPR;
        const isReadOnly : boolean;
        const isOverride : boolean;
        const isFinal : boolean;
    }

    class ValFixture {
        const type : TYPE_EXPR;
        const isReadOnly : boolean;
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

    class UnionType {
        const types : [TYPE_EXPR];
    }

    class ArrayType {
        const types : [TYPE_EXPR];
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
        typeParams : [IDENT],
        params: [TYPE_EXPR],
        result: TYPE_EXPR,
        thisType: TYPE_EXPR?,
        hasRest: bool,
        minArgs: int 
    }

    class ObjectType {
        const types : [FIELD_TYPE];
    }

    class AppType {
        const base : TYPE_EXPR;
        const args : [TYPE_EXPR];
    }

    class NullableType {
        const type : TYPE_EXPR;
        const isNullable : boolean;
    }

    class InstanceType {
        const name : NAME;
        const typeParams : [IDENT];
        const type : TYPE_EXPR;
        const isDynamic : boolean;
    }

    class NominalType {
        const name : NAME;
    }

    // STMTs

    type STMT = (
        EmptyStmt,
        ExprStmt,
        InitStmt,
        ClassBlock,
        ForInStmt,
        ThrowStmt,
        ReturnStmt,
        BreakStmt,
        ContinueStmt,
        BlockStmt,
        LabeledStmt,
        LetStmt,
        WhileStmt,
        DoWhileStmt,
        ForStmt,
        IfStmt,
        WithStmt,
        TryStmt,
        SwitchStmt,
        SwithTypeStmt,
        DXNStmt
    )

    class EmptyStmt { }

    class ExprStmt {
        const expr : EXPR;
    }

    class InitStmt {
    }

    class ClassStmt {
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
        const ident : IDENt?;
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
        const labels : [IDENT];
        const fixtures : FIXTURES?
    }
    
    class DoWhileStmt {
        const expr : EXPR;
        const body : STMT;
        const labels : [IDENT];
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
        TypeDefn
    )

    public function test () {
        intrinsic::print (new EmptyStmt)
    }

    test()
}

