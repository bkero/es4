(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
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
 *)
(* A sketch of the ES4 AST in SML *)

structure Ast = struct

type SOURCE_POS = { line: int, col: int }

type LOC = { file: string, span: SOURCE_POS * SOURCE_POS, post_newline: bool }

type IDENT = Ustring.STRING

type UNIT_NAME = IDENT list

type RIB_ID = int

datatype NAMESPACE =
         Intrinsic
       | OperatorNamespace
       | Private of IDENT
       | Protected of IDENT
       | Public of IDENT
       | Internal of IDENT
       | UserNamespace of Ustring.STRING
       | AnonUserNamespace of int
       | LimitedNamespace of (IDENT * NAMESPACE)

type NAME = { ns: NAMESPACE, id: IDENT }

type MULTINAME = { nss: NAMESPACE list list, id: IDENT }

datatype BINTYPEOP =
         Cast
       | Is
       | To

datatype BINOP =
         Plus
       | Minus
       | Times
       | Divide
       | Remainder
       | LeftShift
       | RightShift
       | RightShiftUnsigned
       | BitwiseAnd
       | BitwiseOr
       | BitwiseXor
       | LogicalAnd
       | LogicalOr
       | InstanceOf
       | In
       | Equals 
       | NotEquals 
       | StrictEquals 
       | StrictNotEquals 
       | Less 
       | LessOrEqual 
       | Greater 
       | GreaterOrEqual 
       | Comma

datatype ASSIGNOP =
         Assign
       | AssignPlus 
       | AssignMinus 
       | AssignTimes 
       | AssignDivide 
       | AssignRemainder 
       | AssignLeftShift
       | AssignRightShift
       | AssignRightShiftUnsigned
       | AssignBitwiseAnd
       | AssignBitwiseOr
       | AssignBitwiseXor
       | AssignLogicalAnd
       | AssignLogicalOr

datatype UNOP =
         Delete
       | Void
       | Typeof
       | PreIncrement 
       | PreDecrement 
       | PostIncrement 
       | PostDecrement 
       | UnaryPlus 
       | UnaryMinus 
       | BitwiseNot
       | LogicalNot
       | Type

datatype VAR_DEFN_TAG =
         Const
       | Var
       | LetVar
       | LetConst

datatype SPECIAL_TY =
         Any
       | Null
       | Undefined
       | VoidType

datatype PRAGMA =
         UseNamespace of EXPR
       | UseDefaultNamespace of EXPR
       | UseDecimalContext of EXPR
       | UseStrict
       | UseStandard
       | Import of
           { package: IDENT list,
             name: IDENT,
             alias: IDENT option }

     and FUNC_NAME_KIND =
         Ordinary
       | Operator
       | Get
       | Set
       | Call
       | Has

     and TY = 
         Ty of 
         { expr: TYPE_EXPR,
           ribId: RIB_ID option }
         
     and CLS =
         Cls of
           { name: NAME,
             typeParams: IDENT list,
             nonnullable: bool,
             dynamic: bool,
             extends: TY option,
             implements: TY list,
             classRib: RIB,
             instanceRib: RIB,
             instanceInits: HEAD,
             constructor: CTOR option,
             classType: TY,  (* ObjectType *)
             instanceType: TY }

     and IFACE =
         Iface of
           { name: NAME,
             typeParams: IDENT list,
             nonnullable: bool,
             extends: TY list,
             instanceRib: RIB,
             instanceType: TY }

     and CTOR =
         Ctor of 
         { settings: HEAD, (* FIXME should be a EXPR list of LetExpr of InitExpr *)
           superArgs: EXPR list,
           func: FUNC }

     and FUNC =
         Func of 
         { name: FUNC_NAME,
           fsig: FUNC_SIG,
           native: bool,
           block: BLOCK option, (* NONE => abstract *)
           param: HEAD,
           defaults: EXPR list,
           ty: TY,
           loc: LOC option }
         
     and DEFN =
         ClassDefn of CLASS_DEFN
       | VariableDefn of VAR_DEFN
       | FunctionDefn of FUNC_DEFN
       | ConstructorDefn of CTOR_DEFN
       | InterfaceDefn of INTERFACE_DEFN
       | NamespaceDefn of NAMESPACE_DEFN
       | TypeDefn of TYPE_DEFN

     and FUNC_SIG =
         FunctionSignature of 
         { typeParams: IDENT list,
           params: BINDINGS,
           paramTypes: TYPE_EXPR list,
           defaults: EXPR list,
           ctorInits: (BINDINGS * EXPR list) option, (* settings + super args *)
           returnType: TYPE_EXPR,
           thisType: TYPE_EXPR option,
           hasRest: bool }

     and BINDING =
         Binding of
           { ident: BINDING_IDENT,    (* FIXME: use tuple *)
             ty: TYPE_EXPR }

     and BINDING_IDENT =
         TempIdent of int
       | ParamIdent of int
       | PropIdent of IDENT

     and INIT_STEP =   (* used to encode init of bindings *)
         InitStep of (BINDING_IDENT * EXPR)
       | AssignStep of (EXPR * EXPR)

     and TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | UnionType of TYPE_EXPR list
       | ArrayType of TYPE_EXPR list
       | TypeName of IDENT_EXPR
       | ElementTypeRef of (TYPE_EXPR * int)
       | FieldTypeRef of (TYPE_EXPR * IDENT)
       | FunctionType of FUNC_TYPE
       | ObjectType of FIELD_TYPE list
       | AppType of 
         { base: TYPE_EXPR,
           args: TYPE_EXPR list }
       | LamType of 
         { params: IDENT list,
           body: TYPE_EXPR }
       | NullableType of 
         { expr:TYPE_EXPR,
           nullable:bool }
       | InstanceType of INSTANCE_TYPE

     and STMT =
         EmptyStmt
       | ExprStmt of EXPR
       | InitStmt of {
             kind: VAR_DEFN_TAG,
             ns: EXPR option,
             prototype: bool,
             static: bool,
             temps: BINDINGS,
             inits: INIT_STEP list }
       | ClassBlock of
           { ns: EXPR option,
             ident: IDENT,
             name: NAME option,
             block: BLOCK }
       | ForInStmt of FOR_ENUM_STMT
       | ThrowStmt of EXPR
       | ReturnStmt of EXPR
       | BreakStmt of IDENT option
       | ContinueStmt of IDENT option
       | BlockStmt of BLOCK
       | LabeledStmt of (IDENT * STMT)
       | LetStmt of BLOCK
       | WhileStmt of WHILE_STMT
       | DoWhileStmt of WHILE_STMT
       | ForStmt of FOR_STMT
       | IfStmt of {
             cnd: EXPR,
             thn: STMT,
             els: STMT }
       | WithStmt of {
             obj: EXPR,
             ty: TY,
             body: STMT }
       | TryStmt of {
             block: BLOCK,
             catches: CATCH_CLAUSE list,
             finally: BLOCK option }

       | SwitchStmt of {         (* FIXME: needs HEAD, DEFNS for defns hoisted from body *)
             cond: EXPR,
             labels: IDENT list,
             cases: CASE list }
       | SwitchTypeStmt of {
             cond: EXPR,
             ty: TY,
             cases: CATCH_CLAUSE list }
       | DXNStmt of {
             expr: EXPR }

     and EXPR =
         TernaryExpr of (EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINTYPEOP * EXPR * TY)
       | ExpectedTypeExpr of (TYPE_EXPR * EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TY
       | ThisExpr
       | YieldExpr of EXPR option
       | SuperExpr of EXPR option
       | LiteralExpr of LITERAL
       | CallExpr of {
             func: EXPR,
             actuals: EXPR list }
       | ApplyTypeExpr of {
             expr: EXPR,  (* apply expr to type list *)
             actuals: TY list }
       | LetExpr of {
             defs: BINDINGS,
             body: EXPR,
             head: HEAD option }
       | NewExpr of {
             obj: EXPR,
             actuals: EXPR list }
       | ObjectRef of {
             base: EXPR,
             ident: IDENT_EXPR,
             loc: LOC option }
       | LexicalRef of {
             ident: IDENT_EXPR,
             loc: LOC option }
       | SetExpr of (ASSIGNOP * EXPR * EXPR)
       | ListExpr of EXPR list
       | InitExpr of (INIT_TARGET * HEAD * INITS)   (* HEAD is for temporaries *)
       | SliceExpr of (EXPR * EXPR * EXPR)
       | GetTemp of int
       | GetParam of int

     and INIT_TARGET = Hoisted
                     | Local
                     | Prototype

     and FIXTURE_NAME = TempName of int
                      | PropName of NAME

     and IDENT_EXPR =
         Identifier of
           { ident : IDENT,
             openNamespaces : NAMESPACE list list }
       | QualifiedExpression of  (* type * *)
           { qual : EXPR,
             expr : EXPR }
       | AttributeIdentifier of IDENT_EXPR
       (* for bracket exprs: o[x] and @[x] *)
       | ExpressionIdentifier of
         { expr: EXPR,
           openNamespaces : NAMESPACE list list }
       | QualifiedIdentifier of
           { qual : EXPR,
             ident : Ustring.STRING }
       | UnresolvedPath of (IDENT list * IDENT_EXPR) (* QualifiedIdentifier or ObjectRef *)
       | WildcardIdentifier

     and LITERAL =
         LiteralNull
       | LiteralUndefined
       | LiteralDouble of Real64.real
       | LiteralDecimal of Decimal.DEC
       | LiteralInt of Int32.int
       | LiteralUInt of Word32.word
       | LiteralBoolean of bool
       | LiteralString of Ustring.STRING
       | LiteralArray of
           { exprs:EXPR list,
             ty:TY option }
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE
       | LiteralObject of
           { expr : FIELD list,
             ty: TY option }
       | LiteralFunction of FUNC
       | LiteralRegExp of
           { str: Ustring.STRING }

     and BLOCK = Block of DIRECTIVES


     (* RIBs are built by the definition phase, not the parser; but they 
      * are patched back into the AST in class-definition and block
      * nodes, so we must define them here. *)

(* ClassFixture only at package level,
 * VirtualValFixture only in classes,
 *)
     and FIXTURE =
         NamespaceFixture of NAMESPACE
       | ClassFixture of CLS
       | InterfaceFixture of IFACE
       | TypeVarFixture
       | TypeFixture of TY
       | MethodFixture of
           { func: FUNC,
             ty: TY,
             readOnly: bool,  (* ES3 funcs are r/w methods with ty=Ast.Special Ast.Any *)
             override: bool,
             final: bool }
       | ValFixture of
           { ty: TY,
             readOnly: bool }
       | VirtualValFixture of
         { ty: TY, 
           getter: FUNC option,
           setter: FUNC option } (* VIRTUAL_VAL_FIXTURE *)
       | InheritedFixture of 
         { baseName: NAME, 
           baseTypeArgs: TY list }

     and HEAD =
         Head of RIB * INITS

withtype

         BINDINGS = (BINDING list * INIT_STEP list)
     and RIB = (FIXTURE_NAME * FIXTURE) list
     and RIBS = ((FIXTURE_NAME * FIXTURE) list) list
     and INITS = (FIXTURE_NAME * EXPR) list

     and INSTANCE_TYPE =
          {  name: NAME,
             typeArgs: TYPE_EXPR list,
             nonnullable: bool,
             superTypes: TYPE_EXPR list,
             ty: TYPE_EXPR,
             dynamic: bool }

     and FIELD =
           { kind: VAR_DEFN_TAG,
             name: IDENT_EXPR,
             init: EXPR }

     and FIELD_TYPE =
           { name: IDENT,
             ty: TYPE_EXPR }

     and FUNC_TYPE =
         { params: TYPE_EXPR list,
           result: TYPE_EXPR,
           thisType: TYPE_EXPR option,
           hasRest: bool,
           minArgs: int }

     and FUNC_DEFN =
           { kind : VAR_DEFN_TAG,
             ns:  EXPR option,
             final: bool,
             override: bool,
             prototype: bool,
             static: bool,
             func : FUNC }

     and CTOR_DEFN = CTOR

     and VAR_DEFN =
           { kind : VAR_DEFN_TAG,
             ns : EXPR option,
             static : bool,
             prototype : bool,
             bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
           }

     and NAMESPACE_DEFN =
           { ident: IDENT,
             ns: EXPR option,
             init: EXPR option }

     and CLASS_DEFN =
           { ns: EXPR option,
             ident: IDENT,             
             nonnullable: bool,
             dynamic: bool,
             final: bool,
             params: IDENT list,
             extends: TYPE_EXPR option, 
             implements: TYPE_EXPR list,
             classDefns: DEFN list,
             instanceDefns: DEFN list,
             instanceStmts: STMT list,
             ctorDefn: CTOR option }

     and INTERFACE_DEFN =
           { ident: IDENT,
             ns: EXPR option,
             nonnullable: bool,
             params: IDENT list,
             extends: TYPE_EXPR list,
             instanceDefns: DEFN list }

     and TYPE_DEFN =
           { ident: IDENT,
             ns: EXPR option,
             init: TYPE_EXPR }

     and FOR_ENUM_STMT =
           { isEach: bool,
             (* VAR_DEFN option *)
             defn: { kind : VAR_DEFN_TAG,
                     ns : EXPR option,
                     static : bool,
                     prototype : bool,
                     bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
                   } option,
             obj: EXPR,
             rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
             next: STMT,
             labels: IDENT list,
             body: STMT }

     and FOR_STMT =
           { rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
             (* VAR_DEFN option *)
             defn: { kind : VAR_DEFN_TAG,
                     ns : EXPR option,
                     static : bool,
                     prototype : bool,
                     bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
                   } option,
             init: STMT list,
             cond: EXPR,
             update: EXPR,
             labels: IDENT list,
             body: STMT }

     and WHILE_STMT =
           { cond: EXPR,
             rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
             body: STMT,
             labels: IDENT list }

     and DIRECTIVES =
           { pragmas: PRAGMA list,
             defns: DEFN list,
             head: HEAD option,
             body: STMT list,
             loc: LOC option }

     and CASE =
           { label: EXPR option,
             inits: ((FIXTURE_NAME * EXPR) list) option, (* INITS option *)
             body: BLOCK }   (* FIXME: should be STMT list *)

     and CATCH_CLAUSE =
         { bindings:(BINDING list * INIT_STEP list), (* BINDINGS *)
           ty: TY, 
           rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
           inits: ((FIXTURE_NAME * EXPR) list) option, (* INITS option *)
           block:BLOCK }

     and FUNC_NAME =
           { kind : FUNC_NAME_KIND,
             ident : IDENT }

type VIRTUAL_VAL_FIXTURE =
           { ty: TY, 
             getter: FUNC option,
             setter: FUNC option }

datatype FRAGMENT = 
         
         Unit of { name: UNIT_NAME option,
                   fragments: FRAGMENT list }

       | Package of { name: IDENT list,
                      fragments: FRAGMENT list }

       | Anon of BLOCK

end
