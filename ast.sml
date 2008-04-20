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

type NONCE = int

datatype NAMESPACE =
         OpaqueNamespace of NONCE
       | StringNamespace of Ustring.STRING

type NAME = { ns: NAMESPACE, id: IDENT }

type MULTINAME = { nss: NAMESPACE list list, id: IDENT }

datatype BINTYPEOP =
         Cast
       | Is

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
       | Spread

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
       | UseStrict
       | UseStandard

     and FUNC_NAME_KIND =
         Ordinary
       | Operator
       | Get
       | Set
       | Call
       | Has
         
     and CLS =
         Cls of
           { name: NAME,
             privateNS: NAMESPACE,
             protectedNS: NAMESPACE,
             parentProtectedNSs: NAMESPACE list,
             typeParams: IDENT list,
             nonnullable: bool,
             dynamic: bool,
             extends: TYPE_EXPR option,
             implements: TYPE_EXPR list,
             classRib: RIB,
             instanceRib: RIB,
             instanceInits: HEAD,
             constructor: CTOR option,
             classType: TYPE_EXPR,  (* ObjectType *)
             instanceType: TYPE_EXPR }

     and IFACE =
         Iface of
           { name: NAME,
             typeParams: IDENT list,
             nonnullable: bool,
             extends: TYPE_EXPR list,
             instanceRib: RIB,
             instanceType: TYPE_EXPR }

     and CTOR =
         Ctor of 
         { settings: HEAD, (* FIXME should be a EXPR list of LetExpr of InitExpr *)
           superArgs: EXPR list,
           func: FUNC }
 
     and FUNC =
         Func of 
         { name: FUNC_NAME,
           fsig: FUNC_SIG,                       (* redundant, not used in verify *)
           native: bool,
           block: BLOCK option, (* NONE => abstract *)
           param: HEAD,         (* CF: not sure what this is ... *)
           defaults: EXPR list,
           ty: TYPE_EXPR,
           loc: LOC option }
         
     and DEFN =
         ClassDefn of CLASS_DEFN
       | VariableDefn of VAR_DEFN
       | FunctionDefn of FUNC_DEFN
       | ConstructorDefn of CTOR_DEFN
       | InterfaceDefn of INTERFACE_DEFN
       | NamespaceDefn of NAMESPACE_DEFN
       | TypeDefn of TYPE_DEFN

     and FUNC_SIG =                             (* redundant, not used in verify *)
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
       | TypeName of (IDENT_EXPR * NONCE option)
       | ElementTypeRef of (TYPE_EXPR * int)
       | FieldTypeRef of (TYPE_EXPR * IDENT)
       | FunctionType of FUNC_TYPE
       | ObjectType of FIELD_TYPE list
       | LikeType of TYPE_EXPR
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
       | TypeVarFixtureRef of NONCE  

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
       | ClassBlock of CLASS_BLOCK                           
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
             ty: TYPE_EXPR,
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
             ty: TYPE_EXPR,
             cases: CATCH_CLAUSE list }
       | DXNStmt of {
             expr: EXPR }

     and EXPR =
         TernaryExpr of (EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINTYPEOP * EXPR * TYPE_EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TYPE_EXPR
       | ThisExpr of THIS_KIND option
       | YieldExpr of EXPR option
       | SuperExpr of EXPR option
       | LiteralExpr of LITERAL
       | CallExpr of {
             func: EXPR,
             actuals: EXPR list }
       | ApplyTypeExpr of {
             expr: EXPR,  (* apply expr to type list *)
             actuals: TYPE_EXPR list }

       (* defs are rewritten into head by defn phase, and so defs are ignored in verifier and in eval *)
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
       | GetTemp of int
       | GetParam of int
       | Comprehension of (EXPR * FOR_ENUM_HEAD list * EXPR option)

     and INIT_TARGET = Hoisted
                     | Local
                     | Prototype

     and THIS_KIND = FunctionThis
                   | GeneratorThis

     and FIXTURE_NAME = TempName of int
                      | PropName of NAME

     and IDENT_EXPR =
         Identifier of
           { ident : IDENT,
             openNamespaces : NAMESPACE list list }
(* CF: the above should be unified with
        type MULTINAME = { nss: NAMESPACE list list, id: IDENT }
   Perhaps Identifier should be Multiname
*)
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

     and LITERAL =
         LiteralNull
       | LiteralUndefined
       | LiteralDouble of Real64.real
       | LiteralDecimal of Decimal.DEC
       | LiteralBoolean of bool
       | LiteralString of Ustring.STRING
       | LiteralArray of
           { exprs: EXPR,  (* FIXME: more specific type here *)
             ty:TYPE_EXPR option }
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE
       | LiteralObject of
           { expr : FIELD list,
             ty: TYPE_EXPR option }
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
       | TypeVarFixture of NONCE
       | TypeFixture of TYPE_EXPR
       | MethodFixture of
           { func: FUNC,
             ty: TYPE_EXPR,
             readOnly: bool,  (* ES3 funcs are r/w methods with ty=Ast.Special Ast.Any *)
             override: bool,
             final: bool }
       | ValFixture of
           { ty: TYPE_EXPR,
             readOnly: bool }
       | VirtualValFixture of
         { ty: TYPE_EXPR, 
           getter: FUNC option,
           setter: FUNC option } (* VIRTUAL_VAL_FIXTURE *)

     and HEAD =
         Head of RIB * INITS

withtype

         BINDINGS = (BINDING list * INIT_STEP list)
     and RIB = (FIXTURE_NAME * FIXTURE) list
     and RIBS = RIB list
     and INITS = (FIXTURE_NAME * EXPR) list

(* cf: a class ref of the form C.<int> is represented as
  AppType
  { base = LamType
             { params = ["X"],
               body = InstanceType
                        { name = { ns = Public "", id = "C"},
                          typeParams = ["X"],
                          typeArgs = [], 
                          ... }},
    args = ... }

  In the above AST, typeArgs is implicitly ["X"]
*)
     and INSTANCE_TYPE =
          {  name: NAME,
             typeParams: IDENT list,      
             typeArgs: TYPE_EXPR list,
             nonnullable: bool,           (* redundant, ignored in verify.sml *)
             superTypes: TYPE_EXPR list,  (* redundant, ignored in verify.sml *)
             ty: TYPE_EXPR,               (* redundant, ignored in verify.sml *)
             dynamic: bool }              (* redundant, ignored in verify.sml *)

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
           hasRest: bool,         (* if true, the last elem in params is array type *)
           minArgs: int }         (* necessary because some of params can have defaults *)

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
             privateNS: NAMESPACE,
             protectedNS: NAMESPACE,
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

     and CLASS_BLOCK = 
         { ns: EXPR option,
           protectedNS: NAMESPACE,
           privateNS: NAMESPACE,
           ident: IDENT,
           name: NAME option,
           block: BLOCK }

     and FOR_ENUM_HEAD =  (* FIXME: use this in FOR_ENUM_STMT *)
           { isEach: bool,
             bindings: (BINDING list * INIT_STEP list),
             expr: EXPR }

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
           { rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)  (* CF: list option seems redundant *)
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
             inits: ((FIXTURE_NAME * EXPR) list) option, (* INITS option, replace by INITS?? *)
             body: BLOCK }   (* FIXME: should be STMT list *)

     and CATCH_CLAUSE =
         { bindings:(BINDING list * INIT_STEP list), (* BINDINGS *)
           ty: TYPE_EXPR, 
           rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
           inits: ((FIXTURE_NAME * EXPR) list) option, (* INITS option, TODO: replace by INITS?? *)
           block:BLOCK }

     and FUNC_NAME =
           { kind : FUNC_NAME_KIND,
             ident : IDENT }

type VIRTUAL_VAL_FIXTURE =
           { ty: TYPE_EXPR, 
             getter: FUNC option,
             setter: FUNC option }

datatype FRAGMENT = 
         
         Anon of BLOCK

end
