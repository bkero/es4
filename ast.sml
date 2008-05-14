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

type IDENTIFIER = Ustring.STRING

type NONCE = int

type OPAQUE_NAMESPACE_IDENTIFIER = (* LDOTS *)
     NONCE

datatype NAMESPACE =
         TransparentNamespace of Ustring.STRING
       | OpaqueNamespace of OPAQUE_NAMESPACE_IDENTIFIER


type NAME = { ns: NAMESPACE, id: IDENTIFIER }

type NAMESPACE_SET = NAMESPACE list

type OPEN_NAMESPACES = NAMESPACE_SET list

type NAME_SET = NAME list

datatype BINTYPEOP =
         Cast
       | Is
       | Like

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

datatype PRAGMA =
         UseNamespace of NAMESPACE_EXPRESSION
       | UseDefaultNamespace of NAMESPACE_EXPRESSION
       | UseStrict
       | UseStandard

     and NAME_EXPRESSION = 
         QualifiedName of { namespace: NAMESPACE_EXPRESSION,
                            identifier: IDENTIFIER }
       | UnqualifiedName of { identifier: IDENTIFIER, 
                              openNamespaces: OPEN_NAMESPACES, 
                              globalNames: NAME_SET }
       | ResolvedName of NAME

     and NAMESPACE_EXPRESSION =
         Namespace of NAMESPACE
       | NamespaceName of NAME_EXPRESSION

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
             typeParams: IDENTIFIER list,
             nonnullable: bool,
             dynamic: bool,
             extends: TYPE option,
             implements: TYPE list,
             classRib: RIB,
             instanceRib: RIB,
             instanceInits: HEAD,
             constructor: CTOR option,
             classType: TYPE,  (* RecordType *)
             instanceType: TYPE }

     and IFACE =
         Iface of
           { name: NAME,
             typeParams: IDENTIFIER list,
             nonnullable: bool,
             extends: TYPE list,
             instanceRib: RIB,
             instanceType: TYPE }

     and CTOR =
         Ctor of 
         { settings: HEAD, (* FIXME should be a EXPRESSION list of LetExpr of InitExpr *)
           superArgs: EXPRESSION list,
           func: FUNC }
 
     and FUNC =
         Func of 
         { name: FUNC_NAME,
           fsig: FUNC_SIG,                       (* redundant, not used in verify *)
           native: bool,
           generator: bool,
           block: BLOCK option, (* NONE => abstract *)
           param: HEAD,         (* CF: not sure what this is ... *)
           defaults: EXPRESSION list,
           ty: TYPE,
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
         { typeParams: IDENTIFIER list,
           params: BINDINGS,
           paramTypes: TYPE list,
           defaults: EXPRESSION list,
           ctorInits: (BINDINGS * EXPRESSION list) option, (* settings + super args *)
           returnType: TYPE option,       (* NONE => void *)
           thisType: TYPE option,
           hasRest: bool }

     and BINDING =
         Binding of
           { ident: BINDING_IDENTIFIER,    (* FIXME: use tuple *)
             ty: TYPE }

     and BINDING_IDENTIFIER =
         TempIdent of int
       | ParamIdent of int
       | PropIdent of IDENTIFIER

     and INIT_STEP =   (* used to encode init of bindings *)
         InitStep of (BINDING_IDENTIFIER * EXPRESSION)
       | AssignStep of (EXPRESSION * EXPRESSION)

(*

datatype TYPE =
         NullType
       | UndefinedType
       | AnyType
       | RecordType of (NAME_EXPRESSION * TYPE) list
       | ArrayType  of (TYPE list * TYPE option)
       | UnionType  of TYPE list
       | NonNullType of TYPE
       | FunctionType of FUNCTION_TYPE
       | AppType of (TYPE * TYPE list)
       | TypeName of (NAME_EXPRESSION * NONCE option)  
       | ClassType of CLS
       | InterfaceType of IFACE

*)

     and TYPE =
         NullType
       | UndefinedType
       | AnyType
       | RecordType of (NAME_EXPRESSION * TYPE) list   
       | ArrayType  of (TYPE list * TYPE option)
       | UnionType of TYPE list
       | NonNullType of TYPE
       | FunctionType of FUNCTION_TYPE
       | AppType of (TYPE * TYPE list)
       | TypeName of (NAME_EXPRESSION * NONCE option)  (* *)
       | InstanceType of INSTANCE_TYPE        (* *)

(* Following will replace InstanceType *)
       | ClassType of CLS
       | InterfaceType of IFACE

(* Following will be removed during defn phase *)
       | TypeNameReferenceType of (TYPE * NAME_EXPRESSION)
       | TypeIndexReferenceType of (TYPE * int)

(* SPEC

datatype STATEMENT =
         EmptyStmt
       | ExprStmt of EXPRESSION
       | ForStmt of FOR_STATEMENT
       | ForInStmt of FOR_ENUM_STATEMENT
       | ThrowStmt of EXPRESSION
       | ReturnStmt of EXPRESSION
       | BreakStmt of IDENTIFIER option
       | ContinueStmt of IDENTIFIER option
       | BlockStmt of BLOCK
       | LabeledStmt of (IDENTIFIER * STATEMENT)
       | WhileStmt of WHILE_STATEMENT
       | DoWhileStmt of WHILE_STATEMENT
       | IfStmt of (EXPRESSION * STATEMENT * STATEMENT)
       | WithStmt of (EXPRESSION * STATEMENT)
       | TryStmt of TRY_STATEMENT
       | SwitchStmt of SWITCH_STATEMENT
       | SwitchTypeStmt of SWITCH_TYPE_STATEMENT

*)

     and STATEMENT =
         EmptyStmt
       | ExprStmt of EXPRESSION
       | InitStmt of {
             kind: VAR_DEFN_TAG,
             ns: NAMESPACE_EXPRESSION option,
             prototype: bool,
             static: bool,
             temps: BINDINGS,
             inits: INIT_STEP list }
       | ClassBlock of CLASS_BLOCK                           
       | ForInStmt of FOR_ENUM_STATEMENT
       | ThrowStmt of EXPRESSION
       | ReturnStmt of EXPRESSION
       | BreakStmt of IDENTIFIER option
       | ContinueStmt of IDENTIFIER option
       | BlockStmt of BLOCK
       | LabeledStmt of (IDENTIFIER * STATEMENT)
       | LetStmt of BLOCK
       | WhileStmt of WHILE_STATEMENT
       | DoWhileStmt of WHILE_STATEMENT
       | ForStmt of FOR_STATEMENT
       | IfStmt of {
             cnd: EXPRESSION,
             thn: STATEMENT,
             els: STATEMENT }
       | WithStmt of {
             obj: EXPRESSION,
             ty: TYPE,
             body: STATEMENT }
       | TryStmt of {
             block: BLOCK,
             catches: CATCH_CLAUSE list,
             finally: BLOCK option }

       | SwitchStmt of {         (* FIXME: needs HEAD, DEFNS for defns hoisted from body *)
             cond: EXPRESSION,
             labels: IDENTIFIER list,
             cases: CASE list }
       | SwitchTypeStmt of {
             cond: EXPRESSION,
             ty: TYPE,
             cases: CATCH_CLAUSE list }
       | DXNStmt of {
             expr: EXPRESSION }

     and EXPRESSION =
         TernaryExpr of (EXPRESSION * EXPRESSION * EXPRESSION)
       | BinaryExpr of (BINOP * EXPRESSION * EXPRESSION)
       | BinaryTypeExpr of (BINTYPEOP * EXPRESSION * TYPE)
       | UnaryExpr of (UNOP * EXPRESSION)
       | TypeExpr of TYPE
       | ThisExpr of THIS_KIND option
       | YieldExpr of EXPRESSION option
       | SuperExpr of EXPRESSION option
       | CallExpr of 
         { func: EXPRESSION,
           actuals: EXPRESSION list }
       | ApplyTypeExpr of 
         { expr: EXPRESSION,  (* apply expr to type list *)
           actuals: TYPE list }

       (* defs are rewritten into head by defn phase, and so defs are ignored in verifier and in eval *)
       | LetExpr of 
         { defs: BINDINGS,  
           body: EXPRESSION,
           head: HEAD option }
       | NewExpr of 
         { obj: EXPRESSION,
           actuals: EXPRESSION list }
       | SetExpr of (ASSIGNOP * EXPRESSION * EXPRESSION)
       | ListExpr of EXPRESSION list
       | InitExpr of (INIT_TARGET * HEAD * INITS)   (* HEAD is for temporaries *)
       | GetTemp of int
       | GetParam of int
       | Comprehension of (EXPRESSION * FOR_ENUM_HEAD list * EXPRESSION option)
       | LiteralExpr of LITERAL
       | LexicalReference of { name: NAME_EXPRESSION,
                               loc: LOC option }
       | ObjectNameReference of { object: EXPRESSION,
								  name: NAME_EXPRESSION,
                                  loc: LOC option }
	   | ObjectIndexReference of { object: EXPRESSION,
								   index: EXPRESSION,
                                   loc: LOC option }

     and INIT_TARGET = Hoisted
                     | Local
                     | Prototype

     and THIS_KIND = FunctionThis
                   | GeneratorThis

     and FIXTURE_NAME = TempName of int
                      | PropName of NAME

     and LITERAL =
         LiteralNull
       | LiteralUndefined
       | LiteralDouble of Real64.real
       | LiteralDecimal of Decimal.DEC
       | LiteralBoolean of bool
       | LiteralString of Ustring.STRING
       | LiteralArray of
           { exprs: EXPRESSION,  (* FIXME: more specific type here *)
             ty:TYPE option }
       | LiteralXML of EXPRESSION list
       | LiteralNamespace of NAMESPACE
       | LiteralObject of
           { expr : FIELD list,
             ty: TYPE option }
       | LiteralFunction of FUNC
       | LiteralRegExp of
           { str: Ustring.STRING }

     and BLOCK = Block of DIRECTIVES

     (* RIBs are built by the definition phase, not the parser; but they 
      * are patched back into the AST in class-definition and block
      * nodes, so we must define them here. *)
(*
datatype FIXTURE =
         NamespaceFixture of NAMESPACE
       | ClassFixture of CLS
       | InterfaceFixture of IFACE
       | TypeVarFixture of TYPEVAR_FIXTURE
       | TypeFixture of TY
       | MethodFixture of METHOD
       | ValFixture of VAL_FIXTURE
       | VirtualValFixture of VIRTUAL_VAL_FIXTURE
*)

(* ClassFixture only at package level,
 * VirtualValFixture only in classes,
 *)
     and FIXTURE =
         NamespaceFixture of NAMESPACE
       | ClassFixture of CLS
       | InterfaceFixture of IFACE
       | TypeVarFixture of NONCE
       | TypeFixture of (IDENTIFIER list * TYPE)
       | MethodFixture of
           { func: FUNC,
             ty: TYPE,
             writable: bool,  (* ES3 funcs are r/w methods with ty=Ast.Special Ast.Any *)
             override: bool,
             final: bool }
       | ValFixture of
           { ty: TYPE,
             writable: bool }
       | VirtualValFixture of
         { ty: TYPE, 
           getter: FUNC option,
           setter: FUNC option } (* VIRTUAL_VAL_FIXTURE *)

     and HEAD =
         Head of RIB * INITS

withtype

         BINDINGS = (BINDING list * INIT_STEP list)
     and RIB = (FIXTURE_NAME * FIXTURE) list
     and RIBS = ((FIXTURE_NAME * FIXTURE) list) (* RIB *)
                list
     and INITS = (FIXTURE_NAME * EXPRESSION) list

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
          {  name : NAME,
             typeArgs : TYPE list,

             (* following fields cached for fast evaluation *)
             nonnullable : bool,   
             typeParams : IDENTIFIER list,      
             superTypes : TYPE list,
             ty : TYPE,             
             dynamic : bool 
          }       

     and FIELD =
           { kind: VAR_DEFN_TAG,
             name: NAME_EXPRESSION,
             init: EXPRESSION }

     and FIELD_TYPE = NAME_EXPRESSION * TYPE
(*
           { name: NAME_EXPRESSION,
             ty: TYPE }
*)
     and FUNCTION_TYPE =
         { typeParams : IDENTIFIER list,
           thisType   : TYPE,
           params  : TYPE list,
           minArgs : int,          
           hasRest : bool,          
           result  : TYPE option    (* NONE indicates return type is void *)
         }

     and FUNC_DEFN =
           { kind : VAR_DEFN_TAG,
             ns:  NAMESPACE_EXPRESSION option,
             final: bool,
             override: bool,
             prototype: bool,
             static: bool,
             func : FUNC }

     and CTOR_DEFN = CTOR

     and VAR_DEFN =
           { kind : VAR_DEFN_TAG,
             ns : NAMESPACE_EXPRESSION option,
             static : bool,
             prototype : bool,
             bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
           }

     and NAMESPACE_DEFN =
           { ident: IDENTIFIER,
             ns: NAMESPACE_EXPRESSION option,
             init: NAMESPACE_EXPRESSION option }

     and CLASS_DEFN =
           { ns: NAMESPACE_EXPRESSION option,
             privateNS: NAMESPACE,
             protectedNS: NAMESPACE,
             ident: IDENTIFIER,             
             nonnullable: bool,
             dynamic: bool,
             final: bool,
             params: IDENTIFIER list,
             extends: TYPE option, 
             implements: TYPE list,
             classDefns: DEFN list,
             instanceDefns: DEFN list,
             instanceStmts: STATEMENT list,
             ctorDefn: CTOR option }

     and INTERFACE_DEFN =
           { ident: IDENTIFIER,
             ns: NAMESPACE_EXPRESSION option,
             nonnullable: bool,
             params: IDENTIFIER list,
             extends: TYPE list,
             instanceDefns: DEFN list }

     and TYPE_DEFN =
           { ident: IDENTIFIER,
             ns: NAMESPACE_EXPRESSION option,
             typeParams : IDENTIFIER list,
             init: TYPE }

     and CLASS_BLOCK = 
         { ns: NAMESPACE_EXPRESSION option,
           protectedNS: NAMESPACE,
           privateNS: NAMESPACE,
           ident: IDENTIFIER,
           name: NAME option,
           block: BLOCK }

     and FOR_ENUM_HEAD =  (* FIXME: use this in FOR_ENUM_STATEMENT *)
           { isEach: bool,
             bindings: (BINDING list * INIT_STEP list),
             expr: EXPRESSION }

     and FOR_ENUM_STATEMENT =
           { isEach: bool,
             (* VAR_DEFN option *)
             defn: { kind : VAR_DEFN_TAG,
                     ns : NAMESPACE_EXPRESSION option,
                     static : bool,
                     prototype : bool,
                     bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
                   } option,
             obj: EXPRESSION,
             rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
             next: STATEMENT,
             labels: IDENTIFIER list,
             body: STATEMENT }

     and FOR_STATEMENT =
           { rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)  (* CF: list option seems redundant *)
             (* VAR_DEFN option *)
             defn: { kind : VAR_DEFN_TAG,
                     ns : NAMESPACE_EXPRESSION option,
                     static : bool,
                     prototype : bool,
                     bindings : (BINDING list * INIT_STEP list) (* BINDINGS *)
                   } option,
             init: STATEMENT list,
             cond: EXPRESSION,
             update: EXPRESSION,
             labels: IDENTIFIER list,
             body: STATEMENT }

     and WHILE_STATEMENT =
           { cond: EXPRESSION,
             rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
             body: STATEMENT,
             labels: IDENTIFIER list }

     and DIRECTIVES =
           { pragmas: PRAGMA list,
             defns: DEFN list,
             head: HEAD option,
             body: STATEMENT list,
             loc: LOC option }

     and CASE =
           { label: EXPRESSION option,
             inits: ((FIXTURE_NAME * EXPRESSION) list) option, (* INITS option, replace by INITS?? *)
             body: BLOCK }   (* FIXME: should be STATEMENT list *)

     and CATCH_CLAUSE =
         { bindings:(BINDING list * INIT_STEP list), (* BINDINGS *)
           ty: TYPE,  (* CF: what is this for? *)
           rib: ((FIXTURE_NAME * FIXTURE) list) option, (* RIB option *)
           inits: ((FIXTURE_NAME * EXPRESSION) list) option, (* INITS option, TODO: replace by INITS?? *)
           block:BLOCK }

     and FUNC_NAME =
           { kind : FUNC_NAME_KIND,
             ident : IDENTIFIER }

type VIRTUAL_VAL_FIXTURE =
           { ty: TYPE, 
             getter: FUNC option,
             setter: FUNC option }

datatype FRAGMENT = 
         
         Anon of BLOCK

end
