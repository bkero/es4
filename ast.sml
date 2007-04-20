(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* A sketch of the ES4 AST in SML *)

structure Ast = struct

type POS = { file: string, span: StreamPos.span, sm: StreamPos.sourcemap, post_newline: bool }

(* not actually unicode, maybe switch to int array to be unicode-y? *)

type USTRING = string

type IDENT = USTRING

datatype NAMESPACE =
         Intrinsic
       | OperatorNamespace
       | Private of IDENT
       | Protected of IDENT
       | Public of IDENT
       | Internal of IDENT
       | UserNamespace of USTRING
       | AnonUserNamespace of int
       | LimitedNamespace of (IDENT * NAMESPACE)

type NAME = { ns: NAMESPACE, id: IDENT }

type MULTINAME = { nss: NAMESPACE list list, id: IDENT }

datatype NUMBER_TYPE =
         Decimal
       | Double
       | Int
       | UInt
       | Number

type NUMERIC_MODE = 
           { numberType: NUMBER_TYPE,
             roundingMode: Decimal.ROUNDING_MODE,
             precision: int }
     
datatype TRIOP =
         Cond

datatype BINTYPEOP =
         Cast
       | Is
       | To

datatype BINOP =
         Plus of NUMERIC_MODE option
       | Minus of NUMERIC_MODE option
       | Times of NUMERIC_MODE option
       | Divide of NUMERIC_MODE option
       | Remainder of NUMERIC_MODE option
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
       | Equals of NUMERIC_MODE option
       | NotEquals of NUMERIC_MODE option
       | StrictEquals of NUMERIC_MODE option
       | StrictNotEquals of NUMERIC_MODE option
       | Less of NUMERIC_MODE option
       | LessOrEqual of NUMERIC_MODE option
       | Greater of NUMERIC_MODE option
       | GreaterOrEqual of NUMERIC_MODE option
       | Comma

datatype ASSIGNOP =
         Assign
       | AssignPlus of NUMERIC_MODE option
       | AssignMinus of NUMERIC_MODE option
       | AssignTimes of NUMERIC_MODE option
       | AssignDivide of NUMERIC_MODE option
       | AssignRemainder of NUMERIC_MODE option
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
       | PreIncrement of NUMERIC_MODE option
       | PreDecrement of NUMERIC_MODE option
       | PostIncrement of NUMERIC_MODE option
       | PostDecrement of NUMERIC_MODE option
       | UnaryPlus of NUMERIC_MODE option
       | UnaryMinus of NUMERIC_MODE option
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
       | UseNumber of NUMBER_TYPE
       | UseRounding of Decimal.ROUNDING_MODE
       | UsePrecision of int
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

     and CLS =
         Cls of
           { name: NAME,
             extends: NAME option,
             implements: NAME list,
             classFixtures: FIXTURES,
             instanceFixtures: FIXTURES,
             instanceInits: HEAD,
             constructor: CTOR option,
             classType: TYPE_EXPR,  (* ObjectType *)
             instanceType: TYPE_EXPR } (* InstanceType *)

     and CTOR =
         Ctor of
           { settings: HEAD,
             superArgs: EXPR list,
             func: FUNC }

     and FUNC =
         Func of 
           { name: FUNC_NAME,
             fsig: FUNC_SIG,
             isNative: bool,
             block: BLOCK,
             param: HEAD,
             defaults: EXPR list,
             ty: FUNC_TYPE }

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

     (* 
      * Note: no type parameters allowed on general typedefs,
      * only the implicit paramters in Function, Class and 
      * Interface types.
      *)

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
       | NullableType of 
           { expr:TYPE_EXPR,
             nullable:bool }
       | InstanceType of
           { name: NAME, 
             typeParams: IDENT list, 
             ty: TYPE_EXPR,
             isDynamic: bool }

     and STMT =
         EmptyStmt
       | ExprStmt of EXPR
       | InitStmt of
           { kind: VAR_DEFN_TAG,
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
       | IfStmt of 
           { cnd: EXPR,
             thn: STMT,
             els: STMT }
       | WithStmt of 
           { obj: EXPR,
             ty: TYPE_EXPR,
             body: STMT }
       | TryStmt of 
           { block: BLOCK,
             catches: CATCH_CLAUSE
                list,
             finally: BLOCK option }

       | SwitchStmt of         (* FIXME: needs HEAD, DEFNS for defns hoisted from body *)
           { mode: NUMERIC_MODE option,
             cond: EXPR,
             labels: IDENT list,
             cases: CASE list }
       | SwitchTypeStmt of 
           { cond: EXPR, 
             ty: TYPE_EXPR,
             cases: TYPE_CASE list }
       | Dxns of 
           { expr: EXPR }

     and EXPR =
         TernaryExpr of (TRIOP * EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINTYPEOP * EXPR * TYPE_EXPR)
       | ExpectedTypeExpr of (TYPE_EXPR * EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TYPE_EXPR
       | ThisExpr
       | YieldExpr of EXPR option
       | SuperExpr of EXPR option
       | LiteralExpr of LITERAL
       | CallExpr of 
           { func: EXPR,
             actuals: EXPR list }
       | ApplyTypeExpr of 
           { expr: EXPR,  (* apply expr to type list *)
             actuals: TYPE_EXPR list }
       | LetExpr of 
           { defs: BINDINGS,                      
             body: EXPR,
             head: HEAD option }
       | NewExpr of 
           { obj: EXPR,
             actuals: EXPR list }
       | ObjectRef of { base: EXPR, ident: IDENT_EXPR, pos: POS option }
       | LexicalRef of { ident: IDENT_EXPR, pos: POS option }
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
             ident : USTRING }
       | TypeIdentifier of (* in a type context, these this will be a AppType *)
           { ident : IDENT_EXPR, 
             typeArgs : TYPE_EXPR list }
       | UnresolvedPath of (IDENT list * IDENT_EXPR)
       | WildcardIdentifier

     and LITERAL =
         LiteralNull
       | LiteralUndefined
       | LiteralContextualDecimal of string        (* Should be erased after defn time. *)
       | LiteralContextualDecimalInteger of string (* Should be erased after defn time. *)
       | LiteralContextualHexInteger of string     (* Should be erased after defn time. *)
       | LiteralDouble of Real64.real
       | LiteralDecimal of Decimal.DEC
       | LiteralInt of Int32.int
       | LiteralUInt of Word32.word
       | LiteralBoolean of bool
       | LiteralString of USTRING
       | LiteralArray of
           { exprs:EXPR list, 
             ty:TYPE_EXPR option }
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE
       | LiteralObject of
           { expr : FIELD list,
             ty: TYPE_EXPR option }
       | LiteralFunction of FUNC
       | LiteralRegExp of
           { str: USTRING }

     and BLOCK = Block of DIRECTIVES


     (* FIXTURES are built by the definition phase, not the parser; but they 
      * are patched back into the AST in class-definition and block
      * nodes, so we must define them here. *)

(* ClassFixture only at package level,
 * VirtualValFixture only in classes,
 *)
     and FIXTURE = 
         NamespaceFixture of NAMESPACE
       | ClassFixture of CLS
       | InterfaceFixture  (* FIXME *)
       | TypeVarFixture
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
         VIRTUAL_VAL_FIXTURE
             

withtype 

         BINDINGS = (BINDING list * INIT_STEP list)
     and FIXTURES = (FIXTURE_NAME * FIXTURE) list
     and INITS = (FIXTURE_NAME * EXPR) list
     and HEAD = (FIXTURES * INITS)

     and FIELD =
           { kind: VAR_DEFN_TAG,
             name: IDENT_EXPR,
             init: EXPR }

     and FIELD_TYPE =
           { name: IDENT,
             ty: TYPE_EXPR }
         
     and TYPED_IDENT =
           { name: IDENT,
             ty: TYPE_EXPR option }

     and FUNC_TYPE = 
         { typeParams: IDENT list,
           params: TYPE_EXPR list,
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
             bindings : BINDINGS }

     and NAMESPACE_DEFN = 
           { ident: IDENT,
             ns: EXPR option,
             init: EXPR option }

     and CLASS_DEFN =
           { ident: IDENT, 
             ns: EXPR option,
             nonnullable: bool,
             dynamic: bool,
             final: bool,
             params: IDENT list,
             extends: IDENT_EXPR option,
             implements: IDENT_EXPR list,
             classDefns: DEFN list,
             instanceDefns: DEFN list,
             instanceStmts: STMT list,
             ctorDefn: CTOR option }

     and INTERFACE_DEFN =
           { ident: IDENT,
             ns: EXPR option,
             nonnullable: bool,
             params: IDENT list,
             extends: IDENT_EXPR list,
             block: BLOCK }
         
     and TYPE_DEFN =
           { ident: IDENT,
             ns: EXPR option,
             init: TYPE_EXPR }

     and FOR_ENUM_STMT =
           { isEach: bool,
             defn: VAR_DEFN option,             
             obj: EXPR,
             fixtures: FIXTURES option,
             next: STMT,
             labels: IDENT list,
             body: STMT }

     and FOR_STMT =
           { fixtures: FIXTURES option,
             defn: VAR_DEFN option,    
             init: STMT list,                  
             cond: EXPR,
             update: EXPR,
             labels: IDENT list,
             body: STMT }

     and WHILE_STMT =
           { cond: EXPR,
             fixtures: FIXTURES option,
             body: STMT,
             labels: IDENT list }

     and DIRECTIVES = 
           { pragmas: PRAGMA list,
             defns: DEFN list,
             head: HEAD option,
             body: STMT list,
             pos: POS option }

     and CASE =
           { label: EXPR option,
             inits: INITS option, 
             body: BLOCK }   (* FIXME: should be STMT list *)

     and TYPE_CASE =
           { ty : TYPE_EXPR option,
             body: STMT }  (* LetStmt, actually *)

     and CATCH_CLAUSE = 
         { bindings:BINDINGS,
           ty: TYPE_EXPR, 
           fixtures: FIXTURES option,
           block:BLOCK }

     and FUNC_NAME =
           { kind : FUNC_NAME_KIND, 
             ident : IDENT }

     and VIRTUAL_VAL_FIXTURE = 
         { ty: TYPE_EXPR, 
           getter: FUNC_DEFN option,
           setter: FUNC_DEFN option }

type PACKAGE =
           { name: IDENT list,
             block: BLOCK }

type PROGRAM =
           { packages: PACKAGE list,
             fixtures: FIXTURES option,
             block: BLOCK }

end
