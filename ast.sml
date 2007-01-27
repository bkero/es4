(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* A sketch of the ES4 AST in SML *)

structure Ast = struct

(* not actually unicode, maybe switch to int array to be unicode-y? *)

type USTRING = string

type IDENT = USTRING

datatype NAMESPACE =
         Private
       | Protected
       | Intrinsic
       | Public of IDENT
       | Internal of IDENT
       | UserDefined of IDENT

type NAME = { ns: NAMESPACE, id: IDENT }

type MULTINAME = { nss: NAMESPACE list, id: IDENT }

datatype NUMBER_TYPE =
         Decimal
       | Double
       | Int
       | UInt
       | Number

datatype ROUNDING_MODE =
         Ceiling
       | Floor
       | Up
       | Down
       | HalfUp
       | HalfDown
       | HalfEven

datatype TRIOP =
         Cond

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
       | DefVar

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
       | MakeNamespace
       | Type

datatype NULOP =
         This
       | Empty

datatype VAR_DEFN_TAG =
         Const
       | Var
       | LetVar
       | LetConst
       | Rest   (* Goes away, redundant with hasRest in FUNC_SIG *)

datatype SPECIAL_TY =
         Any
       | Null
       | Undefined
       | VoidType

datatype PRAGMA =
         UseNamespace of IDENT_EXPR
       | UseDefaultNamespace of IDENT_EXPR
       | UseNumber of NUMBER_TYPE
       | UseRounding of ROUNDING_MODE
       | UsePrecision of LITERAL
       | UseStrict
       | UseStandard
       | Import of { package: IDENT,
                     name: IDENT,
                     alias: IDENT option }

     and FUNC_NAME_KIND =
         Ordinary
       | Operator
       | Get
       | Set
       | Call
       | Construct
       | ToFunc

     and FUNC =
         Func of { name: FUNC_NAME,
                   fsig: FUNC_SIG,                   
                   body: BLOCK,
                   (* Filled in during defn phase. *)
                   typeParamFixtures: FIXTURES option,
                   paramFixtures: FIXTURES option,
                   paramInitializers: INITIALIZERS option,
                   bodyFixtures: FIXTURES option,
                   bodyInitializers: INITIALIZERS option }

     and DEFN =
         ClassDefn of CLASS_DEFN
       | VariableDefn of VAR_DEFN
       | FunctionDefn of FUNC_DEFN
       | InterfaceDefn of INTERFACE_DEFN
       | NamespaceDefn of NAMESPACE_DEFN 
       | TypeDefn of TYPE_DEFN

     and FUNC_SIG =
         FunctionSignature of { typeParams: IDENT list,
                                params: VAR_BINDING list,
                                inits: BINDINGS option, 
                                returnType: TYPE_EXPR,
                                thisType: TYPE_EXPR option,
                                hasBoundThis: bool, (* goes away, redundant with previous option *)
                                hasRest: bool }

     and VAR_BINDING =
         Binding of { pattern: PATTERN,
                      ty: TYPE_EXPR option,
                      init: EXPR option }

     (* 
      * Note: no type parameters allowed on general typedefs,
      * only the implicit paramters in Function, Class and 
      * Interface types.
      *)

     and TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | UnionType of TYPE_EXPR list
       | ArrayType of TYPE_EXPR list
       | NominalType of { ident : IDENT_EXPR }
       | FunctionType of FUNC_SIG
       | ObjectType of FIELD_TYPE list
       | AppType of { base: TYPE_EXPR,
                      args: TYPE_EXPR list }
       | NullableType of { expr:TYPE_EXPR,
                           nullable:bool }
  
     and STMT =
         EmptyStmt
       | ExprStmt of EXPR list
       | ForEachStmt of FOR_ENUM_STMT
       | ForInStmt of FOR_ENUM_STMT
       | ThrowStmt of EXPR list
       | ReturnStmt of EXPR list
       | BreakStmt of IDENT option
       | ContinueStmt of IDENT option
       | BlockStmt of BLOCK
       | LabeledStmt of (IDENT * STMT)
       | LetStmt of ((VAR_BINDING list) * STMT)
       | SuperStmt of EXPR list
       | WhileStmt of WHILE_STMT
       | DoWhileStmt of WHILE_STMT

       | ForStmt of { defns: VAR_BINDING list,
                      init: EXPR list,
                      cond: EXPR list,
                      update: EXPR list,
                      contLabel: IDENT option,
                      body: STMT,
                      (* Filled in by defn phase. *)
                      fixtures: FIXTURES option,
                      initializers: INITIALIZERS option }

       | IfStmt of { cnd: EXPR,
                     thn: STMT,
                     els: STMT }

       | WithStmt of { obj: EXPR list,
                       ty: TYPE_EXPR,
                       body: STMT }

       | TryStmt of { body: BLOCK,
                      catches: { bind:VAR_BINDING,                                  
                                 body:BLOCK,
                                 (* Filled in by defn phase. *)
                                 fixtures: FIXTURES option,
                                 initializers: INITIALIZERS option } list,
                      finally: BLOCK option }
                    
       | SwitchStmt of { cond: EXPR list,
                         cases: CASE list }

       | SwitchTypeStmt of { cond: EXPR list, 
                             ty: TYPE_EXPR,
                             cases: TYPE_CASE list }

       | Dxns of { expr: EXPR }

     and EXPR =
         TrinaryExpr of (TRIOP * EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINTYPEOP * EXPR * TYPE_EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TYPE_EXPR
       | NullaryExpr of NULOP
       | YieldExpr of EXPR list option
       | SuperExpr of EXPR option
       | LiteralExpr of LITERAL

       | CallExpr of {func: EXPR,
                      actuals: EXPR list}

       | ApplyTypeExpr of {expr: EXPR,  (* apply expr to type list *)
                      actuals: TYPE_EXPR list}

       | LetExpr of { defs: VAR_BINDING list,                      
                      body: EXPR list,
                      (* Filled in during defn phase. *)
                      fixtures: FIXTURES option, 
                      initializers: INITIALIZERS option }

       | NewExpr of { obj: EXPR,
                      actuals: EXPR list }

       | FunExpr of { ident: IDENT option,
                      fsig: FUNC_SIG,
                      body: BLOCK,
                      (* Filled in during defn phase. *)
                      typeParamFixtures: FIXTURES option,
                      paramFixtures: FIXTURES option,
                      paramInitializers: INITIALIZERS option,
                      bodyFixtures: FIXTURES option,
                      bodyInitializers: INITIALIZERS option}

       | ObjectRef of { base: EXPR, ident: IDENT_EXPR }

       | LexicalRef of { ident: IDENT_EXPR }

       | SetExpr of (ASSIGNOP * PATTERN * EXPR)

       | ListExpr of EXPR list
       | SliceExpr of (EXPR list * EXPR list * EXPR list)

     and IDENT_EXPR =
         QualifiedIdentifier of { qual : EXPR,
                                  ident : USTRING }
       | QualifiedExpression of { qual : EXPR,
                                  expr : EXPR }
       | AttributeIdentifier of IDENT_EXPR
       | Identifier of { ident : IDENT,
                         openNamespaces : NAMESPACE list }
       | ExpressionIdentifier of EXPR   (* for bracket exprs: o[x] and @[x] *)
       | TypeIdentifier of { ident : IDENT_EXPR, (*deprecated*)
                             typeParams : TYPE_EXPR list }

     and LITERAL =
         LiteralNull
       | LiteralUndefined
       | LiteralNumber of real
       | LiteralBoolean of bool
       | LiteralString of USTRING
       | LiteralArray of { exprs:EXPR list, ty:TYPE_EXPR option }
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE

       | LiteralObject of
         { expr : FIELD list,
           ty: TYPE_EXPR option }

       | LiteralRegExp of
         { str: USTRING }

     and BLOCK = Block of DIRECTIVES

     and PATTERN = 
                (* these IDENT_EXPRs are actually
                   Identifier { id, _ }
                   and should later be changed to IDENT
                 *)
         ObjectPattern of FIELD_PATTERN list
       | ArrayPattern of PATTERN list
       | SimplePattern of EXPR
       | IdentifierPattern of IDENT_EXPR

(* FIXTURES are built by the definition phase, not the parser; but they 
 * are patched back into the AST in class-definition and block
 * nodes, so we must define them here. *)

     and FIXTURE = 
         NamespaceFixture of NAMESPACE
       | ClassFixture of CLASS_DEFN
       | TypeVarFixture
       | TypeFixture of TYPE_EXPR
       | ValFixture of { ty: TYPE_EXPR,
                         readOnly: bool,
                         isOverride: bool }
       | VirtualValFixture of { ty: TYPE_EXPR, 
                                getter: FUNC_DEFN option,
                                setter: FUNC_DEFN option }

     and INITIALIZER = VarInit of (NAME * EXPR)
                     | FunInit of (NAME * FUNC)

                     
withtype FIELD =
         { kind: VAR_DEFN_TAG,
           name: IDENT,
           init: EXPR }

     and FIELD_PATTERN =
         { name: IDENT, 
           ptrn : PATTERN }

     and FIELD_TYPE =
         { name: IDENT,
           ty: TYPE_EXPR }
         
     and TYPED_IDENT =
         { name: IDENT,
           ty: TYPE_EXPR option }

     and ATTRIBUTES =
         { ns: EXPR,
           override: bool,
           static: bool,
           final: bool,
           dynamic: bool,
           prototype: bool,
           native: bool,
           rest: bool }

     and FUNC_DEFN = 
         { kind : VAR_DEFN_TAG,
           ns: EXPR,
           final: bool,
           native: bool,
           override: bool,
           prototype: bool,
           static: bool,
           func : FUNC }

     and VAR_DEFN =
         { kind : VAR_DEFN_TAG,
           ns : EXPR,
           static : bool,
           prototype : bool,
           bindings : VAR_BINDING list }

     and INITIALIZERS = INITIALIZER list

     and FIXTURES = (NAME * FIXTURE) list

     and NAMESPACE_DEFN = 
         { ident: IDENT,
           ns: EXPR,
           init: EXPR option }

     and CLASS_DEFN =
         { ident: IDENT, 
           ns: EXPR,
           nonnullable: bool,
           dynamic: bool,
           final: bool,
           params: IDENT list,
           extends: IDENT_EXPR option,
           implements: IDENT_EXPR list,
           body: BLOCK,
           (* Filled in during defn phase. *)
           classFixtures: FIXTURES option,
           instanceFixtures: FIXTURES option,
           classInitializers: INITIALIZERS option,
           protoInitializers: INITIALIZERS option,
           instanceInitializers: INITIALIZERS option,
           constructor: FUNC_DEFN option,
           initializer: STMT list,

           (* These should go away *)
           protoVars: VAR_DEFN list,
           protoMethods: FUNC_DEFN list,
           instanceVars: VAR_DEFN list,
           instanceMethods: FUNC_DEFN list,
           vars: VAR_DEFN list,
           methods: FUNC_DEFN list }

     and INTERFACE_DEFN =
         { ident: IDENT,
           ns: EXPR,
           nonnullable: bool,
           params: IDENT list,
           extends: IDENT_EXPR list,
           body: BLOCK }
         
     and TYPE_DEFN =
         { ident: IDENT,
           ns: EXPR,
           init: TYPE_EXPR }

     and FOR_ENUM_STMT =
         { ptrn: PATTERN option,
           obj: EXPR list,
           defns: VAR_BINDING list,           
           contLabel: IDENT option,
           body: STMT,
           (* Filled in during defn phase. *)
           fixtures: FIXTURES option,
           initializers: INITIALIZERS option }

     and WHILE_STMT =
         { cond: EXPR,
           body: STMT,
           contLabel: IDENT option }

     and DIRECTIVES = 
         { pragmas: PRAGMA list,
           defns: DEFN list,
           stmts: STMT list,
           (* Filled in during defn phase. *)
           fixtures: FIXTURES option,
           initializers: INITIALIZERS option }

     and BINDINGS =
         { b : VAR_BINDING list,
           i : EXPR list }

     and CASE = (* Perhaps we can collapse this to EXPR list *)
         { label : EXPR list option, body : BLOCK }

     and TYPE_CASE =
         { ptrn : VAR_BINDING option, body : BLOCK }

     and FUNC_NAME =
         { kind : FUNC_NAME_KIND, ident : IDENT }

type PACKAGE =
     { name: USTRING,
       body: BLOCK }
     
type PROGRAM =
     { packages: PACKAGE list,
       body : BLOCK }

end
