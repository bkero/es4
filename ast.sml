(* A sketch of the ES4 AST in SML *)

structure Ast = struct

(* not actually unicode, maybe switch to int array to be unicode-y? *)

type USTRING = string

type IDENT = USTRING

datatype IMPORT_QUAL =
         QualName of IDENT
       | QualStar

datatype NUMBER_TYPE =
         Decimal 
       | Double 
       | Int 
       | Uint 
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
       | LogicalXor
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
       | Assign
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
       | AssignLogicalXor

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
       | Rest

datatype NAMESPACE =
		 Private
       | Protected
       | Intrinsic
       | Public of IDENT
       | Internal of IDENT
       | UserDefined of IDENT

datatype PRIM_KIND =
         Named
       | Nullable
       | NotNullable

datatype SPECIAL_TY =
         Any
       | Null
       | Undefined
       | NoType

datatype DIRECTIVE =
         UseNamespace of EXPR list
       | UseNumber of NUMBER_TYPE
       | UseRounding of ROUNDING_MODE
       | Import of { package: IDENT,
                     qualifier: IMPORT_QUAL,
                     alias: IDENT option }

     and FUNC = 
		 Func of 
         { name: IDENT,
           attrs: ATTRIBUTES,
           formals: FORMAL list,
           ty: TYPE_EXPR option,
           body: BLOCK }
	 
     and DEFINITION =
         ClassDefn of CLASS_DEFN
       | VariableDefn of VAR_DEFN
       | FunctionDefn of FUNC
       | InterfaceDefn of INTERFACE_DEFN
       | NamespaceDefn of { name: IDENT,
                            init: EXPR }

     and FUNC_SIGN =
         FunctionSignature of { typeparams: IDENT list,
                                params: FORMAL list,
                                resulttype: TYPE_EXPR }


     (* Improve this? Probably more mutual exclusion possible. *)
     and ATTRIBUTES =
         Attributes of { ns: NAMESPACE,
                         override: bool,
                         static: bool,
                         final: bool,
                         dynamic: bool,
                         prototype: bool,
                         nullable: bool }

     and VAR_DEFN =
         VariableDefinition of { tag: VAR_DEFN_TAG,
                           init: EXPR option,
                           attrs: ATTRIBUTES,
                           pattern: PATTERN,
                           ty: TYPE_EXPR option }

     and TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | UnionType of TYPE_EXPR list
       | ArrayType of TYPE_EXPR list
       | PrimaryType of { ident : IDENT_EXPR, kind : PRIM_KIND }
       | FunctionType of FUNC_TY
       | RecordType of FIELD_TYPE list
       | InstantiationType of { base: PRIM_TY,
                                params: TYPE_EXPR list }
       | UnresolvedType of EXPR

     and STMT =
         EmptyStmt
       | ExprStmt of EXPR
       | DefineStmt of VAR_DEFN
       | ForEachStmt of FOR_ENUM_STMT
       | ForInStmt of FOR_ENUM_STMT
       | ThrowStmt of EXPR
       | ReturnStmt of EXPR
       | BreakStmt of IDENT option
       | ContinueStmt of IDENT option
       | BlockStmt of BLOCK
       | LabeledStmt of (IDENT * STMT)
       | LetStmt of ((VAR_DEFN list) * BLOCK)
       | SuperStmt of EXPR list
       | WhileStmt of WHILE_STMT
       | DoWhileStmt of WHILE_STMT

       | ForStmt of { isVar: bool,
                      defns: VAR_DEFN list,
                      init: EXPR,
                      cond: EXPR,
                      update: EXPR,
                      contLabel: IDENT option,
                      body: STMT }

       | IfStmt of { cond: EXPR,
                     consequent: STMT,
                     alternative: STMT }

       | WithStmt of { obj: EXPR,
                       body: STMT }

       | TryStmt of { body: BLOCK,
                      catches: (FORMAL * BLOCK) list,
                      finally: BLOCK }

       | SwitchStmt of { cond: EXPR,
                         cases: (EXPR * (STMT list)) list,
                         default: STMT list }

	   | DefnStmt of DEFINITION list

	   | PragmaStmt of DIRECTIVE list

     and EXPR =
         TrinaryExpr of (TRIOP * EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINTYPEOP * EXPR * TYPE_EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TYPE_EXPR
       | NullaryExpr of NULOP
       | YieldExpr of EXPR option
       | SuperExpr of EXPR option
       | LiteralExpr of LITERAL

       | CallExpr of {func: EXPR,
                      actuals: EXPR list}

       | Ref of { base: EXPR option,
                  ident: IDENT_EXPR }

       | LetExpr of { defs: VAR_DEFN list,
                      body: EXPR }

       | NewExpr of { obj: EXPR,
                      actuals: EXPR list }

       | FunExpr of { ident: IDENT option,
                      sign: FUNC_SIGN,
                      body: BLOCK }

       | ListExpr of EXPR list

	   | PatternExpr of PATTERN

 
    and IDENT_EXPR =
         QualifiedIdentifier of { qual : EXPR,
                                  ident : USTRING }
       | QualifiedExpression of { qual : EXPR,
                                  expr : EXPR }
       | AttributeIdentifier of IDENT_EXPR
       | Identifier of { ident : IDENT,
			 openNamespaces : (NAMESPACE list) option ref }
       | Expression of EXPR   (* for bracket exprs: o[x] and @[x] *)
	   | TypeIdentifier of { ident : IDENT_EXPR, typeParams : TYPE_EXPR list }

     and LITERAL =
         LiteralNull
       | LiteralUndefined 
       | LiteralNumber of real
       | LiteralBoolean of bool
       | LiteralString of USTRING
       | LiteralArray of { expr:EXPR list, ty:TYPE_EXPR option }
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE

       | LiteralObject of
         { expr : FIELD list,
		   ty: TYPE_EXPR option }

       | LiteralRegExp of
         { str: USTRING }

    and BLOCK = Block of
         { pragmas: DIRECTIVE list,
           defns: DEFINITION list,
           stmts: STMT list }

	and PATTERN =
		ObjectPattern of { name: EXPR, ptrn : PATTERN } list
	  | ArrayPattern of PATTERN list
	  | SimplePattern of EXPR
	  | IdentifierPattern of IDENT

withtype

		 FIELD =
		 { name: EXPR,
           init: EXPR }

     and FIELD_TYPE =
		 { name: EXPR,
           ty: TYPE_EXPR }

     and FUNC_TY =
         { paramTypes: TYPE_EXPR option list,
           returnType: TYPE_EXPR,
           boundThisType: TYPE_EXPR option,
           hasRest: bool }

     and FORMAL =
         { pattern: PATTERN,
           ty: TYPE_EXPR option,
           init: EXPR option,
           tag: VAR_DEFN_TAG,
           isRest: bool }

     and TYPED_IDENT =
         { name: IDENT,
           ty: TYPE_EXPR option }

     and CLASS_DEFN =
         { name: IDENT,
           attrs: ATTRIBUTES,
           params: IDENT list,
           extends: TYPE_EXPR list,
           implements: TYPE_EXPR list,
           instanceVars: VAR_DEFN list,
           vars: VAR_DEFN list,
           constructor: FUNC,
           methods: FUNC list,
           initializer: STMT list }

     and INTERFACE_DEFN =
         { name: IDENT,
           attrs: ATTRIBUTES,
           params: IDENT list,
           extends: TYPE_EXPR list,
           methods: (IDENT * FUNC_TY) list }

     and PRIM_TY =
         { name: USTRING,
           kind: PRIM_KIND }

     and FOR_ENUM_STMT =
         { isVar: bool,
           init: EXPR,
           obj: EXPR,
           defns: VAR_DEFN list,
           contLabel: IDENT option,
           body: STMT }

     and WHILE_STMT =
         { cond: EXPR,
           body: STMT,
           contLabel: IDENT option }

type PACKAGE =
     { names: IDENT list,
       fullname: USTRING,
       body: BLOCK }

type PROGRAM =
     { packages: PACKAGE list,
       body: BLOCK }

end
