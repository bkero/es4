(* A sketch of the ES4 AST in SML *)

structure Ast = struct

(* not actually unicode, maybe switch to int array to be unicode-y? *)

type ustring =
     string

type ident =
     ustring

datatype importQual =
         QualName of ident
       | QualStar

datatype numberType =
         DECIMAL | DOUBLE | INT | UINT | NUMBER

datatype roundingMode =
         CEILING | FLOOR | UP | DOWN | HALF_UP | HALF_DOWN | HALF_EVEN

datatype triOp =
         COND

datatype binOp =
         PLUS | MINUS | TIMES | DIVIDE | REMAINDER
       | LEFT_SHIFT | RIGHT_SHIFT | RIGHT_SHIFT_UNSIGNED
       | BITWISE_AND | BITWISE_OR | BITWISE_XOR
       | LOGICAL_AND | LOGICAL_OR | LOGICAL_XOR
       | INSTANCEOF | IS | CAST | TO | IN
       | EQUALS | NOT_EQUALS | STRICT_EQUALS | STRICT_NOT_EQUALS
       | LESS | LESS_OR_EQUAL | GREATER | GREATER_OR_EQUAL
       | COMMA | DEFVAR | ASSIGN
       | ASSIGN_PLUS | ASSIGN_MINUS | ASSIGN_TIMES | ASSIGN_DIVIDE
       | ASSIGN_REMAINDER | ASSIGN_LEFT_SHIFT
       | ASSIGN_RIGHT_SHIFT | ASSIGN_RIGHT_SHIFT_UNSIGNED
       | ASSIGN_BITWISE_AND | ASSIGN_BITWISE_OR | ASSIGN_BITWISE_XOR
       | ASSIGN_LOGICAL_AND | ASSIGN_LOGICAL_OR | ASSIGN_LOGICAL_XOR

datatype unOp =
         DELETE | VOID | TYPEOF | PRE_INCREMENT | PRE_DECREMENT
       | POST_INCREMENT | POST_DECREMENT | UNARY_PLUS | UNARY_MINUS | BITWISE_NOT
       | LOGICAL_NOT | MAKE_NAMESPACE | TYPE

datatype nulOp =
         THIS | EMPTY

datatype varDefnTag =
         CONST | VAR | LETVAR | LETCONST | REST

datatype namespaceKind =
         PUBLIC | PRIVATE | PROTECTED | INTERNAL | INTRINSIC | USERDEFINED

datatype primAnnotation =
         NAMED | NULLABLE | NONNULLABLE

datatype specialTy =
         ANY | NULL | UNDEFINED | NOTYPE

datatype directive =
         UseNamespace of expr list
       | UseNumber of numberType
       | UseRounding of roundingMode

       | Import of { package: ident,
                     qualifier: importQual,
                     alias: ident option }

     and visibility =
         Namespace of ( namespaceKind * ustring )

     and definition =
         NamespaceDefn of { name: ident,
                            init: expr }

       | ClassDefn of { name: ident,
                        attrs: attributes,
                        params: ident list,
                        extends: tyExpr list,
                        implements: tyExpr list,
                        instanceVars: varDefn list,
                        vars: varDefn list,
                        constructor: funcDefn,
                        methods: funcDefn list,
                        initializer: stmt list }

       | InterfaceDefn of { name: ident,
                            attrs: attributes,
                            params: ident list,
                            extends: tyExpr list,
                            methods: (ident * funcTy) list }

       | FunctionDefn of funcDefn
       | VariableDefn of varDefn
 
     and funcSign =
         FunctionSignature of { typeparams: literal list,
                                params: formal list,
                                resulttype: tyExpr }
                             

     (* Improve this? Probably more mutual exclusion possible. *)
     and attributes =
         Attributes of { vis: visibility,
                         override: bool,
                         static: bool,
                         final: bool,
                         dynamic: bool,
                         prototype: bool,
                         nullable: bool }

     and varDefn =
         SimpleDefn of { tag: varDefnTag,
                         init: expr option,
                         attrs: attributes,
                         name: ident,
                         ty: tyExpr option }

       | DestructuringDefn of { tag: varDefnTag,
                                init: expr option,
                                attrs: attributes,
                                temp: ident,
                                postInit: expr option,
                                names: ident list,
                                ty: tyExpr option }

     and tyExpr =
         SpecialType of specialTy
       | UnionType of tyExpr list
       | ArrayType of tyExpr list
       | PrimaryType of primTy
       | FunctionType of funcTy
       | RecordType of (expr * tyExpr) list
       | InstantiationType of { base: primTy,
                                params: tyExpr list }
       | UnresolvedType of expr

     and stmt =
         EmptyStmt
       | ExprStmt of expr
       | DefineStmt of varDefn
       | ForEachStmt of forEnumStmt
       | ForInStmt of forEnumStmt
       | ThrowStmt of expr
       | ReturnStmt of expr
       | BreakStmt of ident option
       | ContinueStmt of ident option
       | BlockStmt of block
       | LabeledStmt of (ident * stmt)
       | LetStmt of ((varDefn list) * block)
       | SuperStmt of expr list
       | WhileStmt of whileStmt
       | DoWhileStmt of whileStmt

       | ForStmt of { isVar: bool,
                      defns: varDefn list,
                      init: expr,
                      cond: expr,
                      update: expr,
                      contLabel: ident option,
                      body: stmt }

       | IfStmt of { cond: expr,
                     consequent: stmt,
                     alternative: stmt }

       | WithStmt of { obj: expr,
                       body: stmt }

       | TryStmt of { body: block,
                      catches: (formal * block) list,
                      finally: block }

       | SwitchStmt of { cond: expr,
                         cases: (expr * (stmt list)) list,
                         default: stmt list }

     and expr =
         TrinaryExpr of (triOp * expr * expr * expr)
       | BinaryExpr of (binOp * expr * expr)
       | BinaryTypeExpr of (binOp * expr * tyExpr)
       | UnaryExpr of (unOp * expr)
       | TypeExpr of tyExpr
       | NullaryExpr of nulOp
       | YieldExpr of expr option
       | SuperExpr of expr option
       | LiteralExpr of literal

       | CallExpr of {func: expr,
                      actuals: expr list}

       | Property of { indirect: bool,
                       obj: expr option,
                       field: expr }

	   | Ref of { base : expr option, ident : identExpr }

       | QualIdent of { qual: expr option,
                        ident: ustring,
                        opennss: visibility list }

       | QualExpr of { qual: expr option,
                        expr: expr,
                        opennss: visibility list }

       | AttrQualIdent of { indirect: bool,
                            operand: expr }

       | LetExpr of { defs: varDefn list,
                      body: expr }

       | NewExpr of { obj: expr,
                      actuals: expr list }
       | FunExpr of { ident: ident option,
					  sign: funcSign,
                      body: block }
       | ListExpr of expr list
 
     and identOrExpr =
         Ident of ident
       | Expr of expr

	 and identExpr =
	     QualifiedIdentifier of { qual : expr, ident : ustring }
	   | QualifiedExpression of { qual : expr, expr : expr }
       | AttributeIdentifier of identExpr
	   | Identifier of ident
	   | Expression of expr   (* for bracket exprs: o[x] and @[x] *)

     and literal =
         LiteralNull
       | LiteralUndefined
       | LiteralNumber of real
       | LiteralBoolean of bool
       | LiteralString of ustring
       | LiteralArray of expr list
       | LiteralXML of expr list
       | LiteralNamespace of visibility

       | LiteralObject of { name: expr,
                            init: expr } list

       | LiteralRegExp of { pattern: ustring,
                            global: bool,
                            multiline: bool,
                            caseInsensitive: bool }
    and block = Block of
         { directives: directive list,
           defns: definition list,
           stmts: stmt list }

withtype 

         funcTy =
         { paramTypes: tyExpr list,
           returnType: tyExpr,
           boundThisType: tyExpr option,
           hasRest: bool }

     and formal =
         { name: ident,
           ty: tyExpr option,
           init: expr option,
		   tag: varDefnTag,
           isRest: bool }

     and typedIdent =
         { name: ident,
           ty: tyExpr option }

     and funcDefn =
         { name: ident,
           attrs: attributes,
           formals: formal list,
           ty: tyExpr option,
           body: block }

     and primTy =
         { name: string,
           annotation: primAnnotation }

     and forEnumStmt =
         { isVar: bool,
           init: expr,
           obj: expr,
           defns: varDefn list,
           contLabel: ident option,
           body: stmt }

     and whileStmt =
         { cond: expr,
           body: stmt,
           contLabel: ident option }

type package = { names: ident list,
                 fullname: ustring,
                 body: block }

type program = { packages: package list,
                 body: block }

end
