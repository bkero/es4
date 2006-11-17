structure PrettyCvt = struct
open Ast PrettyRep

fun cvtIdent s = String s
fun cvtIdentOption so =
    case so of
        SOME s => Ctor ("SOME", [String s])
      | NONE => Ctor ("NONE", [])

fun cvtIdentList sl = List (map cvtIdent sl)

fun cvtImportQual q =
    case q of
        QualName n => Ctor ("QualName", [String n])
      | QualStar => Ctor ("QualStar", [])

fun cvtNumberType n =
    Ctor ((case n of
               DECIMAL => "DECIMAL"
             | DOUBLE => "DOUBLE"
             | INT => "INT"
             | UINT => "UINT"
             | NUMBER => "NUMBER"), [])

fun cvtRoundingMode r =
    Ctor
        ((case r of
              CEILING => "CEILING"
            | FLOOR => "FLOOR"
            | UP => "UP"
            | DOWN => "DOWN"
            | HALF_UP => "HALF_UP"
            | HALF_DOWN => "HALF_DOWN"
            | HALF_EVEN => "HALF_EVEN"), [])

fun cvtTriOp COND = Ctor ("COND", [])

fun cvtBinOp b =
    Ctor
        ((case b of
              PLUS => "PLUS" | MINUS => "MINUS" | TIMES => "TIMES" | DIVIDE => "DIVIDE" | REMAINDER => "REMAINDER"
            | LEFT_SHIFT => "LEFT_SHIFT" | RIGHT_SHIFT => "RIGHT_SHIFT" | RIGHT_SHIFT_UNSIGNED => "RIGHT_SHIFT_UNSIGNED"
            | BITWISE_AND => "BITWISE_AND" | BITWISE_OR => "BITWISE_OR" | BITWISE_XOR => "BITWISE_XOR"
            | LOGICAL_AND => "LOGICAL_AND" | LOGICAL_OR => "LOGICAL_OR" | LOGICAL_XOR => "LOGICAL_XOR"
            | INSTANCEOF => "INSTANCEOF" | IS => "IS" | CAST => "CAST" | TO => "TO" | IN => "IN"
            | EQUALS => "EQUALS" | NOT_EQUALS => "NOT_EQUALS" | STRICT_EQUALS => "STRICT_EQUALS" | STRICT_NOT_EQUALS => "STRICT_NOT_EQUALS"
            | LESS => "LESS" | LESS_OR_EQUAL => "LESS_OR_EQUAL" | GREATER => "GREATER" | GREATER_OR_EQUAL => "GREATER_OR_EQUAL"
            | COMMA => "COMMA" | DEFVAR => "DEFVAR" | ASSIGN => "ASSIGN"
            | ASSIGN_PLUS => "ASSIGN_PLUS" | ASSIGN_MINUS => "ASSIGN_MINUS" | ASSIGN_TIMES => "ASSIGN_TIMES" | ASSIGN_DIVIDE => "ASSIGN_DIVIDE"
            | ASSIGN_REMAINDER => "ASSIGN_REMAINDER" | ASSIGN_LEFT_SHIFT => "ASSIGN_LEFT_SHIFT"
            | ASSIGN_RIGHT_SHIFT => "ASSIGN_RIGHT_SHIFT" | ASSIGN_RIGHT_SHIFT_UNSIGNED => "ASSIGN_RIGHT_SHIFT_UNSIGNED"
            | ASSIGN_BITWISE_AND => "ASSIGN_BITWISE_AND" | ASSIGN_BITWISE_OR => "ASSIGN_BITWISE_OR" | ASSIGN_BITWISE_XOR => "ASSIGN_BITWISE_XOR"
            | ASSIGN_LOGICAL_AND => "ASSIGN_LOGICAL_AND" | ASSIGN_LOGICAL_OR => "ASSIGN_LOGICAL_OR" | ASSIGN_LOGICAL_XOR => "ASSIGN_LOGICAL_XOR"), [])

fun cvtUnOp u =
    Ctor
        ((case u of
              DELETE => "DELETE" | VOID => "VOID" | TYPEOF => "TYPEOF" | PRE_INCREMENT => "PRE_INCREMENT" | PRE_DECREMENT => "PRE_DECREMENT"
            | POST_INCREMENT => "POST_INCREMENT" | POST_DECREMENT => "POST_DECREMENT" | UNARY_PLUS => "UNARY_PLUS" | UNARY_MINUS => "UNARY_MINUS"
            | BITWISE_NOT => "BITWISE_NOT" | LOGICAL_NOT => "LOGICAL_NOT" | MAKE_NAMESPACE => "MAKE_NAMESPACE"), [])

fun cvtNullOp n =
    Ctor
        ((case n of
              THIS => "THIS"
            | EMPTY => "EMPTY"), [])

fun cvtVarDefnTag v =
    Ctor
        ((case v of
              CONST => "CONST" | VAR => "VAR" | LETVAR => "LETVAR" | LETCONST => "LETCONST"), [])

fun cvtNamespaceKind n =
    Ctor
        ((case n of
              PUBLIC => "PUBLIC" | PRIVATE => "PRIVATE" | PROTECTED => "PROTECTED" | INTERNAL => "INTERNAL"
	    | INTRINSIC => "INTRINSIC" | USERDEFINED => "USERDEFINED" ), [])

fun cvtVisibility (Namespace (kind,id)) =
    Ctor ("Namespace", [cvtNamespaceKind kind, (String id)])

fun cvtPrimAnnotation a =
    Ctor
        ((case a of
              NAMED => "NAMED" | NULLABLE => "NULLABLE" | NONNULLABLE => "NONNULLABLE"), [])

fun cvtSpecialTy t =
    Ctor
        ((case t of
              ANY => "ANY" | NULL => "NULL" | UNDEFINED => "UNDEFINED" | NOTYPE => "NOTYPE"), [])


fun cvtDirective d =
    case d of
        UseNamespace es => Ctor ("UseNamespace", (map cvtExpr es))
      | UseNumber n => Ctor ("UseNumber", [cvtNumberType n])
      | UseRounding r => Ctor ("UseRounding", [cvtRoundingMode r])
      | Import {package, qualifier, alias} =>
        Ctor ("Import",
              [Rec [("package", String package),
                    ("qualifier", cvtImportQual qualifier),
                    ("alias", (case alias of
                                   SOME a => Ctor ("SOME", [String a])
                                 | NONE => Ctor ("NONE", [])))]])

and cvtFuncSign (FunctionSignature {typeparams, params, resulttype}) = 
    Rec [("typeparams", List (map cvtLiteral typeparams)),
	 ("params", List (map cvtFormal params)),
	 ("resultType", cvtTyExpr resulttype)]

and cvtIdentOrExpr ie =
    case ie of
        Ident i => Ctor ("Ident", [(String i)])
      | Expr e => Ctor ("Expr", [(cvtExpr e)])

and cvtIdentExpr ie =
    case ie of
        QualifiedIdentifier {qual, ident} =>
        Ctor ("QualifiedIdentifier", [Rec [("qual", cvtExpr qual),
                                 ("ident", String ident)]])

      | QualifiedExpression {qual, expr} =>
        Ctor ("QualifiedExpression", [Rec [("qual", cvtExpr qual),
                                 ("expr", cvtExpr expr)]])

      | AttributeIdentifier i =>
        Ctor ("AttributeIdentifier", [(cvtIdentExpr i)])

      | Identifier i => Ctor ("Identifier", [(String i)])

and cvtExprOption eo =
    case eo of
        SOME e => Ctor ("SOME", [cvtExpr e])
      | NONE => Ctor ("NONE", [])

and cvtExprList el = List (map cvtExpr el)

and cvtExpr e =
    case e of
        TrinaryExpr (t, e1, e2, e3) => Ctor ("TrinaryExpr", [cvtTriOp t, cvtExpr e1, cvtExpr e2, cvtExpr e3])
      | BinaryExpr (b, e1, e2) => Ctor ("BinaryExpr", [cvtBinOp b, cvtExpr e1,  cvtExpr e2])
      | BinaryTypeExpr (b, e1, t1) => Ctor ("BinaryTypeExpr", [cvtBinOp b, cvtExpr e1,  cvtTyExpr t1])
      | UnaryExpr (u, e1) => Ctor ("UnaryExpr", [cvtUnOp u, cvtExpr e1])
      | NullaryExpr n => Ctor ("NullaryExpr", [cvtNullOp n])
      | YieldExpr y => Ctor ("YieldExpr", [cvtExprOption y])
      | SuperExpr s => Ctor ("SuperExpr", [cvtExprOption s])
      | LiteralExpr lit => Ctor ("LiteralExpr", [cvtLiteral lit])

      | CallExpr {func, actuals} =>
        Ctor ("Call", [Rec [("func", cvtExpr func),
                            ("actuals", cvtExprList actuals)]])

      | Property {indirect, obj, field} =>
        Ctor ("Property", [Rec [("indirect", Bool indirect),
                                ("obj", cvtExprOption obj),
                                ("field", cvtExpr field)]])

      | Ref {base, ident} =>
        Ctor ("Ref", [Rec [ ("base", cvtExprOption base),
                                ("ident", cvtIdentExpr ident)]])

      | QualIdent {qual, ident, opennss} =>
        Ctor ("QualIdent", [Rec [("qual", cvtExprOption qual),
                                 ("ident", String ident),
                                 ("opennss", List (map cvtVisibility opennss))]])

      | QualExpr {qual, expr, opennss} =>
        Ctor ("QualExpr", [Rec [("qual", cvtExprOption qual),
                                 ("expr", cvtExpr expr),
                                 ("opennss", List (map cvtVisibility opennss))]])

      | AttrQualIdent {indirect, operand} =>
        Ctor ("AttrQualIdent", [Rec [("indirect", Bool indirect),
                                     ("operand", cvtExpr operand)]])

      | LetExpr {defs, body} =>
        Ctor ("LetExpr", [Rec [("defs", List (map cvtVarDefn defs)),
                               ("body", cvtExpr body)]])

      | NewExpr {obj, actuals} =>
        Ctor ("NewExpr", [Rec [("obj", cvtExpr obj),
                               ("actuals", cvtExprList actuals)]])

      | FunExpr {ident, sign, body} =>
        Ctor ("FunExpr", [Rec [("ident", cvtIdentOption ident),
							   ("sign", cvtFuncSign sign),
                               ("body", cvtBlock body)]])

      | ListExpr es =>
        Ctor ("ListExpr", [(cvtExprList es)])


and cvtVarDefnList v = List (map cvtVarDefn v)

and cvtVarDefn v =
    case v of
        SimpleDefn {tag, init, attrs, name, ty} =>
        Ctor ("SimpleDefn", [Rec [("tag", cvtVarDefnTag tag),
                                  ("init", cvtExprOption init),
                                  ("attrs", cvtAttributes attrs),
                                  ("name", String name),
                                  ("ty", cvtTyExprOption ty)]])

      | DestructuringDefn {tag, init, attrs, temp, postInit, names, ty} =>
        Ctor ("DestructuringDefn", [Rec [("tag", cvtVarDefnTag tag),
                                         ("init", cvtExprOption init),
                                         ("attrs", cvtAttributes attrs),
                                         ("temp", String temp),
                                         ("postInit", cvtExprOption postInit),
                                         ("names", List (map cvtIdent names)),
                                         ("ty", cvtTyExprOption ty)]])

and cvtFuncTy { paramTypes,
                returnType,
                boundThisType,
                hasRest } =
    Rec [("paramTypes", cvtTyExprOptionList paramTypes),
         ("returnType", cvtTyExpr returnType),
         ("boundThisType", cvtTyExprOption boundThisType),
         ("hasRest", Bool hasRest)]

and cvtPrimTy {name, annotation} = 
    Rec [("name", String name),
	 ("annotation", cvtPrimAnnotation annotation)]

and cvtTyExprOption to =
    case to of
        SOME t => Ctor ("SOME", [cvtTyExpr t])
      | NONE => Ctor ("NONE", [])

and cvtTyExprList tl = List (map cvtTyExpr tl)

and cvtTyExprOptionList tl = List (map cvtTyExprOption tl)

and cvtTyExpr t =
    case t of
        SpecialType st =>
        Ctor ("SpecialType", [cvtSpecialTy st])

      | UnionType ts =>
        Ctor ("UnionType", [cvtTyExprList ts])

      | ArrayType ts =>
        Ctor ("ArrayType", [cvtTyExprList ts])

      | PrimaryType p =>
        Ctor ("PrimaryType", [cvtPrimTy p])

      | FunctionType f =>
        Ctor ("FunctionType", [cvtFuncTy f])

      | RecordType rows =>
        Ctor ("RecordType", [List (List.map (fn (n,v) =>
                                                Tuple [cvtExpr n, cvtTyExpr v])
                                            rows)])

      | InstantiationType {base, params} =>
        Ctor ("InstantiationType", [Rec [("base", cvtPrimTy base),
                                         ("params", cvtTyExprList params)]])

      | UnresolvedType e => 
	Ctor ("UnresolvedType", [cvtExpr e])

and cvtAttributes (Attributes { vis, override,
                                static, final, dynamic,
                                prototype, nullable }) =
    Ctor ("Attributes",
          [Rec [("vis", cvtVisibility vis),
                ("override", Bool override),
                ("static", Bool static),
                ("final", Bool final),
                ("dynamic", Bool dynamic),
                ("prototype", Bool prototype),
                ("nullable", Bool nullable)]])

and cvtLiteral lit =
    case lit of
        LiteralNull => Ctor ("LiteralNull", [])
      | LiteralUndefined => Ctor ("LiteralUndefined", [])
      | LiteralNumber r => Ctor ("LiteralNumber", [Real r])
      | LiteralBoolean b => Ctor ("LiteralLBoolean", [Bool b])
      | LiteralString s => Ctor ("LiteralString", [String s])
      | LiteralArray a => Ctor ("LiteralArray", [List (map cvtExpr a)])
      | LiteralXML x => Ctor ("LiteralXML", [List (map cvtExpr x)])
      | LiteralNamespace vis => Ctor ("LiteralNamespace", [cvtVisibility vis])
      | LiteralObject fields =>
        let
            fun cvtField {name,init} = Rec [("name", cvtExpr name),
                                            ("init", cvtExpr init)]
        in
            Ctor ("LiteralObject", [List (map cvtField fields)])
        end

      | LiteralRegExp {pattern, global, multiline, caseInsensitive} =>
        Ctor ("LiteralRegExp", [Rec [("pattern", String pattern),
                                     ("global", Bool global),
                                     ("multiline", Bool multiline),
                                     ("caseInsensitive", Bool caseInsensitive)]])

and cvtFormal {name, tag, ty, init, isRest} =
    Rec [("name", cvtIdentOrExpr name),
         ("tag", cvtVarDefnTag tag),
         ("ty", cvtTyExprOption ty),
         ("init", cvtExprOption init),
         ("isRest", Bool isRest)]

and cvtFuncDefn {name, attrs, formals, ty, body} =
    Rec [("name", String name),
         ("attrs", cvtAttributes attrs),
         ("formals", List (map cvtFormal formals)),
         ("ty", cvtTyExprOption ty),
         ("body", cvtBlock body)]

and cvtDefinition d =

    case d of
        NamespaceDefn {name, init} =>
        Ctor ("NamespaceDefn", [Rec [("name", String name),
                                     ("init", cvtExpr init)]])

      | ClassDefn {name, attrs, params, extends,
                   implements, instanceVars, vars,
                   constructor, methods, initializer} =>
        Ctor ("ClassDefn", [Rec [("name", String name),
                                 ("attrs", cvtAttributes attrs),
                                 ("params", List (map cvtIdent params)),
                                 ("extends", cvtTyExprList extends),
                                 ("implements", cvtTyExprList implements),
                                 ("instanceVars", cvtVarDefnList instanceVars),
                                 ("vars", cvtVarDefnList vars),
                                 ("constructor", cvtFuncDefn constructor),
                                 ("methods", List (map cvtFuncDefn methods)),
                                 ("initializer", cvtStmtList initializer)]])

      | InterfaceDefn {name, attrs, params, extends, methods} =>
        Ctor ("InterfaceDefn", [Rec [("name", String name),
                                     ("attrs", cvtAttributes attrs),
                                     ("params", List (map cvtIdent params)),
                                     ("extends", cvtTyExprList extends),
                                     ("methods", List (map (fn (i,fty) =>
                                                               Tuple [String i, cvtFuncTy fty])
                                                           methods))]])

      | FunctionDefn f => Ctor ("FunctionDefn", [cvtFuncDefn f])

      | VariableDefn v => Ctor ("VariableDefn", [cvtVarDefn v])


and cvtBlock (Block {directives, defns, stmts}) =
    Ctor ("Block", [Rec [("directives", List (map cvtDirective directives)),
                         ("defns", List (map cvtDefinition defns)),
                         ("stmts", cvtStmtList stmts)]])

and cvtForEnumStmt {isVar, init, obj, defns, contLabel, body} =
    Rec [("isVar", Bool isVar),
         ("init", cvtExpr init),
         ("obj", cvtExpr obj),
         ("defns", cvtVarDefnList defns),
         ("contLabel", cvtIdentOption contLabel),
         ("body", cvtStmt body)]

and cvtWhileStmt {cond, body, contLabel} =
    Rec [("cond", cvtExpr cond),
         ("body", cvtStmt body),
         ("contLabel", cvtIdentOption contLabel)]

and cvtStmtList ss = List (map cvtStmt ss)

and cvtStmt s =
    case s of
        EmptyStmt => Ctor ("EmptyStmt", [])
      | ExprStmt e => Ctor ("ExprStmt", [cvtExpr e])
      | DefineStmt d => Ctor ("DefineStmt", [cvtVarDefn d])
      | ForEachStmt fe => Ctor ("ForEachStmt", [cvtForEnumStmt fe])
      | ForInStmt fi => Ctor ("ForInstmt", [cvtForEnumStmt fi])
      | ThrowStmt e => Ctor ("Throwstmt", [cvtExpr e])
      | ReturnStmt e => Ctor ("ReturnStmt",  [cvtExpr e])
      | BreakStmt lab => Ctor ("BreakStmt", [cvtIdentOption lab])
      | ContinueStmt lab => Ctor ("ContinueStmt", [cvtIdentOption lab])
      | BlockStmt b => Ctor ("BlockStmt", [cvtBlock b])
      | LabeledStmt (lab, stmt) => Ctor ("LabeledStmt", [String lab, cvtStmt stmt])
      | LetStmt (defs,b) => Ctor ("LetStmt", [cvtVarDefnList defs, cvtBlock b])
      | SuperStmt es => Ctor ("SuperStmt", [cvtExprList es])
      | WhileStmt w => Ctor ("WhileStmt", [cvtWhileStmt w])
      | DoWhileStmt d => Ctor ("DoWhileStmt", [cvtWhileStmt d])

      | ForStmt {isVar, defns, init, cond, update, contLabel, body} =>
        Ctor ("ForStmt", [Rec [("isVar", Bool isVar),
                               ("defns", cvtVarDefnList defns),
                               ("init", cvtExpr init),
                               ("cond", cvtExpr cond),
                               ("update", cvtExpr update),
                               ("contLabel", cvtIdentOption contLabel),
                               ("body", cvtStmt body)]])

      | IfStmt {cond, consequent, alternative} =>
        Ctor ("IfStmt", [Rec [("cond", cvtExpr cond),
                              ("consequent", cvtStmt consequent),
                              ("alternative", cvtStmt alternative)]])

      | WithStmt {obj, body} =>
        Ctor ("WithStmt", [Rec [("obj", cvtExpr obj),
                                ("body", cvtStmt body)]])

      | TryStmt {body, catches, finally} =>
        let
            fun cvtCatchClause (f, b) = Tuple [cvtFormal f, cvtBlock b]
        in
            Ctor ("TryStmt", [Rec [("body", cvtBlock body),
                                   ("catches", List (map cvtCatchClause catches)),
                                   ("finally", cvtBlock finally)]])
        end

      | SwitchStmt {cond, cases, default} =>
        let
            fun cvtCaseClause (e, stmts) = Tuple [cvtExpr e, cvtStmtList stmts]
        in
            Ctor ("SwitchStmt", [Rec [("cond", cvtExpr cond),
                                      ("cases", List (map cvtCaseClause cases)),
                                      ("default", cvtStmtList default)]])
        end

and cvtPackage {names, fullname, body} = 
    Rec [("names", cvtIdentList names),
	 ("fullname", cvtIdent fullname),
	 ("body", cvtBlock body)]

and cvtProgram {packages, body} = 
    Rec [("packages", List (map cvtPackage packages)),
	 ("body", cvtBlock body)]

end
