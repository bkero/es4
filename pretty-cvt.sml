structure PrettyCvt = struct
   open Ast PrettyRep
   fun cvtUstring s0 = String s0
   and cvtIdent x1 = cvtUstring x1
   and cvtImportQual (QualName x2) = Ctor ("QualName", SOME (cvtIdent x2))
     | cvtImportQual (QualStar) = Ctor ("QualStar", NONE)
   and cvtNumberType (DECIMAL) = Ctor ("DECIMAL", NONE)
     | cvtNumberType (DOUBLE) = Ctor ("DOUBLE", NONE)
     | cvtNumberType (INT) = Ctor ("INT", NONE)
     | cvtNumberType (UINT) = Ctor ("UINT", NONE)
     | cvtNumberType (NUMBER) = Ctor ("NUMBER", NONE)
   and cvtRoundingMode (CEILING) = Ctor ("CEILING", NONE)
     | cvtRoundingMode (FLOOR) = Ctor ("FLOOR", NONE)
     | cvtRoundingMode (UP) = Ctor ("UP", NONE)
     | cvtRoundingMode (DOWN) = Ctor ("DOWN", NONE)
     | cvtRoundingMode (HALF_UP) = Ctor ("HALF_UP", NONE)
     | cvtRoundingMode (HALF_DOWN) = Ctor ("HALF_DOWN", NONE)
     | cvtRoundingMode (HALF_EVEN) = Ctor ("HALF_EVEN", NONE)
   and cvtTriOp (COND) = Ctor ("COND", NONE)
   and cvtBinOp (PLUS) = Ctor ("PLUS", NONE)
     | cvtBinOp (MINUS) = Ctor ("MINUS", NONE)
     | cvtBinOp (TIMES) = Ctor ("TIMES", NONE)
     | cvtBinOp (DIVIDE) = Ctor ("DIVIDE", NONE)
     | cvtBinOp (REMAINDER) = Ctor ("REMAINDER", NONE)
     | cvtBinOp (LEFT_SHIFT) = Ctor ("LEFT_SHIFT", NONE)
     | cvtBinOp (RIGHT_SHIFT) = Ctor ("RIGHT_SHIFT", NONE)
     | cvtBinOp (RIGHT_SHIFT_UNSIGNED) = Ctor ("RIGHT_SHIFT_UNSIGNED", NONE)
     | cvtBinOp (BITWISE_AND) = Ctor ("BITWISE_AND", NONE)
     | cvtBinOp (BITWISE_OR) = Ctor ("BITWISE_OR", NONE)
     | cvtBinOp (BITWISE_XOR) = Ctor ("BITWISE_XOR", NONE)
     | cvtBinOp (LOGICAL_AND) = Ctor ("LOGICAL_AND", NONE)
     | cvtBinOp (LOGICAL_OR) = Ctor ("LOGICAL_OR", NONE)
     | cvtBinOp (LOGICAL_XOR) = Ctor ("LOGICAL_XOR", NONE)
     | cvtBinOp (INSTANCEOF) = Ctor ("INSTANCEOF", NONE)
     | cvtBinOp (IS) = Ctor ("IS", NONE)
     | cvtBinOp (CAST) = Ctor ("CAST", NONE)
     | cvtBinOp (TO) = Ctor ("TO", NONE)
     | cvtBinOp (IN) = Ctor ("IN", NONE)
     | cvtBinOp (EQUALS) = Ctor ("EQUALS", NONE)
     | cvtBinOp (NOT_EQUALS) = Ctor ("NOT_EQUALS", NONE)
     | cvtBinOp (STRICT_EQUALS) = Ctor ("STRICT_EQUALS", NONE)
     | cvtBinOp (STRICT_NOT_EQUALS) = Ctor ("STRICT_NOT_EQUALS", NONE)
     | cvtBinOp (LESS) = Ctor ("LESS", NONE)
     | cvtBinOp (LESS_OR_EQUAL) = Ctor ("LESS_OR_EQUAL", NONE)
     | cvtBinOp (GREATER) = Ctor ("GREATER", NONE)
     | cvtBinOp (GREATER_OR_EQUAL) = Ctor ("GREATER_OR_EQUAL", NONE)
     | cvtBinOp (COMMA) = Ctor ("COMMA", NONE)
     | cvtBinOp (DEFVAR) = Ctor ("DEFVAR", NONE)
     | cvtBinOp (ASSIGN) = Ctor ("ASSIGN", NONE)
     | cvtBinOp (ASSIGN_PLUS) = Ctor ("ASSIGN_PLUS", NONE)
     | cvtBinOp (ASSIGN_MINUS) = Ctor ("ASSIGN_MINUS", NONE)
     | cvtBinOp (ASSIGN_TIMES) = Ctor ("ASSIGN_TIMES", NONE)
     | cvtBinOp (ASSIGN_DIVIDE) = Ctor ("ASSIGN_DIVIDE", NONE)
     | cvtBinOp (ASSIGN_REMAINDER) = Ctor ("ASSIGN_REMAINDER", NONE)
     | cvtBinOp (ASSIGN_LEFT_SHIFT) = Ctor ("ASSIGN_LEFT_SHIFT", NONE)
     | cvtBinOp (ASSIGN_RIGHT_SHIFT) = Ctor ("ASSIGN_RIGHT_SHIFT", NONE)
     | cvtBinOp (ASSIGN_RIGHT_SHIFT_UNSIGNED) = Ctor ("ASSIGN_RIGHT_SHIFT_UNSIGNED", 
          NONE)
     | cvtBinOp (ASSIGN_BITWISE_AND) = Ctor ("ASSIGN_BITWISE_AND", NONE)
     | cvtBinOp (ASSIGN_BITWISE_OR) = Ctor ("ASSIGN_BITWISE_OR", NONE)
     | cvtBinOp (ASSIGN_BITWISE_XOR) = Ctor ("ASSIGN_BITWISE_XOR", NONE)
     | cvtBinOp (ASSIGN_LOGICAL_AND) = Ctor ("ASSIGN_LOGICAL_AND", NONE)
     | cvtBinOp (ASSIGN_LOGICAL_OR) = Ctor ("ASSIGN_LOGICAL_OR", NONE)
     | cvtBinOp (ASSIGN_LOGICAL_XOR) = Ctor ("ASSIGN_LOGICAL_XOR", NONE)
   and cvtUnOp (DELETE) = Ctor ("DELETE", NONE)
     | cvtUnOp (VOID) = Ctor ("VOID", NONE)
     | cvtUnOp (TYPEOF) = Ctor ("TYPEOF", NONE)
     | cvtUnOp (PRE_INCREMENT) = Ctor ("PRE_INCREMENT", NONE)
     | cvtUnOp (PRE_DECREMENT) = Ctor ("PRE_DECREMENT", NONE)
     | cvtUnOp (POST_INCREMENT) = Ctor ("POST_INCREMENT", NONE)
     | cvtUnOp (POST_DECREMENT) = Ctor ("POST_DECREMENT", NONE)
     | cvtUnOp (UNARY_PLUS) = Ctor ("UNARY_PLUS", NONE)
     | cvtUnOp (UNARY_MINUS) = Ctor ("UNARY_MINUS", NONE)
     | cvtUnOp (BITWISE_NOT) = Ctor ("BITWISE_NOT", NONE)
     | cvtUnOp (LOGICAL_NOT) = Ctor ("LOGICAL_NOT", NONE)
     | cvtUnOp (MAKE_NAMESPACE) = Ctor ("MAKE_NAMESPACE", NONE)
     | cvtUnOp (TYPE) = Ctor ("TYPE", NONE)
   and cvtNulOp (THIS) = Ctor ("THIS", NONE)
     | cvtNulOp (EMPTY) = Ctor ("EMPTY", NONE)
   and cvtVarDefnTag (CONST) = Ctor ("CONST", NONE)
     | cvtVarDefnTag (VAR) = Ctor ("VAR", NONE)
     | cvtVarDefnTag (LETVAR) = Ctor ("LETVAR", NONE)
     | cvtVarDefnTag (LETCONST) = Ctor ("LETCONST", NONE)
     | cvtVarDefnTag (REST) = Ctor ("REST", NONE)
   and cvtNamespaceKind (PUBLIC) = Ctor ("PUBLIC", NONE)
     | cvtNamespaceKind (PRIVATE) = Ctor ("PRIVATE", NONE)
     | cvtNamespaceKind (PROTECTED) = Ctor ("PROTECTED", NONE)
     | cvtNamespaceKind (INTERNAL) = Ctor ("INTERNAL", NONE)
     | cvtNamespaceKind (INTRINSIC) = Ctor ("INTRINSIC", NONE)
     | cvtNamespaceKind (USERDEFINED) = Ctor ("USERDEFINED", NONE)
   and cvtPrimAnnotation (NAMED) = Ctor ("NAMED", NONE)
     | cvtPrimAnnotation (NULLABLE) = Ctor ("NULLABLE", NONE)
     | cvtPrimAnnotation (NONNULLABLE) = Ctor ("NONNULLABLE", NONE)
   and cvtSpecialTy (ANY) = Ctor ("ANY", NONE)
     | cvtSpecialTy (NULL) = Ctor ("NULL", NONE)
     | cvtSpecialTy (UNDEFINED) = Ctor ("UNDEFINED", NONE)
     | cvtSpecialTy (NOTYPE) = Ctor ("NOTYPE", NONE)
   and cvtDirective (UseNamespace ls97) = Ctor ("UseNamespace", SOME (List (List.map (fn x96 => 
                                                                                            cvtExpr x96
                                                                                     ) ls97)))
     | cvtDirective (UseNumber x103) = Ctor ("UseNumber", SOME (cvtNumberType x103))
     | cvtDirective (UseRounding x106) = Ctor ("UseRounding", SOME (cvtRoundingMode x106))
     | cvtDirective (Import{package=x109, qualifier=x110, alias=opt112}) = 
          Ctor ("Import", SOME (Rec [("package", cvtIdent x109), ("qualifier", 
          cvtImportQual x110), ("alias", 
       (case opt112 of
         NONE => Ctor ("NONE", NONE)
       | SOME x111 => Ctor ("SOME", SOME (cvtIdent x111))
       ))]))
   and cvtVisibility (Namespace(x125, x126)) = Ctor ("Namespace", SOME (Tuple [cvtNamespaceKind x125, 
          cvtUstring x126]))
   and cvtDefinition (NamespaceDefn{name=x130, init=x131}) = Ctor ("NamespaceDefn", 
          SOME (Rec [("name", cvtIdent x130), ("init", cvtExpr x131)]))
     | cvtDefinition (ClassDefn{name=x139, attrs=x140, params=ls142, extends=ls147, 
          implements=ls152, instanceVars=ls157, vars=ls162, constructor=x166, 
          methods=ls168, initializer=ls173}) = Ctor ("ClassDefn", SOME (Rec [("name", 
          cvtIdent x139), ("attrs", cvtAttributes x140), ("params", List (List.map (fn x141 => 
                                                                                          cvtIdent x141
                                                                                   ) ls142)), 
          ("extends", List (List.map (fn x146 => cvtTyExpr x146
                                     ) ls147)), ("implements", List (List.map (fn x151 => 
                                                                                     cvtTyExpr x151
                                                                              ) ls152)), 
          ("instanceVars", List (List.map (fn x156 => cvtVarDefn x156
                                          ) ls157)), ("vars", List (List.map (fn x161 => 
                                                                                    cvtVarDefn x161
                                                                             ) ls162)), 
          ("constructor", cvtFuncDefn x166), ("methods", List (List.map (fn x167 => 
                                                                               cvtFuncDefn x167
                                                                        ) ls168)), 
          ("initializer", List (List.map (fn x172 => cvtStmt x172
                                         ) ls173))]))
     | cvtDefinition (InterfaceDefn{name=x200, attrs=x201, params=ls203, extends=ls208, 
          methods=ls215}) = Ctor ("InterfaceDefn", SOME (Rec [("name", cvtIdent x200), 
          ("attrs", cvtAttributes x201), ("params", List (List.map (fn x202 => 
                                                                          cvtIdent x202
                                                                   ) ls203)), 
          ("extends", List (List.map (fn x207 => cvtTyExpr x207
                                     ) ls208)), ("methods", List (List.map (fn (x212, 
                                                                                  x213) => 
                                                                                  Tuple [cvtIdent x212, 
                                                                                  cvtFuncTy x213]
                                                                           ) ls215))]))
     | cvtDefinition (FunctionDefn x232) = Ctor ("FunctionDefn", SOME (cvtFuncDefn x232))
     | cvtDefinition (VariableDefn x235) = Ctor ("VariableDefn", SOME (cvtVarDefn x235))
   and cvtFuncSign (FunctionSignature{typeparams=ls239, params=ls244, resulttype=x248}) = 
          Ctor ("FunctionSignature", SOME (Rec [("typeparams", List (List.map (fn x238 => 
                                                                                     cvtLiteral x238
                                                                              ) ls239)), 
          ("params", List (List.map (fn x243 => cvtFormal x243
                                    ) ls244)), ("resulttype", cvtTyExpr x248)]))
   and cvtAttributes (Attributes{vis=x258, override=b259, static=b260, final=b261, 
          dynamic=b262, prototype=b263, nullable=b264}) = Ctor ("Attributes", 
          SOME (Rec [("vis", cvtVisibility x258), ("override", Bool b259), 
          ("static", Bool b260), ("final", Bool b261), ("dynamic", Bool b262), 
          ("prototype", Bool b263), ("nullable", Bool b264)]))
   and cvtVarDefn (SimpleDefn{tag=x282, init=opt284, attrs=x288, name=x289, 
          ty=opt291}) = Ctor ("SimpleDefn", SOME (Rec [("tag", cvtVarDefnTag x282), 
          ("init", 
       (case opt284 of
         NONE => Ctor ("NONE", NONE)
       | SOME x283 => Ctor ("SOME", SOME (cvtExpr x283))
       )), ("attrs", cvtAttributes x288), ("name", cvtIdent x289), ("ty", 
       (case opt291 of
         NONE => Ctor ("NONE", NONE)
       | SOME x290 => Ctor ("SOME", SOME (cvtTyExpr x290))
       ))]))
     | cvtVarDefn (DestructuringDefn{tag=x308, init=opt310, attrs=x314, temp=x315, 
          postInit=opt317, names=ls322, ty=opt327}) = Ctor ("DestructuringDefn", 
          SOME (Rec [("tag", cvtVarDefnTag x308), ("init", 
       (case opt310 of
         NONE => Ctor ("NONE", NONE)
       | SOME x309 => Ctor ("SOME", SOME (cvtExpr x309))
       )), ("attrs", cvtAttributes x314), ("temp", cvtIdent x315), ("postInit", 
          
       (case opt317 of
         NONE => Ctor ("NONE", NONE)
       | SOME x316 => Ctor ("SOME", SOME (cvtExpr x316))
       )), ("names", List (List.map (fn x321 => cvtIdent x321
                                    ) ls322)), ("ty", 
       (case opt327 of
         NONE => Ctor ("NONE", NONE)
       | SOME x326 => Ctor ("SOME", SOME (cvtTyExpr x326))
       ))]))
   and cvtTyExpr (SpecialType x348) = Ctor ("SpecialType", SOME (cvtSpecialTy x348))
     | cvtTyExpr (UnionType ls352) = Ctor ("UnionType", SOME (List (List.map (fn x351 => 
                                                                                    cvtTyExpr x351
                                                                             ) ls352)))
     | cvtTyExpr (ArrayType ls359) = Ctor ("ArrayType", SOME (List (List.map (fn x358 => 
                                                                                    cvtTyExpr x358
                                                                             ) ls359)))
     | cvtTyExpr (PrimaryType x365) = Ctor ("PrimaryType", SOME (cvtPrimTy x365))
     | cvtTyExpr (FunctionType x368) = Ctor ("FunctionType", SOME (cvtFuncTy x368))
     | cvtTyExpr (RecordType ls374) = Ctor ("RecordType", SOME (List (List.map (fn (x371, 
                                                                                      x372) => 
                                                                                      Tuple [cvtExpr x371, 
                                                                                      cvtTyExpr x372]
                                                                               ) ls374)))
     | cvtTyExpr (InstantiationType{base=x380, params=ls382}) = Ctor ("InstantiationType", 
          SOME (Rec [("base", cvtPrimTy x380), ("params", List (List.map (fn x381 => 
                                                                                cvtTyExpr x381
                                                                         ) ls382))]))
     | cvtTyExpr (UnresolvedType x393) = Ctor ("UnresolvedType", SOME (cvtExpr x393))
   and cvtStmt (EmptyStmt) = Ctor ("EmptyStmt", NONE)
     | cvtStmt (ExprStmt x397) = Ctor ("ExprStmt", SOME (cvtExpr x397))
     | cvtStmt (DefineStmt x400) = Ctor ("DefineStmt", SOME (cvtVarDefn x400))
     | cvtStmt (ForEachStmt x403) = Ctor ("ForEachStmt", SOME (cvtForEnumStmt x403))
     | cvtStmt (ForInStmt x406) = Ctor ("ForInStmt", SOME (cvtForEnumStmt x406))
     | cvtStmt (ThrowStmt x409) = Ctor ("ThrowStmt", SOME (cvtExpr x409))
     | cvtStmt (ReturnStmt x412) = Ctor ("ReturnStmt", SOME (cvtExpr x412))
     | cvtStmt (BreakStmt opt416) = Ctor ("BreakStmt", SOME 
       (case opt416 of
         NONE => Ctor ("NONE", NONE)
       | SOME x415 => Ctor ("SOME", SOME (cvtIdent x415))
       ))
     | cvtStmt (ContinueStmt opt423) = Ctor ("ContinueStmt", SOME 
       (case opt423 of
         NONE => Ctor ("NONE", NONE)
       | SOME x422 => Ctor ("SOME", SOME (cvtIdent x422))
       ))
     | cvtStmt (BlockStmt x429) = Ctor ("BlockStmt", SOME (cvtBlock x429))
     | cvtStmt (LabeledStmt(x432, x433)) = Ctor ("LabeledStmt", SOME (Tuple [cvtIdent x432, 
          cvtStmt x433]))
     | cvtStmt (LetStmt(ls438, x442)) = Ctor ("LetStmt", SOME (Tuple [List (List.map (fn x437 => 
                                                                                            cvtVarDefn x437
                                                                                     ) ls438), 
          cvtBlock x442]))
     | cvtStmt (SuperStmt ls447) = Ctor ("SuperStmt", SOME (List (List.map (fn x446 => 
                                                                                  cvtExpr x446
                                                                           ) ls447)))
     | cvtStmt (WhileStmt x453) = Ctor ("WhileStmt", SOME (cvtWhileStmt x453))
     | cvtStmt (DoWhileStmt x456) = Ctor ("DoWhileStmt", SOME (cvtWhileStmt x456))
     | cvtStmt (ForStmt{isVar=b459, defns=ls461, init=x465, cond=x466, update=x467, 
          contLabel=opt469, body=x473}) = Ctor ("ForStmt", SOME (Rec [("isVar", 
          Bool b459), ("defns", List (List.map (fn x460 => cvtVarDefn x460
                                               ) ls461)), ("init", cvtExpr x465), 
          ("cond", cvtExpr x466), ("update", cvtExpr x467), ("contLabel", 
       (case opt469 of
         NONE => Ctor ("NONE", NONE)
       | SOME x468 => Ctor ("SOME", SOME (cvtIdent x468))
       )), ("body", cvtStmt x473)]))
     | cvtStmt (IfStmt{cond=x491, consequent=x492, alternative=x493}) = Ctor ("IfStmt", 
          SOME (Rec [("cond", cvtExpr x491), ("consequent", cvtStmt x492), 
          ("alternative", cvtStmt x493)]))
     | cvtStmt (WithStmt{obj=x503, body=x504}) = Ctor ("WithStmt", SOME (Rec [("obj", 
          cvtExpr x503), ("body", cvtStmt x504)]))
     | cvtStmt (TryStmt{body=x512, catches=ls516, finally=x520}) = Ctor ("TryStmt", 
          SOME (Rec [("body", cvtBlock x512), ("catches", List (List.map (fn (x513, 
                                                                                x514) => 
                                                                                Tuple [cvtFormal x513, 
                                                                                cvtBlock x514]
                                                                         ) ls516)), 
          ("finally", cvtBlock x520)]))
     | cvtStmt (SwitchStmt{cond=x530, cases=ls538, default=ls543}) = Ctor ("SwitchStmt", 
          SOME (Rec [("cond", cvtExpr x530), ("cases", List (List.map (fn (x531, 
                                                                             ls533) => 
                                                                             Tuple [cvtExpr x531, 
                                                                             List (List.map (fn x532 => 
                                                                                                   cvtStmt x532
                                                                                            ) ls533)]
                                                                      ) ls538)), 
          ("default", List (List.map (fn x542 => cvtStmt x542
                                     ) ls543))]))
   and cvtExpr (TrinaryExpr(x556, x557, x558, x559)) = Ctor ("TrinaryExpr", 
          SOME (Tuple [cvtTriOp x556, cvtExpr x557, cvtExpr x558, cvtExpr x559]))
     | cvtExpr (BinaryExpr(x563, x564, x565)) = Ctor ("BinaryExpr", SOME (Tuple [cvtBinOp x563, 
          cvtExpr x564, cvtExpr x565]))
     | cvtExpr (BinaryTypeExpr(x569, x570, x571)) = Ctor ("BinaryTypeExpr", 
          SOME (Tuple [cvtBinOp x569, cvtExpr x570, cvtTyExpr x571]))
     | cvtExpr (UnaryExpr(x575, x576)) = Ctor ("UnaryExpr", SOME (Tuple [cvtUnOp x575, 
          cvtExpr x576]))
     | cvtExpr (TypeExpr x580) = Ctor ("TypeExpr", SOME (cvtTyExpr x580))
     | cvtExpr (NullaryExpr x583) = Ctor ("NullaryExpr", SOME (cvtNulOp x583))
     | cvtExpr (YieldExpr opt587) = Ctor ("YieldExpr", SOME 
       (case opt587 of
         NONE => Ctor ("NONE", NONE)
       | SOME x586 => Ctor ("SOME", SOME (cvtExpr x586))
       ))
     | cvtExpr (SuperExpr opt594) = Ctor ("SuperExpr", SOME 
       (case opt594 of
         NONE => Ctor ("NONE", NONE)
       | SOME x593 => Ctor ("SOME", SOME (cvtExpr x593))
       ))
     | cvtExpr (LiteralExpr x600) = Ctor ("LiteralExpr", SOME (cvtLiteral x600))
     | cvtExpr (CallExpr{func=x603, actuals=ls605}) = Ctor ("CallExpr", SOME (Rec [("func", 
          cvtExpr x603), ("actuals", List (List.map (fn x604 => cvtExpr x604
                                                    ) ls605))]))
     | cvtExpr (Property{indirect=b616, obj=opt618, field=x622}) = Ctor ("Property", 
          SOME (Rec [("indirect", Bool b616), ("obj", 
       (case opt618 of
         NONE => Ctor ("NONE", NONE)
       | SOME x617 => Ctor ("SOME", SOME (cvtExpr x617))
       )), ("field", cvtExpr x622)]))
     | cvtExpr (Ref{base=opt633, ident=x637}) = Ctor ("Ref", SOME (Rec [("base", 
          
       (case opt633 of
         NONE => Ctor ("NONE", NONE)
       | SOME x632 => Ctor ("SOME", SOME (cvtExpr x632))
       )), ("ident", cvtIdentExpr x637)]))
     | cvtExpr (QualIdent{qual=opt646, ident=x650, opennss=ls652}) = Ctor ("QualIdent", 
          SOME (Rec [("qual", 
       (case opt646 of
         NONE => Ctor ("NONE", NONE)
       | SOME x645 => Ctor ("SOME", SOME (cvtExpr x645))
       )), ("ident", cvtUstring x650), ("opennss", List (List.map (fn x651 => 
                                                                         cvtVisibility x651
                                                                  ) ls652))]))
     | cvtExpr (QualExpr{qual=opt666, expr=x670, opennss=ls672}) = Ctor ("QualExpr", 
          SOME (Rec [("qual", 
       (case opt666 of
         NONE => Ctor ("NONE", NONE)
       | SOME x665 => Ctor ("SOME", SOME (cvtExpr x665))
       )), ("expr", cvtExpr x670), ("opennss", List (List.map (fn x671 => cvtVisibility x671
                                                              ) ls672))]))
     | cvtExpr (AttrQualIdent{indirect=b685, operand=x686}) = Ctor ("AttrQualIdent", 
          SOME (Rec [("indirect", Bool b685), ("operand", cvtExpr x686)]))
     | cvtExpr (LetExpr{defs=ls695, body=x699}) = Ctor ("LetExpr", SOME (Rec [("defs", 
          List (List.map (fn x694 => cvtVarDefn x694
                         ) ls695)), ("body", cvtExpr x699)]))
     | cvtExpr (NewExpr{obj=x707, actuals=ls709}) = Ctor ("NewExpr", SOME (Rec [("obj", 
          cvtExpr x707), ("actuals", List (List.map (fn x708 => cvtExpr x708
                                                    ) ls709))]))
     | cvtExpr (FunExpr{ident=opt721, sign=x725, body=x726}) = Ctor ("FunExpr", 
          SOME (Rec [("ident", 
       (case opt721 of
         NONE => Ctor ("NONE", NONE)
       | SOME x720 => Ctor ("SOME", SOME (cvtIdent x720))
       )), ("sign", cvtFuncSign x725), ("body", cvtBlock x726)]))
     | cvtExpr (ListExpr ls737) = Ctor ("ListExpr", SOME (List (List.map (fn x736 => 
                                                                                cvtExpr x736
                                                                         ) ls737)))
   and cvtIdentOrExpr (Ident x743) = Ctor ("Ident", SOME (cvtIdent x743))
     | cvtIdentOrExpr (Expr x746) = Ctor ("Expr", SOME (cvtExpr x746))
   and cvtIdentExpr (QualifiedIdentifier{qual=x749, ident=x750}) = Ctor ("QualifiedIdentifier", 
          SOME (Rec [("qual", cvtExpr x749), ("ident", cvtUstring x750)]))
     | cvtIdentExpr (QualifiedExpression{qual=x758, expr=x759}) = Ctor ("QualifiedExpression", 
          SOME (Rec [("qual", cvtExpr x758), ("expr", cvtExpr x759)]))
     | cvtIdentExpr (AttributeIdentifier x767) = Ctor ("AttributeIdentifier", 
          SOME (cvtIdentExpr x767))
     | cvtIdentExpr (Identifier x770) = Ctor ("Identifier", SOME (cvtIdent x770))
     | cvtIdentExpr (Expression x773) = Ctor ("Expression", SOME (cvtExpr x773))
   and cvtLiteral (LiteralNull) = Ctor ("LiteralNull", NONE)
     | cvtLiteral (LiteralUndefined) = Ctor ("LiteralUndefined", NONE)
     | cvtLiteral (LiteralNumber r778) = Ctor ("LiteralNumber", SOME (Real r778))
     | cvtLiteral (LiteralBoolean b781) = Ctor ("LiteralBoolean", SOME (Bool b781))
     | cvtLiteral (LiteralString x784) = Ctor ("LiteralString", SOME (cvtUstring x784))
     | cvtLiteral (LiteralArray ls788) = Ctor ("LiteralArray", SOME (List (List.map (fn x787 => 
                                                                                           cvtExpr x787
                                                                                    ) ls788)))
     | cvtLiteral (LiteralXML ls795) = Ctor ("LiteralXML", SOME (List (List.map (fn x794 => 
                                                                                       cvtExpr x794
                                                                                ) ls795)))
     | cvtLiteral (LiteralNamespace x801) = Ctor ("LiteralNamespace", SOME (cvtVisibility x801))
     | cvtLiteral (LiteralObject ls811) = Ctor ("LiteralObject", SOME (List (List.map (fn {name=x804, 
                                                                                             init=x805} => 
                                                                                             Rec [("name", 
                                                                                             cvtExpr x804), 
                                                                                             ("init", 
                                                                                             cvtExpr x805)]
                                                                                      ) ls811)))
     | cvtLiteral (LiteralRegExp{pattern=x817, global=b818, multiline=b819, 
          caseInsensitive=b820}) = Ctor ("LiteralRegExp", SOME (Rec [("pattern", 
          cvtUstring x817), ("global", Bool b818), ("multiline", Bool b819), 
          ("caseInsensitive", Bool b820)]))
   and cvtBlock (Block{directives=ls833, defns=ls838, stmts=ls843}) = Ctor ("Block", 
          SOME (Rec [("directives", List (List.map (fn x832 => cvtDirective x832
                                                   ) ls833)), ("defns", List (List.map (fn x837 => 
                                                                                              cvtDefinition x837
                                                                                       ) ls838)), 
          ("stmts", List (List.map (fn x842 => cvtStmt x842
                                   ) ls843))]))
   and cvtFuncTy {paramTypes=ls857, returnType=x861, boundThisType=opt863, 
          hasRest=b867} = Rec [("paramTypes", List (List.map (fn x856 => cvtTyExpr x856
                                                             ) ls857)), ("returnType", 
          cvtTyExpr x861), ("boundThisType", 
       (case opt863 of
         NONE => Ctor ("NONE", NONE)
       | SOME x862 => Ctor ("SOME", SOME (cvtTyExpr x862))
       )), ("hasRest", Bool b867)]
   and cvtFormal {name=x877, ty=opt879, init=opt884, tag=x888, isRest=b889} = 
          Rec [("name", cvtIdent x877), ("ty", 
       (case opt879 of
         NONE => Ctor ("NONE", NONE)
       | SOME x878 => Ctor ("SOME", SOME (cvtTyExpr x878))
       )), ("init", 
       (case opt884 of
         NONE => Ctor ("NONE", NONE)
       | SOME x883 => Ctor ("SOME", SOME (cvtExpr x883))
       )), ("tag", cvtVarDefnTag x888), ("isRest", Bool b889)]
   and cvtTypedIdent {name=x901, ty=opt903} = Rec [("name", cvtIdent x901), 
          ("ty", 
       (case opt903 of
         NONE => Ctor ("NONE", NONE)
       | SOME x902 => Ctor ("SOME", SOME (cvtTyExpr x902))
       ))]
   and cvtFuncDefn {name=x912, attrs=x913, formals=ls915, ty=opt920, body=x924} = 
          Rec [("name", cvtIdent x912), ("attrs", cvtAttributes x913), ("formals", 
          List (List.map (fn x914 => cvtFormal x914
                         ) ls915)), ("ty", 
       (case opt920 of
         NONE => Ctor ("NONE", NONE)
       | SOME x919 => Ctor ("SOME", SOME (cvtTyExpr x919))
       )), ("body", cvtBlock x924)]
   and cvtPrimTy {name=s936, annotation=x937} = Rec [("name", String s936), 
          ("annotation", cvtPrimAnnotation x937)]
   and cvtForEnumStmt {isVar=b943, init=x944, obj=x945, defns=ls947, contLabel=opt952, 
          body=x956} = Rec [("isVar", Bool b943), ("init", cvtExpr x944), ("obj", 
          cvtExpr x945), ("defns", List (List.map (fn x946 => cvtVarDefn x946
                                                  ) ls947)), ("contLabel", 
          
       (case opt952 of
         NONE => Ctor ("NONE", NONE)
       | SOME x951 => Ctor ("SOME", SOME (cvtIdent x951))
       )), ("body", cvtStmt x956)]
   and cvtWhileStmt {cond=x970, body=x971, contLabel=opt973} = Rec [("cond", 
          cvtExpr x970), ("body", cvtStmt x971), ("contLabel", 
       (case opt973 of
         NONE => Ctor ("NONE", NONE)
       | SOME x972 => Ctor ("SOME", SOME (cvtIdent x972))
       ))]
   and cvtPackage {names=ls985, fullname=x989, body=x990} = Rec [("names", 
          List (List.map (fn x984 => cvtIdent x984
                         ) ls985)), ("fullname", cvtUstring x989), ("body", 
          cvtBlock x990)]
   and cvtProgram {packages=ls999, body=x1003} = Rec [("packages", List (List.map (fn x998 => 
                                                                                         cvtPackage x998
                                                                                  ) ls999)), 
          ("body", cvtBlock x1003)]
end

