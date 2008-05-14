structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENTIFIER s19 = PrettyRep.UniStr s19
   and cvtNONCE n20 = PrettyRep.Int n20
   and cvtOPAQUE_NAMESPACE_IDENTIFIER x21 = cvtNONCE x21
   and cvtNAMESPACE (TransparentNamespace s22) = PrettyRep.Ctor ("TransparentNamespace", 
          SOME (PrettyRep.UniStr s22))
     | cvtNAMESPACE (OpaqueNamespace x25) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtOPAQUE_NAMESPACE_IDENTIFIER x25))
   and cvtNAME {ns=x28, id=x29} = PrettyRep.Rec [("ns", cvtNAMESPACE x28), 
          ("id", cvtIDENTIFIER x29)]
   and cvtNAMESPACE_SET ls36 = PrettyRep.List (List.map (fn x35 => cvtNAMESPACE x35
                                                        ) ls36)
   and cvtOPEN_NAMESPACES ls41 = PrettyRep.List (List.map (fn x40 => cvtNAMESPACE_SET x40
                                                          ) ls41)
   and cvtNAME_SET ls46 = PrettyRep.List (List.map (fn x45 => cvtNAME x45
                                                   ) ls46)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (Like) = PrettyRep.Ctor ("Like", NONE)
   and cvtBINOP (Plus) = PrettyRep.Ctor ("Plus", NONE)
     | cvtBINOP (Minus) = PrettyRep.Ctor ("Minus", NONE)
     | cvtBINOP (Times) = PrettyRep.Ctor ("Times", NONE)
     | cvtBINOP (Divide) = PrettyRep.Ctor ("Divide", NONE)
     | cvtBINOP (Remainder) = PrettyRep.Ctor ("Remainder", NONE)
     | cvtBINOP (LeftShift) = PrettyRep.Ctor ("LeftShift", NONE)
     | cvtBINOP (RightShift) = PrettyRep.Ctor ("RightShift", NONE)
     | cvtBINOP (RightShiftUnsigned) = PrettyRep.Ctor ("RightShiftUnsigned", 
          NONE)
     | cvtBINOP (BitwiseAnd) = PrettyRep.Ctor ("BitwiseAnd", NONE)
     | cvtBINOP (BitwiseOr) = PrettyRep.Ctor ("BitwiseOr", NONE)
     | cvtBINOP (BitwiseXor) = PrettyRep.Ctor ("BitwiseXor", NONE)
     | cvtBINOP (LogicalAnd) = PrettyRep.Ctor ("LogicalAnd", NONE)
     | cvtBINOP (LogicalOr) = PrettyRep.Ctor ("LogicalOr", NONE)
     | cvtBINOP (InstanceOf) = PrettyRep.Ctor ("InstanceOf", NONE)
     | cvtBINOP (In) = PrettyRep.Ctor ("In", NONE)
     | cvtBINOP (Equals) = PrettyRep.Ctor ("Equals", NONE)
     | cvtBINOP (NotEquals) = PrettyRep.Ctor ("NotEquals", NONE)
     | cvtBINOP (StrictEquals) = PrettyRep.Ctor ("StrictEquals", NONE)
     | cvtBINOP (StrictNotEquals) = PrettyRep.Ctor ("StrictNotEquals", NONE)
     | cvtBINOP (Less) = PrettyRep.Ctor ("Less", NONE)
     | cvtBINOP (LessOrEqual) = PrettyRep.Ctor ("LessOrEqual", NONE)
     | cvtBINOP (Greater) = PrettyRep.Ctor ("Greater", NONE)
     | cvtBINOP (GreaterOrEqual) = PrettyRep.Ctor ("GreaterOrEqual", NONE)
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus) = PrettyRep.Ctor ("AssignPlus", NONE)
     | cvtASSIGNOP (AssignMinus) = PrettyRep.Ctor ("AssignMinus", NONE)
     | cvtASSIGNOP (AssignTimes) = PrettyRep.Ctor ("AssignTimes", NONE)
     | cvtASSIGNOP (AssignDivide) = PrettyRep.Ctor ("AssignDivide", NONE)
     | cvtASSIGNOP (AssignRemainder) = PrettyRep.Ctor ("AssignRemainder", NONE)
     | cvtASSIGNOP (AssignLeftShift) = PrettyRep.Ctor ("AssignLeftShift", NONE)
     | cvtASSIGNOP (AssignRightShift) = PrettyRep.Ctor ("AssignRightShift", 
          NONE)
     | cvtASSIGNOP (AssignRightShiftUnsigned) = PrettyRep.Ctor ("AssignRightShiftUnsigned", 
          NONE)
     | cvtASSIGNOP (AssignBitwiseAnd) = PrettyRep.Ctor ("AssignBitwiseAnd", 
          NONE)
     | cvtASSIGNOP (AssignBitwiseOr) = PrettyRep.Ctor ("AssignBitwiseOr", NONE)
     | cvtASSIGNOP (AssignBitwiseXor) = PrettyRep.Ctor ("AssignBitwiseXor", 
          NONE)
     | cvtASSIGNOP (AssignLogicalAnd) = PrettyRep.Ctor ("AssignLogicalAnd", 
          NONE)
     | cvtASSIGNOP (AssignLogicalOr) = PrettyRep.Ctor ("AssignLogicalOr", NONE)
   and cvtUNOP (Delete) = PrettyRep.Ctor ("Delete", NONE)
     | cvtUNOP (Void) = PrettyRep.Ctor ("Void", NONE)
     | cvtUNOP (Typeof) = PrettyRep.Ctor ("Typeof", NONE)
     | cvtUNOP (PreIncrement) = PrettyRep.Ctor ("PreIncrement", NONE)
     | cvtUNOP (PreDecrement) = PrettyRep.Ctor ("PreDecrement", NONE)
     | cvtUNOP (PostIncrement) = PrettyRep.Ctor ("PostIncrement", NONE)
     | cvtUNOP (PostDecrement) = PrettyRep.Ctor ("PostDecrement", NONE)
     | cvtUNOP (UnaryPlus) = PrettyRep.Ctor ("UnaryPlus", NONE)
     | cvtUNOP (UnaryMinus) = PrettyRep.Ctor ("UnaryMinus", NONE)
     | cvtUNOP (BitwiseNot) = PrettyRep.Ctor ("BitwiseNot", NONE)
     | cvtUNOP (LogicalNot) = PrettyRep.Ctor ("LogicalNot", NONE)
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
     | cvtUNOP (Spread) = PrettyRep.Ctor ("Spread", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtPRAGMA (UseNamespace x108) = PrettyRep.Ctor ("UseNamespace", SOME (cvtNAMESPACE_EXPRESSION x108))
     | cvtPRAGMA (UseDefaultNamespace x111) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtNAMESPACE_EXPRESSION x111))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtNAME_EXPRESSION (QualifiedName{namespace=x116, identifier=x117}) = 
          PrettyRep.Ctor ("QualifiedName", SOME (PrettyRep.Rec [("namespace", 
          cvtNAMESPACE_EXPRESSION x116), ("identifier", cvtIDENTIFIER x117)]))
     | cvtNAME_EXPRESSION (UnqualifiedName{identifier=x125, openNamespaces=x126, 
          globalNames=x127}) = PrettyRep.Ctor ("UnqualifiedName", SOME (PrettyRep.Rec [("identifier", 
          cvtIDENTIFIER x125), ("openNamespaces", cvtOPEN_NAMESPACES x126), 
          ("globalNames", cvtNAME_SET x127)]))
     | cvtNAME_EXPRESSION (ResolvedName x137) = PrettyRep.Ctor ("ResolvedName", 
          SOME (cvtNAME x137))
   and cvtNAMESPACE_EXPRESSION (Namespace x140) = PrettyRep.Ctor ("Namespace", 
          SOME (cvtNAMESPACE x140))
     | cvtNAMESPACE_EXPRESSION (NamespaceName x143) = PrettyRep.Ctor ("NamespaceName", 
          SOME (cvtNAME_EXPRESSION x143))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x152, privateNS=x153, protectedNS=x154, parentProtectedNSs=ls156, 
          typeParams=ls161, nonnullable=b165, dynamic=b166, extends=opt168, 
          implements=ls173, classRib=x177, instanceRib=x178, instanceInits=x179, 
          constructor=opt181, classType=x185, instanceType=x186}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x152), ("privateNS", cvtNAMESPACE x153), 
          ("protectedNS", cvtNAMESPACE x154), ("parentProtectedNSs", PrettyRep.List (List.map (fn x155 => 
                                                                                                     cvtNAMESPACE x155
                                                                                              ) ls156)), 
          ("typeParams", PrettyRep.List (List.map (fn x160 => cvtIDENTIFIER x160
                                                  ) ls161)), ("nonnullable", 
          PrettyRep.Bool b165), ("dynamic", PrettyRep.Bool b166), ("extends", 
          
       (case opt168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x167 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x167))
       )), ("implements", PrettyRep.List (List.map (fn x172 => cvtTYPE x172
                                                   ) ls173)), ("classRib", 
          cvtRIB x177), ("instanceRib", cvtRIB x178), ("instanceInits", cvtHEAD x179), 
          ("constructor", 
       (case opt181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x180 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x180))
       )), ("classType", cvtTYPE x185), ("instanceType", cvtTYPE x186)]))
   and cvtIFACE (Iface{name=x220, typeParams=ls222, nonnullable=b226, extends=ls228, 
          instanceRib=x232, instanceType=x233}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x220), ("typeParams", PrettyRep.List (List.map (fn x221 => 
                                                                                                      cvtIDENTIFIER x221
                                                                                               ) ls222)), 
          ("nonnullable", PrettyRep.Bool b226), ("extends", PrettyRep.List (List.map (fn x227 => 
                                                                                            cvtTYPE x227
                                                                                     ) ls228)), 
          ("instanceRib", cvtRIB x232), ("instanceType", cvtTYPE x233)]))
   and cvtCTOR (Ctor{settings=x249, superArgs=ls251, func=x255}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x249), ("superArgs", PrettyRep.List (List.map (fn x250 => 
                                                                                                         cvtEXPRESSION x250
                                                                                                  ) ls251)), 
          ("func", cvtFUNC x255)]))
   and cvtFUNC (Func{name=x265, fsig=x266, native=b267, generator=b268, block=opt270, 
          param=x274, defaults=ls276, ty=x280, loc=opt282}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x265), ("fsig", cvtFUNC_SIG x266), 
          ("native", PrettyRep.Bool b267), ("generator", PrettyRep.Bool b268), 
          ("block", 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x269))
       )), ("param", cvtHEAD x274), ("defaults", PrettyRep.List (List.map (fn x275 => 
                                                                                 cvtEXPRESSION x275
                                                                          ) ls276)), 
          ("ty", cvtTYPE x280), ("loc", 
       (case opt282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x281 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x281))
       ))]))
   and cvtDEFN (ClassDefn x307) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x307))
     | cvtDEFN (VariableDefn x310) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x310))
     | cvtDEFN (FunctionDefn x313) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x313))
     | cvtDEFN (ConstructorDefn x316) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x316))
     | cvtDEFN (InterfaceDefn x319) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x319))
     | cvtDEFN (NamespaceDefn x322) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x322))
     | cvtDEFN (TypeDefn x325) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x325))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls329, params=x333, paramTypes=ls335, 
          defaults=ls340, ctorInits=opt351, returnType=opt356, thisType=opt361, 
          hasRest=b365}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x328 => cvtIDENTIFIER x328
                                   ) ls329)), ("params", cvtBINDINGS x333), 
          ("paramTypes", PrettyRep.List (List.map (fn x334 => cvtTYPE x334
                                                  ) ls335)), ("defaults", PrettyRep.List (List.map (fn x339 => 
                                                                                                          cvtEXPRESSION x339
                                                                                                   ) ls340)), 
          ("ctorInits", 
       (case opt351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x344, ls346) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x344, 
            PrettyRep.List (List.map (fn x345 => cvtEXPRESSION x345
                                     ) ls346)]))
       )), ("returnType", 
       (case opt356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x355 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x355))
       )), ("thisType", 
       (case opt361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x360 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x360))
       )), ("hasRest", PrettyRep.Bool b365)]))
   and cvtBINDING (Binding{ident=x385, ty=x386}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x385), ("ty", 
          cvtTYPE x386)]))
   and cvtBINDING_IDENTIFIER (TempIdent n394) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n394))
     | cvtBINDING_IDENTIFIER (ParamIdent n397) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n397))
     | cvtBINDING_IDENTIFIER (PropIdent x400) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x400))
   and cvtINIT_STEP (InitStep(x403, x404)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x403, 
          cvtEXPRESSION x404]))
     | cvtINIT_STEP (AssignStep(x408, x409)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x408, cvtEXPRESSION x409]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (RecordType ls419) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn (x416, 
                                                                                                        x417) => 
                                                                                                        PrettyRep.Tuple [cvtNAME_EXPRESSION x416, 
                                                                                                        cvtTYPE x417]
                                                                                                 ) ls419)))
     | cvtTYPE (ArrayType(ls426, opt431)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x425 => 
                                                                                                                                cvtTYPE x425
                                                                                                                         ) ls426), 
          
       (case opt431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x430 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x430))
       )]))
     | cvtTYPE (UnionType ls439) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x438 => 
                                                                                                      cvtTYPE x438
                                                                                               ) ls439)))
     | cvtTYPE (NonNullType x445) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x445))
     | cvtTYPE (FunctionType x448) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x448))
     | cvtTYPE (AppType(x451, ls453)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x451, 
          PrettyRep.List (List.map (fn x452 => cvtTYPE x452
                                   ) ls453)]))
     | cvtTYPE (TypeName(x460, opt462)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x460, 
          
       (case opt462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x461 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x461))
       )]))
     | cvtTYPE (InstanceType x469) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x469))
     | cvtTYPE (TypeNameReferenceType(x472, x473)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x472, cvtNAME_EXPRESSION x473]))
     | cvtTYPE (TypeIndexReferenceType(x477, n478)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x477, PrettyRep.Int n478]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x483) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x483))
     | cvtSTATEMENT (InitStmt{kind=x486, ns=opt488, prototype=b492, static=b493, 
          temps=x494, inits=ls496}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x486), ("ns", 
       (case opt488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x487 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x487))
       )), ("prototype", PrettyRep.Bool b492), ("static", PrettyRep.Bool b493), 
          ("temps", cvtBINDINGS x494), ("inits", PrettyRep.List (List.map (fn x495 => 
                                                                                 cvtINIT_STEP x495
                                                                          ) ls496))]))
     | cvtSTATEMENT (ClassBlock x515) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x515))
     | cvtSTATEMENT (ForInStmt x518) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x518))
     | cvtSTATEMENT (ThrowStmt x521) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x521))
     | cvtSTATEMENT (ReturnStmt x524) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x524))
     | cvtSTATEMENT (BreakStmt opt528) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x527 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x527))
       ))
     | cvtSTATEMENT (ContinueStmt opt535) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x534 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x534))
       ))
     | cvtSTATEMENT (BlockStmt x541) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x541))
     | cvtSTATEMENT (LabeledStmt(x544, x545)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x544, cvtSTATEMENT x545]))
     | cvtSTATEMENT (LetStmt x549) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x549))
     | cvtSTATEMENT (WhileStmt x552) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x552))
     | cvtSTATEMENT (DoWhileStmt x555) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x555))
     | cvtSTATEMENT (ForStmt x558) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x558))
     | cvtSTATEMENT (IfStmt{cnd=x561, thn=x562, els=x563}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x561), ("thn", cvtSTATEMENT x562), 
          ("els", cvtSTATEMENT x563)]))
     | cvtSTATEMENT (WithStmt{obj=x573, ty=x574, body=x575}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x573), ("ty", cvtTYPE x574), 
          ("body", cvtSTATEMENT x575)]))
     | cvtSTATEMENT (TryStmt{block=x585, catches=ls587, finally=opt592}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x585), 
          ("catches", PrettyRep.List (List.map (fn x586 => cvtCATCH_CLAUSE x586
                                               ) ls587)), ("finally", 
       (case opt592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x591 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x591))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x605, labels=ls607, cases=ls612}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x605), ("labels", PrettyRep.List (List.map (fn x606 => 
                                                                                                        cvtIDENTIFIER x606
                                                                                                 ) ls607)), 
          ("cases", PrettyRep.List (List.map (fn x611 => cvtCASE x611
                                             ) ls612))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x625, ty=x626, cases=ls628}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x625), ("ty", cvtTYPE x626), 
          ("cases", PrettyRep.List (List.map (fn x627 => cvtCATCH_CLAUSE x627
                                             ) ls628))]))
     | cvtSTATEMENT (DXNStmt{expr=x641}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x641)]))
   and cvtEXPRESSION (TernaryExpr(x647, x648, x649)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x647, cvtEXPRESSION x648, cvtEXPRESSION x649]))
     | cvtEXPRESSION (BinaryExpr(x653, x654, x655)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x653, cvtEXPRESSION x654, cvtEXPRESSION x655]))
     | cvtEXPRESSION (BinaryTypeExpr(x659, x660, x661)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x659, cvtEXPRESSION x660, cvtTYPE x661]))
     | cvtEXPRESSION (UnaryExpr(x665, x666)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x665, cvtEXPRESSION x666]))
     | cvtEXPRESSION (TypeExpr x670) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x670))
     | cvtEXPRESSION (ThisExpr opt674) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt674 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x673 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x673))
       ))
     | cvtEXPRESSION (YieldExpr opt681) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt681 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x680 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x680))
       ))
     | cvtEXPRESSION (SuperExpr opt688) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x687 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x687))
       ))
     | cvtEXPRESSION (CallExpr{func=x694, actuals=ls696}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x694), ("actuals", PrettyRep.List (List.map (fn x695 => 
                                                                                                         cvtEXPRESSION x695
                                                                                                  ) ls696))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x707, actuals=ls709}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x707), ("actuals", PrettyRep.List (List.map (fn x708 => 
                                                                                                         cvtTYPE x708
                                                                                                  ) ls709))]))
     | cvtEXPRESSION (LetExpr{defs=x720, body=x721, head=opt723}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x720), ("body", cvtEXPRESSION x721), 
          ("head", 
       (case opt723 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x722 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x722))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x736, actuals=ls738}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x736), ("actuals", PrettyRep.List (List.map (fn x737 => 
                                                                                                        cvtEXPRESSION x737
                                                                                                 ) ls738))]))
     | cvtEXPRESSION (SetExpr(x749, x750, x751)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x749, cvtEXPRESSION x750, cvtEXPRESSION x751]))
     | cvtEXPRESSION (ListExpr ls756) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x755 => 
                                                                                                          cvtEXPRESSION x755
                                                                                                   ) ls756)))
     | cvtEXPRESSION (InitExpr(x762, x763, x764)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x762, cvtHEAD x763, cvtINITS x764]))
     | cvtEXPRESSION (GetTemp n768) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n768))
     | cvtEXPRESSION (GetParam n771) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n771))
     | cvtEXPRESSION (Comprehension(x774, ls776, opt781)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x774, PrettyRep.List (List.map (fn x775 => 
                                                                                     cvtFOR_ENUM_HEAD x775
                                                                              ) ls776), 
          
       (case opt781 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x780 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x780))
       )]))
     | cvtEXPRESSION (LiteralExpr x788) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x788))
     | cvtEXPRESSION (LexicalReference{name=x791, loc=opt793}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x791), ("loc", 
       (case opt793 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x792 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x792))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x804, name=x805, loc=opt807}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x804), ("name", cvtNAME_EXPRESSION x805), ("loc", 
       (case opt807 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x806 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x806))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x820, index=x821, loc=opt823}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x820), ("index", cvtEXPRESSION x821), ("loc", 
       (case opt823 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x822 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x822))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n841) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n841))
     | cvtFIXTURE_NAME (PropName x844) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x844))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r849) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r849))
     | cvtLITERAL (LiteralDecimal d852) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d852))
     | cvtLITERAL (LiteralBoolean b855) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b855))
     | cvtLITERAL (LiteralString s858) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s858))
     | cvtLITERAL (LiteralArray{exprs=x861, ty=opt863}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x861), ("ty", 
       (case opt863 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x862 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x862))
       ))]))
     | cvtLITERAL (LiteralXML ls875) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x874 => 
                                                                                                           cvtEXPRESSION x874
                                                                                                    ) ls875)))
     | cvtLITERAL (LiteralNamespace x881) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x881))
     | cvtLITERAL (LiteralObject{expr=ls885, ty=opt890}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x884 => 
                                                                        cvtFIELD x884
                                                                 ) ls885)), 
          ("ty", 
       (case opt890 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x889 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x889))
       ))]))
     | cvtLITERAL (LiteralFunction x901) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x901))
     | cvtLITERAL (LiteralRegExp{str=s904}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s904)]))
   and cvtBLOCK (Block x910) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x910))
   and cvtFIXTURE (NamespaceFixture x913) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x913))
     | cvtFIXTURE (ClassFixture x916) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x916))
     | cvtFIXTURE (InterfaceFixture x919) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x919))
     | cvtFIXTURE (TypeVarFixture x922) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x922))
     | cvtFIXTURE (TypeFixture(ls926, x930)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x925 => cvtIDENTIFIER x925
                                                          ) ls926), cvtTYPE x930]))
     | cvtFIXTURE (MethodFixture{func=x934, ty=x935, writable=b936, override=b937, 
          final=b938}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x934), ("ty", cvtTYPE x935), ("writable", PrettyRep.Bool b936), 
          ("override", PrettyRep.Bool b937), ("final", PrettyRep.Bool b938)]))
     | cvtFIXTURE (ValFixture{ty=x952, writable=b953}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x952), ("writable", PrettyRep.Bool b953)]))
     | cvtFIXTURE (VirtualValFixture{ty=x961, getter=opt963, setter=opt968}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x961), ("getter", 
       (case opt963 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x962 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x962))
       )), ("setter", 
       (case opt968 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x967 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x967))
       ))]))
   and cvtHEAD (Head(x981, x982)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x981, 
          cvtINITS x982]))
   and cvtBINDINGS (ls987, ls992) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x986 => 
                                                                                     cvtBINDING x986
                                                                              ) ls987), 
          PrettyRep.List (List.map (fn x991 => cvtINIT_STEP x991
                                   ) ls992)]
   and cvtRIB ls1000 = PrettyRep.List (List.map (fn (x997, x998) => PrettyRep.Tuple [cvtFIXTURE_NAME x997, 
                                                       cvtFIXTURE x998]
                                                ) ls1000)
   and cvtRIBS ls1011 = PrettyRep.List (List.map (fn ls1007 => PrettyRep.List (List.map (fn (x1004, 
                                                                                               x1005) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1004, 
                                                                                               cvtFIXTURE x1005]
                                                                                        ) ls1007)
                                                 ) ls1011)
   and cvtINITS ls1018 = PrettyRep.List (List.map (fn (x1015, x1016) => PrettyRep.Tuple [cvtFIXTURE_NAME x1015, 
                                                         cvtEXPRESSION x1016]
                                                  ) ls1018)
   and cvtINSTANCE_TYPE {name=x1022, typeArgs=ls1024, nonnullable=b1028, typeParams=ls1030, 
          superTypes=ls1035, ty=x1039, dynamic=b1040} = PrettyRep.Rec [("name", 
          cvtNAME x1022), ("typeArgs", PrettyRep.List (List.map (fn x1023 => 
                                                                       cvtTYPE x1023
                                                                ) ls1024)), 
          ("nonnullable", PrettyRep.Bool b1028), ("typeParams", PrettyRep.List (List.map (fn x1029 => 
                                                                                                cvtIDENTIFIER x1029
                                                                                         ) ls1030)), 
          ("superTypes", PrettyRep.List (List.map (fn x1034 => cvtTYPE x1034
                                                  ) ls1035)), ("ty", cvtTYPE x1039), 
          ("dynamic", PrettyRep.Bool b1040)]
   and cvtFIELD {kind=x1056, name=x1057, init=x1058} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1056), ("name", cvtNAME_EXPRESSION x1057), ("init", 
          cvtEXPRESSION x1058)]
   and cvtFIELD_TYPE (x1066, x1067) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1066, 
          cvtTYPE x1067]
   and cvtFUNCTION_TYPE {typeParams=ls1070, thisType=x1074, params=ls1076, 
          minArgs=n1080, hasRest=b1081, result=opt1083} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1069 => cvtIDENTIFIER x1069
                                   ) ls1070)), ("thisType", cvtTYPE x1074), 
          ("params", PrettyRep.List (List.map (fn x1075 => cvtTYPE x1075
                                              ) ls1076)), ("minArgs", PrettyRep.Int n1080), 
          ("hasRest", PrettyRep.Bool b1081), ("result", 
       (case opt1083 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1082 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1082))
       ))]
   and cvtFUNC_DEFN {kind=x1100, ns=opt1102, final=b1106, override=b1107, prototype=b1108, 
          static=b1109, func=x1110} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1100), 
          ("ns", 
       (case opt1102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1101 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1101))
       )), ("final", PrettyRep.Bool b1106), ("override", PrettyRep.Bool b1107), 
          ("prototype", PrettyRep.Bool b1108), ("static", PrettyRep.Bool b1109), 
          ("func", cvtFUNC x1110)]
   and cvtCTOR_DEFN x1126 = cvtCTOR x1126
   and cvtVAR_DEFN {kind=x1127, ns=opt1129, static=b1133, prototype=b1134, 
          bindings=(ls1136, ls1141)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1127), 
          ("ns", 
       (case opt1129 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1128 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1128))
       )), ("static", PrettyRep.Bool b1133), ("prototype", PrettyRep.Bool b1134), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1135 => 
                                                                        cvtBINDING x1135
                                                                 ) ls1136), 
          PrettyRep.List (List.map (fn x1140 => cvtINIT_STEP x1140
                                   ) ls1141)])]
   and cvtNAMESPACE_DEFN {ident=x1157, ns=opt1159, init=opt1164} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1157), ("ns", 
       (case opt1159 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1158 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1158))
       )), ("init", 
       (case opt1164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1163 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1163))
       ))]
   and cvtCLASS_DEFN {ns=opt1176, privateNS=x1180, protectedNS=x1181, ident=x1182, 
          nonnullable=b1183, dynamic=b1184, final=b1185, params=ls1187, extends=opt1192, 
          implements=ls1197, classDefns=ls1202, instanceDefns=ls1207, instanceStmts=ls1212, 
          ctorDefn=opt1217} = PrettyRep.Rec [("ns", 
       (case opt1176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1175 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1175))
       )), ("privateNS", cvtNAMESPACE x1180), ("protectedNS", cvtNAMESPACE x1181), 
          ("ident", cvtIDENTIFIER x1182), ("nonnullable", PrettyRep.Bool b1183), 
          ("dynamic", PrettyRep.Bool b1184), ("final", PrettyRep.Bool b1185), 
          ("params", PrettyRep.List (List.map (fn x1186 => cvtIDENTIFIER x1186
                                              ) ls1187)), ("extends", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1191))
       )), ("implements", PrettyRep.List (List.map (fn x1196 => cvtTYPE x1196
                                                   ) ls1197)), ("classDefns", 
          PrettyRep.List (List.map (fn x1201 => cvtDEFN x1201
                                   ) ls1202)), ("instanceDefns", PrettyRep.List (List.map (fn x1206 => 
                                                                                                 cvtDEFN x1206
                                                                                          ) ls1207)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1211 => cvtSTATEMENT x1211
                                                     ) ls1212)), ("ctorDefn", 
          
       (case opt1217 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1216 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1216))
       ))]
   and cvtINTERFACE_DEFN {ident=x1250, ns=opt1252, nonnullable=b1256, params=ls1258, 
          extends=ls1263, instanceDefns=ls1268} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1250), ("ns", 
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1251))
       )), ("nonnullable", PrettyRep.Bool b1256), ("params", PrettyRep.List (List.map (fn x1257 => 
                                                                                             cvtIDENTIFIER x1257
                                                                                      ) ls1258)), 
          ("extends", PrettyRep.List (List.map (fn x1262 => cvtTYPE x1262
                                               ) ls1263)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1267 => cvtDEFN x1267
                                   ) ls1268))]
   and cvtTYPE_DEFN {ident=x1285, ns=opt1287, typeParams=ls1292, init=x1296} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1285), ("ns", 
       (case opt1287 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1286 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1286))
       )), ("typeParams", PrettyRep.List (List.map (fn x1291 => cvtIDENTIFIER x1291
                                                   ) ls1292)), ("init", cvtTYPE x1296)]
   and cvtCLASS_BLOCK {ns=opt1307, protectedNS=x1311, privateNS=x1312, ident=x1313, 
          name=opt1315, block=x1319} = PrettyRep.Rec [("ns", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1306))
       )), ("protectedNS", cvtNAMESPACE x1311), ("privateNS", cvtNAMESPACE x1312), 
          ("ident", cvtIDENTIFIER x1313), ("name", 
       (case opt1315 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1314 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1314))
       )), ("block", cvtBLOCK x1319)]
   and cvtFOR_ENUM_HEAD {isEach=b1333, bindings=(ls1335, ls1340), expr=x1345} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1333), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1334 => 
                                                                                                                         cvtBINDING x1334
                                                                                                                  ) ls1335), 
          PrettyRep.List (List.map (fn x1339 => cvtINIT_STEP x1339
                                   ) ls1340)]), ("expr", cvtEXPRESSION x1345)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1353, defn=opt1384, obj=x1388, rib=opt1396, 
          next=x1400, labels=ls1402, body=x1406} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1353), ("defn", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1354, ns=opt1356, static=b1360, prototype=b1361, bindings=(ls1363, 
            ls1368)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1354), ("ns", 
         (case opt1356 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1355))
         )), ("static", PrettyRep.Bool b1360), ("prototype", PrettyRep.Bool b1361), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1362 => 
                                                                          cvtBINDING x1362
                                                                   ) ls1363), 
            PrettyRep.List (List.map (fn x1367 => cvtINIT_STEP x1367
                                     ) ls1368)])]))
       )), ("obj", cvtEXPRESSION x1388), ("rib", 
       (case opt1396 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1392 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1389, 
                                                                                      x1390) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1389, 
                                                                                      cvtFIXTURE x1390]
                                                                               ) ls1392)))
       )), ("next", cvtSTATEMENT x1400), ("labels", PrettyRep.List (List.map (fn x1401 => 
                                                                                    cvtIDENTIFIER x1401
                                                                             ) ls1402)), 
          ("body", cvtSTATEMENT x1406)]
   and cvtFOR_STATEMENT {rib=opt1429, defn=opt1463, init=ls1468, cond=x1472, 
          update=x1473, labels=ls1475, body=x1479} = PrettyRep.Rec [("rib", 
          
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1425 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1422, 
                                                                                      x1423) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1422, 
                                                                                      cvtFIXTURE x1423]
                                                                               ) ls1425)))
       )), ("defn", 
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1433, ns=opt1435, static=b1439, prototype=b1440, bindings=(ls1442, 
            ls1447)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1433), ("ns", 
         (case opt1435 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1434))
         )), ("static", PrettyRep.Bool b1439), ("prototype", PrettyRep.Bool b1440), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1441 => 
                                                                          cvtBINDING x1441
                                                                   ) ls1442), 
            PrettyRep.List (List.map (fn x1446 => cvtINIT_STEP x1446
                                     ) ls1447)])]))
       )), ("init", PrettyRep.List (List.map (fn x1467 => cvtSTATEMENT x1467
                                             ) ls1468)), ("cond", cvtEXPRESSION x1472), 
          ("update", cvtEXPRESSION x1473), ("labels", PrettyRep.List (List.map (fn x1474 => 
                                                                                      cvtIDENTIFIER x1474
                                                                               ) ls1475)), 
          ("body", cvtSTATEMENT x1479)]
   and cvtWHILE_STATEMENT {cond=x1495, rib=opt1503, body=x1507, labels=ls1509} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1495), ("rib", 
       (case opt1503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1499 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1496, 
                                                                                      x1497) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1496, 
                                                                                      cvtFIXTURE x1497]
                                                                               ) ls1499)))
       )), ("body", cvtSTATEMENT x1507), ("labels", PrettyRep.List (List.map (fn x1508 => 
                                                                                    cvtIDENTIFIER x1508
                                                                             ) ls1509))]
   and cvtDIRECTIVES {pragmas=ls1523, defns=ls1528, head=opt1533, body=ls1538, 
          loc=opt1543} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1522 => 
                                                                                    cvtPRAGMA x1522
                                                                             ) ls1523)), 
          ("defns", PrettyRep.List (List.map (fn x1527 => cvtDEFN x1527
                                             ) ls1528)), ("head", 
       (case opt1533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1532 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1532))
       )), ("body", PrettyRep.List (List.map (fn x1537 => cvtSTATEMENT x1537
                                             ) ls1538)), ("loc", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1542))
       ))]
   and cvtCASE {label=opt1559, inits=opt1570, body=x1574} = PrettyRep.Rec [("label", 
          
       (case opt1559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1558 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1558))
       )), ("inits", 
       (case opt1570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1566 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1563, 
                                                                                      x1564) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1563, 
                                                                                      cvtEXPRESSION x1564]
                                                                               ) ls1566)))
       )), ("body", cvtBLOCK x1574)]
   and cvtCATCH_CLAUSE {bindings=(ls1583, ls1588), ty=x1593, rib=opt1601, inits=opt1612, 
          block=x1616} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1582 => 
                                                                                                      cvtBINDING x1582
                                                                                               ) ls1583), 
          PrettyRep.List (List.map (fn x1587 => cvtINIT_STEP x1587
                                   ) ls1588)]), ("ty", cvtTYPE x1593), ("rib", 
          
       (case opt1601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1597 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1594, 
                                                                                      x1595) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1594, 
                                                                                      cvtFIXTURE x1595]
                                                                               ) ls1597)))
       )), ("inits", 
       (case opt1612 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1608 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1605, 
                                                                                      x1606) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1605, 
                                                                                      cvtEXPRESSION x1606]
                                                                               ) ls1608)))
       )), ("block", cvtBLOCK x1616)]
   and cvtFUNC_NAME {kind=x1628, ident=x1629} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1628), 
          ("ident", cvtIDENTIFIER x1629)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1635, getter=opt1637, setter=opt1642} = 
          PrettyRep.Rec [("ty", cvtTYPE x1635), ("getter", 
       (case opt1637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1636 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1636))
       )), ("setter", 
       (case opt1642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1641 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1641))
       ))]
   and cvtFRAGMENT (Anon x1653) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1653))
end

