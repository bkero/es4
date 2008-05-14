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
     | cvtTYPE (ClassType x472) = PrettyRep.Ctor ("ClassType", SOME (cvtCLS x472))
     | cvtTYPE (InterfaceType x475) = PrettyRep.Ctor ("InterfaceType", SOME (cvtIFACE x475))
     | cvtTYPE (TypeNameReferenceType(x478, x479)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x478, cvtNAME_EXPRESSION x479]))
     | cvtTYPE (TypeIndexReferenceType(x483, n484)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x483, PrettyRep.Int n484]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x489) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x489))
     | cvtSTATEMENT (InitStmt{kind=x492, ns=opt494, prototype=b498, static=b499, 
          temps=x500, inits=ls502}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x492), ("ns", 
       (case opt494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x493 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x493))
       )), ("prototype", PrettyRep.Bool b498), ("static", PrettyRep.Bool b499), 
          ("temps", cvtBINDINGS x500), ("inits", PrettyRep.List (List.map (fn x501 => 
                                                                                 cvtINIT_STEP x501
                                                                          ) ls502))]))
     | cvtSTATEMENT (ClassBlock x521) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x521))
     | cvtSTATEMENT (ForInStmt x524) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x524))
     | cvtSTATEMENT (ThrowStmt x527) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x527))
     | cvtSTATEMENT (ReturnStmt x530) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x530))
     | cvtSTATEMENT (BreakStmt opt534) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x533 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x533))
       ))
     | cvtSTATEMENT (ContinueStmt opt541) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x540 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x540))
       ))
     | cvtSTATEMENT (BlockStmt x547) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x547))
     | cvtSTATEMENT (LabeledStmt(x550, x551)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x550, cvtSTATEMENT x551]))
     | cvtSTATEMENT (LetStmt x555) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x555))
     | cvtSTATEMENT (WhileStmt x558) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x558))
     | cvtSTATEMENT (DoWhileStmt x561) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x561))
     | cvtSTATEMENT (ForStmt x564) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x564))
     | cvtSTATEMENT (IfStmt{cnd=x567, thn=x568, els=x569}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x567), ("thn", cvtSTATEMENT x568), 
          ("els", cvtSTATEMENT x569)]))
     | cvtSTATEMENT (WithStmt{obj=x579, ty=x580, body=x581}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x579), ("ty", cvtTYPE x580), 
          ("body", cvtSTATEMENT x581)]))
     | cvtSTATEMENT (TryStmt{block=x591, catches=ls593, finally=opt598}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x591), 
          ("catches", PrettyRep.List (List.map (fn x592 => cvtCATCH_CLAUSE x592
                                               ) ls593)), ("finally", 
       (case opt598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x597 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x597))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x611, labels=ls613, cases=ls618}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x611), ("labels", PrettyRep.List (List.map (fn x612 => 
                                                                                                        cvtIDENTIFIER x612
                                                                                                 ) ls613)), 
          ("cases", PrettyRep.List (List.map (fn x617 => cvtCASE x617
                                             ) ls618))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x631, ty=x632, cases=ls634}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x631), ("ty", cvtTYPE x632), 
          ("cases", PrettyRep.List (List.map (fn x633 => cvtCATCH_CLAUSE x633
                                             ) ls634))]))
     | cvtSTATEMENT (DXNStmt{expr=x647}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x647)]))
   and cvtEXPRESSION (TernaryExpr(x653, x654, x655)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x653, cvtEXPRESSION x654, cvtEXPRESSION x655]))
     | cvtEXPRESSION (BinaryExpr(x659, x660, x661)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x659, cvtEXPRESSION x660, cvtEXPRESSION x661]))
     | cvtEXPRESSION (BinaryTypeExpr(x665, x666, x667)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x665, cvtEXPRESSION x666, cvtTYPE x667]))
     | cvtEXPRESSION (UnaryExpr(x671, x672)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x671, cvtEXPRESSION x672]))
     | cvtEXPRESSION (TypeExpr x676) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x676))
     | cvtEXPRESSION (ThisExpr opt680) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt680 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x679 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x679))
       ))
     | cvtEXPRESSION (YieldExpr opt687) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt687 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x686 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x686))
       ))
     | cvtEXPRESSION (SuperExpr opt694) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x693 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x693))
       ))
     | cvtEXPRESSION (CallExpr{func=x700, actuals=ls702}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x700), ("actuals", PrettyRep.List (List.map (fn x701 => 
                                                                                                         cvtEXPRESSION x701
                                                                                                  ) ls702))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x713, actuals=ls715}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x713), ("actuals", PrettyRep.List (List.map (fn x714 => 
                                                                                                         cvtTYPE x714
                                                                                                  ) ls715))]))
     | cvtEXPRESSION (LetExpr{defs=x726, body=x727, head=opt729}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x726), ("body", cvtEXPRESSION x727), 
          ("head", 
       (case opt729 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x728 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x728))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x742, actuals=ls744}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x742), ("actuals", PrettyRep.List (List.map (fn x743 => 
                                                                                                        cvtEXPRESSION x743
                                                                                                 ) ls744))]))
     | cvtEXPRESSION (SetExpr(x755, x756, x757)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x755, cvtEXPRESSION x756, cvtEXPRESSION x757]))
     | cvtEXPRESSION (ListExpr ls762) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x761 => 
                                                                                                          cvtEXPRESSION x761
                                                                                                   ) ls762)))
     | cvtEXPRESSION (InitExpr(x768, x769, x770)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x768, cvtHEAD x769, cvtINITS x770]))
     | cvtEXPRESSION (GetTemp n774) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n774))
     | cvtEXPRESSION (GetParam n777) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n777))
     | cvtEXPRESSION (Comprehension(x780, ls782, opt787)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x780, PrettyRep.List (List.map (fn x781 => 
                                                                                     cvtFOR_ENUM_HEAD x781
                                                                              ) ls782), 
          
       (case opt787 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x786 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x786))
       )]))
     | cvtEXPRESSION (LiteralExpr x794) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x794))
     | cvtEXPRESSION (LexicalReference{name=x797, loc=opt799}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x797), ("loc", 
       (case opt799 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x798 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x798))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x810, name=x811, loc=opt813}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x810), ("name", cvtNAME_EXPRESSION x811), ("loc", 
       (case opt813 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x812 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x812))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x826, index=x827, loc=opt829}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x826), ("index", cvtEXPRESSION x827), ("loc", 
       (case opt829 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x828 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x828))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n847) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n847))
     | cvtFIXTURE_NAME (PropName x850) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x850))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r855) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r855))
     | cvtLITERAL (LiteralDecimal d858) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d858))
     | cvtLITERAL (LiteralBoolean b861) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b861))
     | cvtLITERAL (LiteralString s864) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s864))
     | cvtLITERAL (LiteralArray{exprs=x867, ty=opt869}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x867), ("ty", 
       (case opt869 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x868 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x868))
       ))]))
     | cvtLITERAL (LiteralXML ls881) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x880 => 
                                                                                                           cvtEXPRESSION x880
                                                                                                    ) ls881)))
     | cvtLITERAL (LiteralNamespace x887) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x887))
     | cvtLITERAL (LiteralObject{expr=ls891, ty=opt896}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x890 => 
                                                                        cvtFIELD x890
                                                                 ) ls891)), 
          ("ty", 
       (case opt896 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x895 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x895))
       ))]))
     | cvtLITERAL (LiteralFunction x907) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x907))
     | cvtLITERAL (LiteralRegExp{str=s910}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s910)]))
   and cvtBLOCK (Block x916) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x916))
   and cvtFIXTURE (NamespaceFixture x919) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x919))
     | cvtFIXTURE (ClassFixture x922) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x922))
     | cvtFIXTURE (InterfaceFixture x925) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x925))
     | cvtFIXTURE (TypeVarFixture x928) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x928))
     | cvtFIXTURE (TypeFixture(ls932, x936)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x931 => cvtIDENTIFIER x931
                                                          ) ls932), cvtTYPE x936]))
     | cvtFIXTURE (MethodFixture{func=x940, ty=x941, writable=b942, override=b943, 
          final=b944}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x940), ("ty", cvtTYPE x941), ("writable", PrettyRep.Bool b942), 
          ("override", PrettyRep.Bool b943), ("final", PrettyRep.Bool b944)]))
     | cvtFIXTURE (ValFixture{ty=x958, writable=b959}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x958), ("writable", PrettyRep.Bool b959)]))
     | cvtFIXTURE (VirtualValFixture{ty=x967, getter=opt969, setter=opt974}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x967), ("getter", 
       (case opt969 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x968 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x968))
       )), ("setter", 
       (case opt974 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x973 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x973))
       ))]))
   and cvtHEAD (Head(x987, x988)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x987, 
          cvtINITS x988]))
   and cvtBINDINGS (ls993, ls998) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x992 => 
                                                                                     cvtBINDING x992
                                                                              ) ls993), 
          PrettyRep.List (List.map (fn x997 => cvtINIT_STEP x997
                                   ) ls998)]
   and cvtRIB ls1006 = PrettyRep.List (List.map (fn (x1003, x1004) => PrettyRep.Tuple [cvtFIXTURE_NAME x1003, 
                                                       cvtFIXTURE x1004]
                                                ) ls1006)
   and cvtRIBS ls1017 = PrettyRep.List (List.map (fn ls1013 => PrettyRep.List (List.map (fn (x1010, 
                                                                                               x1011) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1010, 
                                                                                               cvtFIXTURE x1011]
                                                                                        ) ls1013)
                                                 ) ls1017)
   and cvtINITS ls1024 = PrettyRep.List (List.map (fn (x1021, x1022) => PrettyRep.Tuple [cvtFIXTURE_NAME x1021, 
                                                         cvtEXPRESSION x1022]
                                                  ) ls1024)
   and cvtINSTANCE_TYPE {name=x1028, typeArgs=ls1030, nonnullable=b1034, typeParams=ls1036, 
          superTypes=ls1041, ty=x1045, dynamic=b1046} = PrettyRep.Rec [("name", 
          cvtNAME x1028), ("typeArgs", PrettyRep.List (List.map (fn x1029 => 
                                                                       cvtTYPE x1029
                                                                ) ls1030)), 
          ("nonnullable", PrettyRep.Bool b1034), ("typeParams", PrettyRep.List (List.map (fn x1035 => 
                                                                                                cvtIDENTIFIER x1035
                                                                                         ) ls1036)), 
          ("superTypes", PrettyRep.List (List.map (fn x1040 => cvtTYPE x1040
                                                  ) ls1041)), ("ty", cvtTYPE x1045), 
          ("dynamic", PrettyRep.Bool b1046)]
   and cvtFIELD {kind=x1062, name=x1063, init=x1064} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1062), ("name", cvtNAME_EXPRESSION x1063), ("init", 
          cvtEXPRESSION x1064)]
   and cvtFIELD_TYPE (x1072, x1073) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1072, 
          cvtTYPE x1073]
   and cvtFUNCTION_TYPE {typeParams=ls1076, thisType=x1080, params=ls1082, 
          minArgs=n1086, hasRest=b1087, result=opt1089} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1075 => cvtIDENTIFIER x1075
                                   ) ls1076)), ("thisType", cvtTYPE x1080), 
          ("params", PrettyRep.List (List.map (fn x1081 => cvtTYPE x1081
                                              ) ls1082)), ("minArgs", PrettyRep.Int n1086), 
          ("hasRest", PrettyRep.Bool b1087), ("result", 
       (case opt1089 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1088 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1088))
       ))]
   and cvtFUNC_DEFN {kind=x1106, ns=opt1108, final=b1112, override=b1113, prototype=b1114, 
          static=b1115, func=x1116} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1106), 
          ("ns", 
       (case opt1108 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1107 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1107))
       )), ("final", PrettyRep.Bool b1112), ("override", PrettyRep.Bool b1113), 
          ("prototype", PrettyRep.Bool b1114), ("static", PrettyRep.Bool b1115), 
          ("func", cvtFUNC x1116)]
   and cvtCTOR_DEFN x1132 = cvtCTOR x1132
   and cvtVAR_DEFN {kind=x1133, ns=opt1135, static=b1139, prototype=b1140, 
          bindings=(ls1142, ls1147)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1133), 
          ("ns", 
       (case opt1135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1134 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1134))
       )), ("static", PrettyRep.Bool b1139), ("prototype", PrettyRep.Bool b1140), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1141 => 
                                                                        cvtBINDING x1141
                                                                 ) ls1142), 
          PrettyRep.List (List.map (fn x1146 => cvtINIT_STEP x1146
                                   ) ls1147)])]
   and cvtNAMESPACE_DEFN {ident=x1163, ns=opt1165, init=opt1170} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1163), ("ns", 
       (case opt1165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1164 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1164))
       )), ("init", 
       (case opt1170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1169 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1169))
       ))]
   and cvtCLASS_DEFN {ns=opt1182, privateNS=x1186, protectedNS=x1187, ident=x1188, 
          nonnullable=b1189, dynamic=b1190, final=b1191, params=ls1193, extends=opt1198, 
          implements=ls1203, classDefns=ls1208, instanceDefns=ls1213, instanceStmts=ls1218, 
          ctorDefn=opt1223} = PrettyRep.Rec [("ns", 
       (case opt1182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1181 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1181))
       )), ("privateNS", cvtNAMESPACE x1186), ("protectedNS", cvtNAMESPACE x1187), 
          ("ident", cvtIDENTIFIER x1188), ("nonnullable", PrettyRep.Bool b1189), 
          ("dynamic", PrettyRep.Bool b1190), ("final", PrettyRep.Bool b1191), 
          ("params", PrettyRep.List (List.map (fn x1192 => cvtIDENTIFIER x1192
                                              ) ls1193)), ("extends", 
       (case opt1198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1197 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1197))
       )), ("implements", PrettyRep.List (List.map (fn x1202 => cvtTYPE x1202
                                                   ) ls1203)), ("classDefns", 
          PrettyRep.List (List.map (fn x1207 => cvtDEFN x1207
                                   ) ls1208)), ("instanceDefns", PrettyRep.List (List.map (fn x1212 => 
                                                                                                 cvtDEFN x1212
                                                                                          ) ls1213)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1217 => cvtSTATEMENT x1217
                                                     ) ls1218)), ("ctorDefn", 
          
       (case opt1223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1222 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1222))
       ))]
   and cvtINTERFACE_DEFN {ident=x1256, ns=opt1258, nonnullable=b1262, params=ls1264, 
          extends=ls1269, instanceDefns=ls1274} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1256), ("ns", 
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1257))
       )), ("nonnullable", PrettyRep.Bool b1262), ("params", PrettyRep.List (List.map (fn x1263 => 
                                                                                             cvtIDENTIFIER x1263
                                                                                      ) ls1264)), 
          ("extends", PrettyRep.List (List.map (fn x1268 => cvtTYPE x1268
                                               ) ls1269)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1273 => cvtDEFN x1273
                                   ) ls1274))]
   and cvtTYPE_DEFN {ident=x1291, ns=opt1293, typeParams=ls1298, init=x1302} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1291), ("ns", 
       (case opt1293 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1292 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1292))
       )), ("typeParams", PrettyRep.List (List.map (fn x1297 => cvtIDENTIFIER x1297
                                                   ) ls1298)), ("init", cvtTYPE x1302)]
   and cvtCLASS_BLOCK {ns=opt1313, protectedNS=x1317, privateNS=x1318, ident=x1319, 
          name=opt1321, block=x1325} = PrettyRep.Rec [("ns", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1312))
       )), ("protectedNS", cvtNAMESPACE x1317), ("privateNS", cvtNAMESPACE x1318), 
          ("ident", cvtIDENTIFIER x1319), ("name", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1320))
       )), ("block", cvtBLOCK x1325)]
   and cvtFOR_ENUM_HEAD {isEach=b1339, bindings=(ls1341, ls1346), expr=x1351} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1339), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1340 => 
                                                                                                                         cvtBINDING x1340
                                                                                                                  ) ls1341), 
          PrettyRep.List (List.map (fn x1345 => cvtINIT_STEP x1345
                                   ) ls1346)]), ("expr", cvtEXPRESSION x1351)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1359, defn=opt1390, obj=x1394, rib=opt1402, 
          next=x1406, labels=ls1408, body=x1412} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1359), ("defn", 
       (case opt1390 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1360, ns=opt1362, static=b1366, prototype=b1367, bindings=(ls1369, 
            ls1374)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1360), ("ns", 
         (case opt1362 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1361))
         )), ("static", PrettyRep.Bool b1366), ("prototype", PrettyRep.Bool b1367), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1368 => 
                                                                          cvtBINDING x1368
                                                                   ) ls1369), 
            PrettyRep.List (List.map (fn x1373 => cvtINIT_STEP x1373
                                     ) ls1374)])]))
       )), ("obj", cvtEXPRESSION x1394), ("rib", 
       (case opt1402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1398 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1395, 
                                                                                      x1396) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1395, 
                                                                                      cvtFIXTURE x1396]
                                                                               ) ls1398)))
       )), ("next", cvtSTATEMENT x1406), ("labels", PrettyRep.List (List.map (fn x1407 => 
                                                                                    cvtIDENTIFIER x1407
                                                                             ) ls1408)), 
          ("body", cvtSTATEMENT x1412)]
   and cvtFOR_STATEMENT {rib=opt1435, defn=opt1469, init=ls1474, cond=x1478, 
          update=x1479, labels=ls1481, body=x1485} = PrettyRep.Rec [("rib", 
          
       (case opt1435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1431 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1428, 
                                                                                      x1429) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1428, 
                                                                                      cvtFIXTURE x1429]
                                                                               ) ls1431)))
       )), ("defn", 
       (case opt1469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1439, ns=opt1441, static=b1445, prototype=b1446, bindings=(ls1448, 
            ls1453)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1439), ("ns", 
         (case opt1441 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1440))
         )), ("static", PrettyRep.Bool b1445), ("prototype", PrettyRep.Bool b1446), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1447 => 
                                                                          cvtBINDING x1447
                                                                   ) ls1448), 
            PrettyRep.List (List.map (fn x1452 => cvtINIT_STEP x1452
                                     ) ls1453)])]))
       )), ("init", PrettyRep.List (List.map (fn x1473 => cvtSTATEMENT x1473
                                             ) ls1474)), ("cond", cvtEXPRESSION x1478), 
          ("update", cvtEXPRESSION x1479), ("labels", PrettyRep.List (List.map (fn x1480 => 
                                                                                      cvtIDENTIFIER x1480
                                                                               ) ls1481)), 
          ("body", cvtSTATEMENT x1485)]
   and cvtWHILE_STATEMENT {cond=x1501, rib=opt1509, body=x1513, labels=ls1515} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1501), ("rib", 
       (case opt1509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1505 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1502, 
                                                                                      x1503) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1502, 
                                                                                      cvtFIXTURE x1503]
                                                                               ) ls1505)))
       )), ("body", cvtSTATEMENT x1513), ("labels", PrettyRep.List (List.map (fn x1514 => 
                                                                                    cvtIDENTIFIER x1514
                                                                             ) ls1515))]
   and cvtDIRECTIVES {pragmas=ls1529, defns=ls1534, head=opt1539, body=ls1544, 
          loc=opt1549} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1528 => 
                                                                                    cvtPRAGMA x1528
                                                                             ) ls1529)), 
          ("defns", PrettyRep.List (List.map (fn x1533 => cvtDEFN x1533
                                             ) ls1534)), ("head", 
       (case opt1539 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1538 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1538))
       )), ("body", PrettyRep.List (List.map (fn x1543 => cvtSTATEMENT x1543
                                             ) ls1544)), ("loc", 
       (case opt1549 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1548 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1548))
       ))]
   and cvtCASE {label=opt1565, inits=opt1576, body=x1580} = PrettyRep.Rec [("label", 
          
       (case opt1565 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1564 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1564))
       )), ("inits", 
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1572 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1569, 
                                                                                      x1570) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1569, 
                                                                                      cvtEXPRESSION x1570]
                                                                               ) ls1572)))
       )), ("body", cvtBLOCK x1580)]
   and cvtCATCH_CLAUSE {bindings=(ls1589, ls1594), ty=x1599, rib=opt1607, inits=opt1618, 
          block=x1622} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1588 => 
                                                                                                      cvtBINDING x1588
                                                                                               ) ls1589), 
          PrettyRep.List (List.map (fn x1593 => cvtINIT_STEP x1593
                                   ) ls1594)]), ("ty", cvtTYPE x1599), ("rib", 
          
       (case opt1607 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1603 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1600, 
                                                                                      x1601) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1600, 
                                                                                      cvtFIXTURE x1601]
                                                                               ) ls1603)))
       )), ("inits", 
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1614 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1611, 
                                                                                      x1612) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1611, 
                                                                                      cvtEXPRESSION x1612]
                                                                               ) ls1614)))
       )), ("block", cvtBLOCK x1622)]
   and cvtFUNC_NAME {kind=x1634, ident=x1635} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1634), 
          ("ident", cvtIDENTIFIER x1635)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1641, getter=opt1643, setter=opt1648} = 
          PrettyRep.Rec [("ty", cvtTYPE x1641), ("getter", 
       (case opt1643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1642 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1642))
       )), ("setter", 
       (case opt1648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1647 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1647))
       ))]
   and cvtFRAGMENT (Anon x1659) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1659))
end

