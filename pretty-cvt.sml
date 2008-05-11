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
     | cvtTYPE (RecordType ls417) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn x416 => 
                                                                                                        cvtFIELD_TYPE x416
                                                                                                 ) ls417)))
     | cvtTYPE (ArrayType(ls424, opt429)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x423 => 
                                                                                                                                cvtTYPE x423
                                                                                                                         ) ls424), 
          
       (case opt429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x428 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x428))
       )]))
     | cvtTYPE (UnionType ls437) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x436 => 
                                                                                                      cvtTYPE x436
                                                                                               ) ls437)))
     | cvtTYPE (FunctionType x443) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x443))
     | cvtTYPE (NonNullType x446) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x446))
     | cvtTYPE (AppType(x449, ls451)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x449, 
          PrettyRep.List (List.map (fn x450 => cvtTYPE x450
                                   ) ls451)]))
     | cvtTYPE (TypeName(x458, opt460)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x458, 
          
       (case opt460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x459 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x459))
       )]))
     | cvtTYPE (InstanceType x467) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x467))
     | cvtTYPE (TypeNameReferenceType(x470, x471)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x470, cvtNAME_EXPRESSION x471]))
     | cvtTYPE (TypeIndexReferenceType(x475, n476)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x475, PrettyRep.Int n476]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x481) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x481))
     | cvtSTATEMENT (InitStmt{kind=x484, ns=opt486, prototype=b490, static=b491, 
          temps=x492, inits=ls494}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x484), ("ns", 
       (case opt486 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x485 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x485))
       )), ("prototype", PrettyRep.Bool b490), ("static", PrettyRep.Bool b491), 
          ("temps", cvtBINDINGS x492), ("inits", PrettyRep.List (List.map (fn x493 => 
                                                                                 cvtINIT_STEP x493
                                                                          ) ls494))]))
     | cvtSTATEMENT (ClassBlock x513) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x513))
     | cvtSTATEMENT (ForInStmt x516) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x516))
     | cvtSTATEMENT (ThrowStmt x519) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x519))
     | cvtSTATEMENT (ReturnStmt x522) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x522))
     | cvtSTATEMENT (BreakStmt opt526) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt526 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x525 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x525))
       ))
     | cvtSTATEMENT (ContinueStmt opt533) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x532 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x532))
       ))
     | cvtSTATEMENT (BlockStmt x539) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x539))
     | cvtSTATEMENT (LabeledStmt(x542, x543)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x542, cvtSTATEMENT x543]))
     | cvtSTATEMENT (LetStmt x547) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x547))
     | cvtSTATEMENT (WhileStmt x550) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x550))
     | cvtSTATEMENT (DoWhileStmt x553) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x553))
     | cvtSTATEMENT (ForStmt x556) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x556))
     | cvtSTATEMENT (IfStmt{cnd=x559, thn=x560, els=x561}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x559), ("thn", cvtSTATEMENT x560), 
          ("els", cvtSTATEMENT x561)]))
     | cvtSTATEMENT (WithStmt{obj=x571, ty=x572, body=x573}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x571), ("ty", cvtTYPE x572), 
          ("body", cvtSTATEMENT x573)]))
     | cvtSTATEMENT (TryStmt{block=x583, catches=ls585, finally=opt590}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x583), 
          ("catches", PrettyRep.List (List.map (fn x584 => cvtCATCH_CLAUSE x584
                                               ) ls585)), ("finally", 
       (case opt590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x589 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x589))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x603, labels=ls605, cases=ls610}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x603), ("labels", PrettyRep.List (List.map (fn x604 => 
                                                                                                        cvtIDENTIFIER x604
                                                                                                 ) ls605)), 
          ("cases", PrettyRep.List (List.map (fn x609 => cvtCASE x609
                                             ) ls610))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x623, ty=x624, cases=ls626}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x623), ("ty", cvtTYPE x624), 
          ("cases", PrettyRep.List (List.map (fn x625 => cvtCATCH_CLAUSE x625
                                             ) ls626))]))
     | cvtSTATEMENT (DXNStmt{expr=x639}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x639)]))
   and cvtEXPRESSION (TernaryExpr(x645, x646, x647)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x645, cvtEXPRESSION x646, cvtEXPRESSION x647]))
     | cvtEXPRESSION (BinaryExpr(x651, x652, x653)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x651, cvtEXPRESSION x652, cvtEXPRESSION x653]))
     | cvtEXPRESSION (BinaryTypeExpr(x657, x658, x659)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x657, cvtEXPRESSION x658, cvtTYPE x659]))
     | cvtEXPRESSION (UnaryExpr(x663, x664)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x663, cvtEXPRESSION x664]))
     | cvtEXPRESSION (TypeExpr x668) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x668))
     | cvtEXPRESSION (ThisExpr opt672) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x671))
       ))
     | cvtEXPRESSION (YieldExpr opt679) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x678 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x678))
       ))
     | cvtEXPRESSION (SuperExpr opt686) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt686 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x685 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x685))
       ))
     | cvtEXPRESSION (CallExpr{func=x692, actuals=ls694}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x692), ("actuals", PrettyRep.List (List.map (fn x693 => 
                                                                                                         cvtEXPRESSION x693
                                                                                                  ) ls694))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x705, actuals=ls707}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x705), ("actuals", PrettyRep.List (List.map (fn x706 => 
                                                                                                         cvtTYPE x706
                                                                                                  ) ls707))]))
     | cvtEXPRESSION (LetExpr{defs=x718, body=x719, head=opt721}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x718), ("body", cvtEXPRESSION x719), 
          ("head", 
       (case opt721 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x720 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x720))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x734, actuals=ls736}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x734), ("actuals", PrettyRep.List (List.map (fn x735 => 
                                                                                                        cvtEXPRESSION x735
                                                                                                 ) ls736))]))
     | cvtEXPRESSION (SetExpr(x747, x748, x749)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x747, cvtEXPRESSION x748, cvtEXPRESSION x749]))
     | cvtEXPRESSION (ListExpr ls754) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x753 => 
                                                                                                          cvtEXPRESSION x753
                                                                                                   ) ls754)))
     | cvtEXPRESSION (InitExpr(x760, x761, x762)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x760, cvtHEAD x761, cvtINITS x762]))
     | cvtEXPRESSION (GetTemp n766) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n766))
     | cvtEXPRESSION (GetParam n769) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n769))
     | cvtEXPRESSION (Comprehension(x772, ls774, opt779)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x772, PrettyRep.List (List.map (fn x773 => 
                                                                                     cvtFOR_ENUM_HEAD x773
                                                                              ) ls774), 
          
       (case opt779 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x778 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x778))
       )]))
     | cvtEXPRESSION (LiteralExpr x786) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x786))
     | cvtEXPRESSION (LexicalReference{name=x789, loc=opt791}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x789), ("loc", 
       (case opt791 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x790 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x790))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x802, name=x803, loc=opt805}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x802), ("name", cvtNAME_EXPRESSION x803), ("loc", 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x804))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x818, index=x819, loc=opt821}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x818), ("index", cvtEXPRESSION x819), ("loc", 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x820))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n839) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n839))
     | cvtFIXTURE_NAME (PropName x842) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x842))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r847) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r847))
     | cvtLITERAL (LiteralDecimal d850) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d850))
     | cvtLITERAL (LiteralBoolean b853) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b853))
     | cvtLITERAL (LiteralString s856) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s856))
     | cvtLITERAL (LiteralArray{exprs=x859, ty=opt861}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x859), ("ty", 
       (case opt861 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x860 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x860))
       ))]))
     | cvtLITERAL (LiteralXML ls873) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x872 => 
                                                                                                           cvtEXPRESSION x872
                                                                                                    ) ls873)))
     | cvtLITERAL (LiteralNamespace x879) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x879))
     | cvtLITERAL (LiteralObject{expr=ls883, ty=opt888}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x882 => 
                                                                        cvtFIELD x882
                                                                 ) ls883)), 
          ("ty", 
       (case opt888 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x887 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x887))
       ))]))
     | cvtLITERAL (LiteralFunction x899) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x899))
     | cvtLITERAL (LiteralRegExp{str=s902}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s902)]))
   and cvtBLOCK (Block x908) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x908))
   and cvtFIXTURE (NamespaceFixture x911) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x911))
     | cvtFIXTURE (ClassFixture x914) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x914))
     | cvtFIXTURE (InterfaceFixture x917) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x917))
     | cvtFIXTURE (TypeVarFixture x920) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x920))
     | cvtFIXTURE (TypeFixture(ls924, x928)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x923 => cvtIDENTIFIER x923
                                                          ) ls924), cvtTYPE x928]))
     | cvtFIXTURE (MethodFixture{func=x932, ty=x933, writable=b934, override=b935, 
          final=b936}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x932), ("ty", cvtTYPE x933), ("writable", PrettyRep.Bool b934), 
          ("override", PrettyRep.Bool b935), ("final", PrettyRep.Bool b936)]))
     | cvtFIXTURE (ValFixture{ty=x950, writable=b951}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x950), ("writable", PrettyRep.Bool b951)]))
     | cvtFIXTURE (VirtualValFixture{ty=x959, getter=opt961, setter=opt966}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x959), ("getter", 
       (case opt961 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x960 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x960))
       )), ("setter", 
       (case opt966 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x965 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x965))
       ))]))
   and cvtHEAD (Head(x979, x980)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x979, 
          cvtINITS x980]))
   and cvtBINDINGS (ls985, ls990) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x984 => 
                                                                                     cvtBINDING x984
                                                                              ) ls985), 
          PrettyRep.List (List.map (fn x989 => cvtINIT_STEP x989
                                   ) ls990)]
   and cvtRIB ls998 = PrettyRep.List (List.map (fn (x995, x996) => PrettyRep.Tuple [cvtFIXTURE_NAME x995, 
                                                      cvtFIXTURE x996]
                                               ) ls998)
   and cvtRIBS ls1009 = PrettyRep.List (List.map (fn ls1005 => PrettyRep.List (List.map (fn (x1002, 
                                                                                               x1003) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1002, 
                                                                                               cvtFIXTURE x1003]
                                                                                        ) ls1005)
                                                 ) ls1009)
   and cvtINITS ls1016 = PrettyRep.List (List.map (fn (x1013, x1014) => PrettyRep.Tuple [cvtFIXTURE_NAME x1013, 
                                                         cvtEXPRESSION x1014]
                                                  ) ls1016)
   and cvtINSTANCE_TYPE {name=x1020, typeArgs=ls1022, nonnullable=b1026, typeParams=ls1028, 
          superTypes=ls1033, ty=x1037, dynamic=b1038} = PrettyRep.Rec [("name", 
          cvtNAME x1020), ("typeArgs", PrettyRep.List (List.map (fn x1021 => 
                                                                       cvtTYPE x1021
                                                                ) ls1022)), 
          ("nonnullable", PrettyRep.Bool b1026), ("typeParams", PrettyRep.List (List.map (fn x1027 => 
                                                                                                cvtIDENTIFIER x1027
                                                                                         ) ls1028)), 
          ("superTypes", PrettyRep.List (List.map (fn x1032 => cvtTYPE x1032
                                                  ) ls1033)), ("ty", cvtTYPE x1037), 
          ("dynamic", PrettyRep.Bool b1038)]
   and cvtFIELD {kind=x1054, name=x1055, init=x1056} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1054), ("name", cvtNAME_EXPRESSION x1055), ("init", 
          cvtEXPRESSION x1056)]
   and cvtFIELD_TYPE (x1064, x1065) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1064, 
          cvtTYPE x1065]
   and cvtFUNCTION_TYPE {typeParams=ls1068, thisType=x1072, params=ls1074, 
          minArgs=n1078, hasRest=b1079, result=opt1081} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1067 => cvtIDENTIFIER x1067
                                   ) ls1068)), ("thisType", cvtTYPE x1072), 
          ("params", PrettyRep.List (List.map (fn x1073 => cvtTYPE x1073
                                              ) ls1074)), ("minArgs", PrettyRep.Int n1078), 
          ("hasRest", PrettyRep.Bool b1079), ("result", 
       (case opt1081 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1080 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1080))
       ))]
   and cvtFUNC_DEFN {kind=x1098, ns=opt1100, final=b1104, override=b1105, prototype=b1106, 
          static=b1107, func=x1108} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1098), 
          ("ns", 
       (case opt1100 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1099 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1099))
       )), ("final", PrettyRep.Bool b1104), ("override", PrettyRep.Bool b1105), 
          ("prototype", PrettyRep.Bool b1106), ("static", PrettyRep.Bool b1107), 
          ("func", cvtFUNC x1108)]
   and cvtCTOR_DEFN x1124 = cvtCTOR x1124
   and cvtVAR_DEFN {kind=x1125, ns=opt1127, static=b1131, prototype=b1132, 
          bindings=(ls1134, ls1139)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1125), 
          ("ns", 
       (case opt1127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1126 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1126))
       )), ("static", PrettyRep.Bool b1131), ("prototype", PrettyRep.Bool b1132), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1133 => 
                                                                        cvtBINDING x1133
                                                                 ) ls1134), 
          PrettyRep.List (List.map (fn x1138 => cvtINIT_STEP x1138
                                   ) ls1139)])]
   and cvtNAMESPACE_DEFN {ident=x1155, ns=opt1157, init=opt1162} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1155), ("ns", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1156))
       )), ("init", 
       (case opt1162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1161 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1161))
       ))]
   and cvtCLASS_DEFN {ns=opt1174, privateNS=x1178, protectedNS=x1179, ident=x1180, 
          nonnullable=b1181, dynamic=b1182, final=b1183, params=ls1185, extends=opt1190, 
          implements=ls1195, classDefns=ls1200, instanceDefns=ls1205, instanceStmts=ls1210, 
          ctorDefn=opt1215} = PrettyRep.Rec [("ns", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1173))
       )), ("privateNS", cvtNAMESPACE x1178), ("protectedNS", cvtNAMESPACE x1179), 
          ("ident", cvtIDENTIFIER x1180), ("nonnullable", PrettyRep.Bool b1181), 
          ("dynamic", PrettyRep.Bool b1182), ("final", PrettyRep.Bool b1183), 
          ("params", PrettyRep.List (List.map (fn x1184 => cvtIDENTIFIER x1184
                                              ) ls1185)), ("extends", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1189 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1189))
       )), ("implements", PrettyRep.List (List.map (fn x1194 => cvtTYPE x1194
                                                   ) ls1195)), ("classDefns", 
          PrettyRep.List (List.map (fn x1199 => cvtDEFN x1199
                                   ) ls1200)), ("instanceDefns", PrettyRep.List (List.map (fn x1204 => 
                                                                                                 cvtDEFN x1204
                                                                                          ) ls1205)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1209 => cvtSTATEMENT x1209
                                                     ) ls1210)), ("ctorDefn", 
          
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1214 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1214))
       ))]
   and cvtINTERFACE_DEFN {ident=x1248, ns=opt1250, nonnullable=b1254, params=ls1256, 
          extends=ls1261, instanceDefns=ls1266} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1248), ("ns", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1249))
       )), ("nonnullable", PrettyRep.Bool b1254), ("params", PrettyRep.List (List.map (fn x1255 => 
                                                                                             cvtIDENTIFIER x1255
                                                                                      ) ls1256)), 
          ("extends", PrettyRep.List (List.map (fn x1260 => cvtTYPE x1260
                                               ) ls1261)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1265 => cvtDEFN x1265
                                   ) ls1266))]
   and cvtTYPE_DEFN {ident=x1283, ns=opt1285, typeParams=ls1290, init=x1294} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1283), ("ns", 
       (case opt1285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1284 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1284))
       )), ("typeParams", PrettyRep.List (List.map (fn x1289 => cvtIDENTIFIER x1289
                                                   ) ls1290)), ("init", cvtTYPE x1294)]
   and cvtCLASS_BLOCK {ns=opt1305, protectedNS=x1309, privateNS=x1310, ident=x1311, 
          name=opt1313, block=x1317} = PrettyRep.Rec [("ns", 
       (case opt1305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1304 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1304))
       )), ("protectedNS", cvtNAMESPACE x1309), ("privateNS", cvtNAMESPACE x1310), 
          ("ident", cvtIDENTIFIER x1311), ("name", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1312))
       )), ("block", cvtBLOCK x1317)]
   and cvtFOR_ENUM_HEAD {isEach=b1331, bindings=(ls1333, ls1338), expr=x1343} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1331), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1332 => 
                                                                                                                         cvtBINDING x1332
                                                                                                                  ) ls1333), 
          PrettyRep.List (List.map (fn x1337 => cvtINIT_STEP x1337
                                   ) ls1338)]), ("expr", cvtEXPRESSION x1343)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1351, defn=opt1382, obj=x1386, rib=opt1394, 
          next=x1398, labels=ls1400, body=x1404} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1351), ("defn", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1352, ns=opt1354, static=b1358, prototype=b1359, bindings=(ls1361, 
            ls1366)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1352), ("ns", 
         (case opt1354 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1353 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1353))
         )), ("static", PrettyRep.Bool b1358), ("prototype", PrettyRep.Bool b1359), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1360 => 
                                                                          cvtBINDING x1360
                                                                   ) ls1361), 
            PrettyRep.List (List.map (fn x1365 => cvtINIT_STEP x1365
                                     ) ls1366)])]))
       )), ("obj", cvtEXPRESSION x1386), ("rib", 
       (case opt1394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1390 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1387, 
                                                                                      x1388) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1387, 
                                                                                      cvtFIXTURE x1388]
                                                                               ) ls1390)))
       )), ("next", cvtSTATEMENT x1398), ("labels", PrettyRep.List (List.map (fn x1399 => 
                                                                                    cvtIDENTIFIER x1399
                                                                             ) ls1400)), 
          ("body", cvtSTATEMENT x1404)]
   and cvtFOR_STATEMENT {rib=opt1427, defn=opt1461, init=ls1466, cond=x1470, 
          update=x1471, labels=ls1473, body=x1477} = PrettyRep.Rec [("rib", 
          
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1423 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1420, 
                                                                                      x1421) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1420, 
                                                                                      cvtFIXTURE x1421]
                                                                               ) ls1423)))
       )), ("defn", 
       (case opt1461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1431, ns=opt1433, static=b1437, prototype=b1438, bindings=(ls1440, 
            ls1445)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1431), ("ns", 
         (case opt1433 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1432))
         )), ("static", PrettyRep.Bool b1437), ("prototype", PrettyRep.Bool b1438), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1439 => 
                                                                          cvtBINDING x1439
                                                                   ) ls1440), 
            PrettyRep.List (List.map (fn x1444 => cvtINIT_STEP x1444
                                     ) ls1445)])]))
       )), ("init", PrettyRep.List (List.map (fn x1465 => cvtSTATEMENT x1465
                                             ) ls1466)), ("cond", cvtEXPRESSION x1470), 
          ("update", cvtEXPRESSION x1471), ("labels", PrettyRep.List (List.map (fn x1472 => 
                                                                                      cvtIDENTIFIER x1472
                                                                               ) ls1473)), 
          ("body", cvtSTATEMENT x1477)]
   and cvtWHILE_STATEMENT {cond=x1493, rib=opt1501, body=x1505, labels=ls1507} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1493), ("rib", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1497 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1494, 
                                                                                      x1495) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1494, 
                                                                                      cvtFIXTURE x1495]
                                                                               ) ls1497)))
       )), ("body", cvtSTATEMENT x1505), ("labels", PrettyRep.List (List.map (fn x1506 => 
                                                                                    cvtIDENTIFIER x1506
                                                                             ) ls1507))]
   and cvtDIRECTIVES {pragmas=ls1521, defns=ls1526, head=opt1531, body=ls1536, 
          loc=opt1541} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1520 => 
                                                                                    cvtPRAGMA x1520
                                                                             ) ls1521)), 
          ("defns", PrettyRep.List (List.map (fn x1525 => cvtDEFN x1525
                                             ) ls1526)), ("head", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1530))
       )), ("body", PrettyRep.List (List.map (fn x1535 => cvtSTATEMENT x1535
                                             ) ls1536)), ("loc", 
       (case opt1541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1540 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1540))
       ))]
   and cvtCASE {label=opt1557, inits=opt1568, body=x1572} = PrettyRep.Rec [("label", 
          
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1556))
       )), ("inits", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1564 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1561, 
                                                                                      x1562) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1561, 
                                                                                      cvtEXPRESSION x1562]
                                                                               ) ls1564)))
       )), ("body", cvtBLOCK x1572)]
   and cvtCATCH_CLAUSE {bindings=(ls1581, ls1586), ty=x1591, rib=opt1599, inits=opt1610, 
          block=x1614} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1580 => 
                                                                                                      cvtBINDING x1580
                                                                                               ) ls1581), 
          PrettyRep.List (List.map (fn x1585 => cvtINIT_STEP x1585
                                   ) ls1586)]), ("ty", cvtTYPE x1591), ("rib", 
          
       (case opt1599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1595 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1592, 
                                                                                      x1593) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1592, 
                                                                                      cvtFIXTURE x1593]
                                                                               ) ls1595)))
       )), ("inits", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1606 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1603, 
                                                                                      x1604) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1603, 
                                                                                      cvtEXPRESSION x1604]
                                                                               ) ls1606)))
       )), ("block", cvtBLOCK x1614)]
   and cvtFUNC_NAME {kind=x1626, ident=x1627} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1626), 
          ("ident", cvtIDENTIFIER x1627)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1633, getter=opt1635, setter=opt1640} = 
          PrettyRep.Rec [("ty", cvtTYPE x1633), ("getter", 
       (case opt1635 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1634 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1634))
       )), ("setter", 
       (case opt1640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1639 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1639))
       ))]
   and cvtFRAGMENT (Anon x1651) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1651))
end

