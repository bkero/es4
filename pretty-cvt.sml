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
     | cvtFIXTURE (MethodFixture{func=x932, ty=x933, readOnly=b934, override=b935, 
          final=b936}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x932), ("ty", cvtTYPE x933), ("readOnly", PrettyRep.Bool b934), 
          ("override", PrettyRep.Bool b935), ("final", PrettyRep.Bool b936)]))
     | cvtFIXTURE (ValFixture{ty=x950, readOnly=b951}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x950), ("readOnly", PrettyRep.Bool b951)]))
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
   and cvtFIELD_TYPE {name=x1064, ty=x1065} = PrettyRep.Rec [("name", cvtNAME_EXPRESSION x1064), 
          ("ty", cvtTYPE x1065)]
   and cvtFUNCTION_TYPE {typeParams=ls1072, thisType=x1076, params=ls1078, 
          minArgs=n1082, hasRest=b1083, result=opt1085} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1071 => cvtIDENTIFIER x1071
                                   ) ls1072)), ("thisType", cvtTYPE x1076), 
          ("params", PrettyRep.List (List.map (fn x1077 => cvtTYPE x1077
                                              ) ls1078)), ("minArgs", PrettyRep.Int n1082), 
          ("hasRest", PrettyRep.Bool b1083), ("result", 
       (case opt1085 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1084 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1084))
       ))]
   and cvtFUNC_DEFN {kind=x1102, ns=opt1104, final=b1108, override=b1109, prototype=b1110, 
          static=b1111, func=x1112} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1102), 
          ("ns", 
       (case opt1104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1103 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1103))
       )), ("final", PrettyRep.Bool b1108), ("override", PrettyRep.Bool b1109), 
          ("prototype", PrettyRep.Bool b1110), ("static", PrettyRep.Bool b1111), 
          ("func", cvtFUNC x1112)]
   and cvtCTOR_DEFN x1128 = cvtCTOR x1128
   and cvtVAR_DEFN {kind=x1129, ns=opt1131, static=b1135, prototype=b1136, 
          bindings=(ls1138, ls1143)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1129), 
          ("ns", 
       (case opt1131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1130 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1130))
       )), ("static", PrettyRep.Bool b1135), ("prototype", PrettyRep.Bool b1136), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1137 => 
                                                                        cvtBINDING x1137
                                                                 ) ls1138), 
          PrettyRep.List (List.map (fn x1142 => cvtINIT_STEP x1142
                                   ) ls1143)])]
   and cvtNAMESPACE_DEFN {ident=x1159, ns=opt1161, init=opt1166} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1159), ("ns", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1160))
       )), ("init", 
       (case opt1166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1165 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1165))
       ))]
   and cvtCLASS_DEFN {ns=opt1178, privateNS=x1182, protectedNS=x1183, ident=x1184, 
          nonnullable=b1185, dynamic=b1186, final=b1187, params=ls1189, extends=opt1194, 
          implements=ls1199, classDefns=ls1204, instanceDefns=ls1209, instanceStmts=ls1214, 
          ctorDefn=opt1219} = PrettyRep.Rec [("ns", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1177))
       )), ("privateNS", cvtNAMESPACE x1182), ("protectedNS", cvtNAMESPACE x1183), 
          ("ident", cvtIDENTIFIER x1184), ("nonnullable", PrettyRep.Bool b1185), 
          ("dynamic", PrettyRep.Bool b1186), ("final", PrettyRep.Bool b1187), 
          ("params", PrettyRep.List (List.map (fn x1188 => cvtIDENTIFIER x1188
                                              ) ls1189)), ("extends", 
       (case opt1194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1193 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1193))
       )), ("implements", PrettyRep.List (List.map (fn x1198 => cvtTYPE x1198
                                                   ) ls1199)), ("classDefns", 
          PrettyRep.List (List.map (fn x1203 => cvtDEFN x1203
                                   ) ls1204)), ("instanceDefns", PrettyRep.List (List.map (fn x1208 => 
                                                                                                 cvtDEFN x1208
                                                                                          ) ls1209)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1213 => cvtSTATEMENT x1213
                                                     ) ls1214)), ("ctorDefn", 
          
       (case opt1219 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1218 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1218))
       ))]
   and cvtINTERFACE_DEFN {ident=x1252, ns=opt1254, nonnullable=b1258, params=ls1260, 
          extends=ls1265, instanceDefns=ls1270} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1252), ("ns", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1253))
       )), ("nonnullable", PrettyRep.Bool b1258), ("params", PrettyRep.List (List.map (fn x1259 => 
                                                                                             cvtIDENTIFIER x1259
                                                                                      ) ls1260)), 
          ("extends", PrettyRep.List (List.map (fn x1264 => cvtTYPE x1264
                                               ) ls1265)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1269 => cvtDEFN x1269
                                   ) ls1270))]
   and cvtTYPE_DEFN {ident=x1287, ns=opt1289, typeParams=ls1294, init=x1298} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1287), ("ns", 
       (case opt1289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1288 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1288))
       )), ("typeParams", PrettyRep.List (List.map (fn x1293 => cvtIDENTIFIER x1293
                                                   ) ls1294)), ("init", cvtTYPE x1298)]
   and cvtCLASS_BLOCK {ns=opt1309, protectedNS=x1313, privateNS=x1314, ident=x1315, 
          name=opt1317, block=x1321} = PrettyRep.Rec [("ns", 
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1308))
       )), ("protectedNS", cvtNAMESPACE x1313), ("privateNS", cvtNAMESPACE x1314), 
          ("ident", cvtIDENTIFIER x1315), ("name", 
       (case opt1317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1316 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1316))
       )), ("block", cvtBLOCK x1321)]
   and cvtFOR_ENUM_HEAD {isEach=b1335, bindings=(ls1337, ls1342), expr=x1347} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1335), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1336 => 
                                                                                                                         cvtBINDING x1336
                                                                                                                  ) ls1337), 
          PrettyRep.List (List.map (fn x1341 => cvtINIT_STEP x1341
                                   ) ls1342)]), ("expr", cvtEXPRESSION x1347)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1355, defn=opt1386, obj=x1390, rib=opt1398, 
          next=x1402, labels=ls1404, body=x1408} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1355), ("defn", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1356, ns=opt1358, static=b1362, prototype=b1363, bindings=(ls1365, 
            ls1370)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1356), ("ns", 
         (case opt1358 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1357))
         )), ("static", PrettyRep.Bool b1362), ("prototype", PrettyRep.Bool b1363), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1364 => 
                                                                          cvtBINDING x1364
                                                                   ) ls1365), 
            PrettyRep.List (List.map (fn x1369 => cvtINIT_STEP x1369
                                     ) ls1370)])]))
       )), ("obj", cvtEXPRESSION x1390), ("rib", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1394 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1391, 
                                                                                      x1392) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1391, 
                                                                                      cvtFIXTURE x1392]
                                                                               ) ls1394)))
       )), ("next", cvtSTATEMENT x1402), ("labels", PrettyRep.List (List.map (fn x1403 => 
                                                                                    cvtIDENTIFIER x1403
                                                                             ) ls1404)), 
          ("body", cvtSTATEMENT x1408)]
   and cvtFOR_STATEMENT {rib=opt1431, defn=opt1465, init=ls1470, cond=x1474, 
          update=x1475, labels=ls1477, body=x1481} = PrettyRep.Rec [("rib", 
          
       (case opt1431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1427 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1424, 
                                                                                      x1425) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1424, 
                                                                                      cvtFIXTURE x1425]
                                                                               ) ls1427)))
       )), ("defn", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1435, ns=opt1437, static=b1441, prototype=b1442, bindings=(ls1444, 
            ls1449)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1435), ("ns", 
         (case opt1437 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1436))
         )), ("static", PrettyRep.Bool b1441), ("prototype", PrettyRep.Bool b1442), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1443 => 
                                                                          cvtBINDING x1443
                                                                   ) ls1444), 
            PrettyRep.List (List.map (fn x1448 => cvtINIT_STEP x1448
                                     ) ls1449)])]))
       )), ("init", PrettyRep.List (List.map (fn x1469 => cvtSTATEMENT x1469
                                             ) ls1470)), ("cond", cvtEXPRESSION x1474), 
          ("update", cvtEXPRESSION x1475), ("labels", PrettyRep.List (List.map (fn x1476 => 
                                                                                      cvtIDENTIFIER x1476
                                                                               ) ls1477)), 
          ("body", cvtSTATEMENT x1481)]
   and cvtWHILE_STATEMENT {cond=x1497, rib=opt1505, body=x1509, labels=ls1511} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1497), ("rib", 
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1501 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1498, 
                                                                                      x1499) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1498, 
                                                                                      cvtFIXTURE x1499]
                                                                               ) ls1501)))
       )), ("body", cvtSTATEMENT x1509), ("labels", PrettyRep.List (List.map (fn x1510 => 
                                                                                    cvtIDENTIFIER x1510
                                                                             ) ls1511))]
   and cvtDIRECTIVES {pragmas=ls1525, defns=ls1530, head=opt1535, body=ls1540, 
          loc=opt1545} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1524 => 
                                                                                    cvtPRAGMA x1524
                                                                             ) ls1525)), 
          ("defns", PrettyRep.List (List.map (fn x1529 => cvtDEFN x1529
                                             ) ls1530)), ("head", 
       (case opt1535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1534 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1534))
       )), ("body", PrettyRep.List (List.map (fn x1539 => cvtSTATEMENT x1539
                                             ) ls1540)), ("loc", 
       (case opt1545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1544 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1544))
       ))]
   and cvtCASE {label=opt1561, inits=opt1572, body=x1576} = PrettyRep.Rec [("label", 
          
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1560))
       )), ("inits", 
       (case opt1572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1568 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1565, 
                                                                                      x1566) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1565, 
                                                                                      cvtEXPRESSION x1566]
                                                                               ) ls1568)))
       )), ("body", cvtBLOCK x1576)]
   and cvtCATCH_CLAUSE {bindings=(ls1585, ls1590), ty=x1595, rib=opt1603, inits=opt1614, 
          block=x1618} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1584 => 
                                                                                                      cvtBINDING x1584
                                                                                               ) ls1585), 
          PrettyRep.List (List.map (fn x1589 => cvtINIT_STEP x1589
                                   ) ls1590)]), ("ty", cvtTYPE x1595), ("rib", 
          
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1599 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1596, 
                                                                                      x1597) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1596, 
                                                                                      cvtFIXTURE x1597]
                                                                               ) ls1599)))
       )), ("inits", 
       (case opt1614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1610 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1607, 
                                                                                      x1608) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1607, 
                                                                                      cvtEXPRESSION x1608]
                                                                               ) ls1610)))
       )), ("block", cvtBLOCK x1618)]
   and cvtFUNC_NAME {kind=x1630, ident=x1631} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1630), 
          ("ident", cvtIDENTIFIER x1631)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1637, getter=opt1639, setter=opt1644} = 
          PrettyRep.Rec [("ty", cvtTYPE x1637), ("getter", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1638 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1638))
       )), ("setter", 
       (case opt1644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1643 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1643))
       ))]
   and cvtFRAGMENT (Anon x1655) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1655))
end

