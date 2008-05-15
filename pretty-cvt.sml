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
          constructor=opt181, classType=x185}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x152), ("privateNS", cvtNAMESPACE x153), ("protectedNS", 
          cvtNAMESPACE x154), ("parentProtectedNSs", PrettyRep.List (List.map (fn x155 => 
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
       )), ("classType", cvtTYPE x185)]))
   and cvtIFACE (Iface{name=x217, typeParams=ls219, nonnullable=b223, extends=ls225, 
          instanceRib=x229}) = PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x217), ("typeParams", PrettyRep.List (List.map (fn x218 => 
                                                                        cvtIDENTIFIER x218
                                                                 ) ls219)), 
          ("nonnullable", PrettyRep.Bool b223), ("extends", PrettyRep.List (List.map (fn x224 => 
                                                                                            cvtTYPE x224
                                                                                     ) ls225)), 
          ("instanceRib", cvtRIB x229)]))
   and cvtCTOR (Ctor{settings=x243, superArgs=ls245, func=x249}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x243), ("superArgs", PrettyRep.List (List.map (fn x244 => 
                                                                                                         cvtEXPRESSION x244
                                                                                                  ) ls245)), 
          ("func", cvtFUNC x249)]))
   and cvtFUNC (Func{name=x259, fsig=x260, native=b261, generator=b262, block=opt264, 
          param=x268, defaults=ls270, ty=x274, loc=opt276}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x259), ("fsig", cvtFUNC_SIG x260), 
          ("native", PrettyRep.Bool b261), ("generator", PrettyRep.Bool b262), 
          ("block", 
       (case opt264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x263 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x263))
       )), ("param", cvtHEAD x268), ("defaults", PrettyRep.List (List.map (fn x269 => 
                                                                                 cvtEXPRESSION x269
                                                                          ) ls270)), 
          ("ty", cvtTYPE x274), ("loc", 
       (case opt276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x275 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x275))
       ))]))
   and cvtDEFN (ClassDefn x301) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x301))
     | cvtDEFN (VariableDefn x304) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x304))
     | cvtDEFN (FunctionDefn x307) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x307))
     | cvtDEFN (ConstructorDefn x310) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x310))
     | cvtDEFN (InterfaceDefn x313) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x313))
     | cvtDEFN (NamespaceDefn x316) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x316))
     | cvtDEFN (TypeDefn x319) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x319))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls323, params=x327, paramTypes=ls329, 
          defaults=ls334, ctorInits=opt345, returnType=opt350, thisType=opt355, 
          hasRest=b359}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x322 => cvtIDENTIFIER x322
                                   ) ls323)), ("params", cvtBINDINGS x327), 
          ("paramTypes", PrettyRep.List (List.map (fn x328 => cvtTYPE x328
                                                  ) ls329)), ("defaults", PrettyRep.List (List.map (fn x333 => 
                                                                                                          cvtEXPRESSION x333
                                                                                                   ) ls334)), 
          ("ctorInits", 
       (case opt345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x338, ls340) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x338, 
            PrettyRep.List (List.map (fn x339 => cvtEXPRESSION x339
                                     ) ls340)]))
       )), ("returnType", 
       (case opt350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x349 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x349))
       )), ("thisType", 
       (case opt355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x354 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x354))
       )), ("hasRest", PrettyRep.Bool b359)]))
   and cvtBINDING (Binding{ident=x379, ty=x380}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x379), ("ty", 
          cvtTYPE x380)]))
   and cvtBINDING_IDENTIFIER (TempIdent n388) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n388))
     | cvtBINDING_IDENTIFIER (ParamIdent n391) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n391))
     | cvtBINDING_IDENTIFIER (PropIdent x394) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x394))
   and cvtINIT_STEP (InitStep(x397, x398)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x397, 
          cvtEXPRESSION x398]))
     | cvtINIT_STEP (AssignStep(x402, x403)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x402, cvtEXPRESSION x403]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (RecordType ls413) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn (x410, 
                                                                                                        x411) => 
                                                                                                        PrettyRep.Tuple [cvtNAME_EXPRESSION x410, 
                                                                                                        cvtTYPE x411]
                                                                                                 ) ls413)))
     | cvtTYPE (ArrayType(ls420, opt425)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x419 => 
                                                                                                                                cvtTYPE x419
                                                                                                                         ) ls420), 
          
       (case opt425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x424 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x424))
       )]))
     | cvtTYPE (UnionType ls433) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x432 => 
                                                                                                      cvtTYPE x432
                                                                                               ) ls433)))
     | cvtTYPE (NonNullType x439) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x439))
     | cvtTYPE (FunctionType x442) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x442))
     | cvtTYPE (AppType(x445, ls447)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x445, 
          PrettyRep.List (List.map (fn x446 => cvtTYPE x446
                                   ) ls447)]))
     | cvtTYPE (TypeName(x454, opt456)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x454, 
          
       (case opt456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x455 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x455))
       )]))
     | cvtTYPE (ClassType x463) = PrettyRep.Ctor ("ClassType", SOME (cvtCLS x463))
     | cvtTYPE (InterfaceType x466) = PrettyRep.Ctor ("InterfaceType", SOME (cvtIFACE x466))
     | cvtTYPE (TypeNameReferenceType(x469, x470)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x469, cvtNAME_EXPRESSION x470]))
     | cvtTYPE (TypeIndexReferenceType(x474, n475)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x474, PrettyRep.Int n475]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x480) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x480))
     | cvtSTATEMENT (InitStmt{kind=x483, ns=opt485, prototype=b489, static=b490, 
          temps=x491, inits=ls493}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x483), ("ns", 
       (case opt485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x484 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x484))
       )), ("prototype", PrettyRep.Bool b489), ("static", PrettyRep.Bool b490), 
          ("temps", cvtBINDINGS x491), ("inits", PrettyRep.List (List.map (fn x492 => 
                                                                                 cvtINIT_STEP x492
                                                                          ) ls493))]))
     | cvtSTATEMENT (ClassBlock x512) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x512))
     | cvtSTATEMENT (ForInStmt x515) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x515))
     | cvtSTATEMENT (ThrowStmt x518) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x518))
     | cvtSTATEMENT (ReturnStmt x521) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x521))
     | cvtSTATEMENT (BreakStmt opt525) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x524 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x524))
       ))
     | cvtSTATEMENT (ContinueStmt opt532) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x531 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x531))
       ))
     | cvtSTATEMENT (BlockStmt x538) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x538))
     | cvtSTATEMENT (LabeledStmt(x541, x542)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x541, cvtSTATEMENT x542]))
     | cvtSTATEMENT (LetStmt x546) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x546))
     | cvtSTATEMENT (WhileStmt x549) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x549))
     | cvtSTATEMENT (DoWhileStmt x552) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x552))
     | cvtSTATEMENT (ForStmt x555) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x555))
     | cvtSTATEMENT (IfStmt{cnd=x558, thn=x559, els=x560}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x558), ("thn", cvtSTATEMENT x559), 
          ("els", cvtSTATEMENT x560)]))
     | cvtSTATEMENT (WithStmt{obj=x570, ty=x571, body=x572}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x570), ("ty", cvtTYPE x571), 
          ("body", cvtSTATEMENT x572)]))
     | cvtSTATEMENT (TryStmt{block=x582, catches=ls584, finally=opt589}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x582), 
          ("catches", PrettyRep.List (List.map (fn x583 => cvtCATCH_CLAUSE x583
                                               ) ls584)), ("finally", 
       (case opt589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x588 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x588))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x602, labels=ls604, cases=ls609}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x602), ("labels", PrettyRep.List (List.map (fn x603 => 
                                                                                                        cvtIDENTIFIER x603
                                                                                                 ) ls604)), 
          ("cases", PrettyRep.List (List.map (fn x608 => cvtCASE x608
                                             ) ls609))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x622, ty=x623, cases=ls625}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x622), ("ty", cvtTYPE x623), 
          ("cases", PrettyRep.List (List.map (fn x624 => cvtCATCH_CLAUSE x624
                                             ) ls625))]))
     | cvtSTATEMENT (DXNStmt{expr=x638}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x638)]))
   and cvtEXPRESSION (TernaryExpr(x644, x645, x646)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x644, cvtEXPRESSION x645, cvtEXPRESSION x646]))
     | cvtEXPRESSION (BinaryExpr(x650, x651, x652)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x650, cvtEXPRESSION x651, cvtEXPRESSION x652]))
     | cvtEXPRESSION (BinaryTypeExpr(x656, x657, x658)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x656, cvtEXPRESSION x657, cvtTYPE x658]))
     | cvtEXPRESSION (UnaryExpr(x662, x663)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x662, cvtEXPRESSION x663]))
     | cvtEXPRESSION (TypeExpr x667) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x667))
     | cvtEXPRESSION (ThisExpr opt671) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x670 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x670))
       ))
     | cvtEXPRESSION (YieldExpr opt678) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt678 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x677 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x677))
       ))
     | cvtEXPRESSION (SuperExpr opt685) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt685 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x684 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x684))
       ))
     | cvtEXPRESSION (CallExpr{func=x691, actuals=ls693}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x691), ("actuals", PrettyRep.List (List.map (fn x692 => 
                                                                                                         cvtEXPRESSION x692
                                                                                                  ) ls693))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x704, actuals=ls706}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x704), ("actuals", PrettyRep.List (List.map (fn x705 => 
                                                                                                         cvtTYPE x705
                                                                                                  ) ls706))]))
     | cvtEXPRESSION (LetExpr{defs=x717, body=x718, head=opt720}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x717), ("body", cvtEXPRESSION x718), 
          ("head", 
       (case opt720 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x719 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x719))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x733, actuals=ls735}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x733), ("actuals", PrettyRep.List (List.map (fn x734 => 
                                                                                                        cvtEXPRESSION x734
                                                                                                 ) ls735))]))
     | cvtEXPRESSION (SetExpr(x746, x747, x748)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x746, cvtEXPRESSION x747, cvtEXPRESSION x748]))
     | cvtEXPRESSION (ListExpr ls753) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x752 => 
                                                                                                          cvtEXPRESSION x752
                                                                                                   ) ls753)))
     | cvtEXPRESSION (InitExpr(x759, x760, x761)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x759, cvtHEAD x760, cvtINITS x761]))
     | cvtEXPRESSION (GetTemp n765) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n765))
     | cvtEXPRESSION (GetParam n768) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n768))
     | cvtEXPRESSION (Comprehension(x771, ls773, opt778)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x771, PrettyRep.List (List.map (fn x772 => 
                                                                                     cvtFOR_ENUM_HEAD x772
                                                                              ) ls773), 
          
       (case opt778 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x777 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x777))
       )]))
     | cvtEXPRESSION (LiteralExpr x785) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x785))
     | cvtEXPRESSION (LexicalReference{name=x788, loc=opt790}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x788), ("loc", 
       (case opt790 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x789 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x789))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x801, name=x802, loc=opt804}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x801), ("name", cvtNAME_EXPRESSION x802), ("loc", 
       (case opt804 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x803 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x803))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x817, index=x818, loc=opt820}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x817), ("index", cvtEXPRESSION x818), ("loc", 
       (case opt820 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x819 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x819))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n838) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n838))
     | cvtFIXTURE_NAME (PropName x841) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x841))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r846) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r846))
     | cvtLITERAL (LiteralDecimal d849) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d849))
     | cvtLITERAL (LiteralBoolean b852) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b852))
     | cvtLITERAL (LiteralString s855) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s855))
     | cvtLITERAL (LiteralArray{exprs=x858, ty=opt860}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x858), ("ty", 
       (case opt860 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x859 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x859))
       ))]))
     | cvtLITERAL (LiteralXML ls872) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x871 => 
                                                                                                           cvtEXPRESSION x871
                                                                                                    ) ls872)))
     | cvtLITERAL (LiteralNamespace x878) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x878))
     | cvtLITERAL (LiteralObject{expr=ls882, ty=opt887}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x881 => 
                                                                        cvtFIELD x881
                                                                 ) ls882)), 
          ("ty", 
       (case opt887 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x886 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x886))
       ))]))
     | cvtLITERAL (LiteralFunction x898) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x898))
     | cvtLITERAL (LiteralRegExp{str=s901}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s901)]))
   and cvtBLOCK (Block x907) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x907))
   and cvtFIXTURE (NamespaceFixture x910) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x910))
     | cvtFIXTURE (ClassFixture x913) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x913))
     | cvtFIXTURE (InterfaceFixture x916) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x916))
     | cvtFIXTURE (TypeVarFixture x919) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x919))
     | cvtFIXTURE (TypeFixture(ls923, x927)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x922 => cvtIDENTIFIER x922
                                                          ) ls923), cvtTYPE x927]))
     | cvtFIXTURE (MethodFixture{func=x931, ty=x932, writable=b933, override=b934, 
          final=b935}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x931), ("ty", cvtTYPE x932), ("writable", PrettyRep.Bool b933), 
          ("override", PrettyRep.Bool b934), ("final", PrettyRep.Bool b935)]))
     | cvtFIXTURE (ValFixture{ty=x949, writable=b950}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x949), ("writable", PrettyRep.Bool b950)]))
     | cvtFIXTURE (VirtualValFixture{ty=x958, getter=opt960, setter=opt965}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x958), ("getter", 
       (case opt960 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x959 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x959))
       )), ("setter", 
       (case opt965 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x964 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x964))
       ))]))
   and cvtHEAD (Head(x978, x979)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x978, 
          cvtINITS x979]))
   and cvtBINDINGS (ls984, ls989) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x983 => 
                                                                                     cvtBINDING x983
                                                                              ) ls984), 
          PrettyRep.List (List.map (fn x988 => cvtINIT_STEP x988
                                   ) ls989)]
   and cvtRIB ls997 = PrettyRep.List (List.map (fn (x994, x995) => PrettyRep.Tuple [cvtFIXTURE_NAME x994, 
                                                      cvtFIXTURE x995]
                                               ) ls997)
   and cvtRIBS ls1008 = PrettyRep.List (List.map (fn ls1004 => PrettyRep.List (List.map (fn (x1001, 
                                                                                               x1002) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1001, 
                                                                                               cvtFIXTURE x1002]
                                                                                        ) ls1004)
                                                 ) ls1008)
   and cvtINITS ls1015 = PrettyRep.List (List.map (fn (x1012, x1013) => PrettyRep.Tuple [cvtFIXTURE_NAME x1012, 
                                                         cvtEXPRESSION x1013]
                                                  ) ls1015)
   and cvtINSTANCE_TYPE {name=x1019, typeArgs=ls1021, nonnullable=b1025, typeParams=ls1027, 
          superTypes=ls1032, ty=x1036, dynamic=b1037} = PrettyRep.Rec [("name", 
          cvtNAME x1019), ("typeArgs", PrettyRep.List (List.map (fn x1020 => 
                                                                       cvtTYPE x1020
                                                                ) ls1021)), 
          ("nonnullable", PrettyRep.Bool b1025), ("typeParams", PrettyRep.List (List.map (fn x1026 => 
                                                                                                cvtIDENTIFIER x1026
                                                                                         ) ls1027)), 
          ("superTypes", PrettyRep.List (List.map (fn x1031 => cvtTYPE x1031
                                                  ) ls1032)), ("ty", cvtTYPE x1036), 
          ("dynamic", PrettyRep.Bool b1037)]
   and cvtFIELD {kind=x1053, name=x1054, init=x1055} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1053), ("name", cvtNAME_EXPRESSION x1054), ("init", 
          cvtEXPRESSION x1055)]
   and cvtFIELD_TYPE (x1063, x1064) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1063, 
          cvtTYPE x1064]
   and cvtFUNCTION_TYPE {typeParams=ls1067, thisType=x1071, params=ls1073, 
          minArgs=n1077, hasRest=b1078, result=opt1080} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1066 => cvtIDENTIFIER x1066
                                   ) ls1067)), ("thisType", cvtTYPE x1071), 
          ("params", PrettyRep.List (List.map (fn x1072 => cvtTYPE x1072
                                              ) ls1073)), ("minArgs", PrettyRep.Int n1077), 
          ("hasRest", PrettyRep.Bool b1078), ("result", 
       (case opt1080 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1079 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1079))
       ))]
   and cvtFUNC_DEFN {kind=x1097, ns=opt1099, final=b1103, override=b1104, prototype=b1105, 
          static=b1106, func=x1107} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1097), 
          ("ns", 
       (case opt1099 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1098 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1098))
       )), ("final", PrettyRep.Bool b1103), ("override", PrettyRep.Bool b1104), 
          ("prototype", PrettyRep.Bool b1105), ("static", PrettyRep.Bool b1106), 
          ("func", cvtFUNC x1107)]
   and cvtCTOR_DEFN x1123 = cvtCTOR x1123
   and cvtVAR_DEFN {kind=x1124, ns=opt1126, static=b1130, prototype=b1131, 
          bindings=(ls1133, ls1138)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1124), 
          ("ns", 
       (case opt1126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1125 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1125))
       )), ("static", PrettyRep.Bool b1130), ("prototype", PrettyRep.Bool b1131), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1132 => 
                                                                        cvtBINDING x1132
                                                                 ) ls1133), 
          PrettyRep.List (List.map (fn x1137 => cvtINIT_STEP x1137
                                   ) ls1138)])]
   and cvtNAMESPACE_DEFN {ident=x1154, ns=opt1156, init=opt1161} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1154), ("ns", 
       (case opt1156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1155 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1155))
       )), ("init", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1160))
       ))]
   and cvtCLASS_DEFN {ns=opt1173, privateNS=x1177, protectedNS=x1178, ident=x1179, 
          nonnullable=b1180, dynamic=b1181, final=b1182, params=ls1184, extends=opt1189, 
          implements=ls1194, classDefns=ls1199, instanceDefns=ls1204, instanceStmts=ls1209, 
          ctorDefn=opt1214} = PrettyRep.Rec [("ns", 
       (case opt1173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1172 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1172))
       )), ("privateNS", cvtNAMESPACE x1177), ("protectedNS", cvtNAMESPACE x1178), 
          ("ident", cvtIDENTIFIER x1179), ("nonnullable", PrettyRep.Bool b1180), 
          ("dynamic", PrettyRep.Bool b1181), ("final", PrettyRep.Bool b1182), 
          ("params", PrettyRep.List (List.map (fn x1183 => cvtIDENTIFIER x1183
                                              ) ls1184)), ("extends", 
       (case opt1189 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1188 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1188))
       )), ("implements", PrettyRep.List (List.map (fn x1193 => cvtTYPE x1193
                                                   ) ls1194)), ("classDefns", 
          PrettyRep.List (List.map (fn x1198 => cvtDEFN x1198
                                   ) ls1199)), ("instanceDefns", PrettyRep.List (List.map (fn x1203 => 
                                                                                                 cvtDEFN x1203
                                                                                          ) ls1204)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1208 => cvtSTATEMENT x1208
                                                     ) ls1209)), ("ctorDefn", 
          
       (case opt1214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1213 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1213))
       ))]
   and cvtINTERFACE_DEFN {ident=x1247, ns=opt1249, nonnullable=b1253, params=ls1255, 
          extends=ls1260, instanceDefns=ls1265} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1247), ("ns", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1248))
       )), ("nonnullable", PrettyRep.Bool b1253), ("params", PrettyRep.List (List.map (fn x1254 => 
                                                                                             cvtIDENTIFIER x1254
                                                                                      ) ls1255)), 
          ("extends", PrettyRep.List (List.map (fn x1259 => cvtTYPE x1259
                                               ) ls1260)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1264 => cvtDEFN x1264
                                   ) ls1265))]
   and cvtTYPE_DEFN {ident=x1282, ns=opt1284, typeParams=ls1289, init=x1293} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1282), ("ns", 
       (case opt1284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1283 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1283))
       )), ("typeParams", PrettyRep.List (List.map (fn x1288 => cvtIDENTIFIER x1288
                                                   ) ls1289)), ("init", cvtTYPE x1293)]
   and cvtCLASS_BLOCK {ns=opt1304, protectedNS=x1308, privateNS=x1309, ident=x1310, 
          name=opt1312, block=x1316} = PrettyRep.Rec [("ns", 
       (case opt1304 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1303 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1303))
       )), ("protectedNS", cvtNAMESPACE x1308), ("privateNS", cvtNAMESPACE x1309), 
          ("ident", cvtIDENTIFIER x1310), ("name", 
       (case opt1312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1311 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1311))
       )), ("block", cvtBLOCK x1316)]
   and cvtFOR_ENUM_HEAD {isEach=b1330, bindings=(ls1332, ls1337), expr=x1342} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1330), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1331 => 
                                                                                                                         cvtBINDING x1331
                                                                                                                  ) ls1332), 
          PrettyRep.List (List.map (fn x1336 => cvtINIT_STEP x1336
                                   ) ls1337)]), ("expr", cvtEXPRESSION x1342)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1350, defn=opt1381, obj=x1385, rib=opt1393, 
          next=x1397, labels=ls1399, body=x1403} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1350), ("defn", 
       (case opt1381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1351, ns=opt1353, static=b1357, prototype=b1358, bindings=(ls1360, 
            ls1365)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1351), ("ns", 
         (case opt1353 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1352))
         )), ("static", PrettyRep.Bool b1357), ("prototype", PrettyRep.Bool b1358), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1359 => 
                                                                          cvtBINDING x1359
                                                                   ) ls1360), 
            PrettyRep.List (List.map (fn x1364 => cvtINIT_STEP x1364
                                     ) ls1365)])]))
       )), ("obj", cvtEXPRESSION x1385), ("rib", 
       (case opt1393 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1389 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1386, 
                                                                                      x1387) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1386, 
                                                                                      cvtFIXTURE x1387]
                                                                               ) ls1389)))
       )), ("next", cvtSTATEMENT x1397), ("labels", PrettyRep.List (List.map (fn x1398 => 
                                                                                    cvtIDENTIFIER x1398
                                                                             ) ls1399)), 
          ("body", cvtSTATEMENT x1403)]
   and cvtFOR_STATEMENT {rib=opt1426, defn=opt1460, init=ls1465, cond=x1469, 
          update=x1470, labels=ls1472, body=x1476} = PrettyRep.Rec [("rib", 
          
       (case opt1426 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1422 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1419, 
                                                                                      x1420) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1419, 
                                                                                      cvtFIXTURE x1420]
                                                                               ) ls1422)))
       )), ("defn", 
       (case opt1460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1430, ns=opt1432, static=b1436, prototype=b1437, bindings=(ls1439, 
            ls1444)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1430), ("ns", 
         (case opt1432 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1431 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1431))
         )), ("static", PrettyRep.Bool b1436), ("prototype", PrettyRep.Bool b1437), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1438 => 
                                                                          cvtBINDING x1438
                                                                   ) ls1439), 
            PrettyRep.List (List.map (fn x1443 => cvtINIT_STEP x1443
                                     ) ls1444)])]))
       )), ("init", PrettyRep.List (List.map (fn x1464 => cvtSTATEMENT x1464
                                             ) ls1465)), ("cond", cvtEXPRESSION x1469), 
          ("update", cvtEXPRESSION x1470), ("labels", PrettyRep.List (List.map (fn x1471 => 
                                                                                      cvtIDENTIFIER x1471
                                                                               ) ls1472)), 
          ("body", cvtSTATEMENT x1476)]
   and cvtWHILE_STATEMENT {cond=x1492, rib=opt1500, body=x1504, labels=ls1506} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1492), ("rib", 
       (case opt1500 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1496 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1493, 
                                                                                      x1494) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1493, 
                                                                                      cvtFIXTURE x1494]
                                                                               ) ls1496)))
       )), ("body", cvtSTATEMENT x1504), ("labels", PrettyRep.List (List.map (fn x1505 => 
                                                                                    cvtIDENTIFIER x1505
                                                                             ) ls1506))]
   and cvtDIRECTIVES {pragmas=ls1520, defns=ls1525, head=opt1530, body=ls1535, 
          loc=opt1540} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1519 => 
                                                                                    cvtPRAGMA x1519
                                                                             ) ls1520)), 
          ("defns", PrettyRep.List (List.map (fn x1524 => cvtDEFN x1524
                                             ) ls1525)), ("head", 
       (case opt1530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1529 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1529))
       )), ("body", PrettyRep.List (List.map (fn x1534 => cvtSTATEMENT x1534
                                             ) ls1535)), ("loc", 
       (case opt1540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1539 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1539))
       ))]
   and cvtCASE {label=opt1556, inits=opt1567, body=x1571} = PrettyRep.Rec [("label", 
          
       (case opt1556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1555 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1555))
       )), ("inits", 
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1563 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1560, 
                                                                                      x1561) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1560, 
                                                                                      cvtEXPRESSION x1561]
                                                                               ) ls1563)))
       )), ("body", cvtBLOCK x1571)]
   and cvtCATCH_CLAUSE {bindings=(ls1580, ls1585), ty=x1590, rib=opt1598, inits=opt1609, 
          block=x1613} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1579 => 
                                                                                                      cvtBINDING x1579
                                                                                               ) ls1580), 
          PrettyRep.List (List.map (fn x1584 => cvtINIT_STEP x1584
                                   ) ls1585)]), ("ty", cvtTYPE x1590), ("rib", 
          
       (case opt1598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1594 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1591, 
                                                                                      x1592) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1591, 
                                                                                      cvtFIXTURE x1592]
                                                                               ) ls1594)))
       )), ("inits", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1605 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1602, 
                                                                                      x1603) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1602, 
                                                                                      cvtEXPRESSION x1603]
                                                                               ) ls1605)))
       )), ("block", cvtBLOCK x1613)]
   and cvtFUNC_NAME {kind=x1625, ident=x1626} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1625), 
          ("ident", cvtIDENTIFIER x1626)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1632, getter=opt1634, setter=opt1639} = 
          PrettyRep.Rec [("ty", cvtTYPE x1632), ("getter", 
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1633 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1633))
       )), ("setter", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1638 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1638))
       ))]
   and cvtFRAGMENT (Anon x1650) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1650))
end

