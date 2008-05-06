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
     | cvtTYPE (ArrayType ls424) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                      cvtTYPE x423
                                                                                               ) ls424)))
     | cvtTYPE (UnionType ls431) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x430 => 
                                                                                                      cvtTYPE x430
                                                                                               ) ls431)))
     | cvtTYPE (FunctionType x437) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x437))
     | cvtTYPE (NonNullType x440) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x440))
     | cvtTYPE (AppType{base=x443, args=ls445}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x443), ("args", PrettyRep.List (List.map (fn x444 => 
                                                                                                cvtTYPE x444
                                                                                         ) ls445))]))
     | cvtTYPE (TypeName(x456, opt458)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x456, 
          
       (case opt458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x457 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x457))
       )]))
     | cvtTYPE (TypeNameReferenceType(x465, x466)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x465, cvtNAME_EXPRESSION x466]))
     | cvtTYPE (TypeIndexReferenceType(x470, n471)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x470, PrettyRep.Int n471]))
     | cvtTYPE (InstanceType x475) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x475))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x479) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x479))
     | cvtSTATEMENT (InitStmt{kind=x482, ns=opt484, prototype=b488, static=b489, 
          temps=x490, inits=ls492}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x482), ("ns", 
       (case opt484 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x483 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x483))
       )), ("prototype", PrettyRep.Bool b488), ("static", PrettyRep.Bool b489), 
          ("temps", cvtBINDINGS x490), ("inits", PrettyRep.List (List.map (fn x491 => 
                                                                                 cvtINIT_STEP x491
                                                                          ) ls492))]))
     | cvtSTATEMENT (ClassBlock x511) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x511))
     | cvtSTATEMENT (ForInStmt x514) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x514))
     | cvtSTATEMENT (ThrowStmt x517) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x517))
     | cvtSTATEMENT (ReturnStmt x520) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x520))
     | cvtSTATEMENT (BreakStmt opt524) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x523 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x523))
       ))
     | cvtSTATEMENT (ContinueStmt opt531) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x530 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x530))
       ))
     | cvtSTATEMENT (BlockStmt x537) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x537))
     | cvtSTATEMENT (LabeledStmt(x540, x541)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x540, cvtSTATEMENT x541]))
     | cvtSTATEMENT (LetStmt x545) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x545))
     | cvtSTATEMENT (WhileStmt x548) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x548))
     | cvtSTATEMENT (DoWhileStmt x551) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x551))
     | cvtSTATEMENT (ForStmt x554) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x554))
     | cvtSTATEMENT (IfStmt{cnd=x557, thn=x558, els=x559}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x557), ("thn", cvtSTATEMENT x558), 
          ("els", cvtSTATEMENT x559)]))
     | cvtSTATEMENT (WithStmt{obj=x569, ty=x570, body=x571}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x569), ("ty", cvtTYPE x570), 
          ("body", cvtSTATEMENT x571)]))
     | cvtSTATEMENT (TryStmt{block=x581, catches=ls583, finally=opt588}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x581), 
          ("catches", PrettyRep.List (List.map (fn x582 => cvtCATCH_CLAUSE x582
                                               ) ls583)), ("finally", 
       (case opt588 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x587 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x587))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x601, labels=ls603, cases=ls608}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x601), ("labels", PrettyRep.List (List.map (fn x602 => 
                                                                                                        cvtIDENTIFIER x602
                                                                                                 ) ls603)), 
          ("cases", PrettyRep.List (List.map (fn x607 => cvtCASE x607
                                             ) ls608))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x621, ty=x622, cases=ls624}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x621), ("ty", cvtTYPE x622), 
          ("cases", PrettyRep.List (List.map (fn x623 => cvtCATCH_CLAUSE x623
                                             ) ls624))]))
     | cvtSTATEMENT (DXNStmt{expr=x637}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x637)]))
   and cvtEXPRESSION (TernaryExpr(x643, x644, x645)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x643, cvtEXPRESSION x644, cvtEXPRESSION x645]))
     | cvtEXPRESSION (BinaryExpr(x649, x650, x651)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x649, cvtEXPRESSION x650, cvtEXPRESSION x651]))
     | cvtEXPRESSION (BinaryTypeExpr(x655, x656, x657)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x655, cvtEXPRESSION x656, cvtTYPE x657]))
     | cvtEXPRESSION (UnaryExpr(x661, x662)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x661, cvtEXPRESSION x662]))
     | cvtEXPRESSION (TypeExpr x666) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x666))
     | cvtEXPRESSION (ThisExpr opt670) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x669 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x669))
       ))
     | cvtEXPRESSION (YieldExpr opt677) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt677 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x676 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x676))
       ))
     | cvtEXPRESSION (SuperExpr opt684) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x683 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x683))
       ))
     | cvtEXPRESSION (CallExpr{func=x690, actuals=ls692}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x690), ("actuals", PrettyRep.List (List.map (fn x691 => 
                                                                                                         cvtEXPRESSION x691
                                                                                                  ) ls692))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x703, actuals=ls705}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x703), ("actuals", PrettyRep.List (List.map (fn x704 => 
                                                                                                         cvtTYPE x704
                                                                                                  ) ls705))]))
     | cvtEXPRESSION (LetExpr{defs=x716, body=x717, head=opt719}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x716), ("body", cvtEXPRESSION x717), 
          ("head", 
       (case opt719 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x718 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x718))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x732, actuals=ls734}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x732), ("actuals", PrettyRep.List (List.map (fn x733 => 
                                                                                                        cvtEXPRESSION x733
                                                                                                 ) ls734))]))
     | cvtEXPRESSION (SetExpr(x745, x746, x747)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x745, cvtEXPRESSION x746, cvtEXPRESSION x747]))
     | cvtEXPRESSION (ListExpr ls752) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x751 => 
                                                                                                          cvtEXPRESSION x751
                                                                                                   ) ls752)))
     | cvtEXPRESSION (InitExpr(x758, x759, x760)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x758, cvtHEAD x759, cvtINITS x760]))
     | cvtEXPRESSION (GetTemp n764) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n764))
     | cvtEXPRESSION (GetParam n767) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n767))
     | cvtEXPRESSION (Comprehension(x770, ls772, opt777)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x770, PrettyRep.List (List.map (fn x771 => 
                                                                                     cvtFOR_ENUM_HEAD x771
                                                                              ) ls772), 
          
       (case opt777 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x776 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x776))
       )]))
     | cvtEXPRESSION (LiteralExpr x784) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x784))
     | cvtEXPRESSION (LexicalReference{name=x787, loc=opt789}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x787), ("loc", 
       (case opt789 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x788 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x788))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x800, name=x801, loc=opt803}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x800), ("name", cvtNAME_EXPRESSION x801), ("loc", 
       (case opt803 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x802 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x802))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x816, index=x817, loc=opt819}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x816), ("index", cvtEXPRESSION x817), ("loc", 
       (case opt819 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x818 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x818))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n837) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n837))
     | cvtFIXTURE_NAME (PropName x840) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x840))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r845) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r845))
     | cvtLITERAL (LiteralDecimal d848) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d848))
     | cvtLITERAL (LiteralBoolean b851) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b851))
     | cvtLITERAL (LiteralString s854) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s854))
     | cvtLITERAL (LiteralArray{exprs=x857, ty=opt859}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x857), ("ty", 
       (case opt859 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x858 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x858))
       ))]))
     | cvtLITERAL (LiteralXML ls871) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x870 => 
                                                                                                           cvtEXPRESSION x870
                                                                                                    ) ls871)))
     | cvtLITERAL (LiteralNamespace x877) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x877))
     | cvtLITERAL (LiteralObject{expr=ls881, ty=opt886}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x880 => 
                                                                        cvtFIELD x880
                                                                 ) ls881)), 
          ("ty", 
       (case opt886 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x885 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x885))
       ))]))
     | cvtLITERAL (LiteralFunction x897) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x897))
     | cvtLITERAL (LiteralRegExp{str=s900}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s900)]))
   and cvtBLOCK (Block x906) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x906))
   and cvtFIXTURE (NamespaceFixture x909) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x909))
     | cvtFIXTURE (ClassFixture x912) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x912))
     | cvtFIXTURE (InterfaceFixture x915) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x915))
     | cvtFIXTURE (TypeVarFixture x918) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x918))
     | cvtFIXTURE (TypeFixture(ls922, x926)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x921 => cvtIDENTIFIER x921
                                                          ) ls922), cvtTYPE x926]))
     | cvtFIXTURE (MethodFixture{func=x930, ty=x931, readOnly=b932, override=b933, 
          final=b934}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x930), ("ty", cvtTYPE x931), ("readOnly", PrettyRep.Bool b932), 
          ("override", PrettyRep.Bool b933), ("final", PrettyRep.Bool b934)]))
     | cvtFIXTURE (ValFixture{ty=x948, readOnly=b949}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x948), ("readOnly", PrettyRep.Bool b949)]))
     | cvtFIXTURE (VirtualValFixture{ty=x957, getter=opt959, setter=opt964}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x957), ("getter", 
       (case opt959 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x958 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x958))
       )), ("setter", 
       (case opt964 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x963 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x963))
       ))]))
   and cvtHEAD (Head(x977, x978)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x977, 
          cvtINITS x978]))
   and cvtBINDINGS (ls983, ls988) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x982 => 
                                                                                     cvtBINDING x982
                                                                              ) ls983), 
          PrettyRep.List (List.map (fn x987 => cvtINIT_STEP x987
                                   ) ls988)]
   and cvtRIB ls996 = PrettyRep.List (List.map (fn (x993, x994) => PrettyRep.Tuple [cvtFIXTURE_NAME x993, 
                                                      cvtFIXTURE x994]
                                               ) ls996)
   and cvtRIBS ls1007 = PrettyRep.List (List.map (fn ls1003 => PrettyRep.List (List.map (fn (x1000, 
                                                                                               x1001) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1000, 
                                                                                               cvtFIXTURE x1001]
                                                                                        ) ls1003)
                                                 ) ls1007)
   and cvtINITS ls1014 = PrettyRep.List (List.map (fn (x1011, x1012) => PrettyRep.Tuple [cvtFIXTURE_NAME x1011, 
                                                         cvtEXPRESSION x1012]
                                                  ) ls1014)
   and cvtINSTANCE_TYPE {name=x1018, typeParams=ls1020, typeArgs=ls1025, nonnullable=b1029, 
          superTypes=ls1031, ty=x1035, dynamic=b1036} = PrettyRep.Rec [("name", 
          cvtNAME x1018), ("typeParams", PrettyRep.List (List.map (fn x1019 => 
                                                                         cvtIDENTIFIER x1019
                                                                  ) ls1020)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1024 => cvtTYPE x1024
                                                ) ls1025)), ("nonnullable", 
          PrettyRep.Bool b1029), ("superTypes", PrettyRep.List (List.map (fn x1030 => 
                                                                                cvtTYPE x1030
                                                                         ) ls1031)), 
          ("ty", cvtTYPE x1035), ("dynamic", PrettyRep.Bool b1036)]
   and cvtFIELD {kind=x1052, name=x1053, init=x1054} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1052), ("name", cvtNAME_EXPRESSION x1053), ("init", 
          cvtEXPRESSION x1054)]
   and cvtFIELD_TYPE {name=x1062, ty=x1063} = PrettyRep.Rec [("name", cvtNAME_EXPRESSION x1062), 
          ("ty", cvtTYPE x1063)]
   and cvtFUNC_TYPE {typeParams=ls1070, thisType=x1074, params=ls1076, hasRest=b1080, 
          minArgs=n1081, result=opt1083} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1069 => 
                                                                                                         cvtIDENTIFIER x1069
                                                                                                  ) ls1070)), 
          ("thisType", cvtTYPE x1074), ("params", PrettyRep.List (List.map (fn x1075 => 
                                                                                  cvtTYPE x1075
                                                                           ) ls1076)), 
          ("hasRest", PrettyRep.Bool b1080), ("minArgs", PrettyRep.Int n1081), 
          ("result", 
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

