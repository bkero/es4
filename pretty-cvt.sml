structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtBOOLEAN b19 = PrettyRep.Bool b19
   and cvtIEEE_754_BINARY_64_BIT r20 = PrettyRep.Real64 r20
   and cvtIEEE_754R_DECIMAL_128_BIT d21 = PrettyRep.Dec d21
   and cvtSTRING s22 = PrettyRep.UniStr s22
   and cvtIDENTIFIER x23 = cvtSTRING x23
   and cvtNONCE n24 = PrettyRep.Int n24
   and cvtOPAQUE_NAMESPACE_IDENTIFIER x25 = cvtNONCE x25
   and cvtNAMESPACE (TransparentNamespace x26) = PrettyRep.Ctor ("TransparentNamespace", 
          SOME (cvtSTRING x26))
     | cvtNAMESPACE (OpaqueNamespace x29) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtOPAQUE_NAMESPACE_IDENTIFIER x29))
   and cvtNAME {ns=x32, id=x33} = PrettyRep.Rec [("ns", cvtNAMESPACE x32), 
          ("id", cvtIDENTIFIER x33)]
   and cvtNAMESPACE_SET ls40 = PrettyRep.List (List.map (fn x39 => cvtNAMESPACE x39
                                                        ) ls40)
   and cvtOPEN_NAMESPACES ls45 = PrettyRep.List (List.map (fn x44 => cvtNAMESPACE_SET x44
                                                          ) ls45)
   and cvtNAME_SET ls50 = PrettyRep.List (List.map (fn x49 => cvtNAME x49
                                                   ) ls50)
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
   and cvtPRAGMA (UseNamespace x112) = PrettyRep.Ctor ("UseNamespace", SOME (cvtNAMESPACE_EXPRESSION x112))
     | cvtPRAGMA (UseDefaultNamespace x115) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtNAMESPACE_EXPRESSION x115))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtNAME_EXPRESSION (QualifiedName{namespace=x120, identifier=x121}) = 
          PrettyRep.Ctor ("QualifiedName", SOME (PrettyRep.Rec [("namespace", 
          cvtNAMESPACE_EXPRESSION x120), ("identifier", cvtIDENTIFIER x121)]))
     | cvtNAME_EXPRESSION (UnqualifiedName{identifier=x129, openNamespaces=x130}) = 
          PrettyRep.Ctor ("UnqualifiedName", SOME (PrettyRep.Rec [("identifier", 
          cvtIDENTIFIER x129), ("openNamespaces", cvtOPEN_NAMESPACES x130)]))
   and cvtNAMESPACE_EXPRESSION (Namespace x138) = PrettyRep.Ctor ("Namespace", 
          SOME (cvtNAMESPACE x138))
     | cvtNAMESPACE_EXPRESSION (NamespaceName x141) = PrettyRep.Ctor ("NamespaceName", 
          SOME (cvtNAME_EXPRESSION x141))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLASS (Class{name=x150, privateNS=x151, protectedNS=x152, parentProtectedNSs=ls154, 
          typeParams=ls159, nonnullable=x163, dynamic=x164, extends=opt166, 
          implements=ls171, classFixtureMap=x175, instanceFixtureMap=x176, 
          instanceInits=x177, constructor=opt179}) = PrettyRep.Ctor ("Class", 
          SOME (PrettyRep.Rec [("name", cvtNAME x150), ("privateNS", cvtNAMESPACE x151), 
          ("protectedNS", cvtNAMESPACE x152), ("parentProtectedNSs", PrettyRep.List (List.map (fn x153 => 
                                                                                                     cvtNAMESPACE x153
                                                                                              ) ls154)), 
          ("typeParams", PrettyRep.List (List.map (fn x158 => cvtIDENTIFIER x158
                                                  ) ls159)), ("nonnullable", 
          cvtBOOLEAN x163), ("dynamic", cvtBOOLEAN x164), ("extends", 
       (case opt166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x165 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x165))
       )), ("implements", PrettyRep.List (List.map (fn x170 => cvtTYPE x170
                                                   ) ls171)), ("classFixtureMap", 
          cvtFIXTURE_MAP x175), ("instanceFixtureMap", cvtFIXTURE_MAP x176), 
          ("instanceInits", cvtHEAD x177), ("constructor", 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x178))
       ))]))
   and cvtINTERFACE (Interface{name=x212, typeParams=ls214, nonnullable=x218, 
          extends=ls220, instanceFixtureMap=x224}) = PrettyRep.Ctor ("Interface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x212), ("typeParams", PrettyRep.List (List.map (fn x213 => 
                                                                                                      cvtIDENTIFIER x213
                                                                                               ) ls214)), 
          ("nonnullable", cvtBOOLEAN x218), ("extends", PrettyRep.List (List.map (fn x219 => 
                                                                                        cvtTYPE x219
                                                                                 ) ls220)), 
          ("instanceFixtureMap", cvtFIXTURE_MAP x224)]))
   and cvtCTOR (Ctor{settings=x238, superArgs=ls240, func=x244}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x238), ("superArgs", PrettyRep.List (List.map (fn x239 => 
                                                                                                         cvtEXPRESSION x239
                                                                                                  ) ls240)), 
          ("func", cvtFUNC x244)]))
   and cvtFUNC (Func{name=x254, fsig=x255, native=x256, generator=x257, block=opt259, 
          param=x263, defaults=ls265, ty=x269, loc=opt271}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x254), ("fsig", cvtFUNC_SIG x255), 
          ("native", cvtBOOLEAN x256), ("generator", cvtBOOLEAN x257), ("block", 
          
       (case opt259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x258 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x258))
       )), ("param", cvtHEAD x263), ("defaults", PrettyRep.List (List.map (fn x264 => 
                                                                                 cvtEXPRESSION x264
                                                                          ) ls265)), 
          ("ty", cvtTYPE x269), ("loc", 
       (case opt271 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x270 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x270))
       ))]))
   and cvtDEFN (ClassDefn x296) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x296))
     | cvtDEFN (VariableDefn x299) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x299))
     | cvtDEFN (FunctionDefn x302) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x302))
     | cvtDEFN (ConstructorDefn x305) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x305))
     | cvtDEFN (InterfaceDefn x308) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x308))
     | cvtDEFN (NamespaceDefn x311) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x311))
     | cvtDEFN (TypeDefn x314) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x314))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls318, params=x322, paramTypes=ls324, 
          defaults=ls329, ctorInits=opt340, returnType=opt345, thisType=opt350, 
          hasRest=x354}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x317 => cvtIDENTIFIER x317
                                   ) ls318)), ("params", cvtBINDINGS x322), 
          ("paramTypes", PrettyRep.List (List.map (fn x323 => cvtTYPE x323
                                                  ) ls324)), ("defaults", PrettyRep.List (List.map (fn x328 => 
                                                                                                          cvtEXPRESSION x328
                                                                                                   ) ls329)), 
          ("ctorInits", 
       (case opt340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x333, ls335) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x333, 
            PrettyRep.List (List.map (fn x334 => cvtEXPRESSION x334
                                     ) ls335)]))
       )), ("returnType", 
       (case opt345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x344 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x344))
       )), ("thisType", 
       (case opt350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x349 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x349))
       )), ("hasRest", cvtBOOLEAN x354)]))
   and cvtBINDING (Binding{ident=x374, ty=x375}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x374), ("ty", 
          cvtTYPE x375)]))
   and cvtBINDING_IDENTIFIER (TempIdent n383) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n383))
     | cvtBINDING_IDENTIFIER (ParamIdent n386) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n386))
     | cvtBINDING_IDENTIFIER (PropIdent x389) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x389))
   and cvtINIT_STEP (InitStep(x392, x393)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x392, 
          cvtEXPRESSION x393]))
     | cvtINIT_STEP (AssignStep(x397, x398)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x397, cvtEXPRESSION x398]))
   and cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (RecordType ls408) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn (x405, 
                                                                                                        x406) => 
                                                                                                        PrettyRep.Tuple [cvtNAME_EXPRESSION x405, 
                                                                                                        cvtTYPE x406]
                                                                                                 ) ls408)))
     | cvtTYPE (ArrayType(ls415, opt420)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x414 => 
                                                                                                                                cvtTYPE x414
                                                                                                                         ) ls415), 
          
       (case opt420 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x419 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x419))
       )]))
     | cvtTYPE (UnionType ls428) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x427 => 
                                                                                                      cvtTYPE x427
                                                                                               ) ls428)))
     | cvtTYPE (FunctionType x434) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x434))
     | cvtTYPE (NonNullType x437) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x437))
     | cvtTYPE (AppType(x440, ls442)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x440, 
          PrettyRep.List (List.map (fn x441 => cvtTYPE x441
                                   ) ls442)]))
     | cvtTYPE (TypeName(x449, opt451)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x449, 
          
       (case opt451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x450 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x450))
       )]))
     | cvtTYPE (ClassType x458) = PrettyRep.Ctor ("ClassType", SOME (cvtCLASS x458))
     | cvtTYPE (InstanceType x461) = PrettyRep.Ctor ("InstanceType", SOME (cvtCLASS x461))
     | cvtTYPE (InterfaceType x464) = PrettyRep.Ctor ("InterfaceType", SOME (cvtINTERFACE x464))
     | cvtTYPE (TypeNameReferenceType(x467, x468)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x467, cvtNAME_EXPRESSION x468]))
     | cvtTYPE (TypeIndexReferenceType(x472, n473)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x472, PrettyRep.Int n473]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x478) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x478))
     | cvtSTATEMENT (InitStmt{kind=x481, ns=opt483, prototype=x487, static=x488, 
          temps=x489, inits=ls491}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x481), ("ns", 
       (case opt483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x482 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x482))
       )), ("prototype", cvtBOOLEAN x487), ("static", cvtBOOLEAN x488), ("temps", 
          cvtBINDINGS x489), ("inits", PrettyRep.List (List.map (fn x490 => 
                                                                       cvtINIT_STEP x490
                                                                ) ls491))]))
     | cvtSTATEMENT (ClassBlock x510) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x510))
     | cvtSTATEMENT (ForInStmt x513) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x513))
     | cvtSTATEMENT (ThrowStmt x516) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x516))
     | cvtSTATEMENT (ReturnStmt x519) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x519))
     | cvtSTATEMENT (BreakStmt opt523) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x522 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x522))
       ))
     | cvtSTATEMENT (ContinueStmt opt530) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x529 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x529))
       ))
     | cvtSTATEMENT (BlockStmt x536) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x536))
     | cvtSTATEMENT (LabeledStmt(x539, x540)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x539, cvtSTATEMENT x540]))
     | cvtSTATEMENT (LetStmt x544) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x544))
     | cvtSTATEMENT (WhileStmt x547) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x547))
     | cvtSTATEMENT (DoWhileStmt x550) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x550))
     | cvtSTATEMENT (ForStmt x553) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x553))
     | cvtSTATEMENT (IfStmt{cnd=x556, thn=x557, els=x558}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x556), ("thn", cvtSTATEMENT x557), 
          ("els", cvtSTATEMENT x558)]))
     | cvtSTATEMENT (WithStmt{obj=x568, ty=x569, body=x570}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x568), ("ty", cvtTYPE x569), 
          ("body", cvtSTATEMENT x570)]))
     | cvtSTATEMENT (TryStmt{block=x580, catches=ls582, finally=opt587}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x580), 
          ("catches", PrettyRep.List (List.map (fn x581 => cvtCATCH_CLAUSE x581
                                               ) ls582)), ("finally", 
       (case opt587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x586 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x586))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x600, labels=ls602, cases=ls607}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x600), ("labels", PrettyRep.List (List.map (fn x601 => 
                                                                                                        cvtIDENTIFIER x601
                                                                                                 ) ls602)), 
          ("cases", PrettyRep.List (List.map (fn x606 => cvtCASE x606
                                             ) ls607))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x620, ty=x621, cases=ls623}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x620), ("ty", cvtTYPE x621), 
          ("cases", PrettyRep.List (List.map (fn x622 => cvtCATCH_CLAUSE x622
                                             ) ls623))]))
     | cvtSTATEMENT (DXNStmt{expr=x636}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x636)]))
   and cvtEXPRESSION (ApplyTypeExpression{expr=x642, actuals=ls644}) = PrettyRep.Ctor ("ApplyTypeExpression", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x642), ("actuals", PrettyRep.List (List.map (fn x643 => 
                                                                                                         cvtTYPE x643
                                                                                                  ) ls644))]))
     | cvtEXPRESSION (BinaryExpr(x655, x656, x657)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x655, cvtEXPRESSION x656, cvtEXPRESSION x657]))
     | cvtEXPRESSION (BinaryTypeExpr(x661, x662, x663)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x661, cvtEXPRESSION x662, cvtTYPE x663]))
     | cvtEXPRESSION (CallExpr{func=x667, actuals=ls669}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x667), ("actuals", PrettyRep.List (List.map (fn x668 => 
                                                                                                         cvtEXPRESSION x668
                                                                                                  ) ls669))]))
     | cvtEXPRESSION (Comprehension(x680, ls682, opt687)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x680, PrettyRep.List (List.map (fn x681 => 
                                                                                     cvtFOR_ENUM_HEAD x681
                                                                              ) ls682), 
          
       (case opt687 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x686 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x686))
       )]))
     | cvtEXPRESSION (ConditionalExpression(x694, x695, x696)) = PrettyRep.Ctor ("ConditionalExpression", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x694, cvtEXPRESSION x695, cvtEXPRESSION x696]))
     | cvtEXPRESSION (GetParam n700) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n700))
     | cvtEXPRESSION (GetTemp n703) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n703))
     | cvtEXPRESSION (InitExpr(x706, x707, x708)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x706, cvtHEAD x707, cvtINITS x708]))
     | cvtEXPRESSION (LetExpr{defs=x712, body=x713, head=opt715}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x712), ("body", cvtEXPRESSION x713), 
          ("head", 
       (case opt715 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x714 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x714))
       ))]))
     | cvtEXPRESSION (ListExpr ls729) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x728 => 
                                                                                                          cvtEXPRESSION x728
                                                                                                   ) ls729)))
     | cvtEXPRESSION (LexicalReference{name=x735, loc=opt737}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x735), ("loc", 
       (case opt737 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x736 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x736))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x748, actuals=ls750}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x748), ("actuals", PrettyRep.List (List.map (fn x749 => 
                                                                                                        cvtEXPRESSION x749
                                                                                                 ) ls750))]))
     | cvtEXPRESSION (ObjectNameReference{object=x761, name=x762, loc=opt764}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x761), ("name", cvtNAME_EXPRESSION x762), ("loc", 
       (case opt764 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x763 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x763))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x777, index=x778, loc=opt780}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x777), ("index", cvtEXPRESSION x778), ("loc", 
       (case opt780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x779 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x779))
       ))]))
     | cvtEXPRESSION (SetExpr(x793, x794, x795)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x793, cvtEXPRESSION x794, cvtEXPRESSION x795]))
     | cvtEXPRESSION (SuperExpr opt800) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt800 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x799 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x799))
       ))
     | cvtEXPRESSION (TypeExpr x806) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x806))
     | cvtEXPRESSION (ThisExpr opt810) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x809))
       ))
     | cvtEXPRESSION (UnaryExpr(x816, x817)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x816, cvtEXPRESSION x817]))
     | cvtEXPRESSION (YieldExpr opt822) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt822 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x821 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x821))
       ))
     | cvtEXPRESSION (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtEXPRESSION (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtEXPRESSION (LiteralDouble x830) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (cvtIEEE_754_BINARY_64_BIT x830))
     | cvtEXPRESSION (LiteralDecimal x833) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (cvtIEEE_754R_DECIMAL_128_BIT x833))
     | cvtEXPRESSION (LiteralBoolean x836) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (cvtBOOLEAN x836))
     | cvtEXPRESSION (LiteralString x839) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtSTRING x839))
     | cvtEXPRESSION (LiteralArray{exprs=x842, ty=opt844}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x842), ("ty", 
       (case opt844 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x843 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x843))
       ))]))
     | cvtEXPRESSION (LiteralNamespace x855) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x855))
     | cvtEXPRESSION (LiteralObject{expr=ls859, ty=opt864}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x858 => 
                                                                        cvtFIELD x858
                                                                 ) ls859)), 
          ("ty", 
       (case opt864 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x863 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x863))
       ))]))
     | cvtEXPRESSION (LiteralFunction x875) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x875))
     | cvtEXPRESSION (LiteralRegExp{str=x878}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtSTRING x878)]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n889) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n889))
     | cvtFIXTURE_NAME (PropName x892) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x892))
   and cvtBLOCK (Block x895) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x895))
   and cvtFIXTURE (NamespaceFixture x898) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x898))
     | cvtFIXTURE (ClassFixture x901) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS x901))
     | cvtFIXTURE (InterfaceFixture x904) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtINTERFACE x904))
     | cvtFIXTURE (TypeVarFixture x907) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x907))
     | cvtFIXTURE (TypeFixture(ls911, x915)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x910 => cvtIDENTIFIER x910
                                                          ) ls911), cvtTYPE x915]))
     | cvtFIXTURE (MethodFixture{func=x919, ty=x920, writable=x921, override=x922, 
          final=x923, inheritedFrom=opt925}) = PrettyRep.Ctor ("MethodFixture", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x919), ("ty", cvtTYPE x920), 
          ("writable", cvtBOOLEAN x921), ("override", cvtBOOLEAN x922), ("final", 
          cvtBOOLEAN x923), ("inheritedFrom", 
       (case opt925 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x924 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x924))
       ))]))
     | cvtFIXTURE (ValFixture{ty=x944, writable=x945}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x944), ("writable", cvtBOOLEAN x945)]))
     | cvtFIXTURE (VirtualValFixture{ty=x953, getter=opt961, setter=opt972}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x953), ("getter", 
       (case opt961 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x954, opt956) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x954, 
            
         (case opt956 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x955 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x955))
         )]))
       )), ("setter", 
       (case opt972 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x965, opt967) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x965, 
            
         (case opt967 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x966 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x966))
         )]))
       ))]))
   and cvtHEAD (Head(x985, x986)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtFIXTURE_MAP x985, 
          cvtINITS x986]))
   and cvtBINDINGS (ls991, ls996) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x990 => 
                                                                                     cvtBINDING x990
                                                                              ) ls991), 
          PrettyRep.List (List.map (fn x995 => cvtINIT_STEP x995
                                   ) ls996)]
   and cvtFIXTURE_MAP ls1004 = PrettyRep.List (List.map (fn (x1001, x1002) => 
                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1001, 
                                                               cvtFIXTURE x1002]
                                                        ) ls1004)
   and cvtFIXTURE_MAPS ls1015 = PrettyRep.List (List.map (fn ls1011 => PrettyRep.List (List.map (fn (x1008, 
                                                                                                       x1009) => 
                                                                                                       PrettyRep.Tuple [cvtFIXTURE_NAME x1008, 
                                                                                                       cvtFIXTURE x1009]
                                                                                                ) ls1011)
                                                         ) ls1015)
   and cvtINITS ls1022 = PrettyRep.List (List.map (fn (x1019, x1020) => PrettyRep.Tuple [cvtFIXTURE_NAME x1019, 
                                                         cvtEXPRESSION x1020]
                                                  ) ls1022)
   and cvtINSTANCE_TYPE {name=x1026, typeArgs=ls1028, nonnullable=x1032, typeParams=ls1034, 
          superTypes=ls1039, ty=x1043, dynamic=x1044} = PrettyRep.Rec [("name", 
          cvtNAME x1026), ("typeArgs", PrettyRep.List (List.map (fn x1027 => 
                                                                       cvtTYPE x1027
                                                                ) ls1028)), 
          ("nonnullable", cvtBOOLEAN x1032), ("typeParams", PrettyRep.List (List.map (fn x1033 => 
                                                                                            cvtIDENTIFIER x1033
                                                                                     ) ls1034)), 
          ("superTypes", PrettyRep.List (List.map (fn x1038 => cvtTYPE x1038
                                                  ) ls1039)), ("ty", cvtTYPE x1043), 
          ("dynamic", cvtBOOLEAN x1044)]
   and cvtFIELD {kind=x1060, name=x1061, init=x1062} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1060), ("name", cvtNAME_EXPRESSION x1061), ("init", 
          cvtEXPRESSION x1062)]
   and cvtFIELD_TYPE (x1070, x1071) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1070, 
          cvtTYPE x1071]
   and cvtFUNCTION_TYPE {typeParams=ls1074, thisType=x1078, params=ls1080, 
          minArgs=n1084, hasRest=x1085, result=opt1087} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1073 => cvtIDENTIFIER x1073
                                   ) ls1074)), ("thisType", cvtTYPE x1078), 
          ("params", PrettyRep.List (List.map (fn x1079 => cvtTYPE x1079
                                              ) ls1080)), ("minArgs", PrettyRep.Int n1084), 
          ("hasRest", cvtBOOLEAN x1085), ("result", 
       (case opt1087 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1086 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1086))
       ))]
   and cvtFUNC_DEFN {kind=x1104, ns=opt1106, final=x1110, override=x1111, prototype=x1112, 
          static=x1113, func=x1114} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1104), 
          ("ns", 
       (case opt1106 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1105 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1105))
       )), ("final", cvtBOOLEAN x1110), ("override", cvtBOOLEAN x1111), ("prototype", 
          cvtBOOLEAN x1112), ("static", cvtBOOLEAN x1113), ("func", cvtFUNC x1114)]
   and cvtCTOR_DEFN x1130 = cvtCTOR x1130
   and cvtVAR_DEFN {kind=x1131, ns=opt1133, static=x1137, prototype=x1138, 
          bindings=(ls1140, ls1145)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1131), 
          ("ns", 
       (case opt1133 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1132 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1132))
       )), ("static", cvtBOOLEAN x1137), ("prototype", cvtBOOLEAN x1138), ("bindings", 
          PrettyRep.Tuple [PrettyRep.List (List.map (fn x1139 => cvtBINDING x1139
                                                    ) ls1140), PrettyRep.List (List.map (fn x1144 => 
                                                                                               cvtINIT_STEP x1144
                                                                                        ) ls1145)])]
   and cvtNAMESPACE_DEFN {ident=x1161, ns=opt1163, init=opt1168} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1161), ("ns", 
       (case opt1163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1162 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1162))
       )), ("init", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1167))
       ))]
   and cvtCLASS_DEFN {ns=opt1180, privateNS=x1184, protectedNS=x1185, ident=x1186, 
          nonnullable=x1187, dynamic=x1188, final=x1189, params=ls1191, extends=opt1196, 
          implements=ls1201, classDefns=ls1206, instanceDefns=ls1211, instanceStmts=ls1216, 
          ctorDefn=opt1221} = PrettyRep.Rec [("ns", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1179))
       )), ("privateNS", cvtNAMESPACE x1184), ("protectedNS", cvtNAMESPACE x1185), 
          ("ident", cvtIDENTIFIER x1186), ("nonnullable", cvtBOOLEAN x1187), 
          ("dynamic", cvtBOOLEAN x1188), ("final", cvtBOOLEAN x1189), ("params", 
          PrettyRep.List (List.map (fn x1190 => cvtIDENTIFIER x1190
                                   ) ls1191)), ("extends", 
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1195))
       )), ("implements", PrettyRep.List (List.map (fn x1200 => cvtTYPE x1200
                                                   ) ls1201)), ("classDefns", 
          PrettyRep.List (List.map (fn x1205 => cvtDEFN x1205
                                   ) ls1206)), ("instanceDefns", PrettyRep.List (List.map (fn x1210 => 
                                                                                                 cvtDEFN x1210
                                                                                          ) ls1211)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1215 => cvtSTATEMENT x1215
                                                     ) ls1216)), ("ctorDefn", 
          
       (case opt1221 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1220 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1220))
       ))]
   and cvtINTERFACE_DEFN {ident=x1254, ns=opt1256, nonnullable=x1260, params=ls1262, 
          extends=ls1267, instanceDefns=ls1272} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1254), ("ns", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1255))
       )), ("nonnullable", cvtBOOLEAN x1260), ("params", PrettyRep.List (List.map (fn x1261 => 
                                                                                         cvtIDENTIFIER x1261
                                                                                  ) ls1262)), 
          ("extends", PrettyRep.List (List.map (fn x1266 => cvtTYPE x1266
                                               ) ls1267)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1271 => cvtDEFN x1271
                                   ) ls1272))]
   and cvtTYPE_DEFN {ident=x1289, ns=opt1291, typeParams=ls1296, init=x1300} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1289), ("ns", 
       (case opt1291 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1290 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1290))
       )), ("typeParams", PrettyRep.List (List.map (fn x1295 => cvtIDENTIFIER x1295
                                                   ) ls1296)), ("init", cvtTYPE x1300)]
   and cvtCLASS_BLOCK {ns=opt1311, protectedNS=x1315, privateNS=x1316, ident=x1317, 
          name=opt1319, block=x1323} = PrettyRep.Rec [("ns", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1310))
       )), ("protectedNS", cvtNAMESPACE x1315), ("privateNS", cvtNAMESPACE x1316), 
          ("ident", cvtIDENTIFIER x1317), ("name", 
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1318))
       )), ("block", cvtBLOCK x1323)]
   and cvtFOR_ENUM_HEAD {isEach=x1337, bindings=(ls1339, ls1344), expr=x1349} = 
          PrettyRep.Rec [("isEach", cvtBOOLEAN x1337), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1338 => 
                                                                                                                     cvtBINDING x1338
                                                                                                              ) ls1339), 
          PrettyRep.List (List.map (fn x1343 => cvtINIT_STEP x1343
                                   ) ls1344)]), ("expr", cvtEXPRESSION x1349)]
   and cvtFOR_ENUM_STATEMENT {isEach=x1357, defn=opt1388, obj=x1392, fixtureMap=opt1400, 
          next=x1404, labels=ls1406, body=x1410} = PrettyRep.Rec [("isEach", 
          cvtBOOLEAN x1357), ("defn", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1358, ns=opt1360, static=x1364, prototype=x1365, bindings=(ls1367, 
            ls1372)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1358), ("ns", 
         (case opt1360 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1359))
         )), ("static", cvtBOOLEAN x1364), ("prototype", cvtBOOLEAN x1365), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1366 => 
                                                                          cvtBINDING x1366
                                                                   ) ls1367), 
            PrettyRep.List (List.map (fn x1371 => cvtINIT_STEP x1371
                                     ) ls1372)])]))
       )), ("obj", cvtEXPRESSION x1392), ("fixtureMap", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1396 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1393, 
                                                                                      x1394) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1393, 
                                                                                      cvtFIXTURE x1394]
                                                                               ) ls1396)))
       )), ("next", cvtSTATEMENT x1404), ("labels", PrettyRep.List (List.map (fn x1405 => 
                                                                                    cvtIDENTIFIER x1405
                                                                             ) ls1406)), 
          ("body", cvtSTATEMENT x1410)]
   and cvtFOR_STATEMENT {fixtureMap=opt1433, defn=opt1467, init=ls1472, cond=x1476, 
          update=x1477, labels=ls1479, body=x1483} = PrettyRep.Rec [("fixtureMap", 
          
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1429 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1426, 
                                                                                      x1427) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1426, 
                                                                                      cvtFIXTURE x1427]
                                                                               ) ls1429)))
       )), ("defn", 
       (case opt1467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1437, ns=opt1439, static=x1443, prototype=x1444, bindings=(ls1446, 
            ls1451)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1437), ("ns", 
         (case opt1439 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1438))
         )), ("static", cvtBOOLEAN x1443), ("prototype", cvtBOOLEAN x1444), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1445 => 
                                                                          cvtBINDING x1445
                                                                   ) ls1446), 
            PrettyRep.List (List.map (fn x1450 => cvtINIT_STEP x1450
                                     ) ls1451)])]))
       )), ("init", PrettyRep.List (List.map (fn x1471 => cvtSTATEMENT x1471
                                             ) ls1472)), ("cond", cvtEXPRESSION x1476), 
          ("update", cvtEXPRESSION x1477), ("labels", PrettyRep.List (List.map (fn x1478 => 
                                                                                      cvtIDENTIFIER x1478
                                                                               ) ls1479)), 
          ("body", cvtSTATEMENT x1483)]
   and cvtWHILE_STATEMENT {cond=x1499, fixtureMap=opt1507, body=x1511, labels=ls1513} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1499), ("fixtureMap", 
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1503 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1500, 
                                                                                      x1501) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1500, 
                                                                                      cvtFIXTURE x1501]
                                                                               ) ls1503)))
       )), ("body", cvtSTATEMENT x1511), ("labels", PrettyRep.List (List.map (fn x1512 => 
                                                                                    cvtIDENTIFIER x1512
                                                                             ) ls1513))]
   and cvtDIRECTIVES {pragmas=ls1527, defns=ls1532, head=opt1537, body=ls1542, 
          loc=opt1547} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1526 => 
                                                                                    cvtPRAGMA x1526
                                                                             ) ls1527)), 
          ("defns", PrettyRep.List (List.map (fn x1531 => cvtDEFN x1531
                                             ) ls1532)), ("head", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1536))
       )), ("body", PrettyRep.List (List.map (fn x1541 => cvtSTATEMENT x1541
                                             ) ls1542)), ("loc", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1546))
       ))]
   and cvtCASE {label=opt1563, inits=opt1574, body=x1578} = PrettyRep.Rec [("label", 
          
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1562))
       )), ("inits", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1570 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1567, 
                                                                                      x1568) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1567, 
                                                                                      cvtEXPRESSION x1568]
                                                                               ) ls1570)))
       )), ("body", cvtBLOCK x1578)]
   and cvtCATCH_CLAUSE {bindings=(ls1587, ls1592), ty=x1597, fixtureMap=opt1605, 
          inits=opt1616, block=x1620} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1586 => 
                                                                                                                     cvtBINDING x1586
                                                                                                              ) ls1587), 
          PrettyRep.List (List.map (fn x1591 => cvtINIT_STEP x1591
                                   ) ls1592)]), ("ty", cvtTYPE x1597), ("fixtureMap", 
          
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1601 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1598, 
                                                                                      x1599) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1598, 
                                                                                      cvtFIXTURE x1599]
                                                                               ) ls1601)))
       )), ("inits", 
       (case opt1616 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1612 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1609, 
                                                                                      x1610) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1609, 
                                                                                      cvtEXPRESSION x1610]
                                                                               ) ls1612)))
       )), ("block", cvtBLOCK x1620)]
   and cvtFUNC_NAME {kind=x1632, ident=x1633} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1632), 
          ("ident", cvtIDENTIFIER x1633)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1639, getter=opt1647, setter=opt1658} = 
          PrettyRep.Rec [("ty", cvtTYPE x1639), ("getter", 
       (case opt1647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x1640, opt1642) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x1640, 
            
         (case opt1642 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1641 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x1641))
         )]))
       )), ("setter", 
       (case opt1658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x1651, opt1653) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x1651, 
            
         (case opt1653 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1652 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x1652))
         )]))
       ))]
   and cvtPROGRAM (Program x1669) = PrettyRep.Ctor ("Program", SOME (cvtBLOCK x1669))
end

