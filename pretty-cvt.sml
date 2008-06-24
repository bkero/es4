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
     | cvtNAME_EXPRESSION (UnqualifiedName{identifier=x125, openNamespaces=x126}) = 
          PrettyRep.Ctor ("UnqualifiedName", SOME (PrettyRep.Rec [("identifier", 
          cvtIDENTIFIER x125), ("openNamespaces", cvtOPEN_NAMESPACES x126)]))
   and cvtNAMESPACE_EXPRESSION (Namespace x134) = PrettyRep.Ctor ("Namespace", 
          SOME (cvtNAMESPACE x134))
     | cvtNAMESPACE_EXPRESSION (NamespaceName x137) = PrettyRep.Ctor ("NamespaceName", 
          SOME (cvtNAME_EXPRESSION x137))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLASS (Class{name=x146, privateNS=x147, protectedNS=x148, parentProtectedNSs=ls150, 
          typeParams=ls155, nonnullable=b159, dynamic=b160, extends=opt162, 
          implements=ls167, classFixtureMap=x171, instanceFixtureMap=x172, 
          instanceInits=x173, constructor=opt175}) = PrettyRep.Ctor ("Class", 
          SOME (PrettyRep.Rec [("name", cvtNAME x146), ("privateNS", cvtNAMESPACE x147), 
          ("protectedNS", cvtNAMESPACE x148), ("parentProtectedNSs", PrettyRep.List (List.map (fn x149 => 
                                                                                                     cvtNAMESPACE x149
                                                                                              ) ls150)), 
          ("typeParams", PrettyRep.List (List.map (fn x154 => cvtIDENTIFIER x154
                                                  ) ls155)), ("nonnullable", 
          PrettyRep.Bool b159), ("dynamic", PrettyRep.Bool b160), ("extends", 
          
       (case opt162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x161 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x161))
       )), ("implements", PrettyRep.List (List.map (fn x166 => cvtTYPE x166
                                                   ) ls167)), ("classFixtureMap", 
          cvtFIXTURE_MAP x171), ("instanceFixtureMap", cvtFIXTURE_MAP x172), 
          ("instanceInits", cvtHEAD x173), ("constructor", 
       (case opt175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x174 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x174))
       ))]))
   and cvtINTERFACE (Interface{name=x208, typeParams=ls210, nonnullable=b214, 
          extends=ls216, instanceFixtureMap=x220}) = PrettyRep.Ctor ("Interface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x208), ("typeParams", PrettyRep.List (List.map (fn x209 => 
                                                                                                      cvtIDENTIFIER x209
                                                                                               ) ls210)), 
          ("nonnullable", PrettyRep.Bool b214), ("extends", PrettyRep.List (List.map (fn x215 => 
                                                                                            cvtTYPE x215
                                                                                     ) ls216)), 
          ("instanceFixtureMap", cvtFIXTURE_MAP x220)]))
   and cvtCTOR (Ctor{settings=x234, superArgs=ls236, func=x240}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x234), ("superArgs", PrettyRep.List (List.map (fn x235 => 
                                                                                                         cvtEXPRESSION x235
                                                                                                  ) ls236)), 
          ("func", cvtFUNC x240)]))
   and cvtFUNC (Func{name=x250, fsig=x251, native=b252, generator=b253, block=opt255, 
          param=x259, defaults=ls261, ty=x265, loc=opt267}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x250), ("fsig", cvtFUNC_SIG x251), 
          ("native", PrettyRep.Bool b252), ("generator", PrettyRep.Bool b253), 
          ("block", 
       (case opt255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x254 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x254))
       )), ("param", cvtHEAD x259), ("defaults", PrettyRep.List (List.map (fn x260 => 
                                                                                 cvtEXPRESSION x260
                                                                          ) ls261)), 
          ("ty", cvtTYPE x265), ("loc", 
       (case opt267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x266 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x266))
       ))]))
   and cvtDEFN (ClassDefn x292) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x292))
     | cvtDEFN (VariableDefn x295) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x295))
     | cvtDEFN (FunctionDefn x298) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x298))
     | cvtDEFN (ConstructorDefn x301) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x301))
     | cvtDEFN (InterfaceDefn x304) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x304))
     | cvtDEFN (NamespaceDefn x307) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x307))
     | cvtDEFN (TypeDefn x310) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x310))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls314, params=x318, paramTypes=ls320, 
          defaults=ls325, ctorInits=opt336, returnType=opt341, thisType=opt346, 
          hasRest=b350}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x313 => cvtIDENTIFIER x313
                                   ) ls314)), ("params", cvtBINDINGS x318), 
          ("paramTypes", PrettyRep.List (List.map (fn x319 => cvtTYPE x319
                                                  ) ls320)), ("defaults", PrettyRep.List (List.map (fn x324 => 
                                                                                                          cvtEXPRESSION x324
                                                                                                   ) ls325)), 
          ("ctorInits", 
       (case opt336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x329, ls331) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x329, 
            PrettyRep.List (List.map (fn x330 => cvtEXPRESSION x330
                                     ) ls331)]))
       )), ("returnType", 
       (case opt341 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x340 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x340))
       )), ("thisType", 
       (case opt346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x345 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x345))
       )), ("hasRest", PrettyRep.Bool b350)]))
   and cvtBINDING (Binding{ident=x370, ty=x371}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x370), ("ty", 
          cvtTYPE x371)]))
   and cvtBINDING_IDENTIFIER (TempIdent n379) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n379))
     | cvtBINDING_IDENTIFIER (ParamIdent n382) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n382))
     | cvtBINDING_IDENTIFIER (PropIdent x385) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x385))
   and cvtINIT_STEP (InitStep(x388, x389)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x388, 
          cvtEXPRESSION x389]))
     | cvtINIT_STEP (AssignStep(x393, x394)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x393, cvtEXPRESSION x394]))
   and cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (RecordType ls404) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn (x401, 
                                                                                                        x402) => 
                                                                                                        PrettyRep.Tuple [cvtNAME_EXPRESSION x401, 
                                                                                                        cvtTYPE x402]
                                                                                                 ) ls404)))
     | cvtTYPE (ArrayType(ls411, opt416)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x410 => 
                                                                                                                                cvtTYPE x410
                                                                                                                         ) ls411), 
          
       (case opt416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x415 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x415))
       )]))
     | cvtTYPE (UnionType ls424) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                      cvtTYPE x423
                                                                                               ) ls424)))
     | cvtTYPE (FunctionType x430) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x430))
     | cvtTYPE (NonNullType x433) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x433))
     | cvtTYPE (AppType(x436, ls438)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x436, 
          PrettyRep.List (List.map (fn x437 => cvtTYPE x437
                                   ) ls438)]))
     | cvtTYPE (TypeName(x445, opt447)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x445, 
          
       (case opt447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x446 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x446))
       )]))
     | cvtTYPE (ClassType x454) = PrettyRep.Ctor ("ClassType", SOME (cvtCLASS x454))
     | cvtTYPE (InstanceType x457) = PrettyRep.Ctor ("InstanceType", SOME (cvtCLASS x457))
     | cvtTYPE (InterfaceType x460) = PrettyRep.Ctor ("InterfaceType", SOME (cvtINTERFACE x460))
     | cvtTYPE (TypeNameReferenceType(x463, x464)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x463, cvtNAME_EXPRESSION x464]))
     | cvtTYPE (TypeIndexReferenceType(x468, n469)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x468, PrettyRep.Int n469]))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x474) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x474))
     | cvtSTATEMENT (InitStmt{kind=x477, ns=opt479, prototype=b483, static=b484, 
          temps=x485, inits=ls487}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x477), ("ns", 
       (case opt479 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x478 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x478))
       )), ("prototype", PrettyRep.Bool b483), ("static", PrettyRep.Bool b484), 
          ("temps", cvtBINDINGS x485), ("inits", PrettyRep.List (List.map (fn x486 => 
                                                                                 cvtINIT_STEP x486
                                                                          ) ls487))]))
     | cvtSTATEMENT (ClassBlock x506) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x506))
     | cvtSTATEMENT (ForInStmt x509) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x509))
     | cvtSTATEMENT (ThrowStmt x512) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x512))
     | cvtSTATEMENT (ReturnStmt x515) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x515))
     | cvtSTATEMENT (BreakStmt opt519) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x518 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x518))
       ))
     | cvtSTATEMENT (ContinueStmt opt526) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt526 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x525 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x525))
       ))
     | cvtSTATEMENT (BlockStmt x532) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x532))
     | cvtSTATEMENT (LabeledStmt(x535, x536)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x535, cvtSTATEMENT x536]))
     | cvtSTATEMENT (LetStmt x540) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x540))
     | cvtSTATEMENT (WhileStmt x543) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x543))
     | cvtSTATEMENT (DoWhileStmt x546) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x546))
     | cvtSTATEMENT (ForStmt x549) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x549))
     | cvtSTATEMENT (IfStmt{cnd=x552, thn=x553, els=x554}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x552), ("thn", cvtSTATEMENT x553), 
          ("els", cvtSTATEMENT x554)]))
     | cvtSTATEMENT (WithStmt{obj=x564, ty=x565, body=x566}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x564), ("ty", cvtTYPE x565), 
          ("body", cvtSTATEMENT x566)]))
     | cvtSTATEMENT (TryStmt{block=x576, catches=ls578, finally=opt583}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x576), 
          ("catches", PrettyRep.List (List.map (fn x577 => cvtCATCH_CLAUSE x577
                                               ) ls578)), ("finally", 
       (case opt583 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x582 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x582))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x596, labels=ls598, cases=ls603}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x596), ("labels", PrettyRep.List (List.map (fn x597 => 
                                                                                                        cvtIDENTIFIER x597
                                                                                                 ) ls598)), 
          ("cases", PrettyRep.List (List.map (fn x602 => cvtCASE x602
                                             ) ls603))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x616, ty=x617, cases=ls619}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x616), ("ty", cvtTYPE x617), 
          ("cases", PrettyRep.List (List.map (fn x618 => cvtCATCH_CLAUSE x618
                                             ) ls619))]))
     | cvtSTATEMENT (DXNStmt{expr=x632}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x632)]))
   and cvtEXPRESSION (ApplyTypeExpression{expr=x638, actuals=ls640}) = PrettyRep.Ctor ("ApplyTypeExpression", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x638), ("actuals", PrettyRep.List (List.map (fn x639 => 
                                                                                                         cvtTYPE x639
                                                                                                  ) ls640))]))
     | cvtEXPRESSION (BinaryExpr(x651, x652, x653)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x651, cvtEXPRESSION x652, cvtEXPRESSION x653]))
     | cvtEXPRESSION (BinaryTypeExpr(x657, x658, x659)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x657, cvtEXPRESSION x658, cvtTYPE x659]))
     | cvtEXPRESSION (CallExpr{func=x663, actuals=ls665}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x663), ("actuals", PrettyRep.List (List.map (fn x664 => 
                                                                                                         cvtEXPRESSION x664
                                                                                                  ) ls665))]))
     | cvtEXPRESSION (Comprehension(x676, ls678, opt683)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x676, PrettyRep.List (List.map (fn x677 => 
                                                                                     cvtFOR_ENUM_HEAD x677
                                                                              ) ls678), 
          
       (case opt683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x682 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x682))
       )]))
     | cvtEXPRESSION (ConditionalExpression(x690, x691, x692)) = PrettyRep.Ctor ("ConditionalExpression", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x690, cvtEXPRESSION x691, cvtEXPRESSION x692]))
     | cvtEXPRESSION (GetParam n696) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n696))
     | cvtEXPRESSION (GetTemp n699) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n699))
     | cvtEXPRESSION (InitExpr(x702, x703, x704)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x702, cvtHEAD x703, cvtINITS x704]))
     | cvtEXPRESSION (LetExpr{defs=x708, body=x709, head=opt711}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x708), ("body", cvtEXPRESSION x709), 
          ("head", 
       (case opt711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x710 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x710))
       ))]))
     | cvtEXPRESSION (ListExpr ls725) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x724 => 
                                                                                                          cvtEXPRESSION x724
                                                                                                   ) ls725)))
     | cvtEXPRESSION (LexicalReference{name=x731, loc=opt733}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x731), ("loc", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x732))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x744, actuals=ls746}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x744), ("actuals", PrettyRep.List (List.map (fn x745 => 
                                                                                                        cvtEXPRESSION x745
                                                                                                 ) ls746))]))
     | cvtEXPRESSION (ObjectNameReference{object=x757, name=x758, loc=opt760}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x757), ("name", cvtNAME_EXPRESSION x758), ("loc", 
       (case opt760 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x759 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x759))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x773, index=x774, loc=opt776}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x773), ("index", cvtEXPRESSION x774), ("loc", 
       (case opt776 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x775 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x775))
       ))]))
     | cvtEXPRESSION (SetExpr(x789, x790, x791)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x789, cvtEXPRESSION x790, cvtEXPRESSION x791]))
     | cvtEXPRESSION (SuperExpr opt796) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x795))
       ))
     | cvtEXPRESSION (TypeExpr x802) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x802))
     | cvtEXPRESSION (ThisExpr opt806) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x805 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x805))
       ))
     | cvtEXPRESSION (UnaryExpr(x812, x813)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x812, cvtEXPRESSION x813]))
     | cvtEXPRESSION (YieldExpr opt818) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x817 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x817))
       ))
     | cvtEXPRESSION (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtEXPRESSION (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtEXPRESSION (LiteralDouble r826) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r826))
     | cvtEXPRESSION (LiteralDecimal d829) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d829))
     | cvtEXPRESSION (LiteralBoolean b832) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b832))
     | cvtEXPRESSION (LiteralString s835) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s835))
     | cvtEXPRESSION (LiteralArray{exprs=x838, ty=opt840}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x838), ("ty", 
       (case opt840 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x839 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x839))
       ))]))
     | cvtEXPRESSION (LiteralNamespace x851) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x851))
     | cvtEXPRESSION (LiteralObject{expr=ls855, ty=opt860}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x854 => 
                                                                        cvtFIELD x854
                                                                 ) ls855)), 
          ("ty", 
       (case opt860 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x859 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x859))
       ))]))
     | cvtEXPRESSION (LiteralFunction x871) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x871))
     | cvtEXPRESSION (LiteralRegExp{str=s874}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s874)]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n885) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n885))
     | cvtFIXTURE_NAME (PropName x888) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x888))
   and cvtBLOCK (Block x891) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x891))
   and cvtFIXTURE (NamespaceFixture x894) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x894))
     | cvtFIXTURE (ClassFixture x897) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS x897))
     | cvtFIXTURE (InterfaceFixture x900) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtINTERFACE x900))
     | cvtFIXTURE (TypeVarFixture x903) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x903))
     | cvtFIXTURE (TypeFixture(ls907, x911)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x906 => cvtIDENTIFIER x906
                                                          ) ls907), cvtTYPE x911]))
     | cvtFIXTURE (MethodFixture{func=x915, ty=x916, writable=b917, override=b918, 
          final=b919, inheritedFrom=opt921}) = PrettyRep.Ctor ("MethodFixture", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x915), ("ty", cvtTYPE x916), 
          ("writable", PrettyRep.Bool b917), ("override", PrettyRep.Bool b918), 
          ("final", PrettyRep.Bool b919), ("inheritedFrom", 
       (case opt921 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x920 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x920))
       ))]))
     | cvtFIXTURE (ValFixture{ty=x940, writable=b941}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x940), ("writable", PrettyRep.Bool b941)]))
     | cvtFIXTURE (VirtualValFixture{ty=x949, getter=opt957, setter=opt968}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x949), ("getter", 
       (case opt957 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x950, opt952) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x950, 
            
         (case opt952 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x951 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x951))
         )]))
       )), ("setter", 
       (case opt968 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x961, opt963) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x961, 
            
         (case opt963 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x962 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x962))
         )]))
       ))]))
   and cvtHEAD (Head(x981, x982)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtFIXTURE_MAP x981, 
          cvtINITS x982]))
   and cvtBINDINGS (ls987, ls992) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x986 => 
                                                                                     cvtBINDING x986
                                                                              ) ls987), 
          PrettyRep.List (List.map (fn x991 => cvtINIT_STEP x991
                                   ) ls992)]
   and cvtFIXTURE_MAP ls1000 = PrettyRep.List (List.map (fn (x997, x998) => 
                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x997, 
                                                               cvtFIXTURE x998]
                                                        ) ls1000)
   and cvtFIXTURE_MAPS ls1011 = PrettyRep.List (List.map (fn ls1007 => PrettyRep.List (List.map (fn (x1004, 
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
   and cvtFOR_ENUM_STATEMENT {isEach=b1353, defn=opt1384, obj=x1388, fixtureMap=opt1396, 
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
       )), ("obj", cvtEXPRESSION x1388), ("fixtureMap", 
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
   and cvtFOR_STATEMENT {fixtureMap=opt1429, defn=opt1463, init=ls1468, cond=x1472, 
          update=x1473, labels=ls1475, body=x1479} = PrettyRep.Rec [("fixtureMap", 
          
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
   and cvtWHILE_STATEMENT {cond=x1495, fixtureMap=opt1503, body=x1507, labels=ls1509} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1495), ("fixtureMap", 
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
   and cvtCATCH_CLAUSE {bindings=(ls1583, ls1588), ty=x1593, fixtureMap=opt1601, 
          inits=opt1612, block=x1616} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1582 => 
                                                                                                                     cvtBINDING x1582
                                                                                                              ) ls1583), 
          PrettyRep.List (List.map (fn x1587 => cvtINIT_STEP x1587
                                   ) ls1588)]), ("ty", cvtTYPE x1593), ("fixtureMap", 
          
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
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1635, getter=opt1643, setter=opt1654} = 
          PrettyRep.Rec [("ty", cvtTYPE x1635), ("getter", 
       (case opt1643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x1636, opt1638) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x1636, 
            
         (case opt1638 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1637 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x1637))
         )]))
       )), ("setter", 
       (case opt1654 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x1647, opt1649) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtFUNC x1647, 
            
         (case opt1649 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1648 => PrettyRep.Ctor ("SOME", SOME (cvtCLASS x1648))
         )]))
       ))]
   and cvtPROGRAM (Program x1665) = PrettyRep.Ctor ("Program", SOME (cvtBLOCK x1665))
end

