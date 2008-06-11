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
          implements=ls167, classRib=x171, instanceRib=x172, instanceInits=x173, 
          constructor=opt175, classType=x179}) = PrettyRep.Ctor ("Class", SOME (PrettyRep.Rec [("name", 
          cvtNAME x146), ("privateNS", cvtNAMESPACE x147), ("protectedNS", 
          cvtNAMESPACE x148), ("parentProtectedNSs", PrettyRep.List (List.map (fn x149 => 
                                                                                     cvtNAMESPACE x149
                                                                              ) ls150)), 
          ("typeParams", PrettyRep.List (List.map (fn x154 => cvtIDENTIFIER x154
                                                  ) ls155)), ("nonnullable", 
          PrettyRep.Bool b159), ("dynamic", PrettyRep.Bool b160), ("extends", 
          
       (case opt162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x161 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x161))
       )), ("implements", PrettyRep.List (List.map (fn x166 => cvtTYPE x166
                                                   ) ls167)), ("classRib", 
          cvtRIB x171), ("instanceRib", cvtRIB x172), ("instanceInits", cvtHEAD x173), 
          ("constructor", 
       (case opt175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x174 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x174))
       )), ("classType", cvtTYPE x179)]))
   and cvtINTERFACE (Interface{name=x211, typeParams=ls213, nonnullable=b217, 
          extends=ls219, instanceRib=x223}) = PrettyRep.Ctor ("Interface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x211), ("typeParams", PrettyRep.List (List.map (fn x212 => 
                                                                                                      cvtIDENTIFIER x212
                                                                                               ) ls213)), 
          ("nonnullable", PrettyRep.Bool b217), ("extends", PrettyRep.List (List.map (fn x218 => 
                                                                                            cvtTYPE x218
                                                                                     ) ls219)), 
          ("instanceRib", cvtRIB x223)]))
   and cvtCTOR (Ctor{settings=x237, superArgs=ls239, func=x243}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x237), ("superArgs", PrettyRep.List (List.map (fn x238 => 
                                                                                                         cvtEXPRESSION x238
                                                                                                  ) ls239)), 
          ("func", cvtFUNC x243)]))
   and cvtFUNC (Func{name=x253, fsig=x254, native=b255, generator=b256, block=opt258, 
          param=x262, defaults=ls264, ty=x268, loc=opt270}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x253), ("fsig", cvtFUNC_SIG x254), 
          ("native", PrettyRep.Bool b255), ("generator", PrettyRep.Bool b256), 
          ("block", 
       (case opt258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x257 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x257))
       )), ("param", cvtHEAD x262), ("defaults", PrettyRep.List (List.map (fn x263 => 
                                                                                 cvtEXPRESSION x263
                                                                          ) ls264)), 
          ("ty", cvtTYPE x268), ("loc", 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x269))
       ))]))
   and cvtDEFN (ClassDefn x295) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x295))
     | cvtDEFN (VariableDefn x298) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x298))
     | cvtDEFN (FunctionDefn x301) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x301))
     | cvtDEFN (ConstructorDefn x304) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x304))
     | cvtDEFN (InterfaceDefn x307) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x307))
     | cvtDEFN (NamespaceDefn x310) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x310))
     | cvtDEFN (TypeDefn x313) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x313))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls317, params=x321, paramTypes=ls323, 
          defaults=ls328, ctorInits=opt339, returnType=opt344, thisType=opt349, 
          hasRest=b353}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x316 => cvtIDENTIFIER x316
                                   ) ls317)), ("params", cvtBINDINGS x321), 
          ("paramTypes", PrettyRep.List (List.map (fn x322 => cvtTYPE x322
                                                  ) ls323)), ("defaults", PrettyRep.List (List.map (fn x327 => 
                                                                                                          cvtEXPRESSION x327
                                                                                                   ) ls328)), 
          ("ctorInits", 
       (case opt339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x332, ls334) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x332, 
            PrettyRep.List (List.map (fn x333 => cvtEXPRESSION x333
                                     ) ls334)]))
       )), ("returnType", 
       (case opt344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x343 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x343))
       )), ("thisType", 
       (case opt349 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x348 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x348))
       )), ("hasRest", PrettyRep.Bool b353)]))
   and cvtBINDING (Binding{ident=x373, ty=x374}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x373), ("ty", 
          cvtTYPE x374)]))
   and cvtBINDING_IDENTIFIER (TempIdent n382) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n382))
     | cvtBINDING_IDENTIFIER (ParamIdent n385) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n385))
     | cvtBINDING_IDENTIFIER (PropIdent x388) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x388))
   and cvtINIT_STEP (InitStep(x391, x392)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x391, 
          cvtEXPRESSION x392]))
     | cvtINIT_STEP (AssignStep(x396, x397)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x396, cvtEXPRESSION x397]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (RecordType ls407) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn (x404, 
                                                                                                        x405) => 
                                                                                                        PrettyRep.Tuple [cvtNAME_EXPRESSION x404, 
                                                                                                        cvtTYPE x405]
                                                                                                 ) ls407)))
     | cvtTYPE (ArrayType(ls414, opt419)) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x413 => 
                                                                                                                                cvtTYPE x413
                                                                                                                         ) ls414), 
          
       (case opt419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x418 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x418))
       )]))
     | cvtTYPE (UnionType ls427) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x426 => 
                                                                                                      cvtTYPE x426
                                                                                               ) ls427)))
     | cvtTYPE (NonNullType x433) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x433))
     | cvtTYPE (FunctionType x436) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNCTION_TYPE x436))
     | cvtTYPE (AppType(x439, ls441)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x439, 
          PrettyRep.List (List.map (fn x440 => cvtTYPE x440
                                   ) ls441)]))
     | cvtTYPE (TypeName(x448, opt450)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x448, 
          
       (case opt450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x449 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x449))
       )]))
     | cvtTYPE (ClassType x457) = PrettyRep.Ctor ("ClassType", SOME (cvtCLASS x457))
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
     | cvtFIXTURE (VirtualValFixture{ty=x949, getter=opt951, setter=opt956}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x949), ("getter", 
       (case opt951 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x950 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x950))
       )), ("setter", 
       (case opt956 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x955 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x955))
       ))]))
   and cvtHEAD (Head(x969, x970)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x969, 
          cvtINITS x970]))
   and cvtBINDINGS (ls975, ls980) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x974 => 
                                                                                     cvtBINDING x974
                                                                              ) ls975), 
          PrettyRep.List (List.map (fn x979 => cvtINIT_STEP x979
                                   ) ls980)]
   and cvtRIB ls988 = PrettyRep.List (List.map (fn (x985, x986) => PrettyRep.Tuple [cvtFIXTURE_NAME x985, 
                                                      cvtFIXTURE x986]
                                               ) ls988)
   and cvtRIBS ls999 = PrettyRep.List (List.map (fn ls995 => PrettyRep.List (List.map (fn (x992, 
                                                                                             x993) => 
                                                                                             PrettyRep.Tuple [cvtFIXTURE_NAME x992, 
                                                                                             cvtFIXTURE x993]
                                                                                      ) ls995)
                                                ) ls999)
   and cvtINITS ls1006 = PrettyRep.List (List.map (fn (x1003, x1004) => PrettyRep.Tuple [cvtFIXTURE_NAME x1003, 
                                                         cvtEXPRESSION x1004]
                                                  ) ls1006)
   and cvtINSTANCE_TYPE {name=x1010, typeArgs=ls1012, nonnullable=b1016, typeParams=ls1018, 
          superTypes=ls1023, ty=x1027, dynamic=b1028} = PrettyRep.Rec [("name", 
          cvtNAME x1010), ("typeArgs", PrettyRep.List (List.map (fn x1011 => 
                                                                       cvtTYPE x1011
                                                                ) ls1012)), 
          ("nonnullable", PrettyRep.Bool b1016), ("typeParams", PrettyRep.List (List.map (fn x1017 => 
                                                                                                cvtIDENTIFIER x1017
                                                                                         ) ls1018)), 
          ("superTypes", PrettyRep.List (List.map (fn x1022 => cvtTYPE x1022
                                                  ) ls1023)), ("ty", cvtTYPE x1027), 
          ("dynamic", PrettyRep.Bool b1028)]
   and cvtFIELD {kind=x1044, name=x1045, init=x1046} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1044), ("name", cvtNAME_EXPRESSION x1045), ("init", 
          cvtEXPRESSION x1046)]
   and cvtFIELD_TYPE (x1054, x1055) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1054, 
          cvtTYPE x1055]
   and cvtFUNCTION_TYPE {typeParams=ls1058, thisType=x1062, params=ls1064, 
          minArgs=n1068, hasRest=b1069, result=opt1071} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1057 => cvtIDENTIFIER x1057
                                   ) ls1058)), ("thisType", cvtTYPE x1062), 
          ("params", PrettyRep.List (List.map (fn x1063 => cvtTYPE x1063
                                              ) ls1064)), ("minArgs", PrettyRep.Int n1068), 
          ("hasRest", PrettyRep.Bool b1069), ("result", 
       (case opt1071 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1070 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1070))
       ))]
   and cvtFUNC_DEFN {kind=x1088, ns=opt1090, final=b1094, override=b1095, prototype=b1096, 
          static=b1097, func=x1098} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1088), 
          ("ns", 
       (case opt1090 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1089 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1089))
       )), ("final", PrettyRep.Bool b1094), ("override", PrettyRep.Bool b1095), 
          ("prototype", PrettyRep.Bool b1096), ("static", PrettyRep.Bool b1097), 
          ("func", cvtFUNC x1098)]
   and cvtCTOR_DEFN x1114 = cvtCTOR x1114
   and cvtVAR_DEFN {kind=x1115, ns=opt1117, static=b1121, prototype=b1122, 
          bindings=(ls1124, ls1129)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1115), 
          ("ns", 
       (case opt1117 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1116 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1116))
       )), ("static", PrettyRep.Bool b1121), ("prototype", PrettyRep.Bool b1122), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1123 => 
                                                                        cvtBINDING x1123
                                                                 ) ls1124), 
          PrettyRep.List (List.map (fn x1128 => cvtINIT_STEP x1128
                                   ) ls1129)])]
   and cvtNAMESPACE_DEFN {ident=x1145, ns=opt1147, init=opt1152} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1145), ("ns", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1146))
       )), ("init", 
       (case opt1152 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1151 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1151))
       ))]
   and cvtCLASS_DEFN {ns=opt1164, privateNS=x1168, protectedNS=x1169, ident=x1170, 
          nonnullable=b1171, dynamic=b1172, final=b1173, params=ls1175, extends=opt1180, 
          implements=ls1185, classDefns=ls1190, instanceDefns=ls1195, instanceStmts=ls1200, 
          ctorDefn=opt1205} = PrettyRep.Rec [("ns", 
       (case opt1164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1163 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1163))
       )), ("privateNS", cvtNAMESPACE x1168), ("protectedNS", cvtNAMESPACE x1169), 
          ("ident", cvtIDENTIFIER x1170), ("nonnullable", PrettyRep.Bool b1171), 
          ("dynamic", PrettyRep.Bool b1172), ("final", PrettyRep.Bool b1173), 
          ("params", PrettyRep.List (List.map (fn x1174 => cvtIDENTIFIER x1174
                                              ) ls1175)), ("extends", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1179))
       )), ("implements", PrettyRep.List (List.map (fn x1184 => cvtTYPE x1184
                                                   ) ls1185)), ("classDefns", 
          PrettyRep.List (List.map (fn x1189 => cvtDEFN x1189
                                   ) ls1190)), ("instanceDefns", PrettyRep.List (List.map (fn x1194 => 
                                                                                                 cvtDEFN x1194
                                                                                          ) ls1195)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1199 => cvtSTATEMENT x1199
                                                     ) ls1200)), ("ctorDefn", 
          
       (case opt1205 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1204 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1204))
       ))]
   and cvtINTERFACE_DEFN {ident=x1238, ns=opt1240, nonnullable=b1244, params=ls1246, 
          extends=ls1251, instanceDefns=ls1256} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1238), ("ns", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1239))
       )), ("nonnullable", PrettyRep.Bool b1244), ("params", PrettyRep.List (List.map (fn x1245 => 
                                                                                             cvtIDENTIFIER x1245
                                                                                      ) ls1246)), 
          ("extends", PrettyRep.List (List.map (fn x1250 => cvtTYPE x1250
                                               ) ls1251)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1255 => cvtDEFN x1255
                                   ) ls1256))]
   and cvtTYPE_DEFN {ident=x1273, ns=opt1275, typeParams=ls1280, init=x1284} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1273), ("ns", 
       (case opt1275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1274 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1274))
       )), ("typeParams", PrettyRep.List (List.map (fn x1279 => cvtIDENTIFIER x1279
                                                   ) ls1280)), ("init", cvtTYPE x1284)]
   and cvtCLASS_BLOCK {ns=opt1295, protectedNS=x1299, privateNS=x1300, ident=x1301, 
          name=opt1303, block=x1307} = PrettyRep.Rec [("ns", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1294))
       )), ("protectedNS", cvtNAMESPACE x1299), ("privateNS", cvtNAMESPACE x1300), 
          ("ident", cvtIDENTIFIER x1301), ("name", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1302))
       )), ("block", cvtBLOCK x1307)]
   and cvtFOR_ENUM_HEAD {isEach=b1321, bindings=(ls1323, ls1328), expr=x1333} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1321), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1322 => 
                                                                                                                         cvtBINDING x1322
                                                                                                                  ) ls1323), 
          PrettyRep.List (List.map (fn x1327 => cvtINIT_STEP x1327
                                   ) ls1328)]), ("expr", cvtEXPRESSION x1333)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1341, defn=opt1372, obj=x1376, rib=opt1384, 
          next=x1388, labels=ls1390, body=x1394} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1341), ("defn", 
       (case opt1372 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1342, ns=opt1344, static=b1348, prototype=b1349, bindings=(ls1351, 
            ls1356)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1342), ("ns", 
         (case opt1344 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1343 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1343))
         )), ("static", PrettyRep.Bool b1348), ("prototype", PrettyRep.Bool b1349), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1350 => 
                                                                          cvtBINDING x1350
                                                                   ) ls1351), 
            PrettyRep.List (List.map (fn x1355 => cvtINIT_STEP x1355
                                     ) ls1356)])]))
       )), ("obj", cvtEXPRESSION x1376), ("rib", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1380 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1377, 
                                                                                      x1378) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1377, 
                                                                                      cvtFIXTURE x1378]
                                                                               ) ls1380)))
       )), ("next", cvtSTATEMENT x1388), ("labels", PrettyRep.List (List.map (fn x1389 => 
                                                                                    cvtIDENTIFIER x1389
                                                                             ) ls1390)), 
          ("body", cvtSTATEMENT x1394)]
   and cvtFOR_STATEMENT {rib=opt1417, defn=opt1451, init=ls1456, cond=x1460, 
          update=x1461, labels=ls1463, body=x1467} = PrettyRep.Rec [("rib", 
          
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1413 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1410, 
                                                                                      x1411) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1410, 
                                                                                      cvtFIXTURE x1411]
                                                                               ) ls1413)))
       )), ("defn", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1421, ns=opt1423, static=b1427, prototype=b1428, bindings=(ls1430, 
            ls1435)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1421), ("ns", 
         (case opt1423 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1422 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1422))
         )), ("static", PrettyRep.Bool b1427), ("prototype", PrettyRep.Bool b1428), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1429 => 
                                                                          cvtBINDING x1429
                                                                   ) ls1430), 
            PrettyRep.List (List.map (fn x1434 => cvtINIT_STEP x1434
                                     ) ls1435)])]))
       )), ("init", PrettyRep.List (List.map (fn x1455 => cvtSTATEMENT x1455
                                             ) ls1456)), ("cond", cvtEXPRESSION x1460), 
          ("update", cvtEXPRESSION x1461), ("labels", PrettyRep.List (List.map (fn x1462 => 
                                                                                      cvtIDENTIFIER x1462
                                                                               ) ls1463)), 
          ("body", cvtSTATEMENT x1467)]
   and cvtWHILE_STATEMENT {cond=x1483, rib=opt1491, body=x1495, labels=ls1497} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1483), ("rib", 
       (case opt1491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1487 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1484, 
                                                                                      x1485) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1484, 
                                                                                      cvtFIXTURE x1485]
                                                                               ) ls1487)))
       )), ("body", cvtSTATEMENT x1495), ("labels", PrettyRep.List (List.map (fn x1496 => 
                                                                                    cvtIDENTIFIER x1496
                                                                             ) ls1497))]
   and cvtDIRECTIVES {pragmas=ls1511, defns=ls1516, head=opt1521, body=ls1526, 
          loc=opt1531} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1510 => 
                                                                                    cvtPRAGMA x1510
                                                                             ) ls1511)), 
          ("defns", PrettyRep.List (List.map (fn x1515 => cvtDEFN x1515
                                             ) ls1516)), ("head", 
       (case opt1521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1520 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1520))
       )), ("body", PrettyRep.List (List.map (fn x1525 => cvtSTATEMENT x1525
                                             ) ls1526)), ("loc", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1530))
       ))]
   and cvtCASE {label=opt1547, inits=opt1558, body=x1562} = PrettyRep.Rec [("label", 
          
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1546))
       )), ("inits", 
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1554 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1551, 
                                                                                      x1552) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1551, 
                                                                                      cvtEXPRESSION x1552]
                                                                               ) ls1554)))
       )), ("body", cvtBLOCK x1562)]
   and cvtCATCH_CLAUSE {bindings=(ls1571, ls1576), ty=x1581, rib=opt1589, inits=opt1600, 
          block=x1604} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1570 => 
                                                                                                      cvtBINDING x1570
                                                                                               ) ls1571), 
          PrettyRep.List (List.map (fn x1575 => cvtINIT_STEP x1575
                                   ) ls1576)]), ("ty", cvtTYPE x1581), ("rib", 
          
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1585 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1582, 
                                                                                      x1583) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1582, 
                                                                                      cvtFIXTURE x1583]
                                                                               ) ls1585)))
       )), ("inits", 
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1596 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1593, 
                                                                                      x1594) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1593, 
                                                                                      cvtEXPRESSION x1594]
                                                                               ) ls1596)))
       )), ("block", cvtBLOCK x1604)]
   and cvtFUNC_NAME {kind=x1616, ident=x1617} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1616), 
          ("ident", cvtIDENTIFIER x1617)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1623, getter=opt1625, setter=opt1630} = 
          PrettyRep.Rec [("ty", cvtTYPE x1623), ("getter", 
       (case opt1625 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1624 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1624))
       )), ("setter", 
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1629 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1629))
       ))]
   and cvtPROGRAM (Program x1641) = PrettyRep.Ctor ("Program", SOME (cvtBLOCK x1641))
end

