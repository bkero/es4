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
          final=b919}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x915), ("ty", cvtTYPE x916), ("writable", PrettyRep.Bool b917), 
          ("override", PrettyRep.Bool b918), ("final", PrettyRep.Bool b919)]))
     | cvtFIXTURE (ValFixture{ty=x933, writable=b934}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x933), ("writable", PrettyRep.Bool b934)]))
     | cvtFIXTURE (VirtualValFixture{ty=x942, getter=opt944, setter=opt949}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x942), ("getter", 
       (case opt944 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x943 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x943))
       )), ("setter", 
       (case opt949 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x948 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x948))
       ))]))
   and cvtHEAD (Head(x962, x963)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x962, 
          cvtINITS x963]))
   and cvtBINDINGS (ls968, ls973) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x967 => 
                                                                                     cvtBINDING x967
                                                                              ) ls968), 
          PrettyRep.List (List.map (fn x972 => cvtINIT_STEP x972
                                   ) ls973)]
   and cvtRIB ls981 = PrettyRep.List (List.map (fn (x978, x979) => PrettyRep.Tuple [cvtFIXTURE_NAME x978, 
                                                      cvtFIXTURE x979]
                                               ) ls981)
   and cvtRIBS ls992 = PrettyRep.List (List.map (fn ls988 => PrettyRep.List (List.map (fn (x985, 
                                                                                             x986) => 
                                                                                             PrettyRep.Tuple [cvtFIXTURE_NAME x985, 
                                                                                             cvtFIXTURE x986]
                                                                                      ) ls988)
                                                ) ls992)
   and cvtINITS ls999 = PrettyRep.List (List.map (fn (x996, x997) => PrettyRep.Tuple [cvtFIXTURE_NAME x996, 
                                                        cvtEXPRESSION x997]
                                                 ) ls999)
   and cvtINSTANCE_TYPE {name=x1003, typeArgs=ls1005, nonnullable=b1009, typeParams=ls1011, 
          superTypes=ls1016, ty=x1020, dynamic=b1021} = PrettyRep.Rec [("name", 
          cvtNAME x1003), ("typeArgs", PrettyRep.List (List.map (fn x1004 => 
                                                                       cvtTYPE x1004
                                                                ) ls1005)), 
          ("nonnullable", PrettyRep.Bool b1009), ("typeParams", PrettyRep.List (List.map (fn x1010 => 
                                                                                                cvtIDENTIFIER x1010
                                                                                         ) ls1011)), 
          ("superTypes", PrettyRep.List (List.map (fn x1015 => cvtTYPE x1015
                                                  ) ls1016)), ("ty", cvtTYPE x1020), 
          ("dynamic", PrettyRep.Bool b1021)]
   and cvtFIELD {kind=x1037, name=x1038, init=x1039} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1037), ("name", cvtNAME_EXPRESSION x1038), ("init", 
          cvtEXPRESSION x1039)]
   and cvtFIELD_TYPE (x1047, x1048) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1047, 
          cvtTYPE x1048]
   and cvtFUNCTION_TYPE {typeParams=ls1051, thisType=x1055, params=ls1057, 
          minArgs=n1061, hasRest=b1062, result=opt1064} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1050 => cvtIDENTIFIER x1050
                                   ) ls1051)), ("thisType", cvtTYPE x1055), 
          ("params", PrettyRep.List (List.map (fn x1056 => cvtTYPE x1056
                                              ) ls1057)), ("minArgs", PrettyRep.Int n1061), 
          ("hasRest", PrettyRep.Bool b1062), ("result", 
       (case opt1064 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1063 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1063))
       ))]
   and cvtFUNC_DEFN {kind=x1081, ns=opt1083, final=b1087, override=b1088, prototype=b1089, 
          static=b1090, func=x1091} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1081), 
          ("ns", 
       (case opt1083 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1082 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1082))
       )), ("final", PrettyRep.Bool b1087), ("override", PrettyRep.Bool b1088), 
          ("prototype", PrettyRep.Bool b1089), ("static", PrettyRep.Bool b1090), 
          ("func", cvtFUNC x1091)]
   and cvtCTOR_DEFN x1107 = cvtCTOR x1107
   and cvtVAR_DEFN {kind=x1108, ns=opt1110, static=b1114, prototype=b1115, 
          bindings=(ls1117, ls1122)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1108), 
          ("ns", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1109 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1109))
       )), ("static", PrettyRep.Bool b1114), ("prototype", PrettyRep.Bool b1115), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1116 => 
                                                                        cvtBINDING x1116
                                                                 ) ls1117), 
          PrettyRep.List (List.map (fn x1121 => cvtINIT_STEP x1121
                                   ) ls1122)])]
   and cvtNAMESPACE_DEFN {ident=x1138, ns=opt1140, init=opt1145} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1138), ("ns", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1139))
       )), ("init", 
       (case opt1145 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1144 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1144))
       ))]
   and cvtCLASS_DEFN {ns=opt1157, privateNS=x1161, protectedNS=x1162, ident=x1163, 
          nonnullable=b1164, dynamic=b1165, final=b1166, params=ls1168, extends=opt1173, 
          implements=ls1178, classDefns=ls1183, instanceDefns=ls1188, instanceStmts=ls1193, 
          ctorDefn=opt1198} = PrettyRep.Rec [("ns", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1156))
       )), ("privateNS", cvtNAMESPACE x1161), ("protectedNS", cvtNAMESPACE x1162), 
          ("ident", cvtIDENTIFIER x1163), ("nonnullable", PrettyRep.Bool b1164), 
          ("dynamic", PrettyRep.Bool b1165), ("final", PrettyRep.Bool b1166), 
          ("params", PrettyRep.List (List.map (fn x1167 => cvtIDENTIFIER x1167
                                              ) ls1168)), ("extends", 
       (case opt1173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1172 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1172))
       )), ("implements", PrettyRep.List (List.map (fn x1177 => cvtTYPE x1177
                                                   ) ls1178)), ("classDefns", 
          PrettyRep.List (List.map (fn x1182 => cvtDEFN x1182
                                   ) ls1183)), ("instanceDefns", PrettyRep.List (List.map (fn x1187 => 
                                                                                                 cvtDEFN x1187
                                                                                          ) ls1188)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1192 => cvtSTATEMENT x1192
                                                     ) ls1193)), ("ctorDefn", 
          
       (case opt1198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1197 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1197))
       ))]
   and cvtINTERFACE_DEFN {ident=x1231, ns=opt1233, nonnullable=b1237, params=ls1239, 
          extends=ls1244, instanceDefns=ls1249} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1231), ("ns", 
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1232))
       )), ("nonnullable", PrettyRep.Bool b1237), ("params", PrettyRep.List (List.map (fn x1238 => 
                                                                                             cvtIDENTIFIER x1238
                                                                                      ) ls1239)), 
          ("extends", PrettyRep.List (List.map (fn x1243 => cvtTYPE x1243
                                               ) ls1244)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1248 => cvtDEFN x1248
                                   ) ls1249))]
   and cvtTYPE_DEFN {ident=x1266, ns=opt1268, typeParams=ls1273, init=x1277} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1266), ("ns", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1267))
       )), ("typeParams", PrettyRep.List (List.map (fn x1272 => cvtIDENTIFIER x1272
                                                   ) ls1273)), ("init", cvtTYPE x1277)]
   and cvtCLASS_BLOCK {ns=opt1288, protectedNS=x1292, privateNS=x1293, ident=x1294, 
          name=opt1296, block=x1300} = PrettyRep.Rec [("ns", 
       (case opt1288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1287 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1287))
       )), ("protectedNS", cvtNAMESPACE x1292), ("privateNS", cvtNAMESPACE x1293), 
          ("ident", cvtIDENTIFIER x1294), ("name", 
       (case opt1296 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1295 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1295))
       )), ("block", cvtBLOCK x1300)]
   and cvtFOR_ENUM_HEAD {isEach=b1314, bindings=(ls1316, ls1321), expr=x1326} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1314), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1315 => 
                                                                                                                         cvtBINDING x1315
                                                                                                                  ) ls1316), 
          PrettyRep.List (List.map (fn x1320 => cvtINIT_STEP x1320
                                   ) ls1321)]), ("expr", cvtEXPRESSION x1326)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1334, defn=opt1365, obj=x1369, rib=opt1377, 
          next=x1381, labels=ls1383, body=x1387} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1334), ("defn", 
       (case opt1365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1335, ns=opt1337, static=b1341, prototype=b1342, bindings=(ls1344, 
            ls1349)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1335), ("ns", 
         (case opt1337 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1336 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1336))
         )), ("static", PrettyRep.Bool b1341), ("prototype", PrettyRep.Bool b1342), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1343 => 
                                                                          cvtBINDING x1343
                                                                   ) ls1344), 
            PrettyRep.List (List.map (fn x1348 => cvtINIT_STEP x1348
                                     ) ls1349)])]))
       )), ("obj", cvtEXPRESSION x1369), ("rib", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1373 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1370, 
                                                                                      x1371) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1370, 
                                                                                      cvtFIXTURE x1371]
                                                                               ) ls1373)))
       )), ("next", cvtSTATEMENT x1381), ("labels", PrettyRep.List (List.map (fn x1382 => 
                                                                                    cvtIDENTIFIER x1382
                                                                             ) ls1383)), 
          ("body", cvtSTATEMENT x1387)]
   and cvtFOR_STATEMENT {rib=opt1410, defn=opt1444, init=ls1449, cond=x1453, 
          update=x1454, labels=ls1456, body=x1460} = PrettyRep.Rec [("rib", 
          
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1406 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1403, 
                                                                                      x1404) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1403, 
                                                                                      cvtFIXTURE x1404]
                                                                               ) ls1406)))
       )), ("defn", 
       (case opt1444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1414, ns=opt1416, static=b1420, prototype=b1421, bindings=(ls1423, 
            ls1428)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1414), ("ns", 
         (case opt1416 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1415))
         )), ("static", PrettyRep.Bool b1420), ("prototype", PrettyRep.Bool b1421), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1422 => 
                                                                          cvtBINDING x1422
                                                                   ) ls1423), 
            PrettyRep.List (List.map (fn x1427 => cvtINIT_STEP x1427
                                     ) ls1428)])]))
       )), ("init", PrettyRep.List (List.map (fn x1448 => cvtSTATEMENT x1448
                                             ) ls1449)), ("cond", cvtEXPRESSION x1453), 
          ("update", cvtEXPRESSION x1454), ("labels", PrettyRep.List (List.map (fn x1455 => 
                                                                                      cvtIDENTIFIER x1455
                                                                               ) ls1456)), 
          ("body", cvtSTATEMENT x1460)]
   and cvtWHILE_STATEMENT {cond=x1476, rib=opt1484, body=x1488, labels=ls1490} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1476), ("rib", 
       (case opt1484 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1480 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1477, 
                                                                                      x1478) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1477, 
                                                                                      cvtFIXTURE x1478]
                                                                               ) ls1480)))
       )), ("body", cvtSTATEMENT x1488), ("labels", PrettyRep.List (List.map (fn x1489 => 
                                                                                    cvtIDENTIFIER x1489
                                                                             ) ls1490))]
   and cvtDIRECTIVES {pragmas=ls1504, defns=ls1509, head=opt1514, body=ls1519, 
          loc=opt1524} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1503 => 
                                                                                    cvtPRAGMA x1503
                                                                             ) ls1504)), 
          ("defns", PrettyRep.List (List.map (fn x1508 => cvtDEFN x1508
                                             ) ls1509)), ("head", 
       (case opt1514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1513 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1513))
       )), ("body", PrettyRep.List (List.map (fn x1518 => cvtSTATEMENT x1518
                                             ) ls1519)), ("loc", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1523))
       ))]
   and cvtCASE {label=opt1540, inits=opt1551, body=x1555} = PrettyRep.Rec [("label", 
          
       (case opt1540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1539 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1539))
       )), ("inits", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1547 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1544, 
                                                                                      x1545) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1544, 
                                                                                      cvtEXPRESSION x1545]
                                                                               ) ls1547)))
       )), ("body", cvtBLOCK x1555)]
   and cvtCATCH_CLAUSE {bindings=(ls1564, ls1569), ty=x1574, rib=opt1582, inits=opt1593, 
          block=x1597} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1563 => 
                                                                                                      cvtBINDING x1563
                                                                                               ) ls1564), 
          PrettyRep.List (List.map (fn x1568 => cvtINIT_STEP x1568
                                   ) ls1569)]), ("ty", cvtTYPE x1574), ("rib", 
          
       (case opt1582 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1578 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1575, 
                                                                                      x1576) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1575, 
                                                                                      cvtFIXTURE x1576]
                                                                               ) ls1578)))
       )), ("inits", 
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1589 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1586, 
                                                                                      x1587) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1586, 
                                                                                      cvtEXPRESSION x1587]
                                                                               ) ls1589)))
       )), ("block", cvtBLOCK x1597)]
   and cvtFUNC_NAME {kind=x1609, ident=x1610} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1609), 
          ("ident", cvtIDENTIFIER x1610)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1616, getter=opt1618, setter=opt1623} = 
          PrettyRep.Rec [("ty", cvtTYPE x1616), ("getter", 
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1617 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1617))
       )), ("setter", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1622 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1622))
       ))]
   and cvtFRAGMENT (Anon x1634) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1634))
end

