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
   and cvtINTERFACE (Interface{name=x211, typeParams=ls213, nonnullable=b217, extends=ls219, 
          instanceRib=x223}) = PrettyRep.Ctor ("Interface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x211), ("typeParams", PrettyRep.List (List.map (fn x212 => 
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
   and cvtEXPRESSION (TernaryExpr(x638, x639, x640)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x638, cvtEXPRESSION x639, cvtEXPRESSION x640]))
     | cvtEXPRESSION (BinaryExpr(x644, x645, x646)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x644, cvtEXPRESSION x645, cvtEXPRESSION x646]))
     | cvtEXPRESSION (BinaryTypeExpr(x650, x651, x652)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x650, cvtEXPRESSION x651, cvtTYPE x652]))
     | cvtEXPRESSION (UnaryExpr(x656, x657)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x656, cvtEXPRESSION x657]))
     | cvtEXPRESSION (TypeExpr x661) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x661))
     | cvtEXPRESSION (ThisExpr opt665) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt665 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x664 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x664))
       ))
     | cvtEXPRESSION (YieldExpr opt672) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x671))
       ))
     | cvtEXPRESSION (SuperExpr opt679) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x678 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x678))
       ))
     | cvtEXPRESSION (CallExpr{func=x685, actuals=ls687}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x685), ("actuals", PrettyRep.List (List.map (fn x686 => 
                                                                                                         cvtEXPRESSION x686
                                                                                                  ) ls687))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x698, actuals=ls700}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x698), ("actuals", PrettyRep.List (List.map (fn x699 => 
                                                                                                         cvtTYPE x699
                                                                                                  ) ls700))]))
     | cvtEXPRESSION (LetExpr{defs=x711, body=x712, head=opt714}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x711), ("body", cvtEXPRESSION x712), 
          ("head", 
       (case opt714 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x713 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x713))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x727, actuals=ls729}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x727), ("actuals", PrettyRep.List (List.map (fn x728 => 
                                                                                                        cvtEXPRESSION x728
                                                                                                 ) ls729))]))
     | cvtEXPRESSION (SetExpr(x740, x741, x742)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x740, cvtEXPRESSION x741, cvtEXPRESSION x742]))
     | cvtEXPRESSION (ListExpr ls747) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x746 => 
                                                                                                          cvtEXPRESSION x746
                                                                                                   ) ls747)))
     | cvtEXPRESSION (InitExpr(x753, x754, x755)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x753, cvtHEAD x754, cvtINITS x755]))
     | cvtEXPRESSION (GetTemp n759) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n759))
     | cvtEXPRESSION (GetParam n762) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n762))
     | cvtEXPRESSION (Comprehension(x765, ls767, opt772)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x765, PrettyRep.List (List.map (fn x766 => 
                                                                                     cvtFOR_ENUM_HEAD x766
                                                                              ) ls767), 
          
       (case opt772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x771 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x771))
       )]))
     | cvtEXPRESSION (LiteralExpr x779) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x779))
     | cvtEXPRESSION (LexicalReference{name=x782, loc=opt784}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x782), ("loc", 
       (case opt784 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x783 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x783))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x795, name=x796, loc=opt798}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x795), ("name", cvtNAME_EXPRESSION x796), ("loc", 
       (case opt798 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x797 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x797))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x811, index=x812, loc=opt814}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x811), ("index", cvtEXPRESSION x812), ("loc", 
       (case opt814 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x813 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x813))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n832) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n832))
     | cvtFIXTURE_NAME (PropName x835) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x835))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r840) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r840))
     | cvtLITERAL (LiteralDecimal d843) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d843))
     | cvtLITERAL (LiteralBoolean b846) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b846))
     | cvtLITERAL (LiteralString s849) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s849))
     | cvtLITERAL (LiteralArray{exprs=x852, ty=opt854}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x852), ("ty", 
       (case opt854 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x853 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x853))
       ))]))
     | cvtLITERAL (LiteralXML ls866) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x865 => 
                                                                                                           cvtEXPRESSION x865
                                                                                                    ) ls866)))
     | cvtLITERAL (LiteralNamespace x872) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x872))
     | cvtLITERAL (LiteralObject{expr=ls876, ty=opt881}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x875 => 
                                                                        cvtFIELD x875
                                                                 ) ls876)), 
          ("ty", 
       (case opt881 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x880 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x880))
       ))]))
     | cvtLITERAL (LiteralFunction x892) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x892))
     | cvtLITERAL (LiteralRegExp{str=s895}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s895)]))
   and cvtBLOCK (Block x901) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x901))
   and cvtFIXTURE (NamespaceFixture x904) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x904))
     | cvtFIXTURE (ClassFixture x907) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS x907))
     | cvtFIXTURE (InterfaceFixture x910) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtINTERFACE x910))
     | cvtFIXTURE (TypeVarFixture x913) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x913))
     | cvtFIXTURE (TypeFixture(ls917, x921)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x916 => cvtIDENTIFIER x916
                                                          ) ls917), cvtTYPE x921]))
     | cvtFIXTURE (MethodFixture{func=x925, ty=x926, writable=b927, override=b928, 
          final=b929}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x925), ("ty", cvtTYPE x926), ("writable", PrettyRep.Bool b927), 
          ("override", PrettyRep.Bool b928), ("final", PrettyRep.Bool b929)]))
     | cvtFIXTURE (ValFixture{ty=x943, writable=b944}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x943), ("writable", PrettyRep.Bool b944)]))
     | cvtFIXTURE (VirtualValFixture{ty=x952, getter=opt954, setter=opt959}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x952), ("getter", 
       (case opt954 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x953 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x953))
       )), ("setter", 
       (case opt959 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x958 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x958))
       ))]))
   and cvtHEAD (Head(x972, x973)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x972, 
          cvtINITS x973]))
   and cvtBINDINGS (ls978, ls983) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x977 => 
                                                                                     cvtBINDING x977
                                                                              ) ls978), 
          PrettyRep.List (List.map (fn x982 => cvtINIT_STEP x982
                                   ) ls983)]
   and cvtRIB ls991 = PrettyRep.List (List.map (fn (x988, x989) => PrettyRep.Tuple [cvtFIXTURE_NAME x988, 
                                                      cvtFIXTURE x989]
                                               ) ls991)
   and cvtRIBS ls1002 = PrettyRep.List (List.map (fn ls998 => PrettyRep.List (List.map (fn (x995, 
                                                                                              x996) => 
                                                                                              PrettyRep.Tuple [cvtFIXTURE_NAME x995, 
                                                                                              cvtFIXTURE x996]
                                                                                       ) ls998)
                                                 ) ls1002)
   and cvtINITS ls1009 = PrettyRep.List (List.map (fn (x1006, x1007) => PrettyRep.Tuple [cvtFIXTURE_NAME x1006, 
                                                         cvtEXPRESSION x1007]
                                                  ) ls1009)
   and cvtINSTANCE_TYPE {name=x1013, typeArgs=ls1015, nonnullable=b1019, typeParams=ls1021, 
          superTypes=ls1026, ty=x1030, dynamic=b1031} = PrettyRep.Rec [("name", 
          cvtNAME x1013), ("typeArgs", PrettyRep.List (List.map (fn x1014 => 
                                                                       cvtTYPE x1014
                                                                ) ls1015)), 
          ("nonnullable", PrettyRep.Bool b1019), ("typeParams", PrettyRep.List (List.map (fn x1020 => 
                                                                                                cvtIDENTIFIER x1020
                                                                                         ) ls1021)), 
          ("superTypes", PrettyRep.List (List.map (fn x1025 => cvtTYPE x1025
                                                  ) ls1026)), ("ty", cvtTYPE x1030), 
          ("dynamic", PrettyRep.Bool b1031)]
   and cvtFIELD {kind=x1047, name=x1048, init=x1049} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1047), ("name", cvtNAME_EXPRESSION x1048), ("init", 
          cvtEXPRESSION x1049)]
   and cvtFIELD_TYPE (x1057, x1058) = PrettyRep.Tuple [cvtNAME_EXPRESSION x1057, 
          cvtTYPE x1058]
   and cvtFUNCTION_TYPE {typeParams=ls1061, thisType=x1065, params=ls1067, 
          minArgs=n1071, hasRest=b1072, result=opt1074} = PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x1060 => cvtIDENTIFIER x1060
                                   ) ls1061)), ("thisType", cvtTYPE x1065), 
          ("params", PrettyRep.List (List.map (fn x1066 => cvtTYPE x1066
                                              ) ls1067)), ("minArgs", PrettyRep.Int n1071), 
          ("hasRest", PrettyRep.Bool b1072), ("result", 
       (case opt1074 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1073))
       ))]
   and cvtFUNC_DEFN {kind=x1091, ns=opt1093, final=b1097, override=b1098, prototype=b1099, 
          static=b1100, func=x1101} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1091), 
          ("ns", 
       (case opt1093 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1092 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1092))
       )), ("final", PrettyRep.Bool b1097), ("override", PrettyRep.Bool b1098), 
          ("prototype", PrettyRep.Bool b1099), ("static", PrettyRep.Bool b1100), 
          ("func", cvtFUNC x1101)]
   and cvtCTOR_DEFN x1117 = cvtCTOR x1117
   and cvtVAR_DEFN {kind=x1118, ns=opt1120, static=b1124, prototype=b1125, 
          bindings=(ls1127, ls1132)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1118), 
          ("ns", 
       (case opt1120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1119 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1119))
       )), ("static", PrettyRep.Bool b1124), ("prototype", PrettyRep.Bool b1125), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1126 => 
                                                                        cvtBINDING x1126
                                                                 ) ls1127), 
          PrettyRep.List (List.map (fn x1131 => cvtINIT_STEP x1131
                                   ) ls1132)])]
   and cvtNAMESPACE_DEFN {ident=x1148, ns=opt1150, init=opt1155} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1148), ("ns", 
       (case opt1150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1149 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1149))
       )), ("init", 
       (case opt1155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1154 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1154))
       ))]
   and cvtCLASS_DEFN {ns=opt1167, privateNS=x1171, protectedNS=x1172, ident=x1173, 
          nonnullable=b1174, dynamic=b1175, final=b1176, params=ls1178, extends=opt1183, 
          implements=ls1188, classDefns=ls1193, instanceDefns=ls1198, instanceStmts=ls1203, 
          ctorDefn=opt1208} = PrettyRep.Rec [("ns", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1166))
       )), ("privateNS", cvtNAMESPACE x1171), ("protectedNS", cvtNAMESPACE x1172), 
          ("ident", cvtIDENTIFIER x1173), ("nonnullable", PrettyRep.Bool b1174), 
          ("dynamic", PrettyRep.Bool b1175), ("final", PrettyRep.Bool b1176), 
          ("params", PrettyRep.List (List.map (fn x1177 => cvtIDENTIFIER x1177
                                              ) ls1178)), ("extends", 
       (case opt1183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1182 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1182))
       )), ("implements", PrettyRep.List (List.map (fn x1187 => cvtTYPE x1187
                                                   ) ls1188)), ("classDefns", 
          PrettyRep.List (List.map (fn x1192 => cvtDEFN x1192
                                   ) ls1193)), ("instanceDefns", PrettyRep.List (List.map (fn x1197 => 
                                                                                                 cvtDEFN x1197
                                                                                          ) ls1198)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1202 => cvtSTATEMENT x1202
                                                     ) ls1203)), ("ctorDefn", 
          
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1207))
       ))]
   and cvtINTERFACE_DEFN {ident=x1241, ns=opt1243, nonnullable=b1247, params=ls1249, 
          extends=ls1254, instanceDefns=ls1259} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1241), ("ns", 
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1242))
       )), ("nonnullable", PrettyRep.Bool b1247), ("params", PrettyRep.List (List.map (fn x1248 => 
                                                                                             cvtIDENTIFIER x1248
                                                                                      ) ls1249)), 
          ("extends", PrettyRep.List (List.map (fn x1253 => cvtTYPE x1253
                                               ) ls1254)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1258 => cvtDEFN x1258
                                   ) ls1259))]
   and cvtTYPE_DEFN {ident=x1276, ns=opt1278, typeParams=ls1283, init=x1287} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1276), ("ns", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1277))
       )), ("typeParams", PrettyRep.List (List.map (fn x1282 => cvtIDENTIFIER x1282
                                                   ) ls1283)), ("init", cvtTYPE x1287)]
   and cvtCLASS_BLOCK {ns=opt1298, protectedNS=x1302, privateNS=x1303, ident=x1304, 
          name=opt1306, block=x1310} = PrettyRep.Rec [("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1297))
       )), ("protectedNS", cvtNAMESPACE x1302), ("privateNS", cvtNAMESPACE x1303), 
          ("ident", cvtIDENTIFIER x1304), ("name", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1305))
       )), ("block", cvtBLOCK x1310)]
   and cvtFOR_ENUM_HEAD {isEach=b1324, bindings=(ls1326, ls1331), expr=x1336} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1324), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1325 => 
                                                                                                                         cvtBINDING x1325
                                                                                                                  ) ls1326), 
          PrettyRep.List (List.map (fn x1330 => cvtINIT_STEP x1330
                                   ) ls1331)]), ("expr", cvtEXPRESSION x1336)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1344, defn=opt1375, obj=x1379, rib=opt1387, 
          next=x1391, labels=ls1393, body=x1397} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1344), ("defn", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1345, ns=opt1347, static=b1351, prototype=b1352, bindings=(ls1354, 
            ls1359)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1345), ("ns", 
         (case opt1347 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1346))
         )), ("static", PrettyRep.Bool b1351), ("prototype", PrettyRep.Bool b1352), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1353 => 
                                                                          cvtBINDING x1353
                                                                   ) ls1354), 
            PrettyRep.List (List.map (fn x1358 => cvtINIT_STEP x1358
                                     ) ls1359)])]))
       )), ("obj", cvtEXPRESSION x1379), ("rib", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1383 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1380, 
                                                                                      x1381) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1380, 
                                                                                      cvtFIXTURE x1381]
                                                                               ) ls1383)))
       )), ("next", cvtSTATEMENT x1391), ("labels", PrettyRep.List (List.map (fn x1392 => 
                                                                                    cvtIDENTIFIER x1392
                                                                             ) ls1393)), 
          ("body", cvtSTATEMENT x1397)]
   and cvtFOR_STATEMENT {rib=opt1420, defn=opt1454, init=ls1459, cond=x1463, 
          update=x1464, labels=ls1466, body=x1470} = PrettyRep.Rec [("rib", 
          
       (case opt1420 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1416 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1413, 
                                                                                      x1414) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1413, 
                                                                                      cvtFIXTURE x1414]
                                                                               ) ls1416)))
       )), ("defn", 
       (case opt1454 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1424, ns=opt1426, static=b1430, prototype=b1431, bindings=(ls1433, 
            ls1438)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1424), ("ns", 
         (case opt1426 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1425 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1425))
         )), ("static", PrettyRep.Bool b1430), ("prototype", PrettyRep.Bool b1431), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1432 => 
                                                                          cvtBINDING x1432
                                                                   ) ls1433), 
            PrettyRep.List (List.map (fn x1437 => cvtINIT_STEP x1437
                                     ) ls1438)])]))
       )), ("init", PrettyRep.List (List.map (fn x1458 => cvtSTATEMENT x1458
                                             ) ls1459)), ("cond", cvtEXPRESSION x1463), 
          ("update", cvtEXPRESSION x1464), ("labels", PrettyRep.List (List.map (fn x1465 => 
                                                                                      cvtIDENTIFIER x1465
                                                                               ) ls1466)), 
          ("body", cvtSTATEMENT x1470)]
   and cvtWHILE_STATEMENT {cond=x1486, rib=opt1494, body=x1498, labels=ls1500} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1486), ("rib", 
       (case opt1494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1490 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1487, 
                                                                                      x1488) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1487, 
                                                                                      cvtFIXTURE x1488]
                                                                               ) ls1490)))
       )), ("body", cvtSTATEMENT x1498), ("labels", PrettyRep.List (List.map (fn x1499 => 
                                                                                    cvtIDENTIFIER x1499
                                                                             ) ls1500))]
   and cvtDIRECTIVES {pragmas=ls1514, defns=ls1519, head=opt1524, body=ls1529, 
          loc=opt1534} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1513 => 
                                                                                    cvtPRAGMA x1513
                                                                             ) ls1514)), 
          ("defns", PrettyRep.List (List.map (fn x1518 => cvtDEFN x1518
                                             ) ls1519)), ("head", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1523))
       )), ("body", PrettyRep.List (List.map (fn x1528 => cvtSTATEMENT x1528
                                             ) ls1529)), ("loc", 
       (case opt1534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1533))
       ))]
   and cvtCASE {label=opt1550, inits=opt1561, body=x1565} = PrettyRep.Rec [("label", 
          
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1549))
       )), ("inits", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1557 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1554, 
                                                                                      x1555) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1554, 
                                                                                      cvtEXPRESSION x1555]
                                                                               ) ls1557)))
       )), ("body", cvtBLOCK x1565)]
   and cvtCATCH_CLAUSE {bindings=(ls1574, ls1579), ty=x1584, rib=opt1592, inits=opt1603, 
          block=x1607} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1573 => 
                                                                                                      cvtBINDING x1573
                                                                                               ) ls1574), 
          PrettyRep.List (List.map (fn x1578 => cvtINIT_STEP x1578
                                   ) ls1579)]), ("ty", cvtTYPE x1584), ("rib", 
          
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1588 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1585, 
                                                                                      x1586) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1585, 
                                                                                      cvtFIXTURE x1586]
                                                                               ) ls1588)))
       )), ("inits", 
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1599 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1596, 
                                                                                      x1597) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1596, 
                                                                                      cvtEXPRESSION x1597]
                                                                               ) ls1599)))
       )), ("block", cvtBLOCK x1607)]
   and cvtFUNC_NAME {kind=x1619, ident=x1620} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1619), 
          ("ident", cvtIDENTIFIER x1620)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1626, getter=opt1628, setter=opt1633} = 
          PrettyRep.Rec [("ty", cvtTYPE x1626), ("getter", 
       (case opt1628 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1627 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1627))
       )), ("setter", 
       (case opt1633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1632 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1632))
       ))]
   and cvtFRAGMENT (Anon x1644) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1644))
end

