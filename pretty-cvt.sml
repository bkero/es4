structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtNONCE n20 = PrettyRep.Int n20
   and cvtNAMESPACE (OpaqueNamespace x21) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtNONCE x21))
     | cvtNAMESPACE (StringNamespace s24) = PrettyRep.Ctor ("StringNamespace", 
          SOME (PrettyRep.UniStr s24))
   and cvtNAME {ns=x27, id=x28} = PrettyRep.Rec [("ns", cvtNAMESPACE x27), 
          ("id", cvtIDENT x28)]
   and cvtMULTINAME {nss=ls39, id=x43} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls35 => 
                                                                                                PrettyRep.List (List.map (fn x34 => 
                                                                                                                                cvtNAMESPACE x34
                                                                                                                         ) ls35)
                                                                                         ) ls39)), 
          ("id", cvtIDENT x43)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
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
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x110) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x110))
     | cvtPRAGMA (UseDefaultNamespace x113) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x113))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x124, privateNS=x125, protectedNS=x126, parentProtectedNSs=ls128, 
          typeParams=ls133, nonnullable=b137, dynamic=b138, extends=opt140, 
          implements=ls145, classRib=x149, instanceRib=x150, instanceInits=x151, 
          constructor=opt153, classType=x157, instanceType=x158}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x124), ("privateNS", cvtNAMESPACE x125), 
          ("protectedNS", cvtNAMESPACE x126), ("parentProtectedNSs", PrettyRep.List (List.map (fn x127 => 
                                                                                                     cvtNAMESPACE x127
                                                                                              ) ls128)), 
          ("typeParams", PrettyRep.List (List.map (fn x132 => cvtIDENT x132
                                                  ) ls133)), ("nonnullable", 
          PrettyRep.Bool b137), ("dynamic", PrettyRep.Bool b138), ("extends", 
          
       (case opt140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x139))
       )), ("implements", PrettyRep.List (List.map (fn x144 => cvtTYPE_EXPR x144
                                                   ) ls145)), ("classRib", 
          cvtRIB x149), ("instanceRib", cvtRIB x150), ("instanceInits", cvtHEAD x151), 
          ("constructor", 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x152))
       )), ("classType", cvtTYPE_EXPR x157), ("instanceType", cvtTYPE_EXPR x158)]))
   and cvtIFACE (Iface{name=x192, typeParams=ls194, nonnullable=b198, extends=ls200, 
          instanceRib=x204, instanceType=x205}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x192), ("typeParams", PrettyRep.List (List.map (fn x193 => 
                                                                                                      cvtIDENT x193
                                                                                               ) ls194)), 
          ("nonnullable", PrettyRep.Bool b198), ("extends", PrettyRep.List (List.map (fn x199 => 
                                                                                            cvtTYPE_EXPR x199
                                                                                     ) ls200)), 
          ("instanceRib", cvtRIB x204), ("instanceType", cvtTYPE_EXPR x205)]))
   and cvtCTOR (Ctor{settings=x221, superArgs=ls223, func=x227}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x221), ("superArgs", PrettyRep.List (List.map (fn x222 => 
                                                                                                         cvtEXPR x222
                                                                                                  ) ls223)), 
          ("func", cvtFUNC x227)]))
   and cvtFUNC (Func{name=x237, fsig=x238, native=b239, block=opt241, param=x245, 
          defaults=ls247, ty=x251, loc=opt253}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x237), ("fsig", cvtFUNC_SIG x238), ("native", PrettyRep.Bool b239), 
          ("block", 
       (case opt241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x240 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x240))
       )), ("param", cvtHEAD x245), ("defaults", PrettyRep.List (List.map (fn x246 => 
                                                                                 cvtEXPR x246
                                                                          ) ls247)), 
          ("ty", cvtTYPE_EXPR x251), ("loc", 
       (case opt253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x252 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x252))
       ))]))
   and cvtDEFN (ClassDefn x276) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x276))
     | cvtDEFN (VariableDefn x279) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x279))
     | cvtDEFN (FunctionDefn x282) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x282))
     | cvtDEFN (ConstructorDefn x285) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x285))
     | cvtDEFN (InterfaceDefn x288) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x288))
     | cvtDEFN (NamespaceDefn x291) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x291))
     | cvtDEFN (TypeDefn x294) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x294))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls298, params=x302, paramTypes=ls304, 
          defaults=ls309, ctorInits=opt320, returnType=x324, thisType=opt326, 
          hasRest=b330}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x297 => cvtIDENT x297
                                   ) ls298)), ("params", cvtBINDINGS x302), 
          ("paramTypes", PrettyRep.List (List.map (fn x303 => cvtTYPE_EXPR x303
                                                  ) ls304)), ("defaults", PrettyRep.List (List.map (fn x308 => 
                                                                                                          cvtEXPR x308
                                                                                                   ) ls309)), 
          ("ctorInits", 
       (case opt320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x313, ls315) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x313, 
            PrettyRep.List (List.map (fn x314 => cvtEXPR x314
                                     ) ls315)]))
       )), ("returnType", cvtTYPE_EXPR x324), ("thisType", 
       (case opt326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x325 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x325))
       )), ("hasRest", PrettyRep.Bool b330)]))
   and cvtBINDING (Binding{ident=x350, ty=x351}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x350), ("ty", cvtTYPE_EXPR x351)]))
   and cvtBINDING_IDENT (TempIdent n359) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n359))
     | cvtBINDING_IDENT (ParamIdent n362) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n362))
     | cvtBINDING_IDENT (PropIdent x365) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x365))
   and cvtINIT_STEP (InitStep(x368, x369)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x368, 
          cvtEXPR x369]))
     | cvtINIT_STEP (AssignStep(x373, x374)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x373, cvtEXPR x374]))
   and cvtTYPE_EXPR (SpecialType x378) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x378))
     | cvtTYPE_EXPR (UnionType ls382) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x381 => 
                                                                                                           cvtTYPE_EXPR x381
                                                                                                    ) ls382)))
     | cvtTYPE_EXPR (ArrayType ls389) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x388 => 
                                                                                                           cvtTYPE_EXPR x388
                                                                                                    ) ls389)))
     | cvtTYPE_EXPR (TypeName(x395, opt397)) = PrettyRep.Ctor ("TypeName", 
          SOME (PrettyRep.Tuple [cvtIDENT_EXPR x395, 
       (case opt397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x396 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x396))
       )]))
     | cvtTYPE_EXPR (ElementTypeRef(x404, n405)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x404, PrettyRep.Int n405]))
     | cvtTYPE_EXPR (FieldTypeRef(x409, x410)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x409, cvtIDENT x410]))
     | cvtTYPE_EXPR (FunctionType x414) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x414))
     | cvtTYPE_EXPR (ObjectType ls418) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x417 => 
                                                                                                             cvtFIELD_TYPE x417
                                                                                                      ) ls418)))
     | cvtTYPE_EXPR (LikeType x424) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x424))
     | cvtTYPE_EXPR (AppType{base=x427, args=ls429}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x427), ("args", PrettyRep.List (List.map (fn x428 => 
                                                                                                     cvtTYPE_EXPR x428
                                                                                              ) ls429))]))
     | cvtTYPE_EXPR (LamType{params=ls441, body=x445}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x440 => 
                                                                          cvtIDENT x440
                                                                   ) ls441)), 
          ("body", cvtTYPE_EXPR x445)]))
     | cvtTYPE_EXPR (NullableType{expr=x453, nullable=b454}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x453), ("nullable", PrettyRep.Bool b454)]))
     | cvtTYPE_EXPR (InstanceType x462) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x462))
     | cvtTYPE_EXPR (TypeVarFixtureRef x465) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x465))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x469) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x469))
     | cvtSTMT (InitStmt{kind=x472, ns=opt474, prototype=b478, static=b479, 
          temps=x480, inits=ls482}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x472), ("ns", 
       (case opt474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x473 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x473))
       )), ("prototype", PrettyRep.Bool b478), ("static", PrettyRep.Bool b479), 
          ("temps", cvtBINDINGS x480), ("inits", PrettyRep.List (List.map (fn x481 => 
                                                                                 cvtINIT_STEP x481
                                                                          ) ls482))]))
     | cvtSTMT (ClassBlock x501) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x501))
     | cvtSTMT (ForInStmt x504) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x504))
     | cvtSTMT (ThrowStmt x507) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x507))
     | cvtSTMT (ReturnStmt x510) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x510))
     | cvtSTMT (BreakStmt opt514) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x513 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x513))
       ))
     | cvtSTMT (ContinueStmt opt521) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x520 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x520))
       ))
     | cvtSTMT (BlockStmt x527) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x527))
     | cvtSTMT (LabeledStmt(x530, x531)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x530, 
          cvtSTMT x531]))
     | cvtSTMT (LetStmt x535) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x535))
     | cvtSTMT (WhileStmt x538) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x538))
     | cvtSTMT (DoWhileStmt x541) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x541))
     | cvtSTMT (ForStmt x544) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x544))
     | cvtSTMT (IfStmt{cnd=x547, thn=x548, els=x549}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x547), ("thn", cvtSTMT x548), 
          ("els", cvtSTMT x549)]))
     | cvtSTMT (WithStmt{obj=x559, ty=x560, body=x561}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x559), ("ty", cvtTYPE_EXPR x560), 
          ("body", cvtSTMT x561)]))
     | cvtSTMT (TryStmt{block=x571, catches=ls573, finally=opt578}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x571), ("catches", PrettyRep.List (List.map (fn x572 => 
                                                                                                     cvtCATCH_CLAUSE x572
                                                                                              ) ls573)), 
          ("finally", 
       (case opt578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x577 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x577))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x591, labels=ls593, cases=ls598}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x591), ("labels", PrettyRep.List (List.map (fn x592 => 
                                                                                                  cvtIDENT x592
                                                                                           ) ls593)), 
          ("cases", PrettyRep.List (List.map (fn x597 => cvtCASE x597
                                             ) ls598))]))
     | cvtSTMT (SwitchTypeStmt{cond=x611, ty=x612, cases=ls614}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x611), ("ty", cvtTYPE_EXPR x612), 
          ("cases", PrettyRep.List (List.map (fn x613 => cvtCATCH_CLAUSE x613
                                             ) ls614))]))
     | cvtSTMT (DXNStmt{expr=x627}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x627)]))
   and cvtEXPR (TernaryExpr(x633, x634, x635)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x633, cvtEXPR x634, cvtEXPR x635]))
     | cvtEXPR (BinaryExpr(x639, x640, x641)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x639, cvtEXPR x640, cvtEXPR x641]))
     | cvtEXPR (BinaryTypeExpr(x645, x646, x647)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x645, cvtEXPR x646, cvtTYPE_EXPR x647]))
     | cvtEXPR (UnaryExpr(x651, x652)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x651, 
          cvtEXPR x652]))
     | cvtEXPR (TypeExpr x656) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x656))
     | cvtEXPR (ThisExpr opt660) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt660 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x659 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x659))
       ))
     | cvtEXPR (YieldExpr opt667) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x666 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x666))
       ))
     | cvtEXPR (SuperExpr opt674) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt674 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x673 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x673))
       ))
     | cvtEXPR (CallExpr{func=x680, actuals=ls682}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x680), ("actuals", PrettyRep.List (List.map (fn x681 => 
                                                                                                   cvtEXPR x681
                                                                                            ) ls682))]))
     | cvtEXPR (ApplyTypeExpr{expr=x693, actuals=ls695}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x693), ("actuals", PrettyRep.List (List.map (fn x694 => 
                                                                                                   cvtTYPE_EXPR x694
                                                                                            ) ls695))]))
     | cvtEXPR (LetExpr{defs=x706, body=x707, head=opt709}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x706), ("body", cvtEXPR x707), 
          ("head", 
       (case opt709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x708 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x708))
       ))]))
     | cvtEXPR (NewExpr{obj=x722, actuals=ls724}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x722), ("actuals", PrettyRep.List (List.map (fn x723 => 
                                                                                                  cvtEXPR x723
                                                                                           ) ls724))]))
     | cvtEXPR (ObjectRef{base=x735, ident=x736, loc=opt738}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x735), ("ident", cvtIDENT_EXPR x736), 
          ("loc", 
       (case opt738 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x737 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x737))
       ))]))
     | cvtEXPR (LexicalRef{ident=x751, loc=opt753}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x751), ("loc", 
       (case opt753 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x752 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x752))
       ))]))
     | cvtEXPR (SetExpr(x764, x765, x766)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x764, 
          cvtEXPR x765, cvtEXPR x766]))
     | cvtEXPR (ListExpr ls771) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x770 => 
                                                                                                    cvtEXPR x770
                                                                                             ) ls771)))
     | cvtEXPR (InitExpr(x777, x778, x779)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x777, 
          cvtHEAD x778, cvtINITS x779]))
     | cvtEXPR (GetTemp n783) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n783))
     | cvtEXPR (GetParam n786) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n786))
     | cvtEXPR (Comprehension(x789, ls791, opt796)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x789, PrettyRep.List (List.map (fn x790 => 
                                                                               cvtFOR_ENUM_HEAD x790
                                                                        ) ls791), 
          
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x795))
       )]))
     | cvtEXPR (LiteralExpr x803) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x803))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n811) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n811))
     | cvtFIXTURE_NAME (PropName x814) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x814))
   and cvtIDENT_EXPR (Identifier{ident=x817, openNamespaces=ls823}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x817), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls819 => PrettyRep.List (List.map (fn x818 => 
                                                                                cvtNAMESPACE x818
                                                                         ) ls819)
                                   ) ls823))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x834, expr=x835}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x834), ("expr", cvtEXPR x835)]))
     | cvtIDENT_EXPR (AttributeIdentifier x843) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x843))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x846, openNamespaces=ls852}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x846), ("openNamespaces", PrettyRep.List (List.map (fn ls848 => 
                                                                            PrettyRep.List (List.map (fn x847 => 
                                                                                                            cvtNAMESPACE x847
                                                                                                     ) ls848)
                                                                     ) ls852))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x863, ident=s864}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x863), ("ident", PrettyRep.UniStr s864)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r874) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r874))
     | cvtLITERAL (LiteralDecimal d877) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d877))
     | cvtLITERAL (LiteralBoolean b880) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b880))
     | cvtLITERAL (LiteralString s883) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s883))
     | cvtLITERAL (LiteralArray{exprs=x886, ty=opt888}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x886), ("ty", 
       (case opt888 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x887 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x887))
       ))]))
     | cvtLITERAL (LiteralXML ls900) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x899 => 
                                                                                                           cvtEXPR x899
                                                                                                    ) ls900)))
     | cvtLITERAL (LiteralNamespace x906) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x906))
     | cvtLITERAL (LiteralObject{expr=ls910, ty=opt915}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x909 => 
                                                                        cvtFIELD x909
                                                                 ) ls910)), 
          ("ty", 
       (case opt915 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x914 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x914))
       ))]))
     | cvtLITERAL (LiteralFunction x926) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x926))
     | cvtLITERAL (LiteralRegExp{str=s929}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s929)]))
   and cvtBLOCK (Block x935) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x935))
   and cvtFIXTURE (NamespaceFixture x938) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x938))
     | cvtFIXTURE (ClassFixture x941) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x941))
     | cvtFIXTURE (InterfaceFixture x944) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x944))
     | cvtFIXTURE (TypeVarFixture x947) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x947))
     | cvtFIXTURE (TypeFixture x950) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x950))
     | cvtFIXTURE (MethodFixture{func=x953, ty=x954, readOnly=b955, override=b956, 
          final=b957}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x953), ("ty", cvtTYPE_EXPR x954), ("readOnly", PrettyRep.Bool b955), 
          ("override", PrettyRep.Bool b956), ("final", PrettyRep.Bool b957)]))
     | cvtFIXTURE (ValFixture{ty=x971, readOnly=b972}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x971), ("readOnly", PrettyRep.Bool b972)]))
     | cvtFIXTURE (VirtualValFixture{ty=x980, getter=opt982, setter=opt987}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x980), ("getter", 
       (case opt982 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x981 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x981))
       )), ("setter", 
       (case opt987 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x986 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x986))
       ))]))
   and cvtHEAD (Head(x1000, x1001)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1000, 
          cvtINITS x1001]))
   and cvtBINDINGS (ls1006, ls1011) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1005 => 
                                                                                       cvtBINDING x1005
                                                                                ) ls1006), 
          PrettyRep.List (List.map (fn x1010 => cvtINIT_STEP x1010
                                   ) ls1011)]
   and cvtRIB ls1019 = PrettyRep.List (List.map (fn (x1016, x1017) => PrettyRep.Tuple [cvtFIXTURE_NAME x1016, 
                                                       cvtFIXTURE x1017]
                                                ) ls1019)
   and cvtRIBS ls1024 = PrettyRep.List (List.map (fn x1023 => cvtRIB x1023
                                                 ) ls1024)
   and cvtINITS ls1031 = PrettyRep.List (List.map (fn (x1028, x1029) => PrettyRep.Tuple [cvtFIXTURE_NAME x1028, 
                                                         cvtEXPR x1029]
                                                  ) ls1031)
   and cvtINSTANCE_TYPE {name=x1035, typeParams=ls1037, typeArgs=ls1042, nonnullable=b1046, 
          superTypes=ls1048, ty=x1052, dynamic=b1053} = PrettyRep.Rec [("name", 
          cvtNAME x1035), ("typeParams", PrettyRep.List (List.map (fn x1036 => 
                                                                         cvtIDENT x1036
                                                                  ) ls1037)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1041 => cvtTYPE_EXPR x1041
                                                ) ls1042)), ("nonnullable", 
          PrettyRep.Bool b1046), ("superTypes", PrettyRep.List (List.map (fn x1047 => 
                                                                                cvtTYPE_EXPR x1047
                                                                         ) ls1048)), 
          ("ty", cvtTYPE_EXPR x1052), ("dynamic", PrettyRep.Bool b1053)]
   and cvtFIELD {kind=x1069, name=x1070, init=x1071} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1069), ("name", cvtIDENT_EXPR x1070), ("init", cvtEXPR x1071)]
   and cvtFIELD_TYPE {name=x1079, ty=x1080} = PrettyRep.Rec [("name", cvtIDENT x1079), 
          ("ty", cvtTYPE_EXPR x1080)]
   and cvtFUNC_TYPE {params=ls1087, result=x1091, thisType=opt1093, hasRest=b1097, 
          minArgs=n1098} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1086 => 
                                                                                     cvtTYPE_EXPR x1086
                                                                              ) ls1087)), 
          ("result", cvtTYPE_EXPR x1091), ("thisType", 
       (case opt1093 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1092 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1092))
       )), ("hasRest", PrettyRep.Bool b1097), ("minArgs", PrettyRep.Int n1098)]
   and cvtFUNC_DEFN {kind=x1110, ns=opt1112, final=b1116, override=b1117, prototype=b1118, 
          static=b1119, func=x1120} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1110), 
          ("ns", 
       (case opt1112 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1111 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1111))
       )), ("final", PrettyRep.Bool b1116), ("override", PrettyRep.Bool b1117), 
          ("prototype", PrettyRep.Bool b1118), ("static", PrettyRep.Bool b1119), 
          ("func", cvtFUNC x1120)]
   and cvtCTOR_DEFN x1136 = cvtCTOR x1136
   and cvtVAR_DEFN {kind=x1137, ns=opt1139, static=b1143, prototype=b1144, 
          bindings=(ls1146, ls1151)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1137), 
          ("ns", 
       (case opt1139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1138 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1138))
       )), ("static", PrettyRep.Bool b1143), ("prototype", PrettyRep.Bool b1144), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1145 => 
                                                                        cvtBINDING x1145
                                                                 ) ls1146), 
          PrettyRep.List (List.map (fn x1150 => cvtINIT_STEP x1150
                                   ) ls1151)])]
   and cvtNAMESPACE_DEFN {ident=x1167, ns=opt1169, init=opt1174} = PrettyRep.Rec [("ident", 
          cvtIDENT x1167), ("ns", 
       (case opt1169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1168 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1168))
       )), ("init", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1173))
       ))]
   and cvtCLASS_DEFN {ns=opt1186, privateNS=x1190, protectedNS=x1191, ident=x1192, 
          nonnullable=b1193, dynamic=b1194, final=b1195, params=ls1197, extends=opt1202, 
          implements=ls1207, classDefns=ls1212, instanceDefns=ls1217, instanceStmts=ls1222, 
          ctorDefn=opt1227} = PrettyRep.Rec [("ns", 
       (case opt1186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1185 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1185))
       )), ("privateNS", cvtNAMESPACE x1190), ("protectedNS", cvtNAMESPACE x1191), 
          ("ident", cvtIDENT x1192), ("nonnullable", PrettyRep.Bool b1193), 
          ("dynamic", PrettyRep.Bool b1194), ("final", PrettyRep.Bool b1195), 
          ("params", PrettyRep.List (List.map (fn x1196 => cvtIDENT x1196
                                              ) ls1197)), ("extends", 
       (case opt1202 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1201 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1201))
       )), ("implements", PrettyRep.List (List.map (fn x1206 => cvtTYPE_EXPR x1206
                                                   ) ls1207)), ("classDefns", 
          PrettyRep.List (List.map (fn x1211 => cvtDEFN x1211
                                   ) ls1212)), ("instanceDefns", PrettyRep.List (List.map (fn x1216 => 
                                                                                                 cvtDEFN x1216
                                                                                          ) ls1217)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1221 => cvtSTMT x1221
                                                     ) ls1222)), ("ctorDefn", 
          
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1226))
       ))]
   and cvtINTERFACE_DEFN {ident=x1260, ns=opt1262, nonnullable=b1266, params=ls1268, 
          extends=ls1273, instanceDefns=ls1278} = PrettyRep.Rec [("ident", 
          cvtIDENT x1260), ("ns", 
       (case opt1262 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1261 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1261))
       )), ("nonnullable", PrettyRep.Bool b1266), ("params", PrettyRep.List (List.map (fn x1267 => 
                                                                                             cvtIDENT x1267
                                                                                      ) ls1268)), 
          ("extends", PrettyRep.List (List.map (fn x1272 => cvtTYPE_EXPR x1272
                                               ) ls1273)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1277 => cvtDEFN x1277
                                   ) ls1278))]
   and cvtTYPE_DEFN {ident=x1295, ns=opt1297, init=x1301} = PrettyRep.Rec [("ident", 
          cvtIDENT x1295), ("ns", 
       (case opt1297 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1296 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1296))
       )), ("init", cvtTYPE_EXPR x1301)]
   and cvtCLASS_BLOCK {ns=opt1310, protectedNS=x1314, privateNS=x1315, ident=x1316, 
          name=opt1318, block=x1322} = PrettyRep.Rec [("ns", 
       (case opt1310 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1309 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1309))
       )), ("protectedNS", cvtNAMESPACE x1314), ("privateNS", cvtNAMESPACE x1315), 
          ("ident", cvtIDENT x1316), ("name", 
       (case opt1318 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1317 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1317))
       )), ("block", cvtBLOCK x1322)]
   and cvtFOR_ENUM_HEAD {isEach=b1336, bindings=(ls1338, ls1343), expr=x1348} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1336), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1337 => 
                                                                                                                         cvtBINDING x1337
                                                                                                                  ) ls1338), 
          PrettyRep.List (List.map (fn x1342 => cvtINIT_STEP x1342
                                   ) ls1343)]), ("expr", cvtEXPR x1348)]
   and cvtFOR_ENUM_STMT {isEach=b1356, defn=opt1387, obj=x1391, rib=opt1399, 
          next=x1403, labels=ls1405, body=x1409} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1356), ("defn", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1357, ns=opt1359, static=b1363, prototype=b1364, bindings=(ls1366, 
            ls1371)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1357), ("ns", 
         (case opt1359 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1358))
         )), ("static", PrettyRep.Bool b1363), ("prototype", PrettyRep.Bool b1364), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1365 => 
                                                                          cvtBINDING x1365
                                                                   ) ls1366), 
            PrettyRep.List (List.map (fn x1370 => cvtINIT_STEP x1370
                                     ) ls1371)])]))
       )), ("obj", cvtEXPR x1391), ("rib", 
       (case opt1399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1395 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1392, 
                                                                                      x1393) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1392, 
                                                                                      cvtFIXTURE x1393]
                                                                               ) ls1395)))
       )), ("next", cvtSTMT x1403), ("labels", PrettyRep.List (List.map (fn x1404 => 
                                                                               cvtIDENT x1404
                                                                        ) ls1405)), 
          ("body", cvtSTMT x1409)]
   and cvtFOR_STMT {rib=opt1432, defn=opt1466, init=ls1471, cond=x1475, update=x1476, 
          labels=ls1478, body=x1482} = PrettyRep.Rec [("rib", 
       (case opt1432 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1428 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1425, 
                                                                                      x1426) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1425, 
                                                                                      cvtFIXTURE x1426]
                                                                               ) ls1428)))
       )), ("defn", 
       (case opt1466 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1436, ns=opt1438, static=b1442, prototype=b1443, bindings=(ls1445, 
            ls1450)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1436), ("ns", 
         (case opt1438 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1437))
         )), ("static", PrettyRep.Bool b1442), ("prototype", PrettyRep.Bool b1443), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1444 => 
                                                                          cvtBINDING x1444
                                                                   ) ls1445), 
            PrettyRep.List (List.map (fn x1449 => cvtINIT_STEP x1449
                                     ) ls1450)])]))
       )), ("init", PrettyRep.List (List.map (fn x1470 => cvtSTMT x1470
                                             ) ls1471)), ("cond", cvtEXPR x1475), 
          ("update", cvtEXPR x1476), ("labels", PrettyRep.List (List.map (fn x1477 => 
                                                                                cvtIDENT x1477
                                                                         ) ls1478)), 
          ("body", cvtSTMT x1482)]
   and cvtWHILE_STMT {cond=x1498, rib=opt1506, body=x1510, labels=ls1512} = 
          PrettyRep.Rec [("cond", cvtEXPR x1498), ("rib", 
       (case opt1506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1502 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1499, 
                                                                                      x1500) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1499, 
                                                                                      cvtFIXTURE x1500]
                                                                               ) ls1502)))
       )), ("body", cvtSTMT x1510), ("labels", PrettyRep.List (List.map (fn x1511 => 
                                                                               cvtIDENT x1511
                                                                        ) ls1512))]
   and cvtDIRECTIVES {pragmas=ls1526, defns=ls1531, head=opt1536, body=ls1541, 
          loc=opt1546} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1525 => 
                                                                                    cvtPRAGMA x1525
                                                                             ) ls1526)), 
          ("defns", PrettyRep.List (List.map (fn x1530 => cvtDEFN x1530
                                             ) ls1531)), ("head", 
       (case opt1536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1535 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1535))
       )), ("body", PrettyRep.List (List.map (fn x1540 => cvtSTMT x1540
                                             ) ls1541)), ("loc", 
       (case opt1546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1545 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1545))
       ))]
   and cvtCASE {label=opt1562, inits=opt1573, body=x1577} = PrettyRep.Rec [("label", 
          
       (case opt1562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1561 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1561))
       )), ("inits", 
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1569 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1566, 
                                                                                      x1567) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1566, 
                                                                                      cvtEXPR x1567]
                                                                               ) ls1569)))
       )), ("body", cvtBLOCK x1577)]
   and cvtCATCH_CLAUSE {bindings=(ls1586, ls1591), ty=x1596, rib=opt1604, inits=opt1615, 
          block=x1619} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1585 => 
                                                                                                      cvtBINDING x1585
                                                                                               ) ls1586), 
          PrettyRep.List (List.map (fn x1590 => cvtINIT_STEP x1590
                                   ) ls1591)]), ("ty", cvtTYPE_EXPR x1596), 
          ("rib", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1600 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1597, 
                                                                                      x1598) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1597, 
                                                                                      cvtFIXTURE x1598]
                                                                               ) ls1600)))
       )), ("inits", 
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1611 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1608, 
                                                                                      x1609) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1608, 
                                                                                      cvtEXPR x1609]
                                                                               ) ls1611)))
       )), ("block", cvtBLOCK x1619)]
   and cvtFUNC_NAME {kind=x1631, ident=x1632} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1631), 
          ("ident", cvtIDENT x1632)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1638, getter=opt1640, setter=opt1645} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1638), ("getter", 
       (case opt1640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1639 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1639))
       )), ("setter", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1644 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1644))
       ))]
   and cvtFRAGMENT (Anon x1656) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1656))
end

