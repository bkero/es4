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
     | cvtNAMESPACE (TransparentNamespace s24) = PrettyRep.Ctor ("TransparentNamespace", 
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
     | cvtTYPE_EXPR (AppType{base=x424, args=ls426}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x424), ("args", PrettyRep.List (List.map (fn x425 => 
                                                                                                     cvtTYPE_EXPR x425
                                                                                              ) ls426))]))
     | cvtTYPE_EXPR (LamType{params=ls438, body=x442}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x437 => 
                                                                          cvtIDENT x437
                                                                   ) ls438)), 
          ("body", cvtTYPE_EXPR x442)]))
     | cvtTYPE_EXPR (NullableType{expr=x450, nullable=b451}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x450), ("nullable", PrettyRep.Bool b451)]))
     | cvtTYPE_EXPR (InstanceType x459) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x459))
     | cvtTYPE_EXPR (TypeVarFixtureRef x462) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x462))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x466) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x466))
     | cvtSTMT (InitStmt{kind=x469, ns=opt471, prototype=b475, static=b476, 
          temps=x477, inits=ls479}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x469), ("ns", 
       (case opt471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x470 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x470))
       )), ("prototype", PrettyRep.Bool b475), ("static", PrettyRep.Bool b476), 
          ("temps", cvtBINDINGS x477), ("inits", PrettyRep.List (List.map (fn x478 => 
                                                                                 cvtINIT_STEP x478
                                                                          ) ls479))]))
     | cvtSTMT (ClassBlock x498) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x498))
     | cvtSTMT (ForInStmt x501) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x501))
     | cvtSTMT (ThrowStmt x504) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x504))
     | cvtSTMT (ReturnStmt x507) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x507))
     | cvtSTMT (BreakStmt opt511) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x510 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x510))
       ))
     | cvtSTMT (ContinueStmt opt518) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt518 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x517 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x517))
       ))
     | cvtSTMT (BlockStmt x524) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x524))
     | cvtSTMT (LabeledStmt(x527, x528)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x527, 
          cvtSTMT x528]))
     | cvtSTMT (LetStmt x532) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x532))
     | cvtSTMT (WhileStmt x535) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x535))
     | cvtSTMT (DoWhileStmt x538) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x538))
     | cvtSTMT (ForStmt x541) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x541))
     | cvtSTMT (IfStmt{cnd=x544, thn=x545, els=x546}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x544), ("thn", cvtSTMT x545), 
          ("els", cvtSTMT x546)]))
     | cvtSTMT (WithStmt{obj=x556, ty=x557, body=x558}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x556), ("ty", cvtTYPE_EXPR x557), 
          ("body", cvtSTMT x558)]))
     | cvtSTMT (TryStmt{block=x568, catches=ls570, finally=opt575}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x568), ("catches", PrettyRep.List (List.map (fn x569 => 
                                                                                                     cvtCATCH_CLAUSE x569
                                                                                              ) ls570)), 
          ("finally", 
       (case opt575 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x574 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x574))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x588, labels=ls590, cases=ls595}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x588), ("labels", PrettyRep.List (List.map (fn x589 => 
                                                                                                  cvtIDENT x589
                                                                                           ) ls590)), 
          ("cases", PrettyRep.List (List.map (fn x594 => cvtCASE x594
                                             ) ls595))]))
     | cvtSTMT (SwitchTypeStmt{cond=x608, ty=x609, cases=ls611}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x608), ("ty", cvtTYPE_EXPR x609), 
          ("cases", PrettyRep.List (List.map (fn x610 => cvtCATCH_CLAUSE x610
                                             ) ls611))]))
     | cvtSTMT (DXNStmt{expr=x624}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x624)]))
   and cvtEXPR (TernaryExpr(x630, x631, x632)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x630, cvtEXPR x631, cvtEXPR x632]))
     | cvtEXPR (BinaryExpr(x636, x637, x638)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x636, cvtEXPR x637, cvtEXPR x638]))
     | cvtEXPR (BinaryTypeExpr(x642, x643, x644)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x642, cvtEXPR x643, cvtTYPE_EXPR x644]))
     | cvtEXPR (UnaryExpr(x648, x649)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x648, 
          cvtEXPR x649]))
     | cvtEXPR (TypeExpr x653) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x653))
     | cvtEXPR (ThisExpr opt657) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt657 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x656 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x656))
       ))
     | cvtEXPR (YieldExpr opt664) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt664 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x663 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x663))
       ))
     | cvtEXPR (SuperExpr opt671) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x670 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x670))
       ))
     | cvtEXPR (CallExpr{func=x677, actuals=ls679}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x677), ("actuals", PrettyRep.List (List.map (fn x678 => 
                                                                                                   cvtEXPR x678
                                                                                            ) ls679))]))
     | cvtEXPR (ApplyTypeExpr{expr=x690, actuals=ls692}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x690), ("actuals", PrettyRep.List (List.map (fn x691 => 
                                                                                                   cvtTYPE_EXPR x691
                                                                                            ) ls692))]))
     | cvtEXPR (LetExpr{defs=x703, body=x704, head=opt706}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x703), ("body", cvtEXPR x704), 
          ("head", 
       (case opt706 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x705 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x705))
       ))]))
     | cvtEXPR (NewExpr{obj=x719, actuals=ls721}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x719), ("actuals", PrettyRep.List (List.map (fn x720 => 
                                                                                                  cvtEXPR x720
                                                                                           ) ls721))]))
     | cvtEXPR (ObjectRef{base=x732, ident=x733, loc=opt735}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x732), ("ident", cvtIDENT_EXPR x733), 
          ("loc", 
       (case opt735 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x734 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x734))
       ))]))
     | cvtEXPR (LexicalRef{ident=x748, loc=opt750}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x748), ("loc", 
       (case opt750 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x749 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x749))
       ))]))
     | cvtEXPR (SetExpr(x761, x762, x763)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x761, 
          cvtEXPR x762, cvtEXPR x763]))
     | cvtEXPR (ListExpr ls768) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x767 => 
                                                                                                    cvtEXPR x767
                                                                                             ) ls768)))
     | cvtEXPR (InitExpr(x774, x775, x776)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x774, 
          cvtHEAD x775, cvtINITS x776]))
     | cvtEXPR (GetTemp n780) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n780))
     | cvtEXPR (GetParam n783) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n783))
     | cvtEXPR (Comprehension(x786, ls788, opt793)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x786, PrettyRep.List (List.map (fn x787 => 
                                                                               cvtFOR_ENUM_HEAD x787
                                                                        ) ls788), 
          
       (case opt793 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x792 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x792))
       )]))
     | cvtEXPR (LiteralExpr x800) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x800))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n808) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n808))
     | cvtFIXTURE_NAME (PropName x811) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x811))
   and cvtIDENT_EXPR (Identifier{ident=x814, openNamespaces=ls820}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x814), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls816 => PrettyRep.List (List.map (fn x815 => 
                                                                                cvtNAMESPACE x815
                                                                         ) ls816)
                                   ) ls820))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x831, expr=x832}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x831), ("expr", cvtEXPR x832)]))
     | cvtIDENT_EXPR (AttributeIdentifier x840) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x840))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x843, openNamespaces=ls849}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x843), ("openNamespaces", PrettyRep.List (List.map (fn ls845 => 
                                                                            PrettyRep.List (List.map (fn x844 => 
                                                                                                            cvtNAMESPACE x844
                                                                                                     ) ls845)
                                                                     ) ls849))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x860, ident=s861}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x860), ("ident", PrettyRep.UniStr s861)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r871) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r871))
     | cvtLITERAL (LiteralDecimal d874) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d874))
     | cvtLITERAL (LiteralBoolean b877) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b877))
     | cvtLITERAL (LiteralString s880) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s880))
     | cvtLITERAL (LiteralArray{exprs=x883, ty=opt885}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x883), ("ty", 
       (case opt885 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x884 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x884))
       ))]))
     | cvtLITERAL (LiteralXML ls897) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x896 => 
                                                                                                           cvtEXPR x896
                                                                                                    ) ls897)))
     | cvtLITERAL (LiteralNamespace x903) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x903))
     | cvtLITERAL (LiteralObject{expr=ls907, ty=opt912}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x906 => 
                                                                        cvtFIELD x906
                                                                 ) ls907)), 
          ("ty", 
       (case opt912 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x911 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x911))
       ))]))
     | cvtLITERAL (LiteralFunction x923) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x923))
     | cvtLITERAL (LiteralRegExp{str=s926}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s926)]))
   and cvtBLOCK (Block x932) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x932))
   and cvtFIXTURE (NamespaceFixture x935) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x935))
     | cvtFIXTURE (ClassFixture x938) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x938))
     | cvtFIXTURE (InterfaceFixture x941) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x941))
     | cvtFIXTURE (TypeVarFixture x944) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x944))
     | cvtFIXTURE (TypeFixture x947) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x947))
     | cvtFIXTURE (MethodFixture{func=x950, ty=x951, readOnly=b952, override=b953, 
          final=b954}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x950), ("ty", cvtTYPE_EXPR x951), ("readOnly", PrettyRep.Bool b952), 
          ("override", PrettyRep.Bool b953), ("final", PrettyRep.Bool b954)]))
     | cvtFIXTURE (ValFixture{ty=x968, readOnly=b969}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x968), ("readOnly", PrettyRep.Bool b969)]))
     | cvtFIXTURE (VirtualValFixture{ty=x977, getter=opt979, setter=opt984}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x977), ("getter", 
       (case opt979 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x978 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x978))
       )), ("setter", 
       (case opt984 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x983 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x983))
       ))]))
   and cvtHEAD (Head(x997, x998)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x997, 
          cvtINITS x998]))
   and cvtBINDINGS (ls1003, ls1008) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1002 => 
                                                                                       cvtBINDING x1002
                                                                                ) ls1003), 
          PrettyRep.List (List.map (fn x1007 => cvtINIT_STEP x1007
                                   ) ls1008)]
   and cvtRIB ls1016 = PrettyRep.List (List.map (fn (x1013, x1014) => PrettyRep.Tuple [cvtFIXTURE_NAME x1013, 
                                                       cvtFIXTURE x1014]
                                                ) ls1016)
   and cvtRIBS ls1021 = PrettyRep.List (List.map (fn x1020 => cvtRIB x1020
                                                 ) ls1021)
   and cvtINITS ls1028 = PrettyRep.List (List.map (fn (x1025, x1026) => PrettyRep.Tuple [cvtFIXTURE_NAME x1025, 
                                                         cvtEXPR x1026]
                                                  ) ls1028)
   and cvtINSTANCE_TYPE {name=x1032, typeParams=ls1034, typeArgs=ls1039, nonnullable=b1043, 
          superTypes=ls1045, ty=x1049, dynamic=b1050} = PrettyRep.Rec [("name", 
          cvtNAME x1032), ("typeParams", PrettyRep.List (List.map (fn x1033 => 
                                                                         cvtIDENT x1033
                                                                  ) ls1034)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1038 => cvtTYPE_EXPR x1038
                                                ) ls1039)), ("nonnullable", 
          PrettyRep.Bool b1043), ("superTypes", PrettyRep.List (List.map (fn x1044 => 
                                                                                cvtTYPE_EXPR x1044
                                                                         ) ls1045)), 
          ("ty", cvtTYPE_EXPR x1049), ("dynamic", PrettyRep.Bool b1050)]
   and cvtFIELD {kind=x1066, name=x1067, init=x1068} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1066), ("name", cvtIDENT_EXPR x1067), ("init", cvtEXPR x1068)]
   and cvtFIELD_TYPE {name=x1076, ty=x1077} = PrettyRep.Rec [("name", cvtIDENT x1076), 
          ("ty", cvtTYPE_EXPR x1077)]
   and cvtFUNC_TYPE {params=ls1084, result=x1088, thisType=opt1090, hasRest=b1094, 
          minArgs=n1095} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1083 => 
                                                                                     cvtTYPE_EXPR x1083
                                                                              ) ls1084)), 
          ("result", cvtTYPE_EXPR x1088), ("thisType", 
       (case opt1090 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1089 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1089))
       )), ("hasRest", PrettyRep.Bool b1094), ("minArgs", PrettyRep.Int n1095)]
   and cvtFUNC_DEFN {kind=x1107, ns=opt1109, final=b1113, override=b1114, prototype=b1115, 
          static=b1116, func=x1117} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1107), 
          ("ns", 
       (case opt1109 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1108 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1108))
       )), ("final", PrettyRep.Bool b1113), ("override", PrettyRep.Bool b1114), 
          ("prototype", PrettyRep.Bool b1115), ("static", PrettyRep.Bool b1116), 
          ("func", cvtFUNC x1117)]
   and cvtCTOR_DEFN x1133 = cvtCTOR x1133
   and cvtVAR_DEFN {kind=x1134, ns=opt1136, static=b1140, prototype=b1141, 
          bindings=(ls1143, ls1148)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1134), 
          ("ns", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1135))
       )), ("static", PrettyRep.Bool b1140), ("prototype", PrettyRep.Bool b1141), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1142 => 
                                                                        cvtBINDING x1142
                                                                 ) ls1143), 
          PrettyRep.List (List.map (fn x1147 => cvtINIT_STEP x1147
                                   ) ls1148)])]
   and cvtNAMESPACE_DEFN {ident=x1164, ns=opt1166, init=opt1171} = PrettyRep.Rec [("ident", 
          cvtIDENT x1164), ("ns", 
       (case opt1166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1165 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1165))
       )), ("init", 
       (case opt1171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1170 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1170))
       ))]
   and cvtCLASS_DEFN {ns=opt1183, privateNS=x1187, protectedNS=x1188, ident=x1189, 
          nonnullable=b1190, dynamic=b1191, final=b1192, params=ls1194, extends=opt1199, 
          implements=ls1204, classDefns=ls1209, instanceDefns=ls1214, instanceStmts=ls1219, 
          ctorDefn=opt1224} = PrettyRep.Rec [("ns", 
       (case opt1183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1182 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1182))
       )), ("privateNS", cvtNAMESPACE x1187), ("protectedNS", cvtNAMESPACE x1188), 
          ("ident", cvtIDENT x1189), ("nonnullable", PrettyRep.Bool b1190), 
          ("dynamic", PrettyRep.Bool b1191), ("final", PrettyRep.Bool b1192), 
          ("params", PrettyRep.List (List.map (fn x1193 => cvtIDENT x1193
                                              ) ls1194)), ("extends", 
       (case opt1199 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1198 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1198))
       )), ("implements", PrettyRep.List (List.map (fn x1203 => cvtTYPE_EXPR x1203
                                                   ) ls1204)), ("classDefns", 
          PrettyRep.List (List.map (fn x1208 => cvtDEFN x1208
                                   ) ls1209)), ("instanceDefns", PrettyRep.List (List.map (fn x1213 => 
                                                                                                 cvtDEFN x1213
                                                                                          ) ls1214)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1218 => cvtSTMT x1218
                                                     ) ls1219)), ("ctorDefn", 
          
       (case opt1224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1223 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1223))
       ))]
   and cvtINTERFACE_DEFN {ident=x1257, ns=opt1259, nonnullable=b1263, params=ls1265, 
          extends=ls1270, instanceDefns=ls1275} = PrettyRep.Rec [("ident", 
          cvtIDENT x1257), ("ns", 
       (case opt1259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1258 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1258))
       )), ("nonnullable", PrettyRep.Bool b1263), ("params", PrettyRep.List (List.map (fn x1264 => 
                                                                                             cvtIDENT x1264
                                                                                      ) ls1265)), 
          ("extends", PrettyRep.List (List.map (fn x1269 => cvtTYPE_EXPR x1269
                                               ) ls1270)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1274 => cvtDEFN x1274
                                   ) ls1275))]
   and cvtTYPE_DEFN {ident=x1292, ns=opt1294, init=x1298} = PrettyRep.Rec [("ident", 
          cvtIDENT x1292), ("ns", 
       (case opt1294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1293 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1293))
       )), ("init", cvtTYPE_EXPR x1298)]
   and cvtCLASS_BLOCK {ns=opt1307, protectedNS=x1311, privateNS=x1312, ident=x1313, 
          name=opt1315, block=x1319} = PrettyRep.Rec [("ns", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1306))
       )), ("protectedNS", cvtNAMESPACE x1311), ("privateNS", cvtNAMESPACE x1312), 
          ("ident", cvtIDENT x1313), ("name", 
       (case opt1315 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1314 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1314))
       )), ("block", cvtBLOCK x1319)]
   and cvtFOR_ENUM_HEAD {isEach=b1333, bindings=(ls1335, ls1340), expr=x1345} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1333), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1334 => 
                                                                                                                         cvtBINDING x1334
                                                                                                                  ) ls1335), 
          PrettyRep.List (List.map (fn x1339 => cvtINIT_STEP x1339
                                   ) ls1340)]), ("expr", cvtEXPR x1345)]
   and cvtFOR_ENUM_STMT {isEach=b1353, defn=opt1384, obj=x1388, rib=opt1396, 
          next=x1400, labels=ls1402, body=x1406} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1353), ("defn", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1354, ns=opt1356, static=b1360, prototype=b1361, bindings=(ls1363, 
            ls1368)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1354), ("ns", 
         (case opt1356 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1355))
         )), ("static", PrettyRep.Bool b1360), ("prototype", PrettyRep.Bool b1361), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1362 => 
                                                                          cvtBINDING x1362
                                                                   ) ls1363), 
            PrettyRep.List (List.map (fn x1367 => cvtINIT_STEP x1367
                                     ) ls1368)])]))
       )), ("obj", cvtEXPR x1388), ("rib", 
       (case opt1396 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1392 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1389, 
                                                                                      x1390) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1389, 
                                                                                      cvtFIXTURE x1390]
                                                                               ) ls1392)))
       )), ("next", cvtSTMT x1400), ("labels", PrettyRep.List (List.map (fn x1401 => 
                                                                               cvtIDENT x1401
                                                                        ) ls1402)), 
          ("body", cvtSTMT x1406)]
   and cvtFOR_STMT {rib=opt1429, defn=opt1463, init=ls1468, cond=x1472, update=x1473, 
          labels=ls1475, body=x1479} = PrettyRep.Rec [("rib", 
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
         | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1434))
         )), ("static", PrettyRep.Bool b1439), ("prototype", PrettyRep.Bool b1440), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1441 => 
                                                                          cvtBINDING x1441
                                                                   ) ls1442), 
            PrettyRep.List (List.map (fn x1446 => cvtINIT_STEP x1446
                                     ) ls1447)])]))
       )), ("init", PrettyRep.List (List.map (fn x1467 => cvtSTMT x1467
                                             ) ls1468)), ("cond", cvtEXPR x1472), 
          ("update", cvtEXPR x1473), ("labels", PrettyRep.List (List.map (fn x1474 => 
                                                                                cvtIDENT x1474
                                                                         ) ls1475)), 
          ("body", cvtSTMT x1479)]
   and cvtWHILE_STMT {cond=x1495, rib=opt1503, body=x1507, labels=ls1509} = 
          PrettyRep.Rec [("cond", cvtEXPR x1495), ("rib", 
       (case opt1503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1499 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1496, 
                                                                                      x1497) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1496, 
                                                                                      cvtFIXTURE x1497]
                                                                               ) ls1499)))
       )), ("body", cvtSTMT x1507), ("labels", PrettyRep.List (List.map (fn x1508 => 
                                                                               cvtIDENT x1508
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
       )), ("body", PrettyRep.List (List.map (fn x1537 => cvtSTMT x1537
                                             ) ls1538)), ("loc", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1542))
       ))]
   and cvtCASE {label=opt1559, inits=opt1570, body=x1574} = PrettyRep.Rec [("label", 
          
       (case opt1559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1558 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1558))
       )), ("inits", 
       (case opt1570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1566 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1563, 
                                                                                      x1564) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1563, 
                                                                                      cvtEXPR x1564]
                                                                               ) ls1566)))
       )), ("body", cvtBLOCK x1574)]
   and cvtCATCH_CLAUSE {bindings=(ls1583, ls1588), ty=x1593, rib=opt1601, inits=opt1612, 
          block=x1616} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1582 => 
                                                                                                      cvtBINDING x1582
                                                                                               ) ls1583), 
          PrettyRep.List (List.map (fn x1587 => cvtINIT_STEP x1587
                                   ) ls1588)]), ("ty", cvtTYPE_EXPR x1593), 
          ("rib", 
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
                                                                                      cvtEXPR x1606]
                                                                               ) ls1608)))
       )), ("block", cvtBLOCK x1616)]
   and cvtFUNC_NAME {kind=x1628, ident=x1629} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1628), 
          ("ident", cvtIDENT x1629)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1635, getter=opt1637, setter=opt1642} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1635), ("getter", 
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

