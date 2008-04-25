structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtNONCE n20 = PrettyRep.Int n20
   and cvtNAMESPACE_ID x21 = cvtNONCE x21
   and cvtNAMESPACE (TransparentNamespace s22) = PrettyRep.Ctor ("TransparentNamespace", 
          SOME (PrettyRep.UniStr s22))
     | cvtNAMESPACE (OpaqueNamespace x25) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtNAMESPACE_ID x25))
   and cvtNAME {ns=x28, id=x29} = PrettyRep.Rec [("ns", cvtNAMESPACE x28), 
          ("id", cvtIDENT x29)]
   and cvtMULTINAME {nss=ls40, id=x44} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls36 => 
                                                                                                PrettyRep.List (List.map (fn x35 => 
                                                                                                                                cvtNAMESPACE x35
                                                                                                                         ) ls36)
                                                                                         ) ls40)), 
          ("id", cvtIDENT x44)]
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
   and cvtPRAGMA (UseNamespace x111) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x111))
     | cvtPRAGMA (UseDefaultNamespace x114) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x114))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x125, privateNS=x126, protectedNS=x127, parentProtectedNSs=ls129, 
          typeParams=ls134, nonnullable=b138, dynamic=b139, extends=opt141, 
          implements=ls146, classRib=x150, instanceRib=x151, instanceInits=x152, 
          constructor=opt154, classType=x158, instanceType=x159}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x125), ("privateNS", cvtNAMESPACE x126), 
          ("protectedNS", cvtNAMESPACE x127), ("parentProtectedNSs", PrettyRep.List (List.map (fn x128 => 
                                                                                                     cvtNAMESPACE x128
                                                                                              ) ls129)), 
          ("typeParams", PrettyRep.List (List.map (fn x133 => cvtIDENT x133
                                                  ) ls134)), ("nonnullable", 
          PrettyRep.Bool b138), ("dynamic", PrettyRep.Bool b139), ("extends", 
          
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x140))
       )), ("implements", PrettyRep.List (List.map (fn x145 => cvtTYPE_EXPR x145
                                                   ) ls146)), ("classRib", 
          cvtRIB x150), ("instanceRib", cvtRIB x151), ("instanceInits", cvtHEAD x152), 
          ("constructor", 
       (case opt154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x153 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x153))
       )), ("classType", cvtTYPE_EXPR x158), ("instanceType", cvtTYPE_EXPR x159)]))
   and cvtIFACE (Iface{name=x193, typeParams=ls195, nonnullable=b199, extends=ls201, 
          instanceRib=x205, instanceType=x206}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x193), ("typeParams", PrettyRep.List (List.map (fn x194 => 
                                                                                                      cvtIDENT x194
                                                                                               ) ls195)), 
          ("nonnullable", PrettyRep.Bool b199), ("extends", PrettyRep.List (List.map (fn x200 => 
                                                                                            cvtTYPE_EXPR x200
                                                                                     ) ls201)), 
          ("instanceRib", cvtRIB x205), ("instanceType", cvtTYPE_EXPR x206)]))
   and cvtCTOR (Ctor{settings=x222, superArgs=ls224, func=x228}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x222), ("superArgs", PrettyRep.List (List.map (fn x223 => 
                                                                                                         cvtEXPR x223
                                                                                                  ) ls224)), 
          ("func", cvtFUNC x228)]))
   and cvtFUNC (Func{name=x238, fsig=x239, native=b240, block=opt242, param=x246, 
          defaults=ls248, ty=x252, loc=opt254}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x238), ("fsig", cvtFUNC_SIG x239), ("native", PrettyRep.Bool b240), 
          ("block", 
       (case opt242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x241 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x241))
       )), ("param", cvtHEAD x246), ("defaults", PrettyRep.List (List.map (fn x247 => 
                                                                                 cvtEXPR x247
                                                                          ) ls248)), 
          ("ty", cvtTYPE_EXPR x252), ("loc", 
       (case opt254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x253 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x253))
       ))]))
   and cvtDEFN (ClassDefn x277) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x277))
     | cvtDEFN (VariableDefn x280) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x280))
     | cvtDEFN (FunctionDefn x283) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x283))
     | cvtDEFN (ConstructorDefn x286) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x286))
     | cvtDEFN (InterfaceDefn x289) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x289))
     | cvtDEFN (NamespaceDefn x292) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x292))
     | cvtDEFN (TypeDefn x295) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x295))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls299, params=x303, paramTypes=ls305, 
          defaults=ls310, ctorInits=opt321, returnType=x325, thisType=opt327, 
          hasRest=b331}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x298 => cvtIDENT x298
                                   ) ls299)), ("params", cvtBINDINGS x303), 
          ("paramTypes", PrettyRep.List (List.map (fn x304 => cvtTYPE_EXPR x304
                                                  ) ls305)), ("defaults", PrettyRep.List (List.map (fn x309 => 
                                                                                                          cvtEXPR x309
                                                                                                   ) ls310)), 
          ("ctorInits", 
       (case opt321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x314, ls316) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x314, 
            PrettyRep.List (List.map (fn x315 => cvtEXPR x315
                                     ) ls316)]))
       )), ("returnType", cvtTYPE_EXPR x325), ("thisType", 
       (case opt327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x326 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x326))
       )), ("hasRest", PrettyRep.Bool b331)]))
   and cvtBINDING (Binding{ident=x351, ty=x352}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x351), ("ty", cvtTYPE_EXPR x352)]))
   and cvtBINDING_IDENT (TempIdent n360) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n360))
     | cvtBINDING_IDENT (ParamIdent n363) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n363))
     | cvtBINDING_IDENT (PropIdent x366) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x366))
   and cvtINIT_STEP (InitStep(x369, x370)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x369, 
          cvtEXPR x370]))
     | cvtINIT_STEP (AssignStep(x374, x375)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x374, cvtEXPR x375]))
   and cvtTYPE_EXPR (SpecialType x379) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x379))
     | cvtTYPE_EXPR (UnionType ls383) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x382 => 
                                                                                                           cvtTYPE_EXPR x382
                                                                                                    ) ls383)))
     | cvtTYPE_EXPR (ArrayType ls390) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x389 => 
                                                                                                           cvtTYPE_EXPR x389
                                                                                                    ) ls390)))
     | cvtTYPE_EXPR (TypeName(x396, opt398)) = PrettyRep.Ctor ("TypeName", 
          SOME (PrettyRep.Tuple [cvtIDENT_EXPR x396, 
       (case opt398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x397 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x397))
       )]))
     | cvtTYPE_EXPR (ElementTypeRef(x405, n406)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x405, PrettyRep.Int n406]))
     | cvtTYPE_EXPR (FieldTypeRef(x410, x411)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x410, cvtIDENT x411]))
     | cvtTYPE_EXPR (FunctionType x415) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x415))
     | cvtTYPE_EXPR (ObjectType ls419) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x418 => 
                                                                                                             cvtFIELD_TYPE x418
                                                                                                      ) ls419)))
     | cvtTYPE_EXPR (LikeType x425) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x425))
     | cvtTYPE_EXPR (AppType{base=x428, args=ls430}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x428), ("args", PrettyRep.List (List.map (fn x429 => 
                                                                                                     cvtTYPE_EXPR x429
                                                                                              ) ls430))]))
     | cvtTYPE_EXPR (LamType{params=ls442, body=x446}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x441 => 
                                                                          cvtIDENT x441
                                                                   ) ls442)), 
          ("body", cvtTYPE_EXPR x446)]))
     | cvtTYPE_EXPR (NullableType{expr=x454, nullable=b455}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x454), ("nullable", PrettyRep.Bool b455)]))
     | cvtTYPE_EXPR (InstanceType x463) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x463))
     | cvtTYPE_EXPR (TypeVarFixtureRef x466) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x466))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x470) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x470))
     | cvtSTMT (InitStmt{kind=x473, ns=opt475, prototype=b479, static=b480, 
          temps=x481, inits=ls483}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x473), ("ns", 
       (case opt475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x474 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x474))
       )), ("prototype", PrettyRep.Bool b479), ("static", PrettyRep.Bool b480), 
          ("temps", cvtBINDINGS x481), ("inits", PrettyRep.List (List.map (fn x482 => 
                                                                                 cvtINIT_STEP x482
                                                                          ) ls483))]))
     | cvtSTMT (ClassBlock x502) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x502))
     | cvtSTMT (ForInStmt x505) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x505))
     | cvtSTMT (ThrowStmt x508) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x508))
     | cvtSTMT (ReturnStmt x511) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x511))
     | cvtSTMT (BreakStmt opt515) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt515 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x514 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x514))
       ))
     | cvtSTMT (ContinueStmt opt522) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt522 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x521 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x521))
       ))
     | cvtSTMT (BlockStmt x528) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x528))
     | cvtSTMT (LabeledStmt(x531, x532)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x531, 
          cvtSTMT x532]))
     | cvtSTMT (LetStmt x536) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x536))
     | cvtSTMT (WhileStmt x539) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x539))
     | cvtSTMT (DoWhileStmt x542) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x542))
     | cvtSTMT (ForStmt x545) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x545))
     | cvtSTMT (IfStmt{cnd=x548, thn=x549, els=x550}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x548), ("thn", cvtSTMT x549), 
          ("els", cvtSTMT x550)]))
     | cvtSTMT (WithStmt{obj=x560, ty=x561, body=x562}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x560), ("ty", cvtTYPE_EXPR x561), 
          ("body", cvtSTMT x562)]))
     | cvtSTMT (TryStmt{block=x572, catches=ls574, finally=opt579}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x572), ("catches", PrettyRep.List (List.map (fn x573 => 
                                                                                                     cvtCATCH_CLAUSE x573
                                                                                              ) ls574)), 
          ("finally", 
       (case opt579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x578 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x578))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x592, labels=ls594, cases=ls599}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x592), ("labels", PrettyRep.List (List.map (fn x593 => 
                                                                                                  cvtIDENT x593
                                                                                           ) ls594)), 
          ("cases", PrettyRep.List (List.map (fn x598 => cvtCASE x598
                                             ) ls599))]))
     | cvtSTMT (SwitchTypeStmt{cond=x612, ty=x613, cases=ls615}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x612), ("ty", cvtTYPE_EXPR x613), 
          ("cases", PrettyRep.List (List.map (fn x614 => cvtCATCH_CLAUSE x614
                                             ) ls615))]))
     | cvtSTMT (DXNStmt{expr=x628}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x628)]))
   and cvtEXPR (TernaryExpr(x634, x635, x636)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x634, cvtEXPR x635, cvtEXPR x636]))
     | cvtEXPR (BinaryExpr(x640, x641, x642)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x640, cvtEXPR x641, cvtEXPR x642]))
     | cvtEXPR (BinaryTypeExpr(x646, x647, x648)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x646, cvtEXPR x647, cvtTYPE_EXPR x648]))
     | cvtEXPR (UnaryExpr(x652, x653)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x652, 
          cvtEXPR x653]))
     | cvtEXPR (TypeExpr x657) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x657))
     | cvtEXPR (ThisExpr opt661) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x660 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x660))
       ))
     | cvtEXPR (YieldExpr opt668) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x667 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x667))
       ))
     | cvtEXPR (SuperExpr opt675) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x674 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x674))
       ))
     | cvtEXPR (CallExpr{func=x681, actuals=ls683}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x681), ("actuals", PrettyRep.List (List.map (fn x682 => 
                                                                                                   cvtEXPR x682
                                                                                            ) ls683))]))
     | cvtEXPR (ApplyTypeExpr{expr=x694, actuals=ls696}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x694), ("actuals", PrettyRep.List (List.map (fn x695 => 
                                                                                                   cvtTYPE_EXPR x695
                                                                                            ) ls696))]))
     | cvtEXPR (LetExpr{defs=x707, body=x708, head=opt710}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x707), ("body", cvtEXPR x708), 
          ("head", 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x709 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x709))
       ))]))
     | cvtEXPR (NewExpr{obj=x723, actuals=ls725}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x723), ("actuals", PrettyRep.List (List.map (fn x724 => 
                                                                                                  cvtEXPR x724
                                                                                           ) ls725))]))
     | cvtEXPR (ObjectRef{base=x736, ident=x737, loc=opt739}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x736), ("ident", cvtIDENT_EXPR x737), 
          ("loc", 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x738))
       ))]))
     | cvtEXPR (LexicalRef{ident=x752, loc=opt754}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x752), ("loc", 
       (case opt754 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x753 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x753))
       ))]))
     | cvtEXPR (SetExpr(x765, x766, x767)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x765, 
          cvtEXPR x766, cvtEXPR x767]))
     | cvtEXPR (ListExpr ls772) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x771 => 
                                                                                                    cvtEXPR x771
                                                                                             ) ls772)))
     | cvtEXPR (InitExpr(x778, x779, x780)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x778, 
          cvtHEAD x779, cvtINITS x780]))
     | cvtEXPR (GetTemp n784) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n784))
     | cvtEXPR (GetParam n787) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n787))
     | cvtEXPR (Comprehension(x790, ls792, opt797)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x790, PrettyRep.List (List.map (fn x791 => 
                                                                               cvtFOR_ENUM_HEAD x791
                                                                        ) ls792), 
          
       (case opt797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x796 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x796))
       )]))
     | cvtEXPR (LiteralExpr x804) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x804))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n812) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n812))
     | cvtFIXTURE_NAME (PropName x815) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x815))
   and cvtIDENT_EXPR (Identifier{ident=x818, openNamespaces=ls824}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x818), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls820 => PrettyRep.List (List.map (fn x819 => 
                                                                                cvtNAMESPACE x819
                                                                         ) ls820)
                                   ) ls824))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x835, expr=x836}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x835), ("expr", cvtEXPR x836)]))
     | cvtIDENT_EXPR (AttributeIdentifier x844) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x844))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x847, openNamespaces=ls853}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x847), ("openNamespaces", PrettyRep.List (List.map (fn ls849 => 
                                                                            PrettyRep.List (List.map (fn x848 => 
                                                                                                            cvtNAMESPACE x848
                                                                                                     ) ls849)
                                                                     ) ls853))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x864, ident=s865}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x864), ("ident", PrettyRep.UniStr s865)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r875) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r875))
     | cvtLITERAL (LiteralDecimal d878) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d878))
     | cvtLITERAL (LiteralBoolean b881) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b881))
     | cvtLITERAL (LiteralString s884) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s884))
     | cvtLITERAL (LiteralArray{exprs=x887, ty=opt889}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x887), ("ty", 
       (case opt889 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x888 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x888))
       ))]))
     | cvtLITERAL (LiteralXML ls901) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x900 => 
                                                                                                           cvtEXPR x900
                                                                                                    ) ls901)))
     | cvtLITERAL (LiteralNamespace x907) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x907))
     | cvtLITERAL (LiteralObject{expr=ls911, ty=opt916}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x910 => 
                                                                        cvtFIELD x910
                                                                 ) ls911)), 
          ("ty", 
       (case opt916 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x915 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x915))
       ))]))
     | cvtLITERAL (LiteralFunction x927) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x927))
     | cvtLITERAL (LiteralRegExp{str=s930}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s930)]))
   and cvtBLOCK (Block x936) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x936))
   and cvtFIXTURE (NamespaceFixture x939) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x939))
     | cvtFIXTURE (ClassFixture x942) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x942))
     | cvtFIXTURE (InterfaceFixture x945) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x945))
     | cvtFIXTURE (TypeVarFixture x948) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x948))
     | cvtFIXTURE (TypeFixture x951) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x951))
     | cvtFIXTURE (MethodFixture{func=x954, ty=x955, readOnly=b956, override=b957, 
          final=b958}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x954), ("ty", cvtTYPE_EXPR x955), ("readOnly", PrettyRep.Bool b956), 
          ("override", PrettyRep.Bool b957), ("final", PrettyRep.Bool b958)]))
     | cvtFIXTURE (ValFixture{ty=x972, readOnly=b973}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x972), ("readOnly", PrettyRep.Bool b973)]))
     | cvtFIXTURE (VirtualValFixture{ty=x981, getter=opt983, setter=opt988}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x981), ("getter", 
       (case opt983 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x982 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x982))
       )), ("setter", 
       (case opt988 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x987 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x987))
       ))]))
   and cvtHEAD (Head(x1001, x1002)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1001, 
          cvtINITS x1002]))
   and cvtBINDINGS (ls1007, ls1012) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1006 => 
                                                                                       cvtBINDING x1006
                                                                                ) ls1007), 
          PrettyRep.List (List.map (fn x1011 => cvtINIT_STEP x1011
                                   ) ls1012)]
   and cvtRIB ls1020 = PrettyRep.List (List.map (fn (x1017, x1018) => PrettyRep.Tuple [cvtFIXTURE_NAME x1017, 
                                                       cvtFIXTURE x1018]
                                                ) ls1020)
   and cvtRIBS ls1025 = PrettyRep.List (List.map (fn x1024 => cvtRIB x1024
                                                 ) ls1025)
   and cvtINITS ls1032 = PrettyRep.List (List.map (fn (x1029, x1030) => PrettyRep.Tuple [cvtFIXTURE_NAME x1029, 
                                                         cvtEXPR x1030]
                                                  ) ls1032)
   and cvtINSTANCE_TYPE {name=x1036, typeParams=ls1038, typeArgs=ls1043, nonnullable=b1047, 
          superTypes=ls1049, ty=x1053, dynamic=b1054} = PrettyRep.Rec [("name", 
          cvtNAME x1036), ("typeParams", PrettyRep.List (List.map (fn x1037 => 
                                                                         cvtIDENT x1037
                                                                  ) ls1038)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1042 => cvtTYPE_EXPR x1042
                                                ) ls1043)), ("nonnullable", 
          PrettyRep.Bool b1047), ("superTypes", PrettyRep.List (List.map (fn x1048 => 
                                                                                cvtTYPE_EXPR x1048
                                                                         ) ls1049)), 
          ("ty", cvtTYPE_EXPR x1053), ("dynamic", PrettyRep.Bool b1054)]
   and cvtFIELD {kind=x1070, name=x1071, init=x1072} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1070), ("name", cvtIDENT_EXPR x1071), ("init", cvtEXPR x1072)]
   and cvtFIELD_TYPE {name=x1080, ty=x1081} = PrettyRep.Rec [("name", cvtIDENT x1080), 
          ("ty", cvtTYPE_EXPR x1081)]
   and cvtFUNC_TYPE {params=ls1088, result=x1092, thisType=opt1094, hasRest=b1098, 
          minArgs=n1099} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1087 => 
                                                                                     cvtTYPE_EXPR x1087
                                                                              ) ls1088)), 
          ("result", cvtTYPE_EXPR x1092), ("thisType", 
       (case opt1094 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1093 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1093))
       )), ("hasRest", PrettyRep.Bool b1098), ("minArgs", PrettyRep.Int n1099)]
   and cvtFUNC_DEFN {kind=x1111, ns=opt1113, final=b1117, override=b1118, prototype=b1119, 
          static=b1120, func=x1121} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1111), 
          ("ns", 
       (case opt1113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1112 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1112))
       )), ("final", PrettyRep.Bool b1117), ("override", PrettyRep.Bool b1118), 
          ("prototype", PrettyRep.Bool b1119), ("static", PrettyRep.Bool b1120), 
          ("func", cvtFUNC x1121)]
   and cvtCTOR_DEFN x1137 = cvtCTOR x1137
   and cvtVAR_DEFN {kind=x1138, ns=opt1140, static=b1144, prototype=b1145, 
          bindings=(ls1147, ls1152)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1138), 
          ("ns", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1139))
       )), ("static", PrettyRep.Bool b1144), ("prototype", PrettyRep.Bool b1145), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1146 => 
                                                                        cvtBINDING x1146
                                                                 ) ls1147), 
          PrettyRep.List (List.map (fn x1151 => cvtINIT_STEP x1151
                                   ) ls1152)])]
   and cvtNAMESPACE_DEFN {ident=x1168, ns=opt1170, init=opt1175} = PrettyRep.Rec [("ident", 
          cvtIDENT x1168), ("ns", 
       (case opt1170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1169 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1169))
       )), ("init", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1174))
       ))]
   and cvtCLASS_DEFN {ns=opt1187, privateNS=x1191, protectedNS=x1192, ident=x1193, 
          nonnullable=b1194, dynamic=b1195, final=b1196, params=ls1198, extends=opt1203, 
          implements=ls1208, classDefns=ls1213, instanceDefns=ls1218, instanceStmts=ls1223, 
          ctorDefn=opt1228} = PrettyRep.Rec [("ns", 
       (case opt1187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1186 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1186))
       )), ("privateNS", cvtNAMESPACE x1191), ("protectedNS", cvtNAMESPACE x1192), 
          ("ident", cvtIDENT x1193), ("nonnullable", PrettyRep.Bool b1194), 
          ("dynamic", PrettyRep.Bool b1195), ("final", PrettyRep.Bool b1196), 
          ("params", PrettyRep.List (List.map (fn x1197 => cvtIDENT x1197
                                              ) ls1198)), ("extends", 
       (case opt1203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1202 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1202))
       )), ("implements", PrettyRep.List (List.map (fn x1207 => cvtTYPE_EXPR x1207
                                                   ) ls1208)), ("classDefns", 
          PrettyRep.List (List.map (fn x1212 => cvtDEFN x1212
                                   ) ls1213)), ("instanceDefns", PrettyRep.List (List.map (fn x1217 => 
                                                                                                 cvtDEFN x1217
                                                                                          ) ls1218)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1222 => cvtSTMT x1222
                                                     ) ls1223)), ("ctorDefn", 
          
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1227))
       ))]
   and cvtINTERFACE_DEFN {ident=x1261, ns=opt1263, nonnullable=b1267, params=ls1269, 
          extends=ls1274, instanceDefns=ls1279} = PrettyRep.Rec [("ident", 
          cvtIDENT x1261), ("ns", 
       (case opt1263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1262 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1262))
       )), ("nonnullable", PrettyRep.Bool b1267), ("params", PrettyRep.List (List.map (fn x1268 => 
                                                                                             cvtIDENT x1268
                                                                                      ) ls1269)), 
          ("extends", PrettyRep.List (List.map (fn x1273 => cvtTYPE_EXPR x1273
                                               ) ls1274)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1278 => cvtDEFN x1278
                                   ) ls1279))]
   and cvtTYPE_DEFN {ident=x1296, ns=opt1298, init=x1302} = PrettyRep.Rec [("ident", 
          cvtIDENT x1296), ("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1297))
       )), ("init", cvtTYPE_EXPR x1302)]
   and cvtCLASS_BLOCK {ns=opt1311, protectedNS=x1315, privateNS=x1316, ident=x1317, 
          name=opt1319, block=x1323} = PrettyRep.Rec [("ns", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1310))
       )), ("protectedNS", cvtNAMESPACE x1315), ("privateNS", cvtNAMESPACE x1316), 
          ("ident", cvtIDENT x1317), ("name", 
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1318))
       )), ("block", cvtBLOCK x1323)]
   and cvtFOR_ENUM_HEAD {isEach=b1337, bindings=(ls1339, ls1344), expr=x1349} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1337), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1338 => 
                                                                                                                         cvtBINDING x1338
                                                                                                                  ) ls1339), 
          PrettyRep.List (List.map (fn x1343 => cvtINIT_STEP x1343
                                   ) ls1344)]), ("expr", cvtEXPR x1349)]
   and cvtFOR_ENUM_STMT {isEach=b1357, defn=opt1388, obj=x1392, rib=opt1400, 
          next=x1404, labels=ls1406, body=x1410} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1357), ("defn", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1358, ns=opt1360, static=b1364, prototype=b1365, bindings=(ls1367, 
            ls1372)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1358), ("ns", 
         (case opt1360 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1359))
         )), ("static", PrettyRep.Bool b1364), ("prototype", PrettyRep.Bool b1365), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1366 => 
                                                                          cvtBINDING x1366
                                                                   ) ls1367), 
            PrettyRep.List (List.map (fn x1371 => cvtINIT_STEP x1371
                                     ) ls1372)])]))
       )), ("obj", cvtEXPR x1392), ("rib", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1396 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1393, 
                                                                                      x1394) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1393, 
                                                                                      cvtFIXTURE x1394]
                                                                               ) ls1396)))
       )), ("next", cvtSTMT x1404), ("labels", PrettyRep.List (List.map (fn x1405 => 
                                                                               cvtIDENT x1405
                                                                        ) ls1406)), 
          ("body", cvtSTMT x1410)]
   and cvtFOR_STMT {rib=opt1433, defn=opt1467, init=ls1472, cond=x1476, update=x1477, 
          labels=ls1479, body=x1483} = PrettyRep.Rec [("rib", 
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
       | SOME{kind=x1437, ns=opt1439, static=b1443, prototype=b1444, bindings=(ls1446, 
            ls1451)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1437), ("ns", 
         (case opt1439 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1438))
         )), ("static", PrettyRep.Bool b1443), ("prototype", PrettyRep.Bool b1444), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1445 => 
                                                                          cvtBINDING x1445
                                                                   ) ls1446), 
            PrettyRep.List (List.map (fn x1450 => cvtINIT_STEP x1450
                                     ) ls1451)])]))
       )), ("init", PrettyRep.List (List.map (fn x1471 => cvtSTMT x1471
                                             ) ls1472)), ("cond", cvtEXPR x1476), 
          ("update", cvtEXPR x1477), ("labels", PrettyRep.List (List.map (fn x1478 => 
                                                                                cvtIDENT x1478
                                                                         ) ls1479)), 
          ("body", cvtSTMT x1483)]
   and cvtWHILE_STMT {cond=x1499, rib=opt1507, body=x1511, labels=ls1513} = 
          PrettyRep.Rec [("cond", cvtEXPR x1499), ("rib", 
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1503 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1500, 
                                                                                      x1501) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1500, 
                                                                                      cvtFIXTURE x1501]
                                                                               ) ls1503)))
       )), ("body", cvtSTMT x1511), ("labels", PrettyRep.List (List.map (fn x1512 => 
                                                                               cvtIDENT x1512
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
       )), ("body", PrettyRep.List (List.map (fn x1541 => cvtSTMT x1541
                                             ) ls1542)), ("loc", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1546))
       ))]
   and cvtCASE {label=opt1563, inits=opt1574, body=x1578} = PrettyRep.Rec [("label", 
          
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1562))
       )), ("inits", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1570 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1567, 
                                                                                      x1568) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1567, 
                                                                                      cvtEXPR x1568]
                                                                               ) ls1570)))
       )), ("body", cvtBLOCK x1578)]
   and cvtCATCH_CLAUSE {bindings=(ls1587, ls1592), ty=x1597, rib=opt1605, inits=opt1616, 
          block=x1620} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1586 => 
                                                                                                      cvtBINDING x1586
                                                                                               ) ls1587), 
          PrettyRep.List (List.map (fn x1591 => cvtINIT_STEP x1591
                                   ) ls1592)]), ("ty", cvtTYPE_EXPR x1597), 
          ("rib", 
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
                                                                                      cvtEXPR x1610]
                                                                               ) ls1612)))
       )), ("block", cvtBLOCK x1620)]
   and cvtFUNC_NAME {kind=x1632, ident=x1633} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1632), 
          ("ident", cvtIDENT x1633)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1639, getter=opt1641, setter=opt1646} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1639), ("getter", 
       (case opt1641 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1640 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1640))
       )), ("setter", 
       (case opt1646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1645 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1645))
       ))]
   and cvtFRAGMENT (Anon x1657) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1657))
end

