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
     | cvtTYPE_EXPR (AppType{base=x425, args=ls427}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x425), ("args", PrettyRep.List (List.map (fn x426 => 
                                                                                                     cvtTYPE_EXPR x426
                                                                                              ) ls427))]))
     | cvtTYPE_EXPR (LamType{params=ls439, body=x443}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x438 => 
                                                                          cvtIDENT x438
                                                                   ) ls439)), 
          ("body", cvtTYPE_EXPR x443)]))
     | cvtTYPE_EXPR (NullableType{expr=x451, nullable=b452}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x451), ("nullable", PrettyRep.Bool b452)]))
     | cvtTYPE_EXPR (InstanceType x460) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x460))
     | cvtTYPE_EXPR (TypeVarFixtureRef x463) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x463))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x467) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x467))
     | cvtSTMT (InitStmt{kind=x470, ns=opt472, prototype=b476, static=b477, 
          temps=x478, inits=ls480}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x470), ("ns", 
       (case opt472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x471 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x471))
       )), ("prototype", PrettyRep.Bool b476), ("static", PrettyRep.Bool b477), 
          ("temps", cvtBINDINGS x478), ("inits", PrettyRep.List (List.map (fn x479 => 
                                                                                 cvtINIT_STEP x479
                                                                          ) ls480))]))
     | cvtSTMT (ClassBlock x499) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x499))
     | cvtSTMT (ForInStmt x502) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x502))
     | cvtSTMT (ThrowStmt x505) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x505))
     | cvtSTMT (ReturnStmt x508) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x508))
     | cvtSTMT (BreakStmt opt512) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x511 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x511))
       ))
     | cvtSTMT (ContinueStmt opt519) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x518 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x518))
       ))
     | cvtSTMT (BlockStmt x525) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x525))
     | cvtSTMT (LabeledStmt(x528, x529)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x528, 
          cvtSTMT x529]))
     | cvtSTMT (LetStmt x533) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x533))
     | cvtSTMT (WhileStmt x536) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x536))
     | cvtSTMT (DoWhileStmt x539) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x539))
     | cvtSTMT (ForStmt x542) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x542))
     | cvtSTMT (IfStmt{cnd=x545, thn=x546, els=x547}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x545), ("thn", cvtSTMT x546), 
          ("els", cvtSTMT x547)]))
     | cvtSTMT (WithStmt{obj=x557, ty=x558, body=x559}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x557), ("ty", cvtTYPE_EXPR x558), 
          ("body", cvtSTMT x559)]))
     | cvtSTMT (TryStmt{block=x569, catches=ls571, finally=opt576}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x569), ("catches", PrettyRep.List (List.map (fn x570 => 
                                                                                                     cvtCATCH_CLAUSE x570
                                                                                              ) ls571)), 
          ("finally", 
       (case opt576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x575 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x575))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x589, labels=ls591, cases=ls596}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x589), ("labels", PrettyRep.List (List.map (fn x590 => 
                                                                                                  cvtIDENT x590
                                                                                           ) ls591)), 
          ("cases", PrettyRep.List (List.map (fn x595 => cvtCASE x595
                                             ) ls596))]))
     | cvtSTMT (SwitchTypeStmt{cond=x609, ty=x610, cases=ls612}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x609), ("ty", cvtTYPE_EXPR x610), 
          ("cases", PrettyRep.List (List.map (fn x611 => cvtCATCH_CLAUSE x611
                                             ) ls612))]))
     | cvtSTMT (DXNStmt{expr=x625}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x625)]))
   and cvtEXPR (TernaryExpr(x631, x632, x633)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x631, cvtEXPR x632, cvtEXPR x633]))
     | cvtEXPR (BinaryExpr(x637, x638, x639)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x637, cvtEXPR x638, cvtEXPR x639]))
     | cvtEXPR (BinaryTypeExpr(x643, x644, x645)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x643, cvtEXPR x644, cvtTYPE_EXPR x645]))
     | cvtEXPR (UnaryExpr(x649, x650)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x649, 
          cvtEXPR x650]))
     | cvtEXPR (TypeExpr x654) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x654))
     | cvtEXPR (ThisExpr opt658) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x657 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x657))
       ))
     | cvtEXPR (YieldExpr opt665) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt665 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x664 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x664))
       ))
     | cvtEXPR (SuperExpr opt672) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x671))
       ))
     | cvtEXPR (CallExpr{func=x678, actuals=ls680}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x678), ("actuals", PrettyRep.List (List.map (fn x679 => 
                                                                                                   cvtEXPR x679
                                                                                            ) ls680))]))
     | cvtEXPR (ApplyTypeExpr{expr=x691, actuals=ls693}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x691), ("actuals", PrettyRep.List (List.map (fn x692 => 
                                                                                                   cvtTYPE_EXPR x692
                                                                                            ) ls693))]))
     | cvtEXPR (LetExpr{defs=x704, body=x705, head=opt707}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x704), ("body", cvtEXPR x705), 
          ("head", 
       (case opt707 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x706 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x706))
       ))]))
     | cvtEXPR (NewExpr{obj=x720, actuals=ls722}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x720), ("actuals", PrettyRep.List (List.map (fn x721 => 
                                                                                                  cvtEXPR x721
                                                                                           ) ls722))]))
     | cvtEXPR (ObjectRef{base=x733, ident=x734, loc=opt736}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x733), ("ident", cvtIDENT_EXPR x734), 
          ("loc", 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x735))
       ))]))
     | cvtEXPR (LexicalRef{ident=x749, loc=opt751}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x749), ("loc", 
       (case opt751 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x750 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x750))
       ))]))
     | cvtEXPR (SetExpr(x762, x763, x764)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x762, 
          cvtEXPR x763, cvtEXPR x764]))
     | cvtEXPR (ListExpr ls769) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x768 => 
                                                                                                    cvtEXPR x768
                                                                                             ) ls769)))
     | cvtEXPR (InitExpr(x775, x776, x777)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x775, 
          cvtHEAD x776, cvtINITS x777]))
     | cvtEXPR (GetTemp n781) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n781))
     | cvtEXPR (GetParam n784) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n784))
     | cvtEXPR (Comprehension(x787, ls789, opt794)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x787, PrettyRep.List (List.map (fn x788 => 
                                                                               cvtFOR_ENUM_HEAD x788
                                                                        ) ls789), 
          
       (case opt794 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x793 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x793))
       )]))
     | cvtEXPR (LiteralExpr x801) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x801))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n809) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n809))
     | cvtFIXTURE_NAME (PropName x812) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x812))
   and cvtIDENT_EXPR (Identifier{ident=x815, openNamespaces=ls821, rootRib=opt826}) = 
          PrettyRep.Ctor ("Identifier", SOME (PrettyRep.Rec [("ident", cvtIDENT x815), 
          ("openNamespaces", PrettyRep.List (List.map (fn ls817 => PrettyRep.List (List.map (fn x816 => 
                                                                                                   cvtNAMESPACE x816
                                                                                            ) ls817)
                                                      ) ls821)), ("rootRib", 
          
       (case opt826 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x825 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x825))
       ))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x839, expr=x840}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x839), ("expr", cvtEXPR x840)]))
     | cvtIDENT_EXPR (AttributeIdentifier x848) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x848))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x851, openNamespaces=ls857}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x851), ("openNamespaces", PrettyRep.List (List.map (fn ls853 => 
                                                                            PrettyRep.List (List.map (fn x852 => 
                                                                                                            cvtNAMESPACE x852
                                                                                                     ) ls853)
                                                                     ) ls857))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x868, ident=s869}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x868), ("ident", PrettyRep.UniStr s869)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r879) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r879))
     | cvtLITERAL (LiteralDecimal d882) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d882))
     | cvtLITERAL (LiteralBoolean b885) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b885))
     | cvtLITERAL (LiteralString s888) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s888))
     | cvtLITERAL (LiteralArray{exprs=x891, ty=opt893}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x891), ("ty", 
       (case opt893 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x892 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x892))
       ))]))
     | cvtLITERAL (LiteralXML ls905) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x904 => 
                                                                                                           cvtEXPR x904
                                                                                                    ) ls905)))
     | cvtLITERAL (LiteralNamespace x911) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x911))
     | cvtLITERAL (LiteralObject{expr=ls915, ty=opt920}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x914 => 
                                                                        cvtFIELD x914
                                                                 ) ls915)), 
          ("ty", 
       (case opt920 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x919 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x919))
       ))]))
     | cvtLITERAL (LiteralFunction x931) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x931))
     | cvtLITERAL (LiteralRegExp{str=s934}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s934)]))
   and cvtBLOCK (Block x940) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x940))
   and cvtFIXTURE (NamespaceFixture x943) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x943))
     | cvtFIXTURE (ClassFixture x946) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x946))
     | cvtFIXTURE (InterfaceFixture x949) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x949))
     | cvtFIXTURE (TypeVarFixture x952) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x952))
     | cvtFIXTURE (TypeFixture x955) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x955))
     | cvtFIXTURE (MethodFixture{func=x958, ty=x959, readOnly=b960, override=b961, 
          final=b962}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x958), ("ty", cvtTYPE_EXPR x959), ("readOnly", PrettyRep.Bool b960), 
          ("override", PrettyRep.Bool b961), ("final", PrettyRep.Bool b962)]))
     | cvtFIXTURE (ValFixture{ty=x976, readOnly=b977}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x976), ("readOnly", PrettyRep.Bool b977)]))
     | cvtFIXTURE (VirtualValFixture{ty=x985, getter=opt987, setter=opt992}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x985), ("getter", 
       (case opt987 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x986 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x986))
       )), ("setter", 
       (case opt992 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x991 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x991))
       ))]))
   and cvtHEAD (Head(x1005, x1006)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1005, 
          cvtINITS x1006]))
   and cvtBINDINGS (ls1011, ls1016) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1010 => 
                                                                                       cvtBINDING x1010
                                                                                ) ls1011), 
          PrettyRep.List (List.map (fn x1015 => cvtINIT_STEP x1015
                                   ) ls1016)]
   and cvtRIB ls1024 = PrettyRep.List (List.map (fn (x1021, x1022) => PrettyRep.Tuple [cvtFIXTURE_NAME x1021, 
                                                       cvtFIXTURE x1022]
                                                ) ls1024)
   and cvtRIBS ls1029 = PrettyRep.List (List.map (fn x1028 => cvtRIB x1028
                                                 ) ls1029)
   and cvtINITS ls1036 = PrettyRep.List (List.map (fn (x1033, x1034) => PrettyRep.Tuple [cvtFIXTURE_NAME x1033, 
                                                         cvtEXPR x1034]
                                                  ) ls1036)
   and cvtINSTANCE_TYPE {name=x1040, typeParams=ls1042, typeArgs=ls1047, nonnullable=b1051, 
          superTypes=ls1053, ty=x1057, dynamic=b1058} = PrettyRep.Rec [("name", 
          cvtNAME x1040), ("typeParams", PrettyRep.List (List.map (fn x1041 => 
                                                                         cvtIDENT x1041
                                                                  ) ls1042)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1046 => cvtTYPE_EXPR x1046
                                                ) ls1047)), ("nonnullable", 
          PrettyRep.Bool b1051), ("superTypes", PrettyRep.List (List.map (fn x1052 => 
                                                                                cvtTYPE_EXPR x1052
                                                                         ) ls1053)), 
          ("ty", cvtTYPE_EXPR x1057), ("dynamic", PrettyRep.Bool b1058)]
   and cvtFIELD {kind=x1074, name=x1075, init=x1076} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1074), ("name", cvtIDENT_EXPR x1075), ("init", cvtEXPR x1076)]
   and cvtFIELD_TYPE {name=x1084, ty=x1085} = PrettyRep.Rec [("name", cvtIDENT x1084), 
          ("ty", cvtTYPE_EXPR x1085)]
   and cvtFUNC_TYPE {params=ls1092, result=x1096, thisType=opt1098, hasRest=b1102, 
          minArgs=n1103} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1091 => 
                                                                                     cvtTYPE_EXPR x1091
                                                                              ) ls1092)), 
          ("result", cvtTYPE_EXPR x1096), ("thisType", 
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1097))
       )), ("hasRest", PrettyRep.Bool b1102), ("minArgs", PrettyRep.Int n1103)]
   and cvtFUNC_DEFN {kind=x1115, ns=opt1117, final=b1121, override=b1122, prototype=b1123, 
          static=b1124, func=x1125} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1115), 
          ("ns", 
       (case opt1117 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1116 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1116))
       )), ("final", PrettyRep.Bool b1121), ("override", PrettyRep.Bool b1122), 
          ("prototype", PrettyRep.Bool b1123), ("static", PrettyRep.Bool b1124), 
          ("func", cvtFUNC x1125)]
   and cvtCTOR_DEFN x1141 = cvtCTOR x1141
   and cvtVAR_DEFN {kind=x1142, ns=opt1144, static=b1148, prototype=b1149, 
          bindings=(ls1151, ls1156)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1142), 
          ("ns", 
       (case opt1144 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1143 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1143))
       )), ("static", PrettyRep.Bool b1148), ("prototype", PrettyRep.Bool b1149), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1150 => 
                                                                        cvtBINDING x1150
                                                                 ) ls1151), 
          PrettyRep.List (List.map (fn x1155 => cvtINIT_STEP x1155
                                   ) ls1156)])]
   and cvtNAMESPACE_DEFN {ident=x1172, ns=opt1174, init=opt1179} = PrettyRep.Rec [("ident", 
          cvtIDENT x1172), ("ns", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1173))
       )), ("init", 
       (case opt1179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1178 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1178))
       ))]
   and cvtCLASS_DEFN {ns=opt1191, privateNS=x1195, protectedNS=x1196, ident=x1197, 
          nonnullable=b1198, dynamic=b1199, final=b1200, params=ls1202, extends=opt1207, 
          implements=ls1212, classDefns=ls1217, instanceDefns=ls1222, instanceStmts=ls1227, 
          ctorDefn=opt1232} = PrettyRep.Rec [("ns", 
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1190))
       )), ("privateNS", cvtNAMESPACE x1195), ("protectedNS", cvtNAMESPACE x1196), 
          ("ident", cvtIDENT x1197), ("nonnullable", PrettyRep.Bool b1198), 
          ("dynamic", PrettyRep.Bool b1199), ("final", PrettyRep.Bool b1200), 
          ("params", PrettyRep.List (List.map (fn x1201 => cvtIDENT x1201
                                              ) ls1202)), ("extends", 
       (case opt1207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1206 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1206))
       )), ("implements", PrettyRep.List (List.map (fn x1211 => cvtTYPE_EXPR x1211
                                                   ) ls1212)), ("classDefns", 
          PrettyRep.List (List.map (fn x1216 => cvtDEFN x1216
                                   ) ls1217)), ("instanceDefns", PrettyRep.List (List.map (fn x1221 => 
                                                                                                 cvtDEFN x1221
                                                                                          ) ls1222)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1226 => cvtSTMT x1226
                                                     ) ls1227)), ("ctorDefn", 
          
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1231))
       ))]
   and cvtINTERFACE_DEFN {ident=x1265, ns=opt1267, nonnullable=b1271, params=ls1273, 
          extends=ls1278, instanceDefns=ls1283} = PrettyRep.Rec [("ident", 
          cvtIDENT x1265), ("ns", 
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1266))
       )), ("nonnullable", PrettyRep.Bool b1271), ("params", PrettyRep.List (List.map (fn x1272 => 
                                                                                             cvtIDENT x1272
                                                                                      ) ls1273)), 
          ("extends", PrettyRep.List (List.map (fn x1277 => cvtTYPE_EXPR x1277
                                               ) ls1278)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1282 => cvtDEFN x1282
                                   ) ls1283))]
   and cvtTYPE_DEFN {ident=x1300, ns=opt1302, init=x1306} = PrettyRep.Rec [("ident", 
          cvtIDENT x1300), ("ns", 
       (case opt1302 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1301 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1301))
       )), ("init", cvtTYPE_EXPR x1306)]
   and cvtCLASS_BLOCK {ns=opt1315, protectedNS=x1319, privateNS=x1320, ident=x1321, 
          name=opt1323, block=x1327} = PrettyRep.Rec [("ns", 
       (case opt1315 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1314 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1314))
       )), ("protectedNS", cvtNAMESPACE x1319), ("privateNS", cvtNAMESPACE x1320), 
          ("ident", cvtIDENT x1321), ("name", 
       (case opt1323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1322 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1322))
       )), ("block", cvtBLOCK x1327)]
   and cvtFOR_ENUM_HEAD {isEach=b1341, bindings=(ls1343, ls1348), expr=x1353} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1341), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1342 => 
                                                                                                                         cvtBINDING x1342
                                                                                                                  ) ls1343), 
          PrettyRep.List (List.map (fn x1347 => cvtINIT_STEP x1347
                                   ) ls1348)]), ("expr", cvtEXPR x1353)]
   and cvtFOR_ENUM_STMT {isEach=b1361, defn=opt1392, obj=x1396, rib=opt1404, 
          next=x1408, labels=ls1410, body=x1414} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1361), ("defn", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1362, ns=opt1364, static=b1368, prototype=b1369, bindings=(ls1371, 
            ls1376)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1362), ("ns", 
         (case opt1364 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1363 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1363))
         )), ("static", PrettyRep.Bool b1368), ("prototype", PrettyRep.Bool b1369), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1370 => 
                                                                          cvtBINDING x1370
                                                                   ) ls1371), 
            PrettyRep.List (List.map (fn x1375 => cvtINIT_STEP x1375
                                     ) ls1376)])]))
       )), ("obj", cvtEXPR x1396), ("rib", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1400 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1397, 
                                                                                      x1398) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1397, 
                                                                                      cvtFIXTURE x1398]
                                                                               ) ls1400)))
       )), ("next", cvtSTMT x1408), ("labels", PrettyRep.List (List.map (fn x1409 => 
                                                                               cvtIDENT x1409
                                                                        ) ls1410)), 
          ("body", cvtSTMT x1414)]
   and cvtFOR_STMT {rib=opt1437, defn=opt1471, init=ls1476, cond=x1480, update=x1481, 
          labels=ls1483, body=x1487} = PrettyRep.Rec [("rib", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1433 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1430, 
                                                                                      x1431) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1430, 
                                                                                      cvtFIXTURE x1431]
                                                                               ) ls1433)))
       )), ("defn", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1441, ns=opt1443, static=b1447, prototype=b1448, bindings=(ls1450, 
            ls1455)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1441), ("ns", 
         (case opt1443 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1442 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1442))
         )), ("static", PrettyRep.Bool b1447), ("prototype", PrettyRep.Bool b1448), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1449 => 
                                                                          cvtBINDING x1449
                                                                   ) ls1450), 
            PrettyRep.List (List.map (fn x1454 => cvtINIT_STEP x1454
                                     ) ls1455)])]))
       )), ("init", PrettyRep.List (List.map (fn x1475 => cvtSTMT x1475
                                             ) ls1476)), ("cond", cvtEXPR x1480), 
          ("update", cvtEXPR x1481), ("labels", PrettyRep.List (List.map (fn x1482 => 
                                                                                cvtIDENT x1482
                                                                         ) ls1483)), 
          ("body", cvtSTMT x1487)]
   and cvtWHILE_STMT {cond=x1503, rib=opt1511, body=x1515, labels=ls1517} = 
          PrettyRep.Rec [("cond", cvtEXPR x1503), ("rib", 
       (case opt1511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1507 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1504, 
                                                                                      x1505) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1504, 
                                                                                      cvtFIXTURE x1505]
                                                                               ) ls1507)))
       )), ("body", cvtSTMT x1515), ("labels", PrettyRep.List (List.map (fn x1516 => 
                                                                               cvtIDENT x1516
                                                                        ) ls1517))]
   and cvtDIRECTIVES {pragmas=ls1531, defns=ls1536, head=opt1541, body=ls1546, 
          loc=opt1551} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1530 => 
                                                                                    cvtPRAGMA x1530
                                                                             ) ls1531)), 
          ("defns", PrettyRep.List (List.map (fn x1535 => cvtDEFN x1535
                                             ) ls1536)), ("head", 
       (case opt1541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1540 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1540))
       )), ("body", PrettyRep.List (List.map (fn x1545 => cvtSTMT x1545
                                             ) ls1546)), ("loc", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1550))
       ))]
   and cvtCASE {label=opt1567, inits=opt1578, body=x1582} = PrettyRep.Rec [("label", 
          
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1566 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1566))
       )), ("inits", 
       (case opt1578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1574 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1571, 
                                                                                      x1572) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1571, 
                                                                                      cvtEXPR x1572]
                                                                               ) ls1574)))
       )), ("body", cvtBLOCK x1582)]
   and cvtCATCH_CLAUSE {bindings=(ls1591, ls1596), ty=x1601, rib=opt1609, inits=opt1620, 
          block=x1624} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1590 => 
                                                                                                      cvtBINDING x1590
                                                                                               ) ls1591), 
          PrettyRep.List (List.map (fn x1595 => cvtINIT_STEP x1595
                                   ) ls1596)]), ("ty", cvtTYPE_EXPR x1601), 
          ("rib", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1605 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1602, 
                                                                                      x1603) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1602, 
                                                                                      cvtFIXTURE x1603]
                                                                               ) ls1605)))
       )), ("inits", 
       (case opt1620 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1616 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1613, 
                                                                                      x1614) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1613, 
                                                                                      cvtEXPR x1614]
                                                                               ) ls1616)))
       )), ("block", cvtBLOCK x1624)]
   and cvtFUNC_NAME {kind=x1636, ident=x1637} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1636), 
          ("ident", cvtIDENT x1637)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1643, getter=opt1645, setter=opt1650} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1643), ("getter", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1644 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1644))
       )), ("setter", 
       (case opt1650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1649 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1649))
       ))]
   and cvtFRAGMENT (Anon x1661) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1661))
end

