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
   and cvtIDENT_EXPR (Identifier{ident=x815, openNamespaces=ls821}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x815), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls817 => PrettyRep.List (List.map (fn x816 => 
                                                                                cvtNAMESPACE x816
                                                                         ) ls817)
                                   ) ls821))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x832, expr=x833}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x832), ("expr", cvtEXPR x833)]))
     | cvtIDENT_EXPR (AttributeIdentifier x841) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x841))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x844, openNamespaces=ls850}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x844), ("openNamespaces", PrettyRep.List (List.map (fn ls846 => 
                                                                            PrettyRep.List (List.map (fn x845 => 
                                                                                                            cvtNAMESPACE x845
                                                                                                     ) ls846)
                                                                     ) ls850))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x861, ident=s862}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x861), ("ident", PrettyRep.UniStr s862)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r872) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r872))
     | cvtLITERAL (LiteralDecimal d875) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d875))
     | cvtLITERAL (LiteralBoolean b878) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b878))
     | cvtLITERAL (LiteralString s881) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s881))
     | cvtLITERAL (LiteralArray{exprs=x884, ty=opt886}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x884), ("ty", 
       (case opt886 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x885 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x885))
       ))]))
     | cvtLITERAL (LiteralXML ls898) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x897 => 
                                                                                                           cvtEXPR x897
                                                                                                    ) ls898)))
     | cvtLITERAL (LiteralNamespace x904) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x904))
     | cvtLITERAL (LiteralObject{expr=ls908, ty=opt913}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x907 => 
                                                                        cvtFIELD x907
                                                                 ) ls908)), 
          ("ty", 
       (case opt913 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x912 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x912))
       ))]))
     | cvtLITERAL (LiteralFunction x924) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x924))
     | cvtLITERAL (LiteralRegExp{str=s927}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s927)]))
   and cvtBLOCK (Block x933) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x933))
   and cvtFIXTURE (NamespaceFixture x936) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x936))
     | cvtFIXTURE (ClassFixture x939) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x939))
     | cvtFIXTURE (InterfaceFixture x942) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x942))
     | cvtFIXTURE (TypeVarFixture x945) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x945))
     | cvtFIXTURE (TypeFixture x948) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x948))
     | cvtFIXTURE (MethodFixture{func=x951, ty=x952, readOnly=b953, override=b954, 
          final=b955}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x951), ("ty", cvtTYPE_EXPR x952), ("readOnly", PrettyRep.Bool b953), 
          ("override", PrettyRep.Bool b954), ("final", PrettyRep.Bool b955)]))
     | cvtFIXTURE (ValFixture{ty=x969, readOnly=b970}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x969), ("readOnly", PrettyRep.Bool b970)]))
     | cvtFIXTURE (VirtualValFixture{ty=x978, getter=opt980, setter=opt985}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x978), ("getter", 
       (case opt980 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x979 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x979))
       )), ("setter", 
       (case opt985 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x984 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x984))
       ))]))
   and cvtHEAD (Head(x998, x999)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x998, 
          cvtINITS x999]))
   and cvtBINDINGS (ls1004, ls1009) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1003 => 
                                                                                       cvtBINDING x1003
                                                                                ) ls1004), 
          PrettyRep.List (List.map (fn x1008 => cvtINIT_STEP x1008
                                   ) ls1009)]
   and cvtRIB ls1017 = PrettyRep.List (List.map (fn (x1014, x1015) => PrettyRep.Tuple [cvtFIXTURE_NAME x1014, 
                                                       cvtFIXTURE x1015]
                                                ) ls1017)
   and cvtRIBS ls1022 = PrettyRep.List (List.map (fn x1021 => cvtRIB x1021
                                                 ) ls1022)
   and cvtINITS ls1029 = PrettyRep.List (List.map (fn (x1026, x1027) => PrettyRep.Tuple [cvtFIXTURE_NAME x1026, 
                                                         cvtEXPR x1027]
                                                  ) ls1029)
   and cvtINSTANCE_TYPE {name=x1033, typeParams=ls1035, typeArgs=ls1040, nonnullable=b1044, 
          superTypes=ls1046, ty=x1050, dynamic=b1051} = PrettyRep.Rec [("name", 
          cvtNAME x1033), ("typeParams", PrettyRep.List (List.map (fn x1034 => 
                                                                         cvtIDENT x1034
                                                                  ) ls1035)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1039 => cvtTYPE_EXPR x1039
                                                ) ls1040)), ("nonnullable", 
          PrettyRep.Bool b1044), ("superTypes", PrettyRep.List (List.map (fn x1045 => 
                                                                                cvtTYPE_EXPR x1045
                                                                         ) ls1046)), 
          ("ty", cvtTYPE_EXPR x1050), ("dynamic", PrettyRep.Bool b1051)]
   and cvtFIELD {kind=x1067, name=x1068, init=x1069} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1067), ("name", cvtIDENT_EXPR x1068), ("init", cvtEXPR x1069)]
   and cvtFIELD_TYPE {name=x1077, ty=x1078} = PrettyRep.Rec [("name", cvtIDENT x1077), 
          ("ty", cvtTYPE_EXPR x1078)]
   and cvtFUNC_TYPE {params=ls1085, result=x1089, thisType=opt1091, hasRest=b1095, 
          minArgs=n1096} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1084 => 
                                                                                     cvtTYPE_EXPR x1084
                                                                              ) ls1085)), 
          ("result", cvtTYPE_EXPR x1089), ("thisType", 
       (case opt1091 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1090 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1090))
       )), ("hasRest", PrettyRep.Bool b1095), ("minArgs", PrettyRep.Int n1096)]
   and cvtFUNC_DEFN {kind=x1108, ns=opt1110, final=b1114, override=b1115, prototype=b1116, 
          static=b1117, func=x1118} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1108), 
          ("ns", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1109 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1109))
       )), ("final", PrettyRep.Bool b1114), ("override", PrettyRep.Bool b1115), 
          ("prototype", PrettyRep.Bool b1116), ("static", PrettyRep.Bool b1117), 
          ("func", cvtFUNC x1118)]
   and cvtCTOR_DEFN x1134 = cvtCTOR x1134
   and cvtVAR_DEFN {kind=x1135, ns=opt1137, static=b1141, prototype=b1142, 
          bindings=(ls1144, ls1149)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1135), 
          ("ns", 
       (case opt1137 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1136 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1136))
       )), ("static", PrettyRep.Bool b1141), ("prototype", PrettyRep.Bool b1142), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1143 => 
                                                                        cvtBINDING x1143
                                                                 ) ls1144), 
          PrettyRep.List (List.map (fn x1148 => cvtINIT_STEP x1148
                                   ) ls1149)])]
   and cvtNAMESPACE_DEFN {ident=x1165, ns=opt1167, init=opt1172} = PrettyRep.Rec [("ident", 
          cvtIDENT x1165), ("ns", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1166))
       )), ("init", 
       (case opt1172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1171 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1171))
       ))]
   and cvtCLASS_DEFN {ns=opt1184, privateNS=x1188, protectedNS=x1189, ident=x1190, 
          nonnullable=b1191, dynamic=b1192, final=b1193, params=ls1195, extends=opt1200, 
          implements=ls1205, classDefns=ls1210, instanceDefns=ls1215, instanceStmts=ls1220, 
          ctorDefn=opt1225} = PrettyRep.Rec [("ns", 
       (case opt1184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1183 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1183))
       )), ("privateNS", cvtNAMESPACE x1188), ("protectedNS", cvtNAMESPACE x1189), 
          ("ident", cvtIDENT x1190), ("nonnullable", PrettyRep.Bool b1191), 
          ("dynamic", PrettyRep.Bool b1192), ("final", PrettyRep.Bool b1193), 
          ("params", PrettyRep.List (List.map (fn x1194 => cvtIDENT x1194
                                              ) ls1195)), ("extends", 
       (case opt1200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1199 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1199))
       )), ("implements", PrettyRep.List (List.map (fn x1204 => cvtTYPE_EXPR x1204
                                                   ) ls1205)), ("classDefns", 
          PrettyRep.List (List.map (fn x1209 => cvtDEFN x1209
                                   ) ls1210)), ("instanceDefns", PrettyRep.List (List.map (fn x1214 => 
                                                                                                 cvtDEFN x1214
                                                                                          ) ls1215)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1219 => cvtSTMT x1219
                                                     ) ls1220)), ("ctorDefn", 
          
       (case opt1225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1224 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1224))
       ))]
   and cvtINTERFACE_DEFN {ident=x1258, ns=opt1260, nonnullable=b1264, params=ls1266, 
          extends=ls1271, instanceDefns=ls1276} = PrettyRep.Rec [("ident", 
          cvtIDENT x1258), ("ns", 
       (case opt1260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1259 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1259))
       )), ("nonnullable", PrettyRep.Bool b1264), ("params", PrettyRep.List (List.map (fn x1265 => 
                                                                                             cvtIDENT x1265
                                                                                      ) ls1266)), 
          ("extends", PrettyRep.List (List.map (fn x1270 => cvtTYPE_EXPR x1270
                                               ) ls1271)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1275 => cvtDEFN x1275
                                   ) ls1276))]
   and cvtTYPE_DEFN {ident=x1293, ns=opt1295, init=x1299} = PrettyRep.Rec [("ident", 
          cvtIDENT x1293), ("ns", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1294))
       )), ("init", cvtTYPE_EXPR x1299)]
   and cvtCLASS_BLOCK {ns=opt1308, protectedNS=x1312, privateNS=x1313, ident=x1314, 
          name=opt1316, block=x1320} = PrettyRep.Rec [("ns", 
       (case opt1308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1307 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1307))
       )), ("protectedNS", cvtNAMESPACE x1312), ("privateNS", cvtNAMESPACE x1313), 
          ("ident", cvtIDENT x1314), ("name", 
       (case opt1316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1315 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1315))
       )), ("block", cvtBLOCK x1320)]
   and cvtFOR_ENUM_HEAD {isEach=b1334, bindings=(ls1336, ls1341), expr=x1346} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1334), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1335 => 
                                                                                                                         cvtBINDING x1335
                                                                                                                  ) ls1336), 
          PrettyRep.List (List.map (fn x1340 => cvtINIT_STEP x1340
                                   ) ls1341)]), ("expr", cvtEXPR x1346)]
   and cvtFOR_ENUM_STMT {isEach=b1354, defn=opt1385, obj=x1389, rib=opt1397, 
          next=x1401, labels=ls1403, body=x1407} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1354), ("defn", 
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1355, ns=opt1357, static=b1361, prototype=b1362, bindings=(ls1364, 
            ls1369)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1355), ("ns", 
         (case opt1357 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1356))
         )), ("static", PrettyRep.Bool b1361), ("prototype", PrettyRep.Bool b1362), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1363 => 
                                                                          cvtBINDING x1363
                                                                   ) ls1364), 
            PrettyRep.List (List.map (fn x1368 => cvtINIT_STEP x1368
                                     ) ls1369)])]))
       )), ("obj", cvtEXPR x1389), ("rib", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1393 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1390, 
                                                                                      x1391) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1390, 
                                                                                      cvtFIXTURE x1391]
                                                                               ) ls1393)))
       )), ("next", cvtSTMT x1401), ("labels", PrettyRep.List (List.map (fn x1402 => 
                                                                               cvtIDENT x1402
                                                                        ) ls1403)), 
          ("body", cvtSTMT x1407)]
   and cvtFOR_STMT {rib=opt1430, defn=opt1464, init=ls1469, cond=x1473, update=x1474, 
          labels=ls1476, body=x1480} = PrettyRep.Rec [("rib", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1426 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1423, 
                                                                                      x1424) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1423, 
                                                                                      cvtFIXTURE x1424]
                                                                               ) ls1426)))
       )), ("defn", 
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1434, ns=opt1436, static=b1440, prototype=b1441, bindings=(ls1443, 
            ls1448)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1434), ("ns", 
         (case opt1436 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1435))
         )), ("static", PrettyRep.Bool b1440), ("prototype", PrettyRep.Bool b1441), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1442 => 
                                                                          cvtBINDING x1442
                                                                   ) ls1443), 
            PrettyRep.List (List.map (fn x1447 => cvtINIT_STEP x1447
                                     ) ls1448)])]))
       )), ("init", PrettyRep.List (List.map (fn x1468 => cvtSTMT x1468
                                             ) ls1469)), ("cond", cvtEXPR x1473), 
          ("update", cvtEXPR x1474), ("labels", PrettyRep.List (List.map (fn x1475 => 
                                                                                cvtIDENT x1475
                                                                         ) ls1476)), 
          ("body", cvtSTMT x1480)]
   and cvtWHILE_STMT {cond=x1496, rib=opt1504, body=x1508, labels=ls1510} = 
          PrettyRep.Rec [("cond", cvtEXPR x1496), ("rib", 
       (case opt1504 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1500 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1497, 
                                                                                      x1498) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1497, 
                                                                                      cvtFIXTURE x1498]
                                                                               ) ls1500)))
       )), ("body", cvtSTMT x1508), ("labels", PrettyRep.List (List.map (fn x1509 => 
                                                                               cvtIDENT x1509
                                                                        ) ls1510))]
   and cvtDIRECTIVES {pragmas=ls1524, defns=ls1529, head=opt1534, body=ls1539, 
          loc=opt1544} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1523 => 
                                                                                    cvtPRAGMA x1523
                                                                             ) ls1524)), 
          ("defns", PrettyRep.List (List.map (fn x1528 => cvtDEFN x1528
                                             ) ls1529)), ("head", 
       (case opt1534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1533))
       )), ("body", PrettyRep.List (List.map (fn x1538 => cvtSTMT x1538
                                             ) ls1539)), ("loc", 
       (case opt1544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1543 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1543))
       ))]
   and cvtCASE {label=opt1560, inits=opt1571, body=x1575} = PrettyRep.Rec [("label", 
          
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1559 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1559))
       )), ("inits", 
       (case opt1571 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1567 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1564, 
                                                                                      x1565) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1564, 
                                                                                      cvtEXPR x1565]
                                                                               ) ls1567)))
       )), ("body", cvtBLOCK x1575)]
   and cvtCATCH_CLAUSE {bindings=(ls1584, ls1589), ty=x1594, rib=opt1602, inits=opt1613, 
          block=x1617} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1583 => 
                                                                                                      cvtBINDING x1583
                                                                                               ) ls1584), 
          PrettyRep.List (List.map (fn x1588 => cvtINIT_STEP x1588
                                   ) ls1589)]), ("ty", cvtTYPE_EXPR x1594), 
          ("rib", 
       (case opt1602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1598 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1595, 
                                                                                      x1596) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1595, 
                                                                                      cvtFIXTURE x1596]
                                                                               ) ls1598)))
       )), ("inits", 
       (case opt1613 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1609 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1606, 
                                                                                      x1607) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1606, 
                                                                                      cvtEXPR x1607]
                                                                               ) ls1609)))
       )), ("block", cvtBLOCK x1617)]
   and cvtFUNC_NAME {kind=x1629, ident=x1630} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1629), 
          ("ident", cvtIDENT x1630)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1636, getter=opt1638, setter=opt1643} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1636), ("getter", 
       (case opt1638 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1637 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1637))
       )), ("setter", 
       (case opt1643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1642 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1642))
       ))]
   and cvtFRAGMENT (Anon x1654) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1654))
end

