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
   and cvtPRAGMA (UseNamespace x111) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPRESSION x111))
     | cvtPRAGMA (UseDefaultNamespace x114) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPRESSION x114))
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
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x140))
       )), ("implements", PrettyRep.List (List.map (fn x145 => cvtTYPE x145
                                                   ) ls146)), ("classRib", 
          cvtRIB x150), ("instanceRib", cvtRIB x151), ("instanceInits", cvtHEAD x152), 
          ("constructor", 
       (case opt154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x153 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x153))
       )), ("classType", cvtTYPE x158), ("instanceType", cvtTYPE x159)]))
   and cvtIFACE (Iface{name=x193, typeParams=ls195, nonnullable=b199, extends=ls201, 
          instanceRib=x205, instanceType=x206}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x193), ("typeParams", PrettyRep.List (List.map (fn x194 => 
                                                                                                      cvtIDENT x194
                                                                                               ) ls195)), 
          ("nonnullable", PrettyRep.Bool b199), ("extends", PrettyRep.List (List.map (fn x200 => 
                                                                                            cvtTYPE x200
                                                                                     ) ls201)), 
          ("instanceRib", cvtRIB x205), ("instanceType", cvtTYPE x206)]))
   and cvtCTOR (Ctor{settings=x222, superArgs=ls224, func=x228}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x222), ("superArgs", PrettyRep.List (List.map (fn x223 => 
                                                                                                         cvtEXPRESSION x223
                                                                                                  ) ls224)), 
          ("func", cvtFUNC x228)]))
   and cvtFUNC (Func{name=x238, fsig=x239, native=b240, generator=b241, block=opt243, 
          param=x247, defaults=ls249, ty=x253, loc=opt255}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x238), ("fsig", cvtFUNC_SIG x239), 
          ("native", PrettyRep.Bool b240), ("generator", PrettyRep.Bool b241), 
          ("block", 
       (case opt243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x242))
       )), ("param", cvtHEAD x247), ("defaults", PrettyRep.List (List.map (fn x248 => 
                                                                                 cvtEXPRESSION x248
                                                                          ) ls249)), 
          ("ty", cvtTYPE x253), ("loc", 
       (case opt255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x254 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x254))
       ))]))
   and cvtDEFN (ClassDefn x280) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x280))
     | cvtDEFN (VariableDefn x283) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x283))
     | cvtDEFN (FunctionDefn x286) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x286))
     | cvtDEFN (ConstructorDefn x289) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x289))
     | cvtDEFN (InterfaceDefn x292) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x292))
     | cvtDEFN (NamespaceDefn x295) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x295))
     | cvtDEFN (TypeDefn x298) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x298))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls302, params=x306, paramTypes=ls308, 
          defaults=ls313, ctorInits=opt324, returnType=x328, thisType=opt330, 
          hasRest=b334}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x301 => cvtIDENT x301
                                   ) ls302)), ("params", cvtBINDINGS x306), 
          ("paramTypes", PrettyRep.List (List.map (fn x307 => cvtTYPE x307
                                                  ) ls308)), ("defaults", PrettyRep.List (List.map (fn x312 => 
                                                                                                          cvtEXPRESSION x312
                                                                                                   ) ls313)), 
          ("ctorInits", 
       (case opt324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x317, ls319) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x317, 
            PrettyRep.List (List.map (fn x318 => cvtEXPRESSION x318
                                     ) ls319)]))
       )), ("returnType", cvtTYPE x328), ("thisType", 
       (case opt330 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x329 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x329))
       )), ("hasRest", PrettyRep.Bool b334)]))
   and cvtBINDING (Binding{ident=x354, ty=x355}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x354), ("ty", cvtTYPE x355)]))
   and cvtBINDING_IDENT (TempIdent n363) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n363))
     | cvtBINDING_IDENT (ParamIdent n366) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n366))
     | cvtBINDING_IDENT (PropIdent x369) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x369))
   and cvtINIT_STEP (InitStep(x372, x373)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x372, 
          cvtEXPRESSION x373]))
     | cvtINIT_STEP (AssignStep(x377, x378)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x377, cvtEXPRESSION x378]))
   and cvtTYPE (SpecialType x382) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x382))
     | cvtTYPE (UnionType ls386) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x385 => 
                                                                                                      cvtTYPE x385
                                                                                               ) ls386)))
     | cvtTYPE (ArrayType ls393) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x392 => 
                                                                                                      cvtTYPE x392
                                                                                               ) ls393)))
     | cvtTYPE (TypeName(x399, opt401)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtIDENT_EXPRESSION x399, 
          
       (case opt401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x400 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x400))
       )]))
     | cvtTYPE (ElementTypeRef(x408, n409)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x408, PrettyRep.Int n409]))
     | cvtTYPE (FieldTypeRef(x413, x414)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x413, cvtIDENT x414]))
     | cvtTYPE (FunctionType x418) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x418))
     | cvtTYPE (ObjectType ls422) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x421 => 
                                                                                                        cvtFIELD_TYPE x421
                                                                                                 ) ls422)))
     | cvtTYPE (AppType{base=x428, args=ls430}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x428), ("args", PrettyRep.List (List.map (fn x429 => 
                                                                                                cvtTYPE x429
                                                                                         ) ls430))]))
     | cvtTYPE (LamType{params=ls442, body=x446}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x441 => 
                                                                          cvtIDENT x441
                                                                   ) ls442)), 
          ("body", cvtTYPE x446)]))
     | cvtTYPE (NullableType{expr=x454, nullable=b455}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE x454), ("nullable", PrettyRep.Bool b455)]))
     | cvtTYPE (InstanceType x463) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x463))
     | cvtTYPE (TypeVarFixtureRef x466) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x466))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x470) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x470))
     | cvtSTATEMENT (InitStmt{kind=x473, ns=opt475, prototype=b479, static=b480, 
          temps=x481, inits=ls483}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x473), ("ns", 
       (case opt475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x474 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x474))
       )), ("prototype", PrettyRep.Bool b479), ("static", PrettyRep.Bool b480), 
          ("temps", cvtBINDINGS x481), ("inits", PrettyRep.List (List.map (fn x482 => 
                                                                                 cvtINIT_STEP x482
                                                                          ) ls483))]))
     | cvtSTATEMENT (ClassBlock x502) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x502))
     | cvtSTATEMENT (ForInStmt x505) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x505))
     | cvtSTATEMENT (ThrowStmt x508) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x508))
     | cvtSTATEMENT (ReturnStmt x511) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x511))
     | cvtSTATEMENT (BreakStmt opt515) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt515 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x514 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x514))
       ))
     | cvtSTATEMENT (ContinueStmt opt522) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt522 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x521 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x521))
       ))
     | cvtSTATEMENT (BlockStmt x528) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x528))
     | cvtSTATEMENT (LabeledStmt(x531, x532)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENT x531, cvtSTATEMENT x532]))
     | cvtSTATEMENT (LetStmt x536) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x536))
     | cvtSTATEMENT (WhileStmt x539) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x539))
     | cvtSTATEMENT (DoWhileStmt x542) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x542))
     | cvtSTATEMENT (ForStmt x545) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x545))
     | cvtSTATEMENT (IfStmt{cnd=x548, thn=x549, els=x550}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x548), ("thn", cvtSTATEMENT x549), 
          ("els", cvtSTATEMENT x550)]))
     | cvtSTATEMENT (WithStmt{obj=x560, ty=x561, body=x562}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x560), ("ty", cvtTYPE x561), 
          ("body", cvtSTATEMENT x562)]))
     | cvtSTATEMENT (TryStmt{block=x572, catches=ls574, finally=opt579}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x572), 
          ("catches", PrettyRep.List (List.map (fn x573 => cvtCATCH_CLAUSE x573
                                               ) ls574)), ("finally", 
       (case opt579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x578 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x578))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x592, labels=ls594, cases=ls599}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x592), ("labels", PrettyRep.List (List.map (fn x593 => 
                                                                                                        cvtIDENT x593
                                                                                                 ) ls594)), 
          ("cases", PrettyRep.List (List.map (fn x598 => cvtCASE x598
                                             ) ls599))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x612, ty=x613, cases=ls615}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x612), ("ty", cvtTYPE x613), 
          ("cases", PrettyRep.List (List.map (fn x614 => cvtCATCH_CLAUSE x614
                                             ) ls615))]))
     | cvtSTATEMENT (DXNStmt{expr=x628}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x628)]))
   and cvtEXPRESSION (TernaryExpr(x634, x635, x636)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x634, cvtEXPRESSION x635, cvtEXPRESSION x636]))
     | cvtEXPRESSION (BinaryExpr(x640, x641, x642)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x640, cvtEXPRESSION x641, cvtEXPRESSION x642]))
     | cvtEXPRESSION (BinaryTypeExpr(x646, x647, x648)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x646, cvtEXPRESSION x647, cvtTYPE x648]))
     | cvtEXPRESSION (UnaryExpr(x652, x653)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x652, cvtEXPRESSION x653]))
     | cvtEXPRESSION (TypeExpr x657) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x657))
     | cvtEXPRESSION (ThisExpr opt661) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x660 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x660))
       ))
     | cvtEXPRESSION (YieldExpr opt668) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x667 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x667))
       ))
     | cvtEXPRESSION (SuperExpr opt675) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x674 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x674))
       ))
     | cvtEXPRESSION (CallExpr{func=x681, actuals=ls683}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x681), ("actuals", PrettyRep.List (List.map (fn x682 => 
                                                                                                         cvtEXPRESSION x682
                                                                                                  ) ls683))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x694, actuals=ls696}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x694), ("actuals", PrettyRep.List (List.map (fn x695 => 
                                                                                                         cvtTYPE x695
                                                                                                  ) ls696))]))
     | cvtEXPRESSION (LetExpr{defs=x707, body=x708, head=opt710}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x707), ("body", cvtEXPRESSION x708), 
          ("head", 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x709 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x709))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x723, actuals=ls725}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x723), ("actuals", PrettyRep.List (List.map (fn x724 => 
                                                                                                        cvtEXPRESSION x724
                                                                                                 ) ls725))]))
     | cvtEXPRESSION (ObjectRef{base=x736, ident=x737, loc=opt739}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPRESSION x736), ("ident", cvtIDENT_EXPRESSION x737), 
          ("loc", 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x738))
       ))]))
     | cvtEXPRESSION (LexicalRef{ident=x752, loc=opt754}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPRESSION x752), ("loc", 
          
       (case opt754 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x753 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x753))
       ))]))
     | cvtEXPRESSION (SetExpr(x765, x766, x767)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x765, cvtEXPRESSION x766, cvtEXPRESSION x767]))
     | cvtEXPRESSION (ListExpr ls772) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x771 => 
                                                                                                          cvtEXPRESSION x771
                                                                                                   ) ls772)))
     | cvtEXPRESSION (InitExpr(x778, x779, x780)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x778, cvtHEAD x779, cvtINITS x780]))
     | cvtEXPRESSION (GetTemp n784) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n784))
     | cvtEXPRESSION (GetParam n787) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n787))
     | cvtEXPRESSION (Comprehension(x790, ls792, opt797)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x790, PrettyRep.List (List.map (fn x791 => 
                                                                                     cvtFOR_ENUM_HEAD x791
                                                                              ) ls792), 
          
       (case opt797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x796 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x796))
       )]))
     | cvtEXPRESSION (LiteralExpr x804) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x804))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n812) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n812))
     | cvtFIXTURE_NAME (PropName x815) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x815))
   and cvtIDENT_EXPRESSION (Identifier{ident=x818, openNamespaces=ls824, rootRib=opt829}) = 
          PrettyRep.Ctor ("Identifier", SOME (PrettyRep.Rec [("ident", cvtIDENT x818), 
          ("openNamespaces", PrettyRep.List (List.map (fn ls820 => PrettyRep.List (List.map (fn x819 => 
                                                                                                   cvtNAMESPACE x819
                                                                                            ) ls820)
                                                      ) ls824)), ("rootRib", 
          
       (case opt829 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x828 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x828))
       ))]))
     | cvtIDENT_EXPRESSION (QualifiedExpression{qual=x842, expr=x843}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPRESSION x842), ("expr", cvtEXPRESSION x843)]))
     | cvtIDENT_EXPRESSION (AttributeIdentifier x851) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPRESSION x851))
     | cvtIDENT_EXPRESSION (ExpressionIdentifier{expr=x854, openNamespaces=ls860}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x854), ("openNamespaces", PrettyRep.List (List.map (fn ls856 => 
                                                                                  PrettyRep.List (List.map (fn x855 => 
                                                                                                                  cvtNAMESPACE x855
                                                                                                           ) ls856)
                                                                           ) ls860))]))
     | cvtIDENT_EXPRESSION (QualifiedIdentifier{qual=x871, ident=s872}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPRESSION x871), ("ident", PrettyRep.UniStr s872)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r882) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r882))
     | cvtLITERAL (LiteralDecimal d885) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d885))
     | cvtLITERAL (LiteralBoolean b888) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b888))
     | cvtLITERAL (LiteralString s891) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s891))
     | cvtLITERAL (LiteralArray{exprs=x894, ty=opt896}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x894), ("ty", 
       (case opt896 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x895 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x895))
       ))]))
     | cvtLITERAL (LiteralXML ls908) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x907 => 
                                                                                                           cvtEXPRESSION x907
                                                                                                    ) ls908)))
     | cvtLITERAL (LiteralNamespace x914) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x914))
     | cvtLITERAL (LiteralObject{expr=ls918, ty=opt923}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x917 => 
                                                                        cvtFIELD x917
                                                                 ) ls918)), 
          ("ty", 
       (case opt923 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x922 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x922))
       ))]))
     | cvtLITERAL (LiteralFunction x934) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x934))
     | cvtLITERAL (LiteralRegExp{str=s937}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s937)]))
   and cvtBLOCK (Block x943) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x943))
   and cvtFIXTURE (NamespaceFixture x946) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x946))
     | cvtFIXTURE (ClassFixture x949) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x949))
     | cvtFIXTURE (InterfaceFixture x952) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x952))
     | cvtFIXTURE (TypeVarFixture x955) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x955))
     | cvtFIXTURE (TypeFixture x958) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE x958))
     | cvtFIXTURE (MethodFixture{func=x961, ty=x962, readOnly=b963, override=b964, 
          final=b965}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x961), ("ty", cvtTYPE x962), ("readOnly", PrettyRep.Bool b963), 
          ("override", PrettyRep.Bool b964), ("final", PrettyRep.Bool b965)]))
     | cvtFIXTURE (ValFixture{ty=x979, readOnly=b980}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x979), ("readOnly", PrettyRep.Bool b980)]))
     | cvtFIXTURE (VirtualValFixture{ty=x988, getter=opt990, setter=opt995}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x988), ("getter", 
       (case opt990 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x989 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x989))
       )), ("setter", 
       (case opt995 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x994 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x994))
       ))]))
   and cvtHEAD (Head(x1008, x1009)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1008, 
          cvtINITS x1009]))
   and cvtBINDINGS (ls1014, ls1019) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1013 => 
                                                                                       cvtBINDING x1013
                                                                                ) ls1014), 
          PrettyRep.List (List.map (fn x1018 => cvtINIT_STEP x1018
                                   ) ls1019)]
   and cvtRIB ls1027 = PrettyRep.List (List.map (fn (x1024, x1025) => PrettyRep.Tuple [cvtFIXTURE_NAME x1024, 
                                                       cvtFIXTURE x1025]
                                                ) ls1027)
   and cvtRIBS ls1038 = PrettyRep.List (List.map (fn ls1034 => PrettyRep.List (List.map (fn (x1031, 
                                                                                               x1032) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1031, 
                                                                                               cvtFIXTURE x1032]
                                                                                        ) ls1034)
                                                 ) ls1038)
   and cvtINITS ls1045 = PrettyRep.List (List.map (fn (x1042, x1043) => PrettyRep.Tuple [cvtFIXTURE_NAME x1042, 
                                                         cvtEXPRESSION x1043]
                                                  ) ls1045)
   and cvtINSTANCE_TYPE {name=x1049, typeParams=ls1051, typeArgs=ls1056, nonnullable=b1060, 
          superTypes=ls1062, ty=x1066, dynamic=b1067} = PrettyRep.Rec [("name", 
          cvtNAME x1049), ("typeParams", PrettyRep.List (List.map (fn x1050 => 
                                                                         cvtIDENT x1050
                                                                  ) ls1051)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1055 => cvtTYPE x1055
                                                ) ls1056)), ("nonnullable", 
          PrettyRep.Bool b1060), ("superTypes", PrettyRep.List (List.map (fn x1061 => 
                                                                                cvtTYPE x1061
                                                                         ) ls1062)), 
          ("ty", cvtTYPE x1066), ("dynamic", PrettyRep.Bool b1067)]
   and cvtFIELD {kind=x1083, name=x1084, init=x1085} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1083), ("name", cvtIDENT_EXPRESSION x1084), ("init", 
          cvtEXPRESSION x1085)]
   and cvtFIELD_TYPE {name=x1093, ty=x1094} = PrettyRep.Rec [("name", cvtIDENT x1093), 
          ("ty", cvtTYPE x1094)]
   and cvtFUNC_TYPE {params=ls1101, result=x1105, thisType=opt1107, hasRest=b1111, 
          minArgs=n1112} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1100 => 
                                                                                     cvtTYPE x1100
                                                                              ) ls1101)), 
          ("result", cvtTYPE x1105), ("thisType", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1106))
       )), ("hasRest", PrettyRep.Bool b1111), ("minArgs", PrettyRep.Int n1112)]
   and cvtFUNC_DEFN {kind=x1124, ns=opt1126, final=b1130, override=b1131, prototype=b1132, 
          static=b1133, func=x1134} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1124), 
          ("ns", 
       (case opt1126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1125 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1125))
       )), ("final", PrettyRep.Bool b1130), ("override", PrettyRep.Bool b1131), 
          ("prototype", PrettyRep.Bool b1132), ("static", PrettyRep.Bool b1133), 
          ("func", cvtFUNC x1134)]
   and cvtCTOR_DEFN x1150 = cvtCTOR x1150
   and cvtVAR_DEFN {kind=x1151, ns=opt1153, static=b1157, prototype=b1158, 
          bindings=(ls1160, ls1165)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1151), 
          ("ns", 
       (case opt1153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1152 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1152))
       )), ("static", PrettyRep.Bool b1157), ("prototype", PrettyRep.Bool b1158), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1159 => 
                                                                        cvtBINDING x1159
                                                                 ) ls1160), 
          PrettyRep.List (List.map (fn x1164 => cvtINIT_STEP x1164
                                   ) ls1165)])]
   and cvtNAMESPACE_DEFN {ident=x1181, ns=opt1183, init=opt1188} = PrettyRep.Rec [("ident", 
          cvtIDENT x1181), ("ns", 
       (case opt1183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1182 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1182))
       )), ("init", 
       (case opt1188 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1187 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1187))
       ))]
   and cvtCLASS_DEFN {ns=opt1200, privateNS=x1204, protectedNS=x1205, ident=x1206, 
          nonnullable=b1207, dynamic=b1208, final=b1209, params=ls1211, extends=opt1216, 
          implements=ls1221, classDefns=ls1226, instanceDefns=ls1231, instanceStmts=ls1236, 
          ctorDefn=opt1241} = PrettyRep.Rec [("ns", 
       (case opt1200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1199 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1199))
       )), ("privateNS", cvtNAMESPACE x1204), ("protectedNS", cvtNAMESPACE x1205), 
          ("ident", cvtIDENT x1206), ("nonnullable", PrettyRep.Bool b1207), 
          ("dynamic", PrettyRep.Bool b1208), ("final", PrettyRep.Bool b1209), 
          ("params", PrettyRep.List (List.map (fn x1210 => cvtIDENT x1210
                                              ) ls1211)), ("extends", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1215))
       )), ("implements", PrettyRep.List (List.map (fn x1220 => cvtTYPE x1220
                                                   ) ls1221)), ("classDefns", 
          PrettyRep.List (List.map (fn x1225 => cvtDEFN x1225
                                   ) ls1226)), ("instanceDefns", PrettyRep.List (List.map (fn x1230 => 
                                                                                                 cvtDEFN x1230
                                                                                          ) ls1231)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1235 => cvtSTATEMENT x1235
                                                     ) ls1236)), ("ctorDefn", 
          
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1240))
       ))]
   and cvtINTERFACE_DEFN {ident=x1274, ns=opt1276, nonnullable=b1280, params=ls1282, 
          extends=ls1287, instanceDefns=ls1292} = PrettyRep.Rec [("ident", 
          cvtIDENT x1274), ("ns", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1275))
       )), ("nonnullable", PrettyRep.Bool b1280), ("params", PrettyRep.List (List.map (fn x1281 => 
                                                                                             cvtIDENT x1281
                                                                                      ) ls1282)), 
          ("extends", PrettyRep.List (List.map (fn x1286 => cvtTYPE x1286
                                               ) ls1287)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1291 => cvtDEFN x1291
                                   ) ls1292))]
   and cvtTYPE_DEFN {ident=x1309, ns=opt1311, init=x1315} = PrettyRep.Rec [("ident", 
          cvtIDENT x1309), ("ns", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1310))
       )), ("init", cvtTYPE x1315)]
   and cvtCLASS_BLOCK {ns=opt1324, protectedNS=x1328, privateNS=x1329, ident=x1330, 
          name=opt1332, block=x1336} = PrettyRep.Rec [("ns", 
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1323))
       )), ("protectedNS", cvtNAMESPACE x1328), ("privateNS", cvtNAMESPACE x1329), 
          ("ident", cvtIDENT x1330), ("name", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1331))
       )), ("block", cvtBLOCK x1336)]
   and cvtFOR_ENUM_HEAD {isEach=b1350, bindings=(ls1352, ls1357), expr=x1362} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1350), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1351 => 
                                                                                                                         cvtBINDING x1351
                                                                                                                  ) ls1352), 
          PrettyRep.List (List.map (fn x1356 => cvtINIT_STEP x1356
                                   ) ls1357)]), ("expr", cvtEXPRESSION x1362)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1370, defn=opt1401, obj=x1405, rib=opt1413, 
          next=x1417, labels=ls1419, body=x1423} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1370), ("defn", 
       (case opt1401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1371, ns=opt1373, static=b1377, prototype=b1378, bindings=(ls1380, 
            ls1385)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1371), ("ns", 
         (case opt1373 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1372 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1372))
         )), ("static", PrettyRep.Bool b1377), ("prototype", PrettyRep.Bool b1378), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1379 => 
                                                                          cvtBINDING x1379
                                                                   ) ls1380), 
            PrettyRep.List (List.map (fn x1384 => cvtINIT_STEP x1384
                                     ) ls1385)])]))
       )), ("obj", cvtEXPRESSION x1405), ("rib", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1409 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1406, 
                                                                                      x1407) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1406, 
                                                                                      cvtFIXTURE x1407]
                                                                               ) ls1409)))
       )), ("next", cvtSTATEMENT x1417), ("labels", PrettyRep.List (List.map (fn x1418 => 
                                                                                    cvtIDENT x1418
                                                                             ) ls1419)), 
          ("body", cvtSTATEMENT x1423)]
   and cvtFOR_STATEMENT {rib=opt1446, defn=opt1480, init=ls1485, cond=x1489, 
          update=x1490, labels=ls1492, body=x1496} = PrettyRep.Rec [("rib", 
          
       (case opt1446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1442 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1439, 
                                                                                      x1440) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1439, 
                                                                                      cvtFIXTURE x1440]
                                                                               ) ls1442)))
       )), ("defn", 
       (case opt1480 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1450, ns=opt1452, static=b1456, prototype=b1457, bindings=(ls1459, 
            ls1464)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1450), ("ns", 
         (case opt1452 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1451 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1451))
         )), ("static", PrettyRep.Bool b1456), ("prototype", PrettyRep.Bool b1457), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1458 => 
                                                                          cvtBINDING x1458
                                                                   ) ls1459), 
            PrettyRep.List (List.map (fn x1463 => cvtINIT_STEP x1463
                                     ) ls1464)])]))
       )), ("init", PrettyRep.List (List.map (fn x1484 => cvtSTATEMENT x1484
                                             ) ls1485)), ("cond", cvtEXPRESSION x1489), 
          ("update", cvtEXPRESSION x1490), ("labels", PrettyRep.List (List.map (fn x1491 => 
                                                                                      cvtIDENT x1491
                                                                               ) ls1492)), 
          ("body", cvtSTATEMENT x1496)]
   and cvtWHILE_STATEMENT {cond=x1512, rib=opt1520, body=x1524, labels=ls1526} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1512), ("rib", 
       (case opt1520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1516 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1513, 
                                                                                      x1514) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1513, 
                                                                                      cvtFIXTURE x1514]
                                                                               ) ls1516)))
       )), ("body", cvtSTATEMENT x1524), ("labels", PrettyRep.List (List.map (fn x1525 => 
                                                                                    cvtIDENT x1525
                                                                             ) ls1526))]
   and cvtDIRECTIVES {pragmas=ls1540, defns=ls1545, head=opt1550, body=ls1555, 
          loc=opt1560} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1539 => 
                                                                                    cvtPRAGMA x1539
                                                                             ) ls1540)), 
          ("defns", PrettyRep.List (List.map (fn x1544 => cvtDEFN x1544
                                             ) ls1545)), ("head", 
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1549))
       )), ("body", PrettyRep.List (List.map (fn x1554 => cvtSTATEMENT x1554
                                             ) ls1555)), ("loc", 
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1559 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1559))
       ))]
   and cvtCASE {label=opt1576, inits=opt1587, body=x1591} = PrettyRep.Rec [("label", 
          
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1575 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1575))
       )), ("inits", 
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1583 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1580, 
                                                                                      x1581) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1580, 
                                                                                      cvtEXPRESSION x1581]
                                                                               ) ls1583)))
       )), ("body", cvtBLOCK x1591)]
   and cvtCATCH_CLAUSE {bindings=(ls1600, ls1605), ty=x1610, rib=opt1618, inits=opt1629, 
          block=x1633} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1599 => 
                                                                                                      cvtBINDING x1599
                                                                                               ) ls1600), 
          PrettyRep.List (List.map (fn x1604 => cvtINIT_STEP x1604
                                   ) ls1605)]), ("ty", cvtTYPE x1610), ("rib", 
          
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1614 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1611, 
                                                                                      x1612) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1611, 
                                                                                      cvtFIXTURE x1612]
                                                                               ) ls1614)))
       )), ("inits", 
       (case opt1629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1625 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1622, 
                                                                                      x1623) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1622, 
                                                                                      cvtEXPRESSION x1623]
                                                                               ) ls1625)))
       )), ("block", cvtBLOCK x1633)]
   and cvtFUNC_NAME {kind=x1645, ident=x1646} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1645), 
          ("ident", cvtIDENT x1646)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1652, getter=opt1654, setter=opt1659} = 
          PrettyRep.Rec [("ty", cvtTYPE x1652), ("getter", 
       (case opt1654 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1653 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1653))
       )), ("setter", 
       (case opt1659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1658 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1658))
       ))]
   and cvtFRAGMENT (Anon x1670) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1670))
end

