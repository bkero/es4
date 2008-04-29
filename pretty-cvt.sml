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
   and cvtMULTINAME {nss=ls40, id=x44} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls36 => 
                                                                                                PrettyRep.List (List.map (fn x35 => 
                                                                                                                                cvtNAMESPACE x35
                                                                                                                         ) ls36)
                                                                                         ) ls40)), 
          ("id", cvtIDENTIFIER x44)]
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
          ("typeParams", PrettyRep.List (List.map (fn x133 => cvtIDENTIFIER x133
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
                                                                                                      cvtIDENTIFIER x194
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
          PrettyRep.List (List.map (fn x301 => cvtIDENTIFIER x301
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
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x354), ("ty", 
          cvtTYPE x355)]))
   and cvtBINDING_IDENTIFIER (TempIdent n363) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n363))
     | cvtBINDING_IDENTIFIER (ParamIdent n366) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n366))
     | cvtBINDING_IDENTIFIER (PropIdent x369) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x369))
   and cvtINIT_STEP (InitStep(x372, x373)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x372, 
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
     | cvtTYPE (TypeName(x399, opt401)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtIDENTIFIER_EXPRESSION x399, 
          
       (case opt401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x400 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x400))
       )]))
     | cvtTYPE (ElementTypeRef(x408, n409)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x408, PrettyRep.Int n409]))
     | cvtTYPE (FieldTypeRef(x413, x414)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x413, cvtIDENTIFIER x414]))
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
                                                                          cvtIDENTIFIER x441
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
       | SOME x514 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x514))
       ))
     | cvtSTATEMENT (ContinueStmt opt522) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt522 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x521 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x521))
       ))
     | cvtSTATEMENT (BlockStmt x528) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x528))
     | cvtSTATEMENT (LabeledStmt(x531, x532)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x531, cvtSTATEMENT x532]))
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
                                                                                                        cvtIDENTIFIER x593
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
          SOME (PrettyRep.Rec [("base", cvtEXPRESSION x736), ("ident", cvtIDENTIFIER_EXPRESSION x737), 
          ("loc", 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x738))
       ))]))
     | cvtEXPRESSION (LexicalRef{ident=x752, loc=opt754}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENTIFIER_EXPRESSION x752), ("loc", 
          
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
   and cvtIDENTIFIER_EXPRESSION (Identifier{ident=x818, openNamespaces=ls824, 
          rootRib=opt829}) = PrettyRep.Ctor ("Identifier", SOME (PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x818), ("openNamespaces", PrettyRep.List (List.map (fn ls820 => 
                                                                                  PrettyRep.List (List.map (fn x819 => 
                                                                                                                  cvtNAMESPACE x819
                                                                                                           ) ls820)
                                                                           ) ls824)), 
          ("rootRib", 
       (case opt829 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x828 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x828))
       ))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedExpression{qual=x842, expr=x843}) = 
          PrettyRep.Ctor ("QualifiedExpression", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x842), ("expr", cvtEXPRESSION x843)]))
     | cvtIDENTIFIER_EXPRESSION (ExpressionIdentifier{expr=x851, openNamespaces=ls857}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x851), ("openNamespaces", PrettyRep.List (List.map (fn ls853 => 
                                                                                  PrettyRep.List (List.map (fn x852 => 
                                                                                                                  cvtNAMESPACE x852
                                                                                                           ) ls853)
                                                                           ) ls857))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedIdentifier{qual=x868, ident=s869}) = 
          PrettyRep.Ctor ("QualifiedIdentifier", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x868), ("ident", PrettyRep.UniStr s869)]))
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
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x891), ("ty", 
       (case opt893 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x892 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x892))
       ))]))
     | cvtLITERAL (LiteralXML ls905) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x904 => 
                                                                                                           cvtEXPRESSION x904
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
       | SOME x919 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x919))
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
     | cvtFIXTURE (TypeFixture x955) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE x955))
     | cvtFIXTURE (MethodFixture{func=x958, ty=x959, readOnly=b960, override=b961, 
          final=b962}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x958), ("ty", cvtTYPE x959), ("readOnly", PrettyRep.Bool b960), 
          ("override", PrettyRep.Bool b961), ("final", PrettyRep.Bool b962)]))
     | cvtFIXTURE (ValFixture{ty=x976, readOnly=b977}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x976), ("readOnly", PrettyRep.Bool b977)]))
     | cvtFIXTURE (VirtualValFixture{ty=x985, getter=opt987, setter=opt992}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x985), ("getter", 
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
   and cvtRIBS ls1035 = PrettyRep.List (List.map (fn ls1031 => PrettyRep.List (List.map (fn (x1028, 
                                                                                               x1029) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1028, 
                                                                                               cvtFIXTURE x1029]
                                                                                        ) ls1031)
                                                 ) ls1035)
   and cvtINITS ls1042 = PrettyRep.List (List.map (fn (x1039, x1040) => PrettyRep.Tuple [cvtFIXTURE_NAME x1039, 
                                                         cvtEXPRESSION x1040]
                                                  ) ls1042)
   and cvtINSTANCE_TYPE {name=x1046, typeParams=ls1048, typeArgs=ls1053, nonnullable=b1057, 
          superTypes=ls1059, ty=x1063, dynamic=b1064} = PrettyRep.Rec [("name", 
          cvtNAME x1046), ("typeParams", PrettyRep.List (List.map (fn x1047 => 
                                                                         cvtIDENTIFIER x1047
                                                                  ) ls1048)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1052 => cvtTYPE x1052
                                                ) ls1053)), ("nonnullable", 
          PrettyRep.Bool b1057), ("superTypes", PrettyRep.List (List.map (fn x1058 => 
                                                                                cvtTYPE x1058
                                                                         ) ls1059)), 
          ("ty", cvtTYPE x1063), ("dynamic", PrettyRep.Bool b1064)]
   and cvtFIELD {kind=x1080, name=x1081, init=x1082} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1080), ("name", cvtIDENTIFIER_EXPRESSION x1081), 
          ("init", cvtEXPRESSION x1082)]
   and cvtFIELD_TYPE {name=x1090, ty=x1091} = PrettyRep.Rec [("name", cvtIDENTIFIER x1090), 
          ("ty", cvtTYPE x1091)]
   and cvtFUNC_TYPE {params=ls1098, result=x1102, thisType=opt1104, hasRest=b1108, 
          minArgs=n1109} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1097 => 
                                                                                     cvtTYPE x1097
                                                                              ) ls1098)), 
          ("result", cvtTYPE x1102), ("thisType", 
       (case opt1104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1103 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1103))
       )), ("hasRest", PrettyRep.Bool b1108), ("minArgs", PrettyRep.Int n1109)]
   and cvtFUNC_DEFN {kind=x1121, ns=opt1123, final=b1127, override=b1128, prototype=b1129, 
          static=b1130, func=x1131} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1121), 
          ("ns", 
       (case opt1123 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1122 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1122))
       )), ("final", PrettyRep.Bool b1127), ("override", PrettyRep.Bool b1128), 
          ("prototype", PrettyRep.Bool b1129), ("static", PrettyRep.Bool b1130), 
          ("func", cvtFUNC x1131)]
   and cvtCTOR_DEFN x1147 = cvtCTOR x1147
   and cvtVAR_DEFN {kind=x1148, ns=opt1150, static=b1154, prototype=b1155, 
          bindings=(ls1157, ls1162)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1148), 
          ("ns", 
       (case opt1150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1149 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1149))
       )), ("static", PrettyRep.Bool b1154), ("prototype", PrettyRep.Bool b1155), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1156 => 
                                                                        cvtBINDING x1156
                                                                 ) ls1157), 
          PrettyRep.List (List.map (fn x1161 => cvtINIT_STEP x1161
                                   ) ls1162)])]
   and cvtNAMESPACE_DEFN {ident=x1178, ns=opt1180, init=opt1185} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1178), ("ns", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1179))
       )), ("init", 
       (case opt1185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1184 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1184))
       ))]
   and cvtCLASS_DEFN {ns=opt1197, privateNS=x1201, protectedNS=x1202, ident=x1203, 
          nonnullable=b1204, dynamic=b1205, final=b1206, params=ls1208, extends=opt1213, 
          implements=ls1218, classDefns=ls1223, instanceDefns=ls1228, instanceStmts=ls1233, 
          ctorDefn=opt1238} = PrettyRep.Rec [("ns", 
       (case opt1197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1196 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1196))
       )), ("privateNS", cvtNAMESPACE x1201), ("protectedNS", cvtNAMESPACE x1202), 
          ("ident", cvtIDENTIFIER x1203), ("nonnullable", PrettyRep.Bool b1204), 
          ("dynamic", PrettyRep.Bool b1205), ("final", PrettyRep.Bool b1206), 
          ("params", PrettyRep.List (List.map (fn x1207 => cvtIDENTIFIER x1207
                                              ) ls1208)), ("extends", 
       (case opt1213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1212 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1212))
       )), ("implements", PrettyRep.List (List.map (fn x1217 => cvtTYPE x1217
                                                   ) ls1218)), ("classDefns", 
          PrettyRep.List (List.map (fn x1222 => cvtDEFN x1222
                                   ) ls1223)), ("instanceDefns", PrettyRep.List (List.map (fn x1227 => 
                                                                                                 cvtDEFN x1227
                                                                                          ) ls1228)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1232 => cvtSTATEMENT x1232
                                                     ) ls1233)), ("ctorDefn", 
          
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1237))
       ))]
   and cvtINTERFACE_DEFN {ident=x1271, ns=opt1273, nonnullable=b1277, params=ls1279, 
          extends=ls1284, instanceDefns=ls1289} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1271), ("ns", 
       (case opt1273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1272 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1272))
       )), ("nonnullable", PrettyRep.Bool b1277), ("params", PrettyRep.List (List.map (fn x1278 => 
                                                                                             cvtIDENTIFIER x1278
                                                                                      ) ls1279)), 
          ("extends", PrettyRep.List (List.map (fn x1283 => cvtTYPE x1283
                                               ) ls1284)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1288 => cvtDEFN x1288
                                   ) ls1289))]
   and cvtTYPE_DEFN {ident=x1306, ns=opt1308, init=x1312} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1306), ("ns", 
       (case opt1308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1307 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1307))
       )), ("init", cvtTYPE x1312)]
   and cvtCLASS_BLOCK {ns=opt1321, protectedNS=x1325, privateNS=x1326, ident=x1327, 
          name=opt1329, block=x1333} = PrettyRep.Rec [("ns", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1320))
       )), ("protectedNS", cvtNAMESPACE x1325), ("privateNS", cvtNAMESPACE x1326), 
          ("ident", cvtIDENTIFIER x1327), ("name", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1328))
       )), ("block", cvtBLOCK x1333)]
   and cvtFOR_ENUM_HEAD {isEach=b1347, bindings=(ls1349, ls1354), expr=x1359} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1347), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1348 => 
                                                                                                                         cvtBINDING x1348
                                                                                                                  ) ls1349), 
          PrettyRep.List (List.map (fn x1353 => cvtINIT_STEP x1353
                                   ) ls1354)]), ("expr", cvtEXPRESSION x1359)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1367, defn=opt1398, obj=x1402, rib=opt1410, 
          next=x1414, labels=ls1416, body=x1420} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1367), ("defn", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1368, ns=opt1370, static=b1374, prototype=b1375, bindings=(ls1377, 
            ls1382)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1368), ("ns", 
         (case opt1370 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1369 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1369))
         )), ("static", PrettyRep.Bool b1374), ("prototype", PrettyRep.Bool b1375), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1376 => 
                                                                          cvtBINDING x1376
                                                                   ) ls1377), 
            PrettyRep.List (List.map (fn x1381 => cvtINIT_STEP x1381
                                     ) ls1382)])]))
       )), ("obj", cvtEXPRESSION x1402), ("rib", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1406 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1403, 
                                                                                      x1404) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1403, 
                                                                                      cvtFIXTURE x1404]
                                                                               ) ls1406)))
       )), ("next", cvtSTATEMENT x1414), ("labels", PrettyRep.List (List.map (fn x1415 => 
                                                                                    cvtIDENTIFIER x1415
                                                                             ) ls1416)), 
          ("body", cvtSTATEMENT x1420)]
   and cvtFOR_STATEMENT {rib=opt1443, defn=opt1477, init=ls1482, cond=x1486, 
          update=x1487, labels=ls1489, body=x1493} = PrettyRep.Rec [("rib", 
          
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1439 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1436, 
                                                                                      x1437) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1436, 
                                                                                      cvtFIXTURE x1437]
                                                                               ) ls1439)))
       )), ("defn", 
       (case opt1477 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1447, ns=opt1449, static=b1453, prototype=b1454, bindings=(ls1456, 
            ls1461)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1447), ("ns", 
         (case opt1449 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1448 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1448))
         )), ("static", PrettyRep.Bool b1453), ("prototype", PrettyRep.Bool b1454), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1455 => 
                                                                          cvtBINDING x1455
                                                                   ) ls1456), 
            PrettyRep.List (List.map (fn x1460 => cvtINIT_STEP x1460
                                     ) ls1461)])]))
       )), ("init", PrettyRep.List (List.map (fn x1481 => cvtSTATEMENT x1481
                                             ) ls1482)), ("cond", cvtEXPRESSION x1486), 
          ("update", cvtEXPRESSION x1487), ("labels", PrettyRep.List (List.map (fn x1488 => 
                                                                                      cvtIDENTIFIER x1488
                                                                               ) ls1489)), 
          ("body", cvtSTATEMENT x1493)]
   and cvtWHILE_STATEMENT {cond=x1509, rib=opt1517, body=x1521, labels=ls1523} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1509), ("rib", 
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1513 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1510, 
                                                                                      x1511) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1510, 
                                                                                      cvtFIXTURE x1511]
                                                                               ) ls1513)))
       )), ("body", cvtSTATEMENT x1521), ("labels", PrettyRep.List (List.map (fn x1522 => 
                                                                                    cvtIDENTIFIER x1522
                                                                             ) ls1523))]
   and cvtDIRECTIVES {pragmas=ls1537, defns=ls1542, head=opt1547, body=ls1552, 
          loc=opt1557} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1536 => 
                                                                                    cvtPRAGMA x1536
                                                                             ) ls1537)), 
          ("defns", PrettyRep.List (List.map (fn x1541 => cvtDEFN x1541
                                             ) ls1542)), ("head", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1546))
       )), ("body", PrettyRep.List (List.map (fn x1551 => cvtSTATEMENT x1551
                                             ) ls1552)), ("loc", 
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1556))
       ))]
   and cvtCASE {label=opt1573, inits=opt1584, body=x1588} = PrettyRep.Rec [("label", 
          
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1572 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1572))
       )), ("inits", 
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1580 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1577, 
                                                                                      x1578) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1577, 
                                                                                      cvtEXPRESSION x1578]
                                                                               ) ls1580)))
       )), ("body", cvtBLOCK x1588)]
   and cvtCATCH_CLAUSE {bindings=(ls1597, ls1602), ty=x1607, rib=opt1615, inits=opt1626, 
          block=x1630} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1596 => 
                                                                                                      cvtBINDING x1596
                                                                                               ) ls1597), 
          PrettyRep.List (List.map (fn x1601 => cvtINIT_STEP x1601
                                   ) ls1602)]), ("ty", cvtTYPE x1607), ("rib", 
          
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1611 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1608, 
                                                                                      x1609) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1608, 
                                                                                      cvtFIXTURE x1609]
                                                                               ) ls1611)))
       )), ("inits", 
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1622 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1619, 
                                                                                      x1620) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1619, 
                                                                                      cvtEXPRESSION x1620]
                                                                               ) ls1622)))
       )), ("block", cvtBLOCK x1630)]
   and cvtFUNC_NAME {kind=x1642, ident=x1643} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1642), 
          ("ident", cvtIDENTIFIER x1643)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1649, getter=opt1651, setter=opt1656} = 
          PrettyRep.Rec [("ty", cvtTYPE x1649), ("getter", 
       (case opt1651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1650 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1650))
       )), ("setter", 
       (case opt1656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1655 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1655))
       ))]
   and cvtFRAGMENT (Anon x1667) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1667))
end

