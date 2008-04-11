structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtRIB_ID n20 = PrettyRep.Int n20
   and cvtTYPEVAR_NONCE n21 = PrettyRep.Int n21
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Private x23) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x23))
     | cvtNAMESPACE (Protected x26) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x26))
     | cvtNAMESPACE (Public x29) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x29))
     | cvtNAMESPACE (Internal x32) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x32))
     | cvtNAMESPACE (UserNamespace s35) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s35))
     | cvtNAMESPACE (AnonUserNamespace n38) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n38))
     | cvtNAMESPACE (LimitedNamespace(x41, x42)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x41, cvtNAMESPACE x42]))
   and cvtNAME {ns=x46, id=x47} = PrettyRep.Rec [("ns", cvtNAMESPACE x46), 
          ("id", cvtIDENT x47)]
   and cvtMULTINAME {nss=ls58, id=x62} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls54 => 
                                                                                                PrettyRep.List (List.map (fn x53 => 
                                                                                                                                cvtNAMESPACE x53
                                                                                                                         ) ls54)
                                                                                         ) ls58)), 
          ("id", cvtIDENT x62)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (Wrap) = PrettyRep.Ctor ("Wrap", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
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
     | cvtUNOP (Splat) = PrettyRep.Ctor ("Splat", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x131) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x131))
     | cvtPRAGMA (UseDefaultNamespace x134) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x134))
     | cvtPRAGMA (UseDecimalContext x137) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x137))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls143, name=x147}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x142 => 
                                                                           cvtIDENT x142
                                                                    ) ls143)), 
          ("name", cvtIDENT x147)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x161, typeParams=ls163, nonnullable=b167, dynamic=b168, 
          extends=opt170, implements=ls175, classRib=x179, instanceRib=x180, 
          instanceInits=x181, constructor=opt183, classType=x187, instanceType=x188}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x161), 
          ("typeParams", PrettyRep.List (List.map (fn x162 => cvtIDENT x162
                                                  ) ls163)), ("nonnullable", 
          PrettyRep.Bool b167), ("dynamic", PrettyRep.Bool b168), ("extends", 
          
       (case opt170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x169 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x169))
       )), ("implements", PrettyRep.List (List.map (fn x174 => cvtTYPE_EXPR x174
                                                   ) ls175)), ("classRib", 
          cvtRIB x179), ("instanceRib", cvtRIB x180), ("instanceInits", cvtHEAD x181), 
          ("constructor", 
       (case opt183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x182))
       )), ("classType", cvtTYPE_EXPR x187), ("instanceType", cvtTYPE_EXPR x188)]))
   and cvtIFACE (Iface{name=x216, typeParams=ls218, nonnullable=b222, extends=ls224, 
          instanceRib=x228, instanceType=x229}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x216), ("typeParams", PrettyRep.List (List.map (fn x217 => 
                                                                                                      cvtIDENT x217
                                                                                               ) ls218)), 
          ("nonnullable", PrettyRep.Bool b222), ("extends", PrettyRep.List (List.map (fn x223 => 
                                                                                            cvtTYPE_EXPR x223
                                                                                     ) ls224)), 
          ("instanceRib", cvtRIB x228), ("instanceType", cvtTYPE_EXPR x229)]))
   and cvtCTOR (Ctor{settings=x245, superArgs=ls247, func=x251}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x245), ("superArgs", PrettyRep.List (List.map (fn x246 => 
                                                                                                         cvtEXPR x246
                                                                                                  ) ls247)), 
          ("func", cvtFUNC x251)]))
   and cvtFUNC (Func{name=x261, fsig=x262, native=b263, block=opt265, param=x269, 
          defaults=ls271, ty=x275, loc=opt277}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x261), ("fsig", cvtFUNC_SIG x262), ("native", PrettyRep.Bool b263), 
          ("block", 
       (case opt265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x264 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x264))
       )), ("param", cvtHEAD x269), ("defaults", PrettyRep.List (List.map (fn x270 => 
                                                                                 cvtEXPR x270
                                                                          ) ls271)), 
          ("ty", cvtTYPE_EXPR x275), ("loc", 
       (case opt277 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x276 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x276))
       ))]))
   and cvtDEFN (ClassDefn x300) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x300))
     | cvtDEFN (VariableDefn x303) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x303))
     | cvtDEFN (FunctionDefn x306) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x306))
     | cvtDEFN (ConstructorDefn x309) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x309))
     | cvtDEFN (InterfaceDefn x312) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x312))
     | cvtDEFN (NamespaceDefn x315) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x315))
     | cvtDEFN (TypeDefn x318) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x318))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls322, params=x326, paramTypes=ls328, 
          defaults=ls333, ctorInits=opt344, returnType=x348, thisType=opt350, 
          hasRest=b354}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x321 => cvtIDENT x321
                                   ) ls322)), ("params", cvtBINDINGS x326), 
          ("paramTypes", PrettyRep.List (List.map (fn x327 => cvtTYPE_EXPR x327
                                                  ) ls328)), ("defaults", PrettyRep.List (List.map (fn x332 => 
                                                                                                          cvtEXPR x332
                                                                                                   ) ls333)), 
          ("ctorInits", 
       (case opt344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x337, ls339) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x337, 
            PrettyRep.List (List.map (fn x338 => cvtEXPR x338
                                     ) ls339)]))
       )), ("returnType", cvtTYPE_EXPR x348), ("thisType", 
       (case opt350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x349 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x349))
       )), ("hasRest", PrettyRep.Bool b354)]))
   and cvtBINDING (Binding{ident=x374, ty=x375}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x374), ("ty", cvtTYPE_EXPR x375)]))
   and cvtBINDING_IDENT (TempIdent n383) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n383))
     | cvtBINDING_IDENT (ParamIdent n386) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n386))
     | cvtBINDING_IDENT (PropIdent x389) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x389))
   and cvtINIT_STEP (InitStep(x392, x393)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x392, 
          cvtEXPR x393]))
     | cvtINIT_STEP (AssignStep(x397, x398)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x397, cvtEXPR x398]))
   and cvtTYPE_EXPR (SpecialType x402) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x402))
     | cvtTYPE_EXPR (UnionType ls406) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x405 => 
                                                                                                           cvtTYPE_EXPR x405
                                                                                                    ) ls406)))
     | cvtTYPE_EXPR (ArrayType ls413) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x412 => 
                                                                                                           cvtTYPE_EXPR x412
                                                                                                    ) ls413)))
     | cvtTYPE_EXPR (TypeName x419) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x419))
     | cvtTYPE_EXPR (ElementTypeRef(x422, n423)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x422, PrettyRep.Int n423]))
     | cvtTYPE_EXPR (FieldTypeRef(x427, x428)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x427, cvtIDENT x428]))
     | cvtTYPE_EXPR (FunctionType x432) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x432))
     | cvtTYPE_EXPR (ObjectType ls436) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x435 => 
                                                                                                             cvtFIELD_TYPE x435
                                                                                                      ) ls436)))
     | cvtTYPE_EXPR (LikeType x442) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x442))
     | cvtTYPE_EXPR (WrapType x445) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x445))
     | cvtTYPE_EXPR (AppType{base=x448, args=ls450}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x448), ("args", PrettyRep.List (List.map (fn x449 => 
                                                                                                     cvtTYPE_EXPR x449
                                                                                              ) ls450))]))
     | cvtTYPE_EXPR (LamType{params=ls462, body=x466}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x461 => 
                                                                          cvtIDENT x461
                                                                   ) ls462)), 
          ("body", cvtTYPE_EXPR x466)]))
     | cvtTYPE_EXPR (NullableType{expr=x474, nullable=b475}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x474), ("nullable", PrettyRep.Bool b475)]))
     | cvtTYPE_EXPR (InstanceType x483) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x483))
     | cvtTYPE_EXPR (TypeVarFixtureRef x486) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtTYPEVAR_NONCE x486))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x490) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x490))
     | cvtSTMT (InitStmt{kind=x493, ns=opt495, prototype=b499, static=b500, 
          temps=x501, inits=ls503}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x493), ("ns", 
       (case opt495 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x494 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x494))
       )), ("prototype", PrettyRep.Bool b499), ("static", PrettyRep.Bool b500), 
          ("temps", cvtBINDINGS x501), ("inits", PrettyRep.List (List.map (fn x502 => 
                                                                                 cvtINIT_STEP x502
                                                                          ) ls503))]))
     | cvtSTMT (ClassBlock x522) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x522))
     | cvtSTMT (ForInStmt x525) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x525))
     | cvtSTMT (ThrowStmt x528) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x528))
     | cvtSTMT (ReturnStmt x531) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x531))
     | cvtSTMT (BreakStmt opt535) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x534 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x534))
       ))
     | cvtSTMT (ContinueStmt opt542) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x541 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x541))
       ))
     | cvtSTMT (BlockStmt x548) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x548))
     | cvtSTMT (LabeledStmt(x551, x552)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x551, 
          cvtSTMT x552]))
     | cvtSTMT (LetStmt x556) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x556))
     | cvtSTMT (WhileStmt x559) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x559))
     | cvtSTMT (DoWhileStmt x562) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x562))
     | cvtSTMT (ForStmt x565) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x565))
     | cvtSTMT (IfStmt{cnd=x568, thn=x569, els=x570}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x568), ("thn", cvtSTMT x569), 
          ("els", cvtSTMT x570)]))
     | cvtSTMT (WithStmt{obj=x580, ty=x581, body=x582}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x580), ("ty", cvtTYPE_EXPR x581), 
          ("body", cvtSTMT x582)]))
     | cvtSTMT (TryStmt{block=x592, catches=ls594, finally=opt599}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x592), ("catches", PrettyRep.List (List.map (fn x593 => 
                                                                                                     cvtCATCH_CLAUSE x593
                                                                                              ) ls594)), 
          ("finally", 
       (case opt599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x598 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x598))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x612, labels=ls614, cases=ls619}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x612), ("labels", PrettyRep.List (List.map (fn x613 => 
                                                                                                  cvtIDENT x613
                                                                                           ) ls614)), 
          ("cases", PrettyRep.List (List.map (fn x618 => cvtCASE x618
                                             ) ls619))]))
     | cvtSTMT (SwitchTypeStmt{cond=x632, ty=x633, cases=ls635}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x632), ("ty", cvtTYPE_EXPR x633), 
          ("cases", PrettyRep.List (List.map (fn x634 => cvtCATCH_CLAUSE x634
                                             ) ls635))]))
     | cvtSTMT (DXNStmt{expr=x648}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x648)]))
   and cvtEXPR (TernaryExpr(x654, x655, x656)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x654, cvtEXPR x655, cvtEXPR x656]))
     | cvtEXPR (BinaryExpr(x660, x661, x662)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x660, cvtEXPR x661, cvtEXPR x662]))
     | cvtEXPR (BinaryTypeExpr(x666, x667, x668)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x666, cvtEXPR x667, cvtTYPE_EXPR x668]))
     | cvtEXPR (UnaryExpr(x672, x673)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x672, 
          cvtEXPR x673]))
     | cvtEXPR (TypeExpr x677) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x677))
     | cvtEXPR (ThisExpr opt681) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt681 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x680 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x680))
       ))
     | cvtEXPR (YieldExpr opt688) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x687 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x687))
       ))
     | cvtEXPR (SuperExpr opt695) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt695 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x694 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x694))
       ))
     | cvtEXPR (LiteralExpr x701) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x701))
     | cvtEXPR (CallExpr{func=x704, actuals=ls706}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x704), ("actuals", PrettyRep.List (List.map (fn x705 => 
                                                                                                   cvtEXPR x705
                                                                                            ) ls706))]))
     | cvtEXPR (ApplyTypeExpr{expr=x717, actuals=ls719}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x717), ("actuals", PrettyRep.List (List.map (fn x718 => 
                                                                                                   cvtTYPE_EXPR x718
                                                                                            ) ls719))]))
     | cvtEXPR (LetExpr{defs=x730, body=x731, head=opt733}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x730), ("body", cvtEXPR x731), 
          ("head", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x732))
       ))]))
     | cvtEXPR (NewExpr{obj=x746, actuals=ls748}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x746), ("actuals", PrettyRep.List (List.map (fn x747 => 
                                                                                                  cvtEXPR x747
                                                                                           ) ls748))]))
     | cvtEXPR (ObjectRef{base=x759, ident=x760, loc=opt762}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x759), ("ident", cvtIDENT_EXPR x760), 
          ("loc", 
       (case opt762 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x761 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x761))
       ))]))
     | cvtEXPR (LexicalRef{ident=x775, loc=opt777}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x775), ("loc", 
       (case opt777 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x776 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x776))
       ))]))
     | cvtEXPR (SetExpr(x788, x789, x790)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x788, 
          cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (ListExpr ls795) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x794 => 
                                                                                                    cvtEXPR x794
                                                                                             ) ls795)))
     | cvtEXPR (InitExpr(x801, x802, x803)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x801, 
          cvtHEAD x802, cvtINITS x803]))
     | cvtEXPR (GetTemp n807) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n807))
     | cvtEXPR (GetParam n810) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n810))
     | cvtEXPR (Comprehension(x813, ls815, opt820)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x813, PrettyRep.List (List.map (fn x814 => 
                                                                               cvtFOR_ENUM_HEAD x814
                                                                        ) ls815), 
          
       (case opt820 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x819 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x819))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n832) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n832))
     | cvtFIXTURE_NAME (PropName x835) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x835))
   and cvtIDENT_EXPR (Identifier{ident=x838, openNamespaces=ls844}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x838), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls840 => PrettyRep.List (List.map (fn x839 => 
                                                                                cvtNAMESPACE x839
                                                                         ) ls840)
                                   ) ls844))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x855, expr=x856}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x855), ("expr", cvtEXPR x856)]))
     | cvtIDENT_EXPR (AttributeIdentifier x864) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x864))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x867, openNamespaces=ls873}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x867), ("openNamespaces", PrettyRep.List (List.map (fn ls869 => 
                                                                            PrettyRep.List (List.map (fn x868 => 
                                                                                                            cvtNAMESPACE x868
                                                                                                     ) ls869)
                                                                     ) ls873))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x884, ident=s885}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x884), ("ident", PrettyRep.UniStr s885)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls894, x898)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x893 => cvtIDENT x893
                                                          ) ls894), cvtIDENT_EXPR x898]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r905) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r905))
     | cvtLITERAL (LiteralDecimal d908) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d908))
     | cvtLITERAL (LiteralInt i911) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i911))
     | cvtLITERAL (LiteralUInt u914) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u914))
     | cvtLITERAL (LiteralBoolean b917) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b917))
     | cvtLITERAL (LiteralString s920) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s920))
     | cvtLITERAL (LiteralArray{exprs=x923, ty=opt925}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x923), ("ty", 
       (case opt925 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x924 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x924))
       ))]))
     | cvtLITERAL (LiteralXML ls937) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x936 => 
                                                                                                           cvtEXPR x936
                                                                                                    ) ls937)))
     | cvtLITERAL (LiteralNamespace x943) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x943))
     | cvtLITERAL (LiteralObject{expr=ls947, ty=opt952}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x946 => 
                                                                        cvtFIELD x946
                                                                 ) ls947)), 
          ("ty", 
       (case opt952 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x951 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x951))
       ))]))
     | cvtLITERAL (LiteralFunction x963) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x963))
     | cvtLITERAL (LiteralRegExp{str=s966}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s966)]))
   and cvtBLOCK (Block x972) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x972))
   and cvtFIXTURE (NamespaceFixture x975) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x975))
     | cvtFIXTURE (ClassFixture x978) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x978))
     | cvtFIXTURE (InterfaceFixture x981) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x981))
     | cvtFIXTURE (TypeVarFixture x984) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x984))
     | cvtFIXTURE (TypeFixture x987) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x987))
     | cvtFIXTURE (MethodFixture{func=x990, ty=x991, readOnly=b992, override=b993, 
          final=b994}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x990), ("ty", cvtTYPE_EXPR x991), ("readOnly", PrettyRep.Bool b992), 
          ("override", PrettyRep.Bool b993), ("final", PrettyRep.Bool b994)]))
     | cvtFIXTURE (ValFixture{ty=x1008, readOnly=b1009}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1008), ("readOnly", PrettyRep.Bool b1009)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1017, getter=opt1019, setter=opt1024}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1017), ("getter", 
       (case opt1019 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1018 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1018))
       )), ("setter", 
       (case opt1024 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1023 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1023))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1037, baseTypeArgs=ls1039}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1037), ("baseTypeArgs", PrettyRep.List (List.map (fn x1038 => 
                                                                           cvtTYPE_EXPR x1038
                                                                    ) ls1039))]))
   and cvtHEAD (Head(x1050, x1051)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1050, 
          cvtINITS x1051]))
   and cvtBINDINGS (ls1056, ls1061) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1055 => 
                                                                                       cvtBINDING x1055
                                                                                ) ls1056), 
          PrettyRep.List (List.map (fn x1060 => cvtINIT_STEP x1060
                                   ) ls1061)]
   and cvtRIB ls1069 = PrettyRep.List (List.map (fn (x1066, x1067) => PrettyRep.Tuple [cvtFIXTURE_NAME x1066, 
                                                       cvtFIXTURE x1067]
                                                ) ls1069)
   and cvtRIBS ls1074 = PrettyRep.List (List.map (fn x1073 => cvtRIB x1073
                                                 ) ls1074)
   and cvtINITS ls1081 = PrettyRep.List (List.map (fn (x1078, x1079) => PrettyRep.Tuple [cvtFIXTURE_NAME x1078, 
                                                         cvtEXPR x1079]
                                                  ) ls1081)
   and cvtINSTANCE_TYPE {name=x1085, typeParams=ls1087, typeArgs=ls1092, nonnullable=b1096, 
          superTypes=ls1098, ty=x1102, dynamic=b1103} = PrettyRep.Rec [("name", 
          cvtNAME x1085), ("typeParams", PrettyRep.List (List.map (fn x1086 => 
                                                                         cvtIDENT x1086
                                                                  ) ls1087)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1091 => cvtTYPE_EXPR x1091
                                                ) ls1092)), ("nonnullable", 
          PrettyRep.Bool b1096), ("superTypes", PrettyRep.List (List.map (fn x1097 => 
                                                                                cvtTYPE_EXPR x1097
                                                                         ) ls1098)), 
          ("ty", cvtTYPE_EXPR x1102), ("dynamic", PrettyRep.Bool b1103)]
   and cvtFIELD {kind=x1119, name=x1120, init=x1121} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1119), ("name", cvtIDENT_EXPR x1120), ("init", cvtEXPR x1121)]
   and cvtFIELD_TYPE {name=x1129, ty=x1130} = PrettyRep.Rec [("name", cvtIDENT x1129), 
          ("ty", cvtTYPE_EXPR x1130)]
   and cvtFUNC_TYPE {params=ls1137, result=x1141, thisType=opt1143, hasRest=b1147, 
          minArgs=n1148} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1136 => 
                                                                                     cvtTYPE_EXPR x1136
                                                                              ) ls1137)), 
          ("result", cvtTYPE_EXPR x1141), ("thisType", 
       (case opt1143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1142 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1142))
       )), ("hasRest", PrettyRep.Bool b1147), ("minArgs", PrettyRep.Int n1148)]
   and cvtFUNC_DEFN {kind=x1160, ns=opt1162, final=b1166, override=b1167, prototype=b1168, 
          static=b1169, func=x1170} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1160), 
          ("ns", 
       (case opt1162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1161 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1161))
       )), ("final", PrettyRep.Bool b1166), ("override", PrettyRep.Bool b1167), 
          ("prototype", PrettyRep.Bool b1168), ("static", PrettyRep.Bool b1169), 
          ("func", cvtFUNC x1170)]
   and cvtCTOR_DEFN x1186 = cvtCTOR x1186
   and cvtVAR_DEFN {kind=x1187, ns=opt1189, static=b1193, prototype=b1194, 
          bindings=(ls1196, ls1201)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1187), 
          ("ns", 
       (case opt1189 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1188 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1188))
       )), ("static", PrettyRep.Bool b1193), ("prototype", PrettyRep.Bool b1194), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1195 => 
                                                                        cvtBINDING x1195
                                                                 ) ls1196), 
          PrettyRep.List (List.map (fn x1200 => cvtINIT_STEP x1200
                                   ) ls1201)])]
   and cvtNAMESPACE_DEFN {ident=x1217, ns=opt1219, init=opt1224} = PrettyRep.Rec [("ident", 
          cvtIDENT x1217), ("ns", 
       (case opt1219 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1218 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1218))
       )), ("init", 
       (case opt1224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1223 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1223))
       ))]
   and cvtCLASS_DEFN {ns=opt1236, ident=x1240, nonnullable=b1241, dynamic=b1242, 
          final=b1243, params=ls1245, extends=opt1250, implements=ls1255, classDefns=ls1260, 
          instanceDefns=ls1265, instanceStmts=ls1270, ctorDefn=opt1275} = PrettyRep.Rec [("ns", 
          
       (case opt1236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1235 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1235))
       )), ("ident", cvtIDENT x1240), ("nonnullable", PrettyRep.Bool b1241), 
          ("dynamic", PrettyRep.Bool b1242), ("final", PrettyRep.Bool b1243), 
          ("params", PrettyRep.List (List.map (fn x1244 => cvtIDENT x1244
                                              ) ls1245)), ("extends", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1249))
       )), ("implements", PrettyRep.List (List.map (fn x1254 => cvtTYPE_EXPR x1254
                                                   ) ls1255)), ("classDefns", 
          PrettyRep.List (List.map (fn x1259 => cvtDEFN x1259
                                   ) ls1260)), ("instanceDefns", PrettyRep.List (List.map (fn x1264 => 
                                                                                                 cvtDEFN x1264
                                                                                          ) ls1265)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1269 => cvtSTMT x1269
                                                     ) ls1270)), ("ctorDefn", 
          
       (case opt1275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1274 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1274))
       ))]
   and cvtINTERFACE_DEFN {ident=x1304, ns=opt1306, nonnullable=b1310, params=ls1312, 
          extends=ls1317, instanceDefns=ls1322} = PrettyRep.Rec [("ident", 
          cvtIDENT x1304), ("ns", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1305))
       )), ("nonnullable", PrettyRep.Bool b1310), ("params", PrettyRep.List (List.map (fn x1311 => 
                                                                                             cvtIDENT x1311
                                                                                      ) ls1312)), 
          ("extends", PrettyRep.List (List.map (fn x1316 => cvtTYPE_EXPR x1316
                                               ) ls1317)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1321 => cvtDEFN x1321
                                   ) ls1322))]
   and cvtTYPE_DEFN {ident=x1339, ns=opt1341, init=x1345} = PrettyRep.Rec [("ident", 
          cvtIDENT x1339), ("ns", 
       (case opt1341 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1340 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1340))
       )), ("init", cvtTYPE_EXPR x1345)]
   and cvtCLASS_BLOCK {ns=opt1354, ident=x1358, name=opt1360, block=x1364} = 
          PrettyRep.Rec [("ns", 
       (case opt1354 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1353 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1353))
       )), ("ident", cvtIDENT x1358), ("name", 
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1359))
       )), ("block", cvtBLOCK x1364)]
   and cvtFOR_ENUM_HEAD {isEach=b1374, bindings=(ls1376, ls1381), expr=x1386} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1374), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1375 => 
                                                                                                                         cvtBINDING x1375
                                                                                                                  ) ls1376), 
          PrettyRep.List (List.map (fn x1380 => cvtINIT_STEP x1380
                                   ) ls1381)]), ("expr", cvtEXPR x1386)]
   and cvtFOR_ENUM_STMT {isEach=b1394, defn=opt1425, obj=x1429, rib=opt1437, 
          next=x1441, labels=ls1443, body=x1447} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1394), ("defn", 
       (case opt1425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1395, ns=opt1397, static=b1401, prototype=b1402, bindings=(ls1404, 
            ls1409)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1395), ("ns", 
         (case opt1397 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1396))
         )), ("static", PrettyRep.Bool b1401), ("prototype", PrettyRep.Bool b1402), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1403 => 
                                                                          cvtBINDING x1403
                                                                   ) ls1404), 
            PrettyRep.List (List.map (fn x1408 => cvtINIT_STEP x1408
                                     ) ls1409)])]))
       )), ("obj", cvtEXPR x1429), ("rib", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1433 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1430, 
                                                                                      x1431) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1430, 
                                                                                      cvtFIXTURE x1431]
                                                                               ) ls1433)))
       )), ("next", cvtSTMT x1441), ("labels", PrettyRep.List (List.map (fn x1442 => 
                                                                               cvtIDENT x1442
                                                                        ) ls1443)), 
          ("body", cvtSTMT x1447)]
   and cvtFOR_STMT {rib=opt1470, defn=opt1504, init=ls1509, cond=x1513, update=x1514, 
          labels=ls1516, body=x1520} = PrettyRep.Rec [("rib", 
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1466 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1463, 
                                                                                      x1464) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1463, 
                                                                                      cvtFIXTURE x1464]
                                                                               ) ls1466)))
       )), ("defn", 
       (case opt1504 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1474, ns=opt1476, static=b1480, prototype=b1481, bindings=(ls1483, 
            ls1488)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1474), ("ns", 
         (case opt1476 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1475 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1475))
         )), ("static", PrettyRep.Bool b1480), ("prototype", PrettyRep.Bool b1481), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1482 => 
                                                                          cvtBINDING x1482
                                                                   ) ls1483), 
            PrettyRep.List (List.map (fn x1487 => cvtINIT_STEP x1487
                                     ) ls1488)])]))
       )), ("init", PrettyRep.List (List.map (fn x1508 => cvtSTMT x1508
                                             ) ls1509)), ("cond", cvtEXPR x1513), 
          ("update", cvtEXPR x1514), ("labels", PrettyRep.List (List.map (fn x1515 => 
                                                                                cvtIDENT x1515
                                                                         ) ls1516)), 
          ("body", cvtSTMT x1520)]
   and cvtWHILE_STMT {cond=x1536, rib=opt1544, body=x1548, labels=ls1550} = 
          PrettyRep.Rec [("cond", cvtEXPR x1536), ("rib", 
       (case opt1544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1540 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1537, 
                                                                                      x1538) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1537, 
                                                                                      cvtFIXTURE x1538]
                                                                               ) ls1540)))
       )), ("body", cvtSTMT x1548), ("labels", PrettyRep.List (List.map (fn x1549 => 
                                                                               cvtIDENT x1549
                                                                        ) ls1550))]
   and cvtDIRECTIVES {pragmas=ls1564, defns=ls1569, head=opt1574, body=ls1579, 
          loc=opt1584} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1563 => 
                                                                                    cvtPRAGMA x1563
                                                                             ) ls1564)), 
          ("defns", PrettyRep.List (List.map (fn x1568 => cvtDEFN x1568
                                             ) ls1569)), ("head", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1573))
       )), ("body", PrettyRep.List (List.map (fn x1578 => cvtSTMT x1578
                                             ) ls1579)), ("loc", 
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1583 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1583))
       ))]
   and cvtCASE {label=opt1600, inits=opt1611, body=x1615} = PrettyRep.Rec [("label", 
          
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1599 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1599))
       )), ("inits", 
       (case opt1611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1607 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1604, 
                                                                                      x1605) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1604, 
                                                                                      cvtEXPR x1605]
                                                                               ) ls1607)))
       )), ("body", cvtBLOCK x1615)]
   and cvtCATCH_CLAUSE {bindings=(ls1624, ls1629), ty=x1634, rib=opt1642, inits=opt1653, 
          block=x1657} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1623 => 
                                                                                                      cvtBINDING x1623
                                                                                               ) ls1624), 
          PrettyRep.List (List.map (fn x1628 => cvtINIT_STEP x1628
                                   ) ls1629)]), ("ty", cvtTYPE_EXPR x1634), 
          ("rib", 
       (case opt1642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1638 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1635, 
                                                                                      x1636) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1635, 
                                                                                      cvtFIXTURE x1636]
                                                                               ) ls1638)))
       )), ("inits", 
       (case opt1653 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1649 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1646, 
                                                                                      x1647) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1646, 
                                                                                      cvtEXPR x1647]
                                                                               ) ls1649)))
       )), ("block", cvtBLOCK x1657)]
   and cvtFUNC_NAME {kind=x1669, ident=x1670} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1669), 
          ("ident", cvtIDENT x1670)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1676, getter=opt1678, setter=opt1683} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1676), ("getter", 
       (case opt1678 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1677 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1677))
       )), ("setter", 
       (case opt1683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1682 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1682))
       ))]
   and cvtFRAGMENT (Package{name=ls1695, fragments=ls1700}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1694 => 
                                                                        cvtIDENT x1694
                                                                 ) ls1695)), 
          ("fragments", PrettyRep.List (List.map (fn x1699 => cvtFRAGMENT x1699
                                                 ) ls1700))]))
     | cvtFRAGMENT (Anon x1711) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1711))
end

