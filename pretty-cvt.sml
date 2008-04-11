structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtAST_NONCE n20 = PrettyRep.Int n20
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
     | cvtTYPE_EXPR (TypeName(x419, opt421)) = PrettyRep.Ctor ("TypeName", 
          SOME (PrettyRep.Tuple [cvtIDENT_EXPR x419, 
       (case opt421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x420 => PrettyRep.Ctor ("SOME", SOME (cvtAST_NONCE x420))
       )]))
     | cvtTYPE_EXPR (ElementTypeRef(x428, n429)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x428, PrettyRep.Int n429]))
     | cvtTYPE_EXPR (FieldTypeRef(x433, x434)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x433, cvtIDENT x434]))
     | cvtTYPE_EXPR (FunctionType x438) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x438))
     | cvtTYPE_EXPR (ObjectType ls442) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x441 => 
                                                                                                             cvtFIELD_TYPE x441
                                                                                                      ) ls442)))
     | cvtTYPE_EXPR (LikeType x448) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x448))
     | cvtTYPE_EXPR (WrapType x451) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x451))
     | cvtTYPE_EXPR (AppType{base=x454, args=ls456}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x454), ("args", PrettyRep.List (List.map (fn x455 => 
                                                                                                     cvtTYPE_EXPR x455
                                                                                              ) ls456))]))
     | cvtTYPE_EXPR (LamType{params=ls468, body=x472}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x467 => 
                                                                          cvtIDENT x467
                                                                   ) ls468)), 
          ("body", cvtTYPE_EXPR x472)]))
     | cvtTYPE_EXPR (NullableType{expr=x480, nullable=b481}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x480), ("nullable", PrettyRep.Bool b481)]))
     | cvtTYPE_EXPR (InstanceType x489) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x489))
     | cvtTYPE_EXPR (TypeVarFixtureRef x492) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtTYPEVAR_NONCE x492))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x496) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x496))
     | cvtSTMT (InitStmt{kind=x499, ns=opt501, prototype=b505, static=b506, 
          temps=x507, inits=ls509}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x499), ("ns", 
       (case opt501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x500 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x500))
       )), ("prototype", PrettyRep.Bool b505), ("static", PrettyRep.Bool b506), 
          ("temps", cvtBINDINGS x507), ("inits", PrettyRep.List (List.map (fn x508 => 
                                                                                 cvtINIT_STEP x508
                                                                          ) ls509))]))
     | cvtSTMT (ClassBlock x528) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x528))
     | cvtSTMT (ForInStmt x531) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x531))
     | cvtSTMT (ThrowStmt x534) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x534))
     | cvtSTMT (ReturnStmt x537) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x537))
     | cvtSTMT (BreakStmt opt541) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x540 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x540))
       ))
     | cvtSTMT (ContinueStmt opt548) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt548 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x547 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x547))
       ))
     | cvtSTMT (BlockStmt x554) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x554))
     | cvtSTMT (LabeledStmt(x557, x558)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x557, 
          cvtSTMT x558]))
     | cvtSTMT (LetStmt x562) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x562))
     | cvtSTMT (WhileStmt x565) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x565))
     | cvtSTMT (DoWhileStmt x568) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x568))
     | cvtSTMT (ForStmt x571) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x571))
     | cvtSTMT (IfStmt{cnd=x574, thn=x575, els=x576}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x574), ("thn", cvtSTMT x575), 
          ("els", cvtSTMT x576)]))
     | cvtSTMT (WithStmt{obj=x586, ty=x587, body=x588}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x586), ("ty", cvtTYPE_EXPR x587), 
          ("body", cvtSTMT x588)]))
     | cvtSTMT (TryStmt{block=x598, catches=ls600, finally=opt605}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x598), ("catches", PrettyRep.List (List.map (fn x599 => 
                                                                                                     cvtCATCH_CLAUSE x599
                                                                                              ) ls600)), 
          ("finally", 
       (case opt605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x604 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x604))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x618, labels=ls620, cases=ls625}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x618), ("labels", PrettyRep.List (List.map (fn x619 => 
                                                                                                  cvtIDENT x619
                                                                                           ) ls620)), 
          ("cases", PrettyRep.List (List.map (fn x624 => cvtCASE x624
                                             ) ls625))]))
     | cvtSTMT (SwitchTypeStmt{cond=x638, ty=x639, cases=ls641}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x638), ("ty", cvtTYPE_EXPR x639), 
          ("cases", PrettyRep.List (List.map (fn x640 => cvtCATCH_CLAUSE x640
                                             ) ls641))]))
     | cvtSTMT (DXNStmt{expr=x654}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x654)]))
   and cvtEXPR (TernaryExpr(x660, x661, x662)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x660, cvtEXPR x661, cvtEXPR x662]))
     | cvtEXPR (BinaryExpr(x666, x667, x668)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x666, cvtEXPR x667, cvtEXPR x668]))
     | cvtEXPR (BinaryTypeExpr(x672, x673, x674)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x672, cvtEXPR x673, cvtTYPE_EXPR x674]))
     | cvtEXPR (UnaryExpr(x678, x679)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x678, 
          cvtEXPR x679]))
     | cvtEXPR (TypeExpr x683) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x683))
     | cvtEXPR (ThisExpr opt687) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt687 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x686 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x686))
       ))
     | cvtEXPR (YieldExpr opt694) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x693 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x693))
       ))
     | cvtEXPR (SuperExpr opt701) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt701 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x700 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x700))
       ))
     | cvtEXPR (LiteralExpr x707) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x707))
     | cvtEXPR (CallExpr{func=x710, actuals=ls712}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x710), ("actuals", PrettyRep.List (List.map (fn x711 => 
                                                                                                   cvtEXPR x711
                                                                                            ) ls712))]))
     | cvtEXPR (ApplyTypeExpr{expr=x723, actuals=ls725}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x723), ("actuals", PrettyRep.List (List.map (fn x724 => 
                                                                                                   cvtTYPE_EXPR x724
                                                                                            ) ls725))]))
     | cvtEXPR (LetExpr{defs=x736, body=x737, head=opt739}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x736), ("body", cvtEXPR x737), 
          ("head", 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x738))
       ))]))
     | cvtEXPR (NewExpr{obj=x752, actuals=ls754}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x752), ("actuals", PrettyRep.List (List.map (fn x753 => 
                                                                                                  cvtEXPR x753
                                                                                           ) ls754))]))
     | cvtEXPR (ObjectRef{base=x765, ident=x766, loc=opt768}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x765), ("ident", cvtIDENT_EXPR x766), 
          ("loc", 
       (case opt768 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x767 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x767))
       ))]))
     | cvtEXPR (LexicalRef{ident=x781, loc=opt783}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x781), ("loc", 
       (case opt783 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x782 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x782))
       ))]))
     | cvtEXPR (SetExpr(x794, x795, x796)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x794, 
          cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (ListExpr ls801) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x800 => 
                                                                                                    cvtEXPR x800
                                                                                             ) ls801)))
     | cvtEXPR (InitExpr(x807, x808, x809)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x807, 
          cvtHEAD x808, cvtINITS x809]))
     | cvtEXPR (GetTemp n813) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n813))
     | cvtEXPR (GetParam n816) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n816))
     | cvtEXPR (Comprehension(x819, ls821, opt826)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x819, PrettyRep.List (List.map (fn x820 => 
                                                                               cvtFOR_ENUM_HEAD x820
                                                                        ) ls821), 
          
       (case opt826 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x825 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x825))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n838) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n838))
     | cvtFIXTURE_NAME (PropName x841) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x841))
   and cvtIDENT_EXPR (Identifier{ident=x844, openNamespaces=ls850}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x844), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls846 => PrettyRep.List (List.map (fn x845 => 
                                                                                cvtNAMESPACE x845
                                                                         ) ls846)
                                   ) ls850))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x861, expr=x862}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x861), ("expr", cvtEXPR x862)]))
     | cvtIDENT_EXPR (AttributeIdentifier x870) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x870))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x873, openNamespaces=ls879}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x873), ("openNamespaces", PrettyRep.List (List.map (fn ls875 => 
                                                                            PrettyRep.List (List.map (fn x874 => 
                                                                                                            cvtNAMESPACE x874
                                                                                                     ) ls875)
                                                                     ) ls879))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x890, ident=s891}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x890), ("ident", PrettyRep.UniStr s891)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls900, x904)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x899 => cvtIDENT x899
                                                          ) ls900), cvtIDENT_EXPR x904]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r911) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r911))
     | cvtLITERAL (LiteralDecimal d914) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d914))
     | cvtLITERAL (LiteralInt i917) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i917))
     | cvtLITERAL (LiteralUInt u920) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u920))
     | cvtLITERAL (LiteralBoolean b923) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b923))
     | cvtLITERAL (LiteralString s926) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s926))
     | cvtLITERAL (LiteralArray{exprs=x929, ty=opt931}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x929), ("ty", 
       (case opt931 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x930 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x930))
       ))]))
     | cvtLITERAL (LiteralXML ls943) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x942 => 
                                                                                                           cvtEXPR x942
                                                                                                    ) ls943)))
     | cvtLITERAL (LiteralNamespace x949) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x949))
     | cvtLITERAL (LiteralObject{expr=ls953, ty=opt958}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x952 => 
                                                                        cvtFIELD x952
                                                                 ) ls953)), 
          ("ty", 
       (case opt958 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x957 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x957))
       ))]))
     | cvtLITERAL (LiteralFunction x969) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x969))
     | cvtLITERAL (LiteralRegExp{str=s972}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s972)]))
   and cvtBLOCK (Block x978) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x978))
   and cvtFIXTURE (NamespaceFixture x981) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x981))
     | cvtFIXTURE (ClassFixture x984) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x984))
     | cvtFIXTURE (InterfaceFixture x987) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x987))
     | cvtFIXTURE (TypeVarFixture x990) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x990))
     | cvtFIXTURE (TypeFixture x993) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x993))
     | cvtFIXTURE (MethodFixture{func=x996, ty=x997, readOnly=b998, override=b999, 
          final=b1000}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x996), ("ty", cvtTYPE_EXPR x997), ("readOnly", PrettyRep.Bool b998), 
          ("override", PrettyRep.Bool b999), ("final", PrettyRep.Bool b1000)]))
     | cvtFIXTURE (ValFixture{ty=x1014, readOnly=b1015}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1014), ("readOnly", PrettyRep.Bool b1015)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1023, getter=opt1025, setter=opt1030}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1023), ("getter", 
       (case opt1025 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1024 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1024))
       )), ("setter", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1029))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1043, baseTypeArgs=ls1045}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1043), ("baseTypeArgs", PrettyRep.List (List.map (fn x1044 => 
                                                                           cvtTYPE_EXPR x1044
                                                                    ) ls1045))]))
   and cvtHEAD (Head(x1056, x1057)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1056, 
          cvtINITS x1057]))
   and cvtBINDINGS (ls1062, ls1067) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1061 => 
                                                                                       cvtBINDING x1061
                                                                                ) ls1062), 
          PrettyRep.List (List.map (fn x1066 => cvtINIT_STEP x1066
                                   ) ls1067)]
   and cvtRIB ls1075 = PrettyRep.List (List.map (fn (x1072, x1073) => PrettyRep.Tuple [cvtFIXTURE_NAME x1072, 
                                                       cvtFIXTURE x1073]
                                                ) ls1075)
   and cvtRIBS ls1080 = PrettyRep.List (List.map (fn x1079 => cvtRIB x1079
                                                 ) ls1080)
   and cvtINITS ls1087 = PrettyRep.List (List.map (fn (x1084, x1085) => PrettyRep.Tuple [cvtFIXTURE_NAME x1084, 
                                                         cvtEXPR x1085]
                                                  ) ls1087)
   and cvtINSTANCE_TYPE {name=x1091, typeParams=ls1093, typeArgs=ls1098, nonnullable=b1102, 
          superTypes=ls1104, ty=x1108, dynamic=b1109} = PrettyRep.Rec [("name", 
          cvtNAME x1091), ("typeParams", PrettyRep.List (List.map (fn x1092 => 
                                                                         cvtIDENT x1092
                                                                  ) ls1093)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1097 => cvtTYPE_EXPR x1097
                                                ) ls1098)), ("nonnullable", 
          PrettyRep.Bool b1102), ("superTypes", PrettyRep.List (List.map (fn x1103 => 
                                                                                cvtTYPE_EXPR x1103
                                                                         ) ls1104)), 
          ("ty", cvtTYPE_EXPR x1108), ("dynamic", PrettyRep.Bool b1109)]
   and cvtFIELD {kind=x1125, name=x1126, init=x1127} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1125), ("name", cvtIDENT_EXPR x1126), ("init", cvtEXPR x1127)]
   and cvtFIELD_TYPE {name=x1135, ty=x1136} = PrettyRep.Rec [("name", cvtIDENT x1135), 
          ("ty", cvtTYPE_EXPR x1136)]
   and cvtFUNC_TYPE {params=ls1143, result=x1147, thisType=opt1149, hasRest=b1153, 
          minArgs=n1154} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1142 => 
                                                                                     cvtTYPE_EXPR x1142
                                                                              ) ls1143)), 
          ("result", cvtTYPE_EXPR x1147), ("thisType", 
       (case opt1149 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1148 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1148))
       )), ("hasRest", PrettyRep.Bool b1153), ("minArgs", PrettyRep.Int n1154)]
   and cvtFUNC_DEFN {kind=x1166, ns=opt1168, final=b1172, override=b1173, prototype=b1174, 
          static=b1175, func=x1176} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1166), 
          ("ns", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1167))
       )), ("final", PrettyRep.Bool b1172), ("override", PrettyRep.Bool b1173), 
          ("prototype", PrettyRep.Bool b1174), ("static", PrettyRep.Bool b1175), 
          ("func", cvtFUNC x1176)]
   and cvtCTOR_DEFN x1192 = cvtCTOR x1192
   and cvtVAR_DEFN {kind=x1193, ns=opt1195, static=b1199, prototype=b1200, 
          bindings=(ls1202, ls1207)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1193), 
          ("ns", 
       (case opt1195 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1194 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1194))
       )), ("static", PrettyRep.Bool b1199), ("prototype", PrettyRep.Bool b1200), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1201 => 
                                                                        cvtBINDING x1201
                                                                 ) ls1202), 
          PrettyRep.List (List.map (fn x1206 => cvtINIT_STEP x1206
                                   ) ls1207)])]
   and cvtNAMESPACE_DEFN {ident=x1223, ns=opt1225, init=opt1230} = PrettyRep.Rec [("ident", 
          cvtIDENT x1223), ("ns", 
       (case opt1225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1224 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1224))
       )), ("init", 
       (case opt1230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1229 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1229))
       ))]
   and cvtCLASS_DEFN {ns=opt1242, ident=x1246, nonnullable=b1247, dynamic=b1248, 
          final=b1249, params=ls1251, extends=opt1256, implements=ls1261, classDefns=ls1266, 
          instanceDefns=ls1271, instanceStmts=ls1276, ctorDefn=opt1281} = PrettyRep.Rec [("ns", 
          
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1241))
       )), ("ident", cvtIDENT x1246), ("nonnullable", PrettyRep.Bool b1247), 
          ("dynamic", PrettyRep.Bool b1248), ("final", PrettyRep.Bool b1249), 
          ("params", PrettyRep.List (List.map (fn x1250 => cvtIDENT x1250
                                              ) ls1251)), ("extends", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1255))
       )), ("implements", PrettyRep.List (List.map (fn x1260 => cvtTYPE_EXPR x1260
                                                   ) ls1261)), ("classDefns", 
          PrettyRep.List (List.map (fn x1265 => cvtDEFN x1265
                                   ) ls1266)), ("instanceDefns", PrettyRep.List (List.map (fn x1270 => 
                                                                                                 cvtDEFN x1270
                                                                                          ) ls1271)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1275 => cvtSTMT x1275
                                                     ) ls1276)), ("ctorDefn", 
          
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1280))
       ))]
   and cvtINTERFACE_DEFN {ident=x1310, ns=opt1312, nonnullable=b1316, params=ls1318, 
          extends=ls1323, instanceDefns=ls1328} = PrettyRep.Rec [("ident", 
          cvtIDENT x1310), ("ns", 
       (case opt1312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1311 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1311))
       )), ("nonnullable", PrettyRep.Bool b1316), ("params", PrettyRep.List (List.map (fn x1317 => 
                                                                                             cvtIDENT x1317
                                                                                      ) ls1318)), 
          ("extends", PrettyRep.List (List.map (fn x1322 => cvtTYPE_EXPR x1322
                                               ) ls1323)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1327 => cvtDEFN x1327
                                   ) ls1328))]
   and cvtTYPE_DEFN {ident=x1345, ns=opt1347, init=x1351} = PrettyRep.Rec [("ident", 
          cvtIDENT x1345), ("ns", 
       (case opt1347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1346))
       )), ("init", cvtTYPE_EXPR x1351)]
   and cvtCLASS_BLOCK {ns=opt1360, ident=x1364, name=opt1366, block=x1370} = 
          PrettyRep.Rec [("ns", 
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1359))
       )), ("ident", cvtIDENT x1364), ("name", 
       (case opt1366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1365 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1365))
       )), ("block", cvtBLOCK x1370)]
   and cvtFOR_ENUM_HEAD {isEach=b1380, bindings=(ls1382, ls1387), expr=x1392} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1380), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1381 => 
                                                                                                                         cvtBINDING x1381
                                                                                                                  ) ls1382), 
          PrettyRep.List (List.map (fn x1386 => cvtINIT_STEP x1386
                                   ) ls1387)]), ("expr", cvtEXPR x1392)]
   and cvtFOR_ENUM_STMT {isEach=b1400, defn=opt1431, obj=x1435, rib=opt1443, 
          next=x1447, labels=ls1449, body=x1453} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1400), ("defn", 
       (case opt1431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1401, ns=opt1403, static=b1407, prototype=b1408, bindings=(ls1410, 
            ls1415)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1401), ("ns", 
         (case opt1403 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1402 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1402))
         )), ("static", PrettyRep.Bool b1407), ("prototype", PrettyRep.Bool b1408), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1409 => 
                                                                          cvtBINDING x1409
                                                                   ) ls1410), 
            PrettyRep.List (List.map (fn x1414 => cvtINIT_STEP x1414
                                     ) ls1415)])]))
       )), ("obj", cvtEXPR x1435), ("rib", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1439 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1436, 
                                                                                      x1437) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1436, 
                                                                                      cvtFIXTURE x1437]
                                                                               ) ls1439)))
       )), ("next", cvtSTMT x1447), ("labels", PrettyRep.List (List.map (fn x1448 => 
                                                                               cvtIDENT x1448
                                                                        ) ls1449)), 
          ("body", cvtSTMT x1453)]
   and cvtFOR_STMT {rib=opt1476, defn=opt1510, init=ls1515, cond=x1519, update=x1520, 
          labels=ls1522, body=x1526} = PrettyRep.Rec [("rib", 
       (case opt1476 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1472 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1469, 
                                                                                      x1470) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1469, 
                                                                                      cvtFIXTURE x1470]
                                                                               ) ls1472)))
       )), ("defn", 
       (case opt1510 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1480, ns=opt1482, static=b1486, prototype=b1487, bindings=(ls1489, 
            ls1494)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1480), ("ns", 
         (case opt1482 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1481 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1481))
         )), ("static", PrettyRep.Bool b1486), ("prototype", PrettyRep.Bool b1487), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1488 => 
                                                                          cvtBINDING x1488
                                                                   ) ls1489), 
            PrettyRep.List (List.map (fn x1493 => cvtINIT_STEP x1493
                                     ) ls1494)])]))
       )), ("init", PrettyRep.List (List.map (fn x1514 => cvtSTMT x1514
                                             ) ls1515)), ("cond", cvtEXPR x1519), 
          ("update", cvtEXPR x1520), ("labels", PrettyRep.List (List.map (fn x1521 => 
                                                                                cvtIDENT x1521
                                                                         ) ls1522)), 
          ("body", cvtSTMT x1526)]
   and cvtWHILE_STMT {cond=x1542, rib=opt1550, body=x1554, labels=ls1556} = 
          PrettyRep.Rec [("cond", cvtEXPR x1542), ("rib", 
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1546 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1543, 
                                                                                      x1544) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1543, 
                                                                                      cvtFIXTURE x1544]
                                                                               ) ls1546)))
       )), ("body", cvtSTMT x1554), ("labels", PrettyRep.List (List.map (fn x1555 => 
                                                                               cvtIDENT x1555
                                                                        ) ls1556))]
   and cvtDIRECTIVES {pragmas=ls1570, defns=ls1575, head=opt1580, body=ls1585, 
          loc=opt1590} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1569 => 
                                                                                    cvtPRAGMA x1569
                                                                             ) ls1570)), 
          ("defns", PrettyRep.List (List.map (fn x1574 => cvtDEFN x1574
                                             ) ls1575)), ("head", 
       (case opt1580 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1579 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1579))
       )), ("body", PrettyRep.List (List.map (fn x1584 => cvtSTMT x1584
                                             ) ls1585)), ("loc", 
       (case opt1590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1589 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1589))
       ))]
   and cvtCASE {label=opt1606, inits=opt1617, body=x1621} = PrettyRep.Rec [("label", 
          
       (case opt1606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1605 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1605))
       )), ("inits", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1613 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1610, 
                                                                                      x1611) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1610, 
                                                                                      cvtEXPR x1611]
                                                                               ) ls1613)))
       )), ("body", cvtBLOCK x1621)]
   and cvtCATCH_CLAUSE {bindings=(ls1630, ls1635), ty=x1640, rib=opt1648, inits=opt1659, 
          block=x1663} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1629 => 
                                                                                                      cvtBINDING x1629
                                                                                               ) ls1630), 
          PrettyRep.List (List.map (fn x1634 => cvtINIT_STEP x1634
                                   ) ls1635)]), ("ty", cvtTYPE_EXPR x1640), 
          ("rib", 
       (case opt1648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1644 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1641, 
                                                                                      x1642) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1641, 
                                                                                      cvtFIXTURE x1642]
                                                                               ) ls1644)))
       )), ("inits", 
       (case opt1659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1655 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1652, 
                                                                                      x1653) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1652, 
                                                                                      cvtEXPR x1653]
                                                                               ) ls1655)))
       )), ("block", cvtBLOCK x1663)]
   and cvtFUNC_NAME {kind=x1675, ident=x1676} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1675), 
          ("ident", cvtIDENT x1676)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1682, getter=opt1684, setter=opt1689} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1682), ("getter", 
       (case opt1684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1683 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1683))
       )), ("setter", 
       (case opt1689 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1688 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1688))
       ))]
   and cvtFRAGMENT (Package{name=ls1701, fragments=ls1706}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1700 => 
                                                                        cvtIDENT x1700
                                                                 ) ls1701)), 
          ("fragments", PrettyRep.List (List.map (fn x1705 => cvtFRAGMENT x1705
                                                 ) ls1706))]))
     | cvtFRAGMENT (Anon x1717) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1717))
end

