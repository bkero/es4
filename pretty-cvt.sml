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
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x493) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x493))
     | cvtSTMT (InitStmt{kind=x496, ns=opt498, prototype=b502, static=b503, 
          temps=x504, inits=ls506}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x496), ("ns", 
       (case opt498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x497 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x497))
       )), ("prototype", PrettyRep.Bool b502), ("static", PrettyRep.Bool b503), 
          ("temps", cvtBINDINGS x504), ("inits", PrettyRep.List (List.map (fn x505 => 
                                                                                 cvtINIT_STEP x505
                                                                          ) ls506))]))
     | cvtSTMT (ClassBlock x525) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x525))
     | cvtSTMT (ForInStmt x528) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x528))
     | cvtSTMT (ThrowStmt x531) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x531))
     | cvtSTMT (ReturnStmt x534) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x534))
     | cvtSTMT (BreakStmt opt538) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x537 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x537))
       ))
     | cvtSTMT (ContinueStmt opt545) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x544 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x544))
       ))
     | cvtSTMT (BlockStmt x551) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x551))
     | cvtSTMT (LabeledStmt(x554, x555)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x554, 
          cvtSTMT x555]))
     | cvtSTMT (LetStmt x559) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x559))
     | cvtSTMT (WhileStmt x562) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x562))
     | cvtSTMT (DoWhileStmt x565) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x565))
     | cvtSTMT (ForStmt x568) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x568))
     | cvtSTMT (IfStmt{cnd=x571, thn=x572, els=x573}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x571), ("thn", cvtSTMT x572), 
          ("els", cvtSTMT x573)]))
     | cvtSTMT (WithStmt{obj=x583, ty=x584, body=x585}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x583), ("ty", cvtTYPE_EXPR x584), 
          ("body", cvtSTMT x585)]))
     | cvtSTMT (TryStmt{block=x595, catches=ls597, finally=opt602}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x595), ("catches", PrettyRep.List (List.map (fn x596 => 
                                                                                                     cvtCATCH_CLAUSE x596
                                                                                              ) ls597)), 
          ("finally", 
       (case opt602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x601))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x615, labels=ls617, cases=ls622}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x615), ("labels", PrettyRep.List (List.map (fn x616 => 
                                                                                                  cvtIDENT x616
                                                                                           ) ls617)), 
          ("cases", PrettyRep.List (List.map (fn x621 => cvtCASE x621
                                             ) ls622))]))
     | cvtSTMT (SwitchTypeStmt{cond=x635, ty=x636, cases=ls638}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x635), ("ty", cvtTYPE_EXPR x636), 
          ("cases", PrettyRep.List (List.map (fn x637 => cvtCATCH_CLAUSE x637
                                             ) ls638))]))
     | cvtSTMT (DXNStmt{expr=x651}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x651)]))
   and cvtEXPR (TernaryExpr(x657, x658, x659)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x657, cvtEXPR x658, cvtEXPR x659]))
     | cvtEXPR (BinaryExpr(x663, x664, x665)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x663, cvtEXPR x664, cvtEXPR x665]))
     | cvtEXPR (BinaryTypeExpr(x669, x670, x671)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x669, cvtEXPR x670, cvtTYPE_EXPR x671]))
     | cvtEXPR (UnaryExpr(x675, x676)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x675, 
          cvtEXPR x676]))
     | cvtEXPR (TypeExpr x680) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x680))
     | cvtEXPR (ThisExpr opt684) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x683 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x683))
       ))
     | cvtEXPR (YieldExpr opt691) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt691 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x690 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x690))
       ))
     | cvtEXPR (SuperExpr opt698) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x697 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x697))
       ))
     | cvtEXPR (LiteralExpr x704) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x704))
     | cvtEXPR (CallExpr{func=x707, actuals=ls709}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x707), ("actuals", PrettyRep.List (List.map (fn x708 => 
                                                                                                   cvtEXPR x708
                                                                                            ) ls709))]))
     | cvtEXPR (ApplyTypeExpr{expr=x720, actuals=ls722}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x720), ("actuals", PrettyRep.List (List.map (fn x721 => 
                                                                                                   cvtTYPE_EXPR x721
                                                                                            ) ls722))]))
     | cvtEXPR (LetExpr{defs=x733, body=x734, head=opt736}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x733), ("body", cvtEXPR x734), 
          ("head", 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x735))
       ))]))
     | cvtEXPR (NewExpr{obj=x749, actuals=ls751}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x749), ("actuals", PrettyRep.List (List.map (fn x750 => 
                                                                                                  cvtEXPR x750
                                                                                           ) ls751))]))
     | cvtEXPR (ObjectRef{base=x762, ident=x763, loc=opt765}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x762), ("ident", cvtIDENT_EXPR x763), 
          ("loc", 
       (case opt765 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x764 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x764))
       ))]))
     | cvtEXPR (LexicalRef{ident=x778, loc=opt780}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x778), ("loc", 
       (case opt780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x779 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x779))
       ))]))
     | cvtEXPR (SetExpr(x791, x792, x793)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x791, 
          cvtEXPR x792, cvtEXPR x793]))
     | cvtEXPR (ListExpr ls798) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x797 => 
                                                                                                    cvtEXPR x797
                                                                                             ) ls798)))
     | cvtEXPR (InitExpr(x804, x805, x806)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x804, 
          cvtHEAD x805, cvtINITS x806]))
     | cvtEXPR (GetTemp n810) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n810))
     | cvtEXPR (GetParam n813) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n813))
     | cvtEXPR (Comprehension(x816, ls818, opt823)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x816, PrettyRep.List (List.map (fn x817 => 
                                                                               cvtFOR_ENUM_HEAD x817
                                                                        ) ls818), 
          
       (case opt823 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x822 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x822))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n835) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n835))
     | cvtFIXTURE_NAME (PropName x838) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x838))
   and cvtIDENT_EXPR (Identifier{ident=x841, openNamespaces=ls847}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x841), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls843 => PrettyRep.List (List.map (fn x842 => 
                                                                                cvtNAMESPACE x842
                                                                         ) ls843)
                                   ) ls847))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x858, expr=x859}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x858), ("expr", cvtEXPR x859)]))
     | cvtIDENT_EXPR (AttributeIdentifier x867) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x867))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x870, openNamespaces=ls876}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x870), ("openNamespaces", PrettyRep.List (List.map (fn ls872 => 
                                                                            PrettyRep.List (List.map (fn x871 => 
                                                                                                            cvtNAMESPACE x871
                                                                                                     ) ls872)
                                                                     ) ls876))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x887, ident=s888}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x887), ("ident", PrettyRep.UniStr s888)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls897, x901)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x896 => cvtIDENT x896
                                                          ) ls897), cvtIDENT_EXPR x901]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r908) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r908))
     | cvtLITERAL (LiteralDecimal d911) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d911))
     | cvtLITERAL (LiteralInt i914) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i914))
     | cvtLITERAL (LiteralUInt u917) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u917))
     | cvtLITERAL (LiteralBoolean b920) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b920))
     | cvtLITERAL (LiteralString s923) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s923))
     | cvtLITERAL (LiteralArray{exprs=x926, ty=opt928}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x926), ("ty", 
       (case opt928 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x927 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x927))
       ))]))
     | cvtLITERAL (LiteralXML ls940) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x939 => 
                                                                                                           cvtEXPR x939
                                                                                                    ) ls940)))
     | cvtLITERAL (LiteralNamespace x946) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x946))
     | cvtLITERAL (LiteralObject{expr=ls950, ty=opt955}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x949 => 
                                                                        cvtFIELD x949
                                                                 ) ls950)), 
          ("ty", 
       (case opt955 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x954 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x954))
       ))]))
     | cvtLITERAL (LiteralFunction x966) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x966))
     | cvtLITERAL (LiteralRegExp{str=s969}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s969)]))
   and cvtBLOCK (Block x975) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x975))
   and cvtFIXTURE (NamespaceFixture x978) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x978))
     | cvtFIXTURE (ClassFixture x981) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x981))
     | cvtFIXTURE (InterfaceFixture x984) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x984))
     | cvtFIXTURE (TypeVarFixture x987) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x987))
     | cvtFIXTURE (TypeFixture x990) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x990))
     | cvtFIXTURE (MethodFixture{func=x993, ty=x994, readOnly=b995, override=b996, 
          final=b997}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x993), ("ty", cvtTYPE_EXPR x994), ("readOnly", PrettyRep.Bool b995), 
          ("override", PrettyRep.Bool b996), ("final", PrettyRep.Bool b997)]))
     | cvtFIXTURE (ValFixture{ty=x1011, readOnly=b1012}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1011), ("readOnly", PrettyRep.Bool b1012)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1020, getter=opt1022, setter=opt1027}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1020), ("getter", 
       (case opt1022 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1021 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1021))
       )), ("setter", 
       (case opt1027 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1026 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1026))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1040, baseTypeArgs=ls1042}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1040), ("baseTypeArgs", PrettyRep.List (List.map (fn x1041 => 
                                                                           cvtTYPE_EXPR x1041
                                                                    ) ls1042))]))
   and cvtHEAD (Head(x1053, x1054)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1053, 
          cvtINITS x1054]))
   and cvtBINDINGS (ls1059, ls1064) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1058 => 
                                                                                       cvtBINDING x1058
                                                                                ) ls1059), 
          PrettyRep.List (List.map (fn x1063 => cvtINIT_STEP x1063
                                   ) ls1064)]
   and cvtRIB ls1072 = PrettyRep.List (List.map (fn (x1069, x1070) => PrettyRep.Tuple [cvtFIXTURE_NAME x1069, 
                                                       cvtFIXTURE x1070]
                                                ) ls1072)
   and cvtRIBS ls1077 = PrettyRep.List (List.map (fn x1076 => cvtRIB x1076
                                                 ) ls1077)
   and cvtINITS ls1084 = PrettyRep.List (List.map (fn (x1081, x1082) => PrettyRep.Tuple [cvtFIXTURE_NAME x1081, 
                                                         cvtEXPR x1082]
                                                  ) ls1084)
   and cvtINSTANCE_TYPE {name=x1088, typeParams=ls1090, typeArgs=ls1095, nonnullable=b1099, 
          superTypes=ls1101, ty=x1105, dynamic=b1106} = PrettyRep.Rec [("name", 
          cvtNAME x1088), ("typeParams", PrettyRep.List (List.map (fn x1089 => 
                                                                         cvtIDENT x1089
                                                                  ) ls1090)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1094 => cvtTYPE_EXPR x1094
                                                ) ls1095)), ("nonnullable", 
          PrettyRep.Bool b1099), ("superTypes", PrettyRep.List (List.map (fn x1100 => 
                                                                                cvtTYPE_EXPR x1100
                                                                         ) ls1101)), 
          ("ty", cvtTYPE_EXPR x1105), ("dynamic", PrettyRep.Bool b1106)]
   and cvtFIELD {kind=x1122, name=x1123, init=x1124} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1122), ("name", cvtIDENT_EXPR x1123), ("init", cvtEXPR x1124)]
   and cvtFIELD_TYPE {name=x1132, ty=x1133} = PrettyRep.Rec [("name", cvtIDENT x1132), 
          ("ty", cvtTYPE_EXPR x1133)]
   and cvtFUNC_TYPE {params=ls1140, result=x1144, thisType=opt1146, hasRest=b1150, 
          minArgs=n1151} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1139 => 
                                                                                     cvtTYPE_EXPR x1139
                                                                              ) ls1140)), 
          ("result", cvtTYPE_EXPR x1144), ("thisType", 
       (case opt1146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1145 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1145))
       )), ("hasRest", PrettyRep.Bool b1150), ("minArgs", PrettyRep.Int n1151)]
   and cvtFUNC_DEFN {kind=x1163, ns=opt1165, final=b1169, override=b1170, prototype=b1171, 
          static=b1172, func=x1173} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1163), 
          ("ns", 
       (case opt1165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1164 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1164))
       )), ("final", PrettyRep.Bool b1169), ("override", PrettyRep.Bool b1170), 
          ("prototype", PrettyRep.Bool b1171), ("static", PrettyRep.Bool b1172), 
          ("func", cvtFUNC x1173)]
   and cvtCTOR_DEFN x1189 = cvtCTOR x1189
   and cvtVAR_DEFN {kind=x1190, ns=opt1192, static=b1196, prototype=b1197, 
          bindings=(ls1199, ls1204)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1190), 
          ("ns", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1191))
       )), ("static", PrettyRep.Bool b1196), ("prototype", PrettyRep.Bool b1197), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1198 => 
                                                                        cvtBINDING x1198
                                                                 ) ls1199), 
          PrettyRep.List (List.map (fn x1203 => cvtINIT_STEP x1203
                                   ) ls1204)])]
   and cvtNAMESPACE_DEFN {ident=x1220, ns=opt1222, init=opt1227} = PrettyRep.Rec [("ident", 
          cvtIDENT x1220), ("ns", 
       (case opt1222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1221))
       )), ("init", 
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1226))
       ))]
   and cvtCLASS_DEFN {ns=opt1239, ident=x1243, nonnullable=b1244, dynamic=b1245, 
          final=b1246, params=ls1248, extends=opt1253, implements=ls1258, classDefns=ls1263, 
          instanceDefns=ls1268, instanceStmts=ls1273, ctorDefn=opt1278} = PrettyRep.Rec [("ns", 
          
       (case opt1239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1238 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1238))
       )), ("ident", cvtIDENT x1243), ("nonnullable", PrettyRep.Bool b1244), 
          ("dynamic", PrettyRep.Bool b1245), ("final", PrettyRep.Bool b1246), 
          ("params", PrettyRep.List (List.map (fn x1247 => cvtIDENT x1247
                                              ) ls1248)), ("extends", 
       (case opt1253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1252 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1252))
       )), ("implements", PrettyRep.List (List.map (fn x1257 => cvtTYPE_EXPR x1257
                                                   ) ls1258)), ("classDefns", 
          PrettyRep.List (List.map (fn x1262 => cvtDEFN x1262
                                   ) ls1263)), ("instanceDefns", PrettyRep.List (List.map (fn x1267 => 
                                                                                                 cvtDEFN x1267
                                                                                          ) ls1268)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1272 => cvtSTMT x1272
                                                     ) ls1273)), ("ctorDefn", 
          
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1277))
       ))]
   and cvtINTERFACE_DEFN {ident=x1307, ns=opt1309, nonnullable=b1313, params=ls1315, 
          extends=ls1320, instanceDefns=ls1325} = PrettyRep.Rec [("ident", 
          cvtIDENT x1307), ("ns", 
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1308))
       )), ("nonnullable", PrettyRep.Bool b1313), ("params", PrettyRep.List (List.map (fn x1314 => 
                                                                                             cvtIDENT x1314
                                                                                      ) ls1315)), 
          ("extends", PrettyRep.List (List.map (fn x1319 => cvtTYPE_EXPR x1319
                                               ) ls1320)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1324 => cvtDEFN x1324
                                   ) ls1325))]
   and cvtTYPE_DEFN {ident=x1342, ns=opt1344, init=x1348} = PrettyRep.Rec [("ident", 
          cvtIDENT x1342), ("ns", 
       (case opt1344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1343 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1343))
       )), ("init", cvtTYPE_EXPR x1348)]
   and cvtCLASS_BLOCK {ns=opt1357, ident=x1361, name=opt1363, block=x1367} = 
          PrettyRep.Rec [("ns", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1356))
       )), ("ident", cvtIDENT x1361), ("name", 
       (case opt1363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1362))
       )), ("block", cvtBLOCK x1367)]
   and cvtFOR_ENUM_HEAD {isEach=b1377, bindings=(ls1379, ls1384), expr=x1389} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1377), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1378 => 
                                                                                                                         cvtBINDING x1378
                                                                                                                  ) ls1379), 
          PrettyRep.List (List.map (fn x1383 => cvtINIT_STEP x1383
                                   ) ls1384)]), ("expr", cvtEXPR x1389)]
   and cvtFOR_ENUM_STMT {isEach=b1397, defn=opt1428, obj=x1432, rib=opt1440, 
          next=x1444, labels=ls1446, body=x1450} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1397), ("defn", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1398, ns=opt1400, static=b1404, prototype=b1405, bindings=(ls1407, 
            ls1412)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1398), ("ns", 
         (case opt1400 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1399))
         )), ("static", PrettyRep.Bool b1404), ("prototype", PrettyRep.Bool b1405), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1406 => 
                                                                          cvtBINDING x1406
                                                                   ) ls1407), 
            PrettyRep.List (List.map (fn x1411 => cvtINIT_STEP x1411
                                     ) ls1412)])]))
       )), ("obj", cvtEXPR x1432), ("rib", 
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1436 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1433, 
                                                                                      x1434) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1433, 
                                                                                      cvtFIXTURE x1434]
                                                                               ) ls1436)))
       )), ("next", cvtSTMT x1444), ("labels", PrettyRep.List (List.map (fn x1445 => 
                                                                               cvtIDENT x1445
                                                                        ) ls1446)), 
          ("body", cvtSTMT x1450)]
   and cvtFOR_STMT {rib=opt1473, defn=opt1507, init=ls1512, cond=x1516, update=x1517, 
          labels=ls1519, body=x1523} = PrettyRep.Rec [("rib", 
       (case opt1473 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1469 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1466, 
                                                                                      x1467) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1466, 
                                                                                      cvtFIXTURE x1467]
                                                                               ) ls1469)))
       )), ("defn", 
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1477, ns=opt1479, static=b1483, prototype=b1484, bindings=(ls1486, 
            ls1491)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1477), ("ns", 
         (case opt1479 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1478 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1478))
         )), ("static", PrettyRep.Bool b1483), ("prototype", PrettyRep.Bool b1484), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1485 => 
                                                                          cvtBINDING x1485
                                                                   ) ls1486), 
            PrettyRep.List (List.map (fn x1490 => cvtINIT_STEP x1490
                                     ) ls1491)])]))
       )), ("init", PrettyRep.List (List.map (fn x1511 => cvtSTMT x1511
                                             ) ls1512)), ("cond", cvtEXPR x1516), 
          ("update", cvtEXPR x1517), ("labels", PrettyRep.List (List.map (fn x1518 => 
                                                                                cvtIDENT x1518
                                                                         ) ls1519)), 
          ("body", cvtSTMT x1523)]
   and cvtWHILE_STMT {cond=x1539, rib=opt1547, body=x1551, labels=ls1553} = 
          PrettyRep.Rec [("cond", cvtEXPR x1539), ("rib", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1543 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1540, 
                                                                                      x1541) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1540, 
                                                                                      cvtFIXTURE x1541]
                                                                               ) ls1543)))
       )), ("body", cvtSTMT x1551), ("labels", PrettyRep.List (List.map (fn x1552 => 
                                                                               cvtIDENT x1552
                                                                        ) ls1553))]
   and cvtDIRECTIVES {pragmas=ls1567, defns=ls1572, head=opt1577, body=ls1582, 
          loc=opt1587} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1566 => 
                                                                                    cvtPRAGMA x1566
                                                                             ) ls1567)), 
          ("defns", PrettyRep.List (List.map (fn x1571 => cvtDEFN x1571
                                             ) ls1572)), ("head", 
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1576))
       )), ("body", PrettyRep.List (List.map (fn x1581 => cvtSTMT x1581
                                             ) ls1582)), ("loc", 
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1586))
       ))]
   and cvtCASE {label=opt1603, inits=opt1614, body=x1618} = PrettyRep.Rec [("label", 
          
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1602 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1602))
       )), ("inits", 
       (case opt1614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1610 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1607, 
                                                                                      x1608) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1607, 
                                                                                      cvtEXPR x1608]
                                                                               ) ls1610)))
       )), ("body", cvtBLOCK x1618)]
   and cvtCATCH_CLAUSE {bindings=(ls1627, ls1632), ty=x1637, rib=opt1645, inits=opt1656, 
          block=x1660} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1626 => 
                                                                                                      cvtBINDING x1626
                                                                                               ) ls1627), 
          PrettyRep.List (List.map (fn x1631 => cvtINIT_STEP x1631
                                   ) ls1632)]), ("ty", cvtTYPE_EXPR x1637), 
          ("rib", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1641 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1638, 
                                                                                      x1639) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1638, 
                                                                                      cvtFIXTURE x1639]
                                                                               ) ls1641)))
       )), ("inits", 
       (case opt1656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1652 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1649, 
                                                                                      x1650) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1649, 
                                                                                      cvtEXPR x1650]
                                                                               ) ls1652)))
       )), ("block", cvtBLOCK x1660)]
   and cvtFUNC_NAME {kind=x1672, ident=x1673} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1672), 
          ("ident", cvtIDENT x1673)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1679, getter=opt1681, setter=opt1686} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1679), ("getter", 
       (case opt1681 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1680 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1680))
       )), ("setter", 
       (case opt1686 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1685 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1685))
       ))]
   and cvtFRAGMENT (Package{name=ls1698, fragments=ls1703}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1697 => 
                                                                        cvtIDENT x1697
                                                                 ) ls1698)), 
          ("fragments", PrettyRep.List (List.map (fn x1702 => cvtFRAGMENT x1702
                                                 ) ls1703))]))
     | cvtFRAGMENT (Anon x1714) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1714))
end

