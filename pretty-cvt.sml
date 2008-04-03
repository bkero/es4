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
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x487) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x487))
     | cvtSTMT (InitStmt{kind=x490, ns=opt492, prototype=b496, static=b497, 
          temps=x498, inits=ls500}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x490), ("ns", 
       (case opt492 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x491 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x491))
       )), ("prototype", PrettyRep.Bool b496), ("static", PrettyRep.Bool b497), 
          ("temps", cvtBINDINGS x498), ("inits", PrettyRep.List (List.map (fn x499 => 
                                                                                 cvtINIT_STEP x499
                                                                          ) ls500))]))
     | cvtSTMT (ClassBlock x519) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x519))
     | cvtSTMT (ForInStmt x522) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x522))
     | cvtSTMT (ThrowStmt x525) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x525))
     | cvtSTMT (ReturnStmt x528) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x528))
     | cvtSTMT (BreakStmt opt532) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x531 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x531))
       ))
     | cvtSTMT (ContinueStmt opt539) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt539 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x538 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x538))
       ))
     | cvtSTMT (BlockStmt x545) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x545))
     | cvtSTMT (LabeledStmt(x548, x549)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x548, 
          cvtSTMT x549]))
     | cvtSTMT (LetStmt x553) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x553))
     | cvtSTMT (WhileStmt x556) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x556))
     | cvtSTMT (DoWhileStmt x559) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x559))
     | cvtSTMT (ForStmt x562) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x562))
     | cvtSTMT (IfStmt{cnd=x565, thn=x566, els=x567}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x565), ("thn", cvtSTMT x566), 
          ("els", cvtSTMT x567)]))
     | cvtSTMT (WithStmt{obj=x577, ty=x578, body=x579}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x577), ("ty", cvtTYPE_EXPR x578), 
          ("body", cvtSTMT x579)]))
     | cvtSTMT (TryStmt{block=x589, catches=ls591, finally=opt596}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x589), ("catches", PrettyRep.List (List.map (fn x590 => 
                                                                                                     cvtCATCH_CLAUSE x590
                                                                                              ) ls591)), 
          ("finally", 
       (case opt596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x595 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x595))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x609, labels=ls611, cases=ls616}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x609), ("labels", PrettyRep.List (List.map (fn x610 => 
                                                                                                  cvtIDENT x610
                                                                                           ) ls611)), 
          ("cases", PrettyRep.List (List.map (fn x615 => cvtCASE x615
                                             ) ls616))]))
     | cvtSTMT (SwitchTypeStmt{cond=x629, ty=x630, cases=ls632}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x629), ("ty", cvtTYPE_EXPR x630), 
          ("cases", PrettyRep.List (List.map (fn x631 => cvtCATCH_CLAUSE x631
                                             ) ls632))]))
     | cvtSTMT (DXNStmt{expr=x645}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x645)]))
   and cvtEXPR (TernaryExpr(x651, x652, x653)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x651, cvtEXPR x652, cvtEXPR x653]))
     | cvtEXPR (BinaryExpr(x657, x658, x659)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x657, cvtEXPR x658, cvtEXPR x659]))
     | cvtEXPR (BinaryTypeExpr(x663, x664, x665)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x663, cvtEXPR x664, cvtTYPE_EXPR x665]))
     | cvtEXPR (UnaryExpr(x669, x670)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x669, 
          cvtEXPR x670]))
     | cvtEXPR (TypeExpr x674) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x674))
     | cvtEXPR (ThisExpr opt678) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt678 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x677 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x677))
       ))
     | cvtEXPR (YieldExpr opt685) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt685 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x684 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x684))
       ))
     | cvtEXPR (SuperExpr opt692) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt692 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x691 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x691))
       ))
     | cvtEXPR (LiteralExpr x698) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x698))
     | cvtEXPR (CallExpr{func=x701, actuals=ls703}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x701), ("actuals", PrettyRep.List (List.map (fn x702 => 
                                                                                                   cvtEXPR x702
                                                                                            ) ls703))]))
     | cvtEXPR (ApplyTypeExpr{expr=x714, actuals=ls716}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x714), ("actuals", PrettyRep.List (List.map (fn x715 => 
                                                                                                   cvtTYPE_EXPR x715
                                                                                            ) ls716))]))
     | cvtEXPR (LetExpr{defs=x727, body=x728, head=opt730}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x727), ("body", cvtEXPR x728), 
          ("head", 
       (case opt730 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x729 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x729))
       ))]))
     | cvtEXPR (NewExpr{obj=x743, actuals=ls745}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x743), ("actuals", PrettyRep.List (List.map (fn x744 => 
                                                                                                  cvtEXPR x744
                                                                                           ) ls745))]))
     | cvtEXPR (ObjectRef{base=x756, ident=x757, loc=opt759}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x756), ("ident", cvtIDENT_EXPR x757), 
          ("loc", 
       (case opt759 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x758 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x758))
       ))]))
     | cvtEXPR (LexicalRef{ident=x772, loc=opt774}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x772), ("loc", 
       (case opt774 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x773 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x773))
       ))]))
     | cvtEXPR (SetExpr(x785, x786, x787)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x785, 
          cvtEXPR x786, cvtEXPR x787]))
     | cvtEXPR (ListExpr ls792) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x791 => 
                                                                                                    cvtEXPR x791
                                                                                             ) ls792)))
     | cvtEXPR (InitExpr(x798, x799, x800)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x798, 
          cvtHEAD x799, cvtINITS x800]))
     | cvtEXPR (GetTemp n804) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n804))
     | cvtEXPR (GetParam n807) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n807))
     | cvtEXPR (Comprehension(x810, ls812, opt817)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x810, PrettyRep.List (List.map (fn x811 => 
                                                                               cvtFOR_ENUM_HEAD x811
                                                                        ) ls812), 
          
       (case opt817 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x816 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x816))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n829) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n829))
     | cvtFIXTURE_NAME (PropName x832) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x832))
   and cvtIDENT_EXPR (Identifier{ident=x835, openNamespaces=ls841}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x835), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls837 => PrettyRep.List (List.map (fn x836 => 
                                                                                cvtNAMESPACE x836
                                                                         ) ls837)
                                   ) ls841))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x852, expr=x853}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x852), ("expr", cvtEXPR x853)]))
     | cvtIDENT_EXPR (AttributeIdentifier x861) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x861))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x864, openNamespaces=ls870}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x864), ("openNamespaces", PrettyRep.List (List.map (fn ls866 => 
                                                                            PrettyRep.List (List.map (fn x865 => 
                                                                                                            cvtNAMESPACE x865
                                                                                                     ) ls866)
                                                                     ) ls870))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x881, ident=s882}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x881), ("ident", PrettyRep.UniStr s882)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls891, x895)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x890 => cvtIDENT x890
                                                          ) ls891), cvtIDENT_EXPR x895]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r902) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r902))
     | cvtLITERAL (LiteralDecimal d905) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d905))
     | cvtLITERAL (LiteralInt i908) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i908))
     | cvtLITERAL (LiteralUInt u911) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u911))
     | cvtLITERAL (LiteralBoolean b914) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b914))
     | cvtLITERAL (LiteralString s917) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s917))
     | cvtLITERAL (LiteralArray{exprs=x920, ty=opt922}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x920), ("ty", 
       (case opt922 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x921 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x921))
       ))]))
     | cvtLITERAL (LiteralXML ls934) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x933 => 
                                                                                                           cvtEXPR x933
                                                                                                    ) ls934)))
     | cvtLITERAL (LiteralNamespace x940) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x940))
     | cvtLITERAL (LiteralObject{expr=ls944, ty=opt949}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x943 => 
                                                                        cvtFIELD x943
                                                                 ) ls944)), 
          ("ty", 
       (case opt949 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x948 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x948))
       ))]))
     | cvtLITERAL (LiteralFunction x960) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x960))
     | cvtLITERAL (LiteralRegExp{str=s963}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s963)]))
   and cvtBLOCK (Block x969) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x969))
   and cvtFIXTURE (NamespaceFixture x972) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x972))
     | cvtFIXTURE (ClassFixture x975) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x975))
     | cvtFIXTURE (InterfaceFixture x978) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x978))
     | cvtFIXTURE (TypeVarFixture x981) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x981))
     | cvtFIXTURE (TypeFixture x984) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x984))
     | cvtFIXTURE (MethodFixture{func=x987, ty=x988, readOnly=b989, override=b990, 
          final=b991}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x987), ("ty", cvtTYPE_EXPR x988), ("readOnly", PrettyRep.Bool b989), 
          ("override", PrettyRep.Bool b990), ("final", PrettyRep.Bool b991)]))
     | cvtFIXTURE (ValFixture{ty=x1005, readOnly=b1006}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1005), ("readOnly", PrettyRep.Bool b1006)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1014, getter=opt1016, setter=opt1021}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1014), ("getter", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1015))
       )), ("setter", 
       (case opt1021 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1020 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1020))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1034, baseTypeArgs=ls1036}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1034), ("baseTypeArgs", PrettyRep.List (List.map (fn x1035 => 
                                                                           cvtTYPE_EXPR x1035
                                                                    ) ls1036))]))
   and cvtHEAD (Head(x1047, x1048)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1047, 
          cvtINITS x1048]))
   and cvtBINDINGS (ls1053, ls1058) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1052 => 
                                                                                       cvtBINDING x1052
                                                                                ) ls1053), 
          PrettyRep.List (List.map (fn x1057 => cvtINIT_STEP x1057
                                   ) ls1058)]
   and cvtRIB ls1066 = PrettyRep.List (List.map (fn (x1063, x1064) => PrettyRep.Tuple [cvtFIXTURE_NAME x1063, 
                                                       cvtFIXTURE x1064]
                                                ) ls1066)
   and cvtRIBS ls1077 = PrettyRep.List (List.map (fn ls1073 => PrettyRep.List (List.map (fn (x1070, 
                                                                                               x1071) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1070, 
                                                                                               cvtFIXTURE x1071]
                                                                                        ) ls1073)
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

