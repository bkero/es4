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
   and cvtRIBS ls1071 = PrettyRep.List (List.map (fn x1070 => cvtRIB x1070
                                                 ) ls1071)
   and cvtINITS ls1078 = PrettyRep.List (List.map (fn (x1075, x1076) => PrettyRep.Tuple [cvtFIXTURE_NAME x1075, 
                                                         cvtEXPR x1076]
                                                  ) ls1078)
   and cvtINSTANCE_TYPE {name=x1082, typeParams=ls1084, typeArgs=ls1089, nonnullable=b1093, 
          superTypes=ls1095, ty=x1099, dynamic=b1100} = PrettyRep.Rec [("name", 
          cvtNAME x1082), ("typeParams", PrettyRep.List (List.map (fn x1083 => 
                                                                         cvtIDENT x1083
                                                                  ) ls1084)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1088 => cvtTYPE_EXPR x1088
                                                ) ls1089)), ("nonnullable", 
          PrettyRep.Bool b1093), ("superTypes", PrettyRep.List (List.map (fn x1094 => 
                                                                                cvtTYPE_EXPR x1094
                                                                         ) ls1095)), 
          ("ty", cvtTYPE_EXPR x1099), ("dynamic", PrettyRep.Bool b1100)]
   and cvtFIELD {kind=x1116, name=x1117, init=x1118} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1116), ("name", cvtIDENT_EXPR x1117), ("init", cvtEXPR x1118)]
   and cvtFIELD_TYPE {name=x1126, ty=x1127} = PrettyRep.Rec [("name", cvtIDENT x1126), 
          ("ty", cvtTYPE_EXPR x1127)]
   and cvtFUNC_TYPE {params=ls1134, result=x1138, thisType=opt1140, hasRest=b1144, 
          minArgs=n1145} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1133 => 
                                                                                     cvtTYPE_EXPR x1133
                                                                              ) ls1134)), 
          ("result", cvtTYPE_EXPR x1138), ("thisType", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1139))
       )), ("hasRest", PrettyRep.Bool b1144), ("minArgs", PrettyRep.Int n1145)]
   and cvtFUNC_DEFN {kind=x1157, ns=opt1159, final=b1163, override=b1164, prototype=b1165, 
          static=b1166, func=x1167} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1157), 
          ("ns", 
       (case opt1159 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1158 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1158))
       )), ("final", PrettyRep.Bool b1163), ("override", PrettyRep.Bool b1164), 
          ("prototype", PrettyRep.Bool b1165), ("static", PrettyRep.Bool b1166), 
          ("func", cvtFUNC x1167)]
   and cvtCTOR_DEFN x1183 = cvtCTOR x1183
   and cvtVAR_DEFN {kind=x1184, ns=opt1186, static=b1190, prototype=b1191, 
          bindings=(ls1193, ls1198)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1184), 
          ("ns", 
       (case opt1186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1185 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1185))
       )), ("static", PrettyRep.Bool b1190), ("prototype", PrettyRep.Bool b1191), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1192 => 
                                                                        cvtBINDING x1192
                                                                 ) ls1193), 
          PrettyRep.List (List.map (fn x1197 => cvtINIT_STEP x1197
                                   ) ls1198)])]
   and cvtNAMESPACE_DEFN {ident=x1214, ns=opt1216, init=opt1221} = PrettyRep.Rec [("ident", 
          cvtIDENT x1214), ("ns", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1215))
       )), ("init", 
       (case opt1221 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1220 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1220))
       ))]
   and cvtCLASS_DEFN {ns=opt1233, ident=x1237, nonnullable=b1238, dynamic=b1239, 
          final=b1240, params=ls1242, extends=opt1247, implements=ls1252, classDefns=ls1257, 
          instanceDefns=ls1262, instanceStmts=ls1267, ctorDefn=opt1272} = PrettyRep.Rec [("ns", 
          
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1232))
       )), ("ident", cvtIDENT x1237), ("nonnullable", PrettyRep.Bool b1238), 
          ("dynamic", PrettyRep.Bool b1239), ("final", PrettyRep.Bool b1240), 
          ("params", PrettyRep.List (List.map (fn x1241 => cvtIDENT x1241
                                              ) ls1242)), ("extends", 
       (case opt1247 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1246 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1246))
       )), ("implements", PrettyRep.List (List.map (fn x1251 => cvtTYPE_EXPR x1251
                                                   ) ls1252)), ("classDefns", 
          PrettyRep.List (List.map (fn x1256 => cvtDEFN x1256
                                   ) ls1257)), ("instanceDefns", PrettyRep.List (List.map (fn x1261 => 
                                                                                                 cvtDEFN x1261
                                                                                          ) ls1262)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1266 => cvtSTMT x1266
                                                     ) ls1267)), ("ctorDefn", 
          
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1271))
       ))]
   and cvtINTERFACE_DEFN {ident=x1301, ns=opt1303, nonnullable=b1307, params=ls1309, 
          extends=ls1314, instanceDefns=ls1319} = PrettyRep.Rec [("ident", 
          cvtIDENT x1301), ("ns", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1302))
       )), ("nonnullable", PrettyRep.Bool b1307), ("params", PrettyRep.List (List.map (fn x1308 => 
                                                                                             cvtIDENT x1308
                                                                                      ) ls1309)), 
          ("extends", PrettyRep.List (List.map (fn x1313 => cvtTYPE_EXPR x1313
                                               ) ls1314)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1318 => cvtDEFN x1318
                                   ) ls1319))]
   and cvtTYPE_DEFN {ident=x1336, ns=opt1338, init=x1342} = PrettyRep.Rec [("ident", 
          cvtIDENT x1336), ("ns", 
       (case opt1338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1337 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1337))
       )), ("init", cvtTYPE_EXPR x1342)]
   and cvtCLASS_BLOCK {ns=opt1351, ident=x1355, name=opt1357, block=x1361} = 
          PrettyRep.Rec [("ns", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1350))
       )), ("ident", cvtIDENT x1355), ("name", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1356))
       )), ("block", cvtBLOCK x1361)]
   and cvtFOR_ENUM_HEAD {isEach=b1371, bindings=(ls1373, ls1378), expr=x1383} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1371), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1372 => 
                                                                                                                         cvtBINDING x1372
                                                                                                                  ) ls1373), 
          PrettyRep.List (List.map (fn x1377 => cvtINIT_STEP x1377
                                   ) ls1378)]), ("expr", cvtEXPR x1383)]
   and cvtFOR_ENUM_STMT {isEach=b1391, defn=opt1422, obj=x1426, rib=opt1434, 
          next=x1438, labels=ls1440, body=x1444} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1391), ("defn", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1392, ns=opt1394, static=b1398, prototype=b1399, bindings=(ls1401, 
            ls1406)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1392), ("ns", 
         (case opt1394 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1393 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1393))
         )), ("static", PrettyRep.Bool b1398), ("prototype", PrettyRep.Bool b1399), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1400 => 
                                                                          cvtBINDING x1400
                                                                   ) ls1401), 
            PrettyRep.List (List.map (fn x1405 => cvtINIT_STEP x1405
                                     ) ls1406)])]))
       )), ("obj", cvtEXPR x1426), ("rib", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1430 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1427, 
                                                                                      x1428) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1427, 
                                                                                      cvtFIXTURE x1428]
                                                                               ) ls1430)))
       )), ("next", cvtSTMT x1438), ("labels", PrettyRep.List (List.map (fn x1439 => 
                                                                               cvtIDENT x1439
                                                                        ) ls1440)), 
          ("body", cvtSTMT x1444)]
   and cvtFOR_STMT {rib=opt1467, defn=opt1501, init=ls1506, cond=x1510, update=x1511, 
          labels=ls1513, body=x1517} = PrettyRep.Rec [("rib", 
       (case opt1467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1463 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1460, 
                                                                                      x1461) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1460, 
                                                                                      cvtFIXTURE x1461]
                                                                               ) ls1463)))
       )), ("defn", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1471, ns=opt1473, static=b1477, prototype=b1478, bindings=(ls1480, 
            ls1485)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1471), ("ns", 
         (case opt1473 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1472 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1472))
         )), ("static", PrettyRep.Bool b1477), ("prototype", PrettyRep.Bool b1478), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1479 => 
                                                                          cvtBINDING x1479
                                                                   ) ls1480), 
            PrettyRep.List (List.map (fn x1484 => cvtINIT_STEP x1484
                                     ) ls1485)])]))
       )), ("init", PrettyRep.List (List.map (fn x1505 => cvtSTMT x1505
                                             ) ls1506)), ("cond", cvtEXPR x1510), 
          ("update", cvtEXPR x1511), ("labels", PrettyRep.List (List.map (fn x1512 => 
                                                                                cvtIDENT x1512
                                                                         ) ls1513)), 
          ("body", cvtSTMT x1517)]
   and cvtWHILE_STMT {cond=x1533, rib=opt1541, body=x1545, labels=ls1547} = 
          PrettyRep.Rec [("cond", cvtEXPR x1533), ("rib", 
       (case opt1541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1537 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1534, 
                                                                                      x1535) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1534, 
                                                                                      cvtFIXTURE x1535]
                                                                               ) ls1537)))
       )), ("body", cvtSTMT x1545), ("labels", PrettyRep.List (List.map (fn x1546 => 
                                                                               cvtIDENT x1546
                                                                        ) ls1547))]
   and cvtDIRECTIVES {pragmas=ls1561, defns=ls1566, head=opt1571, body=ls1576, 
          loc=opt1581} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1560 => 
                                                                                    cvtPRAGMA x1560
                                                                             ) ls1561)), 
          ("defns", PrettyRep.List (List.map (fn x1565 => cvtDEFN x1565
                                             ) ls1566)), ("head", 
       (case opt1571 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1570 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1570))
       )), ("body", PrettyRep.List (List.map (fn x1575 => cvtSTMT x1575
                                             ) ls1576)), ("loc", 
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1580))
       ))]
   and cvtCASE {label=opt1597, inits=opt1608, body=x1612} = PrettyRep.Rec [("label", 
          
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1596 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1596))
       )), ("inits", 
       (case opt1608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1604 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1601, 
                                                                                      x1602) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1601, 
                                                                                      cvtEXPR x1602]
                                                                               ) ls1604)))
       )), ("body", cvtBLOCK x1612)]
   and cvtCATCH_CLAUSE {bindings=(ls1621, ls1626), ty=x1631, rib=opt1639, inits=opt1650, 
          block=x1654} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1620 => 
                                                                                                      cvtBINDING x1620
                                                                                               ) ls1621), 
          PrettyRep.List (List.map (fn x1625 => cvtINIT_STEP x1625
                                   ) ls1626)]), ("ty", cvtTYPE_EXPR x1631), 
          ("rib", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1635 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1632, 
                                                                                      x1633) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1632, 
                                                                                      cvtFIXTURE x1633]
                                                                               ) ls1635)))
       )), ("inits", 
       (case opt1650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1646 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1643, 
                                                                                      x1644) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1643, 
                                                                                      cvtEXPR x1644]
                                                                               ) ls1646)))
       )), ("block", cvtBLOCK x1654)]
   and cvtFUNC_NAME {kind=x1666, ident=x1667} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1666), 
          ("ident", cvtIDENT x1667)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1673, getter=opt1675, setter=opt1680} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1673), ("getter", 
       (case opt1675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1674 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1674))
       )), ("setter", 
       (case opt1680 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1679 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1679))
       ))]
   and cvtFRAGMENT (Package{name=ls1692, fragments=ls1697}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1691 => 
                                                                        cvtIDENT x1691
                                                                 ) ls1692)), 
          ("fragments", PrettyRep.List (List.map (fn x1696 => cvtFRAGMENT x1696
                                                 ) ls1697))]))
     | cvtFRAGMENT (Anon x1708) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1708))
end

