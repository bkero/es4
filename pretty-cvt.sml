structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Private) = PrettyRep.Ctor ("Private", NONE)
     | cvtNAMESPACE (Protected) = PrettyRep.Ctor ("Protected", NONE)
     | cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Public x5) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x5))
     | cvtNAMESPACE (Internal x8) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x8))
     | cvtNAMESPACE (UserDefined x11) = PrettyRep.Ctor ("UserDefined", SOME (cvtIDENT x11))
   and cvtNAME {ns=x14, id=x15} = PrettyRep.Rec [("ns", cvtNAMESPACE x14), 
          ("id", cvtIDENT x15)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtROUNDING_MODE (Ceiling) = PrettyRep.Ctor ("Ceiling", NONE)
     | cvtROUNDING_MODE (Floor) = PrettyRep.Ctor ("Floor", NONE)
     | cvtROUNDING_MODE (Up) = PrettyRep.Ctor ("Up", NONE)
     | cvtROUNDING_MODE (Down) = PrettyRep.Ctor ("Down", NONE)
     | cvtROUNDING_MODE (HalfUp) = PrettyRep.Ctor ("HalfUp", NONE)
     | cvtROUNDING_MODE (HalfDown) = PrettyRep.Ctor ("HalfDown", NONE)
     | cvtROUNDING_MODE (HalfEven) = PrettyRep.Ctor ("HalfEven", NONE)
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
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
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
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
     | cvtUNOP (MakeNamespace) = PrettyRep.Ctor ("MakeNamespace", NONE)
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
   and cvtNULOP (This) = PrettyRep.Ctor ("This", NONE)
     | cvtNULOP (Empty) = PrettyRep.Ctor ("Empty", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
     | cvtVAR_DEFN_TAG (Rest) = PrettyRep.Ctor ("Rest", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x100) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x100))
     | cvtPRAGMA (UseDefaultNamespace x103) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x103))
     | cvtPRAGMA (UseNumber x106) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x106))
     | cvtPRAGMA (UseRounding x109) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x109))
     | cvtPRAGMA (UsePrecision x112) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x112))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x117, name=x118, alias=opt120}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x117), ("name", cvtIDENT x118), 
          ("alias", 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x119))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x140, fsig=x141, body=x142}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x140), ("fsig", cvtFUNC_SIG x141), 
          ("body", cvtBLOCK x142)]))
   and cvtDEFN (ClassDefn x152) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x152))
     | cvtDEFN (VariableDefn ls156) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x155 => 
                                                                                                            cvtVAR_BINDING x155
                                                                                                     ) ls156)))
     | cvtDEFN (FunctionDefn x162) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x162))
     | cvtDEFN (InterfaceDefn x165) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x165))
     | cvtDEFN (NamespaceDefn{attrs=x168, ident=x169, init=opt171}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x168), ("ident", cvtIDENT x169), 
          ("init", 
       (case opt171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x170 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x170))
       ))]))
     | cvtDEFN (TypeDefn{attrs=x184, ident=x185, init=x186}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x184), ("ident", cvtIDENT x185), 
          ("init", cvtTYPE_EXPR x186)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls197, params=ls202, returnType=x206, 
          thisType=opt208, hasBoundThis=b212, hasRest=b213}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x196 => 
                                                                              cvtIDENT x196
                                                                       ) ls197)), 
          ("params", PrettyRep.List (List.map (fn x201 => cvtVAR_BINDING x201
                                              ) ls202)), ("returnType", cvtTYPE_EXPR x206), 
          ("thisType", 
       (case opt208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x207 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x207))
       )), ("hasBoundThis", PrettyRep.Bool b212), ("hasRest", PrettyRep.Bool b213)]))
   and cvtATTRIBUTES (Attributes{ns=x229, override=b230, static=b231, final=b232, 
          dynamic=b233, prototype=b234, native=b235, rest=b236}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x229), ("override", PrettyRep.Bool b230), 
          ("static", PrettyRep.Bool b231), ("final", PrettyRep.Bool b232), 
          ("dynamic", PrettyRep.Bool b233), ("prototype", PrettyRep.Bool b234), 
          ("native", PrettyRep.Bool b235), ("rest", PrettyRep.Bool b236)]))
   and cvtVAR_BINDING (Binding{kind=x256, init=opt258, attrs=x262, pattern=x263, 
          ty=opt265}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x256), ("init", 
       (case opt258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x257 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x257))
       )), ("attrs", cvtATTRIBUTES x262), ("pattern", cvtPATTERN x263), ("ty", 
          
       (case opt265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x264 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x264))
       ))]))
   and cvtTYPE_EXPR (SpecialType x282) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x282))
     | cvtTYPE_EXPR (UnionType ls286) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x285 => 
                                                                                                           cvtTYPE_EXPR x285
                                                                                                    ) ls286)))
     | cvtTYPE_EXPR (ArrayType ls293) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x292 => 
                                                                                                           cvtTYPE_EXPR x292
                                                                                                    ) ls293)))
     | cvtTYPE_EXPR (NominalType{ident=x299, nullable=opt301}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x299), ("nullable", 
          
       (case opt301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b300 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b300))
       ))]))
     | cvtTYPE_EXPR (FunctionType x312) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x312))
     | cvtTYPE_EXPR (ObjectType ls316) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x315 => 
                                                                                                             cvtFIELD_TYPE x315
                                                                                                      ) ls316)))
     | cvtTYPE_EXPR (AppType{base=x322, args=ls324}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x322), ("args", PrettyRep.List (List.map (fn x323 => 
                                                                                                     cvtTYPE_EXPR x323
                                                                                              ) ls324))]))
     | cvtTYPE_EXPR (NullableType{expr=x335, nullable=b336}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x335), ("nullable", PrettyRep.Bool b336)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls346) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x345 => 
                                                                                                    cvtEXPR x345
                                                                                             ) ls346)))
     | cvtSTMT (ForEachStmt x352) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x352))
     | cvtSTMT (ForInStmt x355) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x355))
     | cvtSTMT (ThrowStmt ls359) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x358 => 
                                                                                                      cvtEXPR x358
                                                                                               ) ls359)))
     | cvtSTMT (ReturnStmt ls366) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x365 => 
                                                                                                        cvtEXPR x365
                                                                                                 ) ls366)))
     | cvtSTMT (BreakStmt opt373) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x372 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x372))
       ))
     | cvtSTMT (ContinueStmt opt380) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt380 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x379 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x379))
       ))
     | cvtSTMT (BlockStmt x386) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x386))
     | cvtSTMT (LabeledStmt(x389, x390)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x389, 
          cvtSTMT x390]))
     | cvtSTMT (LetStmt(ls395, x399)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x394 => 
                                                                                                                          cvtVAR_BINDING x394
                                                                                                                   ) ls395), 
          cvtSTMT x399]))
     | cvtSTMT (SuperStmt ls404) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x403 => 
                                                                                                      cvtEXPR x403
                                                                                               ) ls404)))
     | cvtSTMT (WhileStmt x410) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x410))
     | cvtSTMT (DoWhileStmt x413) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x413))
     | cvtSTMT (ForStmt{defns=ls417, init=ls422, cond=ls427, update=ls432, 
          contLabel=opt437, body=x441}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x416 => cvtVAR_BINDING x416
                                   ) ls417)), ("init", PrettyRep.List (List.map (fn x421 => 
                                                                                       cvtEXPR x421
                                                                                ) ls422)), 
          ("cond", PrettyRep.List (List.map (fn x426 => cvtEXPR x426
                                            ) ls427)), ("update", PrettyRep.List (List.map (fn x431 => 
                                                                                                  cvtEXPR x431
                                                                                           ) ls432)), 
          ("contLabel", 
       (case opt437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x436 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x436))
       )), ("body", cvtSTMT x441)]))
     | cvtSTMT (IfStmt{cnd=x457, thn=x458, els=x459}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x457), ("thn", cvtSTMT x458), 
          ("els", cvtSTMT x459)]))
     | cvtSTMT (WithStmt{obj=ls470, ty=x474, body=x475}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x469 => 
                                                                       cvtEXPR x469
                                                                ) ls470)), 
          ("ty", cvtTYPE_EXPR x474), ("body", cvtSTMT x475)]))
     | cvtSTMT (TryStmt{body=x485, catches=ls493, finally=opt498}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x485), ("catches", PrettyRep.List (List.map (fn {bind=x486, 
                                                                                                    body=x487} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x486), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x487)]
                                                                                             ) ls493)), 
          ("finally", 
       (case opt498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x497 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x497))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls512, cases=ls517}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x511 => 
                                                                        cvtEXPR x511
                                                                 ) ls512)), 
          ("cases", PrettyRep.List (List.map (fn x516 => cvtCASE x516
                                             ) ls517))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls529, ty=x533, cases=ls535}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x528 => 
                                                                        cvtEXPR x528
                                                                 ) ls529)), 
          ("ty", cvtTYPE_EXPR x533), ("cases", PrettyRep.List (List.map (fn x534 => 
                                                                               cvtTYPE_CASE x534
                                                                        ) ls535))]))
     | cvtSTMT (Dxns{expr=x548}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x548)]))
   and cvtEXPR (TrinaryExpr(x554, x555, x556, x557)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x554, cvtEXPR x555, cvtEXPR x556, 
          cvtEXPR x557]))
     | cvtEXPR (BinaryExpr(x561, x562, x563)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x561, cvtEXPR x562, cvtEXPR x563]))
     | cvtEXPR (BinaryTypeExpr(x567, x568, x569)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x567, cvtEXPR x568, cvtTYPE_EXPR x569]))
     | cvtEXPR (UnaryExpr(x573, x574)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x573, 
          cvtEXPR x574]))
     | cvtEXPR (TypeExpr x578) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x578))
     | cvtEXPR (NullaryExpr x581) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x581))
     | cvtEXPR (YieldExpr opt589) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls585 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x584 => 
                                                                                     cvtEXPR x584
                                                                              ) ls585)))
       ))
     | cvtEXPR (SuperExpr opt596) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x595 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x595))
       ))
     | cvtEXPR (LiteralExpr x602) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x602))
     | cvtEXPR (CallExpr{func=x605, actuals=ls607}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x605), ("actuals", PrettyRep.List (List.map (fn x606 => 
                                                                                                   cvtEXPR x606
                                                                                            ) ls607))]))
     | cvtEXPR (ApplyTypeExpr{expr=x618, actuals=ls620}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x618), ("actuals", PrettyRep.List (List.map (fn x619 => 
                                                                                                   cvtTYPE_EXPR x619
                                                                                            ) ls620))]))
     | cvtEXPR (LetExpr{defs=ls632, body=ls637}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x631 => 
                                                                        cvtVAR_BINDING x631
                                                                 ) ls632)), 
          ("body", PrettyRep.List (List.map (fn x636 => cvtEXPR x636
                                            ) ls637))]))
     | cvtEXPR (NewExpr{obj=x648, actuals=ls650}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x648), ("actuals", PrettyRep.List (List.map (fn x649 => 
                                                                                                  cvtEXPR x649
                                                                                           ) ls650))]))
     | cvtEXPR (FunExpr{ident=opt662, fsig=x666, body=x667}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x661))
       )), ("fsig", cvtFUNC_SIG x666), ("body", cvtBLOCK x667)]))
     | cvtEXPR (ObjectRef{base=x677, ident=x678}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x677), ("ident", cvtIDENT_EXPR x678)]))
     | cvtEXPR (LexicalRef{ident=x686}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x686)]))
     | cvtEXPR (SetExpr(x692, x693, x694)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x692, 
          cvtPATTERN x693, cvtEXPR x694]))
     | cvtEXPR (ListExpr ls699) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x698 => 
                                                                                                    cvtEXPR x698
                                                                                             ) ls699)))
     | cvtEXPR (SliceExpr(ls706, ls711, ls716)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x705 => cvtEXPR x705
                                                          ) ls706), PrettyRep.List (List.map (fn x710 => 
                                                                                                    cvtEXPR x710
                                                                                             ) ls711), 
          PrettyRep.List (List.map (fn x715 => cvtEXPR x715
                                   ) ls716)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x723, ident=x724}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x723), ("ident", cvtUSTRING x724)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x732, expr=x733}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x732), ("expr", cvtEXPR x733)]))
     | cvtIDENT_EXPR (AttributeIdentifier x741) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x741))
     | cvtIDENT_EXPR (Identifier{ident=x744, openNamespaces=ls746}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x744), ("openNamespaces", 
          PrettyRep.List (List.map (fn x745 => cvtNAMESPACE x745
                                   ) ls746))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x757) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x757))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x760, typeParams=ls762}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x760), ("typeParams", 
          PrettyRep.List (List.map (fn x761 => cvtTYPE_EXPR x761
                                   ) ls762))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r775) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r775))
     | cvtLITERAL (LiteralBoolean b778) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b778))
     | cvtLITERAL (LiteralString x781) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x781))
     | cvtLITERAL (LiteralArray{exprs=ls785, ty=opt790}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x784 => 
                                                                         cvtEXPR x784
                                                                  ) ls785)), 
          ("ty", 
       (case opt790 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x789 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x789))
       ))]))
     | cvtLITERAL (LiteralXML ls802) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x801 => 
                                                                                                           cvtEXPR x801
                                                                                                    ) ls802)))
     | cvtLITERAL (LiteralNamespace x808) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x808))
     | cvtLITERAL (LiteralObject{expr=ls812, ty=opt817}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x811 => 
                                                                        cvtFIELD x811
                                                                 ) ls812)), 
          ("ty", 
       (case opt817 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x816 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x816))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x828}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x828)]))
   and cvtBLOCK (Block x834) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x834))
   and cvtPATTERN (ObjectPattern ls844) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x837, ptrn=x838} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x837), ("ptrn", 
                                                cvtPATTERN x838)]
                                         ) ls844)))
     | cvtPATTERN (ArrayPattern ls851) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x850 => 
                                                                                                               cvtPATTERN x850
                                                                                                        ) ls851)))
     | cvtPATTERN (SimplePattern x857) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x857))
     | cvtPATTERN (IdentifierPattern x860) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x860))
   and cvtFIXTURES_TAG (ClassFixtures) = PrettyRep.Ctor ("ClassFixtures", NONE)
     | cvtFIXTURES_TAG (BlockFixtures) = PrettyRep.Ctor ("BlockFixtures", NONE)
     | cvtFIXTURES_TAG (GlobalFixtures) = PrettyRep.Ctor ("GlobalFixtures", 
          NONE)
   and cvtFIXTURES (Fixtures{tag=x866, parent=opt868, fixtures=x872, isExtensible=b873}) = 
          PrettyRep.Ctor ("Fixtures", SOME (PrettyRep.Rec [("tag", cvtFIXTURES_TAG x866), 
          ("parent", 
       (case opt868 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x867 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x867))
       )), ("fixtures", cvtFIXTURE_BINDINGS x872), ("isExtensible", PrettyRep.Bool b873)]))
   and cvtFIXTURE (NamespaceFixture x885) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x885))
     | cvtFIXTURE (TypeFixture x888) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x888))
     | cvtFIXTURE (PropFixture{ty=x891, readOnly=b892, isOverride=b893, subFixtures=opt895}) = 
          PrettyRep.Ctor ("PropFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x891), 
          ("readOnly", PrettyRep.Bool b892), ("isOverride", PrettyRep.Bool b893), 
          ("subFixtures", 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x894))
       ))]))
   and cvtFIELD {kind=x910, name=x911, init=x912} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x910), ("name", cvtIDENT_EXPR x911), ("init", cvtEXPR x912)]
   and cvtFIELD_TYPE {name=x920, ty=x921} = PrettyRep.Rec [("name", cvtIDENT_EXPR x920), 
          ("ty", cvtTYPE_EXPR x921)]
   and cvtTYPED_IDENT {name=x927, ty=opt929} = PrettyRep.Rec [("name", cvtIDENT x927), 
          ("ty", 
       (case opt929 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x928 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x928))
       ))]
   and cvtFUNC_DEFN {attrs=x938, kind=x939, func=x940} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x938), ("kind", cvtVAR_DEFN_TAG x939), ("func", cvtFUNC x940)]
   and cvtFIXTURE_BINDINGS ls951 = PrettyRep.List (List.map (fn (x948, x949) => 
                                                                   PrettyRep.Tuple [cvtNAME x948, 
                                                                   cvtFIXTURE x949]
                                                            ) ls951)
   and cvtCLASS_DEFN {name=x955, nonnullable=b956, attrs=x957, params=ls959, 
          extends=opt964, implements=ls969, classFixtures=opt974, instanceFixtures=opt979, 
          body=x983, instanceVars=ls985, instanceMethods=ls990, vars=ls995, 
          methods=ls1000, constructor=opt1005, initializer=ls1010} = PrettyRep.Rec [("name", 
          cvtIDENT x955), ("nonnullable", PrettyRep.Bool b956), ("attrs", cvtATTRIBUTES x957), 
          ("params", PrettyRep.List (List.map (fn x958 => cvtIDENT x958
                                              ) ls959)), ("extends", 
       (case opt964 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x963 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x963))
       )), ("implements", PrettyRep.List (List.map (fn x968 => cvtIDENT_EXPR x968
                                                   ) ls969)), ("classFixtures", 
          
       (case opt974 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x973 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x973))
       )), ("instanceFixtures", 
       (case opt979 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x978 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x978))
       )), ("body", cvtBLOCK x983), ("instanceVars", PrettyRep.List (List.map (fn x984 => 
                                                                                     cvtVAR_BINDING x984
                                                                              ) ls985)), 
          ("instanceMethods", PrettyRep.List (List.map (fn x989 => cvtFUNC x989
                                                       ) ls990)), ("vars", 
          PrettyRep.List (List.map (fn x994 => cvtVAR_BINDING x994
                                   ) ls995)), ("methods", PrettyRep.List (List.map (fn x999 => 
                                                                                          cvtFUNC x999
                                                                                   ) ls1000)), 
          ("constructor", 
       (case opt1005 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1004 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1004))
       )), ("initializer", PrettyRep.List (List.map (fn x1009 => cvtSTMT x1009
                                                    ) ls1010))]
   and cvtINTERFACE_DEFN {name=x1045, nonnullable=b1046, attrs=x1047, params=ls1049, 
          extends=ls1054, body=x1058} = PrettyRep.Rec [("name", cvtIDENT x1045), 
          ("nonnullable", PrettyRep.Bool b1046), ("attrs", cvtATTRIBUTES x1047), 
          ("params", PrettyRep.List (List.map (fn x1048 => cvtIDENT x1048
                                              ) ls1049)), ("extends", PrettyRep.List (List.map (fn x1053 => 
                                                                                                      cvtIDENT_EXPR x1053
                                                                                               ) ls1054)), 
          ("body", cvtBLOCK x1058)]
   and cvtFOR_ENUM_STMT {ptrn=opt1073, obj=ls1078, defns=ls1083, contLabel=opt1088, 
          body=x1092} = PrettyRep.Rec [("ptrn", 
       (case opt1073 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1072 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1072))
       )), ("obj", PrettyRep.List (List.map (fn x1077 => cvtEXPR x1077
                                            ) ls1078)), ("defns", PrettyRep.List (List.map (fn x1082 => 
                                                                                                  cvtVAR_BINDING x1082
                                                                                           ) ls1083)), 
          ("contLabel", 
       (case opt1088 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1087 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1087))
       )), ("body", cvtSTMT x1092)]
   and cvtWHILE_STMT {cond=x1104, body=x1105, contLabel=opt1107} = PrettyRep.Rec [("cond", 
          cvtEXPR x1104), ("body", cvtSTMT x1105), ("contLabel", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1106))
       ))]
   and cvtDIRECTIVES {pragmas=ls1119, defns=ls1124, stmts=ls1129, fixtures=opt1134} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1118 => 
                                                                     cvtPRAGMA x1118
                                                              ) ls1119)), ("defns", 
          PrettyRep.List (List.map (fn x1123 => cvtDEFN x1123
                                   ) ls1124)), ("stmts", PrettyRep.List (List.map (fn x1128 => 
                                                                                         cvtSTMT x1128
                                                                                  ) ls1129)), 
          ("fixtures", 
       (case opt1134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1133 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1133))
       ))]
   and cvtBINDINGS {defns=ls1148, inits=ls1153} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1147 => cvtVAR_BINDING x1147
                                   ) ls1148)), ("inits", PrettyRep.List (List.map (fn x1152 => 
                                                                                         cvtEXPR x1152
                                                                                  ) ls1153))]
   and cvtCASE {label=opt1167, body=x1171} = PrettyRep.Rec [("label", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1163 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1162 => 
                                                                                      cvtEXPR x1162
                                                                               ) ls1163)))
       )), ("body", cvtBLOCK x1171)]
   and cvtTYPE_CASE {ptrn=opt1178, body=x1182} = PrettyRep.Rec [("ptrn", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1177))
       )), ("body", cvtBLOCK x1182)]
   and cvtFUNC_NAME {kind=x1188, ident=x1189} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1188), 
          ("ident", cvtIDENT x1189)]
   and cvtPACKAGE {name=x1195, body=x1196} = PrettyRep.Rec [("name", cvtUSTRING x1195), 
          ("body", cvtBLOCK x1196)]
   and cvtPROGRAM {packages=ls1203, body=x1207} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1202 => cvtPACKAGE x1202
                                   ) ls1203)), ("body", cvtBLOCK x1207)]
end

