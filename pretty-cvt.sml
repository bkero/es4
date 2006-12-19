structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
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
   and cvtNAMESPACE (Private) = PrettyRep.Ctor ("Private", NONE)
     | cvtNAMESPACE (Protected) = PrettyRep.Ctor ("Protected", NONE)
     | cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Public x80) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x80))
     | cvtNAMESPACE (Internal x83) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x83))
     | cvtNAMESPACE (UserDefined x86) = PrettyRep.Ctor ("UserDefined", SOME (cvtIDENT x86))
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x93) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x93))
     | cvtPRAGMA (UseDefaultNamespace x96) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x96))
     | cvtPRAGMA (UseNumber x99) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x99))
     | cvtPRAGMA (UseRounding x102) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x102))
     | cvtPRAGMA (UsePrecision x105) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x105))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x110, name=x111, alias=opt113}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x110), ("name", cvtIDENT x111), 
          ("alias", 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x112))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x133, fsig=x134, body=x135}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x133), ("fsig", cvtFUNC_SIG x134), 
          ("body", cvtBLOCK x135)]))
   and cvtDEFN (ClassDefn x145) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x145))
     | cvtDEFN (VariableDefn ls149) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x148 => 
                                                                                                            cvtVAR_BINDING x148
                                                                                                     ) ls149)))
     | cvtDEFN (FunctionDefn x155) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x155))
     | cvtDEFN (InterfaceDefn x158) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x158))
     | cvtDEFN (NamespaceDefn{attrs=x161, ident=x162, init=opt164}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x161), ("ident", cvtIDENT x162), 
          ("init", 
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x163))
       ))]))
     | cvtDEFN (TypeDefn{attrs=x177, ident=x178, init=x179}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x177), ("ident", cvtIDENT x178), 
          ("init", cvtTYPE_EXPR x179)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls190, params=ls195, inits=opt200, 
          returnType=x204, thisType=opt206, hasBoundThis=b210, hasRest=b211}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x189 => cvtIDENT x189
                                   ) ls190)), ("params", PrettyRep.List (List.map (fn x194 => 
                                                                                         cvtVAR_BINDING x194
                                                                                  ) ls195)), 
          ("inits", 
       (case opt200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x199 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x199))
       )), ("returnType", cvtTYPE_EXPR x204), ("thisType", 
       (case opt206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x205 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x205))
       )), ("hasBoundThis", PrettyRep.Bool b210), ("hasRest", PrettyRep.Bool b211)]))
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
     | cvtIDENT_EXPR (Identifier{ident=x744, openNamespaces=r750}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x744), ("openNamespaces", 
          
       (case ! r750 of
         ls746 => PrettyRep.Ref (PrettyRep.List (List.map (fn x745 => cvtNAMESPACE x745
                                                          ) ls746))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x761) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x761))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x764, typeParams=ls766}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x764), ("typeParams", 
          PrettyRep.List (List.map (fn x765 => cvtTYPE_EXPR x765
                                   ) ls766))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r779) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r779))
     | cvtLITERAL (LiteralBoolean b782) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b782))
     | cvtLITERAL (LiteralString x785) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x785))
     | cvtLITERAL (LiteralArray{exprs=ls789, ty=opt794}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x788 => 
                                                                         cvtEXPR x788
                                                                  ) ls789)), 
          ("ty", 
       (case opt794 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x793 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x793))
       ))]))
     | cvtLITERAL (LiteralXML ls806) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x805 => 
                                                                                                           cvtEXPR x805
                                                                                                    ) ls806)))
     | cvtLITERAL (LiteralNamespace x812) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x812))
     | cvtLITERAL (LiteralObject{expr=ls816, ty=opt821}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x815 => 
                                                                        cvtFIELD x815
                                                                 ) ls816)), 
          ("ty", 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x820))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x832}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x832)]))
   and cvtBLOCK (Block{pragmas=ls839, defns=ls844, stmts=ls849}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x838 => 
                                                                           cvtPRAGMA x838
                                                                    ) ls839)), 
          ("defns", PrettyRep.List (List.map (fn x843 => cvtDEFN x843
                                             ) ls844)), ("stmts", PrettyRep.List (List.map (fn x848 => 
                                                                                                  cvtSTMT x848
                                                                                           ) ls849))]))
   and cvtPATTERN (ObjectPattern ls869) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x862, ptrn=x863} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x862), ("ptrn", 
                                                cvtPATTERN x863)]
                                         ) ls869)))
     | cvtPATTERN (ArrayPattern ls876) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x875 => 
                                                                                                               cvtPATTERN x875
                                                                                                        ) ls876)))
     | cvtPATTERN (SimplePattern x882) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x882))
     | cvtPATTERN (IdentifierPattern x885) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x885))
   and cvtFIELD {kind=x888, name=x889, init=x890} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x888), ("name", cvtIDENT_EXPR x889), ("init", cvtEXPR x890)]
   and cvtFIELD_TYPE {name=x898, ty=x899} = PrettyRep.Rec [("name", cvtIDENT_EXPR x898), 
          ("ty", cvtTYPE_EXPR x899)]
   and cvtTYPED_IDENT {name=x905, ty=opt907} = PrettyRep.Rec [("name", cvtIDENT x905), 
          ("ty", 
       (case opt907 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x906 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x906))
       ))]
   and cvtFUNC_DEFN {attrs=x916, kind=x917, func=x918} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x916), ("kind", cvtVAR_DEFN_TAG x917), ("func", cvtFUNC x918)]
   and cvtCLASS_DEFN {name=x926, nonnullable=b927, attrs=x928, params=ls930, 
          extends=opt935, implements=ls940, body=x944, instanceVars=ls946, 
          instanceMethods=ls951, vars=ls956, methods=ls961, constructor=opt966, 
          initializer=ls971} = PrettyRep.Rec [("name", cvtIDENT x926), ("nonnullable", 
          PrettyRep.Bool b927), ("attrs", cvtATTRIBUTES x928), ("params", PrettyRep.List (List.map (fn x929 => 
                                                                                                          cvtIDENT x929
                                                                                                   ) ls930)), 
          ("extends", 
       (case opt935 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x934 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x934))
       )), ("implements", PrettyRep.List (List.map (fn x939 => cvtIDENT_EXPR x939
                                                   ) ls940)), ("body", cvtBLOCK x944), 
          ("instanceVars", PrettyRep.List (List.map (fn x945 => cvtVAR_BINDING x945
                                                    ) ls946)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x950 => cvtFUNC x950
                                   ) ls951)), ("vars", PrettyRep.List (List.map (fn x955 => 
                                                                                       cvtVAR_BINDING x955
                                                                                ) ls956)), 
          ("methods", PrettyRep.List (List.map (fn x960 => cvtFUNC x960
                                               ) ls961)), ("constructor", 
       (case opt966 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x965 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x965))
       )), ("initializer", PrettyRep.List (List.map (fn x970 => cvtSTMT x970
                                                    ) ls971))]
   and cvtINTERFACE_DEFN {name=x1002, nonnullable=b1003, attrs=x1004, params=ls1006, 
          extends=ls1011, body=x1015} = PrettyRep.Rec [("name", cvtIDENT x1002), 
          ("nonnullable", PrettyRep.Bool b1003), ("attrs", cvtATTRIBUTES x1004), 
          ("params", PrettyRep.List (List.map (fn x1005 => cvtIDENT x1005
                                              ) ls1006)), ("extends", PrettyRep.List (List.map (fn x1010 => 
                                                                                                      cvtIDENT_EXPR x1010
                                                                                               ) ls1011)), 
          ("body", cvtBLOCK x1015)]
   and cvtFOR_ENUM_STMT {ptrn=opt1030, obj=ls1035, defns=ls1040, contLabel=opt1045, 
          body=x1049} = PrettyRep.Rec [("ptrn", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1029))
       )), ("obj", PrettyRep.List (List.map (fn x1034 => cvtEXPR x1034
                                            ) ls1035)), ("defns", PrettyRep.List (List.map (fn x1039 => 
                                                                                                  cvtVAR_BINDING x1039
                                                                                           ) ls1040)), 
          ("contLabel", 
       (case opt1045 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1044 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1044))
       )), ("body", cvtSTMT x1049)]
   and cvtWHILE_STMT {cond=x1061, body=x1062, contLabel=opt1064} = PrettyRep.Rec [("cond", 
          cvtEXPR x1061), ("body", cvtSTMT x1062), ("contLabel", 
       (case opt1064 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1063 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1063))
       ))]
   and cvtDIRECTIVES {pragmas=ls1076, defns=ls1081, stmts=ls1086} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1075 => cvtPRAGMA x1075
                                   ) ls1076)), ("defns", PrettyRep.List (List.map (fn x1080 => 
                                                                                         cvtDEFN x1080
                                                                                  ) ls1081)), 
          ("stmts", PrettyRep.List (List.map (fn x1085 => cvtSTMT x1085
                                             ) ls1086))]
   and cvtBINDINGS {defns=ls1098, inits=ls1103} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1097 => cvtVAR_BINDING x1097
                                   ) ls1098)), ("inits", PrettyRep.List (List.map (fn x1102 => 
                                                                                         cvtEXPR x1102
                                                                                  ) ls1103))]
   and cvtCASE {label=opt1117, stmts=x1121} = PrettyRep.Rec [("label", 
       (case opt1117 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1113 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1112 => 
                                                                                      cvtEXPR x1112
                                                                               ) ls1113)))
       )), ("stmts", cvtDIRECTIVES x1121)]
   and cvtTYPE_CASE {ptrn=opt1128, body=x1132} = PrettyRep.Rec [("ptrn", 
       (case opt1128 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1127 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1127))
       )), ("body", cvtBLOCK x1132)]
   and cvtFUNC_NAME {kind=x1138, ident=x1139} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1138), 
          ("ident", cvtIDENT x1139)]
   and cvtPACKAGE {name=x1145, body=x1146} = PrettyRep.Rec [("name", cvtUSTRING x1145), 
          ("body", cvtBLOCK x1146)]
   and cvtPROGRAM {packages=ls1153, body=x1157} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1152 => cvtPACKAGE x1152
                                   ) ls1153)), ("body", cvtBLOCK x1157)]
end

