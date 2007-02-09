structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x4) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x4))
     | cvtNAMESPACE (Protected x7) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x7))
     | cvtNAMESPACE (Public x10) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x10))
     | cvtNAMESPACE (Internal x13) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x13))
     | cvtNAMESPACE (UserNamespace x16) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x16))
   and cvtNAME {ns=x19, id=x20} = PrettyRep.Rec [("ns", cvtNAMESPACE x19), 
          ("id", cvtIDENT x20)]
   and cvtMULTINAME {nss=ls31, id=x35} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls27 => 
                                                                                                PrettyRep.List (List.map (fn x26 => 
                                                                                                                                cvtNAMESPACE x26
                                                                                                                         ) ls27)
                                                                                         ) ls31)), 
          ("id", cvtIDENT x35)]
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
   and cvtNUMERIC_MODE {numberType=x53, roundingMode=x54, precision=n55} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x53), ("roundingMode", 
          cvtROUNDING_MODE x54), ("precision", PrettyRep.Int n55)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt68) = PrettyRep.Ctor ("Plus", SOME 
       (case opt68 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x67 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x67))
       ))
     | cvtBINOP (Minus opt75) = PrettyRep.Ctor ("Minus", SOME 
       (case opt75 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x74 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x74))
       ))
     | cvtBINOP (Times opt82) = PrettyRep.Ctor ("Times", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Divide opt89) = PrettyRep.Ctor ("Divide", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
       ))
     | cvtBINOP (Remainder opt96) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt96 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x95 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x95))
       ))
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
     | cvtBINOP (Equals opt113) = PrettyRep.Ctor ("Equals", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (NotEquals opt120) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
       ))
     | cvtBINOP (StrictEquals opt127) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (StrictNotEquals opt134) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (Less opt141) = PrettyRep.Ctor ("Less", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (LessOrEqual opt148) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (Greater opt155) = PrettyRep.Ctor ("Greater", SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (GreaterOrEqual opt162) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x161 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x161))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt172) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x171 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x171))
       ))
     | cvtASSIGNOP (AssignMinus opt179) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x178))
       ))
     | cvtASSIGNOP (AssignTimes opt186) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x185 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x185))
       ))
     | cvtASSIGNOP (AssignDivide opt193) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x192 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x192))
       ))
     | cvtASSIGNOP (AssignRemainder opt200) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x199 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x199))
       ))
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
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x237) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x237))
     | cvtPRAGMA (UseDefaultNamespace x240) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x240))
     | cvtPRAGMA (UseNumber x243) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x243))
     | cvtPRAGMA (UseRounding x246) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x246))
     | cvtPRAGMA (UsePrecision x249) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x249))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x254, name=x255, alias=opt257}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x254), ("name", cvtIDENT x255), 
          ("alias", 
       (case opt257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x256))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x277, fsig=x278, body=x279, fixtures=opt281, inits=ls286}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x277), 
          ("fsig", cvtFUNC_SIG x278), ("body", cvtBLOCK x279), ("fixtures", 
          
       (case opt281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x280 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x280))
       )), ("inits", PrettyRep.List (List.map (fn x285 => cvtSTMT x285
                                              ) ls286))]))
   and cvtDEFN (ClassDefn x303) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x303))
     | cvtDEFN (VariableDefn x306) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x306))
     | cvtDEFN (FunctionDefn x309) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x309))
     | cvtDEFN (InterfaceDefn x312) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x312))
     | cvtDEFN (NamespaceDefn x315) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x315))
     | cvtDEFN (TypeDefn x318) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x318))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls322, params=ls327, inits=ls332, 
          returnType=x336, thisType=opt338, hasBoundThis=b342, hasRest=b343}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x321 => cvtIDENT x321
                                   ) ls322)), ("params", PrettyRep.List (List.map (fn x326 => 
                                                                                         cvtVAR_BINDING x326
                                                                                  ) ls327)), 
          ("inits", PrettyRep.List (List.map (fn x331 => cvtSTMT x331
                                             ) ls332)), ("returnType", cvtTYPE_EXPR x336), 
          ("thisType", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x337))
       )), ("hasBoundThis", PrettyRep.Bool b342), ("hasRest", PrettyRep.Bool b343)]))
   and cvtVAR_BINDING (Binding{pattern=x361, ty=opt363, init=opt368}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x361), ("ty", 
       (case opt363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x362 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x362))
       )), ("init", 
       (case opt368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x367 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x367))
       ))]))
   and cvtTYPE_EXPR (SpecialType x381) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x381))
     | cvtTYPE_EXPR (UnionType ls385) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x384 => 
                                                                                                           cvtTYPE_EXPR x384
                                                                                                    ) ls385)))
     | cvtTYPE_EXPR (ArrayType ls392) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x391 => 
                                                                                                           cvtTYPE_EXPR x391
                                                                                                    ) ls392)))
     | cvtTYPE_EXPR (NominalType{ident=x398}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x398)]))
     | cvtTYPE_EXPR (FunctionType x404) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x404))
     | cvtTYPE_EXPR (ObjectType ls408) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x407 => 
                                                                                                             cvtFIELD_TYPE x407
                                                                                                      ) ls408)))
     | cvtTYPE_EXPR (AppType{base=x414, args=ls416}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x414), ("args", PrettyRep.List (List.map (fn x415 => 
                                                                                                     cvtTYPE_EXPR x415
                                                                                              ) ls416))]))
     | cvtTYPE_EXPR (NullableType{expr=x427, nullable=b428}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x427), ("nullable", PrettyRep.Bool b428)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls438) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x437 => 
                                                                                                    cvtEXPR x437
                                                                                             ) ls438)))
     | cvtSTMT (InitStmt{ns=x444, inits=ls446}) = PrettyRep.Ctor ("InitStmt", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x444), ("inits", PrettyRep.List (List.map (fn x445 => 
                                                                                               cvtEXPR x445
                                                                                        ) ls446))]))
     | cvtSTMT (ForEachStmt x457) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x457))
     | cvtSTMT (ForInStmt x460) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x460))
     | cvtSTMT (ThrowStmt ls464) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x463 => 
                                                                                                      cvtEXPR x463
                                                                                               ) ls464)))
     | cvtSTMT (ReturnStmt ls471) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x470 => 
                                                                                                        cvtEXPR x470
                                                                                                 ) ls471)))
     | cvtSTMT (BreakStmt opt478) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt478 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x477 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x477))
       ))
     | cvtSTMT (ContinueStmt opt485) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x484 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x484))
       ))
     | cvtSTMT (BlockStmt x491) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x491))
     | cvtSTMT (LabeledStmt(x494, x495)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x494, 
          cvtSTMT x495]))
     | cvtSTMT (LetStmt(ls500, x504)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x499 => 
                                                                                                                          cvtVAR_BINDING x499
                                                                                                                   ) ls500), 
          cvtSTMT x504]))
     | cvtSTMT (SuperStmt ls509) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x508 => 
                                                                                                      cvtEXPR x508
                                                                                               ) ls509)))
     | cvtSTMT (WhileStmt x515) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x515))
     | cvtSTMT (DoWhileStmt x518) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x518))
     | cvtSTMT (ForStmt{defns=ls522, fixtures=opt527, init=ls532, cond=ls537, 
          update=ls542, contLabel=opt547, body=x551}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x521 => 
                                                                         cvtVAR_BINDING x521
                                                                  ) ls522)), 
          ("fixtures", 
       (case opt527 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x526 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x526))
       )), ("init", PrettyRep.List (List.map (fn x531 => cvtEXPR x531
                                             ) ls532)), ("cond", PrettyRep.List (List.map (fn x536 => 
                                                                                                 cvtEXPR x536
                                                                                          ) ls537)), 
          ("update", PrettyRep.List (List.map (fn x541 => cvtEXPR x541
                                              ) ls542)), ("contLabel", 
       (case opt547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x546 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x546))
       )), ("body", cvtSTMT x551)]))
     | cvtSTMT (IfStmt{cnd=x569, thn=x570, els=x571}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x569), ("thn", cvtSTMT x570), 
          ("els", cvtSTMT x571)]))
     | cvtSTMT (WithStmt{obj=ls582, ty=x586, body=x587}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x581 => 
                                                                       cvtEXPR x581
                                                                ) ls582)), 
          ("ty", cvtTYPE_EXPR x586), ("body", cvtSTMT x587)]))
     | cvtSTMT (TryStmt{body=x597, catches=ls612, finally=opt617}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x597), ("catches", PrettyRep.List (List.map (fn {bind=x598, 
                                                                                                    fixtures=opt600, 
                                                                                                    body=x604} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x598), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt600 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x599 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x599))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x604)]
                                                                                             ) ls612)), 
          ("finally", 
       (case opt617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x616 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x616))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls631, cases=ls636}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x630 => 
                                                                        cvtEXPR x630
                                                                 ) ls631)), 
          ("cases", PrettyRep.List (List.map (fn x635 => cvtCASE x635
                                             ) ls636))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls648, ty=x652, cases=ls654}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x647 => 
                                                                        cvtEXPR x647
                                                                 ) ls648)), 
          ("ty", cvtTYPE_EXPR x652), ("cases", PrettyRep.List (List.map (fn x653 => 
                                                                               cvtTYPE_CASE x653
                                                                        ) ls654))]))
     | cvtSTMT (Dxns{expr=x667}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x667)]))
   and cvtEXPR (TrinaryExpr(x673, x674, x675, x676)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x673, cvtEXPR x674, cvtEXPR x675, 
          cvtEXPR x676]))
     | cvtEXPR (BinaryExpr(x680, x681, x682)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x680, cvtEXPR x681, cvtEXPR x682]))
     | cvtEXPR (BinaryTypeExpr(x686, x687, x688)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x686, cvtEXPR x687, cvtTYPE_EXPR x688]))
     | cvtEXPR (UnaryExpr(x692, x693)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x692, 
          cvtEXPR x693]))
     | cvtEXPR (TypeExpr x697) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x697))
     | cvtEXPR (NullaryExpr x700) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x700))
     | cvtEXPR (YieldExpr opt708) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt708 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls704 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x703 => 
                                                                                     cvtEXPR x703
                                                                              ) ls704)))
       ))
     | cvtEXPR (SuperExpr opt715) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt715 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x714 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x714))
       ))
     | cvtEXPR (LiteralExpr x721) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x721))
     | cvtEXPR (CallExpr{func=x724, actuals=ls726}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x724), ("actuals", PrettyRep.List (List.map (fn x725 => 
                                                                                                   cvtEXPR x725
                                                                                            ) ls726))]))
     | cvtEXPR (ApplyTypeExpr{expr=x737, actuals=ls739}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x737), ("actuals", PrettyRep.List (List.map (fn x738 => 
                                                                                                   cvtTYPE_EXPR x738
                                                                                            ) ls739))]))
     | cvtEXPR (LetExpr{defs=ls751, body=ls756, fixtures=opt761}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x750 => 
                                                                        cvtVAR_BINDING x750
                                                                 ) ls751)), 
          ("body", PrettyRep.List (List.map (fn x755 => cvtEXPR x755
                                            ) ls756)), ("fixtures", 
       (case opt761 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x760 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x760))
       ))]))
     | cvtEXPR (NewExpr{obj=x774, actuals=ls776}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x774), ("actuals", PrettyRep.List (List.map (fn x775 => 
                                                                                                  cvtEXPR x775
                                                                                           ) ls776))]))
     | cvtEXPR (FunExpr x787) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x787))
     | cvtEXPR (ObjectRef{base=x790, ident=x791}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x790), ("ident", cvtIDENT_EXPR x791)]))
     | cvtEXPR (LexicalRef{ident=x799}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x799)]))
     | cvtEXPR (SetExpr(x805, x806, x807)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x805, 
          cvtPATTERN x806, cvtEXPR x807]))
     | cvtEXPR (AllocTemp(x811, x812)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [cvtIDENT_EXPR x811, 
          cvtEXPR x812]))
     | cvtEXPR (ListExpr ls817) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x816 => 
                                                                                                    cvtEXPR x816
                                                                                             ) ls817)))
     | cvtEXPR (SliceExpr(ls824, ls829, ls834)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x823 => cvtEXPR x823
                                                          ) ls824), PrettyRep.List (List.map (fn x828 => 
                                                                                                    cvtEXPR x828
                                                                                             ) ls829), 
          PrettyRep.List (List.map (fn x833 => cvtEXPR x833
                                   ) ls834)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x841, ident=x842}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x841), ("ident", cvtUSTRING x842)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x850, expr=x851}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x850), ("expr", cvtEXPR x851)]))
     | cvtIDENT_EXPR (AttributeIdentifier x859) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x859))
     | cvtIDENT_EXPR (Identifier{ident=x862, openNamespaces=ls868}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x862), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls864 => PrettyRep.List (List.map (fn x863 => 
                                                                                cvtNAMESPACE x863
                                                                         ) ls864)
                                   ) ls868))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x879) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x879))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x882, typeParams=ls884}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x882), ("typeParams", 
          PrettyRep.List (List.map (fn x883 => cvtTYPE_EXPR x883
                                   ) ls884))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r897) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r897))
     | cvtLITERAL (LiteralBoolean b900) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b900))
     | cvtLITERAL (LiteralString x903) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x903))
     | cvtLITERAL (LiteralArray{exprs=ls907, ty=opt912}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x906 => 
                                                                         cvtEXPR x906
                                                                  ) ls907)), 
          ("ty", 
       (case opt912 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x911 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x911))
       ))]))
     | cvtLITERAL (LiteralXML ls924) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x923 => 
                                                                                                           cvtEXPR x923
                                                                                                    ) ls924)))
     | cvtLITERAL (LiteralNamespace x930) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x930))
     | cvtLITERAL (LiteralObject{expr=ls934, ty=opt939}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x933 => 
                                                                        cvtFIELD x933
                                                                 ) ls934)), 
          ("ty", 
       (case opt939 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x938 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x938))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x950}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x950)]))
   and cvtBLOCK (Block x956) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x956))
   and cvtPATTERN (ObjectPattern ls960) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x959 => cvtFIELD_PATTERN x959
                                         ) ls960)))
     | cvtPATTERN (ArrayPattern ls967) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x966 => 
                                                                                                               cvtPATTERN x966
                                                                                                        ) ls967)))
     | cvtPATTERN (SimplePattern x973) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x973))
     | cvtPATTERN (IdentifierPattern x976) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x976))
   and cvtFIXTURE (NamespaceFixture x979) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x979))
     | cvtFIXTURE (ClassFixture x982) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x982))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x986) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x986))
     | cvtFIXTURE (ValFixture{ty=x989, readOnly=b990, isOverride=b991, init=opt993}) = 
          PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x989), 
          ("readOnly", PrettyRep.Bool b990), ("isOverride", PrettyRep.Bool b991), 
          ("init", 
       (case opt993 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x992 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x992))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1008, getter=opt1010, setter=opt1015}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1008), ("getter", 
       (case opt1010 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1009 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1009))
       )), ("setter", 
       (case opt1015 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1014 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1014))
       ))]))
   and cvtFIELD {kind=x1028, name=x1029, init=x1030} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1028), ("name", cvtIDENT_EXPR x1029), ("init", cvtEXPR x1030)]
   and cvtFIELD_PATTERN {name=x1038, ptrn=x1039} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1038), ("ptrn", cvtPATTERN x1039)]
   and cvtFIELD_TYPE {name=x1045, ty=x1046} = PrettyRep.Rec [("name", cvtIDENT x1045), 
          ("ty", cvtTYPE_EXPR x1046)]
   and cvtTYPED_IDENT {name=x1052, ty=opt1054} = PrettyRep.Rec [("name", cvtIDENT x1052), 
          ("ty", 
       (case opt1054 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1053 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1053))
       ))]
   and cvtATTRIBUTES {ns=x1063, override=b1064, static=b1065, final=b1066, 
          dynamic=b1067, prototype=b1068, native=b1069, rest=b1070} = PrettyRep.Rec [("ns", 
          cvtEXPR x1063), ("override", PrettyRep.Bool b1064), ("static", PrettyRep.Bool b1065), 
          ("final", PrettyRep.Bool b1066), ("dynamic", PrettyRep.Bool b1067), 
          ("prototype", PrettyRep.Bool b1068), ("native", PrettyRep.Bool b1069), 
          ("rest", PrettyRep.Bool b1070)]
   and cvtFUNC_DEFN {kind=x1088, ns=x1089, final=b1090, native=b1091, override=b1092, 
          prototype=b1093, static=b1094, func=x1095} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1088), ("ns", cvtEXPR x1089), ("final", PrettyRep.Bool b1090), 
          ("native", PrettyRep.Bool b1091), ("override", PrettyRep.Bool b1092), 
          ("prototype", PrettyRep.Bool b1093), ("static", PrettyRep.Bool b1094), 
          ("func", cvtFUNC x1095)]
   and cvtVAR_DEFN {kind=x1113, ns=x1114, static=b1115, prototype=b1116, bindings=ls1118} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1113), ("ns", cvtEXPR x1114), 
          ("static", PrettyRep.Bool b1115), ("prototype", PrettyRep.Bool b1116), 
          ("bindings", PrettyRep.List (List.map (fn x1117 => cvtVAR_BINDING x1117
                                                ) ls1118))]
   and cvtFIXTURES ls1136 = PrettyRep.List (List.map (fn (x1133, x1134) => 
                                                            PrettyRep.Tuple [cvtNAME x1133, 
                                                            cvtFIXTURE x1134]
                                                     ) ls1136)
   and cvtNAMESPACE_DEFN {ident=x1140, ns=x1141, init=opt1143} = PrettyRep.Rec [("ident", 
          cvtIDENT x1140), ("ns", cvtEXPR x1141), ("init", 
       (case opt1143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1142 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1142))
       ))]
   and cvtCLASS_DEFN {ident=x1154, ns=x1155, nonnullable=b1156, dynamic=b1157, 
          final=b1158, params=ls1160, extends=opt1165, implements=ls1170, classFixtures=opt1175, 
          instanceFixtures=opt1180, body=x1184, protoVars=ls1186, protoMethods=ls1191, 
          instanceVars=ls1196, instanceMethods=ls1201, vars=ls1206, methods=ls1211, 
          constructor=opt1216, initializer=ls1221} = PrettyRep.Rec [("ident", 
          cvtIDENT x1154), ("ns", cvtEXPR x1155), ("nonnullable", PrettyRep.Bool b1156), 
          ("dynamic", PrettyRep.Bool b1157), ("final", PrettyRep.Bool b1158), 
          ("params", PrettyRep.List (List.map (fn x1159 => cvtIDENT x1159
                                              ) ls1160)), ("extends", 
       (case opt1165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1164 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1164))
       )), ("implements", PrettyRep.List (List.map (fn x1169 => cvtIDENT_EXPR x1169
                                                   ) ls1170)), ("classFixtures", 
          
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1174))
       )), ("instanceFixtures", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1179))
       )), ("body", cvtBLOCK x1184), ("protoVars", PrettyRep.List (List.map (fn x1185 => 
                                                                                   cvtVAR_DEFN x1185
                                                                            ) ls1186)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1190 => cvtFUNC_DEFN x1190
                                                    ) ls1191)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1195 => cvtVAR_DEFN x1195
                                   ) ls1196)), ("instanceMethods", PrettyRep.List (List.map (fn x1200 => 
                                                                                                   cvtFUNC_DEFN x1200
                                                                                            ) ls1201)), 
          ("vars", PrettyRep.List (List.map (fn x1205 => cvtVAR_DEFN x1205
                                            ) ls1206)), ("methods", PrettyRep.List (List.map (fn x1210 => 
                                                                                                    cvtFUNC_DEFN x1210
                                                                                             ) ls1211)), 
          ("constructor", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1215))
       )), ("initializer", PrettyRep.List (List.map (fn x1220 => cvtSTMT x1220
                                                    ) ls1221))]
   and cvtINTERFACE_DEFN {ident=x1264, ns=x1265, nonnullable=b1266, params=ls1268, 
          extends=ls1273, body=x1277} = PrettyRep.Rec [("ident", cvtIDENT x1264), 
          ("ns", cvtEXPR x1265), ("nonnullable", PrettyRep.Bool b1266), ("params", 
          PrettyRep.List (List.map (fn x1267 => cvtIDENT x1267
                                   ) ls1268)), ("extends", PrettyRep.List (List.map (fn x1272 => 
                                                                                           cvtIDENT_EXPR x1272
                                                                                    ) ls1273)), 
          ("body", cvtBLOCK x1277)]
   and cvtTYPE_DEFN {ident=x1291, ns=x1292, init=x1293} = PrettyRep.Rec [("ident", 
          cvtIDENT x1291), ("ns", cvtEXPR x1292), ("init", cvtTYPE_EXPR x1293)]
   and cvtFOR_ENUM_STMT {ptrn=opt1302, obj=ls1307, defns=ls1312, fixtures=opt1317, 
          contLabel=opt1322, body=x1326} = PrettyRep.Rec [("ptrn", 
       (case opt1302 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1301 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1301))
       )), ("obj", PrettyRep.List (List.map (fn x1306 => cvtEXPR x1306
                                            ) ls1307)), ("defns", PrettyRep.List (List.map (fn x1311 => 
                                                                                                  cvtVAR_BINDING x1311
                                                                                           ) ls1312)), 
          ("fixtures", 
       (case opt1317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1316 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1316))
       )), ("contLabel", 
       (case opt1322 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1321 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1321))
       )), ("body", cvtSTMT x1326)]
   and cvtWHILE_STMT {cond=x1340, body=x1341, contLabel=opt1343} = PrettyRep.Rec [("cond", 
          cvtEXPR x1340), ("body", cvtSTMT x1341), ("contLabel", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1342))
       ))]
   and cvtDIRECTIVES {pragmas=ls1355, defns=ls1360, stmts=ls1365, fixtures=opt1370} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1354 => 
                                                                     cvtPRAGMA x1354
                                                              ) ls1355)), ("defns", 
          PrettyRep.List (List.map (fn x1359 => cvtDEFN x1359
                                   ) ls1360)), ("stmts", PrettyRep.List (List.map (fn x1364 => 
                                                                                         cvtSTMT x1364
                                                                                  ) ls1365)), 
          ("fixtures", 
       (case opt1370 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1369 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1369))
       ))]
   and cvtBINDINGS {b=ls1384, i=ls1389} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1383 => 
                                                                                               cvtVAR_BINDING x1383
                                                                                        ) ls1384)), 
          ("i", PrettyRep.List (List.map (fn x1388 => cvtEXPR x1388
                                         ) ls1389))]
   and cvtCASE {label=opt1403, body=x1407} = PrettyRep.Rec [("label", 
       (case opt1403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1399 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1398 => 
                                                                                      cvtEXPR x1398
                                                                               ) ls1399)))
       )), ("body", cvtBLOCK x1407)]
   and cvtTYPE_CASE {ptrn=opt1414, body=x1418} = PrettyRep.Rec [("ptrn", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1413))
       )), ("body", cvtBLOCK x1418)]
   and cvtFUNC_NAME {kind=x1424, ident=x1425} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1424), 
          ("ident", cvtIDENT x1425)]
   and cvtPACKAGE {name=x1431, body=x1432} = PrettyRep.Rec [("name", cvtUSTRING x1431), 
          ("body", cvtBLOCK x1432)]
   and cvtPROGRAM {packages=ls1439, fixtures=opt1444, body=x1448} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1438 => cvtPACKAGE x1438
                                   ) ls1439)), ("fixtures", 
       (case opt1444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1443 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1443))
       )), ("body", cvtBLOCK x1448)]
end

