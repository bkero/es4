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
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x235) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x235))
     | cvtPRAGMA (UseDefaultNamespace x238) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x238))
     | cvtPRAGMA (UseNumber x241) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x241))
     | cvtPRAGMA (UseRounding x244) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x244))
     | cvtPRAGMA (UsePrecision x247) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x247))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x252, name=x253, alias=opt255}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x252), ("name", cvtIDENT x253), 
          ("alias", 
       (case opt255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x254 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x254))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x275, fsig=x276, body=x277, fixtures=opt279, inits=ls284}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x275), 
          ("fsig", cvtFUNC_SIG x276), ("body", cvtBLOCK x277), ("fixtures", 
          
       (case opt279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x278 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x278))
       )), ("inits", PrettyRep.List (List.map (fn x283 => cvtSTMT x283
                                              ) ls284))]))
   and cvtDEFN (ClassDefn x301) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x301))
     | cvtDEFN (VariableDefn x304) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x304))
     | cvtDEFN (FunctionDefn x307) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x307))
     | cvtDEFN (InterfaceDefn x310) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x310))
     | cvtDEFN (NamespaceDefn x313) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x313))
     | cvtDEFN (TypeDefn x316) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x316))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls320, params=ls325, inits=ls330, 
          returnType=x334, thisType=opt336, hasRest=b340}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x319 => 
                                                                              cvtIDENT x319
                                                                       ) ls320)), 
          ("params", PrettyRep.List (List.map (fn x324 => cvtVAR_BINDING x324
                                              ) ls325)), ("inits", PrettyRep.List (List.map (fn x329 => 
                                                                                                   cvtSTMT x329
                                                                                            ) ls330)), 
          ("returnType", cvtTYPE_EXPR x334), ("thisType", 
       (case opt336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x335 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x335))
       )), ("hasRest", PrettyRep.Bool b340)]))
   and cvtVAR_BINDING (Binding{pattern=x356, ty=opt358, init=opt363}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x356), ("ty", 
       (case opt358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x357 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x357))
       )), ("init", 
       (case opt363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x362 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x362))
       ))]))
   and cvtTYPE_EXPR (SpecialType x376) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x376))
     | cvtTYPE_EXPR (UnionType ls380) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x379 => 
                                                                                                           cvtTYPE_EXPR x379
                                                                                                    ) ls380)))
     | cvtTYPE_EXPR (ArrayType ls387) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x386 => 
                                                                                                           cvtTYPE_EXPR x386
                                                                                                    ) ls387)))
     | cvtTYPE_EXPR (NominalType{ident=x393}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x393)]))
     | cvtTYPE_EXPR (FunctionType x399) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x399))
     | cvtTYPE_EXPR (ObjectType ls403) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x402 => 
                                                                                                             cvtFIELD_TYPE x402
                                                                                                      ) ls403)))
     | cvtTYPE_EXPR (AppType{base=x409, args=ls411}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x409), ("args", PrettyRep.List (List.map (fn x410 => 
                                                                                                     cvtTYPE_EXPR x410
                                                                                              ) ls411))]))
     | cvtTYPE_EXPR (NullableType{expr=x422, nullable=b423}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x422), ("nullable", PrettyRep.Bool b423)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls433) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x432 => 
                                                                                                    cvtEXPR x432
                                                                                             ) ls433)))
     | cvtSTMT (InitStmt{kind=x439, ns=x440, prototype=b441, static=b442, inits=ls444}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x439), 
          ("ns", cvtEXPR x440), ("prototype", PrettyRep.Bool b441), ("static", 
          PrettyRep.Bool b442), ("inits", PrettyRep.List (List.map (fn x443 => 
                                                                          cvtEXPR x443
                                                                   ) ls444))]))
     | cvtSTMT (ForEachStmt x461) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x461))
     | cvtSTMT (ForInStmt x464) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x464))
     | cvtSTMT (ThrowStmt ls468) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x467 => 
                                                                                                      cvtEXPR x467
                                                                                               ) ls468)))
     | cvtSTMT (ReturnStmt ls475) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x474 => 
                                                                                                        cvtEXPR x474
                                                                                                 ) ls475)))
     | cvtSTMT (BreakStmt opt482) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt482 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x481 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x481))
       ))
     | cvtSTMT (ContinueStmt opt489) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt489 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x488 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x488))
       ))
     | cvtSTMT (BlockStmt x495) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x495))
     | cvtSTMT (LabeledStmt(x498, x499)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x498, 
          cvtSTMT x499]))
     | cvtSTMT (LetStmt(ls504, x508)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x503 => 
                                                                                                                          cvtVAR_BINDING x503
                                                                                                                   ) ls504), 
          cvtSTMT x508]))
     | cvtSTMT (SuperStmt ls513) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x512 => 
                                                                                                      cvtEXPR x512
                                                                                               ) ls513)))
     | cvtSTMT (WhileStmt x519) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x519))
     | cvtSTMT (DoWhileStmt x522) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x522))
     | cvtSTMT (ForStmt{defns=ls526, fixtures=opt531, init=ls536, cond=ls541, 
          update=ls546, contLabel=opt551, body=x555}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x525 => 
                                                                         cvtVAR_BINDING x525
                                                                  ) ls526)), 
          ("fixtures", 
       (case opt531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x530 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x530))
       )), ("init", PrettyRep.List (List.map (fn x535 => cvtEXPR x535
                                             ) ls536)), ("cond", PrettyRep.List (List.map (fn x540 => 
                                                                                                 cvtEXPR x540
                                                                                          ) ls541)), 
          ("update", PrettyRep.List (List.map (fn x545 => cvtEXPR x545
                                              ) ls546)), ("contLabel", 
       (case opt551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x550 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x550))
       )), ("body", cvtSTMT x555)]))
     | cvtSTMT (IfStmt{cnd=x573, thn=x574, els=x575}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x573), ("thn", cvtSTMT x574), 
          ("els", cvtSTMT x575)]))
     | cvtSTMT (WithStmt{obj=ls586, ty=x590, body=x591}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x585 => 
                                                                       cvtEXPR x585
                                                                ) ls586)), 
          ("ty", cvtTYPE_EXPR x590), ("body", cvtSTMT x591)]))
     | cvtSTMT (TryStmt{body=x601, catches=ls616, finally=opt621}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x601), ("catches", PrettyRep.List (List.map (fn {bind=x602, 
                                                                                                    fixtures=opt604, 
                                                                                                    body=x608} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x602), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt604 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x603 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x603))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x608)]
                                                                                             ) ls616)), 
          ("finally", 
       (case opt621 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x620 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x620))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls635, cases=ls640}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x634 => 
                                                                        cvtEXPR x634
                                                                 ) ls635)), 
          ("cases", PrettyRep.List (List.map (fn x639 => cvtCASE x639
                                             ) ls640))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls652, ty=x656, cases=ls658}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x651 => 
                                                                        cvtEXPR x651
                                                                 ) ls652)), 
          ("ty", cvtTYPE_EXPR x656), ("cases", PrettyRep.List (List.map (fn x657 => 
                                                                               cvtTYPE_CASE x657
                                                                        ) ls658))]))
     | cvtSTMT (Dxns{expr=x671}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x671)]))
   and cvtEXPR (TrinaryExpr(x677, x678, x679, x680)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x677, cvtEXPR x678, cvtEXPR x679, 
          cvtEXPR x680]))
     | cvtEXPR (BinaryExpr(x684, x685, x686)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x684, cvtEXPR x685, cvtEXPR x686]))
     | cvtEXPR (BinaryTypeExpr(x690, x691, x692)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x690, cvtEXPR x691, cvtTYPE_EXPR x692]))
     | cvtEXPR (UnaryExpr(x696, x697)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x696, 
          cvtEXPR x697]))
     | cvtEXPR (TypeExpr x701) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x701))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt710) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls706 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x705 => 
                                                                                     cvtEXPR x705
                                                                              ) ls706)))
       ))
     | cvtEXPR (SuperExpr opt717) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x716 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x716))
       ))
     | cvtEXPR (LiteralExpr x723) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x723))
     | cvtEXPR (CallExpr{func=x726, actuals=ls728}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x726), ("actuals", PrettyRep.List (List.map (fn x727 => 
                                                                                                   cvtEXPR x727
                                                                                            ) ls728))]))
     | cvtEXPR (ApplyTypeExpr{expr=x739, actuals=ls741}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x739), ("actuals", PrettyRep.List (List.map (fn x740 => 
                                                                                                   cvtTYPE_EXPR x740
                                                                                            ) ls741))]))
     | cvtEXPR (LetExpr{defs=ls753, body=ls758, fixtures=opt763}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x752 => 
                                                                        cvtVAR_BINDING x752
                                                                 ) ls753)), 
          ("body", PrettyRep.List (List.map (fn x757 => cvtEXPR x757
                                            ) ls758)), ("fixtures", 
       (case opt763 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x762 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x762))
       ))]))
     | cvtEXPR (NewExpr{obj=x776, actuals=ls778}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x776), ("actuals", PrettyRep.List (List.map (fn x777 => 
                                                                                                  cvtEXPR x777
                                                                                           ) ls778))]))
     | cvtEXPR (FunExpr x789) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x789))
     | cvtEXPR (ObjectRef{base=x792, ident=x793}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x792), ("ident", cvtIDENT_EXPR x793)]))
     | cvtEXPR (LexicalRef{ident=x801}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x801)]))
     | cvtEXPR (SetExpr(x807, x808, x809)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x807, 
          cvtPATTERN x808, cvtEXPR x809]))
     | cvtEXPR (AllocTemp(n813, x814)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n813, 
          cvtEXPR x814]))
     | cvtEXPR (KillTemp n818) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n818))
     | cvtEXPR (GetTemp n821) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n821))
     | cvtEXPR (ListExpr ls825) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x824 => 
                                                                                                    cvtEXPR x824
                                                                                             ) ls825)))
     | cvtEXPR (SliceExpr(ls832, ls837, ls842)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x831 => cvtEXPR x831
                                                          ) ls832), PrettyRep.List (List.map (fn x836 => 
                                                                                                    cvtEXPR x836
                                                                                             ) ls837), 
          PrettyRep.List (List.map (fn x841 => cvtEXPR x841
                                   ) ls842)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x849, ident=x850}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x849), ("ident", cvtUSTRING x850)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x858, expr=x859}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x858), ("expr", cvtEXPR x859)]))
     | cvtIDENT_EXPR (AttributeIdentifier x867) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x867))
     | cvtIDENT_EXPR (Identifier{ident=x870, openNamespaces=ls876}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x870), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls872 => PrettyRep.List (List.map (fn x871 => 
                                                                                cvtNAMESPACE x871
                                                                         ) ls872)
                                   ) ls876))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x887) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x887))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x890, typeParams=ls892}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x890), ("typeParams", 
          PrettyRep.List (List.map (fn x891 => cvtTYPE_EXPR x891
                                   ) ls892))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r905) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r905))
     | cvtLITERAL (LiteralBoolean b908) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b908))
     | cvtLITERAL (LiteralString x911) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x911))
     | cvtLITERAL (LiteralArray{exprs=ls915, ty=opt920}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x914 => 
                                                                         cvtEXPR x914
                                                                  ) ls915)), 
          ("ty", 
       (case opt920 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x919 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x919))
       ))]))
     | cvtLITERAL (LiteralXML ls932) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x931 => 
                                                                                                           cvtEXPR x931
                                                                                                    ) ls932)))
     | cvtLITERAL (LiteralNamespace x938) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x938))
     | cvtLITERAL (LiteralObject{expr=ls942, ty=opt947}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x941 => 
                                                                        cvtFIELD x941
                                                                 ) ls942)), 
          ("ty", 
       (case opt947 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x946 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x946))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x958}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x958)]))
   and cvtBLOCK (Block x964) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x964))
   and cvtPATTERN (ObjectPattern ls968) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x967 => cvtFIELD_PATTERN x967
                                         ) ls968)))
     | cvtPATTERN (ArrayPattern ls975) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x974 => 
                                                                                                               cvtPATTERN x974
                                                                                                        ) ls975)))
     | cvtPATTERN (SimplePattern x981) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x981))
     | cvtPATTERN (IdentifierPattern x984) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x984))
   and cvtFIXTURE (NamespaceFixture x987) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x987))
     | cvtFIXTURE (ClassFixture{extends=opt991, implements=ls996, classFixtures=x1000, 
          instanceFixtures=x1001}) = PrettyRep.Ctor ("ClassFixture", SOME (PrettyRep.Rec [("extends", 
          
       (case opt991 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x990 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x990))
       )), ("implements", PrettyRep.List (List.map (fn x995 => cvtNAME x995
                                                   ) ls996)), ("classFixtures", 
          cvtFIXTURES x1000), ("instanceFixtures", cvtFIXTURES x1001)]))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1014) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1014))
     | cvtFIXTURE (ValFixture{ty=x1017, readOnly=b1018, isOverride=b1019, isFinal=b1020, 
          init=opt1022}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1017), ("readOnly", PrettyRep.Bool b1018), ("isOverride", 
          PrettyRep.Bool b1019), ("isFinal", PrettyRep.Bool b1020), ("init", 
          
       (case opt1022 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1021 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1021))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1039, getter=opt1041, setter=opt1046}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1039), ("getter", 
       (case opt1041 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1040 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1040))
       )), ("setter", 
       (case opt1046 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1045 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1045))
       ))]))
   and cvtFIELD {kind=x1059, name=x1060, init=x1061} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1059), ("name", cvtIDENT_EXPR x1060), ("init", cvtEXPR x1061)]
   and cvtFIELD_PATTERN {name=x1069, ptrn=x1070} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1069), ("ptrn", cvtPATTERN x1070)]
   and cvtFIELD_TYPE {name=x1076, ty=x1077} = PrettyRep.Rec [("name", cvtIDENT x1076), 
          ("ty", cvtTYPE_EXPR x1077)]
   and cvtTYPED_IDENT {name=x1083, ty=opt1085} = PrettyRep.Rec [("name", cvtIDENT x1083), 
          ("ty", 
       (case opt1085 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1084 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1084))
       ))]
   and cvtATTRIBUTES {ns=x1094, override=b1095, static=b1096, final=b1097, 
          dynamic=b1098, prototype=b1099, native=b1100, rest=b1101} = PrettyRep.Rec [("ns", 
          cvtEXPR x1094), ("override", PrettyRep.Bool b1095), ("static", PrettyRep.Bool b1096), 
          ("final", PrettyRep.Bool b1097), ("dynamic", PrettyRep.Bool b1098), 
          ("prototype", PrettyRep.Bool b1099), ("native", PrettyRep.Bool b1100), 
          ("rest", PrettyRep.Bool b1101)]
   and cvtFUNC_DEFN {kind=x1119, ns=x1120, final=b1121, native=b1122, override=b1123, 
          prototype=b1124, static=b1125, func=x1126} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1119), ("ns", cvtEXPR x1120), ("final", PrettyRep.Bool b1121), 
          ("native", PrettyRep.Bool b1122), ("override", PrettyRep.Bool b1123), 
          ("prototype", PrettyRep.Bool b1124), ("static", PrettyRep.Bool b1125), 
          ("func", cvtFUNC x1126)]
   and cvtVAR_DEFN {kind=x1144, ns=x1145, static=b1146, prototype=b1147, bindings=ls1149} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1144), ("ns", cvtEXPR x1145), 
          ("static", PrettyRep.Bool b1146), ("prototype", PrettyRep.Bool b1147), 
          ("bindings", PrettyRep.List (List.map (fn x1148 => cvtVAR_BINDING x1148
                                                ) ls1149))]
   and cvtFIXTURES ls1167 = PrettyRep.List (List.map (fn (x1164, x1165) => 
                                                            PrettyRep.Tuple [cvtNAME x1164, 
                                                            cvtFIXTURE x1165]
                                                     ) ls1167)
   and cvtNAMESPACE_DEFN {ident=x1171, ns=x1172, init=opt1174} = PrettyRep.Rec [("ident", 
          cvtIDENT x1171), ("ns", cvtEXPR x1172), ("init", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1173))
       ))]
   and cvtCLASS_DEFN {ident=x1185, ns=x1186, nonnullable=b1187, dynamic=b1188, 
          final=b1189, params=ls1191, extends=opt1196, implements=ls1201, body=x1205} = 
          PrettyRep.Rec [("ident", cvtIDENT x1185), ("ns", cvtEXPR x1186), 
          ("nonnullable", PrettyRep.Bool b1187), ("dynamic", PrettyRep.Bool b1188), 
          ("final", PrettyRep.Bool b1189), ("params", PrettyRep.List (List.map (fn x1190 => 
                                                                                      cvtIDENT x1190
                                                                               ) ls1191)), 
          ("extends", 
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1195))
       )), ("implements", PrettyRep.List (List.map (fn x1200 => cvtIDENT_EXPR x1200
                                                   ) ls1201)), ("body", cvtBLOCK x1205)]
   and cvtINTERFACE_DEFN {ident=x1225, ns=x1226, nonnullable=b1227, params=ls1229, 
          extends=ls1234, body=x1238} = PrettyRep.Rec [("ident", cvtIDENT x1225), 
          ("ns", cvtEXPR x1226), ("nonnullable", PrettyRep.Bool b1227), ("params", 
          PrettyRep.List (List.map (fn x1228 => cvtIDENT x1228
                                   ) ls1229)), ("extends", PrettyRep.List (List.map (fn x1233 => 
                                                                                           cvtIDENT_EXPR x1233
                                                                                    ) ls1234)), 
          ("body", cvtBLOCK x1238)]
   and cvtTYPE_DEFN {ident=x1252, ns=x1253, init=x1254} = PrettyRep.Rec [("ident", 
          cvtIDENT x1252), ("ns", cvtEXPR x1253), ("init", cvtTYPE_EXPR x1254)]
   and cvtFOR_ENUM_STMT {ptrn=opt1263, obj=ls1268, defns=ls1273, fixtures=opt1278, 
          contLabel=opt1283, body=x1287} = PrettyRep.Rec [("ptrn", 
       (case opt1263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1262 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1262))
       )), ("obj", PrettyRep.List (List.map (fn x1267 => cvtEXPR x1267
                                            ) ls1268)), ("defns", PrettyRep.List (List.map (fn x1272 => 
                                                                                                  cvtVAR_BINDING x1272
                                                                                           ) ls1273)), 
          ("fixtures", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1277))
       )), ("contLabel", 
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1282))
       )), ("body", cvtSTMT x1287)]
   and cvtWHILE_STMT {cond=x1301, body=x1302, contLabel=opt1304} = PrettyRep.Rec [("cond", 
          cvtEXPR x1301), ("body", cvtSTMT x1302), ("contLabel", 
       (case opt1304 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1303 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1303))
       ))]
   and cvtDIRECTIVES {pragmas=ls1316, defns=ls1321, stmts=ls1326, fixtures=opt1331, 
          inits=opt1340} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1315 => 
                                                                                      cvtPRAGMA x1315
                                                                               ) ls1316)), 
          ("defns", PrettyRep.List (List.map (fn x1320 => cvtDEFN x1320
                                             ) ls1321)), ("stmts", PrettyRep.List (List.map (fn x1325 => 
                                                                                                   cvtSTMT x1325
                                                                                            ) ls1326)), 
          ("fixtures", 
       (case opt1331 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1330 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1330))
       )), ("inits", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1336 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1335 => 
                                                                                      cvtSTMT x1335
                                                                               ) ls1336)))
       ))]
   and cvtBINDINGS {b=ls1356, i=ls1361} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1355 => 
                                                                                               cvtVAR_BINDING x1355
                                                                                        ) ls1356)), 
          ("i", PrettyRep.List (List.map (fn x1360 => cvtEXPR x1360
                                         ) ls1361))]
   and cvtCASE {label=opt1375, body=x1379} = PrettyRep.Rec [("label", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1371 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1370 => 
                                                                                      cvtEXPR x1370
                                                                               ) ls1371)))
       )), ("body", cvtBLOCK x1379)]
   and cvtTYPE_CASE {ptrn=opt1386, body=x1390} = PrettyRep.Rec [("ptrn", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1385))
       )), ("body", cvtBLOCK x1390)]
   and cvtFUNC_NAME {kind=x1396, ident=x1397} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1396), 
          ("ident", cvtIDENT x1397)]
   and cvtPACKAGE {name=x1403, body=x1404} = PrettyRep.Rec [("name", cvtUSTRING x1403), 
          ("body", cvtBLOCK x1404)]
   and cvtPROGRAM {packages=ls1411, fixtures=opt1416, body=x1420} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1410 => cvtPACKAGE x1410
                                   ) ls1411)), ("fixtures", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1415))
       )), ("body", cvtBLOCK x1420)]
end

