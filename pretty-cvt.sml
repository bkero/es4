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
   and cvtCLS (Cls{name=x275, extends=opt277, implements=ls282, classFixtures=x286, 
          instanceFixtures=x287, instanceInits=x288, constructor=opt290, classType=x294, 
          instanceType=x295}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x275), ("extends", 
       (case opt277 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x276 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x276))
       )), ("implements", PrettyRep.List (List.map (fn x281 => cvtNAME x281
                                                   ) ls282)), ("classFixtures", 
          cvtFIXTURES x286), ("instanceFixtures", cvtFIXTURES x287), ("instanceInits", 
          cvtINITS x288), ("constructor", 
       (case opt290 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x289 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x289))
       )), ("classType", cvtTYPE_EXPR x294), ("instanceType", cvtTYPE_EXPR x295)]))
   and cvtCTOR (Ctor{settings=x317, func=x318}) = PrettyRep.Ctor ("Ctor", SOME (PrettyRep.Rec [("settings", 
          cvtINITS x317), ("func", cvtFUNC x318)]))
   and cvtFUNC (Func{name=x326, fsig=x327, block=x328, param=x329, defaults=ls331, 
          ty=x335}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x326), ("fsig", cvtFUNC_SIG x327), ("block", cvtBLOCK x328), 
          ("param", cvtHEAD x329), ("defaults", PrettyRep.List (List.map (fn x330 => 
                                                                                cvtEXPR x330
                                                                         ) ls331)), 
          ("ty", cvtFUNC_TYPE x335)]))
   and cvtDEFN (ClassDefn x351) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x351))
     | cvtDEFN (VariableDefn x354) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x354))
     | cvtDEFN (FunctionDefn x357) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x357))
     | cvtDEFN (ConstructorDefn x360) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x360))
     | cvtDEFN (InterfaceDefn x363) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x363))
     | cvtDEFN (NamespaceDefn x366) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x366))
     | cvtDEFN (TypeDefn x369) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x369))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls373, params=x377, settings=opt379, 
          returnType=x383, thisType=opt385, hasRest=b389}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x372 => 
                                                                              cvtIDENT x372
                                                                       ) ls373)), 
          ("params", cvtBINDINGS x377), ("settings", 
       (case opt379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x378 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x378))
       )), ("returnType", cvtTYPE_EXPR x383), ("thisType", 
       (case opt385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x384 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x384))
       )), ("hasRest", PrettyRep.Bool b389)]))
   and cvtBINDING (Binding{ident=x405, ty=opt407}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x405), ("ty", 
       (case opt407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x406 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x406))
       ))]))
   and cvtBINDING_IDENT (TempIdent n418) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n418))
     | cvtBINDING_IDENT (PropIdent x421) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x421))
   and cvtINIT_STEP (InitStep(x424, x425)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x424, 
          cvtEXPR x425]))
     | cvtINIT_STEP (AssignStep(x429, x430)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x429, cvtEXPR x430]))
   and cvtTYPE_EXPR (SpecialType x434) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x434))
     | cvtTYPE_EXPR (UnionType ls438) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x437 => 
                                                                                                           cvtTYPE_EXPR x437
                                                                                                    ) ls438)))
     | cvtTYPE_EXPR (ArrayType ls445) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x444 => 
                                                                                                           cvtTYPE_EXPR x444
                                                                                                    ) ls445)))
     | cvtTYPE_EXPR (TypeName x451) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x451))
     | cvtTYPE_EXPR (TypeRef(x454, x455)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x454, 
          cvtIDENT x455]))
     | cvtTYPE_EXPR (FunctionType x459) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x459))
     | cvtTYPE_EXPR (ObjectType ls463) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x462 => 
                                                                                                             cvtFIELD_TYPE x462
                                                                                                      ) ls463)))
     | cvtTYPE_EXPR (AppType{base=x469, args=ls471}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x469), ("args", PrettyRep.List (List.map (fn x470 => 
                                                                                                     cvtTYPE_EXPR x470
                                                                                              ) ls471))]))
     | cvtTYPE_EXPR (NullableType{expr=x482, nullable=b483}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x482), ("nullable", PrettyRep.Bool b483)]))
     | cvtTYPE_EXPR (InstanceType{name=x491, typeParams=ls493, ty=x497}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x491), 
          ("typeParams", PrettyRep.List (List.map (fn x492 => cvtIDENT x492
                                                  ) ls493)), ("ty", cvtTYPE_EXPR x497)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x508) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x508))
     | cvtSTMT (InitStmt{kind=x511, ns=x512, prototype=b513, static=b514, inits=ls516}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x511), 
          ("ns", cvtEXPR x512), ("prototype", PrettyRep.Bool b513), ("static", 
          PrettyRep.Bool b514), ("inits", PrettyRep.List (List.map (fn x515 => 
                                                                          cvtINIT_STEP x515
                                                                   ) ls516))]))
     | cvtSTMT (ClassBlock{ns=x533, ident=x534, name=opt536, extends=opt541, 
          fixtures=opt546, block=x550}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x533), ("ident", cvtIDENT x534), ("name", 
       (case opt536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x535 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x535))
       )), ("extends", 
       (case opt541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x540 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x540))
       )), ("fixtures", 
       (case opt546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x545 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x545))
       )), ("block", cvtBLOCK x550)]))
     | cvtSTMT (PackageBlock{name=x566, block=x567}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x566), ("block", cvtBLOCK x567)]))
     | cvtSTMT (ForEachStmt x575) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x575))
     | cvtSTMT (ForInStmt x578) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x578))
     | cvtSTMT (ThrowStmt x581) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x581))
     | cvtSTMT (ReturnStmt x584) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x584))
     | cvtSTMT (BreakStmt opt588) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt588 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x587 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x587))
       ))
     | cvtSTMT (ContinueStmt opt595) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x594 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x594))
       ))
     | cvtSTMT (BlockStmt x601) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x601))
     | cvtSTMT (LabeledStmt(x604, x605)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x604, 
          cvtSTMT x605]))
     | cvtSTMT (LetStmt x609) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x609))
     | cvtSTMT (SuperStmt x612) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x612))
     | cvtSTMT (WhileStmt x615) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x615))
     | cvtSTMT (DoWhileStmt x618) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x618))
     | cvtSTMT (ForStmt{fixtures=opt622, defn=opt627, init=ls632, cond=x636, 
          update=x637, contLabel=opt639, body=x643}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x621 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x621))
       )), ("defn", 
       (case opt627 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x626 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x626))
       )), ("init", PrettyRep.List (List.map (fn x631 => cvtSTMT x631
                                             ) ls632)), ("cond", cvtEXPR x636), 
          ("update", cvtEXPR x637), ("contLabel", 
       (case opt639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x638 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x638))
       )), ("body", cvtSTMT x643)]))
     | cvtSTMT (IfStmt{cnd=x661, thn=x662, els=x663}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x661), ("thn", cvtSTMT x662), 
          ("els", cvtSTMT x663)]))
     | cvtSTMT (WithStmt{obj=x673, ty=x674, body=x675}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x673), ("ty", cvtTYPE_EXPR x674), 
          ("body", cvtSTMT x675)]))
     | cvtSTMT (TryStmt{block=x685, catches=ls707, finally=opt712}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x685), ("catches", PrettyRep.List (List.map (fn {bindings=x686, 
                                                                                                     ty=opt688, 
                                                                                                     fixtures=opt693, 
                                                                                                     block=x697} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x686), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt688 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x687 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x687))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt693 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x692 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x692))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x697)]
                                                                                              ) ls707)), 
          ("finally", 
       (case opt712 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x711 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x711))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x725, cases=ls727}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x725), ("cases", PrettyRep.List (List.map (fn x726 => 
                                                                                                 cvtCASE x726
                                                                                          ) ls727))]))
     | cvtSTMT (SwitchTypeStmt{cond=x738, ty=x739, cases=ls741}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x738), ("ty", cvtTYPE_EXPR x739), 
          ("cases", PrettyRep.List (List.map (fn x740 => cvtTYPE_CASE x740
                                             ) ls741))]))
     | cvtSTMT (Dxns{expr=x754}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x754)]))
   and cvtEXPR (TrinaryExpr(x760, x761, x762, x763)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x760, cvtEXPR x761, cvtEXPR x762, 
          cvtEXPR x763]))
     | cvtEXPR (BinaryExpr(x767, x768, x769)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x767, cvtEXPR x768, cvtEXPR x769]))
     | cvtEXPR (BinaryTypeExpr(x773, x774, x775)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x773, cvtEXPR x774, cvtTYPE_EXPR x775]))
     | cvtEXPR (UnaryExpr(x779, x780)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x779, 
          cvtEXPR x780]))
     | cvtEXPR (TypeExpr x784) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x784))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt789) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt789 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x788 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x788))
       ))
     | cvtEXPR (SuperExpr opt796) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x795))
       ))
     | cvtEXPR (LiteralExpr x802) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x802))
     | cvtEXPR (CallExpr{func=x805, actuals=ls807}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x805), ("actuals", PrettyRep.List (List.map (fn x806 => 
                                                                                                   cvtEXPR x806
                                                                                            ) ls807))]))
     | cvtEXPR (ApplyTypeExpr{expr=x818, actuals=ls820}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x818), ("actuals", PrettyRep.List (List.map (fn x819 => 
                                                                                                   cvtTYPE_EXPR x819
                                                                                            ) ls820))]))
     | cvtEXPR (LetExpr{defs=x831, body=x832, head=opt834}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x831), ("body", cvtEXPR x832), 
          ("head", 
       (case opt834 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x833 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x833))
       ))]))
     | cvtEXPR (NewExpr{obj=x847, actuals=ls849}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x847), ("actuals", PrettyRep.List (List.map (fn x848 => 
                                                                                                  cvtEXPR x848
                                                                                           ) ls849))]))
     | cvtEXPR (ObjectRef{base=x860, ident=x861}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x860), ("ident", cvtIDENT_EXPR x861)]))
     | cvtEXPR (LexicalRef{ident=x869}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x869)]))
     | cvtEXPR (SetExpr(x875, x876, x877)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x875, 
          cvtEXPR x876, cvtEXPR x877]))
     | cvtEXPR (ListExpr ls882) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x881 => 
                                                                                                    cvtEXPR x881
                                                                                             ) ls882)))
     | cvtEXPR (InitExpr(x888, x889)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x888, 
          cvtINITS x889]))
     | cvtEXPR (SliceExpr(x893, x894, x895)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x893, cvtEXPR x894, cvtEXPR x895]))
     | cvtEXPR (DefTemp(n899, x900)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n899, 
          cvtEXPR x900]))
     | cvtEXPR (GetTemp n904) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n904))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n911) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n911))
     | cvtFIXTURE_NAME (PropName x914) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x914))
   and cvtIDENT_EXPR (Identifier{ident=x917, openNamespaces=ls923}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x917), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls919 => PrettyRep.List (List.map (fn x918 => 
                                                                                cvtNAMESPACE x918
                                                                         ) ls919)
                                   ) ls923))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x934, expr=x935}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x934), ("expr", cvtEXPR x935)]))
     | cvtIDENT_EXPR (AttributeIdentifier x943) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x943))
     | cvtIDENT_EXPR (ExpressionIdentifier x946) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x946))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x949, ident=x950}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x949), ("ident", cvtUSTRING x950)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x958, typeParams=ls960}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x958), ("typeParams", 
          PrettyRep.List (List.map (fn x959 => cvtTYPE_EXPR x959
                                   ) ls960))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r973) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r973))
     | cvtLITERAL (LiteralBoolean b976) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b976))
     | cvtLITERAL (LiteralString x979) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x979))
     | cvtLITERAL (LiteralArray{exprs=ls983, ty=opt988}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x982 => 
                                                                         cvtEXPR x982
                                                                  ) ls983)), 
          ("ty", 
       (case opt988 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x987 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x987))
       ))]))
     | cvtLITERAL (LiteralXML ls1000) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x999 => 
                                                                                                            cvtEXPR x999
                                                                                                     ) ls1000)))
     | cvtLITERAL (LiteralNamespace x1006) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1006))
     | cvtLITERAL (LiteralObject{expr=ls1010, ty=opt1015}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1009 => 
                                                                        cvtFIELD x1009
                                                                 ) ls1010)), 
          ("ty", 
       (case opt1015 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1014 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1014))
       ))]))
     | cvtLITERAL (LiteralFunction x1026) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1026))
     | cvtLITERAL (LiteralRegExp{str=x1029}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1029)]))
   and cvtBLOCK (Block x1035) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1035))
   and cvtFIXTURE (NamespaceFixture x1038) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1038))
     | cvtFIXTURE (ClassFixture x1041) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1041))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1045) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1045))
     | cvtFIXTURE (MethodFixture{ty=x1048, isOverride=b1049, isFinal=b1050}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1048), 
          ("isOverride", PrettyRep.Bool b1049), ("isFinal", PrettyRep.Bool b1050)]))
     | cvtFIXTURE (ValFixture{ty=x1060, readOnly=b1061}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1060), ("readOnly", PrettyRep.Bool b1061)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1069, getter=opt1071, setter=opt1076}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1069), ("getter", 
       (case opt1071 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1070 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1070))
       )), ("setter", 
       (case opt1076 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1075 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1075))
       ))]))
   and cvtBINDINGS (ls1090, ls1095) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1089 => 
                                                                                       cvtBINDING x1089
                                                                                ) ls1090), 
          PrettyRep.List (List.map (fn x1094 => cvtINIT_STEP x1094
                                   ) ls1095)]
   and cvtFIXTURES ls1103 = PrettyRep.List (List.map (fn (x1100, x1101) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1100, 
                                                            cvtFIXTURE x1101]
                                                     ) ls1103)
   and cvtINITS ls1110 = PrettyRep.List (List.map (fn (x1107, x1108) => PrettyRep.Tuple [cvtFIXTURE_NAME x1107, 
                                                         cvtEXPR x1108]
                                                  ) ls1110)
   and cvtHEAD (x1114, x1115) = PrettyRep.Tuple [cvtFIXTURES x1114, cvtINITS x1115]
   and cvtFIELD {kind=x1117, name=x1118, init=x1119} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1117), ("name", cvtIDENT_EXPR x1118), ("init", cvtEXPR x1119)]
   and cvtFIELD_TYPE {name=x1127, ty=x1128} = PrettyRep.Rec [("name", cvtIDENT x1127), 
          ("ty", cvtTYPE_EXPR x1128)]
   and cvtTYPED_IDENT {name=x1134, ty=opt1136} = PrettyRep.Rec [("name", cvtIDENT x1134), 
          ("ty", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1135))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1146, params=ls1151, result=x1155, thisType=opt1157, 
          hasRest=b1161, minArgs=n1162} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1145 => 
                                                                                                        cvtIDENT x1145
                                                                                                 ) ls1146)), 
          ("params", PrettyRep.List (List.map (fn x1150 => cvtTYPE_EXPR x1150
                                              ) ls1151)), ("result", cvtTYPE_EXPR x1155), 
          ("thisType", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1156))
       )), ("hasRest", PrettyRep.Bool b1161), ("minArgs", PrettyRep.Int n1162)]
   and cvtFUNC_DEFN {kind=x1176, ns=x1177, final=b1178, native=b1179, override=b1180, 
          prototype=b1181, static=b1182, func=x1183} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1176), ("ns", cvtEXPR x1177), ("final", PrettyRep.Bool b1178), 
          ("native", PrettyRep.Bool b1179), ("override", PrettyRep.Bool b1180), 
          ("prototype", PrettyRep.Bool b1181), ("static", PrettyRep.Bool b1182), 
          ("func", cvtFUNC x1183)]
   and cvtCTOR_DEFN {ns=x1201, native=b1202, ctor=x1203} = PrettyRep.Rec [("ns", 
          cvtEXPR x1201), ("native", PrettyRep.Bool b1202), ("ctor", cvtCTOR x1203)]
   and cvtVAR_DEFN {kind=x1211, ns=x1212, static=b1213, prototype=b1214, bindings=x1215} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1211), ("ns", cvtEXPR x1212), 
          ("static", PrettyRep.Bool b1213), ("prototype", PrettyRep.Bool b1214), 
          ("bindings", cvtBINDINGS x1215)]
   and cvtNAMESPACE_DEFN {ident=x1227, ns=x1228, init=opt1230} = PrettyRep.Rec [("ident", 
          cvtIDENT x1227), ("ns", cvtEXPR x1228), ("init", 
       (case opt1230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1229 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1229))
       ))]
   and cvtCLASS_DEFN {ident=x1241, ns=x1242, nonnullable=b1243, dynamic=b1244, 
          final=b1245, params=ls1247, extends=opt1252, implements=ls1257, block=x1261, 
          classDefns=ls1263, instanceDefns=ls1268} = PrettyRep.Rec [("ident", 
          cvtIDENT x1241), ("ns", cvtEXPR x1242), ("nonnullable", PrettyRep.Bool b1243), 
          ("dynamic", PrettyRep.Bool b1244), ("final", PrettyRep.Bool b1245), 
          ("params", PrettyRep.List (List.map (fn x1246 => cvtIDENT x1246
                                              ) ls1247)), ("extends", 
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1251))
       )), ("implements", PrettyRep.List (List.map (fn x1256 => cvtIDENT_EXPR x1256
                                                   ) ls1257)), ("block", cvtBLOCK x1261), 
          ("classDefns", PrettyRep.List (List.map (fn x1262 => cvtDEFN x1262
                                                  ) ls1263)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1267 => cvtDEFN x1267
                                   ) ls1268))]
   and cvtINTERFACE_DEFN {ident=x1295, ns=x1296, nonnullable=b1297, params=ls1299, 
          extends=ls1304, block=x1308} = PrettyRep.Rec [("ident", cvtIDENT x1295), 
          ("ns", cvtEXPR x1296), ("nonnullable", PrettyRep.Bool b1297), ("params", 
          PrettyRep.List (List.map (fn x1298 => cvtIDENT x1298
                                   ) ls1299)), ("extends", PrettyRep.List (List.map (fn x1303 => 
                                                                                           cvtIDENT_EXPR x1303
                                                                                    ) ls1304)), 
          ("block", cvtBLOCK x1308)]
   and cvtTYPE_DEFN {ident=x1322, ns=x1323, init=x1324} = PrettyRep.Rec [("ident", 
          cvtIDENT x1322), ("ns", cvtEXPR x1323), ("init", cvtTYPE_EXPR x1324)]
   and cvtFOR_ENUM_STMT {defn=opt1333, obj=x1337, fixtures=opt1339, inits=opt1344, 
          contLabel=opt1349, body=x1353} = PrettyRep.Rec [("defn", 
       (case opt1333 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1332 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1332))
       )), ("obj", cvtEXPR x1337), ("fixtures", 
       (case opt1339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1338 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1338))
       )), ("inits", 
       (case opt1344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1343 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1343))
       )), ("contLabel", 
       (case opt1349 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1348 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1348))
       )), ("body", cvtSTMT x1353)]
   and cvtWHILE_STMT {cond=x1367, fixtures=opt1369, body=x1373, contLabel=opt1375} = 
          PrettyRep.Rec [("cond", cvtEXPR x1367), ("fixtures", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1368))
       )), ("body", cvtSTMT x1373), ("contLabel", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1374 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1374))
       ))]
   and cvtDIRECTIVES {pragmas=ls1389, defns=ls1394, head=opt1399, body=ls1404} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1388 => 
                                                                     cvtPRAGMA x1388
                                                              ) ls1389)), ("defns", 
          PrettyRep.List (List.map (fn x1393 => cvtDEFN x1393
                                   ) ls1394)), ("head", 
       (case opt1399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1398 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1398))
       )), ("body", PrettyRep.List (List.map (fn x1403 => cvtSTMT x1403
                                             ) ls1404))]
   and cvtCASE {label=opt1418, inits=opt1423, body=x1427} = PrettyRep.Rec [("label", 
          
       (case opt1418 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1417 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1417))
       )), ("inits", 
       (case opt1423 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1422 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1422))
       )), ("body", cvtBLOCK x1427)]
   and cvtTYPE_CASE {ty=opt1436, bindings=x1440, inits=opt1442, body=x1446} = 
          PrettyRep.Rec [("ty", 
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1435))
       )), ("bindings", cvtBINDINGS x1440), ("inits", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1441 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1441))
       )), ("body", cvtBLOCK x1446)]
   and cvtFUNC_NAME {kind=x1456, ident=x1457} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1456), 
          ("ident", cvtIDENT x1457)]
   and cvtPACKAGE {name=x1463, block=x1464} = PrettyRep.Rec [("name", cvtUSTRING x1463), 
          ("block", cvtBLOCK x1464)]
   and cvtPROGRAM {packages=ls1471, fixtures=opt1476, block=x1480} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1470 => cvtPACKAGE x1470
                                   ) ls1471)), ("fixtures", 
       (case opt1476 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1475 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1475))
       )), ("block", cvtBLOCK x1480)]
end

