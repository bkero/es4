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
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls373, params=x377, defaults=ls379, 
          settings=opt384, returnType=x388, thisType=opt390, hasRest=b394}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x372 => cvtIDENT x372
                                   ) ls373)), ("params", cvtBINDINGS x377), 
          ("defaults", PrettyRep.List (List.map (fn x378 => cvtEXPR x378
                                                ) ls379)), ("settings", 
       (case opt384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x383 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x383))
       )), ("returnType", cvtTYPE_EXPR x388), ("thisType", 
       (case opt390 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x389 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x389))
       )), ("hasRest", PrettyRep.Bool b394)]))
   and cvtBINDING (Binding{ident=x412, ty=opt414}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x412), ("ty", 
       (case opt414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x413 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x413))
       ))]))
   and cvtBINDING_IDENT (TempIdent n425) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n425))
     | cvtBINDING_IDENT (PropIdent x428) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x428))
   and cvtINIT_STEP (InitStep(x431, x432)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x431, 
          cvtEXPR x432]))
     | cvtINIT_STEP (AssignStep(x436, x437)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x436, cvtEXPR x437]))
   and cvtTYPE_EXPR (SpecialType x441) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x441))
     | cvtTYPE_EXPR (UnionType ls445) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x444 => 
                                                                                                           cvtTYPE_EXPR x444
                                                                                                    ) ls445)))
     | cvtTYPE_EXPR (ArrayType ls452) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x451 => 
                                                                                                           cvtTYPE_EXPR x451
                                                                                                    ) ls452)))
     | cvtTYPE_EXPR (TypeName x458) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x458))
     | cvtTYPE_EXPR (TypeRef(x461, x462)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x461, 
          cvtIDENT x462]))
     | cvtTYPE_EXPR (FunctionType x466) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x466))
     | cvtTYPE_EXPR (ObjectType ls470) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x469 => 
                                                                                                             cvtFIELD_TYPE x469
                                                                                                      ) ls470)))
     | cvtTYPE_EXPR (AppType{base=x476, args=ls478}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x476), ("args", PrettyRep.List (List.map (fn x477 => 
                                                                                                     cvtTYPE_EXPR x477
                                                                                              ) ls478))]))
     | cvtTYPE_EXPR (NullableType{expr=x489, nullable=b490}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x489), ("nullable", PrettyRep.Bool b490)]))
     | cvtTYPE_EXPR (InstanceType{name=x498, typeParams=ls500, ty=x504}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x498), 
          ("typeParams", PrettyRep.List (List.map (fn x499 => cvtIDENT x499
                                                  ) ls500)), ("ty", cvtTYPE_EXPR x504)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x515) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x515))
     | cvtSTMT (InitStmt{kind=x518, ns=x519, prototype=b520, static=b521, inits=ls523}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x518), 
          ("ns", cvtEXPR x519), ("prototype", PrettyRep.Bool b520), ("static", 
          PrettyRep.Bool b521), ("inits", PrettyRep.List (List.map (fn x522 => 
                                                                          cvtINIT_STEP x522
                                                                   ) ls523))]))
     | cvtSTMT (ClassBlock{ns=x540, ident=x541, name=opt543, extends=opt548, 
          fixtures=opt553, block=x557}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x540), ("ident", cvtIDENT x541), ("name", 
       (case opt543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x542 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x542))
       )), ("extends", 
       (case opt548 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x547 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x547))
       )), ("fixtures", 
       (case opt553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x552 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x552))
       )), ("block", cvtBLOCK x557)]))
     | cvtSTMT (PackageBlock{name=x573, block=x574}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x573), ("block", cvtBLOCK x574)]))
     | cvtSTMT (ForEachStmt x582) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x582))
     | cvtSTMT (ForInStmt x585) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x585))
     | cvtSTMT (ThrowStmt x588) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x588))
     | cvtSTMT (ReturnStmt x591) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x591))
     | cvtSTMT (BreakStmt opt595) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x594 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x594))
       ))
     | cvtSTMT (ContinueStmt opt602) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x601))
       ))
     | cvtSTMT (BlockStmt x608) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x608))
     | cvtSTMT (LabeledStmt(x611, x612)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x611, 
          cvtSTMT x612]))
     | cvtSTMT (LetStmt x616) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x616))
     | cvtSTMT (SuperStmt x619) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x619))
     | cvtSTMT (WhileStmt x622) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x622))
     | cvtSTMT (DoWhileStmt x625) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x625))
     | cvtSTMT (ForStmt{fixtures=opt629, defn=opt634, init=ls639, cond=x643, 
          update=x644, contLabel=opt646, body=x650}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x628 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x628))
       )), ("defn", 
       (case opt634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x633 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x633))
       )), ("init", PrettyRep.List (List.map (fn x638 => cvtSTMT x638
                                             ) ls639)), ("cond", cvtEXPR x643), 
          ("update", cvtEXPR x644), ("contLabel", 
       (case opt646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x645 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x645))
       )), ("body", cvtSTMT x650)]))
     | cvtSTMT (IfStmt{cnd=x668, thn=x669, els=x670}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x668), ("thn", cvtSTMT x669), 
          ("els", cvtSTMT x670)]))
     | cvtSTMT (WithStmt{obj=x680, ty=x681, body=x682}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x680), ("ty", cvtTYPE_EXPR x681), 
          ("body", cvtSTMT x682)]))
     | cvtSTMT (TryStmt{block=x692, catches=ls714, finally=opt719}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x692), ("catches", PrettyRep.List (List.map (fn {bindings=x693, 
                                                                                                     ty=opt695, 
                                                                                                     fixtures=opt700, 
                                                                                                     block=x704} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x693), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt695 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x694 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x694))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt700 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x699 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x699))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x704)]
                                                                                              ) ls714)), 
          ("finally", 
       (case opt719 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x718 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x718))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x732, cases=ls734}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x732), ("cases", PrettyRep.List (List.map (fn x733 => 
                                                                                                 cvtCASE x733
                                                                                          ) ls734))]))
     | cvtSTMT (SwitchTypeStmt{cond=x745, ty=x746, cases=ls748}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x745), ("ty", cvtTYPE_EXPR x746), 
          ("cases", PrettyRep.List (List.map (fn x747 => cvtTYPE_CASE x747
                                             ) ls748))]))
     | cvtSTMT (Dxns{expr=x761}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x761)]))
   and cvtEXPR (TrinaryExpr(x767, x768, x769, x770)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x767, cvtEXPR x768, cvtEXPR x769, 
          cvtEXPR x770]))
     | cvtEXPR (BinaryExpr(x774, x775, x776)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x774, cvtEXPR x775, cvtEXPR x776]))
     | cvtEXPR (BinaryTypeExpr(x780, x781, x782)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x780, cvtEXPR x781, cvtTYPE_EXPR x782]))
     | cvtEXPR (UnaryExpr(x786, x787)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x786, 
          cvtEXPR x787]))
     | cvtEXPR (TypeExpr x791) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x791))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt796) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x795))
       ))
     | cvtEXPR (SuperExpr opt803) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt803 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x802 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x802))
       ))
     | cvtEXPR (LiteralExpr x809) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x809))
     | cvtEXPR (CallExpr{func=x812, actuals=ls814}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x812), ("actuals", PrettyRep.List (List.map (fn x813 => 
                                                                                                   cvtEXPR x813
                                                                                            ) ls814))]))
     | cvtEXPR (ApplyTypeExpr{expr=x825, actuals=ls827}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x825), ("actuals", PrettyRep.List (List.map (fn x826 => 
                                                                                                   cvtTYPE_EXPR x826
                                                                                            ) ls827))]))
     | cvtEXPR (LetExpr{defs=x838, body=x839, head=opt841}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x838), ("body", cvtEXPR x839), 
          ("head", 
       (case opt841 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x840 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x840))
       ))]))
     | cvtEXPR (NewExpr{obj=x854, actuals=ls856}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x854), ("actuals", PrettyRep.List (List.map (fn x855 => 
                                                                                                  cvtEXPR x855
                                                                                           ) ls856))]))
     | cvtEXPR (ObjectRef{base=x867, ident=x868}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x867), ("ident", cvtIDENT_EXPR x868)]))
     | cvtEXPR (LexicalRef{ident=x876}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x876)]))
     | cvtEXPR (SetExpr(x882, x883, x884)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x882, 
          cvtEXPR x883, cvtEXPR x884]))
     | cvtEXPR (ListExpr ls889) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x888 => 
                                                                                                    cvtEXPR x888
                                                                                             ) ls889)))
     | cvtEXPR (InitExpr(x895, x896)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x895, 
          cvtINITS x896]))
     | cvtEXPR (SliceExpr(x900, x901, x902)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x900, cvtEXPR x901, cvtEXPR x902]))
     | cvtEXPR (DefTemp(n906, x907)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n906, 
          cvtEXPR x907]))
     | cvtEXPR (GetTemp n911) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n911))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n918) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n918))
     | cvtFIXTURE_NAME (PropName x921) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x921))
   and cvtIDENT_EXPR (Identifier{ident=x924, openNamespaces=ls930}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x924), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls926 => PrettyRep.List (List.map (fn x925 => 
                                                                                cvtNAMESPACE x925
                                                                         ) ls926)
                                   ) ls930))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x941, expr=x942}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x941), ("expr", cvtEXPR x942)]))
     | cvtIDENT_EXPR (AttributeIdentifier x950) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x950))
     | cvtIDENT_EXPR (ExpressionIdentifier x953) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x953))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x956, ident=x957}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x956), ("ident", cvtUSTRING x957)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x965, typeParams=ls967}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x965), ("typeParams", 
          PrettyRep.List (List.map (fn x966 => cvtTYPE_EXPR x966
                                   ) ls967))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r980) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r980))
     | cvtLITERAL (LiteralBoolean b983) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b983))
     | cvtLITERAL (LiteralString x986) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x986))
     | cvtLITERAL (LiteralArray{exprs=ls990, ty=opt995}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x989 => 
                                                                         cvtEXPR x989
                                                                  ) ls990)), 
          ("ty", 
       (case opt995 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x994 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x994))
       ))]))
     | cvtLITERAL (LiteralXML ls1007) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1006 => 
                                                                                                            cvtEXPR x1006
                                                                                                     ) ls1007)))
     | cvtLITERAL (LiteralNamespace x1013) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1013))
     | cvtLITERAL (LiteralObject{expr=ls1017, ty=opt1022}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1016 => 
                                                                        cvtFIELD x1016
                                                                 ) ls1017)), 
          ("ty", 
       (case opt1022 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1021 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1021))
       ))]))
     | cvtLITERAL (LiteralFunction x1033) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1033))
     | cvtLITERAL (LiteralRegExp{str=x1036}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1036)]))
   and cvtBLOCK (Block x1042) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1042))
   and cvtFIXTURE (NamespaceFixture x1045) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1045))
     | cvtFIXTURE (ClassFixture x1048) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1048))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1052) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1052))
     | cvtFIXTURE (MethodFixture{ty=x1055, isOverride=b1056, isFinal=b1057}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1055), 
          ("isOverride", PrettyRep.Bool b1056), ("isFinal", PrettyRep.Bool b1057)]))
     | cvtFIXTURE (ValFixture{ty=x1067, readOnly=b1068}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1067), ("readOnly", PrettyRep.Bool b1068)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1076, getter=opt1078, setter=opt1083}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1076), ("getter", 
       (case opt1078 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1077 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1077))
       )), ("setter", 
       (case opt1083 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1082 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1082))
       ))]))
   and cvtBINDINGS (ls1097, ls1102) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1096 => 
                                                                                       cvtBINDING x1096
                                                                                ) ls1097), 
          PrettyRep.List (List.map (fn x1101 => cvtINIT_STEP x1101
                                   ) ls1102)]
   and cvtFIXTURES ls1110 = PrettyRep.List (List.map (fn (x1107, x1108) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1107, 
                                                            cvtFIXTURE x1108]
                                                     ) ls1110)
   and cvtINITS ls1117 = PrettyRep.List (List.map (fn (x1114, x1115) => PrettyRep.Tuple [cvtFIXTURE_NAME x1114, 
                                                         cvtEXPR x1115]
                                                  ) ls1117)
   and cvtHEAD (x1121, x1122) = PrettyRep.Tuple [cvtFIXTURES x1121, cvtINITS x1122]
   and cvtFIELD {kind=x1124, name=x1125, init=x1126} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1124), ("name", cvtIDENT_EXPR x1125), ("init", cvtEXPR x1126)]
   and cvtFIELD_TYPE {name=x1134, ty=x1135} = PrettyRep.Rec [("name", cvtIDENT x1134), 
          ("ty", cvtTYPE_EXPR x1135)]
   and cvtTYPED_IDENT {name=x1141, ty=opt1143} = PrettyRep.Rec [("name", cvtIDENT x1141), 
          ("ty", 
       (case opt1143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1142 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1142))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1153, params=ls1158, result=x1162, thisType=opt1164, 
          hasRest=b1168, minArgs=n1169} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1152 => 
                                                                                                        cvtIDENT x1152
                                                                                                 ) ls1153)), 
          ("params", PrettyRep.List (List.map (fn x1157 => cvtTYPE_EXPR x1157
                                              ) ls1158)), ("result", cvtTYPE_EXPR x1162), 
          ("thisType", 
       (case opt1164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1163 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1163))
       )), ("hasRest", PrettyRep.Bool b1168), ("minArgs", PrettyRep.Int n1169)]
   and cvtFUNC_DEFN {kind=x1183, ns=x1184, final=b1185, native=b1186, override=b1187, 
          prototype=b1188, static=b1189, func=x1190} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1183), ("ns", cvtEXPR x1184), ("final", PrettyRep.Bool b1185), 
          ("native", PrettyRep.Bool b1186), ("override", PrettyRep.Bool b1187), 
          ("prototype", PrettyRep.Bool b1188), ("static", PrettyRep.Bool b1189), 
          ("func", cvtFUNC x1190)]
   and cvtCTOR_DEFN {ns=x1208, native=b1209, ctor=x1210} = PrettyRep.Rec [("ns", 
          cvtEXPR x1208), ("native", PrettyRep.Bool b1209), ("ctor", cvtCTOR x1210)]
   and cvtVAR_DEFN {kind=x1218, ns=x1219, static=b1220, prototype=b1221, bindings=x1222} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1218), ("ns", cvtEXPR x1219), 
          ("static", PrettyRep.Bool b1220), ("prototype", PrettyRep.Bool b1221), 
          ("bindings", cvtBINDINGS x1222)]
   and cvtNAMESPACE_DEFN {ident=x1234, ns=x1235, init=opt1237} = PrettyRep.Rec [("ident", 
          cvtIDENT x1234), ("ns", cvtEXPR x1235), ("init", 
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1236))
       ))]
   and cvtCLASS_DEFN {ident=x1248, ns=x1249, nonnullable=b1250, dynamic=b1251, 
          final=b1252, params=ls1254, extends=opt1259, implements=ls1264, block=x1268, 
          classDefns=ls1270, instanceDefns=ls1275} = PrettyRep.Rec [("ident", 
          cvtIDENT x1248), ("ns", cvtEXPR x1249), ("nonnullable", PrettyRep.Bool b1250), 
          ("dynamic", PrettyRep.Bool b1251), ("final", PrettyRep.Bool b1252), 
          ("params", PrettyRep.List (List.map (fn x1253 => cvtIDENT x1253
                                              ) ls1254)), ("extends", 
       (case opt1259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1258 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1258))
       )), ("implements", PrettyRep.List (List.map (fn x1263 => cvtIDENT_EXPR x1263
                                                   ) ls1264)), ("block", cvtBLOCK x1268), 
          ("classDefns", PrettyRep.List (List.map (fn x1269 => cvtDEFN x1269
                                                  ) ls1270)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1274 => cvtDEFN x1274
                                   ) ls1275))]
   and cvtINTERFACE_DEFN {ident=x1302, ns=x1303, nonnullable=b1304, params=ls1306, 
          extends=ls1311, block=x1315} = PrettyRep.Rec [("ident", cvtIDENT x1302), 
          ("ns", cvtEXPR x1303), ("nonnullable", PrettyRep.Bool b1304), ("params", 
          PrettyRep.List (List.map (fn x1305 => cvtIDENT x1305
                                   ) ls1306)), ("extends", PrettyRep.List (List.map (fn x1310 => 
                                                                                           cvtIDENT_EXPR x1310
                                                                                    ) ls1311)), 
          ("block", cvtBLOCK x1315)]
   and cvtTYPE_DEFN {ident=x1329, ns=x1330, init=x1331} = PrettyRep.Rec [("ident", 
          cvtIDENT x1329), ("ns", cvtEXPR x1330), ("init", cvtTYPE_EXPR x1331)]
   and cvtFOR_ENUM_STMT {defn=opt1340, obj=x1344, fixtures=opt1346, inits=opt1351, 
          contLabel=opt1356, body=x1360} = PrettyRep.Rec [("defn", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1339))
       )), ("obj", cvtEXPR x1344), ("fixtures", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1345))
       )), ("inits", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1350))
       )), ("contLabel", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1355))
       )), ("body", cvtSTMT x1360)]
   and cvtWHILE_STMT {cond=x1374, fixtures=opt1376, body=x1380, contLabel=opt1382} = 
          PrettyRep.Rec [("cond", cvtEXPR x1374), ("fixtures", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1375))
       )), ("body", cvtSTMT x1380), ("contLabel", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1381))
       ))]
   and cvtDIRECTIVES {pragmas=ls1396, defns=ls1401, head=opt1406, body=ls1411} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1395 => 
                                                                     cvtPRAGMA x1395
                                                              ) ls1396)), ("defns", 
          PrettyRep.List (List.map (fn x1400 => cvtDEFN x1400
                                   ) ls1401)), ("head", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1405))
       )), ("body", PrettyRep.List (List.map (fn x1410 => cvtSTMT x1410
                                             ) ls1411))]
   and cvtCASE {label=opt1425, inits=opt1430, body=x1434} = PrettyRep.Rec [("label", 
          
       (case opt1425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1424 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1424))
       )), ("inits", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1429 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1429))
       )), ("body", cvtBLOCK x1434)]
   and cvtTYPE_CASE {ty=opt1443, bindings=x1447, inits=opt1449, body=x1453} = 
          PrettyRep.Rec [("ty", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1442 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1442))
       )), ("bindings", cvtBINDINGS x1447), ("inits", 
       (case opt1449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1448 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1448))
       )), ("body", cvtBLOCK x1453)]
   and cvtFUNC_NAME {kind=x1463, ident=x1464} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1463), 
          ("ident", cvtIDENT x1464)]
   and cvtPACKAGE {name=x1470, block=x1471} = PrettyRep.Rec [("name", cvtUSTRING x1470), 
          ("block", cvtBLOCK x1471)]
   and cvtPROGRAM {packages=ls1478, fixtures=opt1483, block=x1487} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1477 => cvtPACKAGE x1477
                                   ) ls1478)), ("fixtures", 
       (case opt1483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1482 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1482))
       )), ("block", cvtBLOCK x1487)]
end

