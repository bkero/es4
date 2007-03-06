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
          cvtHEAD x288), ("constructor", 
       (case opt290 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x289 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x289))
       )), ("classType", cvtTYPE_EXPR x294), ("instanceType", cvtTYPE_EXPR x295)]))
   and cvtCTOR (Ctor{settings=x317, func=x318}) = PrettyRep.Ctor ("Ctor", SOME (PrettyRep.Rec [("settings", 
          cvtHEAD x317), ("func", cvtFUNC x318)]))
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
     | cvtSTMT (InitStmt{kind=x518, ns=x519, prototype=b520, static=b521, temps=x522, 
          inits=ls524}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x518), ("ns", cvtEXPR x519), ("prototype", PrettyRep.Bool b520), 
          ("static", PrettyRep.Bool b521), ("temps", cvtBINDINGS x522), ("inits", 
          PrettyRep.List (List.map (fn x523 => cvtINIT_STEP x523
                                   ) ls524))]))
     | cvtSTMT (ClassBlock{ns=x543, ident=x544, name=opt546, extends=opt551, 
          fixtures=opt556, block=x560}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x543), ("ident", cvtIDENT x544), ("name", 
       (case opt546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x545 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x545))
       )), ("extends", 
       (case opt551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x550 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x550))
       )), ("fixtures", 
       (case opt556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x555 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x555))
       )), ("block", cvtBLOCK x560)]))
     | cvtSTMT (PackageBlock{name=x576, block=x577}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x576), ("block", cvtBLOCK x577)]))
     | cvtSTMT (ForEachStmt x585) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x585))
     | cvtSTMT (ForInStmt x588) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x588))
     | cvtSTMT (ThrowStmt x591) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x591))
     | cvtSTMT (ReturnStmt x594) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x594))
     | cvtSTMT (BreakStmt opt598) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x597 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x597))
       ))
     | cvtSTMT (ContinueStmt opt605) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x604 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x604))
       ))
     | cvtSTMT (BlockStmt x611) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x611))
     | cvtSTMT (LabeledStmt(x614, x615)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x614, 
          cvtSTMT x615]))
     | cvtSTMT (LetStmt x619) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x619))
     | cvtSTMT (SuperStmt x622) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x622))
     | cvtSTMT (WhileStmt x625) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x625))
     | cvtSTMT (DoWhileStmt x628) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x628))
     | cvtSTMT (ForStmt{fixtures=opt632, defn=opt637, init=ls642, cond=x646, 
          update=x647, contLabel=opt649, body=x653}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt632 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x631 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x631))
       )), ("defn", 
       (case opt637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x636 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x636))
       )), ("init", PrettyRep.List (List.map (fn x641 => cvtSTMT x641
                                             ) ls642)), ("cond", cvtEXPR x646), 
          ("update", cvtEXPR x647), ("contLabel", 
       (case opt649 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x648 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x648))
       )), ("body", cvtSTMT x653)]))
     | cvtSTMT (IfStmt{cnd=x671, thn=x672, els=x673}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x671), ("thn", cvtSTMT x672), 
          ("els", cvtSTMT x673)]))
     | cvtSTMT (WithStmt{obj=x683, ty=x684, body=x685}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x683), ("ty", cvtTYPE_EXPR x684), 
          ("body", cvtSTMT x685)]))
     | cvtSTMT (TryStmt{block=x695, catches=ls717, finally=opt722}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x695), ("catches", PrettyRep.List (List.map (fn {bindings=x696, 
                                                                                                     ty=opt698, 
                                                                                                     fixtures=opt703, 
                                                                                                     block=x707} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x696), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt698 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x697 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x697))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt703 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x702 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x702))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x707)]
                                                                                              ) ls717)), 
          ("finally", 
       (case opt722 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x721 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x721))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x735, cases=ls737}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x735), ("cases", PrettyRep.List (List.map (fn x736 => 
                                                                                                 cvtCASE x736
                                                                                          ) ls737))]))
     | cvtSTMT (SwitchTypeStmt{cond=x748, ty=x749, cases=ls751}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x748), ("ty", cvtTYPE_EXPR x749), 
          ("cases", PrettyRep.List (List.map (fn x750 => cvtTYPE_CASE x750
                                             ) ls751))]))
     | cvtSTMT (Dxns{expr=x764}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x764)]))
   and cvtEXPR (TrinaryExpr(x770, x771, x772, x773)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x770, cvtEXPR x771, cvtEXPR x772, 
          cvtEXPR x773]))
     | cvtEXPR (BinaryExpr(x777, x778, x779)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x777, cvtEXPR x778, cvtEXPR x779]))
     | cvtEXPR (BinaryTypeExpr(x783, x784, x785)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x783, cvtEXPR x784, cvtTYPE_EXPR x785]))
     | cvtEXPR (UnaryExpr(x789, x790)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x789, 
          cvtEXPR x790]))
     | cvtEXPR (TypeExpr x794) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x794))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt799) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt799 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x798 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x798))
       ))
     | cvtEXPR (SuperExpr opt806) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x805 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x805))
       ))
     | cvtEXPR (LiteralExpr x812) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x812))
     | cvtEXPR (CallExpr{func=x815, actuals=ls817}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x815), ("actuals", PrettyRep.List (List.map (fn x816 => 
                                                                                                   cvtEXPR x816
                                                                                            ) ls817))]))
     | cvtEXPR (ApplyTypeExpr{expr=x828, actuals=ls830}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x828), ("actuals", PrettyRep.List (List.map (fn x829 => 
                                                                                                   cvtTYPE_EXPR x829
                                                                                            ) ls830))]))
     | cvtEXPR (LetExpr{defs=x841, body=x842, head=opt844}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x841), ("body", cvtEXPR x842), 
          ("head", 
       (case opt844 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x843 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x843))
       ))]))
     | cvtEXPR (NewExpr{obj=x857, actuals=ls859}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x857), ("actuals", PrettyRep.List (List.map (fn x858 => 
                                                                                                  cvtEXPR x858
                                                                                           ) ls859))]))
     | cvtEXPR (ObjectRef{base=x870, ident=x871}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x870), ("ident", cvtIDENT_EXPR x871)]))
     | cvtEXPR (LexicalRef{ident=x879}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x879)]))
     | cvtEXPR (SetExpr(x885, x886, x887)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x885, 
          cvtEXPR x886, cvtEXPR x887]))
     | cvtEXPR (ListExpr ls892) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x891 => 
                                                                                                    cvtEXPR x891
                                                                                             ) ls892)))
     | cvtEXPR (InitExpr(x898, x899, x900)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x898, 
          cvtHEAD x899, cvtINITS x900]))
     | cvtEXPR (SliceExpr(x904, x905, x906)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x904, cvtEXPR x905, cvtEXPR x906]))
     | cvtEXPR (DefTemp(n910, x911)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n910, 
          cvtEXPR x911]))
     | cvtEXPR (GetTemp n915) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n915))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n922) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n922))
     | cvtFIXTURE_NAME (PropName x925) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x925))
   and cvtIDENT_EXPR (Identifier{ident=x928, openNamespaces=ls934}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x928), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls930 => PrettyRep.List (List.map (fn x929 => 
                                                                                cvtNAMESPACE x929
                                                                         ) ls930)
                                   ) ls934))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x945, expr=x946}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x945), ("expr", cvtEXPR x946)]))
     | cvtIDENT_EXPR (AttributeIdentifier x954) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x954))
     | cvtIDENT_EXPR (ExpressionIdentifier x957) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x957))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x960, ident=x961}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x960), ("ident", cvtUSTRING x961)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x969, typeParams=ls971}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x969), ("typeParams", 
          PrettyRep.List (List.map (fn x970 => cvtTYPE_EXPR x970
                                   ) ls971))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r984) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r984))
     | cvtLITERAL (LiteralBoolean b987) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b987))
     | cvtLITERAL (LiteralString x990) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x990))
     | cvtLITERAL (LiteralArray{exprs=ls994, ty=opt999}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x993 => 
                                                                         cvtEXPR x993
                                                                  ) ls994)), 
          ("ty", 
       (case opt999 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x998 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x998))
       ))]))
     | cvtLITERAL (LiteralXML ls1011) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1010 => 
                                                                                                            cvtEXPR x1010
                                                                                                     ) ls1011)))
     | cvtLITERAL (LiteralNamespace x1017) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1017))
     | cvtLITERAL (LiteralObject{expr=ls1021, ty=opt1026}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1020 => 
                                                                        cvtFIELD x1020
                                                                 ) ls1021)), 
          ("ty", 
       (case opt1026 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1025 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1025))
       ))]))
     | cvtLITERAL (LiteralFunction x1037) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1037))
     | cvtLITERAL (LiteralRegExp{str=x1040}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1040)]))
   and cvtBLOCK (Block x1046) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1046))
   and cvtFIXTURE (NamespaceFixture x1049) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1049))
     | cvtFIXTURE (ClassFixture x1052) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1052))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1056) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1056))
     | cvtFIXTURE (MethodFixture{ty=x1059, isOverride=b1060, isFinal=b1061}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1059), 
          ("isOverride", PrettyRep.Bool b1060), ("isFinal", PrettyRep.Bool b1061)]))
     | cvtFIXTURE (ValFixture{ty=x1071, readOnly=b1072}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1071), ("readOnly", PrettyRep.Bool b1072)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1080, getter=opt1082, setter=opt1087}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1080), ("getter", 
       (case opt1082 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1081 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1081))
       )), ("setter", 
       (case opt1087 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1086 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1086))
       ))]))
   and cvtBINDINGS (ls1101, ls1106) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1100 => 
                                                                                       cvtBINDING x1100
                                                                                ) ls1101), 
          PrettyRep.List (List.map (fn x1105 => cvtINIT_STEP x1105
                                   ) ls1106)]
   and cvtFIXTURES ls1114 = PrettyRep.List (List.map (fn (x1111, x1112) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1111, 
                                                            cvtFIXTURE x1112]
                                                     ) ls1114)
   and cvtINITS ls1121 = PrettyRep.List (List.map (fn (x1118, x1119) => PrettyRep.Tuple [cvtFIXTURE_NAME x1118, 
                                                         cvtEXPR x1119]
                                                  ) ls1121)
   and cvtHEAD (x1125, x1126) = PrettyRep.Tuple [cvtFIXTURES x1125, cvtINITS x1126]
   and cvtFIELD {kind=x1128, name=x1129, init=x1130} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1128), ("name", cvtIDENT_EXPR x1129), ("init", cvtEXPR x1130)]
   and cvtFIELD_TYPE {name=x1138, ty=x1139} = PrettyRep.Rec [("name", cvtIDENT x1138), 
          ("ty", cvtTYPE_EXPR x1139)]
   and cvtTYPED_IDENT {name=x1145, ty=opt1147} = PrettyRep.Rec [("name", cvtIDENT x1145), 
          ("ty", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1146))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1157, params=ls1162, result=x1166, thisType=opt1168, 
          hasRest=b1172, minArgs=n1173} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1156 => 
                                                                                                        cvtIDENT x1156
                                                                                                 ) ls1157)), 
          ("params", PrettyRep.List (List.map (fn x1161 => cvtTYPE_EXPR x1161
                                              ) ls1162)), ("result", cvtTYPE_EXPR x1166), 
          ("thisType", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1167))
       )), ("hasRest", PrettyRep.Bool b1172), ("minArgs", PrettyRep.Int n1173)]
   and cvtFUNC_DEFN {kind=x1187, ns=x1188, final=b1189, native=b1190, override=b1191, 
          prototype=b1192, static=b1193, func=x1194} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1187), ("ns", cvtEXPR x1188), ("final", PrettyRep.Bool b1189), 
          ("native", PrettyRep.Bool b1190), ("override", PrettyRep.Bool b1191), 
          ("prototype", PrettyRep.Bool b1192), ("static", PrettyRep.Bool b1193), 
          ("func", cvtFUNC x1194)]
   and cvtCTOR_DEFN {ns=x1212, native=b1213, ctor=x1214} = PrettyRep.Rec [("ns", 
          cvtEXPR x1212), ("native", PrettyRep.Bool b1213), ("ctor", cvtCTOR x1214)]
   and cvtVAR_DEFN {kind=x1222, ns=x1223, static=b1224, prototype=b1225, bindings=x1226} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1222), ("ns", cvtEXPR x1223), 
          ("static", PrettyRep.Bool b1224), ("prototype", PrettyRep.Bool b1225), 
          ("bindings", cvtBINDINGS x1226)]
   and cvtNAMESPACE_DEFN {ident=x1238, ns=x1239, init=opt1241} = PrettyRep.Rec [("ident", 
          cvtIDENT x1238), ("ns", cvtEXPR x1239), ("init", 
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1240))
       ))]
   and cvtCLASS_DEFN {ident=x1252, ns=x1253, nonnullable=b1254, dynamic=b1255, 
          final=b1256, params=ls1258, extends=opt1263, implements=ls1268, classDefns=ls1273, 
          instanceDefns=ls1278, instanceStmts=ls1283, ctorDefn=opt1288} = PrettyRep.Rec [("ident", 
          cvtIDENT x1252), ("ns", cvtEXPR x1253), ("nonnullable", PrettyRep.Bool b1254), 
          ("dynamic", PrettyRep.Bool b1255), ("final", PrettyRep.Bool b1256), 
          ("params", PrettyRep.List (List.map (fn x1257 => cvtIDENT x1257
                                              ) ls1258)), ("extends", 
       (case opt1263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1262 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1262))
       )), ("implements", PrettyRep.List (List.map (fn x1267 => cvtIDENT_EXPR x1267
                                                   ) ls1268)), ("classDefns", 
          PrettyRep.List (List.map (fn x1272 => cvtDEFN x1272
                                   ) ls1273)), ("instanceDefns", PrettyRep.List (List.map (fn x1277 => 
                                                                                                 cvtDEFN x1277
                                                                                          ) ls1278)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1282 => cvtSTMT x1282
                                                     ) ls1283)), ("ctorDefn", 
          
       (case opt1288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1287 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1287))
       ))]
   and cvtINTERFACE_DEFN {ident=x1317, ns=x1318, nonnullable=b1319, params=ls1321, 
          extends=ls1326, block=x1330} = PrettyRep.Rec [("ident", cvtIDENT x1317), 
          ("ns", cvtEXPR x1318), ("nonnullable", PrettyRep.Bool b1319), ("params", 
          PrettyRep.List (List.map (fn x1320 => cvtIDENT x1320
                                   ) ls1321)), ("extends", PrettyRep.List (List.map (fn x1325 => 
                                                                                           cvtIDENT_EXPR x1325
                                                                                    ) ls1326)), 
          ("block", cvtBLOCK x1330)]
   and cvtTYPE_DEFN {ident=x1344, ns=x1345, init=x1346} = PrettyRep.Rec [("ident", 
          cvtIDENT x1344), ("ns", cvtEXPR x1345), ("init", cvtTYPE_EXPR x1346)]
   and cvtFOR_ENUM_STMT {defn=opt1355, obj=x1359, fixtures=opt1361, inits=opt1366, 
          contLabel=opt1371, body=x1375} = PrettyRep.Rec [("defn", 
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1354))
       )), ("obj", cvtEXPR x1359), ("fixtures", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1360))
       )), ("inits", 
       (case opt1366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1365 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1365))
       )), ("contLabel", 
       (case opt1371 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1370 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1370))
       )), ("body", cvtSTMT x1375)]
   and cvtWHILE_STMT {cond=x1389, fixtures=opt1391, body=x1395, contLabel=opt1397} = 
          PrettyRep.Rec [("cond", cvtEXPR x1389), ("fixtures", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1390))
       )), ("body", cvtSTMT x1395), ("contLabel", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1396))
       ))]
   and cvtDIRECTIVES {pragmas=ls1411, defns=ls1416, head=opt1421, body=ls1426} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1410 => 
                                                                     cvtPRAGMA x1410
                                                              ) ls1411)), ("defns", 
          PrettyRep.List (List.map (fn x1415 => cvtDEFN x1415
                                   ) ls1416)), ("head", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1420 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1420))
       )), ("body", PrettyRep.List (List.map (fn x1425 => cvtSTMT x1425
                                             ) ls1426))]
   and cvtCASE {label=opt1440, inits=opt1445, body=x1449} = PrettyRep.Rec [("label", 
          
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1439 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1439))
       )), ("inits", 
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1444))
       )), ("body", cvtBLOCK x1449)]
   and cvtTYPE_CASE {ty=opt1458, bindings=x1462, inits=opt1464, body=x1468} = 
          PrettyRep.Rec [("ty", 
       (case opt1458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1457 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1457))
       )), ("bindings", cvtBINDINGS x1462), ("inits", 
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1463 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1463))
       )), ("body", cvtBLOCK x1468)]
   and cvtFUNC_NAME {kind=x1478, ident=x1479} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1478), 
          ("ident", cvtIDENT x1479)]
   and cvtPACKAGE {name=x1485, block=x1486} = PrettyRep.Rec [("name", cvtUSTRING x1485), 
          ("block", cvtBLOCK x1486)]
   and cvtPROGRAM {packages=ls1493, fixtures=opt1498, block=x1502} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1492 => cvtPACKAGE x1492
                                   ) ls1493)), ("fixtures", 
       (case opt1498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1497 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1497))
       )), ("block", cvtBLOCK x1502)]
end

