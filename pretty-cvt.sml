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
   and cvtCTOR (Ctor{settings=x317, superArgs=ls319, func=x323}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x317), ("superArgs", PrettyRep.List (List.map (fn x318 => 
                                                                                                         cvtEXPR x318
                                                                                                  ) ls319)), 
          ("func", cvtFUNC x323)]))
   and cvtFUNC (Func{name=x333, fsig=x334, isNative=b335, block=x336, param=x337, 
          defaults=ls339, ty=x343}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x333), ("fsig", cvtFUNC_SIG x334), ("isNative", PrettyRep.Bool b335), 
          ("block", cvtBLOCK x336), ("param", cvtHEAD x337), ("defaults", PrettyRep.List (List.map (fn x338 => 
                                                                                                          cvtEXPR x338
                                                                                                   ) ls339)), 
          ("ty", cvtFUNC_TYPE x343)]))
   and cvtDEFN (ClassDefn x361) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x361))
     | cvtDEFN (VariableDefn x364) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x364))
     | cvtDEFN (FunctionDefn x367) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x367))
     | cvtDEFN (ConstructorDefn x370) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x370))
     | cvtDEFN (InterfaceDefn x373) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x373))
     | cvtDEFN (NamespaceDefn x376) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x376))
     | cvtDEFN (TypeDefn x379) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x379))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls383, params=x387, defaults=ls389, 
          ctorInits=opt400, returnType=x404, thisType=opt406, hasRest=b410}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x382 => cvtIDENT x382
                                   ) ls383)), ("params", cvtBINDINGS x387), 
          ("defaults", PrettyRep.List (List.map (fn x388 => cvtEXPR x388
                                                ) ls389)), ("ctorInits", 
       (case opt400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x393, ls395) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x393, 
            PrettyRep.List (List.map (fn x394 => cvtEXPR x394
                                     ) ls395)]))
       )), ("returnType", cvtTYPE_EXPR x404), ("thisType", 
       (case opt406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x405 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x405))
       )), ("hasRest", PrettyRep.Bool b410)]))
   and cvtBINDING (Binding{ident=x428, ty=opt430}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x428), ("ty", 
       (case opt430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x429 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x429))
       ))]))
   and cvtBINDING_IDENT (TempIdent n441) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n441))
     | cvtBINDING_IDENT (PropIdent x444) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x444))
   and cvtINIT_STEP (InitStep(x447, x448)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x447, 
          cvtEXPR x448]))
     | cvtINIT_STEP (AssignStep(x452, x453)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x452, cvtEXPR x453]))
   and cvtTYPE_EXPR (SpecialType x457) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x457))
     | cvtTYPE_EXPR (UnionType ls461) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x460 => 
                                                                                                           cvtTYPE_EXPR x460
                                                                                                    ) ls461)))
     | cvtTYPE_EXPR (ArrayType ls468) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x467 => 
                                                                                                           cvtTYPE_EXPR x467
                                                                                                    ) ls468)))
     | cvtTYPE_EXPR (TypeName x474) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x474))
     | cvtTYPE_EXPR (TypeRef(x477, x478)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x477, 
          cvtIDENT x478]))
     | cvtTYPE_EXPR (FunctionType x482) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x482))
     | cvtTYPE_EXPR (ObjectType ls486) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x485 => 
                                                                                                             cvtFIELD_TYPE x485
                                                                                                      ) ls486)))
     | cvtTYPE_EXPR (AppType{base=x492, args=ls494}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x492), ("args", PrettyRep.List (List.map (fn x493 => 
                                                                                                     cvtTYPE_EXPR x493
                                                                                              ) ls494))]))
     | cvtTYPE_EXPR (NullableType{expr=x505, nullable=b506}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x505), ("nullable", PrettyRep.Bool b506)]))
     | cvtTYPE_EXPR (InstanceType{name=x514, typeParams=ls516, ty=x520}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x514), 
          ("typeParams", PrettyRep.List (List.map (fn x515 => cvtIDENT x515
                                                  ) ls516)), ("ty", cvtTYPE_EXPR x520)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x531) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x531))
     | cvtSTMT (InitStmt{kind=x534, ns=x535, prototype=b536, static=b537, temps=x538, 
          inits=ls540}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x534), ("ns", cvtEXPR x535), ("prototype", PrettyRep.Bool b536), 
          ("static", PrettyRep.Bool b537), ("temps", cvtBINDINGS x538), ("inits", 
          PrettyRep.List (List.map (fn x539 => cvtINIT_STEP x539
                                   ) ls540))]))
     | cvtSTMT (ClassBlock{ns=x559, ident=x560, name=opt562, extends=opt567, 
          fixtures=opt572, block=x576}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x559), ("ident", cvtIDENT x560), ("name", 
       (case opt562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x561 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x561))
       )), ("extends", 
       (case opt567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x566 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x566))
       )), ("fixtures", 
       (case opt572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x571 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x571))
       )), ("block", cvtBLOCK x576)]))
     | cvtSTMT (PackageBlock{name=x592, block=x593}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x592), ("block", cvtBLOCK x593)]))
     | cvtSTMT (ForEachStmt x601) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x601))
     | cvtSTMT (ForInStmt x604) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x604))
     | cvtSTMT (ThrowStmt x607) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x607))
     | cvtSTMT (ReturnStmt x610) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x610))
     | cvtSTMT (BreakStmt opt614) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x613 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x613))
       ))
     | cvtSTMT (ContinueStmt opt621) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt621 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x620 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x620))
       ))
     | cvtSTMT (BlockStmt x627) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x627))
     | cvtSTMT (LabeledStmt(x630, x631)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x630, 
          cvtSTMT x631]))
     | cvtSTMT (LetStmt x635) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x635))
     | cvtSTMT (SuperStmt x638) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x638))
     | cvtSTMT (WhileStmt x641) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x641))
     | cvtSTMT (DoWhileStmt x644) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x644))
     | cvtSTMT (ForStmt x647) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x647))
     | cvtSTMT (IfStmt{cnd=x650, thn=x651, els=x652}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x650), ("thn", cvtSTMT x651), 
          ("els", cvtSTMT x652)]))
     | cvtSTMT (WithStmt{obj=x662, ty=x663, body=x664}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x662), ("ty", cvtTYPE_EXPR x663), 
          ("body", cvtSTMT x664)]))
     | cvtSTMT (TryStmt{block=x674, catches=ls696, finally=opt701}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x674), ("catches", PrettyRep.List (List.map (fn {bindings=x675, 
                                                                                                     ty=opt677, 
                                                                                                     fixtures=opt682, 
                                                                                                     block=x686} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x675), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt677 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x676 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x676))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt682 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x681 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x681))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x686)]
                                                                                              ) ls696)), 
          ("finally", 
       (case opt701 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x700 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x700))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x714, cases=ls716}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x714), ("cases", PrettyRep.List (List.map (fn x715 => 
                                                                                                 cvtCASE x715
                                                                                          ) ls716))]))
     | cvtSTMT (SwitchTypeStmt{cond=x727, ty=x728, cases=ls730}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x727), ("ty", cvtTYPE_EXPR x728), 
          ("cases", PrettyRep.List (List.map (fn x729 => cvtTYPE_CASE x729
                                             ) ls730))]))
     | cvtSTMT (Dxns{expr=x743}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x743)]))
   and cvtEXPR (TrinaryExpr(x749, x750, x751, x752)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x749, cvtEXPR x750, cvtEXPR x751, 
          cvtEXPR x752]))
     | cvtEXPR (BinaryExpr(x756, x757, x758)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x756, cvtEXPR x757, cvtEXPR x758]))
     | cvtEXPR (BinaryTypeExpr(x762, x763, x764)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x762, cvtEXPR x763, cvtTYPE_EXPR x764]))
     | cvtEXPR (UnaryExpr(x768, x769)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x768, 
          cvtEXPR x769]))
     | cvtEXPR (TypeExpr x773) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x773))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt778) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt778 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x777 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x777))
       ))
     | cvtEXPR (SuperExpr opt785) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt785 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x784 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x784))
       ))
     | cvtEXPR (LiteralExpr x791) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x791))
     | cvtEXPR (CallExpr{func=x794, actuals=ls796}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x794), ("actuals", PrettyRep.List (List.map (fn x795 => 
                                                                                                   cvtEXPR x795
                                                                                            ) ls796))]))
     | cvtEXPR (ApplyTypeExpr{expr=x807, actuals=ls809}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x807), ("actuals", PrettyRep.List (List.map (fn x808 => 
                                                                                                   cvtTYPE_EXPR x808
                                                                                            ) ls809))]))
     | cvtEXPR (LetExpr{defs=x820, body=x821, head=opt823}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x820), ("body", cvtEXPR x821), 
          ("head", 
       (case opt823 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x822 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x822))
       ))]))
     | cvtEXPR (NewExpr{obj=x836, actuals=ls838}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x836), ("actuals", PrettyRep.List (List.map (fn x837 => 
                                                                                                  cvtEXPR x837
                                                                                           ) ls838))]))
     | cvtEXPR (ObjectRef{base=x849, ident=x850}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x849), ("ident", cvtIDENT_EXPR x850)]))
     | cvtEXPR (LexicalRef{ident=x858}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x858)]))
     | cvtEXPR (SetExpr(x864, x865, x866)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x864, 
          cvtEXPR x865, cvtEXPR x866]))
     | cvtEXPR (ListExpr ls871) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x870 => 
                                                                                                    cvtEXPR x870
                                                                                             ) ls871)))
     | cvtEXPR (InitExpr(x877, x878, x879)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x877, 
          cvtHEAD x878, cvtINITS x879]))
     | cvtEXPR (SliceExpr(x883, x884, x885)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x883, cvtEXPR x884, cvtEXPR x885]))
     | cvtEXPR (DefTemp(n889, x890)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n889, 
          cvtEXPR x890]))
     | cvtEXPR (GetTemp n894) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n894))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n901) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n901))
     | cvtFIXTURE_NAME (PropName x904) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x904))
   and cvtIDENT_EXPR (Identifier{ident=x907, openNamespaces=ls913}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x907), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls909 => PrettyRep.List (List.map (fn x908 => 
                                                                                cvtNAMESPACE x908
                                                                         ) ls909)
                                   ) ls913))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x924, expr=x925}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x924), ("expr", cvtEXPR x925)]))
     | cvtIDENT_EXPR (AttributeIdentifier x933) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x933))
     | cvtIDENT_EXPR (ExpressionIdentifier x936) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x936))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x939, ident=x940}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x939), ("ident", cvtUSTRING x940)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x948, typeParams=ls950}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x948), ("typeParams", 
          PrettyRep.List (List.map (fn x949 => cvtTYPE_EXPR x949
                                   ) ls950))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r963) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r963))
     | cvtLITERAL (LiteralBoolean b966) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b966))
     | cvtLITERAL (LiteralString x969) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x969))
     | cvtLITERAL (LiteralArray{exprs=ls973, ty=opt978}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x972 => 
                                                                         cvtEXPR x972
                                                                  ) ls973)), 
          ("ty", 
       (case opt978 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x977 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x977))
       ))]))
     | cvtLITERAL (LiteralXML ls990) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x989 => 
                                                                                                           cvtEXPR x989
                                                                                                    ) ls990)))
     | cvtLITERAL (LiteralNamespace x996) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x996))
     | cvtLITERAL (LiteralObject{expr=ls1000, ty=opt1005}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x999 => 
                                                                        cvtFIELD x999
                                                                 ) ls1000)), 
          ("ty", 
       (case opt1005 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1004 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1004))
       ))]))
     | cvtLITERAL (LiteralFunction x1016) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1016))
     | cvtLITERAL (LiteralRegExp{str=x1019}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1019)]))
   and cvtBLOCK (Block x1025) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1025))
   and cvtFIXTURE (NamespaceFixture x1028) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1028))
     | cvtFIXTURE (ClassFixture x1031) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1031))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1035) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1035))
     | cvtFIXTURE (MethodFixture{func=x1038, ty=x1039, readOnly=b1040, override=b1041, 
          final=b1042}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1038), ("ty", cvtTYPE_EXPR x1039), ("readOnly", PrettyRep.Bool b1040), 
          ("override", PrettyRep.Bool b1041), ("final", PrettyRep.Bool b1042)]))
     | cvtFIXTURE (ValFixture{ty=x1056, readOnly=b1057}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1056), ("readOnly", PrettyRep.Bool b1057)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1065, getter=opt1067, setter=opt1072}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1065), ("getter", 
       (case opt1067 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1066 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1066))
       )), ("setter", 
       (case opt1072 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1071 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1071))
       ))]))
   and cvtBINDINGS (ls1086, ls1091) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1085 => 
                                                                                       cvtBINDING x1085
                                                                                ) ls1086), 
          PrettyRep.List (List.map (fn x1090 => cvtINIT_STEP x1090
                                   ) ls1091)]
   and cvtFIXTURES ls1099 = PrettyRep.List (List.map (fn (x1096, x1097) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1096, 
                                                            cvtFIXTURE x1097]
                                                     ) ls1099)
   and cvtINITS ls1106 = PrettyRep.List (List.map (fn (x1103, x1104) => PrettyRep.Tuple [cvtFIXTURE_NAME x1103, 
                                                         cvtEXPR x1104]
                                                  ) ls1106)
   and cvtHEAD (x1110, x1111) = PrettyRep.Tuple [cvtFIXTURES x1110, cvtINITS x1111]
   and cvtFIELD {kind=x1113, name=x1114, init=x1115} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1113), ("name", cvtIDENT_EXPR x1114), ("init", cvtEXPR x1115)]
   and cvtFIELD_TYPE {name=x1123, ty=x1124} = PrettyRep.Rec [("name", cvtIDENT x1123), 
          ("ty", cvtTYPE_EXPR x1124)]
   and cvtTYPED_IDENT {name=x1130, ty=opt1132} = PrettyRep.Rec [("name", cvtIDENT x1130), 
          ("ty", 
       (case opt1132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1131 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1131))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1142, params=ls1147, result=x1151, thisType=opt1153, 
          hasRest=b1157, minArgs=n1158} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1141 => 
                                                                                                        cvtIDENT x1141
                                                                                                 ) ls1142)), 
          ("params", PrettyRep.List (List.map (fn x1146 => cvtTYPE_EXPR x1146
                                              ) ls1147)), ("result", cvtTYPE_EXPR x1151), 
          ("thisType", 
       (case opt1153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1152 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1152))
       )), ("hasRest", PrettyRep.Bool b1157), ("minArgs", PrettyRep.Int n1158)]
   and cvtFUNC_DEFN {kind=x1172, ns=x1173, final=b1174, override=b1175, prototype=b1176, 
          static=b1177, func=x1178} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1172), 
          ("ns", cvtEXPR x1173), ("final", PrettyRep.Bool b1174), ("override", 
          PrettyRep.Bool b1175), ("prototype", PrettyRep.Bool b1176), ("static", 
          PrettyRep.Bool b1177), ("func", cvtFUNC x1178)]
   and cvtCTOR_DEFN {ns=x1194, ctor=x1195} = PrettyRep.Rec [("ns", cvtEXPR x1194), 
          ("ctor", cvtCTOR x1195)]
   and cvtVAR_DEFN {kind=x1201, ns=x1202, static=b1203, prototype=b1204, bindings=x1205} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1201), ("ns", cvtEXPR x1202), 
          ("static", PrettyRep.Bool b1203), ("prototype", PrettyRep.Bool b1204), 
          ("bindings", cvtBINDINGS x1205)]
   and cvtNAMESPACE_DEFN {ident=x1217, ns=x1218, init=opt1220} = PrettyRep.Rec [("ident", 
          cvtIDENT x1217), ("ns", cvtEXPR x1218), ("init", 
       (case opt1220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1219 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1219))
       ))]
   and cvtCLASS_DEFN {ident=x1231, ns=x1232, nonnullable=b1233, dynamic=b1234, 
          final=b1235, params=ls1237, extends=opt1242, implements=ls1247, classDefns=ls1252, 
          instanceDefns=ls1257, instanceStmts=ls1262, ctorDefn=opt1267} = PrettyRep.Rec [("ident", 
          cvtIDENT x1231), ("ns", cvtEXPR x1232), ("nonnullable", PrettyRep.Bool b1233), 
          ("dynamic", PrettyRep.Bool b1234), ("final", PrettyRep.Bool b1235), 
          ("params", PrettyRep.List (List.map (fn x1236 => cvtIDENT x1236
                                              ) ls1237)), ("extends", 
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1241))
       )), ("implements", PrettyRep.List (List.map (fn x1246 => cvtIDENT_EXPR x1246
                                                   ) ls1247)), ("classDefns", 
          PrettyRep.List (List.map (fn x1251 => cvtDEFN x1251
                                   ) ls1252)), ("instanceDefns", PrettyRep.List (List.map (fn x1256 => 
                                                                                                 cvtDEFN x1256
                                                                                          ) ls1257)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1261 => cvtSTMT x1261
                                                     ) ls1262)), ("ctorDefn", 
          
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1266))
       ))]
   and cvtINTERFACE_DEFN {ident=x1296, ns=x1297, nonnullable=b1298, params=ls1300, 
          extends=ls1305, block=x1309} = PrettyRep.Rec [("ident", cvtIDENT x1296), 
          ("ns", cvtEXPR x1297), ("nonnullable", PrettyRep.Bool b1298), ("params", 
          PrettyRep.List (List.map (fn x1299 => cvtIDENT x1299
                                   ) ls1300)), ("extends", PrettyRep.List (List.map (fn x1304 => 
                                                                                           cvtIDENT_EXPR x1304
                                                                                    ) ls1305)), 
          ("block", cvtBLOCK x1309)]
   and cvtTYPE_DEFN {ident=x1323, ns=x1324, init=x1325} = PrettyRep.Rec [("ident", 
          cvtIDENT x1323), ("ns", cvtEXPR x1324), ("init", cvtTYPE_EXPR x1325)]
   and cvtFOR_ENUM_STMT {defn=opt1334, obj=x1338, fixtures=opt1340, inits=opt1345, 
          labels=ls1350, body=x1354} = PrettyRep.Rec [("defn", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1333))
       )), ("obj", cvtEXPR x1338), ("fixtures", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1339))
       )), ("inits", 
       (case opt1345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1344 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1344))
       )), ("labels", PrettyRep.List (List.map (fn x1349 => cvtIDENT x1349
                                               ) ls1350)), ("body", cvtSTMT x1354)]
   and cvtFOR_STMT {fixtures=opt1369, defn=opt1374, init=x1378, cond=x1379, 
          update=x1380, labels=ls1382, body=x1386} = PrettyRep.Rec [("fixtures", 
          
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1368))
       )), ("defn", 
       (case opt1374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1373 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1373))
       )), ("init", cvtSTMT x1378), ("cond", cvtEXPR x1379), ("update", cvtEXPR x1380), 
          ("labels", PrettyRep.List (List.map (fn x1381 => cvtIDENT x1381
                                              ) ls1382)), ("body", cvtSTMT x1386)]
   and cvtWHILE_STMT {cond=x1402, fixtures=opt1404, body=x1408, labels=ls1410} = 
          PrettyRep.Rec [("cond", cvtEXPR x1402), ("fixtures", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1403 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1403))
       )), ("body", cvtSTMT x1408), ("labels", PrettyRep.List (List.map (fn x1409 => 
                                                                               cvtIDENT x1409
                                                                        ) ls1410))]
   and cvtDIRECTIVES {pragmas=ls1424, defns=ls1429, head=opt1434, body=ls1439} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1423 => 
                                                                     cvtPRAGMA x1423
                                                              ) ls1424)), ("defns", 
          PrettyRep.List (List.map (fn x1428 => cvtDEFN x1428
                                   ) ls1429)), ("head", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1433))
       )), ("body", PrettyRep.List (List.map (fn x1438 => cvtSTMT x1438
                                             ) ls1439))]
   and cvtCASE {label=opt1453, inits=opt1458, body=x1462} = PrettyRep.Rec [("label", 
          
       (case opt1453 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1452 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1452))
       )), ("inits", 
       (case opt1458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1457 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1457))
       )), ("body", cvtBLOCK x1462)]
   and cvtTYPE_CASE {ty=opt1471, bindings=x1475, inits=opt1477, body=x1481} = 
          PrettyRep.Rec [("ty", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1470))
       )), ("bindings", cvtBINDINGS x1475), ("inits", 
       (case opt1477 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1476 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1476))
       )), ("body", cvtBLOCK x1481)]
   and cvtFUNC_NAME {kind=x1491, ident=x1492} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1491), 
          ("ident", cvtIDENT x1492)]
   and cvtPACKAGE {name=x1498, block=x1499} = PrettyRep.Rec [("name", cvtUSTRING x1498), 
          ("block", cvtBLOCK x1499)]
   and cvtPROGRAM {packages=ls1506, fixtures=opt1511, block=x1515} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1505 => cvtPACKAGE x1505
                                   ) ls1506)), ("fixtures", 
       (case opt1511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1510 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1510))
       )), ("block", cvtBLOCK x1515)]
end

