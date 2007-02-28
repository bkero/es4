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
   and cvtFUNC (Func{name=x326, fsig=x327, fixtures=opt329, defaults=x333, 
          body=x334}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x326), ("fsig", cvtFUNC_SIG x327), ("fixtures", 
       (case opt329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x328 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x328))
       )), ("defaults", cvtINITS x333), ("body", cvtBLOCK x334)]))
   and cvtDEFN (ClassDefn x348) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x348))
     | cvtDEFN (VariableDefn x351) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x351))
     | cvtDEFN (FunctionDefn x354) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x354))
     | cvtDEFN (ConstructorDefn x357) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x357))
     | cvtDEFN (InterfaceDefn x360) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x360))
     | cvtDEFN (NamespaceDefn x363) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x363))
     | cvtDEFN (TypeDefn x366) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x366))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls370, params=x374, settings=opt376, 
          returnType=x380, thisType=opt382, hasRest=b386}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x369 => 
                                                                              cvtIDENT x369
                                                                       ) ls370)), 
          ("params", cvtBINDINGS x374), ("settings", 
       (case opt376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x375 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x375))
       )), ("returnType", cvtTYPE_EXPR x380), ("thisType", 
       (case opt382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x381 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x381))
       )), ("hasRest", PrettyRep.Bool b386)]))
   and cvtBINDING (Binding{ident=x402, ty=opt404}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x402), ("ty", 
       (case opt404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x403 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x403))
       ))]))
   and cvtBINDING_IDENT (TempIdent n415) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n415))
     | cvtBINDING_IDENT (PropIdent x418) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x418))
   and cvtINIT_STEP (InitStep(x421, x422)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x421, 
          cvtEXPR x422]))
     | cvtINIT_STEP (AssignStep(x426, x427)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x426, cvtEXPR x427]))
   and cvtTYPE_EXPR (SpecialType x431) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x431))
     | cvtTYPE_EXPR (UnionType ls435) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x434 => 
                                                                                                           cvtTYPE_EXPR x434
                                                                                                    ) ls435)))
     | cvtTYPE_EXPR (ArrayType ls442) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x441 => 
                                                                                                           cvtTYPE_EXPR x441
                                                                                                    ) ls442)))
     | cvtTYPE_EXPR (TypeName x448) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x448))
     | cvtTYPE_EXPR (TypeRef(x451, x452)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x451, 
          cvtIDENT x452]))
     | cvtTYPE_EXPR (FunctionType{typeParams=ls457, params=ls462, result=x466, 
          thisType=opt468, hasRest=b472, requiredCount=n473}) = PrettyRep.Ctor ("FunctionType", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x456 => 
                                                                              cvtIDENT x456
                                                                       ) ls457)), 
          ("params", PrettyRep.List (List.map (fn x461 => cvtTYPE_EXPR x461
                                              ) ls462)), ("result", cvtTYPE_EXPR x466), 
          ("thisType", 
       (case opt468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x467 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x467))
       )), ("hasRest", PrettyRep.Bool b472), ("requiredCount", PrettyRep.Int n473)]))
     | cvtTYPE_EXPR (ObjectType ls490) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x489 => 
                                                                                                             cvtFIELD_TYPE x489
                                                                                                      ) ls490)))
     | cvtTYPE_EXPR (AppType{base=x496, args=ls498}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x496), ("args", PrettyRep.List (List.map (fn x497 => 
                                                                                                     cvtTYPE_EXPR x497
                                                                                              ) ls498))]))
     | cvtTYPE_EXPR (NullableType{expr=x509, nullable=b510}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x509), ("nullable", PrettyRep.Bool b510)]))
     | cvtTYPE_EXPR (InstanceType{name=x518, typeParams=ls520, ty=x524}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x518), 
          ("typeParams", PrettyRep.List (List.map (fn x519 => cvtIDENT x519
                                                  ) ls520)), ("ty", cvtTYPE_EXPR x524)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x535) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x535))
     | cvtSTMT (InitStmt{kind=x538, ns=x539, prototype=b540, static=b541, inits=ls543}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x538), 
          ("ns", cvtEXPR x539), ("prototype", PrettyRep.Bool b540), ("static", 
          PrettyRep.Bool b541), ("inits", PrettyRep.List (List.map (fn x542 => 
                                                                          cvtINIT_STEP x542
                                                                   ) ls543))]))
     | cvtSTMT (ClassBlock{ns=x560, ident=x561, name=opt563, extends=opt568, 
          fixtures=opt573, block=x577}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x560), ("ident", cvtIDENT x561), ("name", 
       (case opt563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x562 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x562))
       )), ("extends", 
       (case opt568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x567 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x567))
       )), ("fixtures", 
       (case opt573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x572 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x572))
       )), ("block", cvtBLOCK x577)]))
     | cvtSTMT (PackageBlock{name=x593, block=x594}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x593), ("block", cvtBLOCK x594)]))
     | cvtSTMT (ForEachStmt x602) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x602))
     | cvtSTMT (ForInStmt x605) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x605))
     | cvtSTMT (ThrowStmt x608) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x608))
     | cvtSTMT (ReturnStmt x611) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x611))
     | cvtSTMT (BreakStmt opt615) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x614 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x614))
       ))
     | cvtSTMT (ContinueStmt opt622) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x621 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x621))
       ))
     | cvtSTMT (BlockStmt x628) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x628))
     | cvtSTMT (LabeledStmt(x631, x632)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x631, 
          cvtSTMT x632]))
     | cvtSTMT (LetStmt(x636, x637)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [cvtBINDINGS x636, 
          cvtSTMT x637]))
     | cvtSTMT (SuperStmt x641) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x641))
     | cvtSTMT (WhileStmt x644) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x644))
     | cvtSTMT (DoWhileStmt x647) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x647))
     | cvtSTMT (ForStmt{fixtures=opt651, defns=x655, init=x656, cond=x657, 
          update=x658, contLabel=opt660, body=x664}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x650 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x650))
       )), ("defns", cvtBINDINGS x655), ("init", cvtEXPR x656), ("cond", cvtEXPR x657), 
          ("update", cvtEXPR x658), ("contLabel", 
       (case opt660 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x659 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x659))
       )), ("body", cvtSTMT x664)]))
     | cvtSTMT (IfStmt{cnd=x682, thn=x683, els=x684}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x682), ("thn", cvtSTMT x683), 
          ("els", cvtSTMT x684)]))
     | cvtSTMT (WithStmt{obj=x694, ty=x695, body=x696}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x694), ("ty", cvtTYPE_EXPR x695), 
          ("body", cvtSTMT x696)]))
     | cvtSTMT (TryStmt{body=x706, catches=ls728, finally=opt733}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x706), ("catches", PrettyRep.List (List.map (fn {bindings=x707, 
                                                                                                    ty=opt709, 
                                                                                                    fixtures=opt714, 
                                                                                                    body=x718} => 
                                                                                                    PrettyRep.Rec [("bindings", 
                                                                                                    cvtBINDINGS x707), 
                                                                                                    ("ty", 
                                                                                                    
                                                                                                 (case opt709 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x708 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtTYPE_EXPR x708))
                                                                                                 )), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt714 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x713 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x713))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x718)]
                                                                                             ) ls728)), 
          ("finally", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x732))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x746, cases=ls748}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x746), ("cases", PrettyRep.List (List.map (fn x747 => 
                                                                                                 cvtCASE x747
                                                                                          ) ls748))]))
     | cvtSTMT (SwitchTypeStmt{cond=x759, ty=x760, cases=ls762}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x759), ("ty", cvtTYPE_EXPR x760), 
          ("cases", PrettyRep.List (List.map (fn x761 => cvtTYPE_CASE x761
                                             ) ls762))]))
     | cvtSTMT (Dxns{expr=x775}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x775)]))
   and cvtEXPR (TrinaryExpr(x781, x782, x783, x784)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x781, cvtEXPR x782, cvtEXPR x783, 
          cvtEXPR x784]))
     | cvtEXPR (BinaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryTypeExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x794, cvtEXPR x795, cvtTYPE_EXPR x796]))
     | cvtEXPR (UnaryExpr(x800, x801)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x800, 
          cvtEXPR x801]))
     | cvtEXPR (TypeExpr x805) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x805))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt810) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x809))
       ))
     | cvtEXPR (SuperExpr opt817) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt817 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x816 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x816))
       ))
     | cvtEXPR (LiteralExpr x823) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x823))
     | cvtEXPR (CallExpr{func=x826, actuals=ls828}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x826), ("actuals", PrettyRep.List (List.map (fn x827 => 
                                                                                                   cvtEXPR x827
                                                                                            ) ls828))]))
     | cvtEXPR (ApplyTypeExpr{expr=x839, actuals=ls841}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x839), ("actuals", PrettyRep.List (List.map (fn x840 => 
                                                                                                   cvtTYPE_EXPR x840
                                                                                            ) ls841))]))
     | cvtEXPR (LetExpr{defs=x852, body=x853, fixtures=opt855}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x852), ("body", cvtEXPR x853), 
          ("fixtures", 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x854))
       ))]))
     | cvtEXPR (NewExpr{obj=x868, actuals=ls870}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x868), ("actuals", PrettyRep.List (List.map (fn x869 => 
                                                                                                  cvtEXPR x869
                                                                                           ) ls870))]))
     | cvtEXPR (ObjectRef{base=x881, ident=x882}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x881), ("ident", cvtIDENT_EXPR x882)]))
     | cvtEXPR (LexicalRef{ident=x890}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x890)]))
     | cvtEXPR (SetExpr(x896, x897, x898)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x896, 
          cvtEXPR x897, cvtEXPR x898]))
     | cvtEXPR (BindingExpr x902) = PrettyRep.Ctor ("BindingExpr", SOME (cvtBINDINGS x902))
     | cvtEXPR (ListExpr ls906) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x905 => 
                                                                                                    cvtEXPR x905
                                                                                             ) ls906)))
     | cvtEXPR (SliceExpr(x912, x913, x914)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x912, cvtEXPR x913, cvtEXPR x914]))
     | cvtEXPR (DefTemp(n918, x919)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n918, 
          cvtEXPR x919]))
     | cvtEXPR (GetTemp n923) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n923))
   and cvtFIXTURE_NAME (TempName n926) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n926))
     | cvtFIXTURE_NAME (PropName x929) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x929))
   and cvtIDENT_EXPR (Identifier{ident=x932, openNamespaces=ls938}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x932), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls934 => PrettyRep.List (List.map (fn x933 => 
                                                                                cvtNAMESPACE x933
                                                                         ) ls934)
                                   ) ls938))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x949, expr=x950}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x949), ("expr", cvtEXPR x950)]))
     | cvtIDENT_EXPR (AttributeIdentifier x958) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x958))
     | cvtIDENT_EXPR (ExpressionIdentifier x961) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x961))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x964, ident=x965}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x964), ("ident", cvtUSTRING x965)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x973, typeParams=ls975}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x973), ("typeParams", 
          PrettyRep.List (List.map (fn x974 => cvtTYPE_EXPR x974
                                   ) ls975))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r988) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r988))
     | cvtLITERAL (LiteralBoolean b991) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b991))
     | cvtLITERAL (LiteralString x994) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x994))
     | cvtLITERAL (LiteralArray{exprs=ls998, ty=opt1003}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x997 => 
                                                                         cvtEXPR x997
                                                                  ) ls998)), 
          ("ty", 
       (case opt1003 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1002 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1002))
       ))]))
     | cvtLITERAL (LiteralXML ls1015) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1014 => 
                                                                                                            cvtEXPR x1014
                                                                                                     ) ls1015)))
     | cvtLITERAL (LiteralNamespace x1021) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1021))
     | cvtLITERAL (LiteralObject{expr=ls1025, ty=opt1030}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1024 => 
                                                                        cvtFIELD x1024
                                                                 ) ls1025)), 
          ("ty", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1029))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1041, ty=x1042}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1041), ("ty", cvtTYPE_EXPR x1042)]))
     | cvtLITERAL (LiteralRegExp{str=x1050}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1050)]))
   and cvtBLOCK (Block x1056) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1056))
   and cvtFIXTURE (NamespaceFixture x1059) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1059))
     | cvtFIXTURE (ClassFixture x1062) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1062))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1066) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1066))
     | cvtFIXTURE (MethodFixture{ty=x1069, isOverride=b1070, isFinal=b1071}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1069), 
          ("isOverride", PrettyRep.Bool b1070), ("isFinal", PrettyRep.Bool b1071)]))
     | cvtFIXTURE (ValFixture{ty=x1081, readOnly=b1082}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1081), ("readOnly", PrettyRep.Bool b1082)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1090, getter=opt1092, setter=opt1097}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1090), ("getter", 
       (case opt1092 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1091 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1091))
       )), ("setter", 
       (case opt1097 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1096 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1096))
       ))]))
   and cvtBINDINGS (ls1111, ls1116) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1110 => 
                                                                                       cvtBINDING x1110
                                                                                ) ls1111), 
          PrettyRep.List (List.map (fn x1115 => cvtINIT_STEP x1115
                                   ) ls1116)]
   and cvtFIELD {kind=x1121, name=x1122, init=x1123} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1121), ("name", cvtIDENT_EXPR x1122), ("init", cvtEXPR x1123)]
   and cvtFIELD_TYPE {name=x1131, ty=x1132} = PrettyRep.Rec [("name", cvtIDENT x1131), 
          ("ty", cvtTYPE_EXPR x1132)]
   and cvtTYPED_IDENT {name=x1138, ty=opt1140} = PrettyRep.Rec [("name", cvtIDENT x1138), 
          ("ty", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1139))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1150, params=ls1155, result=x1159, thisType=opt1161, 
          hasRest=b1165} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1149 => 
                                                                                         cvtIDENT x1149
                                                                                  ) ls1150)), 
          ("params", PrettyRep.List (List.map (fn x1154 => cvtTYPE_EXPR x1154
                                              ) ls1155)), ("result", cvtTYPE_EXPR x1159), 
          ("thisType", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       )), ("hasRest", PrettyRep.Bool b1165)]
   and cvtFUNC_DEFN {kind=x1177, ns=x1178, final=b1179, native=b1180, override=b1181, 
          prototype=b1182, static=b1183, func=x1184} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1177), ("ns", cvtEXPR x1178), ("final", PrettyRep.Bool b1179), 
          ("native", PrettyRep.Bool b1180), ("override", PrettyRep.Bool b1181), 
          ("prototype", PrettyRep.Bool b1182), ("static", PrettyRep.Bool b1183), 
          ("func", cvtFUNC x1184)]
   and cvtCTOR_DEFN {ns=x1202, native=b1203, ctor=x1204} = PrettyRep.Rec [("ns", 
          cvtEXPR x1202), ("native", PrettyRep.Bool b1203), ("ctor", cvtCTOR x1204)]
   and cvtVAR_DEFN {kind=x1212, ns=x1213, static=b1214, prototype=b1215, bindings=ls1217} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1212), ("ns", cvtEXPR x1213), 
          ("static", PrettyRep.Bool b1214), ("prototype", PrettyRep.Bool b1215), 
          ("bindings", PrettyRep.List (List.map (fn x1216 => cvtBINDING x1216
                                                ) ls1217))]
   and cvtFIXTURES ls1235 = PrettyRep.List (List.map (fn (x1232, x1233) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1232, 
                                                            cvtFIXTURE x1233]
                                                     ) ls1235)
   and cvtINITS ls1242 = PrettyRep.List (List.map (fn (x1239, x1240) => PrettyRep.Tuple [cvtFIXTURE_NAME x1239, 
                                                         cvtEXPR x1240]
                                                  ) ls1242)
   and cvtNAMESPACE_DEFN {ident=x1246, ns=x1247, init=opt1249} = PrettyRep.Rec [("ident", 
          cvtIDENT x1246), ("ns", cvtEXPR x1247), ("init", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1248))
       ))]
   and cvtCLASS_DEFN {ident=x1260, ns=x1261, nonnullable=b1262, dynamic=b1263, 
          final=b1264, params=ls1266, extends=opt1271, implements=ls1276, body=x1280} = 
          PrettyRep.Rec [("ident", cvtIDENT x1260), ("ns", cvtEXPR x1261), 
          ("nonnullable", PrettyRep.Bool b1262), ("dynamic", PrettyRep.Bool b1263), 
          ("final", PrettyRep.Bool b1264), ("params", PrettyRep.List (List.map (fn x1265 => 
                                                                                      cvtIDENT x1265
                                                                               ) ls1266)), 
          ("extends", 
       (case opt1271 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1270 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1270))
       )), ("implements", PrettyRep.List (List.map (fn x1275 => cvtIDENT_EXPR x1275
                                                   ) ls1276)), ("body", cvtBLOCK x1280)]
   and cvtINTERFACE_DEFN {ident=x1300, ns=x1301, nonnullable=b1302, params=ls1304, 
          extends=ls1309, body=x1313} = PrettyRep.Rec [("ident", cvtIDENT x1300), 
          ("ns", cvtEXPR x1301), ("nonnullable", PrettyRep.Bool b1302), ("params", 
          PrettyRep.List (List.map (fn x1303 => cvtIDENT x1303
                                   ) ls1304)), ("extends", PrettyRep.List (List.map (fn x1308 => 
                                                                                           cvtIDENT_EXPR x1308
                                                                                    ) ls1309)), 
          ("body", cvtBLOCK x1313)]
   and cvtTYPE_DEFN {ident=x1327, ns=x1328, init=x1329} = PrettyRep.Rec [("ident", 
          cvtIDENT x1327), ("ns", cvtEXPR x1328), ("init", cvtTYPE_EXPR x1329)]
   and cvtFOR_ENUM_STMT {defns=x1337, obj=x1338, fixtures=opt1340, contLabel=opt1345, 
          body=x1349} = PrettyRep.Rec [("defns", cvtBINDINGS x1337), ("obj", 
          cvtEXPR x1338), ("fixtures", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1339))
       )), ("contLabel", 
       (case opt1345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1344 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1344))
       )), ("body", cvtSTMT x1349)]
   and cvtWHILE_STMT {cond=x1361, fixtures=opt1363, body=x1367, contLabel=opt1369} = 
          PrettyRep.Rec [("cond", cvtEXPR x1361), ("fixtures", 
       (case opt1363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1362))
       )), ("body", cvtSTMT x1367), ("contLabel", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1368))
       ))]
   and cvtDIRECTIVES {pragmas=ls1383, defns=ls1388, stmts=ls1393, fixtures=opt1398, 
          inits=opt1403} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1382 => 
                                                                                      cvtPRAGMA x1382
                                                                               ) ls1383)), 
          ("defns", PrettyRep.List (List.map (fn x1387 => cvtDEFN x1387
                                             ) ls1388)), ("stmts", PrettyRep.List (List.map (fn x1392 => 
                                                                                                   cvtSTMT x1392
                                                                                            ) ls1393)), 
          ("fixtures", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1397))
       )), ("inits", 
       (case opt1403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1402 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1402))
       ))]
   and cvtCASE {label=opt1419, fixtures=opt1424, body=x1428} = PrettyRep.Rec [("label", 
          
       (case opt1419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1418 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1418))
       )), ("fixtures", 
       (case opt1424 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1423 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1423))
       )), ("body", cvtBLOCK x1428)]
   and cvtTYPE_CASE {ty=opt1437, bindings=x1441, body=x1442} = PrettyRep.Rec [("ty", 
          
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1436))
       )), ("bindings", cvtBINDINGS x1441), ("body", cvtBLOCK x1442)]
   and cvtFUNC_NAME {kind=x1450, ident=x1451} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1450), 
          ("ident", cvtIDENT x1451)]
   and cvtPACKAGE {name=x1457, body=x1458} = PrettyRep.Rec [("name", cvtUSTRING x1457), 
          ("body", cvtBLOCK x1458)]
   and cvtPROGRAM {packages=ls1465, fixtures=opt1470, body=x1474} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1464 => cvtPACKAGE x1464
                                   ) ls1465)), ("fixtures", 
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1469))
       )), ("body", cvtBLOCK x1474)]
end

