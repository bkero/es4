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
     | cvtEXPR (LetExpr{defs=x852, body=x853, fixtures=opt855, inits=opt860}) = 
          PrettyRep.Ctor ("LetExpr", SOME (PrettyRep.Rec [("defs", cvtBINDINGS x852), 
          ("body", cvtEXPR x853), ("fixtures", 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x854))
       )), ("inits", 
       (case opt860 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x859 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x859))
       ))]))
     | cvtEXPR (NewExpr{obj=x875, actuals=ls877}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x875), ("actuals", PrettyRep.List (List.map (fn x876 => 
                                                                                                  cvtEXPR x876
                                                                                           ) ls877))]))
     | cvtEXPR (ObjectRef{base=x888, ident=x889}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x888), ("ident", cvtIDENT_EXPR x889)]))
     | cvtEXPR (LexicalRef{ident=x897}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x897)]))
     | cvtEXPR (SetExpr(x903, x904, x905)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x903, 
          cvtEXPR x904, cvtEXPR x905]))
     | cvtEXPR (BindingExpr x909) = PrettyRep.Ctor ("BindingExpr", SOME (cvtBINDINGS x909))
     | cvtEXPR (ListExpr ls913) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x912 => 
                                                                                                    cvtEXPR x912
                                                                                             ) ls913)))
     | cvtEXPR (InitExpr(b919, b920, x921)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [PrettyRep.Bool b919, 
          PrettyRep.Bool b920, cvtINITS x921]))
     | cvtEXPR (SliceExpr(x925, x926, x927)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x925, cvtEXPR x926, cvtEXPR x927]))
     | cvtEXPR (DefTemp(n931, x932)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n931, 
          cvtEXPR x932]))
     | cvtEXPR (GetTemp n936) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n936))
   and cvtFIXTURE_NAME (TempName n939) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n939))
     | cvtFIXTURE_NAME (PropName x942) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x942))
   and cvtIDENT_EXPR (Identifier{ident=x945, openNamespaces=ls951}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x945), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls947 => PrettyRep.List (List.map (fn x946 => 
                                                                                cvtNAMESPACE x946
                                                                         ) ls947)
                                   ) ls951))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x962, expr=x963}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x962), ("expr", cvtEXPR x963)]))
     | cvtIDENT_EXPR (AttributeIdentifier x971) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x971))
     | cvtIDENT_EXPR (ExpressionIdentifier x974) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x974))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x977, ident=x978}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x977), ("ident", cvtUSTRING x978)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x986, typeParams=ls988}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x986), ("typeParams", 
          PrettyRep.List (List.map (fn x987 => cvtTYPE_EXPR x987
                                   ) ls988))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r1001) = PrettyRep.Ctor ("LiteralNumber", 
          SOME (PrettyRep.Real r1001))
     | cvtLITERAL (LiteralBoolean b1004) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1004))
     | cvtLITERAL (LiteralString x1007) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1007))
     | cvtLITERAL (LiteralArray{exprs=ls1011, ty=opt1016}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1010 => 
                                                                         cvtEXPR x1010
                                                                  ) ls1011)), 
          ("ty", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1015))
       ))]))
     | cvtLITERAL (LiteralXML ls1028) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1027 => 
                                                                                                            cvtEXPR x1027
                                                                                                     ) ls1028)))
     | cvtLITERAL (LiteralNamespace x1034) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1034))
     | cvtLITERAL (LiteralObject{expr=ls1038, ty=opt1043}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1037 => 
                                                                        cvtFIELD x1037
                                                                 ) ls1038)), 
          ("ty", 
       (case opt1043 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1042 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1042))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1054, ty=x1055}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1054), ("ty", cvtTYPE_EXPR x1055)]))
     | cvtLITERAL (LiteralRegExp{str=x1063}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1063)]))
   and cvtBLOCK (Block x1069) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1069))
   and cvtFIXTURE (NamespaceFixture x1072) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1072))
     | cvtFIXTURE (ClassFixture x1075) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1075))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1079) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1079))
     | cvtFIXTURE (MethodFixture{ty=x1082, isOverride=b1083, isFinal=b1084}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1082), 
          ("isOverride", PrettyRep.Bool b1083), ("isFinal", PrettyRep.Bool b1084)]))
     | cvtFIXTURE (ValFixture{ty=x1094, readOnly=b1095}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1094), ("readOnly", PrettyRep.Bool b1095)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1103, getter=opt1105, setter=opt1110}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1103), ("getter", 
       (case opt1105 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1104 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1104))
       )), ("setter", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1109 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1109))
       ))]))
   and cvtBINDINGS (ls1124, ls1129) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1123 => 
                                                                                       cvtBINDING x1123
                                                                                ) ls1124), 
          PrettyRep.List (List.map (fn x1128 => cvtINIT_STEP x1128
                                   ) ls1129)]
   and cvtFIELD {kind=x1134, name=x1135, init=x1136} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1134), ("name", cvtIDENT_EXPR x1135), ("init", cvtEXPR x1136)]
   and cvtFIELD_TYPE {name=x1144, ty=x1145} = PrettyRep.Rec [("name", cvtIDENT x1144), 
          ("ty", cvtTYPE_EXPR x1145)]
   and cvtTYPED_IDENT {name=x1151, ty=opt1153} = PrettyRep.Rec [("name", cvtIDENT x1151), 
          ("ty", 
       (case opt1153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1152 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1152))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1163, params=ls1168, result=x1172, thisType=opt1174, 
          hasRest=b1178} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1162 => 
                                                                                         cvtIDENT x1162
                                                                                  ) ls1163)), 
          ("params", PrettyRep.List (List.map (fn x1167 => cvtTYPE_EXPR x1167
                                              ) ls1168)), ("result", cvtTYPE_EXPR x1172), 
          ("thisType", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1173))
       )), ("hasRest", PrettyRep.Bool b1178)]
   and cvtFUNC_DEFN {kind=x1190, ns=x1191, final=b1192, native=b1193, override=b1194, 
          prototype=b1195, static=b1196, func=x1197, ty=x1198} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1190), ("ns", cvtEXPR x1191), ("final", PrettyRep.Bool b1192), 
          ("native", PrettyRep.Bool b1193), ("override", PrettyRep.Bool b1194), 
          ("prototype", PrettyRep.Bool b1195), ("static", PrettyRep.Bool b1196), 
          ("func", cvtFUNC x1197), ("ty", cvtTYPE_EXPR x1198)]
   and cvtCTOR_DEFN {ns=x1218, native=b1219, ctor=x1220} = PrettyRep.Rec [("ns", 
          cvtEXPR x1218), ("native", PrettyRep.Bool b1219), ("ctor", cvtCTOR x1220)]
   and cvtVAR_DEFN {kind=x1228, ns=x1229, static=b1230, prototype=b1231, bindings=x1232} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1228), ("ns", cvtEXPR x1229), 
          ("static", PrettyRep.Bool b1230), ("prototype", PrettyRep.Bool b1231), 
          ("bindings", cvtBINDINGS x1232)]
   and cvtFIXTURES ls1247 = PrettyRep.List (List.map (fn (x1244, x1245) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1244, 
                                                            cvtFIXTURE x1245]
                                                     ) ls1247)
   and cvtINITS ls1254 = PrettyRep.List (List.map (fn (x1251, x1252) => PrettyRep.Tuple [cvtFIXTURE_NAME x1251, 
                                                         cvtEXPR x1252]
                                                  ) ls1254)
   and cvtNAMESPACE_DEFN {ident=x1258, ns=x1259, init=opt1261} = PrettyRep.Rec [("ident", 
          cvtIDENT x1258), ("ns", cvtEXPR x1259), ("init", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1260))
       ))]
   and cvtCLASS_DEFN {ident=x1272, ns=x1273, nonnullable=b1274, dynamic=b1275, 
          final=b1276, params=ls1278, extends=opt1283, implements=ls1288, body=x1292} = 
          PrettyRep.Rec [("ident", cvtIDENT x1272), ("ns", cvtEXPR x1273), 
          ("nonnullable", PrettyRep.Bool b1274), ("dynamic", PrettyRep.Bool b1275), 
          ("final", PrettyRep.Bool b1276), ("params", PrettyRep.List (List.map (fn x1277 => 
                                                                                      cvtIDENT x1277
                                                                               ) ls1278)), 
          ("extends", 
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1282))
       )), ("implements", PrettyRep.List (List.map (fn x1287 => cvtIDENT_EXPR x1287
                                                   ) ls1288)), ("body", cvtBLOCK x1292)]
   and cvtINTERFACE_DEFN {ident=x1312, ns=x1313, nonnullable=b1314, params=ls1316, 
          extends=ls1321, body=x1325} = PrettyRep.Rec [("ident", cvtIDENT x1312), 
          ("ns", cvtEXPR x1313), ("nonnullable", PrettyRep.Bool b1314), ("params", 
          PrettyRep.List (List.map (fn x1315 => cvtIDENT x1315
                                   ) ls1316)), ("extends", PrettyRep.List (List.map (fn x1320 => 
                                                                                           cvtIDENT_EXPR x1320
                                                                                    ) ls1321)), 
          ("body", cvtBLOCK x1325)]
   and cvtTYPE_DEFN {ident=x1339, ns=x1340, init=x1341} = PrettyRep.Rec [("ident", 
          cvtIDENT x1339), ("ns", cvtEXPR x1340), ("init", cvtTYPE_EXPR x1341)]
   and cvtFOR_ENUM_STMT {defns=x1349, obj=x1350, fixtures=opt1352, inits=opt1357, 
          contLabel=opt1362, body=x1366} = PrettyRep.Rec [("defns", cvtBINDINGS x1349), 
          ("obj", cvtEXPR x1350), ("fixtures", 
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1351))
       )), ("inits", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1356))
       )), ("contLabel", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1361))
       )), ("body", cvtSTMT x1366)]
   and cvtWHILE_STMT {cond=x1380, fixtures=opt1382, body=x1386, contLabel=opt1388} = 
          PrettyRep.Rec [("cond", cvtEXPR x1380), ("fixtures", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1381))
       )), ("body", cvtSTMT x1386), ("contLabel", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1387 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1387))
       ))]
   and cvtDIRECTIVES {pragmas=ls1402, defns=ls1407, stmts=ls1412, fixtures=opt1417, 
          inits=opt1422} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1401 => 
                                                                                      cvtPRAGMA x1401
                                                                               ) ls1402)), 
          ("defns", PrettyRep.List (List.map (fn x1406 => cvtDEFN x1406
                                             ) ls1407)), ("stmts", PrettyRep.List (List.map (fn x1411 => 
                                                                                                   cvtSTMT x1411
                                                                                            ) ls1412)), 
          ("fixtures", 
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1416 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1416))
       )), ("inits", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1421))
       ))]
   and cvtCASE {label=opt1438, inits=opt1443, body=x1447} = PrettyRep.Rec [("label", 
          
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1437))
       )), ("inits", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1442 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1442))
       )), ("body", cvtBLOCK x1447)]
   and cvtTYPE_CASE {ty=opt1456, bindings=x1460, inits=opt1462, body=x1466} = 
          PrettyRep.Rec [("ty", 
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1455 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1455))
       )), ("bindings", cvtBINDINGS x1460), ("inits", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1461 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1461))
       )), ("body", cvtBLOCK x1466)]
   and cvtFUNC_NAME {kind=x1476, ident=x1477} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1476), 
          ("ident", cvtIDENT x1477)]
   and cvtPACKAGE {name=x1483, body=x1484} = PrettyRep.Rec [("name", cvtUSTRING x1483), 
          ("body", cvtBLOCK x1484)]
   and cvtPROGRAM {packages=ls1491, fixtures=opt1496, body=x1500} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1490 => cvtPACKAGE x1490
                                   ) ls1491)), ("fixtures", 
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1495 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1495))
       )), ("body", cvtBLOCK x1500)]
end

