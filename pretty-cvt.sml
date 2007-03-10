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
     | cvtTYPE_EXPR (ElementTypeRef(x477, n478)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x477, PrettyRep.Int n478]))
     | cvtTYPE_EXPR (FieldTypeRef(x482, x483)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x482, cvtIDENT x483]))
     | cvtTYPE_EXPR (FunctionType x487) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x487))
     | cvtTYPE_EXPR (ObjectType ls491) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x490 => 
                                                                                                             cvtFIELD_TYPE x490
                                                                                                      ) ls491)))
     | cvtTYPE_EXPR (AppType{base=x497, args=ls499}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x497), ("args", PrettyRep.List (List.map (fn x498 => 
                                                                                                     cvtTYPE_EXPR x498
                                                                                              ) ls499))]))
     | cvtTYPE_EXPR (NullableType{expr=x510, nullable=b511}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x510), ("nullable", PrettyRep.Bool b511)]))
     | cvtTYPE_EXPR (InstanceType{name=x519, typeParams=ls521, ty=x525}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x519), 
          ("typeParams", PrettyRep.List (List.map (fn x520 => cvtIDENT x520
                                                  ) ls521)), ("ty", cvtTYPE_EXPR x525)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x536) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x536))
     | cvtSTMT (InitStmt{kind=x539, ns=x540, prototype=b541, static=b542, temps=x543, 
          inits=ls545}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x539), ("ns", cvtEXPR x540), ("prototype", PrettyRep.Bool b541), 
          ("static", PrettyRep.Bool b542), ("temps", cvtBINDINGS x543), ("inits", 
          PrettyRep.List (List.map (fn x544 => cvtINIT_STEP x544
                                   ) ls545))]))
     | cvtSTMT (ClassBlock{ns=x564, ident=x565, name=opt567, extends=opt572, 
          block=x576}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x564), ("ident", cvtIDENT x565), ("name", 
       (case opt567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x566 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x566))
       )), ("extends", 
       (case opt572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x571 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x571))
       )), ("block", cvtBLOCK x576)]))
     | cvtSTMT (PackageBlock{name=x590, block=x591}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x590), ("block", cvtBLOCK x591)]))
     | cvtSTMT (ForEachStmt x599) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x599))
     | cvtSTMT (ForInStmt x602) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x602))
     | cvtSTMT (ThrowStmt x605) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x605))
     | cvtSTMT (ReturnStmt x608) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x608))
     | cvtSTMT (BreakStmt opt612) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt612 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x611 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x611))
       ))
     | cvtSTMT (ContinueStmt opt619) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x618 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x618))
       ))
     | cvtSTMT (BlockStmt x625) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x625))
     | cvtSTMT (LabeledStmt(x628, x629)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x628, 
          cvtSTMT x629]))
     | cvtSTMT (LetStmt x633) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x633))
     | cvtSTMT (WhileStmt x636) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x636))
     | cvtSTMT (DoWhileStmt x639) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x639))
     | cvtSTMT (ForStmt x642) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x642))
     | cvtSTMT (IfStmt{cnd=x645, thn=x646, els=x647}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x645), ("thn", cvtSTMT x646), 
          ("els", cvtSTMT x647)]))
     | cvtSTMT (WithStmt{obj=x657, ty=x658, body=x659}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x657), ("ty", cvtTYPE_EXPR x658), 
          ("body", cvtSTMT x659)]))
     | cvtSTMT (TryStmt{block=x669, catches=ls691, finally=opt696}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x669), ("catches", PrettyRep.List (List.map (fn {bindings=x670, 
                                                                                                     ty=opt672, 
                                                                                                     fixtures=opt677, 
                                                                                                     block=x681} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x670), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt672 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x671 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x671))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt677 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x676 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x676))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x681)]
                                                                                              ) ls691)), 
          ("finally", 
       (case opt696 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x695 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x695))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x709, cases=ls711}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x709), ("cases", PrettyRep.List (List.map (fn x710 => 
                                                                                                 cvtCASE x710
                                                                                          ) ls711))]))
     | cvtSTMT (SwitchTypeStmt{cond=x722, ty=x723, cases=ls725}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x722), ("ty", cvtTYPE_EXPR x723), 
          ("cases", PrettyRep.List (List.map (fn x724 => cvtTYPE_CASE x724
                                             ) ls725))]))
     | cvtSTMT (Dxns{expr=x738}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x738)]))
   and cvtEXPR (TrinaryExpr(x744, x745, x746, x747)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x744, cvtEXPR x745, cvtEXPR x746, 
          cvtEXPR x747]))
     | cvtEXPR (BinaryExpr(x751, x752, x753)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x751, cvtEXPR x752, cvtEXPR x753]))
     | cvtEXPR (BinaryTypeExpr(x757, x758, x759)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x757, cvtEXPR x758, cvtTYPE_EXPR x759]))
     | cvtEXPR (UnaryExpr(x763, x764)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x763, 
          cvtEXPR x764]))
     | cvtEXPR (TypeExpr x768) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x768))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt773) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x772 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x772))
       ))
     | cvtEXPR (SuperExpr opt780) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x779 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x779))
       ))
     | cvtEXPR (LiteralExpr x786) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x786))
     | cvtEXPR (CallExpr{func=x789, actuals=ls791}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x789), ("actuals", PrettyRep.List (List.map (fn x790 => 
                                                                                                   cvtEXPR x790
                                                                                            ) ls791))]))
     | cvtEXPR (ApplyTypeExpr{expr=x802, actuals=ls804}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x802), ("actuals", PrettyRep.List (List.map (fn x803 => 
                                                                                                   cvtTYPE_EXPR x803
                                                                                            ) ls804))]))
     | cvtEXPR (LetExpr{defs=x815, body=x816, head=opt818}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x815), ("body", cvtEXPR x816), 
          ("head", 
       (case opt818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x817 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x817))
       ))]))
     | cvtEXPR (NewExpr{obj=x831, actuals=ls833}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x831), ("actuals", PrettyRep.List (List.map (fn x832 => 
                                                                                                  cvtEXPR x832
                                                                                           ) ls833))]))
     | cvtEXPR (ObjectRef{base=x844, ident=x845}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x844), ("ident", cvtIDENT_EXPR x845)]))
     | cvtEXPR (LexicalRef{ident=x853}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x853)]))
     | cvtEXPR (SetExpr(x859, x860, x861)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x859, 
          cvtEXPR x860, cvtEXPR x861]))
     | cvtEXPR (ListExpr ls866) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x865 => 
                                                                                                    cvtEXPR x865
                                                                                             ) ls866)))
     | cvtEXPR (InitExpr(x872, x873, x874)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x872, 
          cvtHEAD x873, cvtINITS x874]))
     | cvtEXPR (SliceExpr(x878, x879, x880)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x878, cvtEXPR x879, cvtEXPR x880]))
     | cvtEXPR (DefTemp(n884, x885)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n884, 
          cvtEXPR x885]))
     | cvtEXPR (GetTemp n889) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n889))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n895) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n895))
     | cvtFIXTURE_NAME (PropName x898) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x898))
   and cvtIDENT_EXPR (Identifier{ident=x901, openNamespaces=ls907}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x901), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls903 => PrettyRep.List (List.map (fn x902 => 
                                                                                cvtNAMESPACE x902
                                                                         ) ls903)
                                   ) ls907))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x918, expr=x919}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x918), ("expr", cvtEXPR x919)]))
     | cvtIDENT_EXPR (AttributeIdentifier x927) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x927))
     | cvtIDENT_EXPR (ExpressionIdentifier x930) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x930))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x933, ident=x934}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x933), ("ident", cvtUSTRING x934)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x942, typeParams=ls944}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x942), ("typeParams", 
          PrettyRep.List (List.map (fn x943 => cvtTYPE_EXPR x943
                                   ) ls944))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r957) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r957))
     | cvtLITERAL (LiteralBoolean b960) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b960))
     | cvtLITERAL (LiteralString x963) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x963))
     | cvtLITERAL (LiteralArray{exprs=ls967, ty=opt972}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x966 => 
                                                                         cvtEXPR x966
                                                                  ) ls967)), 
          ("ty", 
       (case opt972 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x971 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x971))
       ))]))
     | cvtLITERAL (LiteralXML ls984) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x983 => 
                                                                                                           cvtEXPR x983
                                                                                                    ) ls984)))
     | cvtLITERAL (LiteralNamespace x990) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x990))
     | cvtLITERAL (LiteralObject{expr=ls994, ty=opt999}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x993 => 
                                                                        cvtFIELD x993
                                                                 ) ls994)), 
          ("ty", 
       (case opt999 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x998 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x998))
       ))]))
     | cvtLITERAL (LiteralFunction x1010) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1010))
     | cvtLITERAL (LiteralRegExp{str=x1013}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1013)]))
   and cvtBLOCK (Block x1019) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1019))
   and cvtFIXTURE (NamespaceFixture x1022) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1022))
     | cvtFIXTURE (ClassFixture x1025) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1025))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1029) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1029))
     | cvtFIXTURE (MethodFixture{func=x1032, ty=x1033, readOnly=b1034, override=b1035, 
          final=b1036}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1032), ("ty", cvtTYPE_EXPR x1033), ("readOnly", PrettyRep.Bool b1034), 
          ("override", PrettyRep.Bool b1035), ("final", PrettyRep.Bool b1036)]))
     | cvtFIXTURE (ValFixture{ty=x1050, readOnly=b1051}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1050), ("readOnly", PrettyRep.Bool b1051)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1059, getter=opt1061, setter=opt1066}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1059), ("getter", 
       (case opt1061 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1060 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1060))
       )), ("setter", 
       (case opt1066 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1065 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1065))
       ))]))
   and cvtBINDINGS (ls1080, ls1085) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1079 => 
                                                                                       cvtBINDING x1079
                                                                                ) ls1080), 
          PrettyRep.List (List.map (fn x1084 => cvtINIT_STEP x1084
                                   ) ls1085)]
   and cvtFIXTURES ls1093 = PrettyRep.List (List.map (fn (x1090, x1091) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1090, 
                                                            cvtFIXTURE x1091]
                                                     ) ls1093)
   and cvtINITS ls1100 = PrettyRep.List (List.map (fn (x1097, x1098) => PrettyRep.Tuple [cvtFIXTURE_NAME x1097, 
                                                         cvtEXPR x1098]
                                                  ) ls1100)
   and cvtHEAD (x1104, x1105) = PrettyRep.Tuple [cvtFIXTURES x1104, cvtINITS x1105]
   and cvtFIELD {kind=x1107, name=x1108, init=x1109} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1107), ("name", cvtIDENT_EXPR x1108), ("init", cvtEXPR x1109)]
   and cvtFIELD_TYPE {name=x1117, ty=x1118} = PrettyRep.Rec [("name", cvtIDENT x1117), 
          ("ty", cvtTYPE_EXPR x1118)]
   and cvtTYPED_IDENT {name=x1124, ty=opt1126} = PrettyRep.Rec [("name", cvtIDENT x1124), 
          ("ty", 
       (case opt1126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1125 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1125))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1136, params=ls1141, result=x1145, thisType=opt1147, 
          hasRest=b1151, minArgs=n1152} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1135 => 
                                                                                                        cvtIDENT x1135
                                                                                                 ) ls1136)), 
          ("params", PrettyRep.List (List.map (fn x1140 => cvtTYPE_EXPR x1140
                                              ) ls1141)), ("result", cvtTYPE_EXPR x1145), 
          ("thisType", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1146))
       )), ("hasRest", PrettyRep.Bool b1151), ("minArgs", PrettyRep.Int n1152)]
   and cvtFUNC_DEFN {kind=x1166, ns=x1167, final=b1168, override=b1169, prototype=b1170, 
          static=b1171, func=x1172} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1166), 
          ("ns", cvtEXPR x1167), ("final", PrettyRep.Bool b1168), ("override", 
          PrettyRep.Bool b1169), ("prototype", PrettyRep.Bool b1170), ("static", 
          PrettyRep.Bool b1171), ("func", cvtFUNC x1172)]
   and cvtCTOR_DEFN {ns=x1188, ctor=x1189} = PrettyRep.Rec [("ns", cvtEXPR x1188), 
          ("ctor", cvtCTOR x1189)]
   and cvtVAR_DEFN {kind=x1195, ns=x1196, static=b1197, prototype=b1198, bindings=x1199} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1195), ("ns", cvtEXPR x1196), 
          ("static", PrettyRep.Bool b1197), ("prototype", PrettyRep.Bool b1198), 
          ("bindings", cvtBINDINGS x1199)]
   and cvtNAMESPACE_DEFN {ident=x1211, ns=x1212, init=opt1214} = PrettyRep.Rec [("ident", 
          cvtIDENT x1211), ("ns", cvtEXPR x1212), ("init", 
       (case opt1214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1213 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1213))
       ))]
   and cvtCLASS_DEFN {ident=x1225, ns=x1226, nonnullable=b1227, dynamic=b1228, 
          final=b1229, params=ls1231, extends=opt1236, implements=ls1241, classDefns=ls1246, 
          instanceDefns=ls1251, instanceStmts=ls1256, ctorDefn=opt1261} = PrettyRep.Rec [("ident", 
          cvtIDENT x1225), ("ns", cvtEXPR x1226), ("nonnullable", PrettyRep.Bool b1227), 
          ("dynamic", PrettyRep.Bool b1228), ("final", PrettyRep.Bool b1229), 
          ("params", PrettyRep.List (List.map (fn x1230 => cvtIDENT x1230
                                              ) ls1231)), ("extends", 
       (case opt1236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1235 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1235))
       )), ("implements", PrettyRep.List (List.map (fn x1240 => cvtIDENT_EXPR x1240
                                                   ) ls1241)), ("classDefns", 
          PrettyRep.List (List.map (fn x1245 => cvtDEFN x1245
                                   ) ls1246)), ("instanceDefns", PrettyRep.List (List.map (fn x1250 => 
                                                                                                 cvtDEFN x1250
                                                                                          ) ls1251)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1255 => cvtSTMT x1255
                                                     ) ls1256)), ("ctorDefn", 
          
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1260))
       ))]
   and cvtINTERFACE_DEFN {ident=x1290, ns=x1291, nonnullable=b1292, params=ls1294, 
          extends=ls1299, block=x1303} = PrettyRep.Rec [("ident", cvtIDENT x1290), 
          ("ns", cvtEXPR x1291), ("nonnullable", PrettyRep.Bool b1292), ("params", 
          PrettyRep.List (List.map (fn x1293 => cvtIDENT x1293
                                   ) ls1294)), ("extends", PrettyRep.List (List.map (fn x1298 => 
                                                                                           cvtIDENT_EXPR x1298
                                                                                    ) ls1299)), 
          ("block", cvtBLOCK x1303)]
   and cvtTYPE_DEFN {ident=x1317, ns=x1318, init=x1319} = PrettyRep.Rec [("ident", 
          cvtIDENT x1317), ("ns", cvtEXPR x1318), ("init", cvtTYPE_EXPR x1319)]
   and cvtFOR_ENUM_STMT {defn=opt1328, obj=x1332, fixtures=opt1334, inits=opt1339, 
          labels=ls1344, body=x1348} = PrettyRep.Rec [("defn", 
       (case opt1328 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1327 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1327))
       )), ("obj", cvtEXPR x1332), ("fixtures", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1333))
       )), ("inits", 
       (case opt1339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1338 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1338))
       )), ("labels", PrettyRep.List (List.map (fn x1343 => cvtIDENT x1343
                                               ) ls1344)), ("body", cvtSTMT x1348)]
   and cvtFOR_STMT {fixtures=opt1363, defn=opt1368, init=x1372, cond=x1373, 
          update=x1374, labels=ls1376, body=x1380} = PrettyRep.Rec [("fixtures", 
          
       (case opt1363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1362))
       )), ("defn", 
       (case opt1368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1367 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1367))
       )), ("init", cvtSTMT x1372), ("cond", cvtEXPR x1373), ("update", cvtEXPR x1374), 
          ("labels", PrettyRep.List (List.map (fn x1375 => cvtIDENT x1375
                                              ) ls1376)), ("body", cvtSTMT x1380)]
   and cvtWHILE_STMT {cond=x1396, fixtures=opt1398, body=x1402, labels=ls1404} = 
          PrettyRep.Rec [("cond", cvtEXPR x1396), ("fixtures", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1397))
       )), ("body", cvtSTMT x1402), ("labels", PrettyRep.List (List.map (fn x1403 => 
                                                                               cvtIDENT x1403
                                                                        ) ls1404))]
   and cvtDIRECTIVES {pragmas=ls1418, defns=ls1423, head=opt1428, body=ls1433} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1417 => 
                                                                     cvtPRAGMA x1417
                                                              ) ls1418)), ("defns", 
          PrettyRep.List (List.map (fn x1422 => cvtDEFN x1422
                                   ) ls1423)), ("head", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1427 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1427))
       )), ("body", PrettyRep.List (List.map (fn x1432 => cvtSTMT x1432
                                             ) ls1433))]
   and cvtCASE {label=opt1447, inits=opt1452, body=x1456} = PrettyRep.Rec [("label", 
          
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1446 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1446))
       )), ("inits", 
       (case opt1452 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1451 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1451))
       )), ("body", cvtBLOCK x1456)]
   and cvtTYPE_CASE {ty=opt1465, bindings=x1469, inits=opt1471, body=x1475} = 
          PrettyRep.Rec [("ty", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1464))
       )), ("bindings", cvtBINDINGS x1469), ("inits", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1470))
       )), ("body", cvtBLOCK x1475)]
   and cvtFUNC_NAME {kind=x1485, ident=x1486} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1485), 
          ("ident", cvtIDENT x1486)]
   and cvtPACKAGE {name=x1492, block=x1493} = PrettyRep.Rec [("name", cvtUSTRING x1492), 
          ("block", cvtBLOCK x1493)]
   and cvtPROGRAM {packages=ls1500, fixtures=opt1505, block=x1509} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1499 => cvtPACKAGE x1499
                                   ) ls1500)), ("fixtures", 
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1504 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1504))
       )), ("block", cvtBLOCK x1509)]
end

