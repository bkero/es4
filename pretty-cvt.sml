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
     | cvtSTMT (ClassBlock{ns=x564, ident=x565, name=opt567, block=x571}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", cvtEXPR x564), 
          ("ident", cvtIDENT x565), ("name", 
       (case opt567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x566 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x566))
       )), ("block", cvtBLOCK x571)]))
     | cvtSTMT (PackageBlock{name=x583, block=x584}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x583), ("block", cvtBLOCK x584)]))
     | cvtSTMT (ForEachStmt x592) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x592))
     | cvtSTMT (ForInStmt x595) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x595))
     | cvtSTMT (ThrowStmt x598) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x598))
     | cvtSTMT (ReturnStmt x601) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x601))
     | cvtSTMT (BreakStmt opt605) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x604 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x604))
       ))
     | cvtSTMT (ContinueStmt opt612) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt612 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x611 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x611))
       ))
     | cvtSTMT (BlockStmt x618) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x618))
     | cvtSTMT (LabeledStmt(x621, x622)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x621, 
          cvtSTMT x622]))
     | cvtSTMT (LetStmt x626) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x626))
     | cvtSTMT (WhileStmt x629) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x629))
     | cvtSTMT (DoWhileStmt x632) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x632))
     | cvtSTMT (ForStmt x635) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x635))
     | cvtSTMT (IfStmt{cnd=x638, thn=x639, els=x640}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x638), ("thn", cvtSTMT x639), 
          ("els", cvtSTMT x640)]))
     | cvtSTMT (WithStmt{obj=x650, ty=x651, body=x652}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x650), ("ty", cvtTYPE_EXPR x651), 
          ("body", cvtSTMT x652)]))
     | cvtSTMT (TryStmt{block=x662, catches=ls684, finally=opt689}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x662), ("catches", PrettyRep.List (List.map (fn {bindings=x663, 
                                                                                                     ty=opt665, 
                                                                                                     fixtures=opt670, 
                                                                                                     block=x674} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x663), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt665 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x664 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x664))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt670 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x669 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x669))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x674)]
                                                                                              ) ls684)), 
          ("finally", 
       (case opt689 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x688 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x688))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x702, cases=ls704}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x702), ("cases", PrettyRep.List (List.map (fn x703 => 
                                                                                                 cvtCASE x703
                                                                                          ) ls704))]))
     | cvtSTMT (SwitchTypeStmt{cond=x715, ty=x716, cases=ls718}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x715), ("ty", cvtTYPE_EXPR x716), 
          ("cases", PrettyRep.List (List.map (fn x717 => cvtTYPE_CASE x717
                                             ) ls718))]))
     | cvtSTMT (Dxns{expr=x731}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x731)]))
   and cvtEXPR (TrinaryExpr(x737, x738, x739, x740)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x737, cvtEXPR x738, cvtEXPR x739, 
          cvtEXPR x740]))
     | cvtEXPR (BinaryExpr(x744, x745, x746)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x744, cvtEXPR x745, cvtEXPR x746]))
     | cvtEXPR (BinaryTypeExpr(x750, x751, x752)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x750, cvtEXPR x751, cvtTYPE_EXPR x752]))
     | cvtEXPR (UnaryExpr(x756, x757)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x756, 
          cvtEXPR x757]))
     | cvtEXPR (TypeExpr x761) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x761))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt766) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt766 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x765 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x765))
       ))
     | cvtEXPR (SuperExpr opt773) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x772 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x772))
       ))
     | cvtEXPR (LiteralExpr x779) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x779))
     | cvtEXPR (CallExpr{func=x782, actuals=ls784}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x782), ("actuals", PrettyRep.List (List.map (fn x783 => 
                                                                                                   cvtEXPR x783
                                                                                            ) ls784))]))
     | cvtEXPR (ApplyTypeExpr{expr=x795, actuals=ls797}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x795), ("actuals", PrettyRep.List (List.map (fn x796 => 
                                                                                                   cvtTYPE_EXPR x796
                                                                                            ) ls797))]))
     | cvtEXPR (LetExpr{defs=x808, body=x809, head=opt811}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x808), ("body", cvtEXPR x809), 
          ("head", 
       (case opt811 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x810 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x810))
       ))]))
     | cvtEXPR (NewExpr{obj=x824, actuals=ls826}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x824), ("actuals", PrettyRep.List (List.map (fn x825 => 
                                                                                                  cvtEXPR x825
                                                                                           ) ls826))]))
     | cvtEXPR (ObjectRef{base=x837, ident=x838}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x837), ("ident", cvtIDENT_EXPR x838)]))
     | cvtEXPR (LexicalRef{ident=x846}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x846)]))
     | cvtEXPR (SetExpr(x852, x853, x854)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x852, 
          cvtEXPR x853, cvtEXPR x854]))
     | cvtEXPR (ListExpr ls859) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x858 => 
                                                                                                    cvtEXPR x858
                                                                                             ) ls859)))
     | cvtEXPR (InitExpr(x865, x866, x867)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x865, 
          cvtHEAD x866, cvtINITS x867]))
     | cvtEXPR (SliceExpr(x871, x872, x873)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x871, cvtEXPR x872, cvtEXPR x873]))
     | cvtEXPR (DefTemp(n877, x878)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n877, 
          cvtEXPR x878]))
     | cvtEXPR (GetTemp n882) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n882))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n888) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n888))
     | cvtFIXTURE_NAME (PropName x891) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x891))
   and cvtIDENT_EXPR (Identifier{ident=x894, openNamespaces=ls900}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x894), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls896 => PrettyRep.List (List.map (fn x895 => 
                                                                                cvtNAMESPACE x895
                                                                         ) ls896)
                                   ) ls900))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x911, expr=x912}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x911), ("expr", cvtEXPR x912)]))
     | cvtIDENT_EXPR (AttributeIdentifier x920) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x920))
     | cvtIDENT_EXPR (ExpressionIdentifier x923) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x923))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x926, ident=x927}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x926), ("ident", cvtUSTRING x927)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x935, typeParams=ls937}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x935), ("typeParams", 
          PrettyRep.List (List.map (fn x936 => cvtTYPE_EXPR x936
                                   ) ls937))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r950) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r950))
     | cvtLITERAL (LiteralBoolean b953) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b953))
     | cvtLITERAL (LiteralString x956) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x956))
     | cvtLITERAL (LiteralArray{exprs=ls960, ty=opt965}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x959 => 
                                                                         cvtEXPR x959
                                                                  ) ls960)), 
          ("ty", 
       (case opt965 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x964 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x964))
       ))]))
     | cvtLITERAL (LiteralXML ls977) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x976 => 
                                                                                                           cvtEXPR x976
                                                                                                    ) ls977)))
     | cvtLITERAL (LiteralNamespace x983) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x983))
     | cvtLITERAL (LiteralObject{expr=ls987, ty=opt992}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x986 => 
                                                                        cvtFIELD x986
                                                                 ) ls987)), 
          ("ty", 
       (case opt992 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x991 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x991))
       ))]))
     | cvtLITERAL (LiteralFunction x1003) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1003))
     | cvtLITERAL (LiteralRegExp{str=x1006}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1006)]))
   and cvtBLOCK (Block x1012) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1012))
   and cvtFIXTURE (NamespaceFixture x1015) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1015))
     | cvtFIXTURE (ClassFixture x1018) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1018))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1022) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1022))
     | cvtFIXTURE (MethodFixture{func=x1025, ty=x1026, readOnly=b1027, override=b1028, 
          final=b1029}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1025), ("ty", cvtTYPE_EXPR x1026), ("readOnly", PrettyRep.Bool b1027), 
          ("override", PrettyRep.Bool b1028), ("final", PrettyRep.Bool b1029)]))
     | cvtFIXTURE (ValFixture{ty=x1043, readOnly=b1044}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1043), ("readOnly", PrettyRep.Bool b1044)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1052, getter=opt1054, setter=opt1059}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1052), ("getter", 
       (case opt1054 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1053 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1053))
       )), ("setter", 
       (case opt1059 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1058 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1058))
       ))]))
   and cvtBINDINGS (ls1073, ls1078) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1072 => 
                                                                                       cvtBINDING x1072
                                                                                ) ls1073), 
          PrettyRep.List (List.map (fn x1077 => cvtINIT_STEP x1077
                                   ) ls1078)]
   and cvtFIXTURES ls1086 = PrettyRep.List (List.map (fn (x1083, x1084) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1083, 
                                                            cvtFIXTURE x1084]
                                                     ) ls1086)
   and cvtINITS ls1093 = PrettyRep.List (List.map (fn (x1090, x1091) => PrettyRep.Tuple [cvtFIXTURE_NAME x1090, 
                                                         cvtEXPR x1091]
                                                  ) ls1093)
   and cvtHEAD (x1097, x1098) = PrettyRep.Tuple [cvtFIXTURES x1097, cvtINITS x1098]
   and cvtFIELD {kind=x1100, name=x1101, init=x1102} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1100), ("name", cvtIDENT_EXPR x1101), ("init", cvtEXPR x1102)]
   and cvtFIELD_TYPE {name=x1110, ty=x1111} = PrettyRep.Rec [("name", cvtIDENT x1110), 
          ("ty", cvtTYPE_EXPR x1111)]
   and cvtTYPED_IDENT {name=x1117, ty=opt1119} = PrettyRep.Rec [("name", cvtIDENT x1117), 
          ("ty", 
       (case opt1119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1118 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1118))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1129, params=ls1134, result=x1138, thisType=opt1140, 
          hasRest=b1144, minArgs=n1145} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1128 => 
                                                                                                        cvtIDENT x1128
                                                                                                 ) ls1129)), 
          ("params", PrettyRep.List (List.map (fn x1133 => cvtTYPE_EXPR x1133
                                              ) ls1134)), ("result", cvtTYPE_EXPR x1138), 
          ("thisType", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1139))
       )), ("hasRest", PrettyRep.Bool b1144), ("minArgs", PrettyRep.Int n1145)]
   and cvtFUNC_DEFN {kind=x1159, ns=x1160, final=b1161, override=b1162, prototype=b1163, 
          static=b1164, func=x1165} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1159), 
          ("ns", cvtEXPR x1160), ("final", PrettyRep.Bool b1161), ("override", 
          PrettyRep.Bool b1162), ("prototype", PrettyRep.Bool b1163), ("static", 
          PrettyRep.Bool b1164), ("func", cvtFUNC x1165)]
   and cvtCTOR_DEFN {ns=x1181, ctor=x1182} = PrettyRep.Rec [("ns", cvtEXPR x1181), 
          ("ctor", cvtCTOR x1182)]
   and cvtVAR_DEFN {kind=x1188, ns=x1189, static=b1190, prototype=b1191, bindings=x1192} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1188), ("ns", cvtEXPR x1189), 
          ("static", PrettyRep.Bool b1190), ("prototype", PrettyRep.Bool b1191), 
          ("bindings", cvtBINDINGS x1192)]
   and cvtNAMESPACE_DEFN {ident=x1204, ns=x1205, init=opt1207} = PrettyRep.Rec [("ident", 
          cvtIDENT x1204), ("ns", cvtEXPR x1205), ("init", 
       (case opt1207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1206 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1206))
       ))]
   and cvtCLASS_DEFN {ident=x1218, ns=x1219, nonnullable=b1220, dynamic=b1221, 
          final=b1222, params=ls1224, extends=opt1229, implements=ls1234, classDefns=ls1239, 
          instanceDefns=ls1244, instanceStmts=ls1249, ctorDefn=opt1254} = PrettyRep.Rec [("ident", 
          cvtIDENT x1218), ("ns", cvtEXPR x1219), ("nonnullable", PrettyRep.Bool b1220), 
          ("dynamic", PrettyRep.Bool b1221), ("final", PrettyRep.Bool b1222), 
          ("params", PrettyRep.List (List.map (fn x1223 => cvtIDENT x1223
                                              ) ls1224)), ("extends", 
       (case opt1229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1228 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1228))
       )), ("implements", PrettyRep.List (List.map (fn x1233 => cvtIDENT_EXPR x1233
                                                   ) ls1234)), ("classDefns", 
          PrettyRep.List (List.map (fn x1238 => cvtDEFN x1238
                                   ) ls1239)), ("instanceDefns", PrettyRep.List (List.map (fn x1243 => 
                                                                                                 cvtDEFN x1243
                                                                                          ) ls1244)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1248 => cvtSTMT x1248
                                                     ) ls1249)), ("ctorDefn", 
          
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1253))
       ))]
   and cvtINTERFACE_DEFN {ident=x1283, ns=x1284, nonnullable=b1285, params=ls1287, 
          extends=ls1292, block=x1296} = PrettyRep.Rec [("ident", cvtIDENT x1283), 
          ("ns", cvtEXPR x1284), ("nonnullable", PrettyRep.Bool b1285), ("params", 
          PrettyRep.List (List.map (fn x1286 => cvtIDENT x1286
                                   ) ls1287)), ("extends", PrettyRep.List (List.map (fn x1291 => 
                                                                                           cvtIDENT_EXPR x1291
                                                                                    ) ls1292)), 
          ("block", cvtBLOCK x1296)]
   and cvtTYPE_DEFN {ident=x1310, ns=x1311, init=x1312} = PrettyRep.Rec [("ident", 
          cvtIDENT x1310), ("ns", cvtEXPR x1311), ("init", cvtTYPE_EXPR x1312)]
   and cvtFOR_ENUM_STMT {defn=opt1321, obj=x1325, fixtures=opt1327, inits=opt1332, 
          labels=ls1337, body=x1341} = PrettyRep.Rec [("defn", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1320))
       )), ("obj", cvtEXPR x1325), ("fixtures", 
       (case opt1327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1326 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1326))
       )), ("inits", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1331))
       )), ("labels", PrettyRep.List (List.map (fn x1336 => cvtIDENT x1336
                                               ) ls1337)), ("body", cvtSTMT x1341)]
   and cvtFOR_STMT {fixtures=opt1356, defn=opt1361, init=x1365, cond=x1366, 
          update=x1367, labels=ls1369, body=x1373} = PrettyRep.Rec [("fixtures", 
          
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1355))
       )), ("defn", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1360))
       )), ("init", cvtSTMT x1365), ("cond", cvtEXPR x1366), ("update", cvtEXPR x1367), 
          ("labels", PrettyRep.List (List.map (fn x1368 => cvtIDENT x1368
                                              ) ls1369)), ("body", cvtSTMT x1373)]
   and cvtWHILE_STMT {cond=x1389, fixtures=opt1391, body=x1395, labels=ls1397} = 
          PrettyRep.Rec [("cond", cvtEXPR x1389), ("fixtures", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1390))
       )), ("body", cvtSTMT x1395), ("labels", PrettyRep.List (List.map (fn x1396 => 
                                                                               cvtIDENT x1396
                                                                        ) ls1397))]
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

