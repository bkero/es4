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
          fixtures=opt577, block=x581}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x564), ("ident", cvtIDENT x565), ("name", 
       (case opt567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x566 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x566))
       )), ("extends", 
       (case opt572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x571 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x571))
       )), ("fixtures", 
       (case opt577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x576 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x576))
       )), ("block", cvtBLOCK x581)]))
     | cvtSTMT (PackageBlock{name=x597, block=x598}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x597), ("block", cvtBLOCK x598)]))
     | cvtSTMT (ForEachStmt x606) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x606))
     | cvtSTMT (ForInStmt x609) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x609))
     | cvtSTMT (ThrowStmt x612) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x612))
     | cvtSTMT (ReturnStmt x615) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x615))
     | cvtSTMT (BreakStmt opt619) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x618 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x618))
       ))
     | cvtSTMT (ContinueStmt opt626) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x625))
       ))
     | cvtSTMT (BlockStmt x632) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x632))
     | cvtSTMT (LabeledStmt(x635, x636)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x635, 
          cvtSTMT x636]))
     | cvtSTMT (LetStmt x640) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x640))
     | cvtSTMT (WhileStmt x643) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x643))
     | cvtSTMT (DoWhileStmt x646) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x646))
     | cvtSTMT (ForStmt x649) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x649))
     | cvtSTMT (IfStmt{cnd=x652, thn=x653, els=x654}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x652), ("thn", cvtSTMT x653), 
          ("els", cvtSTMT x654)]))
     | cvtSTMT (WithStmt{obj=x664, ty=x665, body=x666}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x664), ("ty", cvtTYPE_EXPR x665), 
          ("body", cvtSTMT x666)]))
     | cvtSTMT (TryStmt{block=x676, catches=ls698, finally=opt703}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x676), ("catches", PrettyRep.List (List.map (fn {bindings=x677, 
                                                                                                     ty=opt679, 
                                                                                                     fixtures=opt684, 
                                                                                                     block=x688} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x677), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt679 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x678 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x678))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt684 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x683 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x683))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x688)]
                                                                                              ) ls698)), 
          ("finally", 
       (case opt703 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x702 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x702))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x716, cases=ls718}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x716), ("cases", PrettyRep.List (List.map (fn x717 => 
                                                                                                 cvtCASE x717
                                                                                          ) ls718))]))
     | cvtSTMT (SwitchTypeStmt{cond=x729, ty=x730, cases=ls732}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x729), ("ty", cvtTYPE_EXPR x730), 
          ("cases", PrettyRep.List (List.map (fn x731 => cvtTYPE_CASE x731
                                             ) ls732))]))
     | cvtSTMT (Dxns{expr=x745}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x745)]))
   and cvtEXPR (TrinaryExpr(x751, x752, x753, x754)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x751, cvtEXPR x752, cvtEXPR x753, 
          cvtEXPR x754]))
     | cvtEXPR (BinaryExpr(x758, x759, x760)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x758, cvtEXPR x759, cvtEXPR x760]))
     | cvtEXPR (BinaryTypeExpr(x764, x765, x766)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x764, cvtEXPR x765, cvtTYPE_EXPR x766]))
     | cvtEXPR (UnaryExpr(x770, x771)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x770, 
          cvtEXPR x771]))
     | cvtEXPR (TypeExpr x775) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x775))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt780) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x779 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x779))
       ))
     | cvtEXPR (SuperExpr opt787) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt787 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x786 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x786))
       ))
     | cvtEXPR (LiteralExpr x793) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x793))
     | cvtEXPR (CallExpr{func=x796, actuals=ls798}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x796), ("actuals", PrettyRep.List (List.map (fn x797 => 
                                                                                                   cvtEXPR x797
                                                                                            ) ls798))]))
     | cvtEXPR (ApplyTypeExpr{expr=x809, actuals=ls811}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x809), ("actuals", PrettyRep.List (List.map (fn x810 => 
                                                                                                   cvtTYPE_EXPR x810
                                                                                            ) ls811))]))
     | cvtEXPR (LetExpr{defs=x822, body=x823, head=opt825}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x822), ("body", cvtEXPR x823), 
          ("head", 
       (case opt825 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x824 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x824))
       ))]))
     | cvtEXPR (NewExpr{obj=x838, actuals=ls840}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x838), ("actuals", PrettyRep.List (List.map (fn x839 => 
                                                                                                  cvtEXPR x839
                                                                                           ) ls840))]))
     | cvtEXPR (ObjectRef{base=x851, ident=x852}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x851), ("ident", cvtIDENT_EXPR x852)]))
     | cvtEXPR (LexicalRef{ident=x860}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x860)]))
     | cvtEXPR (SetExpr(x866, x867, x868)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x866, 
          cvtEXPR x867, cvtEXPR x868]))
     | cvtEXPR (ListExpr ls873) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x872 => 
                                                                                                    cvtEXPR x872
                                                                                             ) ls873)))
     | cvtEXPR (InitExpr(x879, x880, x881)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x879, 
          cvtHEAD x880, cvtINITS x881]))
     | cvtEXPR (SliceExpr(x885, x886, x887)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x885, cvtEXPR x886, cvtEXPR x887]))
     | cvtEXPR (DefTemp(n891, x892)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n891, 
          cvtEXPR x892]))
     | cvtEXPR (GetTemp n896) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n896))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n903) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n903))
     | cvtFIXTURE_NAME (PropName x906) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x906))
   and cvtIDENT_EXPR (Identifier{ident=x909, openNamespaces=ls915}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x909), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls911 => PrettyRep.List (List.map (fn x910 => 
                                                                                cvtNAMESPACE x910
                                                                         ) ls911)
                                   ) ls915))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x926, expr=x927}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x926), ("expr", cvtEXPR x927)]))
     | cvtIDENT_EXPR (AttributeIdentifier x935) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x935))
     | cvtIDENT_EXPR (ExpressionIdentifier x938) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x938))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x941, ident=x942}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x941), ("ident", cvtUSTRING x942)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x950, typeParams=ls952}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x950), ("typeParams", 
          PrettyRep.List (List.map (fn x951 => cvtTYPE_EXPR x951
                                   ) ls952))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r965) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r965))
     | cvtLITERAL (LiteralBoolean b968) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b968))
     | cvtLITERAL (LiteralString x971) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x971))
     | cvtLITERAL (LiteralArray{exprs=ls975, ty=opt980}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x974 => 
                                                                         cvtEXPR x974
                                                                  ) ls975)), 
          ("ty", 
       (case opt980 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x979 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x979))
       ))]))
     | cvtLITERAL (LiteralXML ls992) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x991 => 
                                                                                                           cvtEXPR x991
                                                                                                    ) ls992)))
     | cvtLITERAL (LiteralNamespace x998) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x998))
     | cvtLITERAL (LiteralObject{expr=ls1002, ty=opt1007}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1001 => 
                                                                        cvtFIELD x1001
                                                                 ) ls1002)), 
          ("ty", 
       (case opt1007 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1006 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1006))
       ))]))
     | cvtLITERAL (LiteralFunction x1018) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1018))
     | cvtLITERAL (LiteralRegExp{str=x1021}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1021)]))
   and cvtBLOCK (Block x1027) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1027))
   and cvtFIXTURE (NamespaceFixture x1030) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1030))
     | cvtFIXTURE (ClassFixture x1033) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1033))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1037) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1037))
     | cvtFIXTURE (MethodFixture{func=x1040, ty=x1041, readOnly=b1042, override=b1043, 
          final=b1044}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1040), ("ty", cvtTYPE_EXPR x1041), ("readOnly", PrettyRep.Bool b1042), 
          ("override", PrettyRep.Bool b1043), ("final", PrettyRep.Bool b1044)]))
     | cvtFIXTURE (ValFixture{ty=x1058, readOnly=b1059}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1058), ("readOnly", PrettyRep.Bool b1059)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1067, getter=opt1069, setter=opt1074}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1067), ("getter", 
       (case opt1069 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1068 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1068))
       )), ("setter", 
       (case opt1074 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1073))
       ))]))
   and cvtBINDINGS (ls1088, ls1093) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1087 => 
                                                                                       cvtBINDING x1087
                                                                                ) ls1088), 
          PrettyRep.List (List.map (fn x1092 => cvtINIT_STEP x1092
                                   ) ls1093)]
   and cvtFIXTURES ls1101 = PrettyRep.List (List.map (fn (x1098, x1099) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1098, 
                                                            cvtFIXTURE x1099]
                                                     ) ls1101)
   and cvtINITS ls1108 = PrettyRep.List (List.map (fn (x1105, x1106) => PrettyRep.Tuple [cvtFIXTURE_NAME x1105, 
                                                         cvtEXPR x1106]
                                                  ) ls1108)
   and cvtHEAD (x1112, x1113) = PrettyRep.Tuple [cvtFIXTURES x1112, cvtINITS x1113]
   and cvtFIELD {kind=x1115, name=x1116, init=x1117} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1115), ("name", cvtIDENT_EXPR x1116), ("init", cvtEXPR x1117)]
   and cvtFIELD_TYPE {name=x1125, ty=x1126} = PrettyRep.Rec [("name", cvtIDENT x1125), 
          ("ty", cvtTYPE_EXPR x1126)]
   and cvtTYPED_IDENT {name=x1132, ty=opt1134} = PrettyRep.Rec [("name", cvtIDENT x1132), 
          ("ty", 
       (case opt1134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1133 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1133))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1144, params=ls1149, result=x1153, thisType=opt1155, 
          hasRest=b1159, minArgs=n1160} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1143 => 
                                                                                                        cvtIDENT x1143
                                                                                                 ) ls1144)), 
          ("params", PrettyRep.List (List.map (fn x1148 => cvtTYPE_EXPR x1148
                                              ) ls1149)), ("result", cvtTYPE_EXPR x1153), 
          ("thisType", 
       (case opt1155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1154 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1154))
       )), ("hasRest", PrettyRep.Bool b1159), ("minArgs", PrettyRep.Int n1160)]
   and cvtFUNC_DEFN {kind=x1174, ns=x1175, final=b1176, override=b1177, prototype=b1178, 
          static=b1179, func=x1180} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1174), 
          ("ns", cvtEXPR x1175), ("final", PrettyRep.Bool b1176), ("override", 
          PrettyRep.Bool b1177), ("prototype", PrettyRep.Bool b1178), ("static", 
          PrettyRep.Bool b1179), ("func", cvtFUNC x1180)]
   and cvtCTOR_DEFN {ns=x1196, ctor=x1197} = PrettyRep.Rec [("ns", cvtEXPR x1196), 
          ("ctor", cvtCTOR x1197)]
   and cvtVAR_DEFN {kind=x1203, ns=x1204, static=b1205, prototype=b1206, bindings=x1207} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1203), ("ns", cvtEXPR x1204), 
          ("static", PrettyRep.Bool b1205), ("prototype", PrettyRep.Bool b1206), 
          ("bindings", cvtBINDINGS x1207)]
   and cvtNAMESPACE_DEFN {ident=x1219, ns=x1220, init=opt1222} = PrettyRep.Rec [("ident", 
          cvtIDENT x1219), ("ns", cvtEXPR x1220), ("init", 
       (case opt1222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1221))
       ))]
   and cvtCLASS_DEFN {ident=x1233, ns=x1234, nonnullable=b1235, dynamic=b1236, 
          final=b1237, params=ls1239, extends=opt1244, implements=ls1249, classDefns=ls1254, 
          instanceDefns=ls1259, instanceStmts=ls1264, ctorDefn=opt1269} = PrettyRep.Rec [("ident", 
          cvtIDENT x1233), ("ns", cvtEXPR x1234), ("nonnullable", PrettyRep.Bool b1235), 
          ("dynamic", PrettyRep.Bool b1236), ("final", PrettyRep.Bool b1237), 
          ("params", PrettyRep.List (List.map (fn x1238 => cvtIDENT x1238
                                              ) ls1239)), ("extends", 
       (case opt1244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1243 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1243))
       )), ("implements", PrettyRep.List (List.map (fn x1248 => cvtIDENT_EXPR x1248
                                                   ) ls1249)), ("classDefns", 
          PrettyRep.List (List.map (fn x1253 => cvtDEFN x1253
                                   ) ls1254)), ("instanceDefns", PrettyRep.List (List.map (fn x1258 => 
                                                                                                 cvtDEFN x1258
                                                                                          ) ls1259)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1263 => cvtSTMT x1263
                                                     ) ls1264)), ("ctorDefn", 
          
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1268))
       ))]
   and cvtINTERFACE_DEFN {ident=x1298, ns=x1299, nonnullable=b1300, params=ls1302, 
          extends=ls1307, block=x1311} = PrettyRep.Rec [("ident", cvtIDENT x1298), 
          ("ns", cvtEXPR x1299), ("nonnullable", PrettyRep.Bool b1300), ("params", 
          PrettyRep.List (List.map (fn x1301 => cvtIDENT x1301
                                   ) ls1302)), ("extends", PrettyRep.List (List.map (fn x1306 => 
                                                                                           cvtIDENT_EXPR x1306
                                                                                    ) ls1307)), 
          ("block", cvtBLOCK x1311)]
   and cvtTYPE_DEFN {ident=x1325, ns=x1326, init=x1327} = PrettyRep.Rec [("ident", 
          cvtIDENT x1325), ("ns", cvtEXPR x1326), ("init", cvtTYPE_EXPR x1327)]
   and cvtFOR_ENUM_STMT {defn=opt1336, obj=x1340, fixtures=opt1342, inits=opt1347, 
          labels=ls1352, body=x1356} = PrettyRep.Rec [("defn", 
       (case opt1336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1335 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1335))
       )), ("obj", cvtEXPR x1340), ("fixtures", 
       (case opt1342 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1341 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1341))
       )), ("inits", 
       (case opt1347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1346))
       )), ("labels", PrettyRep.List (List.map (fn x1351 => cvtIDENT x1351
                                               ) ls1352)), ("body", cvtSTMT x1356)]
   and cvtFOR_STMT {fixtures=opt1371, defn=opt1376, init=x1380, cond=x1381, 
          update=x1382, labels=ls1384, body=x1388} = PrettyRep.Rec [("fixtures", 
          
       (case opt1371 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1370 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1370))
       )), ("defn", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1375))
       )), ("init", cvtSTMT x1380), ("cond", cvtEXPR x1381), ("update", cvtEXPR x1382), 
          ("labels", PrettyRep.List (List.map (fn x1383 => cvtIDENT x1383
                                              ) ls1384)), ("body", cvtSTMT x1388)]
   and cvtWHILE_STMT {cond=x1404, fixtures=opt1406, body=x1410, labels=ls1412} = 
          PrettyRep.Rec [("cond", cvtEXPR x1404), ("fixtures", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1405))
       )), ("body", cvtSTMT x1410), ("labels", PrettyRep.List (List.map (fn x1411 => 
                                                                               cvtIDENT x1411
                                                                        ) ls1412))]
   and cvtDIRECTIVES {pragmas=ls1426, defns=ls1431, head=opt1436, body=ls1441} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1425 => 
                                                                     cvtPRAGMA x1425
                                                              ) ls1426)), ("defns", 
          PrettyRep.List (List.map (fn x1430 => cvtDEFN x1430
                                   ) ls1431)), ("head", 
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1435))
       )), ("body", PrettyRep.List (List.map (fn x1440 => cvtSTMT x1440
                                             ) ls1441))]
   and cvtCASE {label=opt1455, inits=opt1460, body=x1464} = PrettyRep.Rec [("label", 
          
       (case opt1455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1454 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1454))
       )), ("inits", 
       (case opt1460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1459))
       )), ("body", cvtBLOCK x1464)]
   and cvtTYPE_CASE {ty=opt1473, bindings=x1477, inits=opt1479, body=x1483} = 
          PrettyRep.Rec [("ty", 
       (case opt1473 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1472 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1472))
       )), ("bindings", cvtBINDINGS x1477), ("inits", 
       (case opt1479 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1478 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1478))
       )), ("body", cvtBLOCK x1483)]
   and cvtFUNC_NAME {kind=x1493, ident=x1494} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1493), 
          ("ident", cvtIDENT x1494)]
   and cvtPACKAGE {name=x1500, block=x1501} = PrettyRep.Rec [("name", cvtUSTRING x1500), 
          ("block", cvtBLOCK x1501)]
   and cvtPROGRAM {packages=ls1508, fixtures=opt1513, block=x1517} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1507 => cvtPACKAGE x1507
                                   ) ls1508)), ("fixtures", 
       (case opt1513 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1512 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1512))
       )), ("block", cvtBLOCK x1517)]
end

