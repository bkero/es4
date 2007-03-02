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
   and cvtFUNC (Func{name=x326, fsig=x327, block=x328, param=x329}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x326), ("fsig", cvtFUNC_SIG x327), 
          ("block", cvtBLOCK x328), ("param", cvtHEAD x329)]))
   and cvtDEFN (ClassDefn x341) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x341))
     | cvtDEFN (VariableDefn x344) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x344))
     | cvtDEFN (FunctionDefn x347) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x347))
     | cvtDEFN (ConstructorDefn x350) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x350))
     | cvtDEFN (InterfaceDefn x353) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x353))
     | cvtDEFN (NamespaceDefn x356) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x356))
     | cvtDEFN (TypeDefn x359) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x359))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls363, params=x367, settings=opt369, 
          returnType=x373, thisType=opt375, hasRest=b379}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x362 => 
                                                                              cvtIDENT x362
                                                                       ) ls363)), 
          ("params", cvtBINDINGS x367), ("settings", 
       (case opt369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x368 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x368))
       )), ("returnType", cvtTYPE_EXPR x373), ("thisType", 
       (case opt375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x374 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x374))
       )), ("hasRest", PrettyRep.Bool b379)]))
   and cvtBINDING (Binding{ident=x395, ty=opt397}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x395), ("ty", 
       (case opt397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x396 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x396))
       ))]))
   and cvtBINDING_IDENT (TempIdent n408) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n408))
     | cvtBINDING_IDENT (PropIdent x411) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x411))
   and cvtINIT_STEP (InitStep(x414, x415)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x414, 
          cvtEXPR x415]))
     | cvtINIT_STEP (AssignStep(x419, x420)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x419, cvtEXPR x420]))
   and cvtTYPE_EXPR (SpecialType x424) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x424))
     | cvtTYPE_EXPR (UnionType ls428) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x427 => 
                                                                                                           cvtTYPE_EXPR x427
                                                                                                    ) ls428)))
     | cvtTYPE_EXPR (ArrayType ls435) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x434 => 
                                                                                                           cvtTYPE_EXPR x434
                                                                                                    ) ls435)))
     | cvtTYPE_EXPR (TypeName x441) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x441))
     | cvtTYPE_EXPR (TypeRef(x444, x445)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x444, 
          cvtIDENT x445]))
     | cvtTYPE_EXPR (FunctionType{typeParams=ls450, params=ls455, result=x459, 
          thisType=opt461, hasRest=b465, requiredCount=n466}) = PrettyRep.Ctor ("FunctionType", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x449 => 
                                                                              cvtIDENT x449
                                                                       ) ls450)), 
          ("params", PrettyRep.List (List.map (fn x454 => cvtTYPE_EXPR x454
                                              ) ls455)), ("result", cvtTYPE_EXPR x459), 
          ("thisType", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x460 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x460))
       )), ("hasRest", PrettyRep.Bool b465), ("requiredCount", PrettyRep.Int n466)]))
     | cvtTYPE_EXPR (ObjectType ls483) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x482 => 
                                                                                                             cvtFIELD_TYPE x482
                                                                                                      ) ls483)))
     | cvtTYPE_EXPR (AppType{base=x489, args=ls491}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x489), ("args", PrettyRep.List (List.map (fn x490 => 
                                                                                                     cvtTYPE_EXPR x490
                                                                                              ) ls491))]))
     | cvtTYPE_EXPR (NullableType{expr=x502, nullable=b503}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x502), ("nullable", PrettyRep.Bool b503)]))
     | cvtTYPE_EXPR (InstanceType{name=x511, typeParams=ls513, ty=x517}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x511), 
          ("typeParams", PrettyRep.List (List.map (fn x512 => cvtIDENT x512
                                                  ) ls513)), ("ty", cvtTYPE_EXPR x517)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x528) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x528))
     | cvtSTMT (InitStmt{kind=x531, ns=x532, prototype=b533, static=b534, inits=ls536}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x531), 
          ("ns", cvtEXPR x532), ("prototype", PrettyRep.Bool b533), ("static", 
          PrettyRep.Bool b534), ("inits", PrettyRep.List (List.map (fn x535 => 
                                                                          cvtINIT_STEP x535
                                                                   ) ls536))]))
     | cvtSTMT (ClassBlock{ns=x553, ident=x554, name=opt556, extends=opt561, 
          fixtures=opt566, block=x570}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x553), ("ident", cvtIDENT x554), ("name", 
       (case opt556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x555 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x555))
       )), ("extends", 
       (case opt561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x560 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x560))
       )), ("fixtures", 
       (case opt566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x565 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x565))
       )), ("block", cvtBLOCK x570)]))
     | cvtSTMT (PackageBlock{name=x586, block=x587}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x586), ("block", cvtBLOCK x587)]))
     | cvtSTMT (ForEachStmt x595) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x595))
     | cvtSTMT (ForInStmt x598) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x598))
     | cvtSTMT (ThrowStmt x601) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x601))
     | cvtSTMT (ReturnStmt x604) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x604))
     | cvtSTMT (BreakStmt opt608) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x607 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x607))
       ))
     | cvtSTMT (ContinueStmt opt615) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x614 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x614))
       ))
     | cvtSTMT (BlockStmt x621) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x621))
     | cvtSTMT (LabeledStmt(x624, x625)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x624, 
          cvtSTMT x625]))
     | cvtSTMT (LetStmt(x629, x630)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [cvtBINDINGS x629, 
          cvtSTMT x630]))
     | cvtSTMT (SuperStmt x634) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x634))
     | cvtSTMT (WhileStmt x637) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x637))
     | cvtSTMT (DoWhileStmt x640) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x640))
     | cvtSTMT (ForStmt{fixtures=opt644, defns=x648, init=x649, cond=x650, 
          update=x651, contLabel=opt653, body=x657}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x643 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x643))
       )), ("defns", cvtBINDINGS x648), ("init", cvtEXPR x649), ("cond", cvtEXPR x650), 
          ("update", cvtEXPR x651), ("contLabel", 
       (case opt653 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x652 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x652))
       )), ("body", cvtSTMT x657)]))
     | cvtSTMT (IfStmt{cnd=x675, thn=x676, els=x677}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x675), ("thn", cvtSTMT x676), 
          ("els", cvtSTMT x677)]))
     | cvtSTMT (WithStmt{obj=x687, ty=x688, body=x689}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x687), ("ty", cvtTYPE_EXPR x688), 
          ("body", cvtSTMT x689)]))
     | cvtSTMT (TryStmt{block=x699, catches=ls721, finally=opt726}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x699), ("catches", PrettyRep.List (List.map (fn {bindings=x700, 
                                                                                                     ty=opt702, 
                                                                                                     fixtures=opt707, 
                                                                                                     block=x711} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x700), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt702 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x701 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x701))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt707 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x706 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x706))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x711)]
                                                                                              ) ls721)), 
          ("finally", 
       (case opt726 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x725 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x725))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x739, cases=ls741}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x739), ("cases", PrettyRep.List (List.map (fn x740 => 
                                                                                                 cvtCASE x740
                                                                                          ) ls741))]))
     | cvtSTMT (SwitchTypeStmt{cond=x752, ty=x753, cases=ls755}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x752), ("ty", cvtTYPE_EXPR x753), 
          ("cases", PrettyRep.List (List.map (fn x754 => cvtTYPE_CASE x754
                                             ) ls755))]))
     | cvtSTMT (Dxns{expr=x768}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x768)]))
   and cvtEXPR (TrinaryExpr(x774, x775, x776, x777)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x774, cvtEXPR x775, cvtEXPR x776, 
          cvtEXPR x777]))
     | cvtEXPR (BinaryExpr(x781, x782, x783)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x781, cvtEXPR x782, cvtEXPR x783]))
     | cvtEXPR (BinaryTypeExpr(x787, x788, x789)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x787, cvtEXPR x788, cvtTYPE_EXPR x789]))
     | cvtEXPR (UnaryExpr(x793, x794)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x793, 
          cvtEXPR x794]))
     | cvtEXPR (TypeExpr x798) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x798))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt803) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt803 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x802 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x802))
       ))
     | cvtEXPR (SuperExpr opt810) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x809))
       ))
     | cvtEXPR (LiteralExpr x816) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x816))
     | cvtEXPR (CallExpr{func=x819, actuals=ls821}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x819), ("actuals", PrettyRep.List (List.map (fn x820 => 
                                                                                                   cvtEXPR x820
                                                                                            ) ls821))]))
     | cvtEXPR (ApplyTypeExpr{expr=x832, actuals=ls834}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x832), ("actuals", PrettyRep.List (List.map (fn x833 => 
                                                                                                   cvtTYPE_EXPR x833
                                                                                            ) ls834))]))
     | cvtEXPR (LetExpr{defs=x845, body=x846, fixtures=opt848, inits=opt853}) = 
          PrettyRep.Ctor ("LetExpr", SOME (PrettyRep.Rec [("defs", cvtBINDINGS x845), 
          ("body", cvtEXPR x846), ("fixtures", 
       (case opt848 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x847 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x847))
       )), ("inits", 
       (case opt853 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x852 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x852))
       ))]))
     | cvtEXPR (BindingExpr x868) = PrettyRep.Ctor ("BindingExpr", SOME (cvtBINDINGS x868))
     | cvtEXPR (NewExpr{obj=x871, actuals=ls873}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x871), ("actuals", PrettyRep.List (List.map (fn x872 => 
                                                                                                  cvtEXPR x872
                                                                                           ) ls873))]))
     | cvtEXPR (ObjectRef{base=x884, ident=x885}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x884), ("ident", cvtIDENT_EXPR x885)]))
     | cvtEXPR (LexicalRef{ident=x893}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x893)]))
     | cvtEXPR (SetExpr(x899, x900, x901)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x899, 
          cvtEXPR x900, cvtEXPR x901]))
     | cvtEXPR (ListExpr ls906) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x905 => 
                                                                                                    cvtEXPR x905
                                                                                             ) ls906)))
     | cvtEXPR (InitExpr(b912, b913, x914)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [PrettyRep.Bool b912, 
          PrettyRep.Bool b913, cvtINITS x914]))
     | cvtEXPR (SliceExpr(x918, x919, x920)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x918, cvtEXPR x919, cvtEXPR x920]))
     | cvtEXPR (DefTemp(n924, x925)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n924, 
          cvtEXPR x925]))
     | cvtEXPR (GetTemp n929) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n929))
   and cvtFIXTURE_NAME (TempName n932) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n932))
     | cvtFIXTURE_NAME (PropName x935) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x935))
   and cvtIDENT_EXPR (Identifier{ident=x938, openNamespaces=ls944}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x938), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls940 => PrettyRep.List (List.map (fn x939 => 
                                                                                cvtNAMESPACE x939
                                                                         ) ls940)
                                   ) ls944))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x955, expr=x956}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x955), ("expr", cvtEXPR x956)]))
     | cvtIDENT_EXPR (AttributeIdentifier x964) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x964))
     | cvtIDENT_EXPR (ExpressionIdentifier x967) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x967))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x970, ident=x971}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x970), ("ident", cvtUSTRING x971)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x979, typeParams=ls981}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x979), ("typeParams", 
          PrettyRep.List (List.map (fn x980 => cvtTYPE_EXPR x980
                                   ) ls981))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r994) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r994))
     | cvtLITERAL (LiteralBoolean b997) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b997))
     | cvtLITERAL (LiteralString x1000) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1000))
     | cvtLITERAL (LiteralArray{exprs=ls1004, ty=opt1009}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1003 => 
                                                                         cvtEXPR x1003
                                                                  ) ls1004)), 
          ("ty", 
       (case opt1009 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1008 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1008))
       ))]))
     | cvtLITERAL (LiteralXML ls1021) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1020 => 
                                                                                                            cvtEXPR x1020
                                                                                                     ) ls1021)))
     | cvtLITERAL (LiteralNamespace x1027) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1027))
     | cvtLITERAL (LiteralObject{expr=ls1031, ty=opt1036}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1030 => 
                                                                        cvtFIELD x1030
                                                                 ) ls1031)), 
          ("ty", 
       (case opt1036 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1035 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1035))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1047, ty=x1048}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1047), ("ty", cvtTYPE_EXPR x1048)]))
     | cvtLITERAL (LiteralRegExp{str=x1056}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1056)]))
   and cvtBLOCK (Block x1062) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1062))
   and cvtFIXTURE (NamespaceFixture x1065) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1065))
     | cvtFIXTURE (ClassFixture x1068) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1068))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1072) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1072))
     | cvtFIXTURE (MethodFixture{ty=x1075, isOverride=b1076, isFinal=b1077}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1075), 
          ("isOverride", PrettyRep.Bool b1076), ("isFinal", PrettyRep.Bool b1077)]))
     | cvtFIXTURE (ValFixture{ty=x1087, readOnly=b1088}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1087), ("readOnly", PrettyRep.Bool b1088)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1096, getter=opt1098, setter=opt1103}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1096), ("getter", 
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1097))
       )), ("setter", 
       (case opt1103 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1102 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1102))
       ))]))
   and cvtBINDINGS (ls1117, ls1122) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1116 => 
                                                                                       cvtBINDING x1116
                                                                                ) ls1117), 
          PrettyRep.List (List.map (fn x1121 => cvtINIT_STEP x1121
                                   ) ls1122)]
   and cvtFIXTURES ls1130 = PrettyRep.List (List.map (fn (x1127, x1128) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1127, 
                                                            cvtFIXTURE x1128]
                                                     ) ls1130)
   and cvtINITS ls1137 = PrettyRep.List (List.map (fn (x1134, x1135) => PrettyRep.Tuple [cvtFIXTURE_NAME x1134, 
                                                         cvtEXPR x1135]
                                                  ) ls1137)
   and cvtHEAD (x1141, x1142) = PrettyRep.Tuple [cvtFIXTURES x1141, cvtINITS x1142]
   and cvtFIELD {kind=x1144, name=x1145, init=x1146} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1144), ("name", cvtIDENT_EXPR x1145), ("init", cvtEXPR x1146)]
   and cvtFIELD_TYPE {name=x1154, ty=x1155} = PrettyRep.Rec [("name", cvtIDENT x1154), 
          ("ty", cvtTYPE_EXPR x1155)]
   and cvtTYPED_IDENT {name=x1161, ty=opt1163} = PrettyRep.Rec [("name", cvtIDENT x1161), 
          ("ty", 
       (case opt1163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1162 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1162))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1173, params=ls1178, result=x1182, thisType=opt1184, 
          hasRest=b1188} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1172 => 
                                                                                         cvtIDENT x1172
                                                                                  ) ls1173)), 
          ("params", PrettyRep.List (List.map (fn x1177 => cvtTYPE_EXPR x1177
                                              ) ls1178)), ("result", cvtTYPE_EXPR x1182), 
          ("thisType", 
       (case opt1184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1183 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1183))
       )), ("hasRest", PrettyRep.Bool b1188)]
   and cvtFUNC_DEFN {kind=x1200, ns=x1201, final=b1202, native=b1203, override=b1204, 
          prototype=b1205, static=b1206, func=x1207, ty=x1208} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1200), ("ns", cvtEXPR x1201), ("final", PrettyRep.Bool b1202), 
          ("native", PrettyRep.Bool b1203), ("override", PrettyRep.Bool b1204), 
          ("prototype", PrettyRep.Bool b1205), ("static", PrettyRep.Bool b1206), 
          ("func", cvtFUNC x1207), ("ty", cvtTYPE_EXPR x1208)]
   and cvtCTOR_DEFN {ns=x1228, native=b1229, ctor=x1230} = PrettyRep.Rec [("ns", 
          cvtEXPR x1228), ("native", PrettyRep.Bool b1229), ("ctor", cvtCTOR x1230)]
   and cvtVAR_DEFN {kind=x1238, ns=x1239, static=b1240, prototype=b1241, bindings=x1242} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1238), ("ns", cvtEXPR x1239), 
          ("static", PrettyRep.Bool b1240), ("prototype", PrettyRep.Bool b1241), 
          ("bindings", cvtBINDINGS x1242)]
   and cvtNAMESPACE_DEFN {ident=x1254, ns=x1255, init=opt1257} = PrettyRep.Rec [("ident", 
          cvtIDENT x1254), ("ns", cvtEXPR x1255), ("init", 
       (case opt1257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1256 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1256))
       ))]
   and cvtCLASS_DEFN {ident=x1268, ns=x1269, nonnullable=b1270, dynamic=b1271, 
          final=b1272, params=ls1274, extends=opt1279, implements=ls1284, block=x1288, 
          classDefns=ls1290, instanceDefns=ls1295} = PrettyRep.Rec [("ident", 
          cvtIDENT x1268), ("ns", cvtEXPR x1269), ("nonnullable", PrettyRep.Bool b1270), 
          ("dynamic", PrettyRep.Bool b1271), ("final", PrettyRep.Bool b1272), 
          ("params", PrettyRep.List (List.map (fn x1273 => cvtIDENT x1273
                                              ) ls1274)), ("extends", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1278))
       )), ("implements", PrettyRep.List (List.map (fn x1283 => cvtIDENT_EXPR x1283
                                                   ) ls1284)), ("block", cvtBLOCK x1288), 
          ("classDefns", PrettyRep.List (List.map (fn x1289 => cvtDEFN x1289
                                                  ) ls1290)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1294 => cvtDEFN x1294
                                   ) ls1295))]
   and cvtINTERFACE_DEFN {ident=x1322, ns=x1323, nonnullable=b1324, params=ls1326, 
          extends=ls1331, block=x1335} = PrettyRep.Rec [("ident", cvtIDENT x1322), 
          ("ns", cvtEXPR x1323), ("nonnullable", PrettyRep.Bool b1324), ("params", 
          PrettyRep.List (List.map (fn x1325 => cvtIDENT x1325
                                   ) ls1326)), ("extends", PrettyRep.List (List.map (fn x1330 => 
                                                                                           cvtIDENT_EXPR x1330
                                                                                    ) ls1331)), 
          ("block", cvtBLOCK x1335)]
   and cvtTYPE_DEFN {ident=x1349, ns=x1350, init=x1351} = PrettyRep.Rec [("ident", 
          cvtIDENT x1349), ("ns", cvtEXPR x1350), ("init", cvtTYPE_EXPR x1351)]
   and cvtFOR_ENUM_STMT {defns=x1359, obj=x1360, fixtures=opt1362, inits=opt1367, 
          contLabel=opt1372, body=x1376} = PrettyRep.Rec [("defns", cvtBINDINGS x1359), 
          ("obj", cvtEXPR x1360), ("fixtures", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1361))
       )), ("inits", 
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1366))
       )), ("contLabel", 
       (case opt1372 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1371 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1371))
       )), ("body", cvtSTMT x1376)]
   and cvtWHILE_STMT {cond=x1390, fixtures=opt1392, body=x1396, contLabel=opt1398} = 
          PrettyRep.Rec [("cond", cvtEXPR x1390), ("fixtures", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1391 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1391))
       )), ("body", cvtSTMT x1396), ("contLabel", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1397))
       ))]
   and cvtDIRECTIVES {pragmas=ls1412, defns=ls1417, head=opt1422, body=ls1427} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1411 => 
                                                                     cvtPRAGMA x1411
                                                              ) ls1412)), ("defns", 
          PrettyRep.List (List.map (fn x1416 => cvtDEFN x1416
                                   ) ls1417)), ("head", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1421))
       )), ("body", PrettyRep.List (List.map (fn x1426 => cvtSTMT x1426
                                             ) ls1427))]
   and cvtCASE {label=opt1441, inits=opt1446, body=x1450} = PrettyRep.Rec [("label", 
          
       (case opt1441 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1440))
       )), ("inits", 
       (case opt1446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1445 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1445))
       )), ("body", cvtBLOCK x1450)]
   and cvtTYPE_CASE {ty=opt1459, bindings=x1463, inits=opt1465, body=x1469} = 
          PrettyRep.Rec [("ty", 
       (case opt1459 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1458 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1458))
       )), ("bindings", cvtBINDINGS x1463), ("inits", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1464))
       )), ("body", cvtBLOCK x1469)]
   and cvtFUNC_NAME {kind=x1479, ident=x1480} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1479), 
          ("ident", cvtIDENT x1480)]
   and cvtPACKAGE {name=x1486, block=x1487} = PrettyRep.Rec [("name", cvtUSTRING x1486), 
          ("block", cvtBLOCK x1487)]
   and cvtPROGRAM {packages=ls1494, fixtures=opt1499, block=x1503} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1493 => cvtPACKAGE x1493
                                   ) ls1494)), ("fixtures", 
       (case opt1499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1498 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1498))
       )), ("block", cvtBLOCK x1503)]
end

