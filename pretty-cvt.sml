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
          thisType=opt461, hasRest=b465, minArgs=n466}) = PrettyRep.Ctor ("FunctionType", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x449 => 
                                                                              cvtIDENT x449
                                                                       ) ls450)), 
          ("params", PrettyRep.List (List.map (fn x454 => cvtTYPE_EXPR x454
                                              ) ls455)), ("result", cvtTYPE_EXPR x459), 
          ("thisType", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x460 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x460))
       )), ("hasRest", PrettyRep.Bool b465), ("minArgs", PrettyRep.Int n466)]))
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
     | cvtSTMT (LetStmt x629) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x629))
     | cvtSTMT (SuperStmt x632) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x632))
     | cvtSTMT (WhileStmt x635) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x635))
     | cvtSTMT (DoWhileStmt x638) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x638))
     | cvtSTMT (ForStmt{fixtures=opt642, defn=opt647, init=ls652, cond=x656, 
          update=x657, contLabel=opt659, body=x663}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x641 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x641))
       )), ("defn", 
       (case opt647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x646 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x646))
       )), ("init", PrettyRep.List (List.map (fn x651 => cvtSTMT x651
                                             ) ls652)), ("cond", cvtEXPR x656), 
          ("update", cvtEXPR x657), ("contLabel", 
       (case opt659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x658 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x658))
       )), ("body", cvtSTMT x663)]))
     | cvtSTMT (IfStmt{cnd=x681, thn=x682, els=x683}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x681), ("thn", cvtSTMT x682), 
          ("els", cvtSTMT x683)]))
     | cvtSTMT (WithStmt{obj=x693, ty=x694, body=x695}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x693), ("ty", cvtTYPE_EXPR x694), 
          ("body", cvtSTMT x695)]))
     | cvtSTMT (TryStmt{block=x705, catches=ls727, finally=opt732}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x705), ("catches", PrettyRep.List (List.map (fn {bindings=x706, 
                                                                                                     ty=opt708, 
                                                                                                     fixtures=opt713, 
                                                                                                     block=x717} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x706), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt708 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x707 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x707))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt713 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x712 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x712))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x717)]
                                                                                              ) ls727)), 
          ("finally", 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x731))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x745, cases=ls747}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x745), ("cases", PrettyRep.List (List.map (fn x746 => 
                                                                                                 cvtCASE x746
                                                                                          ) ls747))]))
     | cvtSTMT (SwitchTypeStmt{cond=x758, ty=x759, cases=ls761}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x758), ("ty", cvtTYPE_EXPR x759), 
          ("cases", PrettyRep.List (List.map (fn x760 => cvtTYPE_CASE x760
                                             ) ls761))]))
     | cvtSTMT (Dxns{expr=x774}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x774)]))
   and cvtEXPR (TrinaryExpr(x780, x781, x782, x783)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x780, cvtEXPR x781, cvtEXPR x782, 
          cvtEXPR x783]))
     | cvtEXPR (BinaryExpr(x787, x788, x789)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x787, cvtEXPR x788, cvtEXPR x789]))
     | cvtEXPR (BinaryTypeExpr(x793, x794, x795)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x793, cvtEXPR x794, cvtTYPE_EXPR x795]))
     | cvtEXPR (UnaryExpr(x799, x800)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x799, 
          cvtEXPR x800]))
     | cvtEXPR (TypeExpr x804) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x804))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt809) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt809 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x808 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x808))
       ))
     | cvtEXPR (SuperExpr opt816) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt816 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x815 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x815))
       ))
     | cvtEXPR (LiteralExpr x822) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x822))
     | cvtEXPR (CallExpr{func=x825, actuals=ls827}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x825), ("actuals", PrettyRep.List (List.map (fn x826 => 
                                                                                                   cvtEXPR x826
                                                                                            ) ls827))]))
     | cvtEXPR (ApplyTypeExpr{expr=x838, actuals=ls840}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x838), ("actuals", PrettyRep.List (List.map (fn x839 => 
                                                                                                   cvtTYPE_EXPR x839
                                                                                            ) ls840))]))
     | cvtEXPR (LetExpr{defs=x851, body=x852, head=opt854}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x851), ("body", cvtEXPR x852), 
          ("head", 
       (case opt854 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x853 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x853))
       ))]))
     | cvtEXPR (NewExpr{obj=x867, actuals=ls869}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x867), ("actuals", PrettyRep.List (List.map (fn x868 => 
                                                                                                  cvtEXPR x868
                                                                                           ) ls869))]))
     | cvtEXPR (ObjectRef{base=x880, ident=x881}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x880), ("ident", cvtIDENT_EXPR x881)]))
     | cvtEXPR (LexicalRef{ident=x889}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x889)]))
     | cvtEXPR (SetExpr(x895, x896, x897)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x895, 
          cvtEXPR x896, cvtEXPR x897]))
     | cvtEXPR (ListExpr ls902) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x901 => 
                                                                                                    cvtEXPR x901
                                                                                             ) ls902)))
     | cvtEXPR (InitExpr x908) = PrettyRep.Ctor ("InitExpr", SOME (cvtINITS x908))
     | cvtEXPR (SliceExpr(x911, x912, x913)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x911, cvtEXPR x912, cvtEXPR x913]))
     | cvtEXPR (DefTemp(n917, x918)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n917, 
          cvtEXPR x918]))
     | cvtEXPR (GetTemp n922) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n922))
   and cvtFIXTURE_NAME (TempName n925) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n925))
     | cvtFIXTURE_NAME (PropName x928) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x928))
   and cvtIDENT_EXPR (Identifier{ident=x931, openNamespaces=ls937}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x931), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls933 => PrettyRep.List (List.map (fn x932 => 
                                                                                cvtNAMESPACE x932
                                                                         ) ls933)
                                   ) ls937))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x948, expr=x949}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x948), ("expr", cvtEXPR x949)]))
     | cvtIDENT_EXPR (AttributeIdentifier x957) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x957))
     | cvtIDENT_EXPR (ExpressionIdentifier x960) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x960))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x963, ident=x964}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x963), ("ident", cvtUSTRING x964)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x972, typeParams=ls974}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x972), ("typeParams", 
          PrettyRep.List (List.map (fn x973 => cvtTYPE_EXPR x973
                                   ) ls974))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r987) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r987))
     | cvtLITERAL (LiteralBoolean b990) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b990))
     | cvtLITERAL (LiteralString x993) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x993))
     | cvtLITERAL (LiteralArray{exprs=ls997, ty=opt1002}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x996 => 
                                                                         cvtEXPR x996
                                                                  ) ls997)), 
          ("ty", 
       (case opt1002 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1001 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1001))
       ))]))
     | cvtLITERAL (LiteralXML ls1014) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1013 => 
                                                                                                            cvtEXPR x1013
                                                                                                     ) ls1014)))
     | cvtLITERAL (LiteralNamespace x1020) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1020))
     | cvtLITERAL (LiteralObject{expr=ls1024, ty=opt1029}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1023 => 
                                                                        cvtFIELD x1023
                                                                 ) ls1024)), 
          ("ty", 
       (case opt1029 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1028 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1028))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1040, ty=x1041}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1040), ("ty", cvtTYPE_EXPR x1041)]))
     | cvtLITERAL (LiteralRegExp{str=x1049}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1049)]))
   and cvtBLOCK (Block x1055) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1055))
   and cvtFIXTURE (NamespaceFixture x1058) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1058))
     | cvtFIXTURE (ClassFixture x1061) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1061))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1065) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1065))
     | cvtFIXTURE (MethodFixture{ty=x1068, isOverride=b1069, isFinal=b1070}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1068), 
          ("isOverride", PrettyRep.Bool b1069), ("isFinal", PrettyRep.Bool b1070)]))
     | cvtFIXTURE (ValFixture{ty=x1080, readOnly=b1081}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1080), ("readOnly", PrettyRep.Bool b1081)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1089, getter=opt1091, setter=opt1096}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1089), ("getter", 
       (case opt1091 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1090 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1090))
       )), ("setter", 
       (case opt1096 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1095 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1095))
       ))]))
   and cvtBINDINGS (ls1110, ls1115) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1109 => 
                                                                                       cvtBINDING x1109
                                                                                ) ls1110), 
          PrettyRep.List (List.map (fn x1114 => cvtINIT_STEP x1114
                                   ) ls1115)]
   and cvtFIXTURES ls1123 = PrettyRep.List (List.map (fn (x1120, x1121) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1120, 
                                                            cvtFIXTURE x1121]
                                                     ) ls1123)
   and cvtINITS ls1130 = PrettyRep.List (List.map (fn (x1127, x1128) => PrettyRep.Tuple [cvtFIXTURE_NAME x1127, 
                                                         cvtEXPR x1128]
                                                  ) ls1130)
   and cvtHEAD (x1134, x1135) = PrettyRep.Tuple [cvtFIXTURES x1134, cvtINITS x1135]
   and cvtFIELD {kind=x1137, name=x1138, init=x1139} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1137), ("name", cvtIDENT_EXPR x1138), ("init", cvtEXPR x1139)]
   and cvtFIELD_TYPE {name=x1147, ty=x1148} = PrettyRep.Rec [("name", cvtIDENT x1147), 
          ("ty", cvtTYPE_EXPR x1148)]
   and cvtTYPED_IDENT {name=x1154, ty=opt1156} = PrettyRep.Rec [("name", cvtIDENT x1154), 
          ("ty", 
       (case opt1156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1155 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1155))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1166, params=ls1171, result=x1175, thisType=opt1177, 
          hasRest=b1181} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1165 => 
                                                                                         cvtIDENT x1165
                                                                                  ) ls1166)), 
          ("params", PrettyRep.List (List.map (fn x1170 => cvtTYPE_EXPR x1170
                                              ) ls1171)), ("result", cvtTYPE_EXPR x1175), 
          ("thisType", 
       (case opt1177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1176 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1176))
       )), ("hasRest", PrettyRep.Bool b1181)]
   and cvtFUNC_DEFN {kind=x1193, ns=x1194, final=b1195, native=b1196, override=b1197, 
          prototype=b1198, static=b1199, func=x1200, ty=x1201} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1193), ("ns", cvtEXPR x1194), ("final", PrettyRep.Bool b1195), 
          ("native", PrettyRep.Bool b1196), ("override", PrettyRep.Bool b1197), 
          ("prototype", PrettyRep.Bool b1198), ("static", PrettyRep.Bool b1199), 
          ("func", cvtFUNC x1200), ("ty", cvtTYPE_EXPR x1201)]
   and cvtCTOR_DEFN {ns=x1221, native=b1222, ctor=x1223} = PrettyRep.Rec [("ns", 
          cvtEXPR x1221), ("native", PrettyRep.Bool b1222), ("ctor", cvtCTOR x1223)]
   and cvtVAR_DEFN {kind=x1231, ns=x1232, static=b1233, prototype=b1234, bindings=x1235} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1231), ("ns", cvtEXPR x1232), 
          ("static", PrettyRep.Bool b1233), ("prototype", PrettyRep.Bool b1234), 
          ("bindings", cvtBINDINGS x1235)]
   and cvtNAMESPACE_DEFN {ident=x1247, ns=x1248, init=opt1250} = PrettyRep.Rec [("ident", 
          cvtIDENT x1247), ("ns", cvtEXPR x1248), ("init", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1249))
       ))]
   and cvtCLASS_DEFN {ident=x1261, ns=x1262, nonnullable=b1263, dynamic=b1264, 
          final=b1265, params=ls1267, extends=opt1272, implements=ls1277, block=x1281, 
          classDefns=ls1283, instanceDefns=ls1288} = PrettyRep.Rec [("ident", 
          cvtIDENT x1261), ("ns", cvtEXPR x1262), ("nonnullable", PrettyRep.Bool b1263), 
          ("dynamic", PrettyRep.Bool b1264), ("final", PrettyRep.Bool b1265), 
          ("params", PrettyRep.List (List.map (fn x1266 => cvtIDENT x1266
                                              ) ls1267)), ("extends", 
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1271))
       )), ("implements", PrettyRep.List (List.map (fn x1276 => cvtIDENT_EXPR x1276
                                                   ) ls1277)), ("block", cvtBLOCK x1281), 
          ("classDefns", PrettyRep.List (List.map (fn x1282 => cvtDEFN x1282
                                                  ) ls1283)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1287 => cvtDEFN x1287
                                   ) ls1288))]
   and cvtINTERFACE_DEFN {ident=x1315, ns=x1316, nonnullable=b1317, params=ls1319, 
          extends=ls1324, block=x1328} = PrettyRep.Rec [("ident", cvtIDENT x1315), 
          ("ns", cvtEXPR x1316), ("nonnullable", PrettyRep.Bool b1317), ("params", 
          PrettyRep.List (List.map (fn x1318 => cvtIDENT x1318
                                   ) ls1319)), ("extends", PrettyRep.List (List.map (fn x1323 => 
                                                                                           cvtIDENT_EXPR x1323
                                                                                    ) ls1324)), 
          ("block", cvtBLOCK x1328)]
   and cvtTYPE_DEFN {ident=x1342, ns=x1343, init=x1344} = PrettyRep.Rec [("ident", 
          cvtIDENT x1342), ("ns", cvtEXPR x1343), ("init", cvtTYPE_EXPR x1344)]
   and cvtFOR_ENUM_STMT {defn=opt1353, obj=x1357, fixtures=opt1359, inits=opt1364, 
          contLabel=opt1369, body=x1373} = PrettyRep.Rec [("defn", 
       (case opt1353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1352))
       )), ("obj", cvtEXPR x1357), ("fixtures", 
       (case opt1359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1358))
       )), ("inits", 
       (case opt1364 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1363 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1363))
       )), ("contLabel", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1368))
       )), ("body", cvtSTMT x1373)]
   and cvtWHILE_STMT {cond=x1387, fixtures=opt1389, body=x1393, contLabel=opt1395} = 
          PrettyRep.Rec [("cond", cvtEXPR x1387), ("fixtures", 
       (case opt1389 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1388 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1388))
       )), ("body", cvtSTMT x1393), ("contLabel", 
       (case opt1395 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1394 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1394))
       ))]
   and cvtDIRECTIVES {pragmas=ls1409, defns=ls1414, head=opt1419, body=ls1424} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1408 => 
                                                                     cvtPRAGMA x1408
                                                              ) ls1409)), ("defns", 
          PrettyRep.List (List.map (fn x1413 => cvtDEFN x1413
                                   ) ls1414)), ("head", 
       (case opt1419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1418 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1418))
       )), ("body", PrettyRep.List (List.map (fn x1423 => cvtSTMT x1423
                                             ) ls1424))]
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
   and cvtPACKAGE {name=x1483, block=x1484} = PrettyRep.Rec [("name", cvtUSTRING x1483), 
          ("block", cvtBLOCK x1484)]
   and cvtPROGRAM {packages=ls1491, fixtures=opt1496, block=x1500} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1490 => cvtPACKAGE x1490
                                   ) ls1491)), ("fixtures", 
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1495 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1495))
       )), ("block", cvtBLOCK x1500)]
end

