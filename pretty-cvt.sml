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
   and cvtFUNC (Func{name=x333, fsig=x334, block=x335, param=x336, defaults=ls338, 
          ty=x342}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x333), ("fsig", cvtFUNC_SIG x334), ("block", cvtBLOCK x335), 
          ("param", cvtHEAD x336), ("defaults", PrettyRep.List (List.map (fn x337 => 
                                                                                cvtEXPR x337
                                                                         ) ls338)), 
          ("ty", cvtFUNC_TYPE x342)]))
   and cvtDEFN (ClassDefn x358) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x358))
     | cvtDEFN (VariableDefn x361) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x361))
     | cvtDEFN (FunctionDefn x364) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x364))
     | cvtDEFN (ConstructorDefn x367) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x367))
     | cvtDEFN (InterfaceDefn x370) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x370))
     | cvtDEFN (NamespaceDefn x373) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x373))
     | cvtDEFN (TypeDefn x376) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x376))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls380, params=x384, defaults=ls386, 
          ctorInits=opt397, returnType=x401, thisType=opt403, hasRest=b407}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x379 => cvtIDENT x379
                                   ) ls380)), ("params", cvtBINDINGS x384), 
          ("defaults", PrettyRep.List (List.map (fn x385 => cvtEXPR x385
                                                ) ls386)), ("ctorInits", 
       (case opt397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x390, ls392) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x390, 
            PrettyRep.List (List.map (fn x391 => cvtEXPR x391
                                     ) ls392)]))
       )), ("returnType", cvtTYPE_EXPR x401), ("thisType", 
       (case opt403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x402 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x402))
       )), ("hasRest", PrettyRep.Bool b407)]))
   and cvtBINDING (Binding{ident=x425, ty=opt427}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x425), ("ty", 
       (case opt427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x426 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x426))
       ))]))
   and cvtBINDING_IDENT (TempIdent n438) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n438))
     | cvtBINDING_IDENT (PropIdent x441) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x441))
   and cvtINIT_STEP (InitStep(x444, x445)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x444, 
          cvtEXPR x445]))
     | cvtINIT_STEP (AssignStep(x449, x450)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x449, cvtEXPR x450]))
   and cvtTYPE_EXPR (SpecialType x454) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x454))
     | cvtTYPE_EXPR (UnionType ls458) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x457 => 
                                                                                                           cvtTYPE_EXPR x457
                                                                                                    ) ls458)))
     | cvtTYPE_EXPR (ArrayType ls465) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x464 => 
                                                                                                           cvtTYPE_EXPR x464
                                                                                                    ) ls465)))
     | cvtTYPE_EXPR (TypeName x471) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x471))
     | cvtTYPE_EXPR (TypeRef(x474, x475)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x474, 
          cvtIDENT x475]))
     | cvtTYPE_EXPR (FunctionType x479) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x479))
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
     | cvtSTMT (InitStmt{kind=x531, ns=x532, prototype=b533, static=b534, temps=x535, 
          inits=ls537}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x531), ("ns", cvtEXPR x532), ("prototype", PrettyRep.Bool b533), 
          ("static", PrettyRep.Bool b534), ("temps", cvtBINDINGS x535), ("inits", 
          PrettyRep.List (List.map (fn x536 => cvtINIT_STEP x536
                                   ) ls537))]))
     | cvtSTMT (ClassBlock{ns=x556, ident=x557, name=opt559, extends=opt564, 
          fixtures=opt569, block=x573}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x556), ("ident", cvtIDENT x557), ("name", 
       (case opt559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x558 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x558))
       )), ("extends", 
       (case opt564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x563 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x563))
       )), ("fixtures", 
       (case opt569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x568 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x568))
       )), ("block", cvtBLOCK x573)]))
     | cvtSTMT (PackageBlock{name=x589, block=x590}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x589), ("block", cvtBLOCK x590)]))
     | cvtSTMT (ForEachStmt x598) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x598))
     | cvtSTMT (ForInStmt x601) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x601))
     | cvtSTMT (ThrowStmt x604) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x604))
     | cvtSTMT (ReturnStmt x607) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x607))
     | cvtSTMT (BreakStmt opt611) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x610 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x610))
       ))
     | cvtSTMT (ContinueStmt opt618) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x617 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x617))
       ))
     | cvtSTMT (BlockStmt x624) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x624))
     | cvtSTMT (LabeledStmt(x627, x628)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x627, 
          cvtSTMT x628]))
     | cvtSTMT (LetStmt x632) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x632))
     | cvtSTMT (SuperStmt x635) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x635))
     | cvtSTMT (WhileStmt x638) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x638))
     | cvtSTMT (DoWhileStmt x641) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x641))
     | cvtSTMT (ForStmt x644) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x644))
     | cvtSTMT (IfStmt{cnd=x647, thn=x648, els=x649}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x647), ("thn", cvtSTMT x648), 
          ("els", cvtSTMT x649)]))
     | cvtSTMT (WithStmt{obj=x659, ty=x660, body=x661}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x659), ("ty", cvtTYPE_EXPR x660), 
          ("body", cvtSTMT x661)]))
     | cvtSTMT (TryStmt{block=x671, catches=ls693, finally=opt698}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x671), ("catches", PrettyRep.List (List.map (fn {bindings=x672, 
                                                                                                     ty=opt674, 
                                                                                                     fixtures=opt679, 
                                                                                                     block=x683} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x672), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt674 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x673 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x673))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt679 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x678 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x678))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x683)]
                                                                                              ) ls693)), 
          ("finally", 
       (case opt698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x697 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x697))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x711, cases=ls713}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x711), ("cases", PrettyRep.List (List.map (fn x712 => 
                                                                                                 cvtCASE x712
                                                                                          ) ls713))]))
     | cvtSTMT (SwitchTypeStmt{cond=x724, ty=x725, cases=ls727}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x724), ("ty", cvtTYPE_EXPR x725), 
          ("cases", PrettyRep.List (List.map (fn x726 => cvtTYPE_CASE x726
                                             ) ls727))]))
     | cvtSTMT (Dxns{expr=x740}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x740)]))
   and cvtEXPR (TrinaryExpr(x746, x747, x748, x749)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x746, cvtEXPR x747, cvtEXPR x748, 
          cvtEXPR x749]))
     | cvtEXPR (BinaryExpr(x753, x754, x755)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x753, cvtEXPR x754, cvtEXPR x755]))
     | cvtEXPR (BinaryTypeExpr(x759, x760, x761)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x759, cvtEXPR x760, cvtTYPE_EXPR x761]))
     | cvtEXPR (UnaryExpr(x765, x766)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x765, 
          cvtEXPR x766]))
     | cvtEXPR (TypeExpr x770) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x770))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt775) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt775 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x774 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x774))
       ))
     | cvtEXPR (SuperExpr opt782) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt782 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x781 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x781))
       ))
     | cvtEXPR (LiteralExpr x788) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x788))
     | cvtEXPR (CallExpr{func=x791, actuals=ls793}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x791), ("actuals", PrettyRep.List (List.map (fn x792 => 
                                                                                                   cvtEXPR x792
                                                                                            ) ls793))]))
     | cvtEXPR (ApplyTypeExpr{expr=x804, actuals=ls806}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x804), ("actuals", PrettyRep.List (List.map (fn x805 => 
                                                                                                   cvtTYPE_EXPR x805
                                                                                            ) ls806))]))
     | cvtEXPR (LetExpr{defs=x817, body=x818, head=opt820}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x817), ("body", cvtEXPR x818), 
          ("head", 
       (case opt820 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x819 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x819))
       ))]))
     | cvtEXPR (NewExpr{obj=x833, actuals=ls835}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x833), ("actuals", PrettyRep.List (List.map (fn x834 => 
                                                                                                  cvtEXPR x834
                                                                                           ) ls835))]))
     | cvtEXPR (ObjectRef{base=x846, ident=x847}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x846), ("ident", cvtIDENT_EXPR x847)]))
     | cvtEXPR (LexicalRef{ident=x855}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x855)]))
     | cvtEXPR (SetExpr(x861, x862, x863)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x861, 
          cvtEXPR x862, cvtEXPR x863]))
     | cvtEXPR (ListExpr ls868) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x867 => 
                                                                                                    cvtEXPR x867
                                                                                             ) ls868)))
     | cvtEXPR (InitExpr(x874, x875, x876)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x874, 
          cvtHEAD x875, cvtINITS x876]))
     | cvtEXPR (SliceExpr(x880, x881, x882)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x880, cvtEXPR x881, cvtEXPR x882]))
     | cvtEXPR (DefTemp(n886, x887)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n886, 
          cvtEXPR x887]))
     | cvtEXPR (GetTemp n891) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n891))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n898) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n898))
     | cvtFIXTURE_NAME (PropName x901) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x901))
   and cvtIDENT_EXPR (Identifier{ident=x904, openNamespaces=ls910}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x904), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls906 => PrettyRep.List (List.map (fn x905 => 
                                                                                cvtNAMESPACE x905
                                                                         ) ls906)
                                   ) ls910))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x921, expr=x922}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x921), ("expr", cvtEXPR x922)]))
     | cvtIDENT_EXPR (AttributeIdentifier x930) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x930))
     | cvtIDENT_EXPR (ExpressionIdentifier x933) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x933))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x936, ident=x937}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x936), ("ident", cvtUSTRING x937)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x945, typeParams=ls947}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x945), ("typeParams", 
          PrettyRep.List (List.map (fn x946 => cvtTYPE_EXPR x946
                                   ) ls947))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r960) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r960))
     | cvtLITERAL (LiteralBoolean b963) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b963))
     | cvtLITERAL (LiteralString x966) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x966))
     | cvtLITERAL (LiteralArray{exprs=ls970, ty=opt975}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x969 => 
                                                                         cvtEXPR x969
                                                                  ) ls970)), 
          ("ty", 
       (case opt975 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x974 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x974))
       ))]))
     | cvtLITERAL (LiteralXML ls987) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x986 => 
                                                                                                           cvtEXPR x986
                                                                                                    ) ls987)))
     | cvtLITERAL (LiteralNamespace x993) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x993))
     | cvtLITERAL (LiteralObject{expr=ls997, ty=opt1002}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x996 => 
                                                                        cvtFIELD x996
                                                                 ) ls997)), 
          ("ty", 
       (case opt1002 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1001 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1001))
       ))]))
     | cvtLITERAL (LiteralFunction x1013) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1013))
     | cvtLITERAL (LiteralRegExp{str=x1016}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1016)]))
   and cvtBLOCK (Block x1022) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1022))
   and cvtFIXTURE (NamespaceFixture x1025) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1025))
     | cvtFIXTURE (ClassFixture x1028) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1028))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1032) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1032))
     | cvtFIXTURE (MethodFixture{func=x1035, ty=x1036, readOnly=b1037, override=b1038, 
          final=b1039}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1035), ("ty", cvtTYPE_EXPR x1036), ("readOnly", PrettyRep.Bool b1037), 
          ("override", PrettyRep.Bool b1038), ("final", PrettyRep.Bool b1039)]))
     | cvtFIXTURE (ValFixture{ty=x1053, readOnly=b1054}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1053), ("readOnly", PrettyRep.Bool b1054)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1062, getter=opt1064, setter=opt1069}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1062), ("getter", 
       (case opt1064 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1063 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1063))
       )), ("setter", 
       (case opt1069 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1068 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1068))
       ))]))
   and cvtBINDINGS (ls1083, ls1088) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1082 => 
                                                                                       cvtBINDING x1082
                                                                                ) ls1083), 
          PrettyRep.List (List.map (fn x1087 => cvtINIT_STEP x1087
                                   ) ls1088)]
   and cvtFIXTURES ls1096 = PrettyRep.List (List.map (fn (x1093, x1094) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1093, 
                                                            cvtFIXTURE x1094]
                                                     ) ls1096)
   and cvtINITS ls1103 = PrettyRep.List (List.map (fn (x1100, x1101) => PrettyRep.Tuple [cvtFIXTURE_NAME x1100, 
                                                         cvtEXPR x1101]
                                                  ) ls1103)
   and cvtHEAD (x1107, x1108) = PrettyRep.Tuple [cvtFIXTURES x1107, cvtINITS x1108]
   and cvtFIELD {kind=x1110, name=x1111, init=x1112} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1110), ("name", cvtIDENT_EXPR x1111), ("init", cvtEXPR x1112)]
   and cvtFIELD_TYPE {name=x1120, ty=x1121} = PrettyRep.Rec [("name", cvtIDENT x1120), 
          ("ty", cvtTYPE_EXPR x1121)]
   and cvtTYPED_IDENT {name=x1127, ty=opt1129} = PrettyRep.Rec [("name", cvtIDENT x1127), 
          ("ty", 
       (case opt1129 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1128 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1128))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1139, params=ls1144, result=x1148, thisType=opt1150, 
          hasRest=b1154, minArgs=n1155} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1138 => 
                                                                                                        cvtIDENT x1138
                                                                                                 ) ls1139)), 
          ("params", PrettyRep.List (List.map (fn x1143 => cvtTYPE_EXPR x1143
                                              ) ls1144)), ("result", cvtTYPE_EXPR x1148), 
          ("thisType", 
       (case opt1150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1149 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1149))
       )), ("hasRest", PrettyRep.Bool b1154), ("minArgs", PrettyRep.Int n1155)]
   and cvtFUNC_DEFN {kind=x1169, ns=x1170, final=b1171, native=b1172, override=b1173, 
          prototype=b1174, static=b1175, func=x1176} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1169), ("ns", cvtEXPR x1170), ("final", PrettyRep.Bool b1171), 
          ("native", PrettyRep.Bool b1172), ("override", PrettyRep.Bool b1173), 
          ("prototype", PrettyRep.Bool b1174), ("static", PrettyRep.Bool b1175), 
          ("func", cvtFUNC x1176)]
   and cvtCTOR_DEFN {ns=x1194, native=b1195, ctor=x1196} = PrettyRep.Rec [("ns", 
          cvtEXPR x1194), ("native", PrettyRep.Bool b1195), ("ctor", cvtCTOR x1196)]
   and cvtVAR_DEFN {kind=x1204, ns=x1205, static=b1206, prototype=b1207, bindings=x1208} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1204), ("ns", cvtEXPR x1205), 
          ("static", PrettyRep.Bool b1206), ("prototype", PrettyRep.Bool b1207), 
          ("bindings", cvtBINDINGS x1208)]
   and cvtNAMESPACE_DEFN {ident=x1220, ns=x1221, init=opt1223} = PrettyRep.Rec [("ident", 
          cvtIDENT x1220), ("ns", cvtEXPR x1221), ("init", 
       (case opt1223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1222 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1222))
       ))]
   and cvtCLASS_DEFN {ident=x1234, ns=x1235, nonnullable=b1236, dynamic=b1237, 
          final=b1238, params=ls1240, extends=opt1245, implements=ls1250, classDefns=ls1255, 
          instanceDefns=ls1260, instanceStmts=ls1265, ctorDefn=opt1270} = PrettyRep.Rec [("ident", 
          cvtIDENT x1234), ("ns", cvtEXPR x1235), ("nonnullable", PrettyRep.Bool b1236), 
          ("dynamic", PrettyRep.Bool b1237), ("final", PrettyRep.Bool b1238), 
          ("params", PrettyRep.List (List.map (fn x1239 => cvtIDENT x1239
                                              ) ls1240)), ("extends", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1244))
       )), ("implements", PrettyRep.List (List.map (fn x1249 => cvtIDENT_EXPR x1249
                                                   ) ls1250)), ("classDefns", 
          PrettyRep.List (List.map (fn x1254 => cvtDEFN x1254
                                   ) ls1255)), ("instanceDefns", PrettyRep.List (List.map (fn x1259 => 
                                                                                                 cvtDEFN x1259
                                                                                          ) ls1260)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1264 => cvtSTMT x1264
                                                     ) ls1265)), ("ctorDefn", 
          
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1269))
       ))]
   and cvtINTERFACE_DEFN {ident=x1299, ns=x1300, nonnullable=b1301, params=ls1303, 
          extends=ls1308, block=x1312} = PrettyRep.Rec [("ident", cvtIDENT x1299), 
          ("ns", cvtEXPR x1300), ("nonnullable", PrettyRep.Bool b1301), ("params", 
          PrettyRep.List (List.map (fn x1302 => cvtIDENT x1302
                                   ) ls1303)), ("extends", PrettyRep.List (List.map (fn x1307 => 
                                                                                           cvtIDENT_EXPR x1307
                                                                                    ) ls1308)), 
          ("block", cvtBLOCK x1312)]
   and cvtTYPE_DEFN {ident=x1326, ns=x1327, init=x1328} = PrettyRep.Rec [("ident", 
          cvtIDENT x1326), ("ns", cvtEXPR x1327), ("init", cvtTYPE_EXPR x1328)]
   and cvtFOR_ENUM_STMT {defn=opt1337, obj=x1341, fixtures=opt1343, inits=opt1348, 
          contLabel=opt1353, body=x1357} = PrettyRep.Rec [("defn", 
       (case opt1337 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1336 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1336))
       )), ("obj", cvtEXPR x1341), ("fixtures", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1342))
       )), ("inits", 
       (case opt1348 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1347 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1347))
       )), ("contLabel", 
       (case opt1353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1352))
       )), ("body", cvtSTMT x1357)]
   and cvtFOR_STMT {fixtures=opt1372, defn=opt1377, init=x1381, cond=x1382, 
          update=x1383, contLabel=opt1385, body=x1389} = PrettyRep.Rec [("fixtures", 
          
       (case opt1372 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1371 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1371))
       )), ("defn", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1376))
       )), ("init", cvtSTMT x1381), ("cond", cvtEXPR x1382), ("update", cvtEXPR x1383), 
          ("contLabel", 
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1384))
       )), ("body", cvtSTMT x1389)]
   and cvtWHILE_STMT {cond=x1405, fixtures=opt1407, body=x1411, contLabel=opt1413} = 
          PrettyRep.Rec [("cond", cvtEXPR x1405), ("fixtures", 
       (case opt1407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1406 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1406))
       )), ("body", cvtSTMT x1411), ("contLabel", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1412))
       ))]
   and cvtDIRECTIVES {pragmas=ls1427, defns=ls1432, head=opt1437, body=ls1442} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1426 => 
                                                                     cvtPRAGMA x1426
                                                              ) ls1427)), ("defns", 
          PrettyRep.List (List.map (fn x1431 => cvtDEFN x1431
                                   ) ls1432)), ("head", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1436))
       )), ("body", PrettyRep.List (List.map (fn x1441 => cvtSTMT x1441
                                             ) ls1442))]
   and cvtCASE {label=opt1456, inits=opt1461, body=x1465} = PrettyRep.Rec [("label", 
          
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1455 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1455))
       )), ("inits", 
       (case opt1461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1460 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1460))
       )), ("body", cvtBLOCK x1465)]
   and cvtTYPE_CASE {ty=opt1474, bindings=x1478, inits=opt1480, body=x1484} = 
          PrettyRep.Rec [("ty", 
       (case opt1474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1473 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1473))
       )), ("bindings", cvtBINDINGS x1478), ("inits", 
       (case opt1480 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1479 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1479))
       )), ("body", cvtBLOCK x1484)]
   and cvtFUNC_NAME {kind=x1494, ident=x1495} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1494), 
          ("ident", cvtIDENT x1495)]
   and cvtPACKAGE {name=x1501, block=x1502} = PrettyRep.Rec [("name", cvtUSTRING x1501), 
          ("block", cvtBLOCK x1502)]
   and cvtPROGRAM {packages=ls1509, fixtures=opt1514, block=x1518} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1508 => cvtPACKAGE x1508
                                   ) ls1509)), ("fixtures", 
       (case opt1514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1513 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1513))
       )), ("block", cvtBLOCK x1518)]
end

