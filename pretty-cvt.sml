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
     | cvtSTMT (ForStmt{fixtures=opt645, defn=opt650, init=ls655, cond=x659, 
          update=x660, contLabel=opt662, body=x666}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x644 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x644))
       )), ("defn", 
       (case opt650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x649 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x649))
       )), ("init", PrettyRep.List (List.map (fn x654 => cvtSTMT x654
                                             ) ls655)), ("cond", cvtEXPR x659), 
          ("update", cvtEXPR x660), ("contLabel", 
       (case opt662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x661))
       )), ("body", cvtSTMT x666)]))
     | cvtSTMT (IfStmt{cnd=x684, thn=x685, els=x686}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x684), ("thn", cvtSTMT x685), 
          ("els", cvtSTMT x686)]))
     | cvtSTMT (WithStmt{obj=x696, ty=x697, body=x698}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x696), ("ty", cvtTYPE_EXPR x697), 
          ("body", cvtSTMT x698)]))
     | cvtSTMT (TryStmt{block=x708, catches=ls730, finally=opt735}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x708), ("catches", PrettyRep.List (List.map (fn {bindings=x709, 
                                                                                                     ty=opt711, 
                                                                                                     fixtures=opt716, 
                                                                                                     block=x720} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x709), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt711 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x710 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x710))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt716 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x715 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x715))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x720)]
                                                                                              ) ls730)), 
          ("finally", 
       (case opt735 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x734 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x734))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x748, cases=ls750}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x748), ("cases", PrettyRep.List (List.map (fn x749 => 
                                                                                                 cvtCASE x749
                                                                                          ) ls750))]))
     | cvtSTMT (SwitchTypeStmt{cond=x761, ty=x762, cases=ls764}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x761), ("ty", cvtTYPE_EXPR x762), 
          ("cases", PrettyRep.List (List.map (fn x763 => cvtTYPE_CASE x763
                                             ) ls764))]))
     | cvtSTMT (Dxns{expr=x777}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x777)]))
   and cvtEXPR (TrinaryExpr(x783, x784, x785, x786)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x783, cvtEXPR x784, cvtEXPR x785, 
          cvtEXPR x786]))
     | cvtEXPR (BinaryExpr(x790, x791, x792)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x790, cvtEXPR x791, cvtEXPR x792]))
     | cvtEXPR (BinaryTypeExpr(x796, x797, x798)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x796, cvtEXPR x797, cvtTYPE_EXPR x798]))
     | cvtEXPR (UnaryExpr(x802, x803)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x802, 
          cvtEXPR x803]))
     | cvtEXPR (TypeExpr x807) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x807))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt812) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt812 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x811 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x811))
       ))
     | cvtEXPR (SuperExpr opt819) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt819 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x818 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x818))
       ))
     | cvtEXPR (LiteralExpr x825) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x825))
     | cvtEXPR (CallExpr{func=x828, actuals=ls830}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x828), ("actuals", PrettyRep.List (List.map (fn x829 => 
                                                                                                   cvtEXPR x829
                                                                                            ) ls830))]))
     | cvtEXPR (ApplyTypeExpr{expr=x841, actuals=ls843}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x841), ("actuals", PrettyRep.List (List.map (fn x842 => 
                                                                                                   cvtTYPE_EXPR x842
                                                                                            ) ls843))]))
     | cvtEXPR (LetExpr{defs=x854, body=x855, head=opt857}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x854), ("body", cvtEXPR x855), 
          ("head", 
       (case opt857 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x856 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x856))
       ))]))
     | cvtEXPR (NewExpr{obj=x870, actuals=ls872}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x870), ("actuals", PrettyRep.List (List.map (fn x871 => 
                                                                                                  cvtEXPR x871
                                                                                           ) ls872))]))
     | cvtEXPR (ObjectRef{base=x883, ident=x884}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x883), ("ident", cvtIDENT_EXPR x884)]))
     | cvtEXPR (LexicalRef{ident=x892}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x892)]))
     | cvtEXPR (SetExpr(x898, x899, x900)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x898, 
          cvtEXPR x899, cvtEXPR x900]))
     | cvtEXPR (ListExpr ls905) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x904 => 
                                                                                                    cvtEXPR x904
                                                                                             ) ls905)))
     | cvtEXPR (InitExpr(x911, x912, x913)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x911, 
          cvtHEAD x912, cvtINITS x913]))
     | cvtEXPR (SliceExpr(x917, x918, x919)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x917, cvtEXPR x918, cvtEXPR x919]))
     | cvtEXPR (DefTemp(n923, x924)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n923, 
          cvtEXPR x924]))
     | cvtEXPR (GetTemp n928) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n928))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
     | cvtINIT_TARGET (Static) = PrettyRep.Ctor ("Static", NONE)
   and cvtFIXTURE_NAME (TempName n935) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n935))
     | cvtFIXTURE_NAME (PropName x938) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x938))
   and cvtIDENT_EXPR (Identifier{ident=x941, openNamespaces=ls947}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x941), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls943 => PrettyRep.List (List.map (fn x942 => 
                                                                                cvtNAMESPACE x942
                                                                         ) ls943)
                                   ) ls947))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x958, expr=x959}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x958), ("expr", cvtEXPR x959)]))
     | cvtIDENT_EXPR (AttributeIdentifier x967) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x967))
     | cvtIDENT_EXPR (ExpressionIdentifier x970) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x970))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x973, ident=x974}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x973), ("ident", cvtUSTRING x974)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x982, typeParams=ls984}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x982), ("typeParams", 
          PrettyRep.List (List.map (fn x983 => cvtTYPE_EXPR x983
                                   ) ls984))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r997) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r997))
     | cvtLITERAL (LiteralBoolean b1000) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1000))
     | cvtLITERAL (LiteralString x1003) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1003))
     | cvtLITERAL (LiteralArray{exprs=ls1007, ty=opt1012}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1006 => 
                                                                         cvtEXPR x1006
                                                                  ) ls1007)), 
          ("ty", 
       (case opt1012 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1011 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1011))
       ))]))
     | cvtLITERAL (LiteralXML ls1024) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1023 => 
                                                                                                            cvtEXPR x1023
                                                                                                     ) ls1024)))
     | cvtLITERAL (LiteralNamespace x1030) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1030))
     | cvtLITERAL (LiteralObject{expr=ls1034, ty=opt1039}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1033 => 
                                                                        cvtFIELD x1033
                                                                 ) ls1034)), 
          ("ty", 
       (case opt1039 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1038 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1038))
       ))]))
     | cvtLITERAL (LiteralFunction x1050) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1050))
     | cvtLITERAL (LiteralRegExp{str=x1053}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1053)]))
   and cvtBLOCK (Block x1059) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1059))
   and cvtFIXTURE (NamespaceFixture x1062) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1062))
     | cvtFIXTURE (ClassFixture x1065) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1065))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1069) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1069))
     | cvtFIXTURE (MethodFixture{ty=x1072, isOverride=b1073, isFinal=b1074}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1072), 
          ("isOverride", PrettyRep.Bool b1073), ("isFinal", PrettyRep.Bool b1074)]))
     | cvtFIXTURE (ValFixture{ty=x1084, readOnly=b1085}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1084), ("readOnly", PrettyRep.Bool b1085)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1093, getter=opt1095, setter=opt1100}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1093), ("getter", 
       (case opt1095 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1094 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1094))
       )), ("setter", 
       (case opt1100 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1099 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1099))
       ))]))
   and cvtBINDINGS (ls1114, ls1119) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1113 => 
                                                                                       cvtBINDING x1113
                                                                                ) ls1114), 
          PrettyRep.List (List.map (fn x1118 => cvtINIT_STEP x1118
                                   ) ls1119)]
   and cvtFIXTURES ls1127 = PrettyRep.List (List.map (fn (x1124, x1125) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1124, 
                                                            cvtFIXTURE x1125]
                                                     ) ls1127)
   and cvtINITS ls1134 = PrettyRep.List (List.map (fn (x1131, x1132) => PrettyRep.Tuple [cvtFIXTURE_NAME x1131, 
                                                         cvtEXPR x1132]
                                                  ) ls1134)
   and cvtHEAD (x1138, x1139) = PrettyRep.Tuple [cvtFIXTURES x1138, cvtINITS x1139]
   and cvtFIELD {kind=x1141, name=x1142, init=x1143} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1141), ("name", cvtIDENT_EXPR x1142), ("init", cvtEXPR x1143)]
   and cvtFIELD_TYPE {name=x1151, ty=x1152} = PrettyRep.Rec [("name", cvtIDENT x1151), 
          ("ty", cvtTYPE_EXPR x1152)]
   and cvtTYPED_IDENT {name=x1158, ty=opt1160} = PrettyRep.Rec [("name", cvtIDENT x1158), 
          ("ty", 
       (case opt1160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1159 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1159))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1170, params=ls1175, result=x1179, thisType=opt1181, 
          hasRest=b1185, minArgs=n1186} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1169 => 
                                                                                                        cvtIDENT x1169
                                                                                                 ) ls1170)), 
          ("params", PrettyRep.List (List.map (fn x1174 => cvtTYPE_EXPR x1174
                                              ) ls1175)), ("result", cvtTYPE_EXPR x1179), 
          ("thisType", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1180))
       )), ("hasRest", PrettyRep.Bool b1185), ("minArgs", PrettyRep.Int n1186)]
   and cvtFUNC_DEFN {kind=x1200, ns=x1201, final=b1202, native=b1203, override=b1204, 
          prototype=b1205, static=b1206, func=x1207} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1200), ("ns", cvtEXPR x1201), ("final", PrettyRep.Bool b1202), 
          ("native", PrettyRep.Bool b1203), ("override", PrettyRep.Bool b1204), 
          ("prototype", PrettyRep.Bool b1205), ("static", PrettyRep.Bool b1206), 
          ("func", cvtFUNC x1207)]
   and cvtCTOR_DEFN {ns=x1225, native=b1226, ctor=x1227} = PrettyRep.Rec [("ns", 
          cvtEXPR x1225), ("native", PrettyRep.Bool b1226), ("ctor", cvtCTOR x1227)]
   and cvtVAR_DEFN {kind=x1235, ns=x1236, static=b1237, prototype=b1238, bindings=x1239} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1235), ("ns", cvtEXPR x1236), 
          ("static", PrettyRep.Bool b1237), ("prototype", PrettyRep.Bool b1238), 
          ("bindings", cvtBINDINGS x1239)]
   and cvtNAMESPACE_DEFN {ident=x1251, ns=x1252, init=opt1254} = PrettyRep.Rec [("ident", 
          cvtIDENT x1251), ("ns", cvtEXPR x1252), ("init", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       ))]
   and cvtCLASS_DEFN {ident=x1265, ns=x1266, nonnullable=b1267, dynamic=b1268, 
          final=b1269, params=ls1271, extends=opt1276, implements=ls1281, classDefns=ls1286, 
          instanceDefns=ls1291, instanceStmts=ls1296, ctorDefn=opt1301} = PrettyRep.Rec [("ident", 
          cvtIDENT x1265), ("ns", cvtEXPR x1266), ("nonnullable", PrettyRep.Bool b1267), 
          ("dynamic", PrettyRep.Bool b1268), ("final", PrettyRep.Bool b1269), 
          ("params", PrettyRep.List (List.map (fn x1270 => cvtIDENT x1270
                                              ) ls1271)), ("extends", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1275))
       )), ("implements", PrettyRep.List (List.map (fn x1280 => cvtIDENT_EXPR x1280
                                                   ) ls1281)), ("classDefns", 
          PrettyRep.List (List.map (fn x1285 => cvtDEFN x1285
                                   ) ls1286)), ("instanceDefns", PrettyRep.List (List.map (fn x1290 => 
                                                                                                 cvtDEFN x1290
                                                                                          ) ls1291)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1295 => cvtSTMT x1295
                                                     ) ls1296)), ("ctorDefn", 
          
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1300))
       ))]
   and cvtINTERFACE_DEFN {ident=x1330, ns=x1331, nonnullable=b1332, params=ls1334, 
          extends=ls1339, block=x1343} = PrettyRep.Rec [("ident", cvtIDENT x1330), 
          ("ns", cvtEXPR x1331), ("nonnullable", PrettyRep.Bool b1332), ("params", 
          PrettyRep.List (List.map (fn x1333 => cvtIDENT x1333
                                   ) ls1334)), ("extends", PrettyRep.List (List.map (fn x1338 => 
                                                                                           cvtIDENT_EXPR x1338
                                                                                    ) ls1339)), 
          ("block", cvtBLOCK x1343)]
   and cvtTYPE_DEFN {ident=x1357, ns=x1358, init=x1359} = PrettyRep.Rec [("ident", 
          cvtIDENT x1357), ("ns", cvtEXPR x1358), ("init", cvtTYPE_EXPR x1359)]
   and cvtFOR_ENUM_STMT {defn=opt1368, obj=x1372, fixtures=opt1374, inits=opt1379, 
          contLabel=opt1384, body=x1388} = PrettyRep.Rec [("defn", 
       (case opt1368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1367 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1367))
       )), ("obj", cvtEXPR x1372), ("fixtures", 
       (case opt1374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1373 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1373))
       )), ("inits", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1378 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1378))
       )), ("contLabel", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1383 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1383))
       )), ("body", cvtSTMT x1388)]
   and cvtWHILE_STMT {cond=x1402, fixtures=opt1404, body=x1408, contLabel=opt1410} = 
          PrettyRep.Rec [("cond", cvtEXPR x1402), ("fixtures", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1403 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1403))
       )), ("body", cvtSTMT x1408), ("contLabel", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1409 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1409))
       ))]
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

