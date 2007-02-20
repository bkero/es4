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
   and cvtCLS (Cls{extends=opt276, implements=ls281, classFixtures=x285, instanceFixtures=x286, 
          classType=x287, instanceType=x288}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("extends", 
          
       (case opt276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x275 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x275))
       )), ("implements", PrettyRep.List (List.map (fn x280 => cvtNAME x280
                                                   ) ls281)), ("classFixtures", 
          cvtFIXTURES x285), ("instanceFixtures", cvtFIXTURES x286), ("classType", 
          cvtTYPE_EXPR x287), ("instanceType", cvtTYPE_EXPR x288)]))
   and cvtFUNC (Func{name=x304, fsig=x305, body=x306, fixtures=opt308, inits=ls313}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x304), 
          ("fsig", cvtFUNC_SIG x305), ("body", cvtBLOCK x306), ("fixtures", 
          
       (case opt308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x307 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x307))
       )), ("inits", PrettyRep.List (List.map (fn x312 => cvtSTMT x312
                                              ) ls313))]))
   and cvtDEFN (ClassDefn x330) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x330))
     | cvtDEFN (VariableDefn x333) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x333))
     | cvtDEFN (FunctionDefn x336) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x336))
     | cvtDEFN (InterfaceDefn x339) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x339))
     | cvtDEFN (NamespaceDefn x342) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x342))
     | cvtDEFN (TypeDefn x345) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x345))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls349, params=ls354, inits=ls359, 
          returnType=x363, thisType=opt365, hasRest=b369}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x348 => 
                                                                              cvtIDENT x348
                                                                       ) ls349)), 
          ("params", PrettyRep.List (List.map (fn x353 => cvtVAR_BINDING x353
                                              ) ls354)), ("inits", PrettyRep.List (List.map (fn x358 => 
                                                                                                   cvtSTMT x358
                                                                                            ) ls359)), 
          ("returnType", cvtTYPE_EXPR x363), ("thisType", 
       (case opt365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x364 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x364))
       )), ("hasRest", PrettyRep.Bool b369)]))
   and cvtVAR_BINDING (Binding{pattern=x385, ty=opt387, init=opt392}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x385), ("ty", 
       (case opt387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x386 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x386))
       )), ("init", 
       (case opt392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x391 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x391))
       ))]))
   and cvtTYPE_EXPR (SpecialType x405) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x405))
     | cvtTYPE_EXPR (UnionType ls409) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x408 => 
                                                                                                           cvtTYPE_EXPR x408
                                                                                                    ) ls409)))
     | cvtTYPE_EXPR (ArrayType ls416) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x415 => 
                                                                                                           cvtTYPE_EXPR x415
                                                                                                    ) ls416)))
     | cvtTYPE_EXPR (NominalType{ident=x422}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x422)]))
     | cvtTYPE_EXPR (FunctionType x428) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x428))
     | cvtTYPE_EXPR (ObjectType ls432) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x431 => 
                                                                                                             cvtFIELD_TYPE x431
                                                                                                      ) ls432)))
     | cvtTYPE_EXPR (AppType{base=x438, args=ls440}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x438), ("args", PrettyRep.List (List.map (fn x439 => 
                                                                                                     cvtTYPE_EXPR x439
                                                                                              ) ls440))]))
     | cvtTYPE_EXPR (NullableType{expr=x451, nullable=b452}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x451), ("nullable", PrettyRep.Bool b452)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls462) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x461 => 
                                                                                                    cvtEXPR x461
                                                                                             ) ls462)))
     | cvtSTMT (InitStmt{kind=x468, ns=x469, prototype=b470, static=b471, inits=ls473}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x468), 
          ("ns", cvtEXPR x469), ("prototype", PrettyRep.Bool b470), ("static", 
          PrettyRep.Bool b471), ("inits", PrettyRep.List (List.map (fn x472 => 
                                                                          cvtEXPR x472
                                                                   ) ls473))]))
     | cvtSTMT (ClassBlock{ns=x490, ident=x491, name=opt493, extends=opt498, 
          fixtures=opt503, block=x507}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x490), ("ident", cvtIDENT x491), ("name", 
       (case opt493 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x492 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x492))
       )), ("extends", 
       (case opt498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x497 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x497))
       )), ("fixtures", 
       (case opt503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x502 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x502))
       )), ("block", cvtBLOCK x507)]))
     | cvtSTMT (PackageBlock{name=x523, block=x524}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x523), ("block", cvtBLOCK x524)]))
     | cvtSTMT (ForEachStmt x532) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x532))
     | cvtSTMT (ForInStmt x535) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x535))
     | cvtSTMT (ThrowStmt ls539) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x538 => 
                                                                                                      cvtEXPR x538
                                                                                               ) ls539)))
     | cvtSTMT (ReturnStmt ls546) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x545 => 
                                                                                                        cvtEXPR x545
                                                                                                 ) ls546)))
     | cvtSTMT (BreakStmt opt553) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x552 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x552))
       ))
     | cvtSTMT (ContinueStmt opt560) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x559 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x559))
       ))
     | cvtSTMT (BlockStmt x566) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x566))
     | cvtSTMT (LabeledStmt(x569, x570)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x569, 
          cvtSTMT x570]))
     | cvtSTMT (LetStmt(ls575, x579)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x574 => 
                                                                                                                          cvtVAR_BINDING x574
                                                                                                                   ) ls575), 
          cvtSTMT x579]))
     | cvtSTMT (SuperStmt ls584) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x583 => 
                                                                                                      cvtEXPR x583
                                                                                               ) ls584)))
     | cvtSTMT (WhileStmt x590) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x590))
     | cvtSTMT (DoWhileStmt x593) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x593))
     | cvtSTMT (ForStmt{defns=ls597, fixtures=opt602, init=ls607, cond=ls612, 
          update=ls617, contLabel=opt622, body=x626}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x596 => 
                                                                         cvtVAR_BINDING x596
                                                                  ) ls597)), 
          ("fixtures", 
       (case opt602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x601))
       )), ("init", PrettyRep.List (List.map (fn x606 => cvtEXPR x606
                                             ) ls607)), ("cond", PrettyRep.List (List.map (fn x611 => 
                                                                                                 cvtEXPR x611
                                                                                          ) ls612)), 
          ("update", PrettyRep.List (List.map (fn x616 => cvtEXPR x616
                                              ) ls617)), ("contLabel", 
       (case opt622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x621 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x621))
       )), ("body", cvtSTMT x626)]))
     | cvtSTMT (IfStmt{cnd=x644, thn=x645, els=x646}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x644), ("thn", cvtSTMT x645), 
          ("els", cvtSTMT x646)]))
     | cvtSTMT (WithStmt{obj=ls657, ty=x661, body=x662}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x656 => 
                                                                       cvtEXPR x656
                                                                ) ls657)), 
          ("ty", cvtTYPE_EXPR x661), ("body", cvtSTMT x662)]))
     | cvtSTMT (TryStmt{body=x672, catches=ls687, finally=opt692}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x672), ("catches", PrettyRep.List (List.map (fn {bind=x673, 
                                                                                                    fixtures=opt675, 
                                                                                                    body=x679} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x673), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt675 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x674 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x674))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x679)]
                                                                                             ) ls687)), 
          ("finally", 
       (case opt692 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x691 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x691))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls706, cases=ls711}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x705 => 
                                                                        cvtEXPR x705
                                                                 ) ls706)), 
          ("cases", PrettyRep.List (List.map (fn x710 => cvtCASE x710
                                             ) ls711))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls723, ty=x727, cases=ls729}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x722 => 
                                                                        cvtEXPR x722
                                                                 ) ls723)), 
          ("ty", cvtTYPE_EXPR x727), ("cases", PrettyRep.List (List.map (fn x728 => 
                                                                               cvtTYPE_CASE x728
                                                                        ) ls729))]))
     | cvtSTMT (Dxns{expr=x742}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x742)]))
   and cvtEXPR (TrinaryExpr(x748, x749, x750, x751)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x748, cvtEXPR x749, cvtEXPR x750, 
          cvtEXPR x751]))
     | cvtEXPR (BinaryExpr(x755, x756, x757)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x755, cvtEXPR x756, cvtEXPR x757]))
     | cvtEXPR (BinaryTypeExpr(x761, x762, x763)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x761, cvtEXPR x762, cvtTYPE_EXPR x763]))
     | cvtEXPR (UnaryExpr(x767, x768)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x767, 
          cvtEXPR x768]))
     | cvtEXPR (TypeExpr x772) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x772))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt781) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt781 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls777 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x776 => 
                                                                                     cvtEXPR x776
                                                                              ) ls777)))
       ))
     | cvtEXPR (SuperExpr opt788) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt788 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x787 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x787))
       ))
     | cvtEXPR (LiteralExpr x794) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x794))
     | cvtEXPR (CallExpr{func=x797, actuals=ls799}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x797), ("actuals", PrettyRep.List (List.map (fn x798 => 
                                                                                                   cvtEXPR x798
                                                                                            ) ls799))]))
     | cvtEXPR (ApplyTypeExpr{expr=x810, actuals=ls812}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x810), ("actuals", PrettyRep.List (List.map (fn x811 => 
                                                                                                   cvtTYPE_EXPR x811
                                                                                            ) ls812))]))
     | cvtEXPR (LetExpr{defs=ls824, body=ls829, fixtures=opt834}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x823 => 
                                                                        cvtVAR_BINDING x823
                                                                 ) ls824)), 
          ("body", PrettyRep.List (List.map (fn x828 => cvtEXPR x828
                                            ) ls829)), ("fixtures", 
       (case opt834 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x833 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x833))
       ))]))
     | cvtEXPR (NewExpr{obj=x847, actuals=ls849}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x847), ("actuals", PrettyRep.List (List.map (fn x848 => 
                                                                                                  cvtEXPR x848
                                                                                           ) ls849))]))
     | cvtEXPR (FunExpr x860) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x860))
     | cvtEXPR (ObjectRef{base=x863, ident=x864}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x863), ("ident", cvtIDENT_EXPR x864)]))
     | cvtEXPR (LexicalRef{ident=x872}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x872)]))
     | cvtEXPR (SetExpr(x878, x879, x880)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x878, 
          cvtPATTERN x879, cvtEXPR x880]))
     | cvtEXPR (AllocTemp(n884, x885)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n884, 
          cvtEXPR x885]))
     | cvtEXPR (KillTemp n889) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n889))
     | cvtEXPR (GetTemp n892) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n892))
     | cvtEXPR (ListExpr ls896) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x895 => 
                                                                                                    cvtEXPR x895
                                                                                             ) ls896)))
     | cvtEXPR (SliceExpr(ls903, ls908, ls913)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x902 => cvtEXPR x902
                                                          ) ls903), PrettyRep.List (List.map (fn x907 => 
                                                                                                    cvtEXPR x907
                                                                                             ) ls908), 
          PrettyRep.List (List.map (fn x912 => cvtEXPR x912
                                   ) ls913)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x920, ident=x921}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x920), ("ident", cvtUSTRING x921)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x929, expr=x930}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x929), ("expr", cvtEXPR x930)]))
     | cvtIDENT_EXPR (AttributeIdentifier x938) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x938))
     | cvtIDENT_EXPR (Identifier{ident=x941, openNamespaces=ls947}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x941), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls943 => PrettyRep.List (List.map (fn x942 => 
                                                                                cvtNAMESPACE x942
                                                                         ) ls943)
                                   ) ls947))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x958) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x958))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x961, typeParams=ls963}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x961), ("typeParams", 
          PrettyRep.List (List.map (fn x962 => cvtTYPE_EXPR x962
                                   ) ls963))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r976) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r976))
     | cvtLITERAL (LiteralBoolean b979) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b979))
     | cvtLITERAL (LiteralString x982) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x982))
     | cvtLITERAL (LiteralArray{exprs=ls986, ty=opt991}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x985 => 
                                                                         cvtEXPR x985
                                                                  ) ls986)), 
          ("ty", 
       (case opt991 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x990 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x990))
       ))]))
     | cvtLITERAL (LiteralXML ls1003) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1002 => 
                                                                                                            cvtEXPR x1002
                                                                                                     ) ls1003)))
     | cvtLITERAL (LiteralNamespace x1009) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1009))
     | cvtLITERAL (LiteralObject{expr=ls1013, ty=opt1018}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1012 => 
                                                                        cvtFIELD x1012
                                                                 ) ls1013)), 
          ("ty", 
       (case opt1018 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1017 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1017))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x1029}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1029)]))
   and cvtBLOCK (Block x1035) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1035))
   and cvtPATTERN (ObjectPattern ls1039) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1038 => cvtFIELD_PATTERN x1038
                                         ) ls1039)))
     | cvtPATTERN (ArrayPattern ls1046) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1045 => 
                                                                                                                cvtPATTERN x1045
                                                                                                         ) ls1046)))
     | cvtPATTERN (SimplePattern x1052) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1052))
     | cvtPATTERN (IdentifierPattern x1055) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1055))
   and cvtFIXTURE (NamespaceFixture x1058) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1058))
     | cvtFIXTURE (ClassFixture x1061) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1061))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1065) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1065))
     | cvtFIXTURE (ValFixture{ty=x1068, readOnly=b1069, isOverride=b1070, isFinal=b1071, 
          init=opt1073}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1068), ("readOnly", PrettyRep.Bool b1069), ("isOverride", 
          PrettyRep.Bool b1070), ("isFinal", PrettyRep.Bool b1071), ("init", 
          
       (case opt1073 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1072 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1072))
       ))]))
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
   and cvtFIELD {kind=x1110, name=x1111, init=x1112} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1110), ("name", cvtIDENT_EXPR x1111), ("init", cvtEXPR x1112)]
   and cvtFIELD_PATTERN {name=x1120, ptrn=x1121} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1120), ("ptrn", cvtPATTERN x1121)]
   and cvtFIELD_TYPE {name=x1127, ty=x1128} = PrettyRep.Rec [("name", cvtIDENT x1127), 
          ("ty", cvtTYPE_EXPR x1128)]
   and cvtTYPED_IDENT {name=x1134, ty=opt1136} = PrettyRep.Rec [("name", cvtIDENT x1134), 
          ("ty", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1135))
       ))]
   and cvtATTRIBUTES {ns=x1145, override=b1146, static=b1147, final=b1148, 
          dynamic=b1149, prototype=b1150, native=b1151, rest=b1152} = PrettyRep.Rec [("ns", 
          cvtEXPR x1145), ("override", PrettyRep.Bool b1146), ("static", PrettyRep.Bool b1147), 
          ("final", PrettyRep.Bool b1148), ("dynamic", PrettyRep.Bool b1149), 
          ("prototype", PrettyRep.Bool b1150), ("native", PrettyRep.Bool b1151), 
          ("rest", PrettyRep.Bool b1152)]
   and cvtFUNC_DEFN {kind=x1170, ns=x1171, final=b1172, native=b1173, override=b1174, 
          prototype=b1175, static=b1176, func=x1177} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1170), ("ns", cvtEXPR x1171), ("final", PrettyRep.Bool b1172), 
          ("native", PrettyRep.Bool b1173), ("override", PrettyRep.Bool b1174), 
          ("prototype", PrettyRep.Bool b1175), ("static", PrettyRep.Bool b1176), 
          ("func", cvtFUNC x1177)]
   and cvtVAR_DEFN {kind=x1195, ns=x1196, static=b1197, prototype=b1198, bindings=ls1200} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1195), ("ns", cvtEXPR x1196), 
          ("static", PrettyRep.Bool b1197), ("prototype", PrettyRep.Bool b1198), 
          ("bindings", PrettyRep.List (List.map (fn x1199 => cvtVAR_BINDING x1199
                                                ) ls1200))]
   and cvtFIXTURES ls1218 = PrettyRep.List (List.map (fn (x1215, x1216) => 
                                                            PrettyRep.Tuple [cvtNAME x1215, 
                                                            cvtFIXTURE x1216]
                                                     ) ls1218)
   and cvtNAMESPACE_DEFN {ident=x1222, ns=x1223, init=opt1225} = PrettyRep.Rec [("ident", 
          cvtIDENT x1222), ("ns", cvtEXPR x1223), ("init", 
       (case opt1225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1224 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1224))
       ))]
   and cvtCLASS_DEFN {ident=x1236, ns=x1237, nonnullable=b1238, dynamic=b1239, 
          final=b1240, params=ls1242, extends=opt1247, implements=ls1252, body=x1256} = 
          PrettyRep.Rec [("ident", cvtIDENT x1236), ("ns", cvtEXPR x1237), 
          ("nonnullable", PrettyRep.Bool b1238), ("dynamic", PrettyRep.Bool b1239), 
          ("final", PrettyRep.Bool b1240), ("params", PrettyRep.List (List.map (fn x1241 => 
                                                                                      cvtIDENT x1241
                                                                               ) ls1242)), 
          ("extends", 
       (case opt1247 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1246 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1246))
       )), ("implements", PrettyRep.List (List.map (fn x1251 => cvtIDENT_EXPR x1251
                                                   ) ls1252)), ("body", cvtBLOCK x1256)]
   and cvtINTERFACE_DEFN {ident=x1276, ns=x1277, nonnullable=b1278, params=ls1280, 
          extends=ls1285, body=x1289} = PrettyRep.Rec [("ident", cvtIDENT x1276), 
          ("ns", cvtEXPR x1277), ("nonnullable", PrettyRep.Bool b1278), ("params", 
          PrettyRep.List (List.map (fn x1279 => cvtIDENT x1279
                                   ) ls1280)), ("extends", PrettyRep.List (List.map (fn x1284 => 
                                                                                           cvtIDENT_EXPR x1284
                                                                                    ) ls1285)), 
          ("body", cvtBLOCK x1289)]
   and cvtTYPE_DEFN {ident=x1303, ns=x1304, init=x1305} = PrettyRep.Rec [("ident", 
          cvtIDENT x1303), ("ns", cvtEXPR x1304), ("init", cvtTYPE_EXPR x1305)]
   and cvtFOR_ENUM_STMT {ptrn=opt1314, obj=ls1319, defns=ls1324, fixtures=opt1329, 
          contLabel=opt1334, body=x1338} = PrettyRep.Rec [("ptrn", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1313))
       )), ("obj", PrettyRep.List (List.map (fn x1318 => cvtEXPR x1318
                                            ) ls1319)), ("defns", PrettyRep.List (List.map (fn x1323 => 
                                                                                                  cvtVAR_BINDING x1323
                                                                                           ) ls1324)), 
          ("fixtures", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1328))
       )), ("contLabel", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1333))
       )), ("body", cvtSTMT x1338)]
   and cvtWHILE_STMT {cond=x1352, body=x1353, contLabel=opt1355} = PrettyRep.Rec [("cond", 
          cvtEXPR x1352), ("body", cvtSTMT x1353), ("contLabel", 
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1354))
       ))]
   and cvtDIRECTIVES {pragmas=ls1367, defns=ls1372, stmts=ls1377, fixtures=opt1382, 
          inits=opt1391} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1366 => 
                                                                                      cvtPRAGMA x1366
                                                                               ) ls1367)), 
          ("defns", PrettyRep.List (List.map (fn x1371 => cvtDEFN x1371
                                             ) ls1372)), ("stmts", PrettyRep.List (List.map (fn x1376 => 
                                                                                                   cvtSTMT x1376
                                                                                            ) ls1377)), 
          ("fixtures", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1381))
       )), ("inits", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1387 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1386 => 
                                                                                      cvtSTMT x1386
                                                                               ) ls1387)))
       ))]
   and cvtBINDINGS {b=ls1407, i=ls1412} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1406 => 
                                                                                               cvtVAR_BINDING x1406
                                                                                        ) ls1407)), 
          ("i", PrettyRep.List (List.map (fn x1411 => cvtEXPR x1411
                                         ) ls1412))]
   and cvtCASE {label=opt1426, body=x1430} = PrettyRep.Rec [("label", 
       (case opt1426 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1422 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1421 => 
                                                                                      cvtEXPR x1421
                                                                               ) ls1422)))
       )), ("body", cvtBLOCK x1430)]
   and cvtTYPE_CASE {ptrn=opt1437, body=x1441} = PrettyRep.Rec [("ptrn", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1436))
       )), ("body", cvtBLOCK x1441)]
   and cvtFUNC_NAME {kind=x1447, ident=x1448} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1447), 
          ("ident", cvtIDENT x1448)]
   and cvtPACKAGE {name=x1454, body=x1455} = PrettyRep.Rec [("name", cvtUSTRING x1454), 
          ("body", cvtBLOCK x1455)]
   and cvtPROGRAM {packages=ls1462, fixtures=opt1467, body=x1471} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1461 => cvtPACKAGE x1461
                                   ) ls1462)), ("fixtures", 
       (case opt1467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1466 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1466))
       )), ("body", cvtBLOCK x1471)]
end

