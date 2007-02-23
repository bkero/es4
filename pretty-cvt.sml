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
   and cvtFUNC (Func{name=x326, fsig=x327, fixtures=opt329, inits=ls334, body=x338}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x326), 
          ("fsig", cvtFUNC_SIG x327), ("fixtures", 
       (case opt329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x328 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x328))
       )), ("inits", PrettyRep.List (List.map (fn x333 => cvtSTMT x333
                                              ) ls334)), ("body", cvtBLOCK x338)]))
   and cvtDEFN (ClassDefn x352) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x352))
     | cvtDEFN (VariableDefn x355) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x355))
     | cvtDEFN (FunctionDefn x358) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x358))
     | cvtDEFN (ConstructorDefn x361) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x361))
     | cvtDEFN (InterfaceDefn x364) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x364))
     | cvtDEFN (NamespaceDefn x367) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x367))
     | cvtDEFN (TypeDefn x370) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x370))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls374, params=ls379, inits=ls384, 
          returnType=x388, thisType=opt390, hasRest=b394}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x373 => 
                                                                              cvtIDENT x373
                                                                       ) ls374)), 
          ("params", PrettyRep.List (List.map (fn x378 => cvtVAR_BINDING x378
                                              ) ls379)), ("inits", PrettyRep.List (List.map (fn x383 => 
                                                                                                   cvtSTMT x383
                                                                                            ) ls384)), 
          ("returnType", cvtTYPE_EXPR x388), ("thisType", 
       (case opt390 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x389 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x389))
       )), ("hasRest", PrettyRep.Bool b394)]))
   and cvtVAR_BINDING (Binding{pattern=x410, ty=opt412, init=opt417}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x410), ("ty", 
       (case opt412 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x411 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x411))
       )), ("init", 
       (case opt417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x416 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x416))
       ))]))
   and cvtTYPE_EXPR (SpecialType x430) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x430))
     | cvtTYPE_EXPR (UnionType ls434) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x433 => 
                                                                                                           cvtTYPE_EXPR x433
                                                                                                    ) ls434)))
     | cvtTYPE_EXPR (ArrayType ls441) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x440 => 
                                                                                                           cvtTYPE_EXPR x440
                                                                                                    ) ls441)))
     | cvtTYPE_EXPR (TypeName x447) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x447))
     | cvtTYPE_EXPR (FunctionType{typeParams=ls451, params=ls456, result=x460, 
          thisType=opt462, hasRest=b466}) = PrettyRep.Ctor ("FunctionType", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x450 => 
                                                                              cvtIDENT x450
                                                                       ) ls451)), 
          ("params", PrettyRep.List (List.map (fn x455 => cvtTYPE_EXPR x455
                                              ) ls456)), ("result", cvtTYPE_EXPR x460), 
          ("thisType", 
       (case opt462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x461 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x461))
       )), ("hasRest", PrettyRep.Bool b466)]))
     | cvtTYPE_EXPR (ObjectType ls481) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x480 => 
                                                                                                             cvtFIELD_TYPE x480
                                                                                                      ) ls481)))
     | cvtTYPE_EXPR (AppType{base=x487, args=ls489}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x487), ("args", PrettyRep.List (List.map (fn x488 => 
                                                                                                     cvtTYPE_EXPR x488
                                                                                              ) ls489))]))
     | cvtTYPE_EXPR (NullableType{expr=x500, nullable=b501}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x500), ("nullable", PrettyRep.Bool b501)]))
     | cvtTYPE_EXPR (InstanceType{name=x509, typeParams=ls511, ty=x515}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x509), 
          ("typeParams", PrettyRep.List (List.map (fn x510 => cvtIDENT x510
                                                  ) ls511)), ("ty", cvtTYPE_EXPR x515)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x526) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x526))
     | cvtSTMT (InitStmt{kind=x529, ns=x530, prototype=b531, static=b532, inits=ls534}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x529), 
          ("ns", cvtEXPR x530), ("prototype", PrettyRep.Bool b531), ("static", 
          PrettyRep.Bool b532), ("inits", PrettyRep.List (List.map (fn x533 => 
                                                                          cvtEXPR x533
                                                                   ) ls534))]))
     | cvtSTMT (ClassBlock{ns=x551, ident=x552, name=opt554, extends=opt559, 
          fixtures=opt564, block=x568}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x551), ("ident", cvtIDENT x552), ("name", 
       (case opt554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x553 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x553))
       )), ("extends", 
       (case opt559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x558 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x558))
       )), ("fixtures", 
       (case opt564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x563 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x563))
       )), ("block", cvtBLOCK x568)]))
     | cvtSTMT (PackageBlock{name=x584, block=x585}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x584), ("block", cvtBLOCK x585)]))
     | cvtSTMT (ForEachStmt x593) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x593))
     | cvtSTMT (ForInStmt x596) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x596))
     | cvtSTMT (ThrowStmt x599) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x599))
     | cvtSTMT (ReturnStmt x602) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x602))
     | cvtSTMT (BreakStmt opt606) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x605 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x605))
       ))
     | cvtSTMT (ContinueStmt opt613) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt613 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x612 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x612))
       ))
     | cvtSTMT (BlockStmt x619) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x619))
     | cvtSTMT (LabeledStmt(x622, x623)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x622, 
          cvtSTMT x623]))
     | cvtSTMT (LetStmt(ls628, x632)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x627 => 
                                                                                                                          cvtVAR_BINDING x627
                                                                                                                   ) ls628), 
          cvtSTMT x632]))
     | cvtSTMT (SuperStmt x636) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x636))
     | cvtSTMT (WhileStmt x639) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x639))
     | cvtSTMT (DoWhileStmt x642) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x642))
     | cvtSTMT (ForStmt{defns=ls646, fixtures=opt651, init=x655, cond=x656, 
          update=x657, contLabel=opt659, body=x663}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x645 => 
                                                                         cvtVAR_BINDING x645
                                                                  ) ls646)), 
          ("fixtures", 
       (case opt651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x650 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x650))
       )), ("init", cvtEXPR x655), ("cond", cvtEXPR x656), ("update", cvtEXPR x657), 
          ("contLabel", 
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
     | cvtSTMT (TryStmt{body=x705, catches=ls720, finally=opt725}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x705), ("catches", PrettyRep.List (List.map (fn {bind=x706, 
                                                                                                    fixtures=opt708, 
                                                                                                    body=x712} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x706), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt708 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x707 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x707))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x712)]
                                                                                             ) ls720)), 
          ("finally", 
       (case opt725 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x724 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x724))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x738, cases=ls740}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x738), ("cases", PrettyRep.List (List.map (fn x739 => 
                                                                                                 cvtCASE x739
                                                                                          ) ls740))]))
     | cvtSTMT (SwitchTypeStmt{cond=x751, ty=x752, cases=ls754}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x751), ("ty", cvtTYPE_EXPR x752), 
          ("cases", PrettyRep.List (List.map (fn x753 => cvtTYPE_CASE x753
                                             ) ls754))]))
     | cvtSTMT (Dxns{expr=x767}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x767)]))
   and cvtEXPR (TrinaryExpr(x773, x774, x775, x776)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x773, cvtEXPR x774, cvtEXPR x775, 
          cvtEXPR x776]))
     | cvtEXPR (BinaryExpr(x780, x781, x782)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x780, cvtEXPR x781, cvtEXPR x782]))
     | cvtEXPR (BinaryTypeExpr(x786, x787, x788)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x786, cvtEXPR x787, cvtTYPE_EXPR x788]))
     | cvtEXPR (UnaryExpr(x792, x793)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x792, 
          cvtEXPR x793]))
     | cvtEXPR (TypeExpr x797) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x797))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt802) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt802 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x801 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x801))
       ))
     | cvtEXPR (SuperExpr opt809) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt809 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x808 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x808))
       ))
     | cvtEXPR (LiteralExpr x815) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x815))
     | cvtEXPR (CallExpr{func=x818, actuals=ls820}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x818), ("actuals", PrettyRep.List (List.map (fn x819 => 
                                                                                                   cvtEXPR x819
                                                                                            ) ls820))]))
     | cvtEXPR (ApplyTypeExpr{expr=x831, actuals=ls833}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x831), ("actuals", PrettyRep.List (List.map (fn x832 => 
                                                                                                   cvtTYPE_EXPR x832
                                                                                            ) ls833))]))
     | cvtEXPR (LetExpr{defs=ls845, body=x849, fixtures=opt851}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x844 => 
                                                                        cvtVAR_BINDING x844
                                                                 ) ls845)), 
          ("body", cvtEXPR x849), ("fixtures", 
       (case opt851 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x850 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x850))
       ))]))
     | cvtEXPR (NewExpr{obj=x864, actuals=ls866}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x864), ("actuals", PrettyRep.List (List.map (fn x865 => 
                                                                                                  cvtEXPR x865
                                                                                           ) ls866))]))
     | cvtEXPR (ObjectRef{base=x877, ident=x878}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x877), ("ident", cvtIDENT_EXPR x878)]))
     | cvtEXPR (LexicalRef{ident=x886}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x886)]))
     | cvtEXPR (SetExpr(x892, x893, x894)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x892, 
          cvtPATTERN x893, cvtEXPR x894]))
     | cvtEXPR (ListExpr ls899) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x898 => 
                                                                                                    cvtEXPR x898
                                                                                             ) ls899)))
     | cvtEXPR (SliceExpr(x905, x906, x907)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x905, cvtEXPR x906, cvtEXPR x907]))
     | cvtEXPR (DefTemp(n911, x912)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n911, 
          cvtEXPR x912]))
     | cvtEXPR (GetTemp n916) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n916))
   and cvtFIXTURE_NAME (TempName n919) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n919))
     | cvtFIXTURE_NAME (PropName x922) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x922))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x925, ident=x926}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x925), ("ident", cvtUSTRING x926)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x934, expr=x935}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x934), ("expr", cvtEXPR x935)]))
     | cvtIDENT_EXPR (AttributeIdentifier x943) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x943))
     | cvtIDENT_EXPR (Identifier{ident=x946, openNamespaces=ls952}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x946), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls948 => PrettyRep.List (List.map (fn x947 => 
                                                                                cvtNAMESPACE x947
                                                                         ) ls948)
                                   ) ls952))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x963) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x963))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x966, typeParams=ls968}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x966), ("typeParams", 
          PrettyRep.List (List.map (fn x967 => cvtTYPE_EXPR x967
                                   ) ls968))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r981) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r981))
     | cvtLITERAL (LiteralBoolean b984) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b984))
     | cvtLITERAL (LiteralString x987) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x987))
     | cvtLITERAL (LiteralArray{exprs=ls991, ty=opt996}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x990 => 
                                                                         cvtEXPR x990
                                                                  ) ls991)), 
          ("ty", 
       (case opt996 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x995 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x995))
       ))]))
     | cvtLITERAL (LiteralXML ls1008) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1007 => 
                                                                                                            cvtEXPR x1007
                                                                                                     ) ls1008)))
     | cvtLITERAL (LiteralNamespace x1014) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1014))
     | cvtLITERAL (LiteralObject{expr=ls1018, ty=opt1023}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1017 => 
                                                                        cvtFIELD x1017
                                                                 ) ls1018)), 
          ("ty", 
       (case opt1023 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1022 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1022))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1034, ty=x1035}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1034), ("ty", cvtTYPE_EXPR x1035)]))
     | cvtLITERAL (LiteralRegExp{str=x1043}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1043)]))
   and cvtBLOCK (Block x1049) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1049))
   and cvtPATTERN (ObjectPattern ls1053) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1052 => cvtFIELD_PATTERN x1052
                                         ) ls1053)))
     | cvtPATTERN (ArrayPattern ls1060) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1059 => 
                                                                                                                cvtPATTERN x1059
                                                                                                         ) ls1060)))
     | cvtPATTERN (SimplePattern x1066) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1066))
     | cvtPATTERN (IdentifierPattern x1069) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1069))
   and cvtFIXTURE (NamespaceFixture x1072) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1072))
     | cvtFIXTURE (ClassFixture x1075) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1075))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1079) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1079))
     | cvtFIXTURE (ValFixture{ty=x1082, readOnly=b1083, isOverride=b1084, isFinal=b1085}) = 
          PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1082), 
          ("readOnly", PrettyRep.Bool b1083), ("isOverride", PrettyRep.Bool b1084), 
          ("isFinal", PrettyRep.Bool b1085)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1097, getter=opt1099, setter=opt1104}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1097), ("getter", 
       (case opt1099 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1098 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1098))
       )), ("setter", 
       (case opt1104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1103 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1103))
       ))]))
   and cvtFIELD {kind=x1117, name=x1118, init=x1119} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1117), ("name", cvtIDENT_EXPR x1118), ("init", cvtEXPR x1119)]
   and cvtFIELD_PATTERN {name=x1127, ptrn=x1128} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1127), ("ptrn", cvtPATTERN x1128)]
   and cvtFIELD_TYPE {name=x1134, ty=x1135} = PrettyRep.Rec [("name", cvtIDENT x1134), 
          ("ty", cvtTYPE_EXPR x1135)]
   and cvtTYPED_IDENT {name=x1141, ty=opt1143} = PrettyRep.Rec [("name", cvtIDENT x1141), 
          ("ty", 
       (case opt1143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1142 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1142))
       ))]
   and cvtFUNC_DEFN {kind=x1152, ns=x1153, final=b1154, native=b1155, override=b1156, 
          prototype=b1157, static=b1158, func=x1159} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1152), ("ns", cvtEXPR x1153), ("final", PrettyRep.Bool b1154), 
          ("native", PrettyRep.Bool b1155), ("override", PrettyRep.Bool b1156), 
          ("prototype", PrettyRep.Bool b1157), ("static", PrettyRep.Bool b1158), 
          ("func", cvtFUNC x1159)]
   and cvtCTOR_DEFN {ns=x1177, native=b1178, ctor=x1179} = PrettyRep.Rec [("ns", 
          cvtEXPR x1177), ("native", PrettyRep.Bool b1178), ("ctor", cvtCTOR x1179)]
   and cvtVAR_DEFN {kind=x1187, ns=x1188, static=b1189, prototype=b1190, bindings=ls1192} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1187), ("ns", cvtEXPR x1188), 
          ("static", PrettyRep.Bool b1189), ("prototype", PrettyRep.Bool b1190), 
          ("bindings", PrettyRep.List (List.map (fn x1191 => cvtVAR_BINDING x1191
                                                ) ls1192))]
   and cvtFIXTURES ls1210 = PrettyRep.List (List.map (fn (x1207, x1208) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1207, 
                                                            cvtFIXTURE x1208]
                                                     ) ls1210)
   and cvtINITS ls1217 = PrettyRep.List (List.map (fn (x1214, x1215) => PrettyRep.Tuple [cvtFIXTURE_NAME x1214, 
                                                         cvtEXPR x1215]
                                                  ) ls1217)
   and cvtNAMESPACE_DEFN {ident=x1221, ns=x1222, init=opt1224} = PrettyRep.Rec [("ident", 
          cvtIDENT x1221), ("ns", cvtEXPR x1222), ("init", 
       (case opt1224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1223 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1223))
       ))]
   and cvtCLASS_DEFN {ident=x1235, ns=x1236, nonnullable=b1237, dynamic=b1238, 
          final=b1239, params=ls1241, extends=opt1246, implements=ls1251, body=x1255} = 
          PrettyRep.Rec [("ident", cvtIDENT x1235), ("ns", cvtEXPR x1236), 
          ("nonnullable", PrettyRep.Bool b1237), ("dynamic", PrettyRep.Bool b1238), 
          ("final", PrettyRep.Bool b1239), ("params", PrettyRep.List (List.map (fn x1240 => 
                                                                                      cvtIDENT x1240
                                                                               ) ls1241)), 
          ("extends", 
       (case opt1246 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1245 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1245))
       )), ("implements", PrettyRep.List (List.map (fn x1250 => cvtIDENT_EXPR x1250
                                                   ) ls1251)), ("body", cvtBLOCK x1255)]
   and cvtINTERFACE_DEFN {ident=x1275, ns=x1276, nonnullable=b1277, params=ls1279, 
          extends=ls1284, body=x1288} = PrettyRep.Rec [("ident", cvtIDENT x1275), 
          ("ns", cvtEXPR x1276), ("nonnullable", PrettyRep.Bool b1277), ("params", 
          PrettyRep.List (List.map (fn x1278 => cvtIDENT x1278
                                   ) ls1279)), ("extends", PrettyRep.List (List.map (fn x1283 => 
                                                                                           cvtIDENT_EXPR x1283
                                                                                    ) ls1284)), 
          ("body", cvtBLOCK x1288)]
   and cvtTYPE_DEFN {ident=x1302, ns=x1303, init=x1304} = PrettyRep.Rec [("ident", 
          cvtIDENT x1302), ("ns", cvtEXPR x1303), ("init", cvtTYPE_EXPR x1304)]
   and cvtFOR_ENUM_STMT {ptrn=opt1313, obj=x1317, defns=ls1319, fixtures=opt1324, 
          contLabel=opt1329, body=x1333} = PrettyRep.Rec [("ptrn", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1312))
       )), ("obj", cvtEXPR x1317), ("defns", PrettyRep.List (List.map (fn x1318 => 
                                                                             cvtVAR_BINDING x1318
                                                                      ) ls1319)), 
          ("fixtures", 
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1323))
       )), ("contLabel", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1328))
       )), ("body", cvtSTMT x1333)]
   and cvtWHILE_STMT {cond=x1347, body=x1348, contLabel=opt1350} = PrettyRep.Rec [("cond", 
          cvtEXPR x1347), ("body", cvtSTMT x1348), ("contLabel", 
       (case opt1350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1349 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1349))
       ))]
   and cvtDIRECTIVES {pragmas=ls1362, defns=ls1367, stmts=ls1372, fixtures=opt1377, 
          inits=opt1382} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1361 => 
                                                                                      cvtPRAGMA x1361
                                                                               ) ls1362)), 
          ("defns", PrettyRep.List (List.map (fn x1366 => cvtDEFN x1366
                                             ) ls1367)), ("stmts", PrettyRep.List (List.map (fn x1371 => 
                                                                                                   cvtSTMT x1371
                                                                                            ) ls1372)), 
          ("fixtures", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1376))
       )), ("inits", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1381))
       ))]
   and cvtBINDINGS {b=ls1398, i=ls1403} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1397 => 
                                                                                               cvtVAR_BINDING x1397
                                                                                        ) ls1398)), 
          ("i", PrettyRep.List (List.map (fn x1402 => cvtEXPR x1402
                                         ) ls1403))]
   and cvtCASE {label=opt1413, body=x1417} = PrettyRep.Rec [("label", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1412))
       )), ("body", cvtBLOCK x1417)]
   and cvtTYPE_CASE {ptrn=opt1424, body=x1428} = PrettyRep.Rec [("ptrn", 
       (case opt1424 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1423 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1423))
       )), ("body", cvtBLOCK x1428)]
   and cvtFUNC_NAME {kind=x1434, ident=x1435} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1434), 
          ("ident", cvtIDENT x1435)]
   and cvtPACKAGE {name=x1441, body=x1442} = PrettyRep.Rec [("name", cvtUSTRING x1441), 
          ("body", cvtBLOCK x1442)]
   and cvtPROGRAM {packages=ls1449, fixtures=opt1454, body=x1458} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1448 => cvtPACKAGE x1448
                                   ) ls1449)), ("fixtures", 
       (case opt1454 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1453 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1453))
       )), ("body", cvtBLOCK x1458)]
end

