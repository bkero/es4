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
     | cvtEXPR (FunExpr x877) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x877))
     | cvtEXPR (ObjectRef{base=x880, ident=x881}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x880), ("ident", cvtIDENT_EXPR x881)]))
     | cvtEXPR (LexicalRef{ident=x889}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x889)]))
     | cvtEXPR (SetExpr(x895, x896, x897)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x895, 
          cvtPATTERN x896, cvtEXPR x897]))
     | cvtEXPR (AllocTemp(n901, x902)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n901, 
          cvtEXPR x902]))
     | cvtEXPR (KillTemp n906) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n906))
     | cvtEXPR (GetTemp n909) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n909))
     | cvtEXPR (ListExpr ls913) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x912 => 
                                                                                                    cvtEXPR x912
                                                                                             ) ls913)))
     | cvtEXPR (SliceExpr(x919, x920, x921)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x919, cvtEXPR x920, cvtEXPR x921]))
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
     | cvtLITERAL (LiteralRegExp{str=x1034}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1034)]))
   and cvtBLOCK (Block x1040) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1040))
   and cvtPATTERN (ObjectPattern ls1044) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1043 => cvtFIELD_PATTERN x1043
                                         ) ls1044)))
     | cvtPATTERN (ArrayPattern ls1051) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1050 => 
                                                                                                                cvtPATTERN x1050
                                                                                                         ) ls1051)))
     | cvtPATTERN (SimplePattern x1057) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1057))
     | cvtPATTERN (IdentifierPattern x1060) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1060))
   and cvtFIXTURE (NamespaceFixture x1063) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1063))
     | cvtFIXTURE (ClassFixture x1066) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1066))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1070) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1070))
     | cvtFIXTURE (ValFixture{ty=x1073, readOnly=b1074, isOverride=b1075, isFinal=b1076, 
          init=opt1078}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1073), ("readOnly", PrettyRep.Bool b1074), ("isOverride", 
          PrettyRep.Bool b1075), ("isFinal", PrettyRep.Bool b1076), ("init", 
          
       (case opt1078 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1077 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1077))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1095, getter=opt1097, setter=opt1102}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1095), ("getter", 
       (case opt1097 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1096 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1096))
       )), ("setter", 
       (case opt1102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1101 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1101))
       ))]))
   and cvtFIELD {kind=x1115, name=x1116, init=x1117} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1115), ("name", cvtIDENT_EXPR x1116), ("init", cvtEXPR x1117)]
   and cvtFIELD_PATTERN {name=x1125, ptrn=x1126} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1125), ("ptrn", cvtPATTERN x1126)]
   and cvtFIELD_TYPE {name=x1132, ty=x1133} = PrettyRep.Rec [("name", cvtIDENT x1132), 
          ("ty", cvtTYPE_EXPR x1133)]
   and cvtTYPED_IDENT {name=x1139, ty=opt1141} = PrettyRep.Rec [("name", cvtIDENT x1139), 
          ("ty", 
       (case opt1141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1140 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1140))
       ))]
   and cvtATTRIBUTES {ns=x1150, override=b1151, static=b1152, final=b1153, 
          dynamic=b1154, prototype=b1155, native=b1156, rest=b1157} = PrettyRep.Rec [("ns", 
          cvtEXPR x1150), ("override", PrettyRep.Bool b1151), ("static", PrettyRep.Bool b1152), 
          ("final", PrettyRep.Bool b1153), ("dynamic", PrettyRep.Bool b1154), 
          ("prototype", PrettyRep.Bool b1155), ("native", PrettyRep.Bool b1156), 
          ("rest", PrettyRep.Bool b1157)]
   and cvtFUNC_DEFN {kind=x1175, ns=x1176, final=b1177, native=b1178, override=b1179, 
          prototype=b1180, static=b1181, func=x1182} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1175), ("ns", cvtEXPR x1176), ("final", PrettyRep.Bool b1177), 
          ("native", PrettyRep.Bool b1178), ("override", PrettyRep.Bool b1179), 
          ("prototype", PrettyRep.Bool b1180), ("static", PrettyRep.Bool b1181), 
          ("func", cvtFUNC x1182)]
   and cvtCTOR_DEFN {ns=x1200, native=b1201, ctor=x1202} = PrettyRep.Rec [("ns", 
          cvtEXPR x1200), ("native", PrettyRep.Bool b1201), ("ctor", cvtCTOR x1202)]
   and cvtVAR_DEFN {kind=x1210, ns=x1211, static=b1212, prototype=b1213, bindings=ls1215} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1210), ("ns", cvtEXPR x1211), 
          ("static", PrettyRep.Bool b1212), ("prototype", PrettyRep.Bool b1213), 
          ("bindings", PrettyRep.List (List.map (fn x1214 => cvtVAR_BINDING x1214
                                                ) ls1215))]
   and cvtFIXTURES ls1233 = PrettyRep.List (List.map (fn (x1230, x1231) => 
                                                            PrettyRep.Tuple [cvtNAME x1230, 
                                                            cvtFIXTURE x1231]
                                                     ) ls1233)
   and cvtINITS ls1240 = PrettyRep.List (List.map (fn (x1237, x1238) => PrettyRep.Tuple [cvtNAME x1237, 
                                                         cvtEXPR x1238]
                                                  ) ls1240)
   and cvtNAMESPACE_DEFN {ident=x1244, ns=x1245, init=opt1247} = PrettyRep.Rec [("ident", 
          cvtIDENT x1244), ("ns", cvtEXPR x1245), ("init", 
       (case opt1247 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1246 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1246))
       ))]
   and cvtCLASS_DEFN {ident=x1258, ns=x1259, nonnullable=b1260, dynamic=b1261, 
          final=b1262, params=ls1264, extends=opt1269, implements=ls1274, body=x1278} = 
          PrettyRep.Rec [("ident", cvtIDENT x1258), ("ns", cvtEXPR x1259), 
          ("nonnullable", PrettyRep.Bool b1260), ("dynamic", PrettyRep.Bool b1261), 
          ("final", PrettyRep.Bool b1262), ("params", PrettyRep.List (List.map (fn x1263 => 
                                                                                      cvtIDENT x1263
                                                                               ) ls1264)), 
          ("extends", 
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1268))
       )), ("implements", PrettyRep.List (List.map (fn x1273 => cvtIDENT_EXPR x1273
                                                   ) ls1274)), ("body", cvtBLOCK x1278)]
   and cvtINTERFACE_DEFN {ident=x1298, ns=x1299, nonnullable=b1300, params=ls1302, 
          extends=ls1307, body=x1311} = PrettyRep.Rec [("ident", cvtIDENT x1298), 
          ("ns", cvtEXPR x1299), ("nonnullable", PrettyRep.Bool b1300), ("params", 
          PrettyRep.List (List.map (fn x1301 => cvtIDENT x1301
                                   ) ls1302)), ("extends", PrettyRep.List (List.map (fn x1306 => 
                                                                                           cvtIDENT_EXPR x1306
                                                                                    ) ls1307)), 
          ("body", cvtBLOCK x1311)]
   and cvtTYPE_DEFN {ident=x1325, ns=x1326, init=x1327} = PrettyRep.Rec [("ident", 
          cvtIDENT x1325), ("ns", cvtEXPR x1326), ("init", cvtTYPE_EXPR x1327)]
   and cvtFOR_ENUM_STMT {ptrn=opt1336, obj=x1340, defns=ls1342, fixtures=opt1347, 
          contLabel=opt1352, body=x1356} = PrettyRep.Rec [("ptrn", 
       (case opt1336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1335 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1335))
       )), ("obj", cvtEXPR x1340), ("defns", PrettyRep.List (List.map (fn x1341 => 
                                                                             cvtVAR_BINDING x1341
                                                                      ) ls1342)), 
          ("fixtures", 
       (case opt1347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1346))
       )), ("contLabel", 
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1351))
       )), ("body", cvtSTMT x1356)]
   and cvtWHILE_STMT {cond=x1370, body=x1371, contLabel=opt1373} = PrettyRep.Rec [("cond", 
          cvtEXPR x1370), ("body", cvtSTMT x1371), ("contLabel", 
       (case opt1373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1372 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1372))
       ))]
   and cvtDIRECTIVES {pragmas=ls1385, defns=ls1390, stmts=ls1395, fixtures=opt1400, 
          inits=opt1409} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1384 => 
                                                                                      cvtPRAGMA x1384
                                                                               ) ls1385)), 
          ("defns", PrettyRep.List (List.map (fn x1389 => cvtDEFN x1389
                                             ) ls1390)), ("stmts", PrettyRep.List (List.map (fn x1394 => 
                                                                                                   cvtSTMT x1394
                                                                                            ) ls1395)), 
          ("fixtures", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1399))
       )), ("inits", 
       (case opt1409 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1405 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1404 => 
                                                                                      cvtSTMT x1404
                                                                               ) ls1405)))
       ))]
   and cvtBINDINGS {b=ls1425, i=ls1430} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1424 => 
                                                                                               cvtVAR_BINDING x1424
                                                                                        ) ls1425)), 
          ("i", PrettyRep.List (List.map (fn x1429 => cvtEXPR x1429
                                         ) ls1430))]
   and cvtCASE {label=opt1444, body=x1448} = PrettyRep.Rec [("label", 
       (case opt1444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1440 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1439 => 
                                                                                      cvtEXPR x1439
                                                                               ) ls1440)))
       )), ("body", cvtBLOCK x1448)]
   and cvtTYPE_CASE {ptrn=opt1455, body=x1459} = PrettyRep.Rec [("ptrn", 
       (case opt1455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1454 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1454))
       )), ("body", cvtBLOCK x1459)]
   and cvtFUNC_NAME {kind=x1465, ident=x1466} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1465), 
          ("ident", cvtIDENT x1466)]
   and cvtPACKAGE {name=x1472, body=x1473} = PrettyRep.Rec [("name", cvtUSTRING x1472), 
          ("body", cvtBLOCK x1473)]
   and cvtPROGRAM {packages=ls1480, fixtures=opt1485, body=x1489} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1479 => cvtPACKAGE x1479
                                   ) ls1480)), ("fixtures", 
       (case opt1485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1484 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1484))
       )), ("body", cvtBLOCK x1489)]
end

