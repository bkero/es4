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
     | cvtEXPR (AllocTemp(n898, x899)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n898, 
          cvtEXPR x899]))
     | cvtEXPR (KillTemp n903) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n903))
     | cvtEXPR (GetTemp n906) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n906))
     | cvtEXPR (ListExpr ls910) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x909 => 
                                                                                                    cvtEXPR x909
                                                                                             ) ls910)))
     | cvtEXPR (SliceExpr(x916, x917, x918)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x916, cvtEXPR x917, cvtEXPR x918]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x922, ident=x923}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x922), ("ident", cvtUSTRING x923)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x931, expr=x932}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x931), ("expr", cvtEXPR x932)]))
     | cvtIDENT_EXPR (AttributeIdentifier x940) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x940))
     | cvtIDENT_EXPR (Identifier{ident=x943, openNamespaces=ls949}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x943), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls945 => PrettyRep.List (List.map (fn x944 => 
                                                                                cvtNAMESPACE x944
                                                                         ) ls945)
                                   ) ls949))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x960) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x960))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x963, typeParams=ls965}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x963), ("typeParams", 
          PrettyRep.List (List.map (fn x964 => cvtTYPE_EXPR x964
                                   ) ls965))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r978) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r978))
     | cvtLITERAL (LiteralBoolean b981) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b981))
     | cvtLITERAL (LiteralString x984) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x984))
     | cvtLITERAL (LiteralArray{exprs=ls988, ty=opt993}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x987 => 
                                                                         cvtEXPR x987
                                                                  ) ls988)), 
          ("ty", 
       (case opt993 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x992 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x992))
       ))]))
     | cvtLITERAL (LiteralXML ls1005) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1004 => 
                                                                                                            cvtEXPR x1004
                                                                                                     ) ls1005)))
     | cvtLITERAL (LiteralNamespace x1011) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1011))
     | cvtLITERAL (LiteralObject{expr=ls1015, ty=opt1020}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1014 => 
                                                                        cvtFIELD x1014
                                                                 ) ls1015)), 
          ("ty", 
       (case opt1020 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1019 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1019))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1031, ty=opt1033}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1031), ("ty", 
       (case opt1033 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1032 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1032))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x1044}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1044)]))
   and cvtBLOCK (Block x1050) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1050))
   and cvtPATTERN (ObjectPattern ls1054) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1053 => cvtFIELD_PATTERN x1053
                                         ) ls1054)))
     | cvtPATTERN (ArrayPattern ls1061) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1060 => 
                                                                                                                cvtPATTERN x1060
                                                                                                         ) ls1061)))
     | cvtPATTERN (SimplePattern x1067) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1067))
     | cvtPATTERN (IdentifierPattern x1070) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1070))
   and cvtFIXTURE (NamespaceFixture x1073) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1073))
     | cvtFIXTURE (ClassFixture x1076) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1076))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1080) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1080))
     | cvtFIXTURE (ValFixture{ty=x1083, readOnly=b1084, isOverride=b1085, isFinal=b1086, 
          init=opt1088}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1083), ("readOnly", PrettyRep.Bool b1084), ("isOverride", 
          PrettyRep.Bool b1085), ("isFinal", PrettyRep.Bool b1086), ("init", 
          
       (case opt1088 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1087 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1087))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1105, getter=opt1107, setter=opt1112}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1105), ("getter", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1106))
       )), ("setter", 
       (case opt1112 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1111 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1111))
       ))]))
   and cvtFIELD {kind=x1125, name=x1126, init=x1127} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1125), ("name", cvtIDENT_EXPR x1126), ("init", cvtEXPR x1127)]
   and cvtFIELD_PATTERN {name=x1135, ptrn=x1136} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1135), ("ptrn", cvtPATTERN x1136)]
   and cvtFIELD_TYPE {name=x1142, ty=x1143} = PrettyRep.Rec [("name", cvtIDENT x1142), 
          ("ty", cvtTYPE_EXPR x1143)]
   and cvtTYPED_IDENT {name=x1149, ty=opt1151} = PrettyRep.Rec [("name", cvtIDENT x1149), 
          ("ty", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1150))
       ))]
   and cvtFUNC_DEFN {kind=x1160, ns=x1161, final=b1162, native=b1163, override=b1164, 
          prototype=b1165, static=b1166, func=x1167} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1160), ("ns", cvtEXPR x1161), ("final", PrettyRep.Bool b1162), 
          ("native", PrettyRep.Bool b1163), ("override", PrettyRep.Bool b1164), 
          ("prototype", PrettyRep.Bool b1165), ("static", PrettyRep.Bool b1166), 
          ("func", cvtFUNC x1167)]
   and cvtCTOR_DEFN {ns=x1185, native=b1186, ctor=x1187} = PrettyRep.Rec [("ns", 
          cvtEXPR x1185), ("native", PrettyRep.Bool b1186), ("ctor", cvtCTOR x1187)]
   and cvtVAR_DEFN {kind=x1195, ns=x1196, static=b1197, prototype=b1198, bindings=ls1200} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1195), ("ns", cvtEXPR x1196), 
          ("static", PrettyRep.Bool b1197), ("prototype", PrettyRep.Bool b1198), 
          ("bindings", PrettyRep.List (List.map (fn x1199 => cvtVAR_BINDING x1199
                                                ) ls1200))]
   and cvtFIXTURES ls1218 = PrettyRep.List (List.map (fn (x1215, x1216) => 
                                                            PrettyRep.Tuple [cvtNAME x1215, 
                                                            cvtFIXTURE x1216]
                                                     ) ls1218)
   and cvtINITS ls1225 = PrettyRep.List (List.map (fn (x1222, x1223) => PrettyRep.Tuple [cvtNAME x1222, 
                                                         cvtEXPR x1223]
                                                  ) ls1225)
   and cvtNAMESPACE_DEFN {ident=x1229, ns=x1230, init=opt1232} = PrettyRep.Rec [("ident", 
          cvtIDENT x1229), ("ns", cvtEXPR x1230), ("init", 
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1231))
       ))]
   and cvtCLASS_DEFN {ident=x1243, ns=x1244, nonnullable=b1245, dynamic=b1246, 
          final=b1247, params=ls1249, extends=opt1254, implements=ls1259, body=x1263} = 
          PrettyRep.Rec [("ident", cvtIDENT x1243), ("ns", cvtEXPR x1244), 
          ("nonnullable", PrettyRep.Bool b1245), ("dynamic", PrettyRep.Bool b1246), 
          ("final", PrettyRep.Bool b1247), ("params", PrettyRep.List (List.map (fn x1248 => 
                                                                                      cvtIDENT x1248
                                                                               ) ls1249)), 
          ("extends", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1253))
       )), ("implements", PrettyRep.List (List.map (fn x1258 => cvtIDENT_EXPR x1258
                                                   ) ls1259)), ("body", cvtBLOCK x1263)]
   and cvtINTERFACE_DEFN {ident=x1283, ns=x1284, nonnullable=b1285, params=ls1287, 
          extends=ls1292, body=x1296} = PrettyRep.Rec [("ident", cvtIDENT x1283), 
          ("ns", cvtEXPR x1284), ("nonnullable", PrettyRep.Bool b1285), ("params", 
          PrettyRep.List (List.map (fn x1286 => cvtIDENT x1286
                                   ) ls1287)), ("extends", PrettyRep.List (List.map (fn x1291 => 
                                                                                           cvtIDENT_EXPR x1291
                                                                                    ) ls1292)), 
          ("body", cvtBLOCK x1296)]
   and cvtTYPE_DEFN {ident=x1310, ns=x1311, init=x1312} = PrettyRep.Rec [("ident", 
          cvtIDENT x1310), ("ns", cvtEXPR x1311), ("init", cvtTYPE_EXPR x1312)]
   and cvtFOR_ENUM_STMT {ptrn=opt1321, obj=x1325, defns=ls1327, fixtures=opt1332, 
          contLabel=opt1337, body=x1341} = PrettyRep.Rec [("ptrn", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1320))
       )), ("obj", cvtEXPR x1325), ("defns", PrettyRep.List (List.map (fn x1326 => 
                                                                             cvtVAR_BINDING x1326
                                                                      ) ls1327)), 
          ("fixtures", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1331))
       )), ("contLabel", 
       (case opt1337 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1336 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1336))
       )), ("body", cvtSTMT x1341)]
   and cvtWHILE_STMT {cond=x1355, body=x1356, contLabel=opt1358} = PrettyRep.Rec [("cond", 
          cvtEXPR x1355), ("body", cvtSTMT x1356), ("contLabel", 
       (case opt1358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1357))
       ))]
   and cvtDIRECTIVES {pragmas=ls1370, defns=ls1375, stmts=ls1380, fixtures=opt1385, 
          inits=opt1394} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1369 => 
                                                                                      cvtPRAGMA x1369
                                                                               ) ls1370)), 
          ("defns", PrettyRep.List (List.map (fn x1374 => cvtDEFN x1374
                                             ) ls1375)), ("stmts", PrettyRep.List (List.map (fn x1379 => 
                                                                                                   cvtSTMT x1379
                                                                                            ) ls1380)), 
          ("fixtures", 
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1384))
       )), ("inits", 
       (case opt1394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1390 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1389 => 
                                                                                      cvtSTMT x1389
                                                                               ) ls1390)))
       ))]
   and cvtBINDINGS {b=ls1410, i=ls1415} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1409 => 
                                                                                               cvtVAR_BINDING x1409
                                                                                        ) ls1410)), 
          ("i", PrettyRep.List (List.map (fn x1414 => cvtEXPR x1414
                                         ) ls1415))]
   and cvtCASE {label=opt1429, body=x1433} = PrettyRep.Rec [("label", 
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1425 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1424 => 
                                                                                      cvtEXPR x1424
                                                                               ) ls1425)))
       )), ("body", cvtBLOCK x1433)]
   and cvtTYPE_CASE {ptrn=opt1440, body=x1444} = PrettyRep.Rec [("ptrn", 
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1439 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1439))
       )), ("body", cvtBLOCK x1444)]
   and cvtFUNC_NAME {kind=x1450, ident=x1451} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1450), 
          ("ident", cvtIDENT x1451)]
   and cvtPACKAGE {name=x1457, body=x1458} = PrettyRep.Rec [("name", cvtUSTRING x1457), 
          ("body", cvtBLOCK x1458)]
   and cvtPROGRAM {packages=ls1465, fixtures=opt1470, body=x1474} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1464 => cvtPACKAGE x1464
                                   ) ls1465)), ("fixtures", 
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1469))
       )), ("body", cvtBLOCK x1474)]
end

