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
     | cvtLITERAL (LiteralFunction{func=x1031, ty=x1032}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1031), ("ty", cvtTYPE_EXPR x1032)]))
     | cvtLITERAL (LiteralRegExp{str=x1040}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1040)]))
   and cvtBLOCK (Block x1046) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1046))
   and cvtPATTERN (ObjectPattern ls1050) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1049 => cvtFIELD_PATTERN x1049
                                         ) ls1050)))
     | cvtPATTERN (ArrayPattern ls1057) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1056 => 
                                                                                                                cvtPATTERN x1056
                                                                                                         ) ls1057)))
     | cvtPATTERN (SimplePattern x1063) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1063))
     | cvtPATTERN (IdentifierPattern x1066) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1066))
   and cvtFIXTURE (NamespaceFixture x1069) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1069))
     | cvtFIXTURE (ClassFixture x1072) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1072))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1076) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1076))
     | cvtFIXTURE (ValFixture{ty=x1079, readOnly=b1080, isOverride=b1081, isFinal=b1082, 
          init=opt1084}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1079), ("readOnly", PrettyRep.Bool b1080), ("isOverride", 
          PrettyRep.Bool b1081), ("isFinal", PrettyRep.Bool b1082), ("init", 
          
       (case opt1084 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1083 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1083))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1101, getter=opt1103, setter=opt1108}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1101), ("getter", 
       (case opt1103 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1102 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1102))
       )), ("setter", 
       (case opt1108 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1107 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1107))
       ))]))
   and cvtFIELD {kind=x1121, name=x1122, init=x1123} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1121), ("name", cvtIDENT_EXPR x1122), ("init", cvtEXPR x1123)]
   and cvtFIELD_PATTERN {name=x1131, ptrn=x1132} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1131), ("ptrn", cvtPATTERN x1132)]
   and cvtFIELD_TYPE {name=x1138, ty=x1139} = PrettyRep.Rec [("name", cvtIDENT x1138), 
          ("ty", cvtTYPE_EXPR x1139)]
   and cvtTYPED_IDENT {name=x1145, ty=opt1147} = PrettyRep.Rec [("name", cvtIDENT x1145), 
          ("ty", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1146))
       ))]
   and cvtATTRIBUTES {ns=x1156, override=b1157, static=b1158, final=b1159, 
          dynamic=b1160, prototype=b1161, native=b1162, rest=b1163} = PrettyRep.Rec [("ns", 
          cvtEXPR x1156), ("override", PrettyRep.Bool b1157), ("static", PrettyRep.Bool b1158), 
          ("final", PrettyRep.Bool b1159), ("dynamic", PrettyRep.Bool b1160), 
          ("prototype", PrettyRep.Bool b1161), ("native", PrettyRep.Bool b1162), 
          ("rest", PrettyRep.Bool b1163)]
   and cvtFUNC_DEFN {kind=x1181, ns=x1182, final=b1183, native=b1184, override=b1185, 
          prototype=b1186, static=b1187, func=x1188} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1181), ("ns", cvtEXPR x1182), ("final", PrettyRep.Bool b1183), 
          ("native", PrettyRep.Bool b1184), ("override", PrettyRep.Bool b1185), 
          ("prototype", PrettyRep.Bool b1186), ("static", PrettyRep.Bool b1187), 
          ("func", cvtFUNC x1188)]
   and cvtCTOR_DEFN {ns=x1206, native=b1207, ctor=x1208} = PrettyRep.Rec [("ns", 
          cvtEXPR x1206), ("native", PrettyRep.Bool b1207), ("ctor", cvtCTOR x1208)]
   and cvtVAR_DEFN {kind=x1216, ns=x1217, static=b1218, prototype=b1219, bindings=ls1221} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1216), ("ns", cvtEXPR x1217), 
          ("static", PrettyRep.Bool b1218), ("prototype", PrettyRep.Bool b1219), 
          ("bindings", PrettyRep.List (List.map (fn x1220 => cvtVAR_BINDING x1220
                                                ) ls1221))]
   and cvtFIXTURES ls1239 = PrettyRep.List (List.map (fn (x1236, x1237) => 
                                                            PrettyRep.Tuple [cvtNAME x1236, 
                                                            cvtFIXTURE x1237]
                                                     ) ls1239)
   and cvtINITS ls1246 = PrettyRep.List (List.map (fn (x1243, x1244) => PrettyRep.Tuple [cvtNAME x1243, 
                                                         cvtEXPR x1244]
                                                  ) ls1246)
   and cvtNAMESPACE_DEFN {ident=x1250, ns=x1251, init=opt1253} = PrettyRep.Rec [("ident", 
          cvtIDENT x1250), ("ns", cvtEXPR x1251), ("init", 
       (case opt1253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1252 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1252))
       ))]
   and cvtCLASS_DEFN {ident=x1264, ns=x1265, nonnullable=b1266, dynamic=b1267, 
          final=b1268, params=ls1270, extends=opt1275, implements=ls1280, body=x1284} = 
          PrettyRep.Rec [("ident", cvtIDENT x1264), ("ns", cvtEXPR x1265), 
          ("nonnullable", PrettyRep.Bool b1266), ("dynamic", PrettyRep.Bool b1267), 
          ("final", PrettyRep.Bool b1268), ("params", PrettyRep.List (List.map (fn x1269 => 
                                                                                      cvtIDENT x1269
                                                                               ) ls1270)), 
          ("extends", 
       (case opt1275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1274 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1274))
       )), ("implements", PrettyRep.List (List.map (fn x1279 => cvtIDENT_EXPR x1279
                                                   ) ls1280)), ("body", cvtBLOCK x1284)]
   and cvtINTERFACE_DEFN {ident=x1304, ns=x1305, nonnullable=b1306, params=ls1308, 
          extends=ls1313, body=x1317} = PrettyRep.Rec [("ident", cvtIDENT x1304), 
          ("ns", cvtEXPR x1305), ("nonnullable", PrettyRep.Bool b1306), ("params", 
          PrettyRep.List (List.map (fn x1307 => cvtIDENT x1307
                                   ) ls1308)), ("extends", PrettyRep.List (List.map (fn x1312 => 
                                                                                           cvtIDENT_EXPR x1312
                                                                                    ) ls1313)), 
          ("body", cvtBLOCK x1317)]
   and cvtTYPE_DEFN {ident=x1331, ns=x1332, init=x1333} = PrettyRep.Rec [("ident", 
          cvtIDENT x1331), ("ns", cvtEXPR x1332), ("init", cvtTYPE_EXPR x1333)]
   and cvtFOR_ENUM_STMT {ptrn=opt1342, obj=x1346, defns=ls1348, fixtures=opt1353, 
          contLabel=opt1358, body=x1362} = PrettyRep.Rec [("ptrn", 
       (case opt1342 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1341 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1341))
       )), ("obj", cvtEXPR x1346), ("defns", PrettyRep.List (List.map (fn x1347 => 
                                                                             cvtVAR_BINDING x1347
                                                                      ) ls1348)), 
          ("fixtures", 
       (case opt1353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1352))
       )), ("contLabel", 
       (case opt1358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1357))
       )), ("body", cvtSTMT x1362)]
   and cvtWHILE_STMT {cond=x1376, body=x1377, contLabel=opt1379} = PrettyRep.Rec [("cond", 
          cvtEXPR x1376), ("body", cvtSTMT x1377), ("contLabel", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1378 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1378))
       ))]
   and cvtDIRECTIVES {pragmas=ls1391, defns=ls1396, stmts=ls1401, fixtures=opt1406, 
          inits=opt1415} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1390 => 
                                                                                      cvtPRAGMA x1390
                                                                               ) ls1391)), 
          ("defns", PrettyRep.List (List.map (fn x1395 => cvtDEFN x1395
                                             ) ls1396)), ("stmts", PrettyRep.List (List.map (fn x1400 => 
                                                                                                   cvtSTMT x1400
                                                                                            ) ls1401)), 
          ("fixtures", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1405))
       )), ("inits", 
       (case opt1415 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1411 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1410 => 
                                                                                      cvtSTMT x1410
                                                                               ) ls1411)))
       ))]
   and cvtBINDINGS {b=ls1431, i=ls1436} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1430 => 
                                                                                               cvtVAR_BINDING x1430
                                                                                        ) ls1431)), 
          ("i", PrettyRep.List (List.map (fn x1435 => cvtEXPR x1435
                                         ) ls1436))]
   and cvtCASE {label=opt1446, body=x1450} = PrettyRep.Rec [("label", 
       (case opt1446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1445 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1445))
       )), ("body", cvtBLOCK x1450)]
   and cvtTYPE_CASE {ptrn=opt1457, body=x1461} = PrettyRep.Rec [("ptrn", 
       (case opt1457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1456 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1456))
       )), ("body", cvtBLOCK x1461)]
   and cvtFUNC_NAME {kind=x1467, ident=x1468} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1467), 
          ("ident", cvtIDENT x1468)]
   and cvtPACKAGE {name=x1474, body=x1475} = PrettyRep.Rec [("name", cvtUSTRING x1474), 
          ("body", cvtBLOCK x1475)]
   and cvtPROGRAM {packages=ls1482, fixtures=opt1487, body=x1491} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1481 => cvtPACKAGE x1481
                                   ) ls1482)), ("fixtures", 
       (case opt1487 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1486 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1486))
       )), ("body", cvtBLOCK x1491)]
end

