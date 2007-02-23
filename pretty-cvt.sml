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
   and cvtFIXTURE_NAME (TempName n922) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n922))
     | cvtFIXTURE_NAME (PropName x925) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x925))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x928, ident=x929}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x928), ("ident", cvtUSTRING x929)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x937, expr=x938}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x937), ("expr", cvtEXPR x938)]))
     | cvtIDENT_EXPR (AttributeIdentifier x946) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x946))
     | cvtIDENT_EXPR (Identifier{ident=x949, openNamespaces=ls955}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x949), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls951 => PrettyRep.List (List.map (fn x950 => 
                                                                                cvtNAMESPACE x950
                                                                         ) ls951)
                                   ) ls955))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x966) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x966))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x969, typeParams=ls971}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x969), ("typeParams", 
          PrettyRep.List (List.map (fn x970 => cvtTYPE_EXPR x970
                                   ) ls971))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r984) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r984))
     | cvtLITERAL (LiteralBoolean b987) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b987))
     | cvtLITERAL (LiteralString x990) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x990))
     | cvtLITERAL (LiteralArray{exprs=ls994, ty=opt999}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x993 => 
                                                                         cvtEXPR x993
                                                                  ) ls994)), 
          ("ty", 
       (case opt999 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x998 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x998))
       ))]))
     | cvtLITERAL (LiteralXML ls1011) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1010 => 
                                                                                                            cvtEXPR x1010
                                                                                                     ) ls1011)))
     | cvtLITERAL (LiteralNamespace x1017) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1017))
     | cvtLITERAL (LiteralObject{expr=ls1021, ty=opt1026}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1020 => 
                                                                        cvtFIELD x1020
                                                                 ) ls1021)), 
          ("ty", 
       (case opt1026 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1025 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1025))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1037, ty=x1038}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1037), ("ty", cvtTYPE_EXPR x1038)]))
     | cvtLITERAL (LiteralRegExp{str=x1046}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1046)]))
   and cvtBLOCK (Block x1052) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1052))
   and cvtPATTERN (ObjectPattern ls1056) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1055 => cvtFIELD_PATTERN x1055
                                         ) ls1056)))
     | cvtPATTERN (ArrayPattern ls1063) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1062 => 
                                                                                                                cvtPATTERN x1062
                                                                                                         ) ls1063)))
     | cvtPATTERN (SimplePattern x1069) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1069))
     | cvtPATTERN (IdentifierPattern x1072) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1072))
   and cvtFIXTURE (NamespaceFixture x1075) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1075))
     | cvtFIXTURE (ClassFixture x1078) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1078))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1082) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1082))
     | cvtFIXTURE (ValFixture{ty=x1085, readOnly=b1086, isOverride=b1087, isFinal=b1088, 
          init=opt1090}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1085), ("readOnly", PrettyRep.Bool b1086), ("isOverride", 
          PrettyRep.Bool b1087), ("isFinal", PrettyRep.Bool b1088), ("init", 
          
       (case opt1090 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1089 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1089))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1107, getter=opt1109, setter=opt1114}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1107), ("getter", 
       (case opt1109 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1108 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1108))
       )), ("setter", 
       (case opt1114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1113 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1113))
       ))]))
   and cvtFIELD {kind=x1127, name=x1128, init=x1129} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1127), ("name", cvtIDENT_EXPR x1128), ("init", cvtEXPR x1129)]
   and cvtFIELD_PATTERN {name=x1137, ptrn=x1138} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1137), ("ptrn", cvtPATTERN x1138)]
   and cvtFIELD_TYPE {name=x1144, ty=x1145} = PrettyRep.Rec [("name", cvtIDENT x1144), 
          ("ty", cvtTYPE_EXPR x1145)]
   and cvtTYPED_IDENT {name=x1151, ty=opt1153} = PrettyRep.Rec [("name", cvtIDENT x1151), 
          ("ty", 
       (case opt1153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1152 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1152))
       ))]
   and cvtFUNC_DEFN {kind=x1162, ns=x1163, final=b1164, native=b1165, override=b1166, 
          prototype=b1167, static=b1168, func=x1169} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1162), ("ns", cvtEXPR x1163), ("final", PrettyRep.Bool b1164), 
          ("native", PrettyRep.Bool b1165), ("override", PrettyRep.Bool b1166), 
          ("prototype", PrettyRep.Bool b1167), ("static", PrettyRep.Bool b1168), 
          ("func", cvtFUNC x1169)]
   and cvtCTOR_DEFN {ns=x1187, native=b1188, ctor=x1189} = PrettyRep.Rec [("ns", 
          cvtEXPR x1187), ("native", PrettyRep.Bool b1188), ("ctor", cvtCTOR x1189)]
   and cvtVAR_DEFN {kind=x1197, ns=x1198, static=b1199, prototype=b1200, bindings=ls1202} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1197), ("ns", cvtEXPR x1198), 
          ("static", PrettyRep.Bool b1199), ("prototype", PrettyRep.Bool b1200), 
          ("bindings", PrettyRep.List (List.map (fn x1201 => cvtVAR_BINDING x1201
                                                ) ls1202))]
   and cvtFIXTURES ls1220 = PrettyRep.List (List.map (fn (x1217, x1218) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1217, 
                                                            cvtFIXTURE x1218]
                                                     ) ls1220)
   and cvtINITS ls1227 = PrettyRep.List (List.map (fn (x1224, x1225) => PrettyRep.Tuple [cvtFIXTURE_NAME x1224, 
                                                         cvtEXPR x1225]
                                                  ) ls1227)
   and cvtNAMESPACE_DEFN {ident=x1231, ns=x1232, init=opt1234} = PrettyRep.Rec [("ident", 
          cvtIDENT x1231), ("ns", cvtEXPR x1232), ("init", 
       (case opt1234 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1233 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1233))
       ))]
   and cvtCLASS_DEFN {ident=x1245, ns=x1246, nonnullable=b1247, dynamic=b1248, 
          final=b1249, params=ls1251, extends=opt1256, implements=ls1261, body=x1265} = 
          PrettyRep.Rec [("ident", cvtIDENT x1245), ("ns", cvtEXPR x1246), 
          ("nonnullable", PrettyRep.Bool b1247), ("dynamic", PrettyRep.Bool b1248), 
          ("final", PrettyRep.Bool b1249), ("params", PrettyRep.List (List.map (fn x1250 => 
                                                                                      cvtIDENT x1250
                                                                               ) ls1251)), 
          ("extends", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1255))
       )), ("implements", PrettyRep.List (List.map (fn x1260 => cvtIDENT_EXPR x1260
                                                   ) ls1261)), ("body", cvtBLOCK x1265)]
   and cvtINTERFACE_DEFN {ident=x1285, ns=x1286, nonnullable=b1287, params=ls1289, 
          extends=ls1294, body=x1298} = PrettyRep.Rec [("ident", cvtIDENT x1285), 
          ("ns", cvtEXPR x1286), ("nonnullable", PrettyRep.Bool b1287), ("params", 
          PrettyRep.List (List.map (fn x1288 => cvtIDENT x1288
                                   ) ls1289)), ("extends", PrettyRep.List (List.map (fn x1293 => 
                                                                                           cvtIDENT_EXPR x1293
                                                                                    ) ls1294)), 
          ("body", cvtBLOCK x1298)]
   and cvtTYPE_DEFN {ident=x1312, ns=x1313, init=x1314} = PrettyRep.Rec [("ident", 
          cvtIDENT x1312), ("ns", cvtEXPR x1313), ("init", cvtTYPE_EXPR x1314)]
   and cvtFOR_ENUM_STMT {ptrn=opt1323, obj=x1327, defns=ls1329, fixtures=opt1334, 
          contLabel=opt1339, body=x1343} = PrettyRep.Rec [("ptrn", 
       (case opt1323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1322 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1322))
       )), ("obj", cvtEXPR x1327), ("defns", PrettyRep.List (List.map (fn x1328 => 
                                                                             cvtVAR_BINDING x1328
                                                                      ) ls1329)), 
          ("fixtures", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1333))
       )), ("contLabel", 
       (case opt1339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1338 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1338))
       )), ("body", cvtSTMT x1343)]
   and cvtWHILE_STMT {cond=x1357, body=x1358, contLabel=opt1360} = PrettyRep.Rec [("cond", 
          cvtEXPR x1357), ("body", cvtSTMT x1358), ("contLabel", 
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1359))
       ))]
   and cvtDIRECTIVES {pragmas=ls1372, defns=ls1377, stmts=ls1382, fixtures=opt1387, 
          inits=opt1396} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1371 => 
                                                                                      cvtPRAGMA x1371
                                                                               ) ls1372)), 
          ("defns", PrettyRep.List (List.map (fn x1376 => cvtDEFN x1376
                                             ) ls1377)), ("stmts", PrettyRep.List (List.map (fn x1381 => 
                                                                                                   cvtSTMT x1381
                                                                                            ) ls1382)), 
          ("fixtures", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1386 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1386))
       )), ("inits", 
       (case opt1396 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1392 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1391 => 
                                                                                      cvtSTMT x1391
                                                                               ) ls1392)))
       ))]
   and cvtBINDINGS {b=ls1412, i=ls1417} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1411 => 
                                                                                               cvtVAR_BINDING x1411
                                                                                        ) ls1412)), 
          ("i", PrettyRep.List (List.map (fn x1416 => cvtEXPR x1416
                                         ) ls1417))]
   and cvtCASE {label=opt1427, body=x1431} = PrettyRep.Rec [("label", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1426))
       )), ("body", cvtBLOCK x1431)]
   and cvtTYPE_CASE {ptrn=opt1438, body=x1442} = PrettyRep.Rec [("ptrn", 
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1437))
       )), ("body", cvtBLOCK x1442)]
   and cvtFUNC_NAME {kind=x1448, ident=x1449} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1448), 
          ("ident", cvtIDENT x1449)]
   and cvtPACKAGE {name=x1455, body=x1456} = PrettyRep.Rec [("name", cvtUSTRING x1455), 
          ("body", cvtBLOCK x1456)]
   and cvtPROGRAM {packages=ls1463, fixtures=opt1468, body=x1472} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1462 => cvtPACKAGE x1462
                                   ) ls1463)), ("fixtures", 
       (case opt1468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1467))
       )), ("body", cvtBLOCK x1472)]
end

