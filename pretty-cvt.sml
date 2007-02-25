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
          thisType=opt462, hasRest=b466, requiredCount=n467}) = PrettyRep.Ctor ("FunctionType", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x450 => 
                                                                              cvtIDENT x450
                                                                       ) ls451)), 
          ("params", PrettyRep.List (List.map (fn x455 => cvtTYPE_EXPR x455
                                              ) ls456)), ("result", cvtTYPE_EXPR x460), 
          ("thisType", 
       (case opt462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x461 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x461))
       )), ("hasRest", PrettyRep.Bool b466), ("requiredCount", PrettyRep.Int n467)]))
     | cvtTYPE_EXPR (ObjectType ls484) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x483 => 
                                                                                                             cvtFIELD_TYPE x483
                                                                                                      ) ls484)))
     | cvtTYPE_EXPR (AppType{base=x490, args=ls492}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x490), ("args", PrettyRep.List (List.map (fn x491 => 
                                                                                                     cvtTYPE_EXPR x491
                                                                                              ) ls492))]))
     | cvtTYPE_EXPR (NullableType{expr=x503, nullable=b504}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x503), ("nullable", PrettyRep.Bool b504)]))
     | cvtTYPE_EXPR (InstanceType{name=x512, typeParams=ls514, ty=x518}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x512), 
          ("typeParams", PrettyRep.List (List.map (fn x513 => cvtIDENT x513
                                                  ) ls514)), ("ty", cvtTYPE_EXPR x518)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x529) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x529))
     | cvtSTMT (InitStmt{kind=x532, ns=x533, prototype=b534, static=b535, inits=ls537}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x532), 
          ("ns", cvtEXPR x533), ("prototype", PrettyRep.Bool b534), ("static", 
          PrettyRep.Bool b535), ("inits", PrettyRep.List (List.map (fn x536 => 
                                                                          cvtEXPR x536
                                                                   ) ls537))]))
     | cvtSTMT (ClassBlock{ns=x554, ident=x555, name=opt557, extends=opt562, 
          fixtures=opt567, block=x571}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x554), ("ident", cvtIDENT x555), ("name", 
       (case opt557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x556 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x556))
       )), ("extends", 
       (case opt562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x561 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x561))
       )), ("fixtures", 
       (case opt567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x566 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x566))
       )), ("block", cvtBLOCK x571)]))
     | cvtSTMT (PackageBlock{name=x587, block=x588}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x587), ("block", cvtBLOCK x588)]))
     | cvtSTMT (ForEachStmt x596) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x596))
     | cvtSTMT (ForInStmt x599) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x599))
     | cvtSTMT (ThrowStmt x602) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x602))
     | cvtSTMT (ReturnStmt x605) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x605))
     | cvtSTMT (BreakStmt opt609) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x608 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x608))
       ))
     | cvtSTMT (ContinueStmt opt616) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt616 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x615 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x615))
       ))
     | cvtSTMT (BlockStmt x622) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x622))
     | cvtSTMT (LabeledStmt(x625, x626)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x625, 
          cvtSTMT x626]))
     | cvtSTMT (LetStmt(ls631, x635)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x630 => 
                                                                                                                          cvtVAR_BINDING x630
                                                                                                                   ) ls631), 
          cvtSTMT x635]))
     | cvtSTMT (SuperStmt x639) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x639))
     | cvtSTMT (WhileStmt x642) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x642))
     | cvtSTMT (DoWhileStmt x645) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x645))
     | cvtSTMT (ForStmt{defns=ls649, fixtures=opt654, init=x658, cond=x659, 
          update=x660, contLabel=opt662, body=x666}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x648 => 
                                                                         cvtVAR_BINDING x648
                                                                  ) ls649)), 
          ("fixtures", 
       (case opt654 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x653 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x653))
       )), ("init", cvtEXPR x658), ("cond", cvtEXPR x659), ("update", cvtEXPR x660), 
          ("contLabel", 
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
     | cvtSTMT (TryStmt{body=x708, catches=ls723, finally=opt728}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x708), ("catches", PrettyRep.List (List.map (fn {bind=x709, 
                                                                                                    fixtures=opt711, 
                                                                                                    body=x715} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x709), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt711 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x710 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x710))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x715)]
                                                                                             ) ls723)), 
          ("finally", 
       (case opt728 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x727 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x727))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x741, cases=ls743}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x741), ("cases", PrettyRep.List (List.map (fn x742 => 
                                                                                                 cvtCASE x742
                                                                                          ) ls743))]))
     | cvtSTMT (SwitchTypeStmt{cond=x754, ty=x755, cases=ls757}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x754), ("ty", cvtTYPE_EXPR x755), 
          ("cases", PrettyRep.List (List.map (fn x756 => cvtTYPE_CASE x756
                                             ) ls757))]))
     | cvtSTMT (Dxns{expr=x770}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x770)]))
   and cvtEXPR (TrinaryExpr(x776, x777, x778, x779)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x776, cvtEXPR x777, cvtEXPR x778, 
          cvtEXPR x779]))
     | cvtEXPR (BinaryExpr(x783, x784, x785)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x783, cvtEXPR x784, cvtEXPR x785]))
     | cvtEXPR (BinaryTypeExpr(x789, x790, x791)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x789, cvtEXPR x790, cvtTYPE_EXPR x791]))
     | cvtEXPR (UnaryExpr(x795, x796)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x795, 
          cvtEXPR x796]))
     | cvtEXPR (TypeExpr x800) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x800))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt805) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x804))
       ))
     | cvtEXPR (SuperExpr opt812) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt812 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x811 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x811))
       ))
     | cvtEXPR (LiteralExpr x818) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x818))
     | cvtEXPR (CallExpr{func=x821, actuals=ls823}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x821), ("actuals", PrettyRep.List (List.map (fn x822 => 
                                                                                                   cvtEXPR x822
                                                                                            ) ls823))]))
     | cvtEXPR (ApplyTypeExpr{expr=x834, actuals=ls836}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x834), ("actuals", PrettyRep.List (List.map (fn x835 => 
                                                                                                   cvtTYPE_EXPR x835
                                                                                            ) ls836))]))
     | cvtEXPR (LetExpr{defs=ls848, body=x852, fixtures=opt854}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x847 => 
                                                                        cvtVAR_BINDING x847
                                                                 ) ls848)), 
          ("body", cvtEXPR x852), ("fixtures", 
       (case opt854 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x853 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x853))
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
          cvtPATTERN x896, cvtEXPR x897]))
     | cvtEXPR (ListExpr ls902) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x901 => 
                                                                                                    cvtEXPR x901
                                                                                             ) ls902)))
     | cvtEXPR (SliceExpr(x908, x909, x910)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x908, cvtEXPR x909, cvtEXPR x910]))
     | cvtEXPR (DefTemp(n914, x915)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n914, 
          cvtEXPR x915]))
     | cvtEXPR (GetTemp n919) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n919))
   and cvtFIXTURE_NAME (TempName n922) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n922))
     | cvtFIXTURE_NAME (PropName x925) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x925))
   and cvtSTATIC_IDENT_EXPR (StaticQualifiedIdentifier{qual=x928, ident=x929}) = 
          PrettyRep.Ctor ("StaticQualifiedIdentifier", SOME (PrettyRep.Rec [("qual", 
          cvtEXPR x928), ("ident", cvtIDENT x929)]))
     | cvtSTATIC_IDENT_EXPR (StaticIdentifier{ident=x937, openNamespaces=ls943}) = 
          PrettyRep.Ctor ("StaticIdentifier", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT x937), ("openNamespaces", PrettyRep.List (List.map (fn ls939 => 
                                                                             PrettyRep.List (List.map (fn x938 => 
                                                                                                             cvtNAMESPACE x938
                                                                                                      ) ls939)
                                                                      ) ls943))]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x954, ident=x955}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x954), ("ident", cvtUSTRING x955)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x963, expr=x964}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x963), ("expr", cvtEXPR x964)]))
     | cvtIDENT_EXPR (AttributeIdentifier x972) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x972))
     | cvtIDENT_EXPR (Identifier{ident=x975, openNamespaces=ls981}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x975), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls977 => PrettyRep.List (List.map (fn x976 => 
                                                                                cvtNAMESPACE x976
                                                                         ) ls977)
                                   ) ls981))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x992) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x992))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x995, typeParams=ls997}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x995), ("typeParams", 
          PrettyRep.List (List.map (fn x996 => cvtTYPE_EXPR x996
                                   ) ls997))]))
     | cvtIDENT_EXPR (StaticIdentExpr x1008) = PrettyRep.Ctor ("StaticIdentExpr", 
          SOME (cvtSTATIC_IDENT_EXPR x1008))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r1013) = PrettyRep.Ctor ("LiteralNumber", 
          SOME (PrettyRep.Real r1013))
     | cvtLITERAL (LiteralBoolean b1016) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1016))
     | cvtLITERAL (LiteralString x1019) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1019))
     | cvtLITERAL (LiteralArray{exprs=ls1023, ty=opt1028}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1022 => 
                                                                         cvtEXPR x1022
                                                                  ) ls1023)), 
          ("ty", 
       (case opt1028 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1027 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1027))
       ))]))
     | cvtLITERAL (LiteralXML ls1040) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1039 => 
                                                                                                            cvtEXPR x1039
                                                                                                     ) ls1040)))
     | cvtLITERAL (LiteralNamespace x1046) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1046))
     | cvtLITERAL (LiteralObject{expr=ls1050, ty=opt1055}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1049 => 
                                                                        cvtFIELD x1049
                                                                 ) ls1050)), 
          ("ty", 
       (case opt1055 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1054 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1054))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1066, ty=x1067}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1066), ("ty", cvtTYPE_EXPR x1067)]))
     | cvtLITERAL (LiteralRegExp{str=x1075}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1075)]))
   and cvtBLOCK (Block x1081) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1081))
   and cvtPATTERN (ObjectPattern ls1085) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1084 => cvtFIELD_PATTERN x1084
                                         ) ls1085)))
     | cvtPATTERN (ArrayPattern ls1092) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1091 => 
                                                                                                                cvtPATTERN x1091
                                                                                                         ) ls1092)))
     | cvtPATTERN (SimplePattern x1098) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1098))
     | cvtPATTERN (IdentifierPattern x1101) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1101))
   and cvtFIXTURE (NamespaceFixture x1104) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1104))
     | cvtFIXTURE (ClassFixture x1107) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1107))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1111) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1111))
     | cvtFIXTURE (ValFixture{ty=x1114, readOnly=b1115, isOverride=b1116, isFinal=b1117}) = 
          PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1114), 
          ("readOnly", PrettyRep.Bool b1115), ("isOverride", PrettyRep.Bool b1116), 
          ("isFinal", PrettyRep.Bool b1117)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1129, getter=opt1131, setter=opt1136}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1129), ("getter", 
       (case opt1131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1130 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1130))
       )), ("setter", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1135))
       ))]))
   and cvtFIELD {kind=x1149, name=x1150, init=x1151} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1149), ("name", cvtIDENT_EXPR x1150), ("init", cvtEXPR x1151)]
   and cvtFIELD_PATTERN {name=x1159, ptrn=x1160} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1159), ("ptrn", cvtPATTERN x1160)]
   and cvtFIELD_TYPE {name=x1166, ty=x1167} = PrettyRep.Rec [("name", cvtIDENT x1166), 
          ("ty", cvtTYPE_EXPR x1167)]
   and cvtTYPED_IDENT {name=x1173, ty=opt1175} = PrettyRep.Rec [("name", cvtIDENT x1173), 
          ("ty", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1174))
       ))]
   and cvtFUNC_DEFN {kind=x1184, ns=x1185, final=b1186, native=b1187, override=b1188, 
          prototype=b1189, static=b1190, func=x1191} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1184), ("ns", cvtEXPR x1185), ("final", PrettyRep.Bool b1186), 
          ("native", PrettyRep.Bool b1187), ("override", PrettyRep.Bool b1188), 
          ("prototype", PrettyRep.Bool b1189), ("static", PrettyRep.Bool b1190), 
          ("func", cvtFUNC x1191)]
   and cvtCTOR_DEFN {ns=x1209, native=b1210, ctor=x1211} = PrettyRep.Rec [("ns", 
          cvtEXPR x1209), ("native", PrettyRep.Bool b1210), ("ctor", cvtCTOR x1211)]
   and cvtVAR_DEFN {kind=x1219, ns=x1220, static=b1221, prototype=b1222, bindings=ls1224} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1219), ("ns", cvtEXPR x1220), 
          ("static", PrettyRep.Bool b1221), ("prototype", PrettyRep.Bool b1222), 
          ("bindings", PrettyRep.List (List.map (fn x1223 => cvtVAR_BINDING x1223
                                                ) ls1224))]
   and cvtFIXTURES ls1242 = PrettyRep.List (List.map (fn (x1239, x1240) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1239, 
                                                            cvtFIXTURE x1240]
                                                     ) ls1242)
   and cvtINITS ls1249 = PrettyRep.List (List.map (fn (x1246, x1247) => PrettyRep.Tuple [cvtFIXTURE_NAME x1246, 
                                                         cvtEXPR x1247]
                                                  ) ls1249)
   and cvtNAMESPACE_DEFN {ident=x1253, ns=x1254, init=opt1256} = PrettyRep.Rec [("ident", 
          cvtIDENT x1253), ("ns", cvtEXPR x1254), ("init", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1255))
       ))]
   and cvtCLASS_DEFN {ident=x1267, ns=x1268, nonnullable=b1269, dynamic=b1270, 
          final=b1271, params=ls1273, extends=opt1278, implements=ls1283, body=x1287} = 
          PrettyRep.Rec [("ident", cvtIDENT x1267), ("ns", cvtEXPR x1268), 
          ("nonnullable", PrettyRep.Bool b1269), ("dynamic", PrettyRep.Bool b1270), 
          ("final", PrettyRep.Bool b1271), ("params", PrettyRep.List (List.map (fn x1272 => 
                                                                                      cvtIDENT x1272
                                                                               ) ls1273)), 
          ("extends", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1277))
       )), ("implements", PrettyRep.List (List.map (fn x1282 => cvtIDENT_EXPR x1282
                                                   ) ls1283)), ("body", cvtBLOCK x1287)]
   and cvtINTERFACE_DEFN {ident=x1307, ns=x1308, nonnullable=b1309, params=ls1311, 
          extends=ls1316, body=x1320} = PrettyRep.Rec [("ident", cvtIDENT x1307), 
          ("ns", cvtEXPR x1308), ("nonnullable", PrettyRep.Bool b1309), ("params", 
          PrettyRep.List (List.map (fn x1310 => cvtIDENT x1310
                                   ) ls1311)), ("extends", PrettyRep.List (List.map (fn x1315 => 
                                                                                           cvtIDENT_EXPR x1315
                                                                                    ) ls1316)), 
          ("body", cvtBLOCK x1320)]
   and cvtTYPE_DEFN {ident=x1334, ns=x1335, init=x1336} = PrettyRep.Rec [("ident", 
          cvtIDENT x1334), ("ns", cvtEXPR x1335), ("init", cvtTYPE_EXPR x1336)]
   and cvtFOR_ENUM_STMT {ptrn=opt1345, obj=x1349, defns=ls1351, fixtures=opt1356, 
          contLabel=opt1361, body=x1365} = PrettyRep.Rec [("ptrn", 
       (case opt1345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1344 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1344))
       )), ("obj", cvtEXPR x1349), ("defns", PrettyRep.List (List.map (fn x1350 => 
                                                                             cvtVAR_BINDING x1350
                                                                      ) ls1351)), 
          ("fixtures", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1355))
       )), ("contLabel", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1360))
       )), ("body", cvtSTMT x1365)]
   and cvtWHILE_STMT {cond=x1379, body=x1380, contLabel=opt1382} = PrettyRep.Rec [("cond", 
          cvtEXPR x1379), ("body", cvtSTMT x1380), ("contLabel", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1381))
       ))]
   and cvtDIRECTIVES {pragmas=ls1394, defns=ls1399, stmts=ls1404, fixtures=opt1409, 
          inits=opt1414} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1393 => 
                                                                                      cvtPRAGMA x1393
                                                                               ) ls1394)), 
          ("defns", PrettyRep.List (List.map (fn x1398 => cvtDEFN x1398
                                             ) ls1399)), ("stmts", PrettyRep.List (List.map (fn x1403 => 
                                                                                                   cvtSTMT x1403
                                                                                            ) ls1404)), 
          ("fixtures", 
       (case opt1409 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1408 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1408))
       )), ("inits", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1413))
       ))]
   and cvtBINDINGS {b=ls1430, i=ls1435} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1429 => 
                                                                                               cvtVAR_BINDING x1429
                                                                                        ) ls1430)), 
          ("i", PrettyRep.List (List.map (fn x1434 => cvtEXPR x1434
                                         ) ls1435))]
   and cvtCASE {label=opt1445, body=x1449} = PrettyRep.Rec [("label", 
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1444))
       )), ("body", cvtBLOCK x1449)]
   and cvtTYPE_CASE {ptrn=opt1456, body=x1460} = PrettyRep.Rec [("ptrn", 
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1455 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1455))
       )), ("body", cvtBLOCK x1460)]
   and cvtFUNC_NAME {kind=x1466, ident=x1467} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1466), 
          ("ident", cvtIDENT x1467)]
   and cvtPACKAGE {name=x1473, body=x1474} = PrettyRep.Rec [("name", cvtUSTRING x1473), 
          ("body", cvtBLOCK x1474)]
   and cvtPROGRAM {packages=ls1481, fixtures=opt1486, body=x1490} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1480 => cvtPACKAGE x1480
                                   ) ls1481)), ("fixtures", 
       (case opt1486 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1485 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1485))
       )), ("body", cvtBLOCK x1490)]
end

