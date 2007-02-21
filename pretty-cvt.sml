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
     | cvtTYPE_EXPR (NominalType{ident=x447}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x447)]))
     | cvtTYPE_EXPR (FunctionType x453) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x453))
     | cvtTYPE_EXPR (ObjectType ls457) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x456 => 
                                                                                                             cvtFIELD_TYPE x456
                                                                                                      ) ls457)))
     | cvtTYPE_EXPR (AppType{base=x463, args=ls465}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x463), ("args", PrettyRep.List (List.map (fn x464 => 
                                                                                                     cvtTYPE_EXPR x464
                                                                                              ) ls465))]))
     | cvtTYPE_EXPR (NullableType{expr=x476, nullable=b477}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x476), ("nullable", PrettyRep.Bool b477)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls487) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x486 => 
                                                                                                    cvtEXPR x486
                                                                                             ) ls487)))
     | cvtSTMT (InitStmt{kind=x493, ns=x494, prototype=b495, static=b496, inits=ls498}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x493), 
          ("ns", cvtEXPR x494), ("prototype", PrettyRep.Bool b495), ("static", 
          PrettyRep.Bool b496), ("inits", PrettyRep.List (List.map (fn x497 => 
                                                                          cvtEXPR x497
                                                                   ) ls498))]))
     | cvtSTMT (ClassBlock{ns=x515, ident=x516, name=opt518, extends=opt523, 
          fixtures=opt528, block=x532}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x515), ("ident", cvtIDENT x516), ("name", 
       (case opt518 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x517 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x517))
       )), ("extends", 
       (case opt523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x522 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x522))
       )), ("fixtures", 
       (case opt528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x527 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x527))
       )), ("block", cvtBLOCK x532)]))
     | cvtSTMT (PackageBlock{name=x548, block=x549}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x548), ("block", cvtBLOCK x549)]))
     | cvtSTMT (ForEachStmt x557) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x557))
     | cvtSTMT (ForInStmt x560) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x560))
     | cvtSTMT (ThrowStmt ls564) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x563 => 
                                                                                                      cvtEXPR x563
                                                                                               ) ls564)))
     | cvtSTMT (ReturnStmt ls571) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x570 => 
                                                                                                        cvtEXPR x570
                                                                                                 ) ls571)))
     | cvtSTMT (BreakStmt opt578) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x577 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x577))
       ))
     | cvtSTMT (ContinueStmt opt585) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt585 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x584 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x584))
       ))
     | cvtSTMT (BlockStmt x591) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x591))
     | cvtSTMT (LabeledStmt(x594, x595)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x594, 
          cvtSTMT x595]))
     | cvtSTMT (LetStmt(ls600, x604)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x599 => 
                                                                                                                          cvtVAR_BINDING x599
                                                                                                                   ) ls600), 
          cvtSTMT x604]))
     | cvtSTMT (SuperStmt ls609) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x608 => 
                                                                                                      cvtEXPR x608
                                                                                               ) ls609)))
     | cvtSTMT (WhileStmt x615) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x615))
     | cvtSTMT (DoWhileStmt x618) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x618))
     | cvtSTMT (ForStmt{defns=ls622, fixtures=opt627, init=ls632, cond=ls637, 
          update=ls642, contLabel=opt647, body=x651}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x621 => 
                                                                         cvtVAR_BINDING x621
                                                                  ) ls622)), 
          ("fixtures", 
       (case opt627 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x626 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x626))
       )), ("init", PrettyRep.List (List.map (fn x631 => cvtEXPR x631
                                             ) ls632)), ("cond", PrettyRep.List (List.map (fn x636 => 
                                                                                                 cvtEXPR x636
                                                                                          ) ls637)), 
          ("update", PrettyRep.List (List.map (fn x641 => cvtEXPR x641
                                              ) ls642)), ("contLabel", 
       (case opt647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x646 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x646))
       )), ("body", cvtSTMT x651)]))
     | cvtSTMT (IfStmt{cnd=x669, thn=x670, els=x671}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x669), ("thn", cvtSTMT x670), 
          ("els", cvtSTMT x671)]))
     | cvtSTMT (WithStmt{obj=ls682, ty=x686, body=x687}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x681 => 
                                                                       cvtEXPR x681
                                                                ) ls682)), 
          ("ty", cvtTYPE_EXPR x686), ("body", cvtSTMT x687)]))
     | cvtSTMT (TryStmt{body=x697, catches=ls712, finally=opt717}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x697), ("catches", PrettyRep.List (List.map (fn {bind=x698, 
                                                                                                    fixtures=opt700, 
                                                                                                    body=x704} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x698), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt700 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x699 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x699))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x704)]
                                                                                             ) ls712)), 
          ("finally", 
       (case opt717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x716 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x716))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls731, cases=ls736}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x730 => 
                                                                        cvtEXPR x730
                                                                 ) ls731)), 
          ("cases", PrettyRep.List (List.map (fn x735 => cvtCASE x735
                                             ) ls736))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls748, ty=x752, cases=ls754}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x747 => 
                                                                        cvtEXPR x747
                                                                 ) ls748)), 
          ("ty", cvtTYPE_EXPR x752), ("cases", PrettyRep.List (List.map (fn x753 => 
                                                                               cvtTYPE_CASE x753
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
     | cvtEXPR (YieldExpr opt806) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls802 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x801 => 
                                                                                     cvtEXPR x801
                                                                              ) ls802)))
       ))
     | cvtEXPR (SuperExpr opt813) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt813 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x812 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x812))
       ))
     | cvtEXPR (LiteralExpr x819) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x819))
     | cvtEXPR (CallExpr{func=x822, actuals=ls824}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x822), ("actuals", PrettyRep.List (List.map (fn x823 => 
                                                                                                   cvtEXPR x823
                                                                                            ) ls824))]))
     | cvtEXPR (ApplyTypeExpr{expr=x835, actuals=ls837}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x835), ("actuals", PrettyRep.List (List.map (fn x836 => 
                                                                                                   cvtTYPE_EXPR x836
                                                                                            ) ls837))]))
     | cvtEXPR (LetExpr{defs=ls849, body=ls854, fixtures=opt859}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x848 => 
                                                                        cvtVAR_BINDING x848
                                                                 ) ls849)), 
          ("body", PrettyRep.List (List.map (fn x853 => cvtEXPR x853
                                            ) ls854)), ("fixtures", 
       (case opt859 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x858 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x858))
       ))]))
     | cvtEXPR (NewExpr{obj=x872, actuals=ls874}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x872), ("actuals", PrettyRep.List (List.map (fn x873 => 
                                                                                                  cvtEXPR x873
                                                                                           ) ls874))]))
     | cvtEXPR (FunExpr x885) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x885))
     | cvtEXPR (ObjectRef{base=x888, ident=x889}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x888), ("ident", cvtIDENT_EXPR x889)]))
     | cvtEXPR (LexicalRef{ident=x897}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x897)]))
     | cvtEXPR (SetExpr(x903, x904, x905)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x903, 
          cvtPATTERN x904, cvtEXPR x905]))
     | cvtEXPR (AllocTemp(n909, x910)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n909, 
          cvtEXPR x910]))
     | cvtEXPR (KillTemp n914) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n914))
     | cvtEXPR (GetTemp n917) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n917))
     | cvtEXPR (ListExpr ls921) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x920 => 
                                                                                                    cvtEXPR x920
                                                                                             ) ls921)))
     | cvtEXPR (SliceExpr(ls928, ls933, ls938)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x927 => cvtEXPR x927
                                                          ) ls928), PrettyRep.List (List.map (fn x932 => 
                                                                                                    cvtEXPR x932
                                                                                             ) ls933), 
          PrettyRep.List (List.map (fn x937 => cvtEXPR x937
                                   ) ls938)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x945, ident=x946}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x945), ("ident", cvtUSTRING x946)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x954, expr=x955}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x954), ("expr", cvtEXPR x955)]))
     | cvtIDENT_EXPR (AttributeIdentifier x963) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x963))
     | cvtIDENT_EXPR (Identifier{ident=x966, openNamespaces=ls972}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x966), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls968 => PrettyRep.List (List.map (fn x967 => 
                                                                                cvtNAMESPACE x967
                                                                         ) ls968)
                                   ) ls972))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x983) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x983))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x986, typeParams=ls988}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x986), ("typeParams", 
          PrettyRep.List (List.map (fn x987 => cvtTYPE_EXPR x987
                                   ) ls988))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r1001) = PrettyRep.Ctor ("LiteralNumber", 
          SOME (PrettyRep.Real r1001))
     | cvtLITERAL (LiteralBoolean b1004) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1004))
     | cvtLITERAL (LiteralString x1007) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1007))
     | cvtLITERAL (LiteralArray{exprs=ls1011, ty=opt1016}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1010 => 
                                                                         cvtEXPR x1010
                                                                  ) ls1011)), 
          ("ty", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1015))
       ))]))
     | cvtLITERAL (LiteralXML ls1028) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1027 => 
                                                                                                            cvtEXPR x1027
                                                                                                     ) ls1028)))
     | cvtLITERAL (LiteralNamespace x1034) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1034))
     | cvtLITERAL (LiteralObject{expr=ls1038, ty=opt1043}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1037 => 
                                                                        cvtFIELD x1037
                                                                 ) ls1038)), 
          ("ty", 
       (case opt1043 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1042 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1042))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x1054}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1054)]))
   and cvtBLOCK (Block x1060) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1060))
   and cvtPATTERN (ObjectPattern ls1064) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1063 => cvtFIELD_PATTERN x1063
                                         ) ls1064)))
     | cvtPATTERN (ArrayPattern ls1071) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1070 => 
                                                                                                                cvtPATTERN x1070
                                                                                                         ) ls1071)))
     | cvtPATTERN (SimplePattern x1077) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1077))
     | cvtPATTERN (IdentifierPattern x1080) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1080))
   and cvtFIXTURE (NamespaceFixture x1083) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1083))
     | cvtFIXTURE (ClassFixture x1086) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1086))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1090) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1090))
     | cvtFIXTURE (ValFixture{ty=x1093, readOnly=b1094, isOverride=b1095, isFinal=b1096, 
          init=opt1098}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1093), ("readOnly", PrettyRep.Bool b1094), ("isOverride", 
          PrettyRep.Bool b1095), ("isFinal", PrettyRep.Bool b1096), ("init", 
          
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1097))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1115, getter=opt1117, setter=opt1122}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1115), ("getter", 
       (case opt1117 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1116 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1116))
       )), ("setter", 
       (case opt1122 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1121 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1121))
       ))]))
   and cvtFIELD {kind=x1135, name=x1136, init=x1137} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1135), ("name", cvtIDENT_EXPR x1136), ("init", cvtEXPR x1137)]
   and cvtFIELD_PATTERN {name=x1145, ptrn=x1146} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1145), ("ptrn", cvtPATTERN x1146)]
   and cvtFIELD_TYPE {name=x1152, ty=x1153} = PrettyRep.Rec [("name", cvtIDENT x1152), 
          ("ty", cvtTYPE_EXPR x1153)]
   and cvtTYPED_IDENT {name=x1159, ty=opt1161} = PrettyRep.Rec [("name", cvtIDENT x1159), 
          ("ty", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       ))]
   and cvtATTRIBUTES {ns=x1170, override=b1171, static=b1172, final=b1173, 
          dynamic=b1174, prototype=b1175, native=b1176, rest=b1177} = PrettyRep.Rec [("ns", 
          cvtEXPR x1170), ("override", PrettyRep.Bool b1171), ("static", PrettyRep.Bool b1172), 
          ("final", PrettyRep.Bool b1173), ("dynamic", PrettyRep.Bool b1174), 
          ("prototype", PrettyRep.Bool b1175), ("native", PrettyRep.Bool b1176), 
          ("rest", PrettyRep.Bool b1177)]
   and cvtFUNC_DEFN {kind=x1195, ns=x1196, final=b1197, native=b1198, override=b1199, 
          prototype=b1200, static=b1201, func=x1202} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1195), ("ns", cvtEXPR x1196), ("final", PrettyRep.Bool b1197), 
          ("native", PrettyRep.Bool b1198), ("override", PrettyRep.Bool b1199), 
          ("prototype", PrettyRep.Bool b1200), ("static", PrettyRep.Bool b1201), 
          ("func", cvtFUNC x1202)]
   and cvtCTOR_DEFN {ns=x1220, native=b1221, ctor=x1222} = PrettyRep.Rec [("ns", 
          cvtEXPR x1220), ("native", PrettyRep.Bool b1221), ("ctor", cvtCTOR x1222)]
   and cvtVAR_DEFN {kind=x1230, ns=x1231, static=b1232, prototype=b1233, bindings=ls1235} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1230), ("ns", cvtEXPR x1231), 
          ("static", PrettyRep.Bool b1232), ("prototype", PrettyRep.Bool b1233), 
          ("bindings", PrettyRep.List (List.map (fn x1234 => cvtVAR_BINDING x1234
                                                ) ls1235))]
   and cvtFIXTURES ls1253 = PrettyRep.List (List.map (fn (x1250, x1251) => 
                                                            PrettyRep.Tuple [cvtNAME x1250, 
                                                            cvtFIXTURE x1251]
                                                     ) ls1253)
   and cvtINITS ls1260 = PrettyRep.List (List.map (fn (x1257, x1258) => PrettyRep.Tuple [cvtNAME x1257, 
                                                         cvtEXPR x1258]
                                                  ) ls1260)
   and cvtNAMESPACE_DEFN {ident=x1264, ns=x1265, init=opt1267} = PrettyRep.Rec [("ident", 
          cvtIDENT x1264), ("ns", cvtEXPR x1265), ("init", 
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1266))
       ))]
   and cvtCLASS_DEFN {ident=x1278, ns=x1279, nonnullable=b1280, dynamic=b1281, 
          final=b1282, params=ls1284, extends=opt1289, implements=ls1294, body=x1298} = 
          PrettyRep.Rec [("ident", cvtIDENT x1278), ("ns", cvtEXPR x1279), 
          ("nonnullable", PrettyRep.Bool b1280), ("dynamic", PrettyRep.Bool b1281), 
          ("final", PrettyRep.Bool b1282), ("params", PrettyRep.List (List.map (fn x1283 => 
                                                                                      cvtIDENT x1283
                                                                               ) ls1284)), 
          ("extends", 
       (case opt1289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1288 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1288))
       )), ("implements", PrettyRep.List (List.map (fn x1293 => cvtIDENT_EXPR x1293
                                                   ) ls1294)), ("body", cvtBLOCK x1298)]
   and cvtINTERFACE_DEFN {ident=x1318, ns=x1319, nonnullable=b1320, params=ls1322, 
          extends=ls1327, body=x1331} = PrettyRep.Rec [("ident", cvtIDENT x1318), 
          ("ns", cvtEXPR x1319), ("nonnullable", PrettyRep.Bool b1320), ("params", 
          PrettyRep.List (List.map (fn x1321 => cvtIDENT x1321
                                   ) ls1322)), ("extends", PrettyRep.List (List.map (fn x1326 => 
                                                                                           cvtIDENT_EXPR x1326
                                                                                    ) ls1327)), 
          ("body", cvtBLOCK x1331)]
   and cvtTYPE_DEFN {ident=x1345, ns=x1346, init=x1347} = PrettyRep.Rec [("ident", 
          cvtIDENT x1345), ("ns", cvtEXPR x1346), ("init", cvtTYPE_EXPR x1347)]
   and cvtFOR_ENUM_STMT {ptrn=opt1356, obj=ls1361, defns=ls1366, fixtures=opt1371, 
          contLabel=opt1376, body=x1380} = PrettyRep.Rec [("ptrn", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1355))
       )), ("obj", PrettyRep.List (List.map (fn x1360 => cvtEXPR x1360
                                            ) ls1361)), ("defns", PrettyRep.List (List.map (fn x1365 => 
                                                                                                  cvtVAR_BINDING x1365
                                                                                           ) ls1366)), 
          ("fixtures", 
       (case opt1371 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1370 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1370))
       )), ("contLabel", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1375))
       )), ("body", cvtSTMT x1380)]
   and cvtWHILE_STMT {cond=x1394, body=x1395, contLabel=opt1397} = PrettyRep.Rec [("cond", 
          cvtEXPR x1394), ("body", cvtSTMT x1395), ("contLabel", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1396))
       ))]
   and cvtDIRECTIVES {pragmas=ls1409, defns=ls1414, stmts=ls1419, fixtures=opt1424, 
          inits=opt1433} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1408 => 
                                                                                      cvtPRAGMA x1408
                                                                               ) ls1409)), 
          ("defns", PrettyRep.List (List.map (fn x1413 => cvtDEFN x1413
                                             ) ls1414)), ("stmts", PrettyRep.List (List.map (fn x1418 => 
                                                                                                   cvtSTMT x1418
                                                                                            ) ls1419)), 
          ("fixtures", 
       (case opt1424 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1423 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1423))
       )), ("inits", 
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1429 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1428 => 
                                                                                      cvtSTMT x1428
                                                                               ) ls1429)))
       ))]
   and cvtBINDINGS {b=ls1449, i=ls1454} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1448 => 
                                                                                               cvtVAR_BINDING x1448
                                                                                        ) ls1449)), 
          ("i", PrettyRep.List (List.map (fn x1453 => cvtEXPR x1453
                                         ) ls1454))]
   and cvtCASE {label=opt1468, body=x1472} = PrettyRep.Rec [("label", 
       (case opt1468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1464 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1463 => 
                                                                                      cvtEXPR x1463
                                                                               ) ls1464)))
       )), ("body", cvtBLOCK x1472)]
   and cvtTYPE_CASE {ptrn=opt1479, body=x1483} = PrettyRep.Rec [("ptrn", 
       (case opt1479 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1478 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1478))
       )), ("body", cvtBLOCK x1483)]
   and cvtFUNC_NAME {kind=x1489, ident=x1490} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1489), 
          ("ident", cvtIDENT x1490)]
   and cvtPACKAGE {name=x1496, body=x1497} = PrettyRep.Rec [("name", cvtUSTRING x1496), 
          ("body", cvtBLOCK x1497)]
   and cvtPROGRAM {packages=ls1504, fixtures=opt1509, body=x1513} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1503 => cvtPACKAGE x1503
                                   ) ls1504)), ("fixtures", 
       (case opt1509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1508 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1508))
       )), ("body", cvtBLOCK x1513)]
end

