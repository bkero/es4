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
   and cvtFUNC (Func{name=x326, fsig=x327, block=x328, param=x329, defaults=ls331, 
          ty=opt336}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x326), ("fsig", cvtFUNC_SIG x327), ("block", cvtBLOCK x328), 
          ("param", cvtHEAD x329), ("defaults", PrettyRep.List (List.map (fn x330 => 
                                                                                cvtEXPR x330
                                                                         ) ls331)), 
          ("ty", 
       (case opt336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x335 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_TYPE x335))
       ))]))
   and cvtDEFN (ClassDefn x355) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x355))
     | cvtDEFN (VariableDefn x358) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x358))
     | cvtDEFN (FunctionDefn x361) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x361))
     | cvtDEFN (ConstructorDefn x364) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x364))
     | cvtDEFN (InterfaceDefn x367) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x367))
     | cvtDEFN (NamespaceDefn x370) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x370))
     | cvtDEFN (TypeDefn x373) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x373))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls377, params=x381, settings=opt383, 
          returnType=x387, thisType=opt389, hasRest=b393}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x376 => 
                                                                              cvtIDENT x376
                                                                       ) ls377)), 
          ("params", cvtBINDINGS x381), ("settings", 
       (case opt383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x382 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x382))
       )), ("returnType", cvtTYPE_EXPR x387), ("thisType", 
       (case opt389 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x388 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x388))
       )), ("hasRest", PrettyRep.Bool b393)]))
   and cvtBINDING (Binding{ident=x409, ty=opt411}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x409), ("ty", 
       (case opt411 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x410 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x410))
       ))]))
   and cvtBINDING_IDENT (TempIdent n422) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n422))
     | cvtBINDING_IDENT (PropIdent x425) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x425))
   and cvtINIT_STEP (InitStep(x428, x429)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x428, 
          cvtEXPR x429]))
     | cvtINIT_STEP (AssignStep(x433, x434)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x433, cvtEXPR x434]))
   and cvtTYPE_EXPR (SpecialType x438) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x438))
     | cvtTYPE_EXPR (UnionType ls442) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x441 => 
                                                                                                           cvtTYPE_EXPR x441
                                                                                                    ) ls442)))
     | cvtTYPE_EXPR (ArrayType ls449) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x448 => 
                                                                                                           cvtTYPE_EXPR x448
                                                                                                    ) ls449)))
     | cvtTYPE_EXPR (TypeName x455) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x455))
     | cvtTYPE_EXPR (TypeRef(x458, x459)) = PrettyRep.Ctor ("TypeRef", SOME (PrettyRep.Tuple [cvtTYPE_EXPR x458, 
          cvtIDENT x459]))
     | cvtTYPE_EXPR (FunctionType x463) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x463))
     | cvtTYPE_EXPR (ObjectType ls467) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x466 => 
                                                                                                             cvtFIELD_TYPE x466
                                                                                                      ) ls467)))
     | cvtTYPE_EXPR (AppType{base=x473, args=ls475}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x473), ("args", PrettyRep.List (List.map (fn x474 => 
                                                                                                     cvtTYPE_EXPR x474
                                                                                              ) ls475))]))
     | cvtTYPE_EXPR (NullableType{expr=x486, nullable=b487}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x486), ("nullable", PrettyRep.Bool b487)]))
     | cvtTYPE_EXPR (InstanceType{name=x495, typeParams=ls497, ty=x501}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x495), 
          ("typeParams", PrettyRep.List (List.map (fn x496 => cvtIDENT x496
                                                  ) ls497)), ("ty", cvtTYPE_EXPR x501)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x512) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x512))
     | cvtSTMT (InitStmt{kind=x515, ns=x516, prototype=b517, static=b518, inits=ls520}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x515), 
          ("ns", cvtEXPR x516), ("prototype", PrettyRep.Bool b517), ("static", 
          PrettyRep.Bool b518), ("inits", PrettyRep.List (List.map (fn x519 => 
                                                                          cvtINIT_STEP x519
                                                                   ) ls520))]))
     | cvtSTMT (ClassBlock{ns=x537, ident=x538, name=opt540, extends=opt545, 
          fixtures=opt550, block=x554}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x537), ("ident", cvtIDENT x538), ("name", 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x539 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x539))
       )), ("extends", 
       (case opt545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x544 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x544))
       )), ("fixtures", 
       (case opt550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x549 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x549))
       )), ("block", cvtBLOCK x554)]))
     | cvtSTMT (PackageBlock{name=x570, block=x571}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x570), ("block", cvtBLOCK x571)]))
     | cvtSTMT (ForEachStmt x579) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x579))
     | cvtSTMT (ForInStmt x582) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x582))
     | cvtSTMT (ThrowStmt x585) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x585))
     | cvtSTMT (ReturnStmt x588) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x588))
     | cvtSTMT (BreakStmt opt592) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x591 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x591))
       ))
     | cvtSTMT (ContinueStmt opt599) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x598 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x598))
       ))
     | cvtSTMT (BlockStmt x605) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x605))
     | cvtSTMT (LabeledStmt(x608, x609)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x608, 
          cvtSTMT x609]))
     | cvtSTMT (LetStmt x613) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x613))
     | cvtSTMT (SuperStmt x616) = PrettyRep.Ctor ("SuperStmt", SOME (cvtEXPR x616))
     | cvtSTMT (WhileStmt x619) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x619))
     | cvtSTMT (DoWhileStmt x622) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x622))
     | cvtSTMT (ForStmt{fixtures=opt626, defn=opt631, init=ls636, cond=x640, 
          update=x641, contLabel=opt643, body=x647}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("fixtures", 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x625))
       )), ("defn", 
       (case opt631 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x630 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x630))
       )), ("init", PrettyRep.List (List.map (fn x635 => cvtSTMT x635
                                             ) ls636)), ("cond", cvtEXPR x640), 
          ("update", cvtEXPR x641), ("contLabel", 
       (case opt643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x642 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x642))
       )), ("body", cvtSTMT x647)]))
     | cvtSTMT (IfStmt{cnd=x665, thn=x666, els=x667}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x665), ("thn", cvtSTMT x666), 
          ("els", cvtSTMT x667)]))
     | cvtSTMT (WithStmt{obj=x677, ty=x678, body=x679}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x677), ("ty", cvtTYPE_EXPR x678), 
          ("body", cvtSTMT x679)]))
     | cvtSTMT (TryStmt{block=x689, catches=ls711, finally=opt716}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x689), ("catches", PrettyRep.List (List.map (fn {bindings=x690, 
                                                                                                     ty=opt692, 
                                                                                                     fixtures=opt697, 
                                                                                                     block=x701} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x690), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt692 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x691 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x691))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt697 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x696 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x696))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x701)]
                                                                                              ) ls711)), 
          ("finally", 
       (case opt716 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x715 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x715))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x729, cases=ls731}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x729), ("cases", PrettyRep.List (List.map (fn x730 => 
                                                                                                 cvtCASE x730
                                                                                          ) ls731))]))
     | cvtSTMT (SwitchTypeStmt{cond=x742, ty=x743, cases=ls745}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x742), ("ty", cvtTYPE_EXPR x743), 
          ("cases", PrettyRep.List (List.map (fn x744 => cvtTYPE_CASE x744
                                             ) ls745))]))
     | cvtSTMT (Dxns{expr=x758}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x758)]))
   and cvtEXPR (TrinaryExpr(x764, x765, x766, x767)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x764, cvtEXPR x765, cvtEXPR x766, 
          cvtEXPR x767]))
     | cvtEXPR (BinaryExpr(x771, x772, x773)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x771, cvtEXPR x772, cvtEXPR x773]))
     | cvtEXPR (BinaryTypeExpr(x777, x778, x779)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x777, cvtEXPR x778, cvtTYPE_EXPR x779]))
     | cvtEXPR (UnaryExpr(x783, x784)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x783, 
          cvtEXPR x784]))
     | cvtEXPR (TypeExpr x788) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x788))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt793) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt793 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x792 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x792))
       ))
     | cvtEXPR (SuperExpr opt800) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt800 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x799 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x799))
       ))
     | cvtEXPR (LiteralExpr x806) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x806))
     | cvtEXPR (CallExpr{func=x809, actuals=ls811}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x809), ("actuals", PrettyRep.List (List.map (fn x810 => 
                                                                                                   cvtEXPR x810
                                                                                            ) ls811))]))
     | cvtEXPR (ApplyTypeExpr{expr=x822, actuals=ls824}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x822), ("actuals", PrettyRep.List (List.map (fn x823 => 
                                                                                                   cvtTYPE_EXPR x823
                                                                                            ) ls824))]))
     | cvtEXPR (LetExpr{defs=x835, body=x836, head=opt838}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x835), ("body", cvtEXPR x836), 
          ("head", 
       (case opt838 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x837 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x837))
       ))]))
     | cvtEXPR (NewExpr{obj=x851, actuals=ls853}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x851), ("actuals", PrettyRep.List (List.map (fn x852 => 
                                                                                                  cvtEXPR x852
                                                                                           ) ls853))]))
     | cvtEXPR (ObjectRef{base=x864, ident=x865}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x864), ("ident", cvtIDENT_EXPR x865)]))
     | cvtEXPR (LexicalRef{ident=x873}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x873)]))
     | cvtEXPR (SetExpr(x879, x880, x881)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x879, 
          cvtEXPR x880, cvtEXPR x881]))
     | cvtEXPR (ListExpr ls886) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x885 => 
                                                                                                    cvtEXPR x885
                                                                                             ) ls886)))
     | cvtEXPR (InitExpr x892) = PrettyRep.Ctor ("InitExpr", SOME (cvtINITS x892))
     | cvtEXPR (SliceExpr(x895, x896, x897)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x895, cvtEXPR x896, cvtEXPR x897]))
     | cvtEXPR (DefTemp(n901, x902)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n901, 
          cvtEXPR x902]))
     | cvtEXPR (GetTemp n906) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n906))
   and cvtFIXTURE_NAME (TempName n909) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n909))
     | cvtFIXTURE_NAME (PropName x912) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x912))
   and cvtIDENT_EXPR (Identifier{ident=x915, openNamespaces=ls921}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x915), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls917 => PrettyRep.List (List.map (fn x916 => 
                                                                                cvtNAMESPACE x916
                                                                         ) ls917)
                                   ) ls921))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x932, expr=x933}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x932), ("expr", cvtEXPR x933)]))
     | cvtIDENT_EXPR (AttributeIdentifier x941) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x941))
     | cvtIDENT_EXPR (ExpressionIdentifier x944) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x944))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x947, ident=x948}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x947), ("ident", cvtUSTRING x948)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x956, typeParams=ls958}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x956), ("typeParams", 
          PrettyRep.List (List.map (fn x957 => cvtTYPE_EXPR x957
                                   ) ls958))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r971) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r971))
     | cvtLITERAL (LiteralBoolean b974) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b974))
     | cvtLITERAL (LiteralString x977) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x977))
     | cvtLITERAL (LiteralArray{exprs=ls981, ty=opt986}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x980 => 
                                                                         cvtEXPR x980
                                                                  ) ls981)), 
          ("ty", 
       (case opt986 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x985 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x985))
       ))]))
     | cvtLITERAL (LiteralXML ls998) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x997 => 
                                                                                                           cvtEXPR x997
                                                                                                    ) ls998)))
     | cvtLITERAL (LiteralNamespace x1004) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1004))
     | cvtLITERAL (LiteralObject{expr=ls1008, ty=opt1013}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1007 => 
                                                                        cvtFIELD x1007
                                                                 ) ls1008)), 
          ("ty", 
       (case opt1013 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1012 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1012))
       ))]))
     | cvtLITERAL (LiteralFunction{func=x1024, ty=x1025}) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1024), ("ty", cvtTYPE_EXPR x1025)]))
     | cvtLITERAL (LiteralRegExp{str=x1033}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1033)]))
   and cvtBLOCK (Block x1039) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1039))
   and cvtFIXTURE (NamespaceFixture x1042) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1042))
     | cvtFIXTURE (ClassFixture x1045) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1045))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1049) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1049))
     | cvtFIXTURE (MethodFixture{ty=x1052, isOverride=b1053, isFinal=b1054}) = 
          PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1052), 
          ("isOverride", PrettyRep.Bool b1053), ("isFinal", PrettyRep.Bool b1054)]))
     | cvtFIXTURE (ValFixture{ty=x1064, readOnly=b1065}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1064), ("readOnly", PrettyRep.Bool b1065)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1073, getter=opt1075, setter=opt1080}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1073), ("getter", 
       (case opt1075 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1074 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1074))
       )), ("setter", 
       (case opt1080 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1079 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1079))
       ))]))
   and cvtBINDINGS (ls1094, ls1099) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1093 => 
                                                                                       cvtBINDING x1093
                                                                                ) ls1094), 
          PrettyRep.List (List.map (fn x1098 => cvtINIT_STEP x1098
                                   ) ls1099)]
   and cvtFIXTURES ls1107 = PrettyRep.List (List.map (fn (x1104, x1105) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1104, 
                                                            cvtFIXTURE x1105]
                                                     ) ls1107)
   and cvtINITS ls1114 = PrettyRep.List (List.map (fn (x1111, x1112) => PrettyRep.Tuple [cvtFIXTURE_NAME x1111, 
                                                         cvtEXPR x1112]
                                                  ) ls1114)
   and cvtHEAD (x1118, x1119) = PrettyRep.Tuple [cvtFIXTURES x1118, cvtINITS x1119]
   and cvtFIELD {kind=x1121, name=x1122, init=x1123} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1121), ("name", cvtIDENT_EXPR x1122), ("init", cvtEXPR x1123)]
   and cvtFIELD_TYPE {name=x1131, ty=x1132} = PrettyRep.Rec [("name", cvtIDENT x1131), 
          ("ty", cvtTYPE_EXPR x1132)]
   and cvtTYPED_IDENT {name=x1138, ty=opt1140} = PrettyRep.Rec [("name", cvtIDENT x1138), 
          ("ty", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1139))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1150, params=ls1155, result=x1159, thisType=opt1161, 
          hasRest=b1165, minArgs=n1166} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1149 => 
                                                                                                        cvtIDENT x1149
                                                                                                 ) ls1150)), 
          ("params", PrettyRep.List (List.map (fn x1154 => cvtTYPE_EXPR x1154
                                              ) ls1155)), ("result", cvtTYPE_EXPR x1159), 
          ("thisType", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       )), ("hasRest", PrettyRep.Bool b1165), ("minArgs", PrettyRep.Int n1166)]
   and cvtFUNC_DEFN {kind=x1180, ns=x1181, final=b1182, native=b1183, override=b1184, 
          prototype=b1185, static=b1186, func=x1187, ty=x1188} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1180), ("ns", cvtEXPR x1181), ("final", PrettyRep.Bool b1182), 
          ("native", PrettyRep.Bool b1183), ("override", PrettyRep.Bool b1184), 
          ("prototype", PrettyRep.Bool b1185), ("static", PrettyRep.Bool b1186), 
          ("func", cvtFUNC x1187), ("ty", cvtTYPE_EXPR x1188)]
   and cvtCTOR_DEFN {ns=x1208, native=b1209, ctor=x1210} = PrettyRep.Rec [("ns", 
          cvtEXPR x1208), ("native", PrettyRep.Bool b1209), ("ctor", cvtCTOR x1210)]
   and cvtVAR_DEFN {kind=x1218, ns=x1219, static=b1220, prototype=b1221, bindings=x1222} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1218), ("ns", cvtEXPR x1219), 
          ("static", PrettyRep.Bool b1220), ("prototype", PrettyRep.Bool b1221), 
          ("bindings", cvtBINDINGS x1222)]
   and cvtNAMESPACE_DEFN {ident=x1234, ns=x1235, init=opt1237} = PrettyRep.Rec [("ident", 
          cvtIDENT x1234), ("ns", cvtEXPR x1235), ("init", 
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1236))
       ))]
   and cvtCLASS_DEFN {ident=x1248, ns=x1249, nonnullable=b1250, dynamic=b1251, 
          final=b1252, params=ls1254, extends=opt1259, implements=ls1264, block=x1268, 
          classDefns=ls1270, instanceDefns=ls1275} = PrettyRep.Rec [("ident", 
          cvtIDENT x1248), ("ns", cvtEXPR x1249), ("nonnullable", PrettyRep.Bool b1250), 
          ("dynamic", PrettyRep.Bool b1251), ("final", PrettyRep.Bool b1252), 
          ("params", PrettyRep.List (List.map (fn x1253 => cvtIDENT x1253
                                              ) ls1254)), ("extends", 
       (case opt1259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1258 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1258))
       )), ("implements", PrettyRep.List (List.map (fn x1263 => cvtIDENT_EXPR x1263
                                                   ) ls1264)), ("block", cvtBLOCK x1268), 
          ("classDefns", PrettyRep.List (List.map (fn x1269 => cvtDEFN x1269
                                                  ) ls1270)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1274 => cvtDEFN x1274
                                   ) ls1275))]
   and cvtINTERFACE_DEFN {ident=x1302, ns=x1303, nonnullable=b1304, params=ls1306, 
          extends=ls1311, block=x1315} = PrettyRep.Rec [("ident", cvtIDENT x1302), 
          ("ns", cvtEXPR x1303), ("nonnullable", PrettyRep.Bool b1304), ("params", 
          PrettyRep.List (List.map (fn x1305 => cvtIDENT x1305
                                   ) ls1306)), ("extends", PrettyRep.List (List.map (fn x1310 => 
                                                                                           cvtIDENT_EXPR x1310
                                                                                    ) ls1311)), 
          ("block", cvtBLOCK x1315)]
   and cvtTYPE_DEFN {ident=x1329, ns=x1330, init=x1331} = PrettyRep.Rec [("ident", 
          cvtIDENT x1329), ("ns", cvtEXPR x1330), ("init", cvtTYPE_EXPR x1331)]
   and cvtFOR_ENUM_STMT {defn=opt1340, obj=x1344, fixtures=opt1346, inits=opt1351, 
          contLabel=opt1356, body=x1360} = PrettyRep.Rec [("defn", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1339))
       )), ("obj", cvtEXPR x1344), ("fixtures", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1345))
       )), ("inits", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1350))
       )), ("contLabel", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1355))
       )), ("body", cvtSTMT x1360)]
   and cvtWHILE_STMT {cond=x1374, fixtures=opt1376, body=x1380, contLabel=opt1382} = 
          PrettyRep.Rec [("cond", cvtEXPR x1374), ("fixtures", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1375))
       )), ("body", cvtSTMT x1380), ("contLabel", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1381))
       ))]
   and cvtDIRECTIVES {pragmas=ls1396, defns=ls1401, head=opt1406, body=ls1411} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1395 => 
                                                                     cvtPRAGMA x1395
                                                              ) ls1396)), ("defns", 
          PrettyRep.List (List.map (fn x1400 => cvtDEFN x1400
                                   ) ls1401)), ("head", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1405))
       )), ("body", PrettyRep.List (List.map (fn x1410 => cvtSTMT x1410
                                             ) ls1411))]
   and cvtCASE {label=opt1425, inits=opt1430, body=x1434} = PrettyRep.Rec [("label", 
          
       (case opt1425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1424 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1424))
       )), ("inits", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1429 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1429))
       )), ("body", cvtBLOCK x1434)]
   and cvtTYPE_CASE {ty=opt1443, bindings=x1447, inits=opt1449, body=x1453} = 
          PrettyRep.Rec [("ty", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1442 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1442))
       )), ("bindings", cvtBINDINGS x1447), ("inits", 
       (case opt1449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1448 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1448))
       )), ("body", cvtBLOCK x1453)]
   and cvtFUNC_NAME {kind=x1463, ident=x1464} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1463), 
          ("ident", cvtIDENT x1464)]
   and cvtPACKAGE {name=x1470, block=x1471} = PrettyRep.Rec [("name", cvtUSTRING x1470), 
          ("block", cvtBLOCK x1471)]
   and cvtPROGRAM {packages=ls1478, fixtures=opt1483, block=x1487} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1477 => cvtPACKAGE x1477
                                   ) ls1478)), ("fixtures", 
       (case opt1483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1482 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1482))
       )), ("block", cvtBLOCK x1487)]
end

