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
   and cvtNUMERIC_MODE {numberType=x46, roundingMode=r47, precision=n48} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x46), ("roundingMode", 
          PrettyRep.DecRm r47), ("precision", PrettyRep.Int n48)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt61) = PrettyRep.Ctor ("Plus", SOME 
       (case opt61 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x60 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x60))
       ))
     | cvtBINOP (Minus opt68) = PrettyRep.Ctor ("Minus", SOME 
       (case opt68 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x67 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x67))
       ))
     | cvtBINOP (Times opt75) = PrettyRep.Ctor ("Times", SOME 
       (case opt75 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x74 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x74))
       ))
     | cvtBINOP (Divide opt82) = PrettyRep.Ctor ("Divide", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Remainder opt89) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
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
     | cvtBINOP (Equals opt106) = PrettyRep.Ctor ("Equals", SOME 
       (case opt106 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x105 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x105))
       ))
     | cvtBINOP (NotEquals opt113) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (StrictEquals opt120) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
       ))
     | cvtBINOP (StrictNotEquals opt127) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (Less opt134) = PrettyRep.Ctor ("Less", SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (LessOrEqual opt141) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (Greater opt148) = PrettyRep.Ctor ("Greater", SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (GreaterOrEqual opt155) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt164) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x163))
       ))
     | cvtASSIGNOP (AssignMinus opt171) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x170 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x170))
       ))
     | cvtASSIGNOP (AssignTimes opt178) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x177))
       ))
     | cvtASSIGNOP (AssignDivide opt185) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x184 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x184))
       ))
     | cvtASSIGNOP (AssignRemainder opt192) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x191 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x191))
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
     | cvtUNOP (PreIncrement opt210) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x209 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x209))
       ))
     | cvtUNOP (PreDecrement opt217) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt217 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x216 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x216))
       ))
     | cvtUNOP (PostIncrement opt224) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x223 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x223))
       ))
     | cvtUNOP (PostDecrement opt231) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x230 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x230))
       ))
     | cvtUNOP (UnaryPlus opt238) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x237 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x237))
       ))
     | cvtUNOP (UnaryMinus opt245) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x244 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x244))
       ))
     | cvtUNOP (BitwiseNot) = PrettyRep.Ctor ("BitwiseNot", NONE)
     | cvtUNOP (LogicalNot) = PrettyRep.Ctor ("LogicalNot", NONE)
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x262) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x262))
     | cvtPRAGMA (UseDefaultNamespace x265) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x265))
     | cvtPRAGMA (UseNumber x268) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x268))
     | cvtPRAGMA (UseRounding r271) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r271))
     | cvtPRAGMA (UsePrecision n274) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n274))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls280, name=x284, alias=opt286}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x279 => 
                                                                           cvtIDENT x279
                                                                    ) ls280)), 
          ("name", cvtIDENT x284), ("alias", 
       (case opt286 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x285 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x285))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtCLS (Cls{name=x306, extends=opt308, implements=ls313, classFixtures=x317, 
          instanceFixtures=x318, instanceInits=x319, constructor=opt321, classType=x325, 
          instanceType=x326}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x306), ("extends", 
       (case opt308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x307 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x307))
       )), ("implements", PrettyRep.List (List.map (fn x312 => cvtNAME x312
                                                   ) ls313)), ("classFixtures", 
          cvtFIXTURES x317), ("instanceFixtures", cvtFIXTURES x318), ("instanceInits", 
          cvtHEAD x319), ("constructor", 
       (case opt321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x320 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x320))
       )), ("classType", cvtTYPE_EXPR x325), ("instanceType", cvtTYPE_EXPR x326)]))
   and cvtCTOR (Ctor{settings=x348, superArgs=ls350, func=x354}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x348), ("superArgs", PrettyRep.List (List.map (fn x349 => 
                                                                                                         cvtEXPR x349
                                                                                                  ) ls350)), 
          ("func", cvtFUNC x354)]))
   and cvtFUNC (Func{name=x364, fsig=x365, isNative=b366, block=x367, param=x368, 
          defaults=ls370, ty=x374}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x364), ("fsig", cvtFUNC_SIG x365), ("isNative", PrettyRep.Bool b366), 
          ("block", cvtBLOCK x367), ("param", cvtHEAD x368), ("defaults", PrettyRep.List (List.map (fn x369 => 
                                                                                                          cvtEXPR x369
                                                                                                   ) ls370)), 
          ("ty", cvtFUNC_TYPE x374)]))
   and cvtDEFN (ClassDefn x392) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x392))
     | cvtDEFN (VariableDefn x395) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x395))
     | cvtDEFN (FunctionDefn x398) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x398))
     | cvtDEFN (ConstructorDefn x401) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x401))
     | cvtDEFN (InterfaceDefn x404) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x404))
     | cvtDEFN (NamespaceDefn x407) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x407))
     | cvtDEFN (TypeDefn x410) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x410))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls414, params=x418, defaults=ls420, 
          ctorInits=opt431, returnType=x435, thisType=opt437, hasRest=b441}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x413 => cvtIDENT x413
                                   ) ls414)), ("params", cvtBINDINGS x418), 
          ("defaults", PrettyRep.List (List.map (fn x419 => cvtEXPR x419
                                                ) ls420)), ("ctorInits", 
       (case opt431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x424, ls426) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x424, 
            PrettyRep.List (List.map (fn x425 => cvtEXPR x425
                                     ) ls426)]))
       )), ("returnType", cvtTYPE_EXPR x435), ("thisType", 
       (case opt437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x436 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x436))
       )), ("hasRest", PrettyRep.Bool b441)]))
   and cvtBINDING (Binding{ident=x459, ty=opt461}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x459), ("ty", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x460 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x460))
       ))]))
   and cvtBINDING_IDENT (TempIdent n472) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n472))
     | cvtBINDING_IDENT (PropIdent x475) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x475))
   and cvtINIT_STEP (InitStep(x478, x479)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x478, 
          cvtEXPR x479]))
     | cvtINIT_STEP (AssignStep(x483, x484)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x483, cvtEXPR x484]))
   and cvtTYPE_EXPR (SpecialType x488) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x488))
     | cvtTYPE_EXPR (UnionType ls492) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x491 => 
                                                                                                           cvtTYPE_EXPR x491
                                                                                                    ) ls492)))
     | cvtTYPE_EXPR (ArrayType ls499) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x498 => 
                                                                                                           cvtTYPE_EXPR x498
                                                                                                    ) ls499)))
     | cvtTYPE_EXPR (TypeName x505) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x505))
     | cvtTYPE_EXPR (ElementTypeRef(x508, n509)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x508, PrettyRep.Int n509]))
     | cvtTYPE_EXPR (FieldTypeRef(x513, x514)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x513, cvtIDENT x514]))
     | cvtTYPE_EXPR (FunctionType x518) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x518))
     | cvtTYPE_EXPR (ObjectType ls522) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x521 => 
                                                                                                             cvtFIELD_TYPE x521
                                                                                                      ) ls522)))
     | cvtTYPE_EXPR (AppType{base=x528, args=ls530}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x528), ("args", PrettyRep.List (List.map (fn x529 => 
                                                                                                     cvtTYPE_EXPR x529
                                                                                              ) ls530))]))
     | cvtTYPE_EXPR (NullableType{expr=x541, nullable=b542}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x541), ("nullable", PrettyRep.Bool b542)]))
     | cvtTYPE_EXPR (InstanceType{name=x550, typeParams=ls552, ty=x556, isDynamic=b557}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x550), 
          ("typeParams", PrettyRep.List (List.map (fn x551 => cvtIDENT x551
                                                  ) ls552)), ("ty", cvtTYPE_EXPR x556), 
          ("isDynamic", PrettyRep.Bool b557)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x570) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x570))
     | cvtSTMT (InitStmt{kind=x573, ns=opt575, prototype=b579, static=b580, 
          temps=x581, inits=ls583}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x573), ("ns", 
       (case opt575 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x574 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x574))
       )), ("prototype", PrettyRep.Bool b579), ("static", PrettyRep.Bool b580), 
          ("temps", cvtBINDINGS x581), ("inits", PrettyRep.List (List.map (fn x582 => 
                                                                                 cvtINIT_STEP x582
                                                                          ) ls583))]))
     | cvtSTMT (ClassBlock{ns=opt603, ident=x607, name=opt609, block=x613}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x602 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x602))
       )), ("ident", cvtIDENT x607), ("name", 
       (case opt609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x608 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x608))
       )), ("block", cvtBLOCK x613)]))
     | cvtSTMT (ForEachStmt x625) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x625))
     | cvtSTMT (ForInStmt x628) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x628))
     | cvtSTMT (ThrowStmt x631) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x631))
     | cvtSTMT (ReturnStmt x634) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x634))
     | cvtSTMT (BreakStmt opt638) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt638 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x637 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x637))
       ))
     | cvtSTMT (ContinueStmt opt645) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x644 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x644))
       ))
     | cvtSTMT (BlockStmt x651) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x651))
     | cvtSTMT (LabeledStmt(x654, x655)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x654, 
          cvtSTMT x655]))
     | cvtSTMT (LetStmt x659) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x659))
     | cvtSTMT (WhileStmt x662) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x662))
     | cvtSTMT (DoWhileStmt x665) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x665))
     | cvtSTMT (ForStmt x668) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x668))
     | cvtSTMT (IfStmt{cnd=x671, thn=x672, els=x673}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x671), ("thn", cvtSTMT x672), 
          ("els", cvtSTMT x673)]))
     | cvtSTMT (WithStmt{obj=x683, ty=x684, body=x685}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x683), ("ty", cvtTYPE_EXPR x684), 
          ("body", cvtSTMT x685)]))
     | cvtSTMT (TryStmt{block=x695, catches=ls717, finally=opt722}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x695), ("catches", PrettyRep.List (List.map (fn {bindings=x696, 
                                                                                                     ty=opt698, 
                                                                                                     fixtures=opt703, 
                                                                                                     block=x707} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x696), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt698 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x697 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x697))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt703 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x702 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x702))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x707)]
                                                                                              ) ls717)), 
          ("finally", 
       (case opt722 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x721 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x721))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt736, cond=x740, cases=ls742}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x735))
       )), ("cond", cvtEXPR x740), ("cases", PrettyRep.List (List.map (fn x741 => 
                                                                             cvtCASE x741
                                                                      ) ls742))]))
     | cvtSTMT (SwitchTypeStmt{cond=x755, ty=x756, cases=ls758}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x755), ("ty", cvtTYPE_EXPR x756), 
          ("cases", PrettyRep.List (List.map (fn x757 => cvtTYPE_CASE x757
                                             ) ls758))]))
     | cvtSTMT (Dxns{expr=x771}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x771)]))
   and cvtEXPR (TrinaryExpr(x777, x778, x779, x780)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x777, cvtEXPR x778, cvtEXPR x779, 
          cvtEXPR x780]))
     | cvtEXPR (BinaryExpr(x784, x785, x786)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x784, cvtEXPR x785, cvtEXPR x786]))
     | cvtEXPR (BinaryTypeExpr(x790, x791, x792)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x790, cvtEXPR x791, cvtTYPE_EXPR x792]))
     | cvtEXPR (UnaryExpr(x796, x797)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x796, 
          cvtEXPR x797]))
     | cvtEXPR (TypeExpr x801) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x801))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt806) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x805 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x805))
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
     | cvtEXPR (LetExpr{defs=x848, body=x849, head=opt851}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x848), ("body", cvtEXPR x849), 
          ("head", 
       (case opt851 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x850 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x850))
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
          cvtEXPR x893, cvtEXPR x894]))
     | cvtEXPR (ListExpr ls899) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x898 => 
                                                                                                    cvtEXPR x898
                                                                                             ) ls899)))
     | cvtEXPR (InitExpr(x905, x906, x907)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x905, 
          cvtHEAD x906, cvtINITS x907]))
     | cvtEXPR (SliceExpr(x911, x912, x913)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x911, cvtEXPR x912, cvtEXPR x913]))
     | cvtEXPR (GetTemp n917) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n917))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n923) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n923))
     | cvtFIXTURE_NAME (PropName x926) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x926))
   and cvtIDENT_EXPR (Identifier{ident=x929, openNamespaces=ls935}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x929), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls931 => PrettyRep.List (List.map (fn x930 => 
                                                                                cvtNAMESPACE x930
                                                                         ) ls931)
                                   ) ls935))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x946, expr=x947}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x946), ("expr", cvtEXPR x947)]))
     | cvtIDENT_EXPR (AttributeIdentifier x955) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x955))
     | cvtIDENT_EXPR (ExpressionIdentifier x958) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x958))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x961, ident=x962}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x961), ("ident", cvtUSTRING x962)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x970, typeArgs=ls972}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x970), ("typeArgs", 
          PrettyRep.List (List.map (fn x971 => cvtTYPE_EXPR x971
                                   ) ls972))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls984, x988)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x983 => cvtIDENT x983
                                                          ) ls984), cvtIDENT_EXPR x988]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s994) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s994))
     | cvtLITERAL (LiteralContextualDecimalInteger s997) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s997))
     | cvtLITERAL (LiteralContextualHexInteger s1000) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1000))
     | cvtLITERAL (LiteralDouble r1003) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1003))
     | cvtLITERAL (LiteralDecimal d1006) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1006))
     | cvtLITERAL (LiteralInt i1009) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1009))
     | cvtLITERAL (LiteralUInt u1012) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1012))
     | cvtLITERAL (LiteralBoolean b1015) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1015))
     | cvtLITERAL (LiteralString x1018) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1018))
     | cvtLITERAL (LiteralArray{exprs=ls1022, ty=opt1027}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1021 => 
                                                                         cvtEXPR x1021
                                                                  ) ls1022)), 
          ("ty", 
       (case opt1027 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1026 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1026))
       ))]))
     | cvtLITERAL (LiteralXML ls1039) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1038 => 
                                                                                                            cvtEXPR x1038
                                                                                                     ) ls1039)))
     | cvtLITERAL (LiteralNamespace x1045) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1045))
     | cvtLITERAL (LiteralObject{expr=ls1049, ty=opt1054}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1048 => 
                                                                        cvtFIELD x1048
                                                                 ) ls1049)), 
          ("ty", 
       (case opt1054 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1053 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1053))
       ))]))
     | cvtLITERAL (LiteralFunction x1065) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1065))
     | cvtLITERAL (LiteralRegExp{str=x1068}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1068)]))
   and cvtBLOCK (Block x1074) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1074))
   and cvtFIXTURE (NamespaceFixture x1077) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1077))
     | cvtFIXTURE (ClassFixture x1080) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1080))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1084) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1084))
     | cvtFIXTURE (MethodFixture{func=x1087, ty=x1088, readOnly=b1089, override=b1090, 
          final=b1091}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1087), ("ty", cvtTYPE_EXPR x1088), ("readOnly", PrettyRep.Bool b1089), 
          ("override", PrettyRep.Bool b1090), ("final", PrettyRep.Bool b1091)]))
     | cvtFIXTURE (ValFixture{ty=x1105, readOnly=b1106}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1105), ("readOnly", PrettyRep.Bool b1106)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1114, getter=opt1116, setter=opt1121}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1114), ("getter", 
       (case opt1116 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1115 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1115))
       )), ("setter", 
       (case opt1121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1120 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1120))
       ))]))
   and cvtBINDINGS (ls1135, ls1140) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1134 => 
                                                                                       cvtBINDING x1134
                                                                                ) ls1135), 
          PrettyRep.List (List.map (fn x1139 => cvtINIT_STEP x1139
                                   ) ls1140)]
   and cvtFIXTURES ls1148 = PrettyRep.List (List.map (fn (x1145, x1146) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1145, 
                                                            cvtFIXTURE x1146]
                                                     ) ls1148)
   and cvtINITS ls1155 = PrettyRep.List (List.map (fn (x1152, x1153) => PrettyRep.Tuple [cvtFIXTURE_NAME x1152, 
                                                         cvtEXPR x1153]
                                                  ) ls1155)
   and cvtHEAD (x1159, x1160) = PrettyRep.Tuple [cvtFIXTURES x1159, cvtINITS x1160]
   and cvtFIELD {kind=x1162, name=x1163, init=x1164} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1162), ("name", cvtIDENT_EXPR x1163), ("init", cvtEXPR x1164)]
   and cvtFIELD_TYPE {name=x1172, ty=x1173} = PrettyRep.Rec [("name", cvtIDENT x1172), 
          ("ty", cvtTYPE_EXPR x1173)]
   and cvtTYPED_IDENT {name=x1179, ty=opt1181} = PrettyRep.Rec [("name", cvtIDENT x1179), 
          ("ty", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1180))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1191, params=ls1196, result=x1200, thisType=opt1202, 
          hasRest=b1206, minArgs=n1207} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1190 => 
                                                                                                        cvtIDENT x1190
                                                                                                 ) ls1191)), 
          ("params", PrettyRep.List (List.map (fn x1195 => cvtTYPE_EXPR x1195
                                              ) ls1196)), ("result", cvtTYPE_EXPR x1200), 
          ("thisType", 
       (case opt1202 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1201 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1201))
       )), ("hasRest", PrettyRep.Bool b1206), ("minArgs", PrettyRep.Int n1207)]
   and cvtFUNC_DEFN {kind=x1221, ns=opt1223, final=b1227, override=b1228, prototype=b1229, 
          static=b1230, func=x1231} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1221), 
          ("ns", 
       (case opt1223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1222 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1222))
       )), ("final", PrettyRep.Bool b1227), ("override", PrettyRep.Bool b1228), 
          ("prototype", PrettyRep.Bool b1229), ("static", PrettyRep.Bool b1230), 
          ("func", cvtFUNC x1231)]
   and cvtCTOR_DEFN x1247 = cvtCTOR x1247
   and cvtVAR_DEFN {kind=x1248, ns=opt1250, static=b1254, prototype=b1255, 
          bindings=x1256} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1248), 
          ("ns", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1249))
       )), ("static", PrettyRep.Bool b1254), ("prototype", PrettyRep.Bool b1255), 
          ("bindings", cvtBINDINGS x1256)]
   and cvtNAMESPACE_DEFN {ident=x1268, ns=opt1270, init=opt1275} = PrettyRep.Rec [("ident", 
          cvtIDENT x1268), ("ns", 
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1269))
       )), ("init", 
       (case opt1275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1274 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1274))
       ))]
   and cvtCLASS_DEFN {ident=x1286, ns=opt1288, nonnullable=b1292, dynamic=b1293, 
          final=b1294, params=ls1296, extends=opt1301, implements=ls1306, classDefns=ls1311, 
          instanceDefns=ls1316, instanceStmts=ls1321, ctorDefn=opt1326} = PrettyRep.Rec [("ident", 
          cvtIDENT x1286), ("ns", 
       (case opt1288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1287 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1287))
       )), ("nonnullable", PrettyRep.Bool b1292), ("dynamic", PrettyRep.Bool b1293), 
          ("final", PrettyRep.Bool b1294), ("params", PrettyRep.List (List.map (fn x1295 => 
                                                                                      cvtIDENT x1295
                                                                               ) ls1296)), 
          ("extends", 
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1300))
       )), ("implements", PrettyRep.List (List.map (fn x1305 => cvtIDENT_EXPR x1305
                                                   ) ls1306)), ("classDefns", 
          PrettyRep.List (List.map (fn x1310 => cvtDEFN x1310
                                   ) ls1311)), ("instanceDefns", PrettyRep.List (List.map (fn x1315 => 
                                                                                                 cvtDEFN x1315
                                                                                          ) ls1316)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1320 => cvtSTMT x1320
                                                     ) ls1321)), ("ctorDefn", 
          
       (case opt1326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1325 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1325))
       ))]
   and cvtINTERFACE_DEFN {ident=x1355, ns=opt1357, nonnullable=b1361, params=ls1363, 
          extends=ls1368, block=x1372} = PrettyRep.Rec [("ident", cvtIDENT x1355), 
          ("ns", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1356))
       )), ("nonnullable", PrettyRep.Bool b1361), ("params", PrettyRep.List (List.map (fn x1362 => 
                                                                                             cvtIDENT x1362
                                                                                      ) ls1363)), 
          ("extends", PrettyRep.List (List.map (fn x1367 => cvtIDENT_EXPR x1367
                                               ) ls1368)), ("block", cvtBLOCK x1372)]
   and cvtTYPE_DEFN {ident=x1386, ns=opt1388, init=x1392} = PrettyRep.Rec [("ident", 
          cvtIDENT x1386), ("ns", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1387 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1387))
       )), ("init", cvtTYPE_EXPR x1392)]
   and cvtFOR_ENUM_STMT {defn=opt1401, obj=x1405, fixtures=opt1407, inits=opt1412, 
          labels=ls1417, body=x1421} = PrettyRep.Rec [("defn", 
       (case opt1401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1400 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1400))
       )), ("obj", cvtEXPR x1405), ("fixtures", 
       (case opt1407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1406 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1406))
       )), ("inits", 
       (case opt1412 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1411 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1411))
       )), ("labels", PrettyRep.List (List.map (fn x1416 => cvtIDENT x1416
                                               ) ls1417)), ("body", cvtSTMT x1421)]
   and cvtFOR_STMT {fixtures=opt1436, defn=opt1441, init=x1445, cond=x1446, 
          update=x1447, labels=ls1449, body=x1453} = PrettyRep.Rec [("fixtures", 
          
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1435))
       )), ("defn", 
       (case opt1441 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1440))
       )), ("init", cvtSTMT x1445), ("cond", cvtEXPR x1446), ("update", cvtEXPR x1447), 
          ("labels", PrettyRep.List (List.map (fn x1448 => cvtIDENT x1448
                                              ) ls1449)), ("body", cvtSTMT x1453)]
   and cvtWHILE_STMT {cond=x1469, fixtures=opt1471, body=x1475, labels=ls1477} = 
          PrettyRep.Rec [("cond", cvtEXPR x1469), ("fixtures", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1470))
       )), ("body", cvtSTMT x1475), ("labels", PrettyRep.List (List.map (fn x1476 => 
                                                                               cvtIDENT x1476
                                                                        ) ls1477))]
   and cvtDIRECTIVES {pragmas=ls1491, defns=ls1496, head=opt1501, body=ls1506} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1490 => 
                                                                     cvtPRAGMA x1490
                                                              ) ls1491)), ("defns", 
          PrettyRep.List (List.map (fn x1495 => cvtDEFN x1495
                                   ) ls1496)), ("head", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1500 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1500))
       )), ("body", PrettyRep.List (List.map (fn x1505 => cvtSTMT x1505
                                             ) ls1506))]
   and cvtCASE {label=opt1520, inits=opt1525, body=x1529} = PrettyRep.Rec [("label", 
          
       (case opt1520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1519 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1519))
       )), ("inits", 
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1524 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1524))
       )), ("body", cvtBLOCK x1529)]
   and cvtTYPE_CASE {ty=opt1538, bindings=x1542, inits=opt1544, body=x1548} = 
          PrettyRep.Rec [("ty", 
       (case opt1538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1537 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1537))
       )), ("bindings", cvtBINDINGS x1542), ("inits", 
       (case opt1544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1543 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1543))
       )), ("body", cvtBLOCK x1548)]
   and cvtFUNC_NAME {kind=x1558, ident=x1559} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1558), 
          ("ident", cvtIDENT x1559)]
   and cvtPACKAGE {name=ls1566, block=x1570} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1565 => 
                                                                                                       cvtIDENT x1565
                                                                                                ) ls1566)), 
          ("block", cvtBLOCK x1570)]
   and cvtPROGRAM {packages=ls1577, fixtures=opt1582, block=x1586} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1576 => cvtPACKAGE x1576
                                   ) ls1577)), ("fixtures", 
       (case opt1582 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1581 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1581))
       )), ("block", cvtBLOCK x1586)]
end

