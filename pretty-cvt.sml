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
     | cvtSTMT (SwitchStmt{cond=x735, cases=ls737}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x735), ("cases", PrettyRep.List (List.map (fn x736 => 
                                                                                                 cvtCASE x736
                                                                                          ) ls737))]))
     | cvtSTMT (SwitchTypeStmt{cond=x748, ty=x749, cases=ls751}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x748), ("ty", cvtTYPE_EXPR x749), 
          ("cases", PrettyRep.List (List.map (fn x750 => cvtTYPE_CASE x750
                                             ) ls751))]))
     | cvtSTMT (Dxns{expr=x764}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x764)]))
   and cvtEXPR (TrinaryExpr(x770, x771, x772, x773)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x770, cvtEXPR x771, cvtEXPR x772, 
          cvtEXPR x773]))
     | cvtEXPR (BinaryExpr(x777, x778, x779)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x777, cvtEXPR x778, cvtEXPR x779]))
     | cvtEXPR (BinaryTypeExpr(x783, x784, x785)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x783, cvtEXPR x784, cvtTYPE_EXPR x785]))
     | cvtEXPR (UnaryExpr(x789, x790)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x789, 
          cvtEXPR x790]))
     | cvtEXPR (TypeExpr x794) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x794))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt799) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt799 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x798 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x798))
       ))
     | cvtEXPR (SuperExpr opt806) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x805 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x805))
       ))
     | cvtEXPR (LiteralExpr x812) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x812))
     | cvtEXPR (CallExpr{func=x815, actuals=ls817}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x815), ("actuals", PrettyRep.List (List.map (fn x816 => 
                                                                                                   cvtEXPR x816
                                                                                            ) ls817))]))
     | cvtEXPR (ApplyTypeExpr{expr=x828, actuals=ls830}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x828), ("actuals", PrettyRep.List (List.map (fn x829 => 
                                                                                                   cvtTYPE_EXPR x829
                                                                                            ) ls830))]))
     | cvtEXPR (LetExpr{defs=x841, body=x842, head=opt844}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x841), ("body", cvtEXPR x842), 
          ("head", 
       (case opt844 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x843 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x843))
       ))]))
     | cvtEXPR (NewExpr{obj=x857, actuals=ls859}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x857), ("actuals", PrettyRep.List (List.map (fn x858 => 
                                                                                                  cvtEXPR x858
                                                                                           ) ls859))]))
     | cvtEXPR (ObjectRef{base=x870, ident=x871}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x870), ("ident", cvtIDENT_EXPR x871)]))
     | cvtEXPR (LexicalRef{ident=x879}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x879)]))
     | cvtEXPR (SetExpr(x885, x886, x887)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x885, 
          cvtEXPR x886, cvtEXPR x887]))
     | cvtEXPR (ListExpr ls892) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x891 => 
                                                                                                    cvtEXPR x891
                                                                                             ) ls892)))
     | cvtEXPR (InitExpr(x898, x899, x900)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x898, 
          cvtHEAD x899, cvtINITS x900]))
     | cvtEXPR (SliceExpr(x904, x905, x906)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x904, cvtEXPR x905, cvtEXPR x906]))
     | cvtEXPR (GetTemp n910) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n910))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n916) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n916))
     | cvtFIXTURE_NAME (PropName x919) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x919))
   and cvtIDENT_EXPR (Identifier{ident=x922, openNamespaces=ls928}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x922), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls924 => PrettyRep.List (List.map (fn x923 => 
                                                                                cvtNAMESPACE x923
                                                                         ) ls924)
                                   ) ls928))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x939, expr=x940}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x939), ("expr", cvtEXPR x940)]))
     | cvtIDENT_EXPR (AttributeIdentifier x948) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x948))
     | cvtIDENT_EXPR (ExpressionIdentifier x951) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x951))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x954, ident=x955}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x954), ("ident", cvtUSTRING x955)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x963, typeArgs=ls965}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x963), ("typeArgs", 
          PrettyRep.List (List.map (fn x964 => cvtTYPE_EXPR x964
                                   ) ls965))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls977, x981)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x976 => cvtIDENT x976
                                                          ) ls977), cvtIDENT_EXPR x981]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s987) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s987))
     | cvtLITERAL (LiteralContextualDecimalInteger s990) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s990))
     | cvtLITERAL (LiteralContextualHexInteger s993) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s993))
     | cvtLITERAL (LiteralDouble r996) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r996))
     | cvtLITERAL (LiteralDecimal d999) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d999))
     | cvtLITERAL (LiteralInt i1002) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1002))
     | cvtLITERAL (LiteralUInt u1005) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1005))
     | cvtLITERAL (LiteralBoolean b1008) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1008))
     | cvtLITERAL (LiteralString x1011) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1011))
     | cvtLITERAL (LiteralArray{exprs=ls1015, ty=opt1020}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1014 => 
                                                                         cvtEXPR x1014
                                                                  ) ls1015)), 
          ("ty", 
       (case opt1020 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1019 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1019))
       ))]))
     | cvtLITERAL (LiteralXML ls1032) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1031 => 
                                                                                                            cvtEXPR x1031
                                                                                                     ) ls1032)))
     | cvtLITERAL (LiteralNamespace x1038) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1038))
     | cvtLITERAL (LiteralObject{expr=ls1042, ty=opt1047}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1041 => 
                                                                        cvtFIELD x1041
                                                                 ) ls1042)), 
          ("ty", 
       (case opt1047 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1046 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1046))
       ))]))
     | cvtLITERAL (LiteralFunction x1058) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1058))
     | cvtLITERAL (LiteralRegExp{str=x1061}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1061)]))
   and cvtBLOCK (Block x1067) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1067))
   and cvtFIXTURE (NamespaceFixture x1070) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1070))
     | cvtFIXTURE (ClassFixture x1073) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1073))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1077) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1077))
     | cvtFIXTURE (MethodFixture{func=x1080, ty=x1081, readOnly=b1082, override=b1083, 
          final=b1084}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1080), ("ty", cvtTYPE_EXPR x1081), ("readOnly", PrettyRep.Bool b1082), 
          ("override", PrettyRep.Bool b1083), ("final", PrettyRep.Bool b1084)]))
     | cvtFIXTURE (ValFixture{ty=x1098, readOnly=b1099}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1098), ("readOnly", PrettyRep.Bool b1099)]))
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
   and cvtBINDINGS (ls1128, ls1133) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1127 => 
                                                                                       cvtBINDING x1127
                                                                                ) ls1128), 
          PrettyRep.List (List.map (fn x1132 => cvtINIT_STEP x1132
                                   ) ls1133)]
   and cvtFIXTURES ls1141 = PrettyRep.List (List.map (fn (x1138, x1139) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1138, 
                                                            cvtFIXTURE x1139]
                                                     ) ls1141)
   and cvtINITS ls1148 = PrettyRep.List (List.map (fn (x1145, x1146) => PrettyRep.Tuple [cvtFIXTURE_NAME x1145, 
                                                         cvtEXPR x1146]
                                                  ) ls1148)
   and cvtHEAD (x1152, x1153) = PrettyRep.Tuple [cvtFIXTURES x1152, cvtINITS x1153]
   and cvtFIELD {kind=x1155, name=x1156, init=x1157} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1155), ("name", cvtIDENT_EXPR x1156), ("init", cvtEXPR x1157)]
   and cvtFIELD_TYPE {name=x1165, ty=x1166} = PrettyRep.Rec [("name", cvtIDENT x1165), 
          ("ty", cvtTYPE_EXPR x1166)]
   and cvtTYPED_IDENT {name=x1172, ty=opt1174} = PrettyRep.Rec [("name", cvtIDENT x1172), 
          ("ty", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1173))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1184, params=ls1189, result=x1193, thisType=opt1195, 
          hasRest=b1199, minArgs=n1200} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1183 => 
                                                                                                        cvtIDENT x1183
                                                                                                 ) ls1184)), 
          ("params", PrettyRep.List (List.map (fn x1188 => cvtTYPE_EXPR x1188
                                              ) ls1189)), ("result", cvtTYPE_EXPR x1193), 
          ("thisType", 
       (case opt1195 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1194 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1194))
       )), ("hasRest", PrettyRep.Bool b1199), ("minArgs", PrettyRep.Int n1200)]
   and cvtFUNC_DEFN {kind=x1214, ns=opt1216, final=b1220, override=b1221, prototype=b1222, 
          static=b1223, func=x1224} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1214), 
          ("ns", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1215))
       )), ("final", PrettyRep.Bool b1220), ("override", PrettyRep.Bool b1221), 
          ("prototype", PrettyRep.Bool b1222), ("static", PrettyRep.Bool b1223), 
          ("func", cvtFUNC x1224)]
   and cvtCTOR_DEFN x1240 = cvtCTOR x1240
   and cvtVAR_DEFN {kind=x1241, ns=opt1243, static=b1247, prototype=b1248, 
          bindings=x1249} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1241), 
          ("ns", 
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       )), ("static", PrettyRep.Bool b1247), ("prototype", PrettyRep.Bool b1248), 
          ("bindings", cvtBINDINGS x1249)]
   and cvtNAMESPACE_DEFN {ident=x1261, ns=opt1263, init=opt1268} = PrettyRep.Rec [("ident", 
          cvtIDENT x1261), ("ns", 
       (case opt1263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1262 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1262))
       )), ("init", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1267))
       ))]
   and cvtCLASS_DEFN {ident=x1279, ns=opt1281, nonnullable=b1285, dynamic=b1286, 
          final=b1287, params=ls1289, extends=opt1294, implements=ls1299, classDefns=ls1304, 
          instanceDefns=ls1309, instanceStmts=ls1314, ctorDefn=opt1319} = PrettyRep.Rec [("ident", 
          cvtIDENT x1279), ("ns", 
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1280))
       )), ("nonnullable", PrettyRep.Bool b1285), ("dynamic", PrettyRep.Bool b1286), 
          ("final", PrettyRep.Bool b1287), ("params", PrettyRep.List (List.map (fn x1288 => 
                                                                                      cvtIDENT x1288
                                                                               ) ls1289)), 
          ("extends", 
       (case opt1294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1293 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1293))
       )), ("implements", PrettyRep.List (List.map (fn x1298 => cvtIDENT_EXPR x1298
                                                   ) ls1299)), ("classDefns", 
          PrettyRep.List (List.map (fn x1303 => cvtDEFN x1303
                                   ) ls1304)), ("instanceDefns", PrettyRep.List (List.map (fn x1308 => 
                                                                                                 cvtDEFN x1308
                                                                                          ) ls1309)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1313 => cvtSTMT x1313
                                                     ) ls1314)), ("ctorDefn", 
          
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1318))
       ))]
   and cvtINTERFACE_DEFN {ident=x1348, ns=opt1350, nonnullable=b1354, params=ls1356, 
          extends=ls1361, block=x1365} = PrettyRep.Rec [("ident", cvtIDENT x1348), 
          ("ns", 
       (case opt1350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1349 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1349))
       )), ("nonnullable", PrettyRep.Bool b1354), ("params", PrettyRep.List (List.map (fn x1355 => 
                                                                                             cvtIDENT x1355
                                                                                      ) ls1356)), 
          ("extends", PrettyRep.List (List.map (fn x1360 => cvtIDENT_EXPR x1360
                                               ) ls1361)), ("block", cvtBLOCK x1365)]
   and cvtTYPE_DEFN {ident=x1379, ns=opt1381, init=x1385} = PrettyRep.Rec [("ident", 
          cvtIDENT x1379), ("ns", 
       (case opt1381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1380 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1380))
       )), ("init", cvtTYPE_EXPR x1385)]
   and cvtFOR_ENUM_STMT {defn=opt1394, obj=x1398, fixtures=opt1400, inits=opt1405, 
          labels=ls1410, body=x1414} = PrettyRep.Rec [("defn", 
       (case opt1394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1393 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1393))
       )), ("obj", cvtEXPR x1398), ("fixtures", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1399))
       )), ("inits", 
       (case opt1405 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1404 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1404))
       )), ("labels", PrettyRep.List (List.map (fn x1409 => cvtIDENT x1409
                                               ) ls1410)), ("body", cvtSTMT x1414)]
   and cvtFOR_STMT {fixtures=opt1429, defn=opt1434, init=x1438, cond=x1439, 
          update=x1440, labels=ls1442, body=x1446} = PrettyRep.Rec [("fixtures", 
          
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1428 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1428))
       )), ("defn", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1433))
       )), ("init", cvtSTMT x1438), ("cond", cvtEXPR x1439), ("update", cvtEXPR x1440), 
          ("labels", PrettyRep.List (List.map (fn x1441 => cvtIDENT x1441
                                              ) ls1442)), ("body", cvtSTMT x1446)]
   and cvtWHILE_STMT {cond=x1462, fixtures=opt1464, body=x1468, labels=ls1470} = 
          PrettyRep.Rec [("cond", cvtEXPR x1462), ("fixtures", 
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1463 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1463))
       )), ("body", cvtSTMT x1468), ("labels", PrettyRep.List (List.map (fn x1469 => 
                                                                               cvtIDENT x1469
                                                                        ) ls1470))]
   and cvtDIRECTIVES {pragmas=ls1484, defns=ls1489, head=opt1494, body=ls1499} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1483 => 
                                                                     cvtPRAGMA x1483
                                                              ) ls1484)), ("defns", 
          PrettyRep.List (List.map (fn x1488 => cvtDEFN x1488
                                   ) ls1489)), ("head", 
       (case opt1494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1493))
       )), ("body", PrettyRep.List (List.map (fn x1498 => cvtSTMT x1498
                                             ) ls1499))]
   and cvtCASE {label=opt1513, inits=opt1518, body=x1522} = PrettyRep.Rec [("label", 
          
       (case opt1513 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1512 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1512))
       )), ("inits", 
       (case opt1518 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1517 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1517))
       )), ("body", cvtBLOCK x1522)]
   and cvtTYPE_CASE {ty=opt1531, bindings=x1535, inits=opt1537, body=x1541} = 
          PrettyRep.Rec [("ty", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1530))
       )), ("bindings", cvtBINDINGS x1535), ("inits", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1536))
       )), ("body", cvtBLOCK x1541)]
   and cvtFUNC_NAME {kind=x1551, ident=x1552} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1551), 
          ("ident", cvtIDENT x1552)]
   and cvtPACKAGE {name=ls1559, block=x1563} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1558 => 
                                                                                                       cvtIDENT x1558
                                                                                                ) ls1559)), 
          ("block", cvtBLOCK x1563)]
   and cvtPROGRAM {packages=ls1570, fixtures=opt1575, block=x1579} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1569 => cvtPACKAGE x1569
                                   ) ls1570)), ("fixtures", 
       (case opt1575 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1574 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1574))
       )), ("block", cvtBLOCK x1579)]
end

