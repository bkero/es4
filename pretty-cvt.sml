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
     | cvtTYPE_EXPR (InstanceType{name=x550, typeParams=ls552, ty=x556}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x550), 
          ("typeParams", PrettyRep.List (List.map (fn x551 => cvtIDENT x551
                                                  ) ls552)), ("ty", cvtTYPE_EXPR x556)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x567) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x567))
     | cvtSTMT (InitStmt{kind=x570, ns=opt572, prototype=b576, static=b577, 
          temps=x578, inits=ls580}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x570), ("ns", 
       (case opt572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x571 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x571))
       )), ("prototype", PrettyRep.Bool b576), ("static", PrettyRep.Bool b577), 
          ("temps", cvtBINDINGS x578), ("inits", PrettyRep.List (List.map (fn x579 => 
                                                                                 cvtINIT_STEP x579
                                                                          ) ls580))]))
     | cvtSTMT (ClassBlock{ns=opt600, ident=x604, name=opt606, block=x610}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x599 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x599))
       )), ("ident", cvtIDENT x604), ("name", 
       (case opt606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x605 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x605))
       )), ("block", cvtBLOCK x610)]))
     | cvtSTMT (PackageBlock{name=x622, block=x623}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x622), ("block", cvtBLOCK x623)]))
     | cvtSTMT (ForEachStmt x631) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x631))
     | cvtSTMT (ForInStmt x634) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x634))
     | cvtSTMT (ThrowStmt x637) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x637))
     | cvtSTMT (ReturnStmt x640) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x640))
     | cvtSTMT (BreakStmt opt644) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x643 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x643))
       ))
     | cvtSTMT (ContinueStmt opt651) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x650 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x650))
       ))
     | cvtSTMT (BlockStmt x657) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x657))
     | cvtSTMT (LabeledStmt(x660, x661)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x660, 
          cvtSTMT x661]))
     | cvtSTMT (LetStmt x665) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x665))
     | cvtSTMT (WhileStmt x668) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x668))
     | cvtSTMT (DoWhileStmt x671) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x671))
     | cvtSTMT (ForStmt x674) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x674))
     | cvtSTMT (IfStmt{cnd=x677, thn=x678, els=x679}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x677), ("thn", cvtSTMT x678), 
          ("els", cvtSTMT x679)]))
     | cvtSTMT (WithStmt{obj=x689, ty=x690, body=x691}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x689), ("ty", cvtTYPE_EXPR x690), 
          ("body", cvtSTMT x691)]))
     | cvtSTMT (TryStmt{block=x701, catches=ls723, finally=opt728}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x701), ("catches", PrettyRep.List (List.map (fn {bindings=x702, 
                                                                                                     ty=opt704, 
                                                                                                     fixtures=opt709, 
                                                                                                     block=x713} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x702), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt704 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x703 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x703))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt709 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x708 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x708))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x713)]
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
     | cvtEXPR (LetExpr{defs=x847, body=x848, head=opt850}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x847), ("body", cvtEXPR x848), 
          ("head", 
       (case opt850 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x849 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x849))
       ))]))
     | cvtEXPR (NewExpr{obj=x863, actuals=ls865}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x863), ("actuals", PrettyRep.List (List.map (fn x864 => 
                                                                                                  cvtEXPR x864
                                                                                           ) ls865))]))
     | cvtEXPR (ObjectRef{base=x876, ident=x877}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x876), ("ident", cvtIDENT_EXPR x877)]))
     | cvtEXPR (LexicalRef{ident=x885}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x885)]))
     | cvtEXPR (SetExpr(x891, x892, x893)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x891, 
          cvtEXPR x892, cvtEXPR x893]))
     | cvtEXPR (ListExpr ls898) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x897 => 
                                                                                                    cvtEXPR x897
                                                                                             ) ls898)))
     | cvtEXPR (InitExpr(x904, x905, x906)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x904, 
          cvtHEAD x905, cvtINITS x906]))
     | cvtEXPR (SliceExpr(x910, x911, x912)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x910, cvtEXPR x911, cvtEXPR x912]))
     | cvtEXPR (GetTemp n916) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n916))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n922) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n922))
     | cvtFIXTURE_NAME (PropName x925) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x925))
   and cvtIDENT_EXPR (Identifier{ident=x928, openNamespaces=ls934}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x928), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls930 => PrettyRep.List (List.map (fn x929 => 
                                                                                cvtNAMESPACE x929
                                                                         ) ls930)
                                   ) ls934))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x945, expr=x946}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x945), ("expr", cvtEXPR x946)]))
     | cvtIDENT_EXPR (AttributeIdentifier x954) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x954))
     | cvtIDENT_EXPR (ExpressionIdentifier x957) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x957))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x960, ident=x961}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x960), ("ident", cvtUSTRING x961)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x969, typeArgs=ls971}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x969), ("typeArgs", 
          PrettyRep.List (List.map (fn x970 => cvtTYPE_EXPR x970
                                   ) ls971))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls983, x987)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x982 => cvtIDENT x982
                                                          ) ls983), cvtIDENT_EXPR x987]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s993) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s993))
     | cvtLITERAL (LiteralContextualDecimalInteger s996) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s996))
     | cvtLITERAL (LiteralContextualHexInteger s999) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s999))
     | cvtLITERAL (LiteralDouble r1002) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1002))
     | cvtLITERAL (LiteralDecimal d1005) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1005))
     | cvtLITERAL (LiteralInt i1008) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1008))
     | cvtLITERAL (LiteralUInt u1011) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1011))
     | cvtLITERAL (LiteralBoolean b1014) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1014))
     | cvtLITERAL (LiteralString x1017) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1017))
     | cvtLITERAL (LiteralArray{exprs=ls1021, ty=opt1026}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1020 => 
                                                                         cvtEXPR x1020
                                                                  ) ls1021)), 
          ("ty", 
       (case opt1026 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1025 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1025))
       ))]))
     | cvtLITERAL (LiteralXML ls1038) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1037 => 
                                                                                                            cvtEXPR x1037
                                                                                                     ) ls1038)))
     | cvtLITERAL (LiteralNamespace x1044) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1044))
     | cvtLITERAL (LiteralObject{expr=ls1048, ty=opt1053}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1047 => 
                                                                        cvtFIELD x1047
                                                                 ) ls1048)), 
          ("ty", 
       (case opt1053 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1052 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1052))
       ))]))
     | cvtLITERAL (LiteralFunction x1064) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1064))
     | cvtLITERAL (LiteralRegExp{str=x1067}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1067)]))
   and cvtBLOCK (Block x1073) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1073))
   and cvtFIXTURE (NamespaceFixture x1076) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1076))
     | cvtFIXTURE (ClassFixture x1079) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1079))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1083) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1083))
     | cvtFIXTURE (MethodFixture{func=x1086, ty=x1087, readOnly=b1088, override=b1089, 
          final=b1090}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1086), ("ty", cvtTYPE_EXPR x1087), ("readOnly", PrettyRep.Bool b1088), 
          ("override", PrettyRep.Bool b1089), ("final", PrettyRep.Bool b1090)]))
     | cvtFIXTURE (ValFixture{ty=x1104, readOnly=b1105}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1104), ("readOnly", PrettyRep.Bool b1105)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1113, getter=opt1115, setter=opt1120}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1113), ("getter", 
       (case opt1115 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1114 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1114))
       )), ("setter", 
       (case opt1120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1119 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1119))
       ))]))
   and cvtBINDINGS (ls1134, ls1139) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1133 => 
                                                                                       cvtBINDING x1133
                                                                                ) ls1134), 
          PrettyRep.List (List.map (fn x1138 => cvtINIT_STEP x1138
                                   ) ls1139)]
   and cvtFIXTURES ls1147 = PrettyRep.List (List.map (fn (x1144, x1145) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1144, 
                                                            cvtFIXTURE x1145]
                                                     ) ls1147)
   and cvtINITS ls1154 = PrettyRep.List (List.map (fn (x1151, x1152) => PrettyRep.Tuple [cvtFIXTURE_NAME x1151, 
                                                         cvtEXPR x1152]
                                                  ) ls1154)
   and cvtHEAD (x1158, x1159) = PrettyRep.Tuple [cvtFIXTURES x1158, cvtINITS x1159]
   and cvtFIELD {kind=x1161, name=x1162, init=x1163} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1161), ("name", cvtIDENT_EXPR x1162), ("init", cvtEXPR x1163)]
   and cvtFIELD_TYPE {name=x1171, ty=x1172} = PrettyRep.Rec [("name", cvtIDENT x1171), 
          ("ty", cvtTYPE_EXPR x1172)]
   and cvtTYPED_IDENT {name=x1178, ty=opt1180} = PrettyRep.Rec [("name", cvtIDENT x1178), 
          ("ty", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1179))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1190, params=ls1195, result=x1199, thisType=opt1201, 
          hasRest=b1205, minArgs=n1206} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1189 => 
                                                                                                        cvtIDENT x1189
                                                                                                 ) ls1190)), 
          ("params", PrettyRep.List (List.map (fn x1194 => cvtTYPE_EXPR x1194
                                              ) ls1195)), ("result", cvtTYPE_EXPR x1199), 
          ("thisType", 
       (case opt1201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1200 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1200))
       )), ("hasRest", PrettyRep.Bool b1205), ("minArgs", PrettyRep.Int n1206)]
   and cvtFUNC_DEFN {kind=x1220, ns=opt1222, final=b1226, override=b1227, prototype=b1228, 
          static=b1229, func=x1230} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1220), 
          ("ns", 
       (case opt1222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1221))
       )), ("final", PrettyRep.Bool b1226), ("override", PrettyRep.Bool b1227), 
          ("prototype", PrettyRep.Bool b1228), ("static", PrettyRep.Bool b1229), 
          ("func", cvtFUNC x1230)]
   and cvtCTOR_DEFN x1246 = cvtCTOR x1246
   and cvtVAR_DEFN {kind=x1247, ns=opt1249, static=b1253, prototype=b1254, 
          bindings=x1255} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1247), 
          ("ns", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1248))
       )), ("static", PrettyRep.Bool b1253), ("prototype", PrettyRep.Bool b1254), 
          ("bindings", cvtBINDINGS x1255)]
   and cvtNAMESPACE_DEFN {ident=x1267, ns=opt1269, init=opt1274} = PrettyRep.Rec [("ident", 
          cvtIDENT x1267), ("ns", 
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1268))
       )), ("init", 
       (case opt1274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1273 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1273))
       ))]
   and cvtCLASS_DEFN {ident=x1285, ns=opt1287, nonnullable=b1291, dynamic=b1292, 
          final=b1293, params=ls1295, extends=opt1300, implements=ls1305, classDefns=ls1310, 
          instanceDefns=ls1315, instanceStmts=ls1320, ctorDefn=opt1325} = PrettyRep.Rec [("ident", 
          cvtIDENT x1285), ("ns", 
       (case opt1287 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1286 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1286))
       )), ("nonnullable", PrettyRep.Bool b1291), ("dynamic", PrettyRep.Bool b1292), 
          ("final", PrettyRep.Bool b1293), ("params", PrettyRep.List (List.map (fn x1294 => 
                                                                                      cvtIDENT x1294
                                                                               ) ls1295)), 
          ("extends", 
       (case opt1300 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1299 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1299))
       )), ("implements", PrettyRep.List (List.map (fn x1304 => cvtIDENT_EXPR x1304
                                                   ) ls1305)), ("classDefns", 
          PrettyRep.List (List.map (fn x1309 => cvtDEFN x1309
                                   ) ls1310)), ("instanceDefns", PrettyRep.List (List.map (fn x1314 => 
                                                                                                 cvtDEFN x1314
                                                                                          ) ls1315)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1319 => cvtSTMT x1319
                                                     ) ls1320)), ("ctorDefn", 
          
       (case opt1325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1324 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1324))
       ))]
   and cvtINTERFACE_DEFN {ident=x1354, ns=opt1356, nonnullable=b1360, params=ls1362, 
          extends=ls1367, block=x1371} = PrettyRep.Rec [("ident", cvtIDENT x1354), 
          ("ns", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1355))
       )), ("nonnullable", PrettyRep.Bool b1360), ("params", PrettyRep.List (List.map (fn x1361 => 
                                                                                             cvtIDENT x1361
                                                                                      ) ls1362)), 
          ("extends", PrettyRep.List (List.map (fn x1366 => cvtIDENT_EXPR x1366
                                               ) ls1367)), ("block", cvtBLOCK x1371)]
   and cvtTYPE_DEFN {ident=x1385, ns=opt1387, init=x1391} = PrettyRep.Rec [("ident", 
          cvtIDENT x1385), ("ns", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1386 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1386))
       )), ("init", cvtTYPE_EXPR x1391)]
   and cvtFOR_ENUM_STMT {defn=opt1400, obj=x1404, fixtures=opt1406, inits=opt1411, 
          labels=ls1416, body=x1420} = PrettyRep.Rec [("defn", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1399))
       )), ("obj", cvtEXPR x1404), ("fixtures", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1405))
       )), ("inits", 
       (case opt1411 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1410 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1410))
       )), ("labels", PrettyRep.List (List.map (fn x1415 => cvtIDENT x1415
                                               ) ls1416)), ("body", cvtSTMT x1420)]
   and cvtFOR_STMT {fixtures=opt1435, defn=opt1440, init=x1444, cond=x1445, 
          update=x1446, labels=ls1448, body=x1452} = PrettyRep.Rec [("fixtures", 
          
       (case opt1435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1434))
       )), ("defn", 
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1439 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1439))
       )), ("init", cvtSTMT x1444), ("cond", cvtEXPR x1445), ("update", cvtEXPR x1446), 
          ("labels", PrettyRep.List (List.map (fn x1447 => cvtIDENT x1447
                                              ) ls1448)), ("body", cvtSTMT x1452)]
   and cvtWHILE_STMT {cond=x1468, fixtures=opt1470, body=x1474, labels=ls1476} = 
          PrettyRep.Rec [("cond", cvtEXPR x1468), ("fixtures", 
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1469))
       )), ("body", cvtSTMT x1474), ("labels", PrettyRep.List (List.map (fn x1475 => 
                                                                               cvtIDENT x1475
                                                                        ) ls1476))]
   and cvtDIRECTIVES {pragmas=ls1490, defns=ls1495, head=opt1500, body=ls1505} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1489 => 
                                                                     cvtPRAGMA x1489
                                                              ) ls1490)), ("defns", 
          PrettyRep.List (List.map (fn x1494 => cvtDEFN x1494
                                   ) ls1495)), ("head", 
       (case opt1500 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1499 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1499))
       )), ("body", PrettyRep.List (List.map (fn x1504 => cvtSTMT x1504
                                             ) ls1505))]
   and cvtCASE {label=opt1519, inits=opt1524, body=x1528} = PrettyRep.Rec [("label", 
          
       (case opt1519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1518 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1518))
       )), ("inits", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1523))
       )), ("body", cvtBLOCK x1528)]
   and cvtTYPE_CASE {ty=opt1537, bindings=x1541, inits=opt1543, body=x1547} = 
          PrettyRep.Rec [("ty", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1536))
       )), ("bindings", cvtBINDINGS x1541), ("inits", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1542))
       )), ("body", cvtBLOCK x1547)]
   and cvtFUNC_NAME {kind=x1557, ident=x1558} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1557), 
          ("ident", cvtIDENT x1558)]
   and cvtPACKAGE {name=ls1565, block=x1569} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1564 => 
                                                                                                       cvtIDENT x1564
                                                                                                ) ls1565)), 
          ("block", cvtBLOCK x1569)]
   and cvtPROGRAM {packages=ls1576, fixtures=opt1581, block=x1585} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1575 => cvtPACKAGE x1575
                                   ) ls1576)), ("fixtures", 
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1580))
       )), ("block", cvtBLOCK x1585)]
end

