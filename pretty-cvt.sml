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
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
   and cvtCLS (Cls{name=x302, extends=opt304, implements=ls309, classFixtures=x313, 
          instanceFixtures=x314, instanceInits=x315, constructor=opt317, classType=x321, 
          instanceType=x322}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x302), ("extends", 
       (case opt304 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x303 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x303))
       )), ("implements", PrettyRep.List (List.map (fn x308 => cvtNAME x308
                                                   ) ls309)), ("classFixtures", 
          cvtFIXTURES x313), ("instanceFixtures", cvtFIXTURES x314), ("instanceInits", 
          cvtHEAD x315), ("constructor", 
       (case opt317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x316 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x316))
       )), ("classType", cvtTYPE_EXPR x321), ("instanceType", cvtTYPE_EXPR x322)]))
   and cvtCTOR (Ctor{settings=x344, superArgs=ls346, func=x350}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x344), ("superArgs", PrettyRep.List (List.map (fn x345 => 
                                                                                                         cvtEXPR x345
                                                                                                  ) ls346)), 
          ("func", cvtFUNC x350)]))
   and cvtFUNC (Func{name=x360, fsig=x361, isNative=b362, block=x363, param=x364, 
          defaults=ls366, ty=x370}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x360), ("fsig", cvtFUNC_SIG x361), ("isNative", PrettyRep.Bool b362), 
          ("block", cvtBLOCK x363), ("param", cvtHEAD x364), ("defaults", PrettyRep.List (List.map (fn x365 => 
                                                                                                          cvtEXPR x365
                                                                                                   ) ls366)), 
          ("ty", cvtFUNC_TYPE x370)]))
   and cvtDEFN (ClassDefn x388) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x388))
     | cvtDEFN (VariableDefn x391) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x391))
     | cvtDEFN (FunctionDefn x394) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x394))
     | cvtDEFN (ConstructorDefn x397) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x397))
     | cvtDEFN (InterfaceDefn x400) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x400))
     | cvtDEFN (NamespaceDefn x403) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x403))
     | cvtDEFN (TypeDefn x406) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x406))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls410, params=x414, defaults=ls416, 
          ctorInits=opt427, returnType=x431, thisType=opt433, hasRest=b437}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x409 => cvtIDENT x409
                                   ) ls410)), ("params", cvtBINDINGS x414), 
          ("defaults", PrettyRep.List (List.map (fn x415 => cvtEXPR x415
                                                ) ls416)), ("ctorInits", 
       (case opt427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x420, ls422) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x420, 
            PrettyRep.List (List.map (fn x421 => cvtEXPR x421
                                     ) ls422)]))
       )), ("returnType", cvtTYPE_EXPR x431), ("thisType", 
       (case opt433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x432 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x432))
       )), ("hasRest", PrettyRep.Bool b437)]))
   and cvtBINDING (Binding{ident=x455, ty=opt457}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x455), ("ty", 
       (case opt457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x456 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x456))
       ))]))
   and cvtBINDING_IDENT (TempIdent n468) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n468))
     | cvtBINDING_IDENT (PropIdent x471) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x471))
   and cvtINIT_STEP (InitStep(x474, x475)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x474, 
          cvtEXPR x475]))
     | cvtINIT_STEP (AssignStep(x479, x480)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x479, cvtEXPR x480]))
   and cvtTYPE_EXPR (SpecialType x484) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x484))
     | cvtTYPE_EXPR (UnionType ls488) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x487 => 
                                                                                                           cvtTYPE_EXPR x487
                                                                                                    ) ls488)))
     | cvtTYPE_EXPR (ArrayType ls495) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x494 => 
                                                                                                           cvtTYPE_EXPR x494
                                                                                                    ) ls495)))
     | cvtTYPE_EXPR (TypeName x501) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x501))
     | cvtTYPE_EXPR (ElementTypeRef(x504, n505)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x504, PrettyRep.Int n505]))
     | cvtTYPE_EXPR (FieldTypeRef(x509, x510)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x509, cvtIDENT x510]))
     | cvtTYPE_EXPR (FunctionType x514) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x514))
     | cvtTYPE_EXPR (ObjectType ls518) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x517 => 
                                                                                                             cvtFIELD_TYPE x517
                                                                                                      ) ls518)))
     | cvtTYPE_EXPR (AppType{base=x524, args=ls526}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x524), ("args", PrettyRep.List (List.map (fn x525 => 
                                                                                                     cvtTYPE_EXPR x525
                                                                                              ) ls526))]))
     | cvtTYPE_EXPR (NullableType{expr=x537, nullable=b538}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x537), ("nullable", PrettyRep.Bool b538)]))
     | cvtTYPE_EXPR (InstanceType{name=x546, typeParams=ls548, ty=x552, isDynamic=b553}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x546), 
          ("typeParams", PrettyRep.List (List.map (fn x547 => cvtIDENT x547
                                                  ) ls548)), ("ty", cvtTYPE_EXPR x552), 
          ("isDynamic", PrettyRep.Bool b553)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x566) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x566))
     | cvtSTMT (InitStmt{kind=x569, ns=opt571, prototype=b575, static=b576, 
          temps=x577, inits=ls579}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x569), ("ns", 
       (case opt571 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x570 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x570))
       )), ("prototype", PrettyRep.Bool b575), ("static", PrettyRep.Bool b576), 
          ("temps", cvtBINDINGS x577), ("inits", PrettyRep.List (List.map (fn x578 => 
                                                                                 cvtINIT_STEP x578
                                                                          ) ls579))]))
     | cvtSTMT (ClassBlock{ns=opt599, ident=x603, name=opt605, block=x609}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x598 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x598))
       )), ("ident", cvtIDENT x603), ("name", 
       (case opt605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x604 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x604))
       )), ("block", cvtBLOCK x609)]))
     | cvtSTMT (ForEachStmt x621) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x621))
     | cvtSTMT (ForInStmt x624) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x624))
     | cvtSTMT (ThrowStmt x627) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x627))
     | cvtSTMT (ReturnStmt x630) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x630))
     | cvtSTMT (BreakStmt opt634) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x633 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x633))
       ))
     | cvtSTMT (ContinueStmt opt641) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt641 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x640 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x640))
       ))
     | cvtSTMT (BlockStmt x647) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x647))
     | cvtSTMT (LabeledStmt(x650, x651)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x650, 
          cvtSTMT x651]))
     | cvtSTMT (LetStmt x655) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x655))
     | cvtSTMT (WhileStmt x658) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x658))
     | cvtSTMT (DoWhileStmt x661) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x661))
     | cvtSTMT (ForStmt x664) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x664))
     | cvtSTMT (IfStmt{cnd=x667, thn=x668, els=x669}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x667), ("thn", cvtSTMT x668), 
          ("els", cvtSTMT x669)]))
     | cvtSTMT (WithStmt{obj=x679, ty=x680, body=x681}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x679), ("ty", cvtTYPE_EXPR x680), 
          ("body", cvtSTMT x681)]))
     | cvtSTMT (TryStmt{block=x691, catches=ls713, finally=opt718}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x691), ("catches", PrettyRep.List (List.map (fn {bindings=x692, 
                                                                                                     ty=opt694, 
                                                                                                     fixtures=opt699, 
                                                                                                     block=x703} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x692), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt694 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x693 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x693))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt699 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x698 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x698))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x703)]
                                                                                              ) ls713)), 
          ("finally", 
       (case opt718 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x717 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x717))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt732, cond=x736, cases=ls738}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x731))
       )), ("cond", cvtEXPR x736), ("cases", PrettyRep.List (List.map (fn x737 => 
                                                                             cvtCASE x737
                                                                      ) ls738))]))
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
     | cvtEXPR (LetExpr{defs=x844, body=x845, head=opt847}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x844), ("body", cvtEXPR x845), 
          ("head", 
       (case opt847 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x846 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x846))
       ))]))
     | cvtEXPR (NewExpr{obj=x860, actuals=ls862}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x860), ("actuals", PrettyRep.List (List.map (fn x861 => 
                                                                                                  cvtEXPR x861
                                                                                           ) ls862))]))
     | cvtEXPR (ObjectRef{base=x873, ident=x874}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x873), ("ident", cvtIDENT_EXPR x874)]))
     | cvtEXPR (LexicalRef{ident=x882}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x882)]))
     | cvtEXPR (SetExpr(x888, x889, x890)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x888, 
          cvtEXPR x889, cvtEXPR x890]))
     | cvtEXPR (ListExpr ls895) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x894 => 
                                                                                                    cvtEXPR x894
                                                                                             ) ls895)))
     | cvtEXPR (InitExpr(x901, x902, x903)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x901, 
          cvtHEAD x902, cvtINITS x903]))
     | cvtEXPR (SliceExpr(x907, x908, x909)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x907, cvtEXPR x908, cvtEXPR x909]))
     | cvtEXPR (GetTemp n913) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n913))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n919) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n919))
     | cvtFIXTURE_NAME (PropName x922) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x922))
   and cvtIDENT_EXPR (Identifier{ident=x925, openNamespaces=ls931}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x925), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls927 => PrettyRep.List (List.map (fn x926 => 
                                                                                cvtNAMESPACE x926
                                                                         ) ls927)
                                   ) ls931))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x942, expr=x943}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x942), ("expr", cvtEXPR x943)]))
     | cvtIDENT_EXPR (AttributeIdentifier x951) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x951))
     | cvtIDENT_EXPR (ExpressionIdentifier x954) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x954))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x957, ident=x958}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x957), ("ident", cvtUSTRING x958)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x966, typeArgs=ls968}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x966), ("typeArgs", 
          PrettyRep.List (List.map (fn x967 => cvtTYPE_EXPR x967
                                   ) ls968))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls980, x984)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x979 => cvtIDENT x979
                                                          ) ls980), cvtIDENT_EXPR x984]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s990) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s990))
     | cvtLITERAL (LiteralContextualDecimalInteger s993) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s993))
     | cvtLITERAL (LiteralContextualHexInteger s996) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s996))
     | cvtLITERAL (LiteralDouble r999) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r999))
     | cvtLITERAL (LiteralDecimal d1002) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1002))
     | cvtLITERAL (LiteralInt i1005) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1005))
     | cvtLITERAL (LiteralUInt u1008) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1008))
     | cvtLITERAL (LiteralBoolean b1011) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1011))
     | cvtLITERAL (LiteralString x1014) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1014))
     | cvtLITERAL (LiteralArray{exprs=ls1018, ty=opt1023}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1017 => 
                                                                         cvtEXPR x1017
                                                                  ) ls1018)), 
          ("ty", 
       (case opt1023 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1022 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1022))
       ))]))
     | cvtLITERAL (LiteralXML ls1035) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1034 => 
                                                                                                            cvtEXPR x1034
                                                                                                     ) ls1035)))
     | cvtLITERAL (LiteralNamespace x1041) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1041))
     | cvtLITERAL (LiteralObject{expr=ls1045, ty=opt1050}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1044 => 
                                                                        cvtFIELD x1044
                                                                 ) ls1045)), 
          ("ty", 
       (case opt1050 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1049 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1049))
       ))]))
     | cvtLITERAL (LiteralFunction x1061) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1061))
     | cvtLITERAL (LiteralRegExp{str=x1064}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1064)]))
   and cvtBLOCK (Block x1070) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1070))
   and cvtFIXTURE (NamespaceFixture x1073) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1073))
     | cvtFIXTURE (ClassFixture x1076) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1076))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1080) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1080))
     | cvtFIXTURE (MethodFixture{func=x1083, ty=x1084, readOnly=b1085, override=b1086, 
          final=b1087}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1083), ("ty", cvtTYPE_EXPR x1084), ("readOnly", PrettyRep.Bool b1085), 
          ("override", PrettyRep.Bool b1086), ("final", PrettyRep.Bool b1087)]))
     | cvtFIXTURE (ValFixture{ty=x1101, readOnly=b1102}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1101), ("readOnly", PrettyRep.Bool b1102)]))
     | cvtFIXTURE (VirtualValFixture x1110) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1110))
   and cvtBINDINGS (ls1114, ls1119) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1113 => 
                                                                                       cvtBINDING x1113
                                                                                ) ls1114), 
          PrettyRep.List (List.map (fn x1118 => cvtINIT_STEP x1118
                                   ) ls1119)]
   and cvtFIXTURES ls1127 = PrettyRep.List (List.map (fn (x1124, x1125) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1124, 
                                                            cvtFIXTURE x1125]
                                                     ) ls1127)
   and cvtINITS ls1134 = PrettyRep.List (List.map (fn (x1131, x1132) => PrettyRep.Tuple [cvtFIXTURE_NAME x1131, 
                                                         cvtEXPR x1132]
                                                  ) ls1134)
   and cvtHEAD (x1138, x1139) = PrettyRep.Tuple [cvtFIXTURES x1138, cvtINITS x1139]
   and cvtFIELD {kind=x1141, name=x1142, init=x1143} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1141), ("name", cvtIDENT_EXPR x1142), ("init", cvtEXPR x1143)]
   and cvtFIELD_TYPE {name=x1151, ty=x1152} = PrettyRep.Rec [("name", cvtIDENT x1151), 
          ("ty", cvtTYPE_EXPR x1152)]
   and cvtTYPED_IDENT {name=x1158, ty=opt1160} = PrettyRep.Rec [("name", cvtIDENT x1158), 
          ("ty", 
       (case opt1160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1159 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1159))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1170, params=ls1175, result=x1179, thisType=opt1181, 
          hasRest=b1185, minArgs=n1186} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1169 => 
                                                                                                        cvtIDENT x1169
                                                                                                 ) ls1170)), 
          ("params", PrettyRep.List (List.map (fn x1174 => cvtTYPE_EXPR x1174
                                              ) ls1175)), ("result", cvtTYPE_EXPR x1179), 
          ("thisType", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1180))
       )), ("hasRest", PrettyRep.Bool b1185), ("minArgs", PrettyRep.Int n1186)]
   and cvtFUNC_DEFN {kind=x1200, ns=opt1202, final=b1206, override=b1207, prototype=b1208, 
          static=b1209, func=x1210} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1200), 
          ("ns", 
       (case opt1202 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1201 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1201))
       )), ("final", PrettyRep.Bool b1206), ("override", PrettyRep.Bool b1207), 
          ("prototype", PrettyRep.Bool b1208), ("static", PrettyRep.Bool b1209), 
          ("func", cvtFUNC x1210)]
   and cvtCTOR_DEFN x1226 = cvtCTOR x1226
   and cvtVAR_DEFN {kind=x1227, ns=opt1229, static=b1233, prototype=b1234, 
          bindings=x1235} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1227), 
          ("ns", 
       (case opt1229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1228 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1228))
       )), ("static", PrettyRep.Bool b1233), ("prototype", PrettyRep.Bool b1234), 
          ("bindings", cvtBINDINGS x1235)]
   and cvtNAMESPACE_DEFN {ident=x1247, ns=opt1249, init=opt1254} = PrettyRep.Rec [("ident", 
          cvtIDENT x1247), ("ns", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1248))
       )), ("init", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       ))]
   and cvtCLASS_DEFN {ident=x1265, ns=opt1267, nonnullable=b1271, dynamic=b1272, 
          final=b1273, params=ls1275, extends=opt1280, implements=ls1285, classDefns=ls1290, 
          instanceDefns=ls1295, instanceStmts=ls1300, ctorDefn=opt1305} = PrettyRep.Rec [("ident", 
          cvtIDENT x1265), ("ns", 
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1266))
       )), ("nonnullable", PrettyRep.Bool b1271), ("dynamic", PrettyRep.Bool b1272), 
          ("final", PrettyRep.Bool b1273), ("params", PrettyRep.List (List.map (fn x1274 => 
                                                                                      cvtIDENT x1274
                                                                               ) ls1275)), 
          ("extends", 
       (case opt1280 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1279 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1279))
       )), ("implements", PrettyRep.List (List.map (fn x1284 => cvtIDENT_EXPR x1284
                                                   ) ls1285)), ("classDefns", 
          PrettyRep.List (List.map (fn x1289 => cvtDEFN x1289
                                   ) ls1290)), ("instanceDefns", PrettyRep.List (List.map (fn x1294 => 
                                                                                                 cvtDEFN x1294
                                                                                          ) ls1295)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1299 => cvtSTMT x1299
                                                     ) ls1300)), ("ctorDefn", 
          
       (case opt1305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1304 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1304))
       ))]
   and cvtINTERFACE_DEFN {ident=x1334, ns=opt1336, nonnullable=b1340, params=ls1342, 
          extends=ls1347, block=x1351} = PrettyRep.Rec [("ident", cvtIDENT x1334), 
          ("ns", 
       (case opt1336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1335 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1335))
       )), ("nonnullable", PrettyRep.Bool b1340), ("params", PrettyRep.List (List.map (fn x1341 => 
                                                                                             cvtIDENT x1341
                                                                                      ) ls1342)), 
          ("extends", PrettyRep.List (List.map (fn x1346 => cvtIDENT_EXPR x1346
                                               ) ls1347)), ("block", cvtBLOCK x1351)]
   and cvtTYPE_DEFN {ident=x1365, ns=opt1367, init=x1371} = PrettyRep.Rec [("ident", 
          cvtIDENT x1365), ("ns", 
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1366))
       )), ("init", cvtTYPE_EXPR x1371)]
   and cvtFOR_ENUM_STMT {defn=opt1380, obj=x1384, fixtures=opt1386, inits=opt1391, 
          labels=ls1396, body=x1400} = PrettyRep.Rec [("defn", 
       (case opt1380 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1379 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1379))
       )), ("obj", cvtEXPR x1384), ("fixtures", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1385))
       )), ("inits", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1390))
       )), ("labels", PrettyRep.List (List.map (fn x1395 => cvtIDENT x1395
                                               ) ls1396)), ("body", cvtSTMT x1400)]
   and cvtFOR_STMT {fixtures=opt1415, defn=opt1420, init=x1424, cond=x1425, 
          update=x1426, labels=ls1428, body=x1432} = PrettyRep.Rec [("fixtures", 
          
       (case opt1415 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1414 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1414))
       )), ("defn", 
       (case opt1420 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1419 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1419))
       )), ("init", cvtSTMT x1424), ("cond", cvtEXPR x1425), ("update", cvtEXPR x1426), 
          ("labels", PrettyRep.List (List.map (fn x1427 => cvtIDENT x1427
                                              ) ls1428)), ("body", cvtSTMT x1432)]
   and cvtWHILE_STMT {cond=x1448, fixtures=opt1450, body=x1454, labels=ls1456} = 
          PrettyRep.Rec [("cond", cvtEXPR x1448), ("fixtures", 
       (case opt1450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1449 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1449))
       )), ("body", cvtSTMT x1454), ("labels", PrettyRep.List (List.map (fn x1455 => 
                                                                               cvtIDENT x1455
                                                                        ) ls1456))]
   and cvtDIRECTIVES {pragmas=ls1470, defns=ls1475, head=opt1480, body=ls1485} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1469 => 
                                                                     cvtPRAGMA x1469
                                                              ) ls1470)), ("defns", 
          PrettyRep.List (List.map (fn x1474 => cvtDEFN x1474
                                   ) ls1475)), ("head", 
       (case opt1480 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1479 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1479))
       )), ("body", PrettyRep.List (List.map (fn x1484 => cvtSTMT x1484
                                             ) ls1485))]
   and cvtCASE {label=opt1499, inits=opt1504, body=x1508} = PrettyRep.Rec [("label", 
          
       (case opt1499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1498 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1498))
       )), ("inits", 
       (case opt1504 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1503 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1503))
       )), ("body", cvtBLOCK x1508)]
   and cvtTYPE_CASE {ty=opt1517, bindings=x1521, inits=opt1523, body=x1527} = 
          PrettyRep.Rec [("ty", 
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1516 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1516))
       )), ("bindings", cvtBINDINGS x1521), ("inits", 
       (case opt1523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1522 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1522))
       )), ("body", cvtBLOCK x1527)]
   and cvtFUNC_NAME {kind=x1537, ident=x1538} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1537), 
          ("ident", cvtIDENT x1538)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1544, getter=opt1546, setter=opt1551} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1544), ("getter", 
       (case opt1546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1545 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1545))
       )), ("setter", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1550))
       ))]
   and cvtPACKAGE {name=ls1563, block=x1567} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1562 => 
                                                                                                       cvtIDENT x1562
                                                                                                ) ls1563)), 
          ("block", cvtBLOCK x1567)]
   and cvtPROGRAM {packages=ls1574, fixtures=opt1579, block=x1583} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1573 => cvtPACKAGE x1573
                                   ) ls1574)), ("fixtures", 
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1578 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1578))
       )), ("block", cvtBLOCK x1583)]
end

