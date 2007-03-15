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
     | cvtPRAGMA (Import{package=x279, name=x280, alias=opt282}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x279), ("name", cvtIDENT x280), 
          ("alias", 
       (case opt282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x281 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x281))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
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
     | cvtTYPE_EXPR (InstanceType{name=x546, typeParams=ls548, ty=x552}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x546), 
          ("typeParams", PrettyRep.List (List.map (fn x547 => cvtIDENT x547
                                                  ) ls548)), ("ty", cvtTYPE_EXPR x552)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x563) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x563))
     | cvtSTMT (InitStmt{kind=x566, ns=opt568, prototype=b572, static=b573, 
          temps=x574, inits=ls576}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x566), ("ns", 
       (case opt568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x567 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x567))
       )), ("prototype", PrettyRep.Bool b572), ("static", PrettyRep.Bool b573), 
          ("temps", cvtBINDINGS x574), ("inits", PrettyRep.List (List.map (fn x575 => 
                                                                                 cvtINIT_STEP x575
                                                                          ) ls576))]))
     | cvtSTMT (ClassBlock{ns=opt596, ident=x600, name=opt602, block=x606}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x595 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x595))
       )), ("ident", cvtIDENT x600), ("name", 
       (case opt602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x601))
       )), ("block", cvtBLOCK x606)]))
     | cvtSTMT (PackageBlock{name=x618, block=x619}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x618), ("block", cvtBLOCK x619)]))
     | cvtSTMT (ForEachStmt x627) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x627))
     | cvtSTMT (ForInStmt x630) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x630))
     | cvtSTMT (ThrowStmt x633) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x633))
     | cvtSTMT (ReturnStmt x636) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x636))
     | cvtSTMT (BreakStmt opt640) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x639 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x639))
       ))
     | cvtSTMT (ContinueStmt opt647) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x646 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x646))
       ))
     | cvtSTMT (BlockStmt x653) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x653))
     | cvtSTMT (LabeledStmt(x656, x657)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x656, 
          cvtSTMT x657]))
     | cvtSTMT (LetStmt x661) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x661))
     | cvtSTMT (WhileStmt x664) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x664))
     | cvtSTMT (DoWhileStmt x667) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x667))
     | cvtSTMT (ForStmt x670) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x670))
     | cvtSTMT (IfStmt{cnd=x673, thn=x674, els=x675}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x673), ("thn", cvtSTMT x674), 
          ("els", cvtSTMT x675)]))
     | cvtSTMT (WithStmt{obj=x685, ty=x686, body=x687}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x685), ("ty", cvtTYPE_EXPR x686), 
          ("body", cvtSTMT x687)]))
     | cvtSTMT (TryStmt{block=x697, catches=ls719, finally=opt724}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x697), ("catches", PrettyRep.List (List.map (fn {bindings=x698, 
                                                                                                     ty=opt700, 
                                                                                                     fixtures=opt705, 
                                                                                                     block=x709} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x698), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt700 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x699 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x699))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt705 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x704 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x704))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x709)]
                                                                                              ) ls719)), 
          ("finally", 
       (case opt724 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x723 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x723))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x737, cases=ls739}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x737), ("cases", PrettyRep.List (List.map (fn x738 => 
                                                                                                 cvtCASE x738
                                                                                          ) ls739))]))
     | cvtSTMT (SwitchTypeStmt{cond=x750, ty=x751, cases=ls753}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x750), ("ty", cvtTYPE_EXPR x751), 
          ("cases", PrettyRep.List (List.map (fn x752 => cvtTYPE_CASE x752
                                             ) ls753))]))
     | cvtSTMT (Dxns{expr=x766}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x766)]))
   and cvtEXPR (TrinaryExpr(x772, x773, x774, x775)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x772, cvtEXPR x773, cvtEXPR x774, 
          cvtEXPR x775]))
     | cvtEXPR (BinaryExpr(x779, x780, x781)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x779, cvtEXPR x780, cvtEXPR x781]))
     | cvtEXPR (BinaryTypeExpr(x785, x786, x787)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x785, cvtEXPR x786, cvtTYPE_EXPR x787]))
     | cvtEXPR (UnaryExpr(x791, x792)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x791, 
          cvtEXPR x792]))
     | cvtEXPR (TypeExpr x796) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x796))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt801) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt801 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x800 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x800))
       ))
     | cvtEXPR (SuperExpr opt808) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt808 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x807 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x807))
       ))
     | cvtEXPR (LiteralExpr x814) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x814))
     | cvtEXPR (CallExpr{func=x817, actuals=ls819}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x817), ("actuals", PrettyRep.List (List.map (fn x818 => 
                                                                                                   cvtEXPR x818
                                                                                            ) ls819))]))
     | cvtEXPR (ApplyTypeExpr{expr=x830, actuals=ls832}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x830), ("actuals", PrettyRep.List (List.map (fn x831 => 
                                                                                                   cvtTYPE_EXPR x831
                                                                                            ) ls832))]))
     | cvtEXPR (LetExpr{defs=x843, body=x844, head=opt846}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x843), ("body", cvtEXPR x844), 
          ("head", 
       (case opt846 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x845 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x845))
       ))]))
     | cvtEXPR (NewExpr{obj=x859, actuals=ls861}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x859), ("actuals", PrettyRep.List (List.map (fn x860 => 
                                                                                                  cvtEXPR x860
                                                                                           ) ls861))]))
     | cvtEXPR (ObjectRef{base=x872, ident=x873}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x872), ("ident", cvtIDENT_EXPR x873)]))
     | cvtEXPR (LexicalRef{ident=x881}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x881)]))
     | cvtEXPR (SetExpr(x887, x888, x889)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x887, 
          cvtEXPR x888, cvtEXPR x889]))
     | cvtEXPR (ListExpr ls894) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x893 => 
                                                                                                    cvtEXPR x893
                                                                                             ) ls894)))
     | cvtEXPR (InitExpr(x900, x901, x902)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x900, 
          cvtHEAD x901, cvtINITS x902]))
     | cvtEXPR (SliceExpr(x906, x907, x908)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x906, cvtEXPR x907, cvtEXPR x908]))
     | cvtEXPR (GetTemp n912) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n912))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n918) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n918))
     | cvtFIXTURE_NAME (PropName x921) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x921))
   and cvtIDENT_EXPR (Identifier{ident=x924, openNamespaces=ls930}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x924), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls926 => PrettyRep.List (List.map (fn x925 => 
                                                                                cvtNAMESPACE x925
                                                                         ) ls926)
                                   ) ls930))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x941, expr=x942}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x941), ("expr", cvtEXPR x942)]))
     | cvtIDENT_EXPR (AttributeIdentifier x950) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x950))
     | cvtIDENT_EXPR (ExpressionIdentifier x953) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x953))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x956, ident=x957}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x956), ("ident", cvtUSTRING x957)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x965, typeParams=ls967}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x965), ("typeParams", 
          PrettyRep.List (List.map (fn x966 => cvtTYPE_EXPR x966
                                   ) ls967))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s980) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s980))
     | cvtLITERAL (LiteralContextualDecimalInteger s983) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s983))
     | cvtLITERAL (LiteralContextualHexInteger s986) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s986))
     | cvtLITERAL (LiteralDouble r989) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r989))
     | cvtLITERAL (LiteralDecimal d992) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d992))
     | cvtLITERAL (LiteralInt i995) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i995))
     | cvtLITERAL (LiteralUInt u998) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u998))
     | cvtLITERAL (LiteralBoolean b1001) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1001))
     | cvtLITERAL (LiteralString x1004) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1004))
     | cvtLITERAL (LiteralArray{exprs=ls1008, ty=opt1013}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1007 => 
                                                                         cvtEXPR x1007
                                                                  ) ls1008)), 
          ("ty", 
       (case opt1013 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1012 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1012))
       ))]))
     | cvtLITERAL (LiteralXML ls1025) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1024 => 
                                                                                                            cvtEXPR x1024
                                                                                                     ) ls1025)))
     | cvtLITERAL (LiteralNamespace x1031) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1031))
     | cvtLITERAL (LiteralObject{expr=ls1035, ty=opt1040}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1034 => 
                                                                        cvtFIELD x1034
                                                                 ) ls1035)), 
          ("ty", 
       (case opt1040 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1039 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1039))
       ))]))
     | cvtLITERAL (LiteralFunction x1051) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1051))
     | cvtLITERAL (LiteralRegExp{str=x1054}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1054)]))
   and cvtBLOCK (Block x1060) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1060))
   and cvtFIXTURE (NamespaceFixture x1063) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1063))
     | cvtFIXTURE (ClassFixture x1066) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1066))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1070) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1070))
     | cvtFIXTURE (MethodFixture{func=x1073, ty=x1074, readOnly=b1075, override=b1076, 
          final=b1077}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1073), ("ty", cvtTYPE_EXPR x1074), ("readOnly", PrettyRep.Bool b1075), 
          ("override", PrettyRep.Bool b1076), ("final", PrettyRep.Bool b1077)]))
     | cvtFIXTURE (ValFixture{ty=x1091, readOnly=b1092}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1091), ("readOnly", PrettyRep.Bool b1092)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1100, getter=opt1102, setter=opt1107}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1100), ("getter", 
       (case opt1102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1101 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1101))
       )), ("setter", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1106))
       ))]))
   and cvtBINDINGS (ls1121, ls1126) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1120 => 
                                                                                       cvtBINDING x1120
                                                                                ) ls1121), 
          PrettyRep.List (List.map (fn x1125 => cvtINIT_STEP x1125
                                   ) ls1126)]
   and cvtFIXTURES ls1134 = PrettyRep.List (List.map (fn (x1131, x1132) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1131, 
                                                            cvtFIXTURE x1132]
                                                     ) ls1134)
   and cvtINITS ls1141 = PrettyRep.List (List.map (fn (x1138, x1139) => PrettyRep.Tuple [cvtFIXTURE_NAME x1138, 
                                                         cvtEXPR x1139]
                                                  ) ls1141)
   and cvtHEAD (x1145, x1146) = PrettyRep.Tuple [cvtFIXTURES x1145, cvtINITS x1146]
   and cvtFIELD {kind=x1148, name=x1149, init=x1150} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1148), ("name", cvtIDENT_EXPR x1149), ("init", cvtEXPR x1150)]
   and cvtFIELD_TYPE {name=x1158, ty=x1159} = PrettyRep.Rec [("name", cvtIDENT x1158), 
          ("ty", cvtTYPE_EXPR x1159)]
   and cvtTYPED_IDENT {name=x1165, ty=opt1167} = PrettyRep.Rec [("name", cvtIDENT x1165), 
          ("ty", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1166))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1177, params=ls1182, result=x1186, thisType=opt1188, 
          hasRest=b1192, minArgs=n1193} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1176 => 
                                                                                                        cvtIDENT x1176
                                                                                                 ) ls1177)), 
          ("params", PrettyRep.List (List.map (fn x1181 => cvtTYPE_EXPR x1181
                                              ) ls1182)), ("result", cvtTYPE_EXPR x1186), 
          ("thisType", 
       (case opt1188 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1187 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1187))
       )), ("hasRest", PrettyRep.Bool b1192), ("minArgs", PrettyRep.Int n1193)]
   and cvtFUNC_DEFN {kind=x1207, ns=opt1209, final=b1213, override=b1214, prototype=b1215, 
          static=b1216, func=x1217} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1207), 
          ("ns", 
       (case opt1209 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1208 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1208))
       )), ("final", PrettyRep.Bool b1213), ("override", PrettyRep.Bool b1214), 
          ("prototype", PrettyRep.Bool b1215), ("static", PrettyRep.Bool b1216), 
          ("func", cvtFUNC x1217)]
   and cvtCTOR_DEFN x1233 = cvtCTOR x1233
   and cvtVAR_DEFN {kind=x1234, ns=opt1236, static=b1240, prototype=b1241, 
          bindings=x1242} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1234), 
          ("ns", 
       (case opt1236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1235 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1235))
       )), ("static", PrettyRep.Bool b1240), ("prototype", PrettyRep.Bool b1241), 
          ("bindings", cvtBINDINGS x1242)]
   and cvtNAMESPACE_DEFN {ident=x1254, ns=opt1256, init=opt1261} = PrettyRep.Rec [("ident", 
          cvtIDENT x1254), ("ns", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1255))
       )), ("init", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1260))
       ))]
   and cvtCLASS_DEFN {ident=x1272, ns=opt1274, nonnullable=b1278, dynamic=b1279, 
          final=b1280, params=ls1282, extends=opt1287, implements=ls1292, classDefns=ls1297, 
          instanceDefns=ls1302, instanceStmts=ls1307, ctorDefn=opt1312} = PrettyRep.Rec [("ident", 
          cvtIDENT x1272), ("ns", 
       (case opt1274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1273 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1273))
       )), ("nonnullable", PrettyRep.Bool b1278), ("dynamic", PrettyRep.Bool b1279), 
          ("final", PrettyRep.Bool b1280), ("params", PrettyRep.List (List.map (fn x1281 => 
                                                                                      cvtIDENT x1281
                                                                               ) ls1282)), 
          ("extends", 
       (case opt1287 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1286 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1286))
       )), ("implements", PrettyRep.List (List.map (fn x1291 => cvtIDENT_EXPR x1291
                                                   ) ls1292)), ("classDefns", 
          PrettyRep.List (List.map (fn x1296 => cvtDEFN x1296
                                   ) ls1297)), ("instanceDefns", PrettyRep.List (List.map (fn x1301 => 
                                                                                                 cvtDEFN x1301
                                                                                          ) ls1302)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1306 => cvtSTMT x1306
                                                     ) ls1307)), ("ctorDefn", 
          
       (case opt1312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1311 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1311))
       ))]
   and cvtINTERFACE_DEFN {ident=x1341, ns=opt1343, nonnullable=b1347, params=ls1349, 
          extends=ls1354, block=x1358} = PrettyRep.Rec [("ident", cvtIDENT x1341), 
          ("ns", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1342))
       )), ("nonnullable", PrettyRep.Bool b1347), ("params", PrettyRep.List (List.map (fn x1348 => 
                                                                                             cvtIDENT x1348
                                                                                      ) ls1349)), 
          ("extends", PrettyRep.List (List.map (fn x1353 => cvtIDENT_EXPR x1353
                                               ) ls1354)), ("block", cvtBLOCK x1358)]
   and cvtTYPE_DEFN {ident=x1372, ns=opt1374, init=x1378} = PrettyRep.Rec [("ident", 
          cvtIDENT x1372), ("ns", 
       (case opt1374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1373 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1373))
       )), ("init", cvtTYPE_EXPR x1378)]
   and cvtFOR_ENUM_STMT {defn=opt1387, obj=x1391, fixtures=opt1393, inits=opt1398, 
          labels=ls1403, body=x1407} = PrettyRep.Rec [("defn", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1386 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1386))
       )), ("obj", cvtEXPR x1391), ("fixtures", 
       (case opt1393 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1392 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1392))
       )), ("inits", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1397))
       )), ("labels", PrettyRep.List (List.map (fn x1402 => cvtIDENT x1402
                                               ) ls1403)), ("body", cvtSTMT x1407)]
   and cvtFOR_STMT {fixtures=opt1422, defn=opt1427, init=x1431, cond=x1432, 
          update=x1433, labels=ls1435, body=x1439} = PrettyRep.Rec [("fixtures", 
          
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1421))
       )), ("defn", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1426))
       )), ("init", cvtSTMT x1431), ("cond", cvtEXPR x1432), ("update", cvtEXPR x1433), 
          ("labels", PrettyRep.List (List.map (fn x1434 => cvtIDENT x1434
                                              ) ls1435)), ("body", cvtSTMT x1439)]
   and cvtWHILE_STMT {cond=x1455, fixtures=opt1457, body=x1461, labels=ls1463} = 
          PrettyRep.Rec [("cond", cvtEXPR x1455), ("fixtures", 
       (case opt1457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1456 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1456))
       )), ("body", cvtSTMT x1461), ("labels", PrettyRep.List (List.map (fn x1462 => 
                                                                               cvtIDENT x1462
                                                                        ) ls1463))]
   and cvtDIRECTIVES {pragmas=ls1477, defns=ls1482, head=opt1487, body=ls1492} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1476 => 
                                                                     cvtPRAGMA x1476
                                                              ) ls1477)), ("defns", 
          PrettyRep.List (List.map (fn x1481 => cvtDEFN x1481
                                   ) ls1482)), ("head", 
       (case opt1487 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1486 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1486))
       )), ("body", PrettyRep.List (List.map (fn x1491 => cvtSTMT x1491
                                             ) ls1492))]
   and cvtCASE {label=opt1506, inits=opt1511, body=x1515} = PrettyRep.Rec [("label", 
          
       (case opt1506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1505 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1505))
       )), ("inits", 
       (case opt1511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1510 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1510))
       )), ("body", cvtBLOCK x1515)]
   and cvtTYPE_CASE {ty=opt1524, bindings=x1528, inits=opt1530, body=x1534} = 
          PrettyRep.Rec [("ty", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1523))
       )), ("bindings", cvtBINDINGS x1528), ("inits", 
       (case opt1530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1529 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1529))
       )), ("body", cvtBLOCK x1534)]
   and cvtFUNC_NAME {kind=x1544, ident=x1545} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1544), 
          ("ident", cvtIDENT x1545)]
   and cvtPACKAGE {name=x1551, block=x1552} = PrettyRep.Rec [("name", cvtUSTRING x1551), 
          ("block", cvtBLOCK x1552)]
   and cvtPROGRAM {packages=ls1559, fixtures=opt1564, block=x1568} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1558 => cvtPACKAGE x1558
                                   ) ls1559)), ("fixtures", 
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1563))
       )), ("block", cvtBLOCK x1568)]
end

