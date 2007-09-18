structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtUNIT_NAME ls21 = PrettyRep.List (List.map (fn x20 => cvtIDENT x20
                                                    ) ls21)
   and cvtFRAME_ID n25 = PrettyRep.Int n25
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x28) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Protected x31) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x31))
     | cvtNAMESPACE (Public x34) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x34))
     | cvtNAMESPACE (Internal x37) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x37))
     | cvtNAMESPACE (UserNamespace s40) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s40))
     | cvtNAMESPACE (AnonUserNamespace n43) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n43))
     | cvtNAMESPACE (LimitedNamespace(x46, x47)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x46, cvtNAMESPACE x47]))
   and cvtNAME {ns=x51, id=x52} = PrettyRep.Rec [("ns", cvtNAMESPACE x51), 
          ("id", cvtIDENT x52)]
   and cvtMULTINAME {nss=ls63, id=x67} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls59 => 
                                                                                                PrettyRep.List (List.map (fn x58 => 
                                                                                                                                cvtNAMESPACE x58
                                                                                                                         ) ls59)
                                                                                         ) ls63)), 
          ("id", cvtIDENT x67)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x78, roundingMode=r79, precision=n80} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x78), ("roundingMode", 
          PrettyRep.DecRm r79), ("precision", PrettyRep.Int n80)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt92) = PrettyRep.Ctor ("Plus", SOME 
       (case opt92 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x91 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x91))
       ))
     | cvtBINOP (Minus opt99) = PrettyRep.Ctor ("Minus", SOME 
       (case opt99 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x98 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x98))
       ))
     | cvtBINOP (Times opt106) = PrettyRep.Ctor ("Times", SOME 
       (case opt106 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x105 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x105))
       ))
     | cvtBINOP (Divide opt113) = PrettyRep.Ctor ("Divide", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (Remainder opt120) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
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
     | cvtBINOP (Equals opt137) = PrettyRep.Ctor ("Equals", SOME 
       (case opt137 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x136 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x136))
       ))
     | cvtBINOP (NotEquals opt144) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt144 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x143 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x143))
       ))
     | cvtBINOP (StrictEquals opt151) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x150 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x150))
       ))
     | cvtBINOP (StrictNotEquals opt158) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x157 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x157))
       ))
     | cvtBINOP (Less opt165) = PrettyRep.Ctor ("Less", SOME 
       (case opt165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x164 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x164))
       ))
     | cvtBINOP (LessOrEqual opt172) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x171 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x171))
       ))
     | cvtBINOP (Greater opt179) = PrettyRep.Ctor ("Greater", SOME 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x178))
       ))
     | cvtBINOP (GreaterOrEqual opt186) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x185 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x185))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt195) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt195 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x194 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x194))
       ))
     | cvtASSIGNOP (AssignMinus opt202) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt202 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x201 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x201))
       ))
     | cvtASSIGNOP (AssignTimes opt209) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt209 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x208 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x208))
       ))
     | cvtASSIGNOP (AssignDivide opt216) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x215 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x215))
       ))
     | cvtASSIGNOP (AssignRemainder opt223) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x222 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x222))
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
     | cvtUNOP (PreIncrement opt241) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x240 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x240))
       ))
     | cvtUNOP (PreDecrement opt248) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x247 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x247))
       ))
     | cvtUNOP (PostIncrement opt255) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x254 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x254))
       ))
     | cvtUNOP (PostDecrement opt262) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt262 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x261 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x261))
       ))
     | cvtUNOP (UnaryPlus opt269) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x268 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x268))
       ))
     | cvtUNOP (UnaryMinus opt276) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x275 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x275))
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
   and cvtPRAGMA (UseNamespace x293) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x293))
     | cvtPRAGMA (UseDefaultNamespace x296) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x296))
     | cvtPRAGMA (UseNumber x299) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x299))
     | cvtPRAGMA (UseRounding r302) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r302))
     | cvtPRAGMA (UsePrecision n305) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n305))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls311, name=x315, alias=opt317}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x310 => 
                                                                           cvtIDENT x310
                                                                    ) ls311)), 
          ("name", cvtIDENT x315), ("alias", 
       (case opt317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x316 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x316))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x336, frameId=opt338, topUnit=opt343}) = PrettyRep.Ctor ("Ty", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x336), ("frameId", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtFRAME_ID x337))
       )), ("topUnit", 
       (case opt343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x342 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x342))
       ))]))
   and cvtCLS (Cls{name=x356, typeParams=ls358, nonnullable=b362, dynamic=b363, 
          extends=opt365, implements=ls370, classRib=x374, instanceRib=x375, 
          instanceInits=x376, constructor=opt378, classType=x382, instanceType=x383}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x356), 
          ("typeParams", PrettyRep.List (List.map (fn x357 => cvtIDENT x357
                                                  ) ls358)), ("nonnullable", 
          PrettyRep.Bool b362), ("dynamic", PrettyRep.Bool b363), ("extends", 
          
       (case opt365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x364 => PrettyRep.Ctor ("SOME", SOME (cvtTY x364))
       )), ("implements", PrettyRep.List (List.map (fn x369 => cvtTY x369
                                                   ) ls370)), ("classRib", 
          cvtRIB x374), ("instanceRib", cvtRIB x375), ("instanceInits", cvtHEAD x376), 
          ("constructor", 
       (case opt378 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x377 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x377))
       )), ("classType", cvtTY x382), ("instanceType", cvtTY x383)]))
   and cvtIFACE (Iface{name=x411, typeParams=ls413, nonnullable=b417, extends=ls419, 
          instanceRib=x423, instanceType=x424}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x411), ("typeParams", PrettyRep.List (List.map (fn x412 => 
                                                                                                      cvtIDENT x412
                                                                                               ) ls413)), 
          ("nonnullable", PrettyRep.Bool b417), ("extends", PrettyRep.List (List.map (fn x418 => 
                                                                                            cvtTY x418
                                                                                     ) ls419)), 
          ("instanceRib", cvtRIB x423), ("instanceType", cvtTY x424)]))
   and cvtCTOR (Ctor{settings=x440, superArgs=ls442, func=x446}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x440), ("superArgs", PrettyRep.List (List.map (fn x441 => 
                                                                                                         cvtEXPR x441
                                                                                                  ) ls442)), 
          ("func", cvtFUNC x446)]))
   and cvtFUNC (Func{name=x456, fsig=x457, native=b458, block=opt460, param=x464, 
          defaults=ls466, ty=x470, loc=opt472}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x456), ("fsig", cvtFUNC_SIG x457), ("native", PrettyRep.Bool b458), 
          ("block", 
       (case opt460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x459 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x459))
       )), ("param", cvtHEAD x464), ("defaults", PrettyRep.List (List.map (fn x465 => 
                                                                                 cvtEXPR x465
                                                                          ) ls466)), 
          ("ty", cvtTY x470), ("loc", 
       (case opt472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x471 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x471))
       ))]))
   and cvtDEFN (ClassDefn x495) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x495))
     | cvtDEFN (VariableDefn x498) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x498))
     | cvtDEFN (FunctionDefn x501) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x501))
     | cvtDEFN (ConstructorDefn x504) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x504))
     | cvtDEFN (InterfaceDefn x507) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x507))
     | cvtDEFN (NamespaceDefn x510) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x510))
     | cvtDEFN (TypeDefn x513) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x513))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls517, params=x521, paramTypes=ls523, 
          defaults=ls528, ctorInits=opt539, returnType=x543, thisType=opt545, 
          hasRest=b549}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x516 => cvtIDENT x516
                                   ) ls517)), ("params", cvtBINDINGS x521), 
          ("paramTypes", PrettyRep.List (List.map (fn x522 => cvtTYPE_EXPR x522
                                                  ) ls523)), ("defaults", PrettyRep.List (List.map (fn x527 => 
                                                                                                          cvtEXPR x527
                                                                                                   ) ls528)), 
          ("ctorInits", 
       (case opt539 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x532, ls534) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x532, 
            PrettyRep.List (List.map (fn x533 => cvtEXPR x533
                                     ) ls534)]))
       )), ("returnType", cvtTYPE_EXPR x543), ("thisType", 
       (case opt545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x544 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x544))
       )), ("hasRest", PrettyRep.Bool b549)]))
   and cvtBINDING (Binding{ident=x569, ty=x570}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x569), ("ty", cvtTYPE_EXPR x570)]))
   and cvtBINDING_IDENT (TempIdent n578) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n578))
     | cvtBINDING_IDENT (ParamIdent n581) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n581))
     | cvtBINDING_IDENT (PropIdent x584) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x584))
   and cvtINIT_STEP (InitStep(x587, x588)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x587, 
          cvtEXPR x588]))
     | cvtINIT_STEP (AssignStep(x592, x593)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x592, cvtEXPR x593]))
   and cvtTYPE_EXPR (SpecialType x597) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x597))
     | cvtTYPE_EXPR (UnionType ls601) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x600 => 
                                                                                                           cvtTYPE_EXPR x600
                                                                                                    ) ls601)))
     | cvtTYPE_EXPR (ArrayType ls608) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x607 => 
                                                                                                           cvtTYPE_EXPR x607
                                                                                                    ) ls608)))
     | cvtTYPE_EXPR (TypeName x614) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x614))
     | cvtTYPE_EXPR (ElementTypeRef(x617, n618)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x617, PrettyRep.Int n618]))
     | cvtTYPE_EXPR (FieldTypeRef(x622, x623)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x622, cvtIDENT x623]))
     | cvtTYPE_EXPR (FunctionType x627) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x627))
     | cvtTYPE_EXPR (ObjectType ls631) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x630 => 
                                                                                                             cvtFIELD_TYPE x630
                                                                                                      ) ls631)))
     | cvtTYPE_EXPR (AppType{base=x637, args=ls639}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x637), ("args", PrettyRep.List (List.map (fn x638 => 
                                                                                                     cvtTYPE_EXPR x638
                                                                                              ) ls639))]))
     | cvtTYPE_EXPR (LamType{params=ls651, body=x655}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x650 => 
                                                                          cvtIDENT x650
                                                                   ) ls651)), 
          ("body", cvtTYPE_EXPR x655)]))
     | cvtTYPE_EXPR (NullableType{expr=x663, nullable=b664}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x663), ("nullable", PrettyRep.Bool b664)]))
     | cvtTYPE_EXPR (InstanceType x672) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x672))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x676) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x676))
     | cvtSTMT (InitStmt{kind=x679, ns=opt681, prototype=b685, static=b686, 
          temps=x687, inits=ls689}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x679), ("ns", 
       (case opt681 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x680 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x680))
       )), ("prototype", PrettyRep.Bool b685), ("static", PrettyRep.Bool b686), 
          ("temps", cvtBINDINGS x687), ("inits", PrettyRep.List (List.map (fn x688 => 
                                                                                 cvtINIT_STEP x688
                                                                          ) ls689))]))
     | cvtSTMT (ClassBlock{ns=opt709, ident=x713, name=opt715, block=x719}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x708 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x708))
       )), ("ident", cvtIDENT x713), ("name", 
       (case opt715 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x714 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x714))
       )), ("block", cvtBLOCK x719)]))
     | cvtSTMT (ForInStmt x731) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x731))
     | cvtSTMT (ThrowStmt x734) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x734))
     | cvtSTMT (ReturnStmt x737) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x737))
     | cvtSTMT (BreakStmt opt741) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt741 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x740 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x740))
       ))
     | cvtSTMT (ContinueStmt opt748) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt748 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x747 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x747))
       ))
     | cvtSTMT (BlockStmt x754) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x754))
     | cvtSTMT (LabeledStmt(x757, x758)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x757, 
          cvtSTMT x758]))
     | cvtSTMT (LetStmt x762) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x762))
     | cvtSTMT (WhileStmt x765) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x765))
     | cvtSTMT (DoWhileStmt x768) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x768))
     | cvtSTMT (ForStmt x771) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x771))
     | cvtSTMT (IfStmt{cnd=x774, thn=x775, els=x776}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x774), ("thn", cvtSTMT x775), 
          ("els", cvtSTMT x776)]))
     | cvtSTMT (WithStmt{obj=x786, ty=x787, body=x788}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x786), ("ty", cvtTY x787), ("body", 
          cvtSTMT x788)]))
     | cvtSTMT (TryStmt{block=x798, catches=ls800, finally=opt805}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x798), ("catches", PrettyRep.List (List.map (fn x799 => 
                                                                                                     cvtCATCH_CLAUSE x799
                                                                                              ) ls800)), 
          ("finally", 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x804))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt819, cond=x823, labels=ls825, cases=ls830}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt819 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x818 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x818))
       )), ("cond", cvtEXPR x823), ("labels", PrettyRep.List (List.map (fn x824 => 
                                                                              cvtIDENT x824
                                                                       ) ls825)), 
          ("cases", PrettyRep.List (List.map (fn x829 => cvtCASE x829
                                             ) ls830))]))
     | cvtSTMT (SwitchTypeStmt{cond=x845, ty=x846, cases=ls848}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x845), ("ty", cvtTY x846), 
          ("cases", PrettyRep.List (List.map (fn x847 => cvtCATCH_CLAUSE x847
                                             ) ls848))]))
     | cvtSTMT (DXNStmt{expr=x861}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x861)]))
   and cvtEXPR (TernaryExpr(x867, x868, x869)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x867, cvtEXPR x868, cvtEXPR x869]))
     | cvtEXPR (BinaryExpr(x873, x874, x875)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x873, cvtEXPR x874, cvtEXPR x875]))
     | cvtEXPR (BinaryTypeExpr(x879, x880, x881)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x879, cvtEXPR x880, cvtTY x881]))
     | cvtEXPR (ExpectedTypeExpr(x885, x886)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTY x885, cvtEXPR x886]))
     | cvtEXPR (UnaryExpr(x890, x891)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x890, 
          cvtEXPR x891]))
     | cvtEXPR (TypeExpr x895) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x895))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt900) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt900 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x899 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x899))
       ))
     | cvtEXPR (SuperExpr opt907) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt907 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x906 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x906))
       ))
     | cvtEXPR (LiteralExpr x913) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x913))
     | cvtEXPR (CallExpr{func=x916, actuals=ls918}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x916), ("actuals", PrettyRep.List (List.map (fn x917 => 
                                                                                                   cvtEXPR x917
                                                                                            ) ls918))]))
     | cvtEXPR (ApplyTypeExpr{expr=x929, actuals=ls931}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x929), ("actuals", PrettyRep.List (List.map (fn x930 => 
                                                                                                   cvtTY x930
                                                                                            ) ls931))]))
     | cvtEXPR (LetExpr{defs=x942, body=x943, head=opt945}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x942), ("body", cvtEXPR x943), 
          ("head", 
       (case opt945 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x944 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x944))
       ))]))
     | cvtEXPR (NewExpr{obj=x958, actuals=ls960}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x958), ("actuals", PrettyRep.List (List.map (fn x959 => 
                                                                                                  cvtEXPR x959
                                                                                           ) ls960))]))
     | cvtEXPR (ObjectRef{base=x971, ident=x972, loc=opt974}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x971), ("ident", cvtIDENT_EXPR x972), 
          ("loc", 
       (case opt974 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x973 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x973))
       ))]))
     | cvtEXPR (LexicalRef{ident=x987, loc=opt989}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x987), ("loc", 
       (case opt989 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x988 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x988))
       ))]))
     | cvtEXPR (SetExpr(x1000, x1001, x1002)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x1000, cvtEXPR x1001, cvtEXPR x1002]))
     | cvtEXPR (ListExpr ls1007) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x1006 => 
                                                                                                     cvtEXPR x1006
                                                                                              ) ls1007)))
     | cvtEXPR (InitExpr(x1013, x1014, x1015)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x1013, cvtHEAD x1014, cvtINITS x1015]))
     | cvtEXPR (SliceExpr(x1019, x1020, x1021)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x1019, cvtEXPR x1020, cvtEXPR x1021]))
     | cvtEXPR (GetTemp n1025) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n1025))
     | cvtEXPR (GetParam n1028) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n1028))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1034) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1034))
     | cvtFIXTURE_NAME (PropName x1037) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1037))
   and cvtIDENT_EXPR (Identifier{ident=x1040, openNamespaces=ls1046}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1040), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1042 => PrettyRep.List (List.map (fn x1041 => 
                                                                                 cvtNAMESPACE x1041
                                                                          ) ls1042)
                                   ) ls1046))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1057, expr=x1058}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1057), ("expr", cvtEXPR x1058)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1066) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1066))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1069, openNamespaces=ls1075}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1069), ("openNamespaces", PrettyRep.List (List.map (fn ls1071 => 
                                                                             PrettyRep.List (List.map (fn x1070 => 
                                                                                                             cvtNAMESPACE x1070
                                                                                                      ) ls1071)
                                                                      ) ls1075))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1086, ident=s1087}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1086), ("ident", PrettyRep.UniStr s1087)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1095, typeArgs=ls1097}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1095), ("typeArgs", 
          PrettyRep.List (List.map (fn x1096 => cvtTY x1096
                                   ) ls1097))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1109, x1113)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1108 => cvtIDENT x1108
                                                          ) ls1109), cvtIDENT_EXPR x1113]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1120) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1120))
     | cvtLITERAL (LiteralContextualDecimalInteger s1123) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1123))
     | cvtLITERAL (LiteralContextualHexInteger s1126) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1126))
     | cvtLITERAL (LiteralDouble r1129) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1129))
     | cvtLITERAL (LiteralDecimal d1132) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1132))
     | cvtLITERAL (LiteralInt i1135) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1135))
     | cvtLITERAL (LiteralUInt u1138) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1138))
     | cvtLITERAL (LiteralBoolean b1141) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1141))
     | cvtLITERAL (LiteralString s1144) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1144))
     | cvtLITERAL (LiteralArray{exprs=ls1148, ty=opt1153}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1147 => 
                                                                         cvtEXPR x1147
                                                                  ) ls1148)), 
          ("ty", 
       (case opt1153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1152 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1152))
       ))]))
     | cvtLITERAL (LiteralXML ls1165) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1164 => 
                                                                                                            cvtEXPR x1164
                                                                                                     ) ls1165)))
     | cvtLITERAL (LiteralNamespace x1171) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1171))
     | cvtLITERAL (LiteralObject{expr=ls1175, ty=opt1180}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1174 => 
                                                                        cvtFIELD x1174
                                                                 ) ls1175)), 
          ("ty", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1179))
       ))]))
     | cvtLITERAL (LiteralFunction x1191) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1191))
     | cvtLITERAL (LiteralRegExp{str=s1194}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1194)]))
   and cvtBLOCK (Block x1200) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1200))
   and cvtFIXTURE (NamespaceFixture x1203) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1203))
     | cvtFIXTURE (ClassFixture x1206) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1206))
     | cvtFIXTURE (InterfaceFixture x1209) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1209))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1213) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1213))
     | cvtFIXTURE (MethodFixture{func=x1216, ty=x1217, readOnly=b1218, override=b1219, 
          final=b1220}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1216), ("ty", cvtTY x1217), ("readOnly", PrettyRep.Bool b1218), 
          ("override", PrettyRep.Bool b1219), ("final", PrettyRep.Bool b1220)]))
     | cvtFIXTURE (ValFixture{ty=x1234, readOnly=b1235}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1234), ("readOnly", PrettyRep.Bool b1235)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1243, getter=opt1245, setter=opt1250}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1243), ("getter", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1244))
       )), ("setter", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1249))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1263, baseTypeArgs=ls1265}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1263), ("baseTypeArgs", PrettyRep.List (List.map (fn x1264 => 
                                                                           cvtTY x1264
                                                                    ) ls1265))]))
   and cvtHEAD (Head(x1276, x1277)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1276, 
          cvtINITS x1277]))
   and cvtBINDINGS (ls1282, ls1287) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1281 => 
                                                                                       cvtBINDING x1281
                                                                                ) ls1282), 
          PrettyRep.List (List.map (fn x1286 => cvtINIT_STEP x1286
                                   ) ls1287)]
   and cvtRIB ls1295 = PrettyRep.List (List.map (fn (x1292, x1293) => PrettyRep.Tuple [cvtFIXTURE_NAME x1292, 
                                                       cvtFIXTURE x1293]
                                                ) ls1295)
   and cvtRIBS ls1300 = PrettyRep.List (List.map (fn x1299 => cvtRIB x1299
                                                 ) ls1300)
   and cvtINITS ls1307 = PrettyRep.List (List.map (fn (x1304, x1305) => PrettyRep.Tuple [cvtFIXTURE_NAME x1304, 
                                                         cvtEXPR x1305]
                                                  ) ls1307)
   and cvtINSTANCE_TYPE {name=x1311, typeArgs=ls1313, nonnullable=b1317, superTypes=ls1319, 
          ty=x1323, conversionTy=opt1325, dynamic=b1329} = PrettyRep.Rec [("name", 
          cvtNAME x1311), ("typeArgs", PrettyRep.List (List.map (fn x1312 => 
                                                                       cvtTYPE_EXPR x1312
                                                                ) ls1313)), 
          ("nonnullable", PrettyRep.Bool b1317), ("superTypes", PrettyRep.List (List.map (fn x1318 => 
                                                                                                cvtTYPE_EXPR x1318
                                                                                         ) ls1319)), 
          ("ty", cvtTYPE_EXPR x1323), ("conversionTy", 
       (case opt1325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1324 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1324))
       )), ("dynamic", PrettyRep.Bool b1329)]
   and cvtFIELD {kind=x1345, name=x1346, init=x1347} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1345), ("name", cvtIDENT_EXPR x1346), ("init", cvtEXPR x1347)]
   and cvtFIELD_TYPE {name=x1355, ty=x1356} = PrettyRep.Rec [("name", cvtIDENT x1355), 
          ("ty", cvtTYPE_EXPR x1356)]
   and cvtFUNC_TYPE {params=ls1363, result=x1367, thisType=opt1369, hasRest=b1373, 
          minArgs=n1374} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1362 => 
                                                                                     cvtTYPE_EXPR x1362
                                                                              ) ls1363)), 
          ("result", cvtTYPE_EXPR x1367), ("thisType", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1368))
       )), ("hasRest", PrettyRep.Bool b1373), ("minArgs", PrettyRep.Int n1374)]
   and cvtFUNC_DEFN {kind=x1386, ns=opt1388, final=b1392, override=b1393, prototype=b1394, 
          static=b1395, func=x1396} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1386), 
          ("ns", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1387 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1387))
       )), ("final", PrettyRep.Bool b1392), ("override", PrettyRep.Bool b1393), 
          ("prototype", PrettyRep.Bool b1394), ("static", PrettyRep.Bool b1395), 
          ("func", cvtFUNC x1396)]
   and cvtCTOR_DEFN x1412 = cvtCTOR x1412
   and cvtVAR_DEFN {kind=x1413, ns=opt1415, static=b1419, prototype=b1420, 
          bindings=(ls1422, ls1427)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1413), 
          ("ns", 
       (case opt1415 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1414 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1414))
       )), ("static", PrettyRep.Bool b1419), ("prototype", PrettyRep.Bool b1420), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1421 => 
                                                                        cvtBINDING x1421
                                                                 ) ls1422), 
          PrettyRep.List (List.map (fn x1426 => cvtINIT_STEP x1426
                                   ) ls1427)])]
   and cvtNAMESPACE_DEFN {ident=x1443, ns=opt1445, init=opt1450} = PrettyRep.Rec [("ident", 
          cvtIDENT x1443), ("ns", 
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1444))
       )), ("init", 
       (case opt1450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1449 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1449))
       ))]
   and cvtCLASS_DEFN {ns=opt1462, ident=x1466, nonnullable=b1467, dynamic=b1468, 
          final=b1469, params=ls1471, extends=opt1476, implements=ls1481, classDefns=ls1486, 
          instanceDefns=ls1491, instanceStmts=ls1496, ctorDefn=opt1501} = PrettyRep.Rec [("ns", 
          
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1461 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1461))
       )), ("ident", cvtIDENT x1466), ("nonnullable", PrettyRep.Bool b1467), 
          ("dynamic", PrettyRep.Bool b1468), ("final", PrettyRep.Bool b1469), 
          ("params", PrettyRep.List (List.map (fn x1470 => cvtIDENT x1470
                                              ) ls1471)), ("extends", 
       (case opt1476 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1475 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1475))
       )), ("implements", PrettyRep.List (List.map (fn x1480 => cvtTYPE_EXPR x1480
                                                   ) ls1481)), ("classDefns", 
          PrettyRep.List (List.map (fn x1485 => cvtDEFN x1485
                                   ) ls1486)), ("instanceDefns", PrettyRep.List (List.map (fn x1490 => 
                                                                                                 cvtDEFN x1490
                                                                                          ) ls1491)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1495 => cvtSTMT x1495
                                                     ) ls1496)), ("ctorDefn", 
          
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1500 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1500))
       ))]
   and cvtINTERFACE_DEFN {ident=x1530, ns=opt1532, nonnullable=b1536, params=ls1538, 
          extends=ls1543, instanceDefns=ls1548} = PrettyRep.Rec [("ident", 
          cvtIDENT x1530), ("ns", 
       (case opt1532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1531 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1531))
       )), ("nonnullable", PrettyRep.Bool b1536), ("params", PrettyRep.List (List.map (fn x1537 => 
                                                                                             cvtIDENT x1537
                                                                                      ) ls1538)), 
          ("extends", PrettyRep.List (List.map (fn x1542 => cvtTYPE_EXPR x1542
                                               ) ls1543)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1547 => cvtDEFN x1547
                                   ) ls1548))]
   and cvtTYPE_DEFN {ident=x1565, ns=opt1567, init=x1571} = PrettyRep.Rec [("ident", 
          cvtIDENT x1565), ("ns", 
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1566 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1566))
       )), ("init", cvtTYPE_EXPR x1571)]
   and cvtFOR_ENUM_STMT {isEach=b1579, defn=opt1610, obj=x1614, rib=opt1622, 
          next=x1626, labels=ls1628, body=x1632} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1579), ("defn", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1580, ns=opt1582, static=b1586, prototype=b1587, bindings=(ls1589, 
            ls1594)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1580), ("ns", 
         (case opt1582 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1581 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1581))
         )), ("static", PrettyRep.Bool b1586), ("prototype", PrettyRep.Bool b1587), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1588 => 
                                                                          cvtBINDING x1588
                                                                   ) ls1589), 
            PrettyRep.List (List.map (fn x1593 => cvtINIT_STEP x1593
                                     ) ls1594)])]))
       )), ("obj", cvtEXPR x1614), ("rib", 
       (case opt1622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1618 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1615, 
                                                                                      x1616) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1615, 
                                                                                      cvtFIXTURE x1616]
                                                                               ) ls1618)))
       )), ("next", cvtSTMT x1626), ("labels", PrettyRep.List (List.map (fn x1627 => 
                                                                               cvtIDENT x1627
                                                                        ) ls1628)), 
          ("body", cvtSTMT x1632)]
   and cvtFOR_STMT {rib=opt1655, defn=opt1689, init=ls1694, cond=x1698, update=x1699, 
          labels=ls1701, body=x1705} = PrettyRep.Rec [("rib", 
       (case opt1655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1651 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1648, 
                                                                                      x1649) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1648, 
                                                                                      cvtFIXTURE x1649]
                                                                               ) ls1651)))
       )), ("defn", 
       (case opt1689 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1659, ns=opt1661, static=b1665, prototype=b1666, bindings=(ls1668, 
            ls1673)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1659), ("ns", 
         (case opt1661 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1660 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1660))
         )), ("static", PrettyRep.Bool b1665), ("prototype", PrettyRep.Bool b1666), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1667 => 
                                                                          cvtBINDING x1667
                                                                   ) ls1668), 
            PrettyRep.List (List.map (fn x1672 => cvtINIT_STEP x1672
                                     ) ls1673)])]))
       )), ("init", PrettyRep.List (List.map (fn x1693 => cvtSTMT x1693
                                             ) ls1694)), ("cond", cvtEXPR x1698), 
          ("update", cvtEXPR x1699), ("labels", PrettyRep.List (List.map (fn x1700 => 
                                                                                cvtIDENT x1700
                                                                         ) ls1701)), 
          ("body", cvtSTMT x1705)]
   and cvtWHILE_STMT {cond=x1721, rib=opt1729, body=x1733, labels=ls1735} = 
          PrettyRep.Rec [("cond", cvtEXPR x1721), ("rib", 
       (case opt1729 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1725 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1722, 
                                                                                      x1723) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1722, 
                                                                                      cvtFIXTURE x1723]
                                                                               ) ls1725)))
       )), ("body", cvtSTMT x1733), ("labels", PrettyRep.List (List.map (fn x1734 => 
                                                                               cvtIDENT x1734
                                                                        ) ls1735))]
   and cvtDIRECTIVES {pragmas=ls1749, defns=ls1754, head=opt1759, body=ls1764, 
          loc=opt1769} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1748 => 
                                                                                    cvtPRAGMA x1748
                                                                             ) ls1749)), 
          ("defns", PrettyRep.List (List.map (fn x1753 => cvtDEFN x1753
                                             ) ls1754)), ("head", 
       (case opt1759 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1758 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1758))
       )), ("body", PrettyRep.List (List.map (fn x1763 => cvtSTMT x1763
                                             ) ls1764)), ("loc", 
       (case opt1769 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1768 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1768))
       ))]
   and cvtCASE {label=opt1785, inits=opt1796, body=x1800} = PrettyRep.Rec [("label", 
          
       (case opt1785 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1784 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1784))
       )), ("inits", 
       (case opt1796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1792 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1789, 
                                                                                      x1790) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1789, 
                                                                                      cvtEXPR x1790]
                                                                               ) ls1792)))
       )), ("body", cvtBLOCK x1800)]
   and cvtCATCH_CLAUSE {bindings=(ls1809, ls1814), ty=x1819, rib=opt1827, inits=opt1838, 
          block=x1842} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1808 => 
                                                                                                      cvtBINDING x1808
                                                                                               ) ls1809), 
          PrettyRep.List (List.map (fn x1813 => cvtINIT_STEP x1813
                                   ) ls1814)]), ("ty", cvtTY x1819), ("rib", 
          
       (case opt1827 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1823 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1820, 
                                                                                      x1821) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1820, 
                                                                                      cvtFIXTURE x1821]
                                                                               ) ls1823)))
       )), ("inits", 
       (case opt1838 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1834 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1831, 
                                                                                      x1832) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1831, 
                                                                                      cvtEXPR x1832]
                                                                               ) ls1834)))
       )), ("block", cvtBLOCK x1842)]
   and cvtFUNC_NAME {kind=x1854, ident=x1855} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1854), 
          ("ident", cvtIDENT x1855)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1861, getter=opt1863, setter=opt1868} = 
          PrettyRep.Rec [("ty", cvtTY x1861), ("getter", 
       (case opt1863 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1862 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1862))
       )), ("setter", 
       (case opt1868 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1867 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1867))
       ))]
   and cvtFRAGMENT (Unit{name=opt1880, fragments=ls1885}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1880 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1879 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1879))
       )), ("fragments", PrettyRep.List (List.map (fn x1884 => cvtFRAGMENT x1884
                                                  ) ls1885))]))
     | cvtFRAGMENT (Package{name=ls1897, fragments=ls1902}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1896 => 
                                                                        cvtIDENT x1896
                                                                 ) ls1897)), 
          ("fragments", PrettyRep.List (List.map (fn x1901 => cvtFRAGMENT x1901
                                                 ) ls1902))]))
     | cvtFRAGMENT (Anon x1913) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1913))
end

