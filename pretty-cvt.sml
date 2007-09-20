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
     | cvtIDENT_EXPR (UnresolvedPath(ls1096, x1100)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1095 => cvtIDENT x1095
                                                          ) ls1096), cvtIDENT_EXPR x1100]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1107) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1107))
     | cvtLITERAL (LiteralContextualDecimalInteger s1110) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1110))
     | cvtLITERAL (LiteralContextualHexInteger s1113) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1113))
     | cvtLITERAL (LiteralDouble r1116) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1116))
     | cvtLITERAL (LiteralDecimal d1119) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1119))
     | cvtLITERAL (LiteralInt i1122) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1122))
     | cvtLITERAL (LiteralUInt u1125) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1125))
     | cvtLITERAL (LiteralBoolean b1128) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1128))
     | cvtLITERAL (LiteralString s1131) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1131))
     | cvtLITERAL (LiteralArray{exprs=ls1135, ty=opt1140}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1134 => 
                                                                         cvtEXPR x1134
                                                                  ) ls1135)), 
          ("ty", 
       (case opt1140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1139 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1139))
       ))]))
     | cvtLITERAL (LiteralXML ls1152) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1151 => 
                                                                                                            cvtEXPR x1151
                                                                                                     ) ls1152)))
     | cvtLITERAL (LiteralNamespace x1158) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1158))
     | cvtLITERAL (LiteralObject{expr=ls1162, ty=opt1167}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1161 => 
                                                                        cvtFIELD x1161
                                                                 ) ls1162)), 
          ("ty", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1166))
       ))]))
     | cvtLITERAL (LiteralFunction x1178) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1178))
     | cvtLITERAL (LiteralRegExp{str=s1181}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1181)]))
   and cvtBLOCK (Block x1187) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1187))
   and cvtFIXTURE (NamespaceFixture x1190) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1190))
     | cvtFIXTURE (ClassFixture x1193) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1193))
     | cvtFIXTURE (InterfaceFixture x1196) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1196))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1200) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1200))
     | cvtFIXTURE (MethodFixture{func=x1203, ty=x1204, readOnly=b1205, override=b1206, 
          final=b1207}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1203), ("ty", cvtTY x1204), ("readOnly", PrettyRep.Bool b1205), 
          ("override", PrettyRep.Bool b1206), ("final", PrettyRep.Bool b1207)]))
     | cvtFIXTURE (ValFixture{ty=x1221, readOnly=b1222}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1221), ("readOnly", PrettyRep.Bool b1222)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1230, getter=opt1232, setter=opt1237}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1230), ("getter", 
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1231))
       )), ("setter", 
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1236))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1250, baseTypeArgs=ls1252}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1250), ("baseTypeArgs", PrettyRep.List (List.map (fn x1251 => 
                                                                           cvtTY x1251
                                                                    ) ls1252))]))
   and cvtHEAD (Head(x1263, x1264)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1263, 
          cvtINITS x1264]))
   and cvtBINDINGS (ls1269, ls1274) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1268 => 
                                                                                       cvtBINDING x1268
                                                                                ) ls1269), 
          PrettyRep.List (List.map (fn x1273 => cvtINIT_STEP x1273
                                   ) ls1274)]
   and cvtRIB ls1282 = PrettyRep.List (List.map (fn (x1279, x1280) => PrettyRep.Tuple [cvtFIXTURE_NAME x1279, 
                                                       cvtFIXTURE x1280]
                                                ) ls1282)
   and cvtRIBS ls1287 = PrettyRep.List (List.map (fn x1286 => cvtRIB x1286
                                                 ) ls1287)
   and cvtINITS ls1294 = PrettyRep.List (List.map (fn (x1291, x1292) => PrettyRep.Tuple [cvtFIXTURE_NAME x1291, 
                                                         cvtEXPR x1292]
                                                  ) ls1294)
   and cvtINSTANCE_TYPE {name=x1298, typeArgs=ls1300, nonnullable=b1304, superTypes=ls1306, 
          ty=x1310, conversionTy=opt1312, dynamic=b1316} = PrettyRep.Rec [("name", 
          cvtNAME x1298), ("typeArgs", PrettyRep.List (List.map (fn x1299 => 
                                                                       cvtTYPE_EXPR x1299
                                                                ) ls1300)), 
          ("nonnullable", PrettyRep.Bool b1304), ("superTypes", PrettyRep.List (List.map (fn x1305 => 
                                                                                                cvtTYPE_EXPR x1305
                                                                                         ) ls1306)), 
          ("ty", cvtTYPE_EXPR x1310), ("conversionTy", 
       (case opt1312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1311 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1311))
       )), ("dynamic", PrettyRep.Bool b1316)]
   and cvtFIELD {kind=x1332, name=x1333, init=x1334} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1332), ("name", cvtIDENT_EXPR x1333), ("init", cvtEXPR x1334)]
   and cvtFIELD_TYPE {name=x1342, ty=x1343} = PrettyRep.Rec [("name", cvtIDENT x1342), 
          ("ty", cvtTYPE_EXPR x1343)]
   and cvtFUNC_TYPE {params=ls1350, result=x1354, thisType=opt1356, hasRest=b1360, 
          minArgs=n1361} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1349 => 
                                                                                     cvtTYPE_EXPR x1349
                                                                              ) ls1350)), 
          ("result", cvtTYPE_EXPR x1354), ("thisType", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1355))
       )), ("hasRest", PrettyRep.Bool b1360), ("minArgs", PrettyRep.Int n1361)]
   and cvtFUNC_DEFN {kind=x1373, ns=opt1375, final=b1379, override=b1380, prototype=b1381, 
          static=b1382, func=x1383} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1373), 
          ("ns", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1374 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1374))
       )), ("final", PrettyRep.Bool b1379), ("override", PrettyRep.Bool b1380), 
          ("prototype", PrettyRep.Bool b1381), ("static", PrettyRep.Bool b1382), 
          ("func", cvtFUNC x1383)]
   and cvtCTOR_DEFN x1399 = cvtCTOR x1399
   and cvtVAR_DEFN {kind=x1400, ns=opt1402, static=b1406, prototype=b1407, 
          bindings=(ls1409, ls1414)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1400), 
          ("ns", 
       (case opt1402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1401 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1401))
       )), ("static", PrettyRep.Bool b1406), ("prototype", PrettyRep.Bool b1407), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1408 => 
                                                                        cvtBINDING x1408
                                                                 ) ls1409), 
          PrettyRep.List (List.map (fn x1413 => cvtINIT_STEP x1413
                                   ) ls1414)])]
   and cvtNAMESPACE_DEFN {ident=x1430, ns=opt1432, init=opt1437} = PrettyRep.Rec [("ident", 
          cvtIDENT x1430), ("ns", 
       (case opt1432 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1431 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1431))
       )), ("init", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1436))
       ))]
   and cvtCLASS_DEFN {ns=opt1449, ident=x1453, nonnullable=b1454, dynamic=b1455, 
          final=b1456, params=ls1458, extends=opt1463, implements=ls1468, classDefns=ls1473, 
          instanceDefns=ls1478, instanceStmts=ls1483, ctorDefn=opt1488} = PrettyRep.Rec [("ns", 
          
       (case opt1449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1448 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1448))
       )), ("ident", cvtIDENT x1453), ("nonnullable", PrettyRep.Bool b1454), 
          ("dynamic", PrettyRep.Bool b1455), ("final", PrettyRep.Bool b1456), 
          ("params", PrettyRep.List (List.map (fn x1457 => cvtIDENT x1457
                                              ) ls1458)), ("extends", 
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1462 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1462))
       )), ("implements", PrettyRep.List (List.map (fn x1467 => cvtTYPE_EXPR x1467
                                                   ) ls1468)), ("classDefns", 
          PrettyRep.List (List.map (fn x1472 => cvtDEFN x1472
                                   ) ls1473)), ("instanceDefns", PrettyRep.List (List.map (fn x1477 => 
                                                                                                 cvtDEFN x1477
                                                                                          ) ls1478)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1482 => cvtSTMT x1482
                                                     ) ls1483)), ("ctorDefn", 
          
       (case opt1488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1487 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1487))
       ))]
   and cvtINTERFACE_DEFN {ident=x1517, ns=opt1519, nonnullable=b1523, params=ls1525, 
          extends=ls1530, instanceDefns=ls1535} = PrettyRep.Rec [("ident", 
          cvtIDENT x1517), ("ns", 
       (case opt1519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1518 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1518))
       )), ("nonnullable", PrettyRep.Bool b1523), ("params", PrettyRep.List (List.map (fn x1524 => 
                                                                                             cvtIDENT x1524
                                                                                      ) ls1525)), 
          ("extends", PrettyRep.List (List.map (fn x1529 => cvtTYPE_EXPR x1529
                                               ) ls1530)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1534 => cvtDEFN x1534
                                   ) ls1535))]
   and cvtTYPE_DEFN {ident=x1552, ns=opt1554, init=x1558} = PrettyRep.Rec [("ident", 
          cvtIDENT x1552), ("ns", 
       (case opt1554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1553 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1553))
       )), ("init", cvtTYPE_EXPR x1558)]
   and cvtFOR_ENUM_STMT {isEach=b1566, defn=opt1597, obj=x1601, rib=opt1609, 
          next=x1613, labels=ls1615, body=x1619} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1566), ("defn", 
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1567, ns=opt1569, static=b1573, prototype=b1574, bindings=(ls1576, 
            ls1581)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1567), ("ns", 
         (case opt1569 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1568))
         )), ("static", PrettyRep.Bool b1573), ("prototype", PrettyRep.Bool b1574), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1575 => 
                                                                          cvtBINDING x1575
                                                                   ) ls1576), 
            PrettyRep.List (List.map (fn x1580 => cvtINIT_STEP x1580
                                     ) ls1581)])]))
       )), ("obj", cvtEXPR x1601), ("rib", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1605 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1602, 
                                                                                      x1603) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1602, 
                                                                                      cvtFIXTURE x1603]
                                                                               ) ls1605)))
       )), ("next", cvtSTMT x1613), ("labels", PrettyRep.List (List.map (fn x1614 => 
                                                                               cvtIDENT x1614
                                                                        ) ls1615)), 
          ("body", cvtSTMT x1619)]
   and cvtFOR_STMT {rib=opt1642, defn=opt1676, init=ls1681, cond=x1685, update=x1686, 
          labels=ls1688, body=x1692} = PrettyRep.Rec [("rib", 
       (case opt1642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1638 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1635, 
                                                                                      x1636) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1635, 
                                                                                      cvtFIXTURE x1636]
                                                                               ) ls1638)))
       )), ("defn", 
       (case opt1676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1646, ns=opt1648, static=b1652, prototype=b1653, bindings=(ls1655, 
            ls1660)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1646), ("ns", 
         (case opt1648 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1647 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1647))
         )), ("static", PrettyRep.Bool b1652), ("prototype", PrettyRep.Bool b1653), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1654 => 
                                                                          cvtBINDING x1654
                                                                   ) ls1655), 
            PrettyRep.List (List.map (fn x1659 => cvtINIT_STEP x1659
                                     ) ls1660)])]))
       )), ("init", PrettyRep.List (List.map (fn x1680 => cvtSTMT x1680
                                             ) ls1681)), ("cond", cvtEXPR x1685), 
          ("update", cvtEXPR x1686), ("labels", PrettyRep.List (List.map (fn x1687 => 
                                                                                cvtIDENT x1687
                                                                         ) ls1688)), 
          ("body", cvtSTMT x1692)]
   and cvtWHILE_STMT {cond=x1708, rib=opt1716, body=x1720, labels=ls1722} = 
          PrettyRep.Rec [("cond", cvtEXPR x1708), ("rib", 
       (case opt1716 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1712 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1709, 
                                                                                      x1710) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1709, 
                                                                                      cvtFIXTURE x1710]
                                                                               ) ls1712)))
       )), ("body", cvtSTMT x1720), ("labels", PrettyRep.List (List.map (fn x1721 => 
                                                                               cvtIDENT x1721
                                                                        ) ls1722))]
   and cvtDIRECTIVES {pragmas=ls1736, defns=ls1741, head=opt1746, body=ls1751, 
          loc=opt1756} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1735 => 
                                                                                    cvtPRAGMA x1735
                                                                             ) ls1736)), 
          ("defns", PrettyRep.List (List.map (fn x1740 => cvtDEFN x1740
                                             ) ls1741)), ("head", 
       (case opt1746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1745 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1745))
       )), ("body", PrettyRep.List (List.map (fn x1750 => cvtSTMT x1750
                                             ) ls1751)), ("loc", 
       (case opt1756 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1755 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1755))
       ))]
   and cvtCASE {label=opt1772, inits=opt1783, body=x1787} = PrettyRep.Rec [("label", 
          
       (case opt1772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1771 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1771))
       )), ("inits", 
       (case opt1783 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1779 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1776, 
                                                                                      x1777) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1776, 
                                                                                      cvtEXPR x1777]
                                                                               ) ls1779)))
       )), ("body", cvtBLOCK x1787)]
   and cvtCATCH_CLAUSE {bindings=(ls1796, ls1801), ty=x1806, rib=opt1814, inits=opt1825, 
          block=x1829} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1795 => 
                                                                                                      cvtBINDING x1795
                                                                                               ) ls1796), 
          PrettyRep.List (List.map (fn x1800 => cvtINIT_STEP x1800
                                   ) ls1801)]), ("ty", cvtTY x1806), ("rib", 
          
       (case opt1814 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1810 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1807, 
                                                                                      x1808) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1807, 
                                                                                      cvtFIXTURE x1808]
                                                                               ) ls1810)))
       )), ("inits", 
       (case opt1825 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1821 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1818, 
                                                                                      x1819) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1818, 
                                                                                      cvtEXPR x1819]
                                                                               ) ls1821)))
       )), ("block", cvtBLOCK x1829)]
   and cvtFUNC_NAME {kind=x1841, ident=x1842} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1841), 
          ("ident", cvtIDENT x1842)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1848, getter=opt1850, setter=opt1855} = 
          PrettyRep.Rec [("ty", cvtTY x1848), ("getter", 
       (case opt1850 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1849 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1849))
       )), ("setter", 
       (case opt1855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1854 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1854))
       ))]
   and cvtFRAGMENT (Unit{name=opt1867, fragments=ls1872}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1867 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1866 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1866))
       )), ("fragments", PrettyRep.List (List.map (fn x1871 => cvtFRAGMENT x1871
                                                  ) ls1872))]))
     | cvtFRAGMENT (Package{name=ls1884, fragments=ls1889}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1883 => 
                                                                        cvtIDENT x1883
                                                                 ) ls1884)), 
          ("fragments", PrettyRep.List (List.map (fn x1888 => cvtFRAGMENT x1888
                                                 ) ls1889))]))
     | cvtFRAGMENT (Anon x1900) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1900))
end

