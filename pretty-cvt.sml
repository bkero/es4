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
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x27) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x27))
     | cvtNAMESPACE (Protected x30) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x30))
     | cvtNAMESPACE (Public x33) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x33))
     | cvtNAMESPACE (Internal x36) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x36))
     | cvtNAMESPACE (UserNamespace s39) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s39))
     | cvtNAMESPACE (AnonUserNamespace n42) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n42))
     | cvtNAMESPACE (LimitedNamespace(x45, x46)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x45, cvtNAMESPACE x46]))
   and cvtNAME {ns=x50, id=x51} = PrettyRep.Rec [("ns", cvtNAMESPACE x50), 
          ("id", cvtIDENT x51)]
   and cvtMULTINAME {nss=ls62, id=x66} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls58 => 
                                                                                                PrettyRep.List (List.map (fn x57 => 
                                                                                                                                cvtNAMESPACE x57
                                                                                                                         ) ls58)
                                                                                         ) ls62)), 
          ("id", cvtIDENT x66)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x77, roundingMode=r78, precision=n79} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x77), ("roundingMode", 
          PrettyRep.DecRm r78), ("precision", PrettyRep.Int n79)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt91) = PrettyRep.Ctor ("Plus", SOME 
       (case opt91 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x90 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x90))
       ))
     | cvtBINOP (Minus opt98) = PrettyRep.Ctor ("Minus", SOME 
       (case opt98 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x97 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x97))
       ))
     | cvtBINOP (Times opt105) = PrettyRep.Ctor ("Times", SOME 
       (case opt105 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x104 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x104))
       ))
     | cvtBINOP (Divide opt112) = PrettyRep.Ctor ("Divide", SOME 
       (case opt112 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x111 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x111))
       ))
     | cvtBINOP (Remainder opt119) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x118 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x118))
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
     | cvtBINOP (Equals opt136) = PrettyRep.Ctor ("Equals", SOME 
       (case opt136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x135 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x135))
       ))
     | cvtBINOP (NotEquals opt143) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x142 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x142))
       ))
     | cvtBINOP (StrictEquals opt150) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x149 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x149))
       ))
     | cvtBINOP (StrictNotEquals opt157) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x156 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x156))
       ))
     | cvtBINOP (Less opt164) = PrettyRep.Ctor ("Less", SOME 
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x163))
       ))
     | cvtBINOP (LessOrEqual opt171) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x170 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x170))
       ))
     | cvtBINOP (Greater opt178) = PrettyRep.Ctor ("Greater", SOME 
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x177))
       ))
     | cvtBINOP (GreaterOrEqual opt185) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x184 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x184))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt194) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x193 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x193))
       ))
     | cvtASSIGNOP (AssignMinus opt201) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x200 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x200))
       ))
     | cvtASSIGNOP (AssignTimes opt208) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x207 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x207))
       ))
     | cvtASSIGNOP (AssignDivide opt215) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x214 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x214))
       ))
     | cvtASSIGNOP (AssignRemainder opt222) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x221 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x221))
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
     | cvtUNOP (PreIncrement opt240) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x239 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x239))
       ))
     | cvtUNOP (PreDecrement opt247) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt247 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x246 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x246))
       ))
     | cvtUNOP (PostIncrement opt254) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x253 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x253))
       ))
     | cvtUNOP (PostDecrement opt261) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x260 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x260))
       ))
     | cvtUNOP (UnaryPlus opt268) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x267 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x267))
       ))
     | cvtUNOP (UnaryMinus opt275) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x274 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x274))
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
   and cvtPRAGMA (UseNamespace x292) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x292))
     | cvtPRAGMA (UseDefaultNamespace x295) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x295))
     | cvtPRAGMA (UseNumber x298) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x298))
     | cvtPRAGMA (UseRounding r301) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r301))
     | cvtPRAGMA (UsePrecision n304) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n304))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls310, name=x314, alias=opt316}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x309 => 
                                                                           cvtIDENT x309
                                                                    ) ls310)), 
          ("name", cvtIDENT x314), ("alias", 
       (case opt316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x315 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x315))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x335, nonTopRibs=x336, topUnit=opt338}) = PrettyRep.Ctor ("Ty", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x335), ("nonTopRibs", 
          cvtRIBS x336), ("topUnit", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x337))
       ))]))
   and cvtCLS (Cls{name=x351, typeParams=ls353, nonnullable=b357, dynamic=b358, 
          extends=opt360, implements=ls365, classRib=x369, instanceRib=x370, 
          instanceInits=x371, constructor=opt373, classType=x377, instanceType=x378}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x351), 
          ("typeParams", PrettyRep.List (List.map (fn x352 => cvtIDENT x352
                                                  ) ls353)), ("nonnullable", 
          PrettyRep.Bool b357), ("dynamic", PrettyRep.Bool b358), ("extends", 
          
       (case opt360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x359 => PrettyRep.Ctor ("SOME", SOME (cvtTY x359))
       )), ("implements", PrettyRep.List (List.map (fn x364 => cvtTY x364
                                                   ) ls365)), ("classRib", 
          cvtRIB x369), ("instanceRib", cvtRIB x370), ("instanceInits", cvtHEAD x371), 
          ("constructor", 
       (case opt373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x372 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x372))
       )), ("classType", cvtTY x377), ("instanceType", cvtTY x378)]))
   and cvtIFACE (Iface{name=x406, typeParams=ls408, nonnullable=b412, extends=ls414, 
          instanceRib=x418, instanceType=x419}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x406), ("typeParams", PrettyRep.List (List.map (fn x407 => 
                                                                                                      cvtIDENT x407
                                                                                               ) ls408)), 
          ("nonnullable", PrettyRep.Bool b412), ("extends", PrettyRep.List (List.map (fn x413 => 
                                                                                            cvtTY x413
                                                                                     ) ls414)), 
          ("instanceRib", cvtRIB x418), ("instanceType", cvtTY x419)]))
   and cvtCTOR (Ctor{settings=x435, superArgs=ls437, func=x441}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x435), ("superArgs", PrettyRep.List (List.map (fn x436 => 
                                                                                                         cvtEXPR x436
                                                                                                  ) ls437)), 
          ("func", cvtFUNC x441)]))
   and cvtFUNC (Func{name=x451, typeParams=ls453, fsig=x457, native=b458, block=opt460, 
          param=x464, defaults=ls466, ty=x470, loc=opt472}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x451), ("typeParams", 
          PrettyRep.List (List.map (fn x452 => cvtIDENT x452
                                   ) ls453)), ("fsig", cvtFUNC_SIG x457), ("native", 
          PrettyRep.Bool b458), ("block", 
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
   and cvtDEFN (ClassDefn x497) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x497))
     | cvtDEFN (VariableDefn x500) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x500))
     | cvtDEFN (FunctionDefn x503) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x503))
     | cvtDEFN (ConstructorDefn x506) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x506))
     | cvtDEFN (InterfaceDefn x509) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x509))
     | cvtDEFN (NamespaceDefn x512) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x512))
     | cvtDEFN (TypeDefn x515) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x515))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls519, params=x523, paramTypes=ls525, 
          defaults=ls530, ctorInits=opt541, returnType=x545, thisType=opt547, 
          hasRest=b551}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x518 => cvtTYPE_EXPR x518
                                   ) ls519)), ("params", cvtBINDINGS x523), 
          ("paramTypes", PrettyRep.List (List.map (fn x524 => cvtTYPE_EXPR x524
                                                  ) ls525)), ("defaults", PrettyRep.List (List.map (fn x529 => 
                                                                                                          cvtEXPR x529
                                                                                                   ) ls530)), 
          ("ctorInits", 
       (case opt541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x534, ls536) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x534, 
            PrettyRep.List (List.map (fn x535 => cvtEXPR x535
                                     ) ls536)]))
       )), ("returnType", cvtTYPE_EXPR x545), ("thisType", 
       (case opt547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x546 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x546))
       )), ("hasRest", PrettyRep.Bool b551)]))
   and cvtBINDING (Binding{ident=x571, ty=x572}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x571), ("ty", cvtTYPE_EXPR x572)]))
   and cvtBINDING_IDENT (TempIdent n580) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n580))
     | cvtBINDING_IDENT (ParamIdent n583) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n583))
     | cvtBINDING_IDENT (PropIdent x586) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x586))
   and cvtINIT_STEP (InitStep(x589, x590)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x589, 
          cvtEXPR x590]))
     | cvtINIT_STEP (AssignStep(x594, x595)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x594, cvtEXPR x595]))
   and cvtTYPE_EXPR (SpecialType x599) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x599))
     | cvtTYPE_EXPR (UnionType ls603) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x602 => 
                                                                                                           cvtTYPE_EXPR x602
                                                                                                    ) ls603)))
     | cvtTYPE_EXPR (ArrayType ls610) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x609 => 
                                                                                                           cvtTYPE_EXPR x609
                                                                                                    ) ls610)))
     | cvtTYPE_EXPR (TypeName x616) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x616))
     | cvtTYPE_EXPR (ElementTypeRef(x619, n620)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x619, PrettyRep.Int n620]))
     | cvtTYPE_EXPR (FieldTypeRef(x624, x625)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x624, cvtIDENT x625]))
     | cvtTYPE_EXPR (FunctionType x629) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x629))
     | cvtTYPE_EXPR (ObjectType ls633) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x632 => 
                                                                                                             cvtFIELD_TYPE x632
                                                                                                      ) ls633)))
     | cvtTYPE_EXPR (AppType{base=x639, args=ls641}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x639), ("args", PrettyRep.List (List.map (fn x640 => 
                                                                                                     cvtTYPE_EXPR x640
                                                                                              ) ls641))]))
     | cvtTYPE_EXPR (LamType{params=ls653, body=x657}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x652 => 
                                                                          cvtIDENT x652
                                                                   ) ls653)), 
          ("body", cvtTYPE_EXPR x657)]))
     | cvtTYPE_EXPR (NullableType{expr=x665, nullable=b666}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x665), ("nullable", PrettyRep.Bool b666)]))
     | cvtTYPE_EXPR (InstanceType x674) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x674))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x678) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x678))
     | cvtSTMT (InitStmt{kind=x681, ns=opt683, prototype=b687, static=b688, 
          temps=x689, inits=ls691}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x681), ("ns", 
       (case opt683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x682 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x682))
       )), ("prototype", PrettyRep.Bool b687), ("static", PrettyRep.Bool b688), 
          ("temps", cvtBINDINGS x689), ("inits", PrettyRep.List (List.map (fn x690 => 
                                                                                 cvtINIT_STEP x690
                                                                          ) ls691))]))
     | cvtSTMT (ClassBlock{ns=opt711, ident=x715, name=opt717, block=x721}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x710 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x710))
       )), ("ident", cvtIDENT x715), ("name", 
       (case opt717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x716 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x716))
       )), ("block", cvtBLOCK x721)]))
     | cvtSTMT (ForInStmt x733) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x733))
     | cvtSTMT (ThrowStmt x736) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x736))
     | cvtSTMT (ReturnStmt x739) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x739))
     | cvtSTMT (BreakStmt opt743) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt743 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x742 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x742))
       ))
     | cvtSTMT (ContinueStmt opt750) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt750 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x749 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x749))
       ))
     | cvtSTMT (BlockStmt x756) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x756))
     | cvtSTMT (LabeledStmt(x759, x760)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x759, 
          cvtSTMT x760]))
     | cvtSTMT (LetStmt x764) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x764))
     | cvtSTMT (WhileStmt x767) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x767))
     | cvtSTMT (DoWhileStmt x770) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x770))
     | cvtSTMT (ForStmt x773) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x773))
     | cvtSTMT (IfStmt{cnd=x776, thn=x777, els=x778}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x776), ("thn", cvtSTMT x777), 
          ("els", cvtSTMT x778)]))
     | cvtSTMT (WithStmt{obj=x788, ty=x789, body=x790}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x788), ("ty", cvtTY x789), ("body", 
          cvtSTMT x790)]))
     | cvtSTMT (TryStmt{block=x800, catches=ls802, finally=opt807}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x800), ("catches", PrettyRep.List (List.map (fn x801 => 
                                                                                                     cvtCATCH_CLAUSE x801
                                                                                              ) ls802)), 
          ("finally", 
       (case opt807 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x806 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x806))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt821, cond=x825, labels=ls827, cases=ls832}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x820))
       )), ("cond", cvtEXPR x825), ("labels", PrettyRep.List (List.map (fn x826 => 
                                                                              cvtIDENT x826
                                                                       ) ls827)), 
          ("cases", PrettyRep.List (List.map (fn x831 => cvtCASE x831
                                             ) ls832))]))
     | cvtSTMT (SwitchTypeStmt{cond=x847, ty=x848, cases=ls850}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x847), ("ty", cvtTY x848), 
          ("cases", PrettyRep.List (List.map (fn x849 => cvtCATCH_CLAUSE x849
                                             ) ls850))]))
     | cvtSTMT (DXNStmt{expr=x863}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x863)]))
   and cvtEXPR (TernaryExpr(x869, x870, x871)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x869, cvtEXPR x870, cvtEXPR x871]))
     | cvtEXPR (BinaryExpr(x875, x876, x877)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x875, cvtEXPR x876, cvtEXPR x877]))
     | cvtEXPR (BinaryTypeExpr(x881, x882, x883)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x881, cvtEXPR x882, cvtTY x883]))
     | cvtEXPR (ExpectedTypeExpr(x887, x888)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTY x887, cvtEXPR x888]))
     | cvtEXPR (UnaryExpr(x892, x893)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x892, 
          cvtEXPR x893]))
     | cvtEXPR (TypeExpr x897) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x897))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt902) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt902 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x901 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x901))
       ))
     | cvtEXPR (SuperExpr opt909) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt909 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x908 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x908))
       ))
     | cvtEXPR (LiteralExpr x915) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x915))
     | cvtEXPR (CallExpr{func=x918, actuals=ls920}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x918), ("actuals", PrettyRep.List (List.map (fn x919 => 
                                                                                                   cvtEXPR x919
                                                                                            ) ls920))]))
     | cvtEXPR (ApplyTypeExpr{expr=x931, actuals=ls933}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x931), ("actuals", PrettyRep.List (List.map (fn x932 => 
                                                                                                   cvtTY x932
                                                                                            ) ls933))]))
     | cvtEXPR (LetExpr{defs=x944, body=x945, head=opt947}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x944), ("body", cvtEXPR x945), 
          ("head", 
       (case opt947 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x946 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x946))
       ))]))
     | cvtEXPR (NewExpr{obj=x960, actuals=ls962}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x960), ("actuals", PrettyRep.List (List.map (fn x961 => 
                                                                                                  cvtEXPR x961
                                                                                           ) ls962))]))
     | cvtEXPR (ObjectRef{base=x973, ident=x974, loc=opt976}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x973), ("ident", cvtIDENT_EXPR x974), 
          ("loc", 
       (case opt976 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x975 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x975))
       ))]))
     | cvtEXPR (LexicalRef{ident=x989, loc=opt991}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x989), ("loc", 
       (case opt991 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x990 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x990))
       ))]))
     | cvtEXPR (SetExpr(x1002, x1003, x1004)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x1002, cvtEXPR x1003, cvtEXPR x1004]))
     | cvtEXPR (ListExpr ls1009) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x1008 => 
                                                                                                     cvtEXPR x1008
                                                                                              ) ls1009)))
     | cvtEXPR (InitExpr(x1015, x1016, x1017)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x1015, cvtHEAD x1016, cvtINITS x1017]))
     | cvtEXPR (SliceExpr(x1021, x1022, x1023)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x1021, cvtEXPR x1022, cvtEXPR x1023]))
     | cvtEXPR (GetTemp n1027) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n1027))
     | cvtEXPR (GetParam n1030) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n1030))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1036) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1036))
     | cvtFIXTURE_NAME (PropName x1039) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1039))
   and cvtIDENT_EXPR (Identifier{ident=x1042, openNamespaces=ls1048}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1042), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1044 => PrettyRep.List (List.map (fn x1043 => 
                                                                                 cvtNAMESPACE x1043
                                                                          ) ls1044)
                                   ) ls1048))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1059, expr=x1060}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1059), ("expr", cvtEXPR x1060)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1068) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1068))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1071, openNamespaces=ls1077}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1071), ("openNamespaces", PrettyRep.List (List.map (fn ls1073 => 
                                                                             PrettyRep.List (List.map (fn x1072 => 
                                                                                                             cvtNAMESPACE x1072
                                                                                                      ) ls1073)
                                                                      ) ls1077))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1088, ident=s1089}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1088), ("ident", PrettyRep.UniStr s1089)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1098, x1102)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1097 => cvtIDENT x1097
                                                          ) ls1098), cvtIDENT_EXPR x1102]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1109) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1109))
     | cvtLITERAL (LiteralContextualDecimalInteger s1112) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1112))
     | cvtLITERAL (LiteralContextualHexInteger s1115) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1115))
     | cvtLITERAL (LiteralDouble r1118) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1118))
     | cvtLITERAL (LiteralDecimal d1121) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1121))
     | cvtLITERAL (LiteralInt i1124) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1124))
     | cvtLITERAL (LiteralUInt u1127) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1127))
     | cvtLITERAL (LiteralBoolean b1130) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1130))
     | cvtLITERAL (LiteralString s1133) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1133))
     | cvtLITERAL (LiteralArray{exprs=ls1137, ty=opt1142}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1136 => 
                                                                         cvtEXPR x1136
                                                                  ) ls1137)), 
          ("ty", 
       (case opt1142 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1141 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1141))
       ))]))
     | cvtLITERAL (LiteralXML ls1154) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1153 => 
                                                                                                            cvtEXPR x1153
                                                                                                     ) ls1154)))
     | cvtLITERAL (LiteralNamespace x1160) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1160))
     | cvtLITERAL (LiteralObject{expr=ls1164, ty=opt1169}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1163 => 
                                                                        cvtFIELD x1163
                                                                 ) ls1164)), 
          ("ty", 
       (case opt1169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1168 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1168))
       ))]))
     | cvtLITERAL (LiteralFunction x1180) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1180))
     | cvtLITERAL (LiteralRegExp{str=s1183}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1183)]))
   and cvtBLOCK (Block x1189) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1189))
   and cvtFIXTURE (NamespaceFixture x1192) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1192))
     | cvtFIXTURE (ClassFixture x1195) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1195))
     | cvtFIXTURE (InterfaceFixture x1198) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1198))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1202) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1202))
     | cvtFIXTURE (MethodFixture{func=x1205, ty=x1206, readOnly=b1207, override=b1208, 
          final=b1209}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1205), ("ty", cvtTY x1206), ("readOnly", PrettyRep.Bool b1207), 
          ("override", PrettyRep.Bool b1208), ("final", PrettyRep.Bool b1209)]))
     | cvtFIXTURE (ValFixture{ty=x1223, readOnly=b1224}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1223), ("readOnly", PrettyRep.Bool b1224)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1232, getter=opt1234, setter=opt1239}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1232), ("getter", 
       (case opt1234 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1233 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1233))
       )), ("setter", 
       (case opt1239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1238 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1238))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1252, baseTypeArgs=ls1254}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1252), ("baseTypeArgs", PrettyRep.List (List.map (fn x1253 => 
                                                                           cvtTY x1253
                                                                    ) ls1254))]))
   and cvtHEAD (Head(x1265, x1266)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1265, 
          cvtINITS x1266]))
   and cvtBINDINGS (ls1271, ls1276) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1270 => 
                                                                                       cvtBINDING x1270
                                                                                ) ls1271), 
          PrettyRep.List (List.map (fn x1275 => cvtINIT_STEP x1275
                                   ) ls1276)]
   and cvtRIB ls1284 = PrettyRep.List (List.map (fn (x1281, x1282) => PrettyRep.Tuple [cvtFIXTURE_NAME x1281, 
                                                       cvtFIXTURE x1282]
                                                ) ls1284)
   and cvtRIBS ls1289 = PrettyRep.List (List.map (fn x1288 => cvtRIB x1288
                                                 ) ls1289)
   and cvtINITS ls1296 = PrettyRep.List (List.map (fn (x1293, x1294) => PrettyRep.Tuple [cvtFIXTURE_NAME x1293, 
                                                         cvtEXPR x1294]
                                                  ) ls1296)
   and cvtINSTANCE_TYPE {name=x1300, typeArgs=ls1302, nonnullable=b1306, superTypes=ls1308, 
          ty=x1312, conversionTy=opt1314, dynamic=b1318} = PrettyRep.Rec [("name", 
          cvtNAME x1300), ("typeArgs", PrettyRep.List (List.map (fn x1301 => 
                                                                       cvtTYPE_EXPR x1301
                                                                ) ls1302)), 
          ("nonnullable", PrettyRep.Bool b1306), ("superTypes", PrettyRep.List (List.map (fn x1307 => 
                                                                                                cvtTYPE_EXPR x1307
                                                                                         ) ls1308)), 
          ("ty", cvtTYPE_EXPR x1312), ("conversionTy", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1313))
       )), ("dynamic", PrettyRep.Bool b1318)]
   and cvtFIELD {kind=x1334, name=x1335, init=x1336} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1334), ("name", cvtIDENT_EXPR x1335), ("init", cvtEXPR x1336)]
   and cvtFIELD_TYPE {name=x1344, ty=x1345} = PrettyRep.Rec [("name", cvtIDENT x1344), 
          ("ty", cvtTYPE_EXPR x1345)]
   and cvtFUNC_TYPE {params=ls1352, result=x1356, thisType=opt1358, hasRest=b1362, 
          minArgs=n1363} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1351 => 
                                                                                     cvtTYPE_EXPR x1351
                                                                              ) ls1352)), 
          ("result", cvtTYPE_EXPR x1356), ("thisType", 
       (case opt1358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1357))
       )), ("hasRest", PrettyRep.Bool b1362), ("minArgs", PrettyRep.Int n1363)]
   and cvtFUNC_DEFN {kind=x1375, ns=opt1377, final=b1381, override=b1382, prototype=b1383, 
          static=b1384, func=x1385} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1375), 
          ("ns", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1376))
       )), ("final", PrettyRep.Bool b1381), ("override", PrettyRep.Bool b1382), 
          ("prototype", PrettyRep.Bool b1383), ("static", PrettyRep.Bool b1384), 
          ("func", cvtFUNC x1385)]
   and cvtCTOR_DEFN x1401 = cvtCTOR x1401
   and cvtVAR_DEFN {kind=x1402, ns=opt1404, static=b1408, prototype=b1409, 
          bindings=(ls1411, ls1416)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1402), 
          ("ns", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1403 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1403))
       )), ("static", PrettyRep.Bool b1408), ("prototype", PrettyRep.Bool b1409), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1410 => 
                                                                        cvtBINDING x1410
                                                                 ) ls1411), 
          PrettyRep.List (List.map (fn x1415 => cvtINIT_STEP x1415
                                   ) ls1416)])]
   and cvtNAMESPACE_DEFN {ident=x1432, ns=opt1434, init=opt1439} = PrettyRep.Rec [("ident", 
          cvtIDENT x1432), ("ns", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1433))
       )), ("init", 
       (case opt1439 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1438))
       ))]
   and cvtCLASS_DEFN {ns=opt1451, ident=x1455, nonnullable=b1456, dynamic=b1457, 
          final=b1458, params=ls1460, extends=opt1465, implements=ls1470, classDefns=ls1475, 
          instanceDefns=ls1480, instanceStmts=ls1485, ctorDefn=opt1490} = PrettyRep.Rec [("ns", 
          
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1450))
       )), ("ident", cvtIDENT x1455), ("nonnullable", PrettyRep.Bool b1456), 
          ("dynamic", PrettyRep.Bool b1457), ("final", PrettyRep.Bool b1458), 
          ("params", PrettyRep.List (List.map (fn x1459 => cvtIDENT x1459
                                              ) ls1460)), ("extends", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1464))
       )), ("implements", PrettyRep.List (List.map (fn x1469 => cvtTYPE_EXPR x1469
                                                   ) ls1470)), ("classDefns", 
          PrettyRep.List (List.map (fn x1474 => cvtDEFN x1474
                                   ) ls1475)), ("instanceDefns", PrettyRep.List (List.map (fn x1479 => 
                                                                                                 cvtDEFN x1479
                                                                                          ) ls1480)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1484 => cvtSTMT x1484
                                                     ) ls1485)), ("ctorDefn", 
          
       (case opt1490 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1489 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1489))
       ))]
   and cvtINTERFACE_DEFN {ident=x1519, ns=opt1521, nonnullable=b1525, params=ls1527, 
          extends=ls1532, instanceDefns=ls1537} = PrettyRep.Rec [("ident", 
          cvtIDENT x1519), ("ns", 
       (case opt1521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1520 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1520))
       )), ("nonnullable", PrettyRep.Bool b1525), ("params", PrettyRep.List (List.map (fn x1526 => 
                                                                                             cvtIDENT x1526
                                                                                      ) ls1527)), 
          ("extends", PrettyRep.List (List.map (fn x1531 => cvtTYPE_EXPR x1531
                                               ) ls1532)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1536 => cvtDEFN x1536
                                   ) ls1537))]
   and cvtTYPE_DEFN {ident=x1554, ns=opt1556, init=x1560} = PrettyRep.Rec [("ident", 
          cvtIDENT x1554), ("ns", 
       (case opt1556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1555 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1555))
       )), ("init", cvtTYPE_EXPR x1560)]
   and cvtFOR_ENUM_STMT {isEach=b1568, defn=opt1599, obj=x1603, rib=opt1611, 
          next=x1615, labels=ls1617, body=x1621} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1568), ("defn", 
       (case opt1599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1569, ns=opt1571, static=b1575, prototype=b1576, bindings=(ls1578, 
            ls1583)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1569), ("ns", 
         (case opt1571 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1570 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1570))
         )), ("static", PrettyRep.Bool b1575), ("prototype", PrettyRep.Bool b1576), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1577 => 
                                                                          cvtBINDING x1577
                                                                   ) ls1578), 
            PrettyRep.List (List.map (fn x1582 => cvtINIT_STEP x1582
                                     ) ls1583)])]))
       )), ("obj", cvtEXPR x1603), ("rib", 
       (case opt1611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1607 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1604, 
                                                                                      x1605) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1604, 
                                                                                      cvtFIXTURE x1605]
                                                                               ) ls1607)))
       )), ("next", cvtSTMT x1615), ("labels", PrettyRep.List (List.map (fn x1616 => 
                                                                               cvtIDENT x1616
                                                                        ) ls1617)), 
          ("body", cvtSTMT x1621)]
   and cvtFOR_STMT {rib=opt1644, defn=opt1678, init=ls1683, cond=x1687, update=x1688, 
          labels=ls1690, body=x1694} = PrettyRep.Rec [("rib", 
       (case opt1644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1640 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1637, 
                                                                                      x1638) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1637, 
                                                                                      cvtFIXTURE x1638]
                                                                               ) ls1640)))
       )), ("defn", 
       (case opt1678 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1648, ns=opt1650, static=b1654, prototype=b1655, bindings=(ls1657, 
            ls1662)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1648), ("ns", 
         (case opt1650 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1649 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1649))
         )), ("static", PrettyRep.Bool b1654), ("prototype", PrettyRep.Bool b1655), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1656 => 
                                                                          cvtBINDING x1656
                                                                   ) ls1657), 
            PrettyRep.List (List.map (fn x1661 => cvtINIT_STEP x1661
                                     ) ls1662)])]))
       )), ("init", PrettyRep.List (List.map (fn x1682 => cvtSTMT x1682
                                             ) ls1683)), ("cond", cvtEXPR x1687), 
          ("update", cvtEXPR x1688), ("labels", PrettyRep.List (List.map (fn x1689 => 
                                                                                cvtIDENT x1689
                                                                         ) ls1690)), 
          ("body", cvtSTMT x1694)]
   and cvtWHILE_STMT {cond=x1710, rib=opt1718, body=x1722, labels=ls1724} = 
          PrettyRep.Rec [("cond", cvtEXPR x1710), ("rib", 
       (case opt1718 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1714 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1711, 
                                                                                      x1712) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1711, 
                                                                                      cvtFIXTURE x1712]
                                                                               ) ls1714)))
       )), ("body", cvtSTMT x1722), ("labels", PrettyRep.List (List.map (fn x1723 => 
                                                                               cvtIDENT x1723
                                                                        ) ls1724))]
   and cvtDIRECTIVES {pragmas=ls1738, defns=ls1743, head=opt1748, body=ls1753, 
          loc=opt1758} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1737 => 
                                                                                    cvtPRAGMA x1737
                                                                             ) ls1738)), 
          ("defns", PrettyRep.List (List.map (fn x1742 => cvtDEFN x1742
                                             ) ls1743)), ("head", 
       (case opt1748 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1747 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1747))
       )), ("body", PrettyRep.List (List.map (fn x1752 => cvtSTMT x1752
                                             ) ls1753)), ("loc", 
       (case opt1758 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1757 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1757))
       ))]
   and cvtCASE {label=opt1774, inits=opt1785, body=x1789} = PrettyRep.Rec [("label", 
          
       (case opt1774 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1773 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1773))
       )), ("inits", 
       (case opt1785 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1781 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1778, 
                                                                                      x1779) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1778, 
                                                                                      cvtEXPR x1779]
                                                                               ) ls1781)))
       )), ("body", cvtBLOCK x1789)]
   and cvtCATCH_CLAUSE {bindings=(ls1798, ls1803), ty=x1808, rib=opt1816, inits=opt1827, 
          block=x1831} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1797 => 
                                                                                                      cvtBINDING x1797
                                                                                               ) ls1798), 
          PrettyRep.List (List.map (fn x1802 => cvtINIT_STEP x1802
                                   ) ls1803)]), ("ty", cvtTY x1808), ("rib", 
          
       (case opt1816 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1812 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1809, 
                                                                                      x1810) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1809, 
                                                                                      cvtFIXTURE x1810]
                                                                               ) ls1812)))
       )), ("inits", 
       (case opt1827 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1823 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1820, 
                                                                                      x1821) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1820, 
                                                                                      cvtEXPR x1821]
                                                                               ) ls1823)))
       )), ("block", cvtBLOCK x1831)]
   and cvtFUNC_NAME {kind=x1843, ident=x1844} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1843), 
          ("ident", cvtIDENT x1844)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1850, getter=opt1852, setter=opt1857} = 
          PrettyRep.Rec [("ty", cvtTY x1850), ("getter", 
       (case opt1852 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1851 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1851))
       )), ("setter", 
       (case opt1857 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1856 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1856))
       ))]
   and cvtFRAGMENT (Unit{name=opt1869, fragments=ls1874}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1869 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1868 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1868))
       )), ("fragments", PrettyRep.List (List.map (fn x1873 => cvtFRAGMENT x1873
                                                  ) ls1874))]))
     | cvtFRAGMENT (Package{name=ls1886, fragments=ls1891}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1885 => 
                                                                        cvtIDENT x1885
                                                                 ) ls1886)), 
          ("fragments", PrettyRep.List (List.map (fn x1890 => cvtFRAGMENT x1890
                                                 ) ls1891))]))
     | cvtFRAGMENT (Anon x1902) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1902))
end

