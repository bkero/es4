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
   and cvtFUNC_SIG (FunctionSignature{params=x518, paramTypes=ls520, defaults=ls525, 
          ctorInits=opt536, returnType=x540, thisType=opt542, hasRest=b546}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("params", 
          cvtBINDINGS x518), ("paramTypes", PrettyRep.List (List.map (fn x519 => 
                                                                            cvtTYPE_EXPR x519
                                                                     ) ls520)), 
          ("defaults", PrettyRep.List (List.map (fn x524 => cvtEXPR x524
                                                ) ls525)), ("ctorInits", 
       (case opt536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x529, ls531) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x529, 
            PrettyRep.List (List.map (fn x530 => cvtEXPR x530
                                     ) ls531)]))
       )), ("returnType", cvtTYPE_EXPR x540), ("thisType", 
       (case opt542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x541 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x541))
       )), ("hasRest", PrettyRep.Bool b546)]))
   and cvtBINDING (Binding{ident=x564, ty=x565}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x564), ("ty", cvtTYPE_EXPR x565)]))
   and cvtBINDING_IDENT (TempIdent n573) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n573))
     | cvtBINDING_IDENT (ParamIdent n576) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n576))
     | cvtBINDING_IDENT (PropIdent x579) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x579))
   and cvtINIT_STEP (InitStep(x582, x583)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x582, 
          cvtEXPR x583]))
     | cvtINIT_STEP (AssignStep(x587, x588)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x587, cvtEXPR x588]))
   and cvtTYPE_EXPR (SpecialType x592) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x592))
     | cvtTYPE_EXPR (UnionType ls596) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x595 => 
                                                                                                           cvtTYPE_EXPR x595
                                                                                                    ) ls596)))
     | cvtTYPE_EXPR (ArrayType ls603) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x602 => 
                                                                                                           cvtTYPE_EXPR x602
                                                                                                    ) ls603)))
     | cvtTYPE_EXPR (TypeName x609) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x609))
     | cvtTYPE_EXPR (ElementTypeRef(x612, n613)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x612, PrettyRep.Int n613]))
     | cvtTYPE_EXPR (FieldTypeRef(x617, x618)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x617, cvtIDENT x618]))
     | cvtTYPE_EXPR (FunctionType x622) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x622))
     | cvtTYPE_EXPR (ObjectType ls626) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x625 => 
                                                                                                             cvtFIELD_TYPE x625
                                                                                                      ) ls626)))
     | cvtTYPE_EXPR (AppType{base=x632, args=ls634}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x632), ("args", PrettyRep.List (List.map (fn x633 => 
                                                                                                     cvtTYPE_EXPR x633
                                                                                              ) ls634))]))
     | cvtTYPE_EXPR (LamType{params=ls646, body=x650}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x645 => 
                                                                          cvtIDENT x645
                                                                   ) ls646)), 
          ("body", cvtTYPE_EXPR x650)]))
     | cvtTYPE_EXPR (NullableType{expr=x658, nullable=b659}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x658), ("nullable", PrettyRep.Bool b659)]))
     | cvtTYPE_EXPR (InstanceType x667) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x667))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x671) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x671))
     | cvtSTMT (InitStmt{kind=x674, ns=opt676, prototype=b680, static=b681, 
          temps=x682, inits=ls684}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x674), ("ns", 
       (case opt676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x675 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x675))
       )), ("prototype", PrettyRep.Bool b680), ("static", PrettyRep.Bool b681), 
          ("temps", cvtBINDINGS x682), ("inits", PrettyRep.List (List.map (fn x683 => 
                                                                                 cvtINIT_STEP x683
                                                                          ) ls684))]))
     | cvtSTMT (ClassBlock{ns=opt704, ident=x708, name=opt710, block=x714}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt704 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x703 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x703))
       )), ("ident", cvtIDENT x708), ("name", 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x709 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x709))
       )), ("block", cvtBLOCK x714)]))
     | cvtSTMT (ForInStmt x726) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x726))
     | cvtSTMT (ThrowStmt x729) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x729))
     | cvtSTMT (ReturnStmt x732) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x732))
     | cvtSTMT (BreakStmt opt736) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x735))
       ))
     | cvtSTMT (ContinueStmt opt743) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt743 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x742 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x742))
       ))
     | cvtSTMT (BlockStmt x749) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x749))
     | cvtSTMT (LabeledStmt(x752, x753)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x752, 
          cvtSTMT x753]))
     | cvtSTMT (LetStmt x757) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x757))
     | cvtSTMT (WhileStmt x760) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x760))
     | cvtSTMT (DoWhileStmt x763) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x763))
     | cvtSTMT (ForStmt x766) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x766))
     | cvtSTMT (IfStmt{cnd=x769, thn=x770, els=x771}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x769), ("thn", cvtSTMT x770), 
          ("els", cvtSTMT x771)]))
     | cvtSTMT (WithStmt{obj=x781, ty=x782, body=x783}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x781), ("ty", cvtTY x782), ("body", 
          cvtSTMT x783)]))
     | cvtSTMT (TryStmt{block=x793, catches=ls795, finally=opt800}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x793), ("catches", PrettyRep.List (List.map (fn x794 => 
                                                                                                     cvtCATCH_CLAUSE x794
                                                                                              ) ls795)), 
          ("finally", 
       (case opt800 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x799 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x799))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt814, cond=x818, labels=ls820, cases=ls825}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt814 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x813 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x813))
       )), ("cond", cvtEXPR x818), ("labels", PrettyRep.List (List.map (fn x819 => 
                                                                              cvtIDENT x819
                                                                       ) ls820)), 
          ("cases", PrettyRep.List (List.map (fn x824 => cvtCASE x824
                                             ) ls825))]))
     | cvtSTMT (SwitchTypeStmt{cond=x840, ty=x841, cases=ls843}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x840), ("ty", cvtTY x841), 
          ("cases", PrettyRep.List (List.map (fn x842 => cvtCATCH_CLAUSE x842
                                             ) ls843))]))
     | cvtSTMT (DXNStmt{expr=x856}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x856)]))
   and cvtEXPR (TernaryExpr(x862, x863, x864)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x862, cvtEXPR x863, cvtEXPR x864]))
     | cvtEXPR (BinaryExpr(x868, x869, x870)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x868, cvtEXPR x869, cvtEXPR x870]))
     | cvtEXPR (BinaryTypeExpr(x874, x875, x876)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x874, cvtEXPR x875, cvtTY x876]))
     | cvtEXPR (ExpectedTypeExpr(x880, x881)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTY x880, cvtEXPR x881]))
     | cvtEXPR (UnaryExpr(x885, x886)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x885, 
          cvtEXPR x886]))
     | cvtEXPR (TypeExpr x890) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x890))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt895) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x894))
       ))
     | cvtEXPR (SuperExpr opt902) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt902 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x901 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x901))
       ))
     | cvtEXPR (LiteralExpr x908) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x908))
     | cvtEXPR (CallExpr{func=x911, actuals=ls913}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x911), ("actuals", PrettyRep.List (List.map (fn x912 => 
                                                                                                   cvtEXPR x912
                                                                                            ) ls913))]))
     | cvtEXPR (ApplyTypeExpr{expr=x924, actuals=ls926}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x924), ("actuals", PrettyRep.List (List.map (fn x925 => 
                                                                                                   cvtTY x925
                                                                                            ) ls926))]))
     | cvtEXPR (LetExpr{defs=x937, body=x938, head=opt940}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x937), ("body", cvtEXPR x938), 
          ("head", 
       (case opt940 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x939 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x939))
       ))]))
     | cvtEXPR (NewExpr{obj=x953, actuals=ls955}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x953), ("actuals", PrettyRep.List (List.map (fn x954 => 
                                                                                                  cvtEXPR x954
                                                                                           ) ls955))]))
     | cvtEXPR (ObjectRef{base=x966, ident=x967, loc=opt969}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x966), ("ident", cvtIDENT_EXPR x967), 
          ("loc", 
       (case opt969 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x968 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x968))
       ))]))
     | cvtEXPR (LexicalRef{ident=x982, loc=opt984}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x982), ("loc", 
       (case opt984 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x983 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x983))
       ))]))
     | cvtEXPR (SetExpr(x995, x996, x997)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x995, 
          cvtEXPR x996, cvtEXPR x997]))
     | cvtEXPR (ListExpr ls1002) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x1001 => 
                                                                                                     cvtEXPR x1001
                                                                                              ) ls1002)))
     | cvtEXPR (InitExpr(x1008, x1009, x1010)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x1008, cvtHEAD x1009, cvtINITS x1010]))
     | cvtEXPR (SliceExpr(x1014, x1015, x1016)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x1014, cvtEXPR x1015, cvtEXPR x1016]))
     | cvtEXPR (GetTemp n1020) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n1020))
     | cvtEXPR (GetParam n1023) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n1023))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1029) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1029))
     | cvtFIXTURE_NAME (PropName x1032) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1032))
   and cvtIDENT_EXPR (Identifier{ident=x1035, openNamespaces=ls1041}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1035), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1037 => PrettyRep.List (List.map (fn x1036 => 
                                                                                 cvtNAMESPACE x1036
                                                                          ) ls1037)
                                   ) ls1041))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1052, expr=x1053}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1052), ("expr", cvtEXPR x1053)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1061) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1061))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1064, openNamespaces=ls1070}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1064), ("openNamespaces", PrettyRep.List (List.map (fn ls1066 => 
                                                                             PrettyRep.List (List.map (fn x1065 => 
                                                                                                             cvtNAMESPACE x1065
                                                                                                      ) ls1066)
                                                                      ) ls1070))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1081, ident=s1082}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1081), ("ident", PrettyRep.UniStr s1082)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1091, x1095)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1090 => cvtIDENT x1090
                                                          ) ls1091), cvtIDENT_EXPR x1095]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1102) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1102))
     | cvtLITERAL (LiteralContextualDecimalInteger s1105) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1105))
     | cvtLITERAL (LiteralContextualHexInteger s1108) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1108))
     | cvtLITERAL (LiteralDouble r1111) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1111))
     | cvtLITERAL (LiteralDecimal d1114) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1114))
     | cvtLITERAL (LiteralInt i1117) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1117))
     | cvtLITERAL (LiteralUInt u1120) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1120))
     | cvtLITERAL (LiteralBoolean b1123) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1123))
     | cvtLITERAL (LiteralString s1126) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1126))
     | cvtLITERAL (LiteralArray{exprs=ls1130, ty=opt1135}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1129 => 
                                                                         cvtEXPR x1129
                                                                  ) ls1130)), 
          ("ty", 
       (case opt1135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1134 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1134))
       ))]))
     | cvtLITERAL (LiteralXML ls1147) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1146 => 
                                                                                                            cvtEXPR x1146
                                                                                                     ) ls1147)))
     | cvtLITERAL (LiteralNamespace x1153) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1153))
     | cvtLITERAL (LiteralObject{expr=ls1157, ty=opt1162}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1156 => 
                                                                        cvtFIELD x1156
                                                                 ) ls1157)), 
          ("ty", 
       (case opt1162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1161 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1161))
       ))]))
     | cvtLITERAL (LiteralFunction x1173) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1173))
     | cvtLITERAL (LiteralRegExp{str=s1176}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1176)]))
   and cvtBLOCK (Block x1182) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1182))
   and cvtFIXTURE (NamespaceFixture x1185) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1185))
     | cvtFIXTURE (ClassFixture x1188) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1188))
     | cvtFIXTURE (InterfaceFixture x1191) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1191))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1195) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1195))
     | cvtFIXTURE (MethodFixture{func=x1198, ty=x1199, readOnly=b1200, override=b1201, 
          final=b1202}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1198), ("ty", cvtTY x1199), ("readOnly", PrettyRep.Bool b1200), 
          ("override", PrettyRep.Bool b1201), ("final", PrettyRep.Bool b1202)]))
     | cvtFIXTURE (ValFixture{ty=x1216, readOnly=b1217}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1216), ("readOnly", PrettyRep.Bool b1217)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1225, getter=opt1227, setter=opt1232}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1225), ("getter", 
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1226))
       )), ("setter", 
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1231))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1245, baseTypeArgs=ls1247}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1245), ("baseTypeArgs", PrettyRep.List (List.map (fn x1246 => 
                                                                           cvtTY x1246
                                                                    ) ls1247))]))
   and cvtHEAD (Head(x1258, x1259)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1258, 
          cvtINITS x1259]))
   and cvtBINDINGS (ls1264, ls1269) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1263 => 
                                                                                       cvtBINDING x1263
                                                                                ) ls1264), 
          PrettyRep.List (List.map (fn x1268 => cvtINIT_STEP x1268
                                   ) ls1269)]
   and cvtRIB ls1277 = PrettyRep.List (List.map (fn (x1274, x1275) => PrettyRep.Tuple [cvtFIXTURE_NAME x1274, 
                                                       cvtFIXTURE x1275]
                                                ) ls1277)
   and cvtRIBS ls1282 = PrettyRep.List (List.map (fn x1281 => cvtRIB x1281
                                                 ) ls1282)
   and cvtINITS ls1289 = PrettyRep.List (List.map (fn (x1286, x1287) => PrettyRep.Tuple [cvtFIXTURE_NAME x1286, 
                                                         cvtEXPR x1287]
                                                  ) ls1289)
   and cvtINSTANCE_TYPE {name=x1293, typeArgs=ls1295, nonnullable=b1299, superTypes=ls1301, 
          ty=x1305, conversionTy=opt1307, dynamic=b1311} = PrettyRep.Rec [("name", 
          cvtNAME x1293), ("typeArgs", PrettyRep.List (List.map (fn x1294 => 
                                                                       cvtTYPE_EXPR x1294
                                                                ) ls1295)), 
          ("nonnullable", PrettyRep.Bool b1299), ("superTypes", PrettyRep.List (List.map (fn x1300 => 
                                                                                                cvtTYPE_EXPR x1300
                                                                                         ) ls1301)), 
          ("ty", cvtTYPE_EXPR x1305), ("conversionTy", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1306))
       )), ("dynamic", PrettyRep.Bool b1311)]
   and cvtFIELD {kind=x1327, name=x1328, init=x1329} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1327), ("name", cvtIDENT_EXPR x1328), ("init", cvtEXPR x1329)]
   and cvtFIELD_TYPE {name=x1337, ty=x1338} = PrettyRep.Rec [("name", cvtIDENT x1337), 
          ("ty", cvtTYPE_EXPR x1338)]
   and cvtFUNC_TYPE {params=ls1345, result=x1349, thisType=opt1351, hasRest=b1355, 
          minArgs=n1356} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1344 => 
                                                                                     cvtTYPE_EXPR x1344
                                                                              ) ls1345)), 
          ("result", cvtTYPE_EXPR x1349), ("thisType", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1350))
       )), ("hasRest", PrettyRep.Bool b1355), ("minArgs", PrettyRep.Int n1356)]
   and cvtFUNC_DEFN {kind=x1368, ns=opt1370, final=b1374, override=b1375, prototype=b1376, 
          static=b1377, func=x1378} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1368), 
          ("ns", 
       (case opt1370 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1369 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1369))
       )), ("final", PrettyRep.Bool b1374), ("override", PrettyRep.Bool b1375), 
          ("prototype", PrettyRep.Bool b1376), ("static", PrettyRep.Bool b1377), 
          ("func", cvtFUNC x1378)]
   and cvtCTOR_DEFN x1394 = cvtCTOR x1394
   and cvtVAR_DEFN {kind=x1395, ns=opt1397, static=b1401, prototype=b1402, 
          bindings=(ls1404, ls1409)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1395), 
          ("ns", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1396))
       )), ("static", PrettyRep.Bool b1401), ("prototype", PrettyRep.Bool b1402), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1403 => 
                                                                        cvtBINDING x1403
                                                                 ) ls1404), 
          PrettyRep.List (List.map (fn x1408 => cvtINIT_STEP x1408
                                   ) ls1409)])]
   and cvtNAMESPACE_DEFN {ident=x1425, ns=opt1427, init=opt1432} = PrettyRep.Rec [("ident", 
          cvtIDENT x1425), ("ns", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1426))
       )), ("init", 
       (case opt1432 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1431 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1431))
       ))]
   and cvtCLASS_DEFN {ns=opt1444, ident=x1448, nonnullable=b1449, dynamic=b1450, 
          final=b1451, params=ls1453, extends=opt1458, implements=ls1463, classDefns=ls1468, 
          instanceDefns=ls1473, instanceStmts=ls1478, ctorDefn=opt1483} = PrettyRep.Rec [("ns", 
          
       (case opt1444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1443 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1443))
       )), ("ident", cvtIDENT x1448), ("nonnullable", PrettyRep.Bool b1449), 
          ("dynamic", PrettyRep.Bool b1450), ("final", PrettyRep.Bool b1451), 
          ("params", PrettyRep.List (List.map (fn x1452 => cvtIDENT x1452
                                              ) ls1453)), ("extends", 
       (case opt1458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1457 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1457))
       )), ("implements", PrettyRep.List (List.map (fn x1462 => cvtTYPE_EXPR x1462
                                                   ) ls1463)), ("classDefns", 
          PrettyRep.List (List.map (fn x1467 => cvtDEFN x1467
                                   ) ls1468)), ("instanceDefns", PrettyRep.List (List.map (fn x1472 => 
                                                                                                 cvtDEFN x1472
                                                                                          ) ls1473)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1477 => cvtSTMT x1477
                                                     ) ls1478)), ("ctorDefn", 
          
       (case opt1483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1482 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1482))
       ))]
   and cvtINTERFACE_DEFN {ident=x1512, ns=opt1514, nonnullable=b1518, params=ls1520, 
          extends=ls1525, instanceDefns=ls1530} = PrettyRep.Rec [("ident", 
          cvtIDENT x1512), ("ns", 
       (case opt1514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1513 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1513))
       )), ("nonnullable", PrettyRep.Bool b1518), ("params", PrettyRep.List (List.map (fn x1519 => 
                                                                                             cvtIDENT x1519
                                                                                      ) ls1520)), 
          ("extends", PrettyRep.List (List.map (fn x1524 => cvtTYPE_EXPR x1524
                                               ) ls1525)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1529 => cvtDEFN x1529
                                   ) ls1530))]
   and cvtTYPE_DEFN {ident=x1547, ns=opt1549, init=x1553} = PrettyRep.Rec [("ident", 
          cvtIDENT x1547), ("ns", 
       (case opt1549 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1548 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1548))
       )), ("init", cvtTYPE_EXPR x1553)]
   and cvtFOR_ENUM_STMT {isEach=b1561, defn=opt1592, obj=x1596, rib=opt1604, 
          next=x1608, labels=ls1610, body=x1614} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1561), ("defn", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1562, ns=opt1564, static=b1568, prototype=b1569, bindings=(ls1571, 
            ls1576)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1562), ("ns", 
         (case opt1564 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1563))
         )), ("static", PrettyRep.Bool b1568), ("prototype", PrettyRep.Bool b1569), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1570 => 
                                                                          cvtBINDING x1570
                                                                   ) ls1571), 
            PrettyRep.List (List.map (fn x1575 => cvtINIT_STEP x1575
                                     ) ls1576)])]))
       )), ("obj", cvtEXPR x1596), ("rib", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1600 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1597, 
                                                                                      x1598) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1597, 
                                                                                      cvtFIXTURE x1598]
                                                                               ) ls1600)))
       )), ("next", cvtSTMT x1608), ("labels", PrettyRep.List (List.map (fn x1609 => 
                                                                               cvtIDENT x1609
                                                                        ) ls1610)), 
          ("body", cvtSTMT x1614)]
   and cvtFOR_STMT {rib=opt1637, defn=opt1671, init=ls1676, cond=x1680, update=x1681, 
          labels=ls1683, body=x1687} = PrettyRep.Rec [("rib", 
       (case opt1637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1633 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1630, 
                                                                                      x1631) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1630, 
                                                                                      cvtFIXTURE x1631]
                                                                               ) ls1633)))
       )), ("defn", 
       (case opt1671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1641, ns=opt1643, static=b1647, prototype=b1648, bindings=(ls1650, 
            ls1655)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1641), ("ns", 
         (case opt1643 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1642 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1642))
         )), ("static", PrettyRep.Bool b1647), ("prototype", PrettyRep.Bool b1648), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1649 => 
                                                                          cvtBINDING x1649
                                                                   ) ls1650), 
            PrettyRep.List (List.map (fn x1654 => cvtINIT_STEP x1654
                                     ) ls1655)])]))
       )), ("init", PrettyRep.List (List.map (fn x1675 => cvtSTMT x1675
                                             ) ls1676)), ("cond", cvtEXPR x1680), 
          ("update", cvtEXPR x1681), ("labels", PrettyRep.List (List.map (fn x1682 => 
                                                                                cvtIDENT x1682
                                                                         ) ls1683)), 
          ("body", cvtSTMT x1687)]
   and cvtWHILE_STMT {cond=x1703, rib=opt1711, body=x1715, labels=ls1717} = 
          PrettyRep.Rec [("cond", cvtEXPR x1703), ("rib", 
       (case opt1711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1707 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1704, 
                                                                                      x1705) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1704, 
                                                                                      cvtFIXTURE x1705]
                                                                               ) ls1707)))
       )), ("body", cvtSTMT x1715), ("labels", PrettyRep.List (List.map (fn x1716 => 
                                                                               cvtIDENT x1716
                                                                        ) ls1717))]
   and cvtDIRECTIVES {pragmas=ls1731, defns=ls1736, head=opt1741, body=ls1746, 
          loc=opt1751} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1730 => 
                                                                                    cvtPRAGMA x1730
                                                                             ) ls1731)), 
          ("defns", PrettyRep.List (List.map (fn x1735 => cvtDEFN x1735
                                             ) ls1736)), ("head", 
       (case opt1741 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1740 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1740))
       )), ("body", PrettyRep.List (List.map (fn x1745 => cvtSTMT x1745
                                             ) ls1746)), ("loc", 
       (case opt1751 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1750 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1750))
       ))]
   and cvtCASE {label=opt1767, inits=opt1778, body=x1782} = PrettyRep.Rec [("label", 
          
       (case opt1767 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1766 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1766))
       )), ("inits", 
       (case opt1778 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1774 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1771, 
                                                                                      x1772) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1771, 
                                                                                      cvtEXPR x1772]
                                                                               ) ls1774)))
       )), ("body", cvtBLOCK x1782)]
   and cvtCATCH_CLAUSE {bindings=(ls1791, ls1796), ty=x1801, rib=opt1809, inits=opt1820, 
          block=x1824} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1790 => 
                                                                                                      cvtBINDING x1790
                                                                                               ) ls1791), 
          PrettyRep.List (List.map (fn x1795 => cvtINIT_STEP x1795
                                   ) ls1796)]), ("ty", cvtTY x1801), ("rib", 
          
       (case opt1809 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1805 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1802, 
                                                                                      x1803) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1802, 
                                                                                      cvtFIXTURE x1803]
                                                                               ) ls1805)))
       )), ("inits", 
       (case opt1820 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1816 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1813, 
                                                                                      x1814) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1813, 
                                                                                      cvtEXPR x1814]
                                                                               ) ls1816)))
       )), ("block", cvtBLOCK x1824)]
   and cvtFUNC_NAME {kind=x1836, ident=x1837} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1836), 
          ("ident", cvtIDENT x1837)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1843, getter=opt1845, setter=opt1850} = 
          PrettyRep.Rec [("ty", cvtTY x1843), ("getter", 
       (case opt1845 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1844 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1844))
       )), ("setter", 
       (case opt1850 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1849 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1849))
       ))]
   and cvtFRAGMENT (Unit{name=opt1862, fragments=ls1867}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1862 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1861 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1861))
       )), ("fragments", PrettyRep.List (List.map (fn x1866 => cvtFRAGMENT x1866
                                                  ) ls1867))]))
     | cvtFRAGMENT (Package{name=ls1879, fragments=ls1884}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1878 => 
                                                                        cvtIDENT x1878
                                                                 ) ls1879)), 
          ("fragments", PrettyRep.List (List.map (fn x1883 => cvtFRAGMENT x1883
                                                 ) ls1884))]))
     | cvtFRAGMENT (Anon x1895) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1895))
end

