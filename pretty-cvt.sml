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
   and cvtTY (Ty{expr=x335, env=x336, unit=opt338}) = PrettyRep.Ctor ("Ty", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x335), ("env", cvtRIBS x336), 
          ("unit", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x337))
       ))]))
   and cvtCLS (Cls{name=x351, nonnullable=b352, dynamic=b353, extends=opt355, 
          implements=ls360, classRib=x364, instanceRib=x365, instanceInits=x366, 
          constructor=opt368, classType=x372, instanceType=x373}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x351), ("nonnullable", PrettyRep.Bool b352), 
          ("dynamic", PrettyRep.Bool b353), ("extends", 
       (case opt355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x354 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x354))
       )), ("implements", PrettyRep.List (List.map (fn x359 => cvtNAME x359
                                                   ) ls360)), ("classRib", 
          cvtRIB x364), ("instanceRib", cvtRIB x365), ("instanceInits", cvtHEAD x366), 
          ("constructor", 
       (case opt368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x367 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x367))
       )), ("classType", cvtTY x372), ("instanceType", cvtTY x373)]))
   and cvtIFACE (Iface{name=x399, nonnullable=b400, extends=ls402, instanceRib=x406, 
          instanceType=x407}) = PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x399), ("nonnullable", PrettyRep.Bool b400), ("extends", 
          PrettyRep.List (List.map (fn x401 => cvtNAME x401
                                   ) ls402)), ("instanceRib", cvtRIB x406), 
          ("instanceType", cvtTY x407)]))
   and cvtCTOR (Ctor{settings=x421, superArgs=ls423, func=x427}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x421), ("superArgs", PrettyRep.List (List.map (fn x422 => 
                                                                                                         cvtEXPR x422
                                                                                                  ) ls423)), 
          ("func", cvtFUNC x427)]))
   and cvtFUNC (Func{name=x437, fsig=x438, native=b439, block=x440, param=x441, 
          defaults=ls443, ty=x447, loc=opt449}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x437), ("fsig", cvtFUNC_SIG x438), ("native", PrettyRep.Bool b439), 
          ("block", cvtBLOCK x440), ("param", cvtHEAD x441), ("defaults", PrettyRep.List (List.map (fn x442 => 
                                                                                                          cvtEXPR x442
                                                                                                   ) ls443)), 
          ("ty", cvtTY x447), ("loc", 
       (case opt449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x448 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x448))
       ))]))
   and cvtDEFN (ClassDefn x472) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x472))
     | cvtDEFN (VariableDefn x475) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x475))
     | cvtDEFN (FunctionDefn x478) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x478))
     | cvtDEFN (ConstructorDefn x481) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x481))
     | cvtDEFN (InterfaceDefn x484) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x484))
     | cvtDEFN (NamespaceDefn x487) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x487))
     | cvtDEFN (TypeDefn x490) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x490))
   and cvtFUNC_SIG (FunctionSignature{params=x493, paramTypes=ls495, defaults=ls500, 
          ctorInits=opt511, returnType=x515, thisType=opt517, hasRest=b521}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("params", 
          cvtBINDINGS x493), ("paramTypes", PrettyRep.List (List.map (fn x494 => 
                                                                            cvtTYPE_EXPR x494
                                                                     ) ls495)), 
          ("defaults", PrettyRep.List (List.map (fn x499 => cvtEXPR x499
                                                ) ls500)), ("ctorInits", 
       (case opt511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x504, ls506) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x504, 
            PrettyRep.List (List.map (fn x505 => cvtEXPR x505
                                     ) ls506)]))
       )), ("returnType", cvtTYPE_EXPR x515), ("thisType", 
       (case opt517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x516 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x516))
       )), ("hasRest", PrettyRep.Bool b521)]))
   and cvtBINDING (Binding{ident=x539, ty=x540}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x539), ("ty", cvtTYPE_EXPR x540)]))
   and cvtBINDING_IDENT (TempIdent n548) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n548))
     | cvtBINDING_IDENT (ParamIdent n551) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n551))
     | cvtBINDING_IDENT (PropIdent x554) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x554))
   and cvtINIT_STEP (InitStep(x557, x558)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x557, 
          cvtEXPR x558]))
     | cvtINIT_STEP (AssignStep(x562, x563)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x562, cvtEXPR x563]))
   and cvtTYPE_EXPR (SpecialType x567) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x567))
     | cvtTYPE_EXPR (UnionType ls571) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x570 => 
                                                                                                           cvtTYPE_EXPR x570
                                                                                                    ) ls571)))
     | cvtTYPE_EXPR (ArrayType ls578) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x577 => 
                                                                                                           cvtTYPE_EXPR x577
                                                                                                    ) ls578)))
     | cvtTYPE_EXPR (TypeName x584) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x584))
     | cvtTYPE_EXPR (ElementTypeRef(x587, n588)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x587, PrettyRep.Int n588]))
     | cvtTYPE_EXPR (FieldTypeRef(x592, x593)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x592, cvtIDENT x593]))
     | cvtTYPE_EXPR (FunctionType x597) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x597))
     | cvtTYPE_EXPR (ObjectType ls601) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x600 => 
                                                                                                             cvtFIELD_TYPE x600
                                                                                                      ) ls601)))
     | cvtTYPE_EXPR (AppType{base=x607, args=ls609}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x607), ("args", PrettyRep.List (List.map (fn x608 => 
                                                                                                     cvtTYPE_EXPR x608
                                                                                              ) ls609))]))
     | cvtTYPE_EXPR (LamType{params=ls621, body=x625}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x620 => 
                                                                          cvtIDENT x620
                                                                   ) ls621)), 
          ("body", cvtTYPE_EXPR x625)]))
     | cvtTYPE_EXPR (NullableType{expr=x633, nullable=b634}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x633), ("nullable", PrettyRep.Bool b634)]))
     | cvtTYPE_EXPR (InstanceType x642) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x642))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x646) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x646))
     | cvtSTMT (InitStmt{kind=x649, ns=opt651, prototype=b655, static=b656, 
          temps=x657, inits=ls659}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x649), ("ns", 
       (case opt651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x650 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x650))
       )), ("prototype", PrettyRep.Bool b655), ("static", PrettyRep.Bool b656), 
          ("temps", cvtBINDINGS x657), ("inits", PrettyRep.List (List.map (fn x658 => 
                                                                                 cvtINIT_STEP x658
                                                                          ) ls659))]))
     | cvtSTMT (ClassBlock{ns=opt679, ident=x683, name=opt685, block=x689}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x678 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x678))
       )), ("ident", cvtIDENT x683), ("name", 
       (case opt685 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x684 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x684))
       )), ("block", cvtBLOCK x689)]))
     | cvtSTMT (ForInStmt x701) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x701))
     | cvtSTMT (ThrowStmt x704) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x704))
     | cvtSTMT (ReturnStmt x707) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x707))
     | cvtSTMT (BreakStmt opt711) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x710 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x710))
       ))
     | cvtSTMT (ContinueStmt opt718) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt718 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x717 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x717))
       ))
     | cvtSTMT (BlockStmt x724) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x724))
     | cvtSTMT (LabeledStmt(x727, x728)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x727, 
          cvtSTMT x728]))
     | cvtSTMT (LetStmt x732) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x732))
     | cvtSTMT (WhileStmt x735) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x735))
     | cvtSTMT (DoWhileStmt x738) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x738))
     | cvtSTMT (ForStmt x741) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x741))
     | cvtSTMT (IfStmt{cnd=x744, thn=x745, els=x746}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x744), ("thn", cvtSTMT x745), 
          ("els", cvtSTMT x746)]))
     | cvtSTMT (WithStmt{obj=x756, ty=x757, body=x758}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x756), ("ty", cvtTY x757), ("body", 
          cvtSTMT x758)]))
     | cvtSTMT (TryStmt{block=x768, catches=ls770, finally=opt775}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x768), ("catches", PrettyRep.List (List.map (fn x769 => 
                                                                                                     cvtCATCH_CLAUSE x769
                                                                                              ) ls770)), 
          ("finally", 
       (case opt775 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x774 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x774))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt789, cond=x793, labels=ls795, cases=ls800}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt789 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x788 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x788))
       )), ("cond", cvtEXPR x793), ("labels", PrettyRep.List (List.map (fn x794 => 
                                                                              cvtIDENT x794
                                                                       ) ls795)), 
          ("cases", PrettyRep.List (List.map (fn x799 => cvtCASE x799
                                             ) ls800))]))
     | cvtSTMT (SwitchTypeStmt{cond=x815, ty=x816, cases=ls818}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x815), ("ty", cvtTY x816), 
          ("cases", PrettyRep.List (List.map (fn x817 => cvtCATCH_CLAUSE x817
                                             ) ls818))]))
     | cvtSTMT (DXNStmt{expr=x831}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x831)]))
   and cvtEXPR (TernaryExpr(x837, x838, x839)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x837, cvtEXPR x838, cvtEXPR x839]))
     | cvtEXPR (BinaryExpr(x843, x844, x845)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x843, cvtEXPR x844, cvtEXPR x845]))
     | cvtEXPR (BinaryTypeExpr(x849, x850, x851)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x849, cvtEXPR x850, cvtTY x851]))
     | cvtEXPR (ExpectedTypeExpr(x855, x856)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTY x855, cvtEXPR x856]))
     | cvtEXPR (UnaryExpr(x860, x861)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x860, 
          cvtEXPR x861]))
     | cvtEXPR (TypeExpr x865) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x865))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt870) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt870 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x869 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x869))
       ))
     | cvtEXPR (SuperExpr opt877) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt877 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x876 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x876))
       ))
     | cvtEXPR (LiteralExpr x883) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x883))
     | cvtEXPR (CallExpr{func=x886, actuals=ls888}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x886), ("actuals", PrettyRep.List (List.map (fn x887 => 
                                                                                                   cvtEXPR x887
                                                                                            ) ls888))]))
     | cvtEXPR (ApplyTypeExpr{expr=x899, actuals=ls901}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x899), ("actuals", PrettyRep.List (List.map (fn x900 => 
                                                                                                   cvtTY x900
                                                                                            ) ls901))]))
     | cvtEXPR (LetExpr{defs=x912, body=x913, head=opt915}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x912), ("body", cvtEXPR x913), 
          ("head", 
       (case opt915 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x914 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x914))
       ))]))
     | cvtEXPR (NewExpr{obj=x928, actuals=ls930}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x928), ("actuals", PrettyRep.List (List.map (fn x929 => 
                                                                                                  cvtEXPR x929
                                                                                           ) ls930))]))
     | cvtEXPR (ObjectRef{base=x941, ident=x942, loc=opt944}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x941), ("ident", cvtIDENT_EXPR x942), 
          ("loc", 
       (case opt944 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x943 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x943))
       ))]))
     | cvtEXPR (LexicalRef{ident=x957, loc=opt959}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x957), ("loc", 
       (case opt959 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x958 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x958))
       ))]))
     | cvtEXPR (SetExpr(x970, x971, x972)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x970, 
          cvtEXPR x971, cvtEXPR x972]))
     | cvtEXPR (ListExpr ls977) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x976 => 
                                                                                                    cvtEXPR x976
                                                                                             ) ls977)))
     | cvtEXPR (InitExpr(x983, x984, x985)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x983, 
          cvtHEAD x984, cvtINITS x985]))
     | cvtEXPR (SliceExpr(x989, x990, x991)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x989, cvtEXPR x990, cvtEXPR x991]))
     | cvtEXPR (GetTemp n995) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n995))
     | cvtEXPR (GetParam n998) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n998))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1004) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1004))
     | cvtFIXTURE_NAME (PropName x1007) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1007))
   and cvtIDENT_EXPR (Identifier{ident=x1010, openNamespaces=ls1016}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1010), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1012 => PrettyRep.List (List.map (fn x1011 => 
                                                                                 cvtNAMESPACE x1011
                                                                          ) ls1012)
                                   ) ls1016))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1027, expr=x1028}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1027), ("expr", cvtEXPR x1028)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1036) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1036))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1039, openNamespaces=ls1045}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1039), ("openNamespaces", PrettyRep.List (List.map (fn ls1041 => 
                                                                             PrettyRep.List (List.map (fn x1040 => 
                                                                                                             cvtNAMESPACE x1040
                                                                                                      ) ls1041)
                                                                      ) ls1045))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1056, ident=s1057}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1056), ("ident", PrettyRep.UniStr s1057)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1066, x1070)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1065 => cvtIDENT x1065
                                                          ) ls1066), cvtIDENT_EXPR x1070]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1077) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1077))
     | cvtLITERAL (LiteralContextualDecimalInteger s1080) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1080))
     | cvtLITERAL (LiteralContextualHexInteger s1083) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1083))
     | cvtLITERAL (LiteralDouble r1086) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1086))
     | cvtLITERAL (LiteralDecimal d1089) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1089))
     | cvtLITERAL (LiteralInt i1092) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1092))
     | cvtLITERAL (LiteralUInt u1095) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1095))
     | cvtLITERAL (LiteralBoolean b1098) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1098))
     | cvtLITERAL (LiteralString s1101) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1101))
     | cvtLITERAL (LiteralArray{exprs=ls1105, ty=opt1110}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1104 => 
                                                                         cvtEXPR x1104
                                                                  ) ls1105)), 
          ("ty", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1109 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1109))
       ))]))
     | cvtLITERAL (LiteralXML ls1122) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1121 => 
                                                                                                            cvtEXPR x1121
                                                                                                     ) ls1122)))
     | cvtLITERAL (LiteralNamespace x1128) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1128))
     | cvtLITERAL (LiteralObject{expr=ls1132, ty=opt1137}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1131 => 
                                                                        cvtFIELD x1131
                                                                 ) ls1132)), 
          ("ty", 
       (case opt1137 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1136 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1136))
       ))]))
     | cvtLITERAL (LiteralFunction x1148) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1148))
     | cvtLITERAL (LiteralRegExp{str=s1151}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1151)]))
   and cvtBLOCK (Block x1157) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1157))
   and cvtFIXTURE (NamespaceFixture x1160) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1160))
     | cvtFIXTURE (ClassFixture x1163) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1163))
     | cvtFIXTURE (InterfaceFixture x1166) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1166))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1170) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1170))
     | cvtFIXTURE (MethodFixture{func=x1173, ty=x1174, readOnly=b1175, override=b1176, 
          final=b1177, abstract=b1178}) = PrettyRep.Ctor ("MethodFixture", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1173), ("ty", cvtTY x1174), 
          ("readOnly", PrettyRep.Bool b1175), ("override", PrettyRep.Bool b1176), 
          ("final", PrettyRep.Bool b1177), ("abstract", PrettyRep.Bool b1178)]))
     | cvtFIXTURE (ValFixture{ty=x1194, readOnly=b1195}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1194), ("readOnly", PrettyRep.Bool b1195)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1203, getter=opt1205, setter=opt1210}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1203), ("getter", 
       (case opt1205 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1204 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1204))
       )), ("setter", 
       (case opt1210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1209 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1209))
       ))]))
   and cvtHEAD (Head(x1223, x1224)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1223, 
          cvtINITS x1224]))
   and cvtBINDINGS (ls1229, ls1234) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1228 => 
                                                                                       cvtBINDING x1228
                                                                                ) ls1229), 
          PrettyRep.List (List.map (fn x1233 => cvtINIT_STEP x1233
                                   ) ls1234)]
   and cvtRIB ls1242 = PrettyRep.List (List.map (fn (x1239, x1240) => PrettyRep.Tuple [cvtFIXTURE_NAME x1239, 
                                                       cvtFIXTURE x1240]
                                                ) ls1242)
   and cvtRIBS ls1247 = PrettyRep.List (List.map (fn x1246 => cvtRIB x1246
                                                 ) ls1247)
   and cvtINITS ls1254 = PrettyRep.List (List.map (fn (x1251, x1252) => PrettyRep.Tuple [cvtFIXTURE_NAME x1251, 
                                                         cvtEXPR x1252]
                                                  ) ls1254)
   and cvtINSTANCE_TYPE {name=x1258, typeArgs=ls1260, nonnullable=b1264, superTypes=ls1266, 
          ty=x1270, conversionTy=opt1272, dynamic=b1276} = PrettyRep.Rec [("name", 
          cvtNAME x1258), ("typeArgs", PrettyRep.List (List.map (fn x1259 => 
                                                                       cvtTYPE_EXPR x1259
                                                                ) ls1260)), 
          ("nonnullable", PrettyRep.Bool b1264), ("superTypes", PrettyRep.List (List.map (fn x1265 => 
                                                                                                cvtTYPE_EXPR x1265
                                                                                         ) ls1266)), 
          ("ty", cvtTYPE_EXPR x1270), ("conversionTy", 
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1271))
       )), ("dynamic", PrettyRep.Bool b1276)]
   and cvtFIELD {kind=x1292, name=x1293, init=x1294} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1292), ("name", cvtIDENT_EXPR x1293), ("init", cvtEXPR x1294)]
   and cvtFIELD_TYPE {name=x1302, ty=x1303} = PrettyRep.Rec [("name", cvtIDENT x1302), 
          ("ty", cvtTYPE_EXPR x1303)]
   and cvtFUNC_TYPE {params=ls1310, result=x1314, thisType=opt1316, hasRest=b1320, 
          minArgs=n1321} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1309 => 
                                                                                     cvtTYPE_EXPR x1309
                                                                              ) ls1310)), 
          ("result", cvtTYPE_EXPR x1314), ("thisType", 
       (case opt1316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1315 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1315))
       )), ("hasRest", PrettyRep.Bool b1320), ("minArgs", PrettyRep.Int n1321)]
   and cvtFUNC_DEFN {kind=x1333, ns=opt1335, final=b1339, override=b1340, prototype=b1341, 
          static=b1342, abstract=b1343, func=x1344} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1333), ("ns", 
       (case opt1335 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1334 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1334))
       )), ("final", PrettyRep.Bool b1339), ("override", PrettyRep.Bool b1340), 
          ("prototype", PrettyRep.Bool b1341), ("static", PrettyRep.Bool b1342), 
          ("abstract", PrettyRep.Bool b1343), ("func", cvtFUNC x1344)]
   and cvtCTOR_DEFN x1362 = cvtCTOR x1362
   and cvtVAR_DEFN {kind=x1363, ns=opt1365, static=b1369, prototype=b1370, 
          bindings=(ls1372, ls1377)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1363), 
          ("ns", 
       (case opt1365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1364))
       )), ("static", PrettyRep.Bool b1369), ("prototype", PrettyRep.Bool b1370), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1371 => 
                                                                        cvtBINDING x1371
                                                                 ) ls1372), 
          PrettyRep.List (List.map (fn x1376 => cvtINIT_STEP x1376
                                   ) ls1377)])]
   and cvtNAMESPACE_DEFN {ident=x1393, ns=opt1395, init=opt1400} = PrettyRep.Rec [("ident", 
          cvtIDENT x1393), ("ns", 
       (case opt1395 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1394 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1394))
       )), ("init", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1399))
       ))]
   and cvtCLASS_DEFN {ident=x1411, ns=opt1413, nonnullable=b1417, dynamic=b1418, 
          final=b1419, params=ls1421, extends=opt1426, implements=ls1431, classDefns=ls1436, 
          instanceDefns=ls1441, instanceStmts=ls1446, ctorDefn=opt1451} = PrettyRep.Rec [("ident", 
          cvtIDENT x1411), ("ns", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1412))
       )), ("nonnullable", PrettyRep.Bool b1417), ("dynamic", PrettyRep.Bool b1418), 
          ("final", PrettyRep.Bool b1419), ("params", PrettyRep.List (List.map (fn x1420 => 
                                                                                      cvtIDENT x1420
                                                                               ) ls1421)), 
          ("extends", 
       (case opt1426 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1425 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1425))
       )), ("implements", PrettyRep.List (List.map (fn x1430 => cvtIDENT_EXPR x1430
                                                   ) ls1431)), ("classDefns", 
          PrettyRep.List (List.map (fn x1435 => cvtDEFN x1435
                                   ) ls1436)), ("instanceDefns", PrettyRep.List (List.map (fn x1440 => 
                                                                                                 cvtDEFN x1440
                                                                                          ) ls1441)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1445 => cvtSTMT x1445
                                                     ) ls1446)), ("ctorDefn", 
          
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1450))
       ))]
   and cvtINTERFACE_DEFN {ident=x1480, ns=opt1482, nonnullable=b1486, params=ls1488, 
          extends=ls1493, instanceDefns=ls1498} = PrettyRep.Rec [("ident", 
          cvtIDENT x1480), ("ns", 
       (case opt1482 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1481 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1481))
       )), ("nonnullable", PrettyRep.Bool b1486), ("params", PrettyRep.List (List.map (fn x1487 => 
                                                                                             cvtIDENT x1487
                                                                                      ) ls1488)), 
          ("extends", PrettyRep.List (List.map (fn x1492 => cvtIDENT_EXPR x1492
                                               ) ls1493)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1497 => cvtDEFN x1497
                                   ) ls1498))]
   and cvtTYPE_DEFN {ident=x1515, ns=opt1517, init=x1521} = PrettyRep.Rec [("ident", 
          cvtIDENT x1515), ("ns", 
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1516 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1516))
       )), ("init", cvtTYPE_EXPR x1521)]
   and cvtFOR_ENUM_STMT {isEach=b1529, defn=opt1560, obj=x1564, rib=opt1572, 
          next=x1576, labels=ls1578, body=x1582} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1529), ("defn", 
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1530, ns=opt1532, static=b1536, prototype=b1537, bindings=(ls1539, 
            ls1544)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1530), ("ns", 
         (case opt1532 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1531 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1531))
         )), ("static", PrettyRep.Bool b1536), ("prototype", PrettyRep.Bool b1537), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1538 => 
                                                                          cvtBINDING x1538
                                                                   ) ls1539), 
            PrettyRep.List (List.map (fn x1543 => cvtINIT_STEP x1543
                                     ) ls1544)])]))
       )), ("obj", cvtEXPR x1564), ("rib", 
       (case opt1572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1568 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1565, 
                                                                                      x1566) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1565, 
                                                                                      cvtFIXTURE x1566]
                                                                               ) ls1568)))
       )), ("next", cvtSTMT x1576), ("labels", PrettyRep.List (List.map (fn x1577 => 
                                                                               cvtIDENT x1577
                                                                        ) ls1578)), 
          ("body", cvtSTMT x1582)]
   and cvtFOR_STMT {rib=opt1605, defn=opt1639, init=ls1644, cond=x1648, update=x1649, 
          labels=ls1651, body=x1655} = PrettyRep.Rec [("rib", 
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1601 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1598, 
                                                                                      x1599) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1598, 
                                                                                      cvtFIXTURE x1599]
                                                                               ) ls1601)))
       )), ("defn", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1609, ns=opt1611, static=b1615, prototype=b1616, bindings=(ls1618, 
            ls1623)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1609), ("ns", 
         (case opt1611 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1610 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1610))
         )), ("static", PrettyRep.Bool b1615), ("prototype", PrettyRep.Bool b1616), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1617 => 
                                                                          cvtBINDING x1617
                                                                   ) ls1618), 
            PrettyRep.List (List.map (fn x1622 => cvtINIT_STEP x1622
                                     ) ls1623)])]))
       )), ("init", PrettyRep.List (List.map (fn x1643 => cvtSTMT x1643
                                             ) ls1644)), ("cond", cvtEXPR x1648), 
          ("update", cvtEXPR x1649), ("labels", PrettyRep.List (List.map (fn x1650 => 
                                                                                cvtIDENT x1650
                                                                         ) ls1651)), 
          ("body", cvtSTMT x1655)]
   and cvtWHILE_STMT {cond=x1671, rib=opt1679, body=x1683, labels=ls1685} = 
          PrettyRep.Rec [("cond", cvtEXPR x1671), ("rib", 
       (case opt1679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1675 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1672, 
                                                                                      x1673) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1672, 
                                                                                      cvtFIXTURE x1673]
                                                                               ) ls1675)))
       )), ("body", cvtSTMT x1683), ("labels", PrettyRep.List (List.map (fn x1684 => 
                                                                               cvtIDENT x1684
                                                                        ) ls1685))]
   and cvtDIRECTIVES {pragmas=ls1699, defns=ls1704, head=opt1709, body=ls1714, 
          loc=opt1719} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1698 => 
                                                                                    cvtPRAGMA x1698
                                                                             ) ls1699)), 
          ("defns", PrettyRep.List (List.map (fn x1703 => cvtDEFN x1703
                                             ) ls1704)), ("head", 
       (case opt1709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1708 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1708))
       )), ("body", PrettyRep.List (List.map (fn x1713 => cvtSTMT x1713
                                             ) ls1714)), ("loc", 
       (case opt1719 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1718 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1718))
       ))]
   and cvtCASE {label=opt1735, inits=opt1746, body=x1750} = PrettyRep.Rec [("label", 
          
       (case opt1735 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1734 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1734))
       )), ("inits", 
       (case opt1746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1742 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1739, 
                                                                                      x1740) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1739, 
                                                                                      cvtEXPR x1740]
                                                                               ) ls1742)))
       )), ("body", cvtBLOCK x1750)]
   and cvtCATCH_CLAUSE {bindings=(ls1759, ls1764), ty=x1769, rib=opt1777, inits=opt1788, 
          block=x1792} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1758 => 
                                                                                                      cvtBINDING x1758
                                                                                               ) ls1759), 
          PrettyRep.List (List.map (fn x1763 => cvtINIT_STEP x1763
                                   ) ls1764)]), ("ty", cvtTY x1769), ("rib", 
          
       (case opt1777 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1773 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1770, 
                                                                                      x1771) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1770, 
                                                                                      cvtFIXTURE x1771]
                                                                               ) ls1773)))
       )), ("inits", 
       (case opt1788 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1784 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1781, 
                                                                                      x1782) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1781, 
                                                                                      cvtEXPR x1782]
                                                                               ) ls1784)))
       )), ("block", cvtBLOCK x1792)]
   and cvtFUNC_NAME {kind=x1804, ident=x1805} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1804), 
          ("ident", cvtIDENT x1805)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1811, getter=opt1813, setter=opt1818} = 
          PrettyRep.Rec [("ty", cvtTY x1811), ("getter", 
       (case opt1813 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1812 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1812))
       )), ("setter", 
       (case opt1818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1817 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1817))
       ))]
   and cvtFRAGMENT (Unit{name=opt1830, fragments=ls1835}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1830 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1829 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1829))
       )), ("fragments", PrettyRep.List (List.map (fn x1834 => cvtFRAGMENT x1834
                                                  ) ls1835))]))
     | cvtFRAGMENT (Package{name=ls1847, fragments=ls1852}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1846 => 
                                                                        cvtIDENT x1846
                                                                 ) ls1847)), 
          ("fragments", PrettyRep.List (List.map (fn x1851 => cvtFRAGMENT x1851
                                                 ) ls1852))]))
     | cvtFRAGMENT (Anon x1863) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1863))
end

