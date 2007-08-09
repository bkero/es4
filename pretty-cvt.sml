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
   and cvtCLS (Cls{name=x351, typeParams=ls353, nonnullable=b357, dynamic=b358, 
          extends=opt360, implements=ls365, classRib=x369, instanceRib=x370, 
          instanceInits=x371, constructor=opt373, classType=x377, instanceType=x378}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x351), 
          ("typeParams", PrettyRep.List (List.map (fn x352 => cvtIDENT x352
                                                  ) ls353)), ("nonnullable", 
          PrettyRep.Bool b357), ("dynamic", PrettyRep.Bool b358), ("extends", 
          
       (case opt360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x359 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x359))
       )), ("implements", PrettyRep.List (List.map (fn x364 => cvtNAME x364
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
                                                                                            cvtNAME x413
                                                                                     ) ls414)), 
          ("instanceRib", cvtRIB x418), ("instanceType", cvtTY x419)]))
   and cvtCTOR (Ctor{settings=x435, superArgs=ls437, func=x441}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x435), ("superArgs", PrettyRep.List (List.map (fn x436 => 
                                                                                                         cvtEXPR x436
                                                                                                  ) ls437)), 
          ("func", cvtFUNC x441)]))
   and cvtFUNC (Func{name=x451, typeParams=ls453, fsig=x457, native=b458, block=x459, 
          param=x460, defaults=ls462, ty=x466, loc=opt468}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x451), ("typeParams", 
          PrettyRep.List (List.map (fn x452 => cvtIDENT x452
                                   ) ls453)), ("fsig", cvtFUNC_SIG x457), ("native", 
          PrettyRep.Bool b458), ("block", cvtBLOCK x459), ("param", cvtHEAD x460), 
          ("defaults", PrettyRep.List (List.map (fn x461 => cvtEXPR x461
                                                ) ls462)), ("ty", cvtTY x466), 
          ("loc", 
       (case opt468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x467 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x467))
       ))]))
   and cvtDEFN (ClassDefn x493) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x493))
     | cvtDEFN (VariableDefn x496) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x496))
     | cvtDEFN (FunctionDefn x499) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x499))
     | cvtDEFN (ConstructorDefn x502) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x502))
     | cvtDEFN (InterfaceDefn x505) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x505))
     | cvtDEFN (NamespaceDefn x508) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x508))
     | cvtDEFN (TypeDefn x511) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x511))
   and cvtFUNC_SIG (FunctionSignature{params=x514, paramTypes=ls516, defaults=ls521, 
          ctorInits=opt532, returnType=x536, thisType=opt538, hasRest=b542}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("params", 
          cvtBINDINGS x514), ("paramTypes", PrettyRep.List (List.map (fn x515 => 
                                                                            cvtTYPE_EXPR x515
                                                                     ) ls516)), 
          ("defaults", PrettyRep.List (List.map (fn x520 => cvtEXPR x520
                                                ) ls521)), ("ctorInits", 
       (case opt532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x525, ls527) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x525, 
            PrettyRep.List (List.map (fn x526 => cvtEXPR x526
                                     ) ls527)]))
       )), ("returnType", cvtTYPE_EXPR x536), ("thisType", 
       (case opt538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x537 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x537))
       )), ("hasRest", PrettyRep.Bool b542)]))
   and cvtBINDING (Binding{ident=x560, ty=x561}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x560), ("ty", cvtTYPE_EXPR x561)]))
   and cvtBINDING_IDENT (TempIdent n569) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n569))
     | cvtBINDING_IDENT (ParamIdent n572) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n572))
     | cvtBINDING_IDENT (PropIdent x575) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x575))
   and cvtINIT_STEP (InitStep(x578, x579)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x578, 
          cvtEXPR x579]))
     | cvtINIT_STEP (AssignStep(x583, x584)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x583, cvtEXPR x584]))
   and cvtTYPE_EXPR (SpecialType x588) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x588))
     | cvtTYPE_EXPR (UnionType ls592) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x591 => 
                                                                                                           cvtTYPE_EXPR x591
                                                                                                    ) ls592)))
     | cvtTYPE_EXPR (ArrayType ls599) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x598 => 
                                                                                                           cvtTYPE_EXPR x598
                                                                                                    ) ls599)))
     | cvtTYPE_EXPR (TypeName x605) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x605))
     | cvtTYPE_EXPR (ElementTypeRef(x608, n609)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x608, PrettyRep.Int n609]))
     | cvtTYPE_EXPR (FieldTypeRef(x613, x614)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x613, cvtIDENT x614]))
     | cvtTYPE_EXPR (FunctionType x618) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x618))
     | cvtTYPE_EXPR (ObjectType ls622) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x621 => 
                                                                                                             cvtFIELD_TYPE x621
                                                                                                      ) ls622)))
     | cvtTYPE_EXPR (AppType{base=x628, args=ls630}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x628), ("args", PrettyRep.List (List.map (fn x629 => 
                                                                                                     cvtTYPE_EXPR x629
                                                                                              ) ls630))]))
     | cvtTYPE_EXPR (LamType{params=ls642, body=x646}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x641 => 
                                                                          cvtIDENT x641
                                                                   ) ls642)), 
          ("body", cvtTYPE_EXPR x646)]))
     | cvtTYPE_EXPR (NullableType{expr=x654, nullable=b655}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x654), ("nullable", PrettyRep.Bool b655)]))
     | cvtTYPE_EXPR (InstanceType x663) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x663))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x667) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x667))
     | cvtSTMT (InitStmt{kind=x670, ns=opt672, prototype=b676, static=b677, 
          temps=x678, inits=ls680}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x670), ("ns", 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x671))
       )), ("prototype", PrettyRep.Bool b676), ("static", PrettyRep.Bool b677), 
          ("temps", cvtBINDINGS x678), ("inits", PrettyRep.List (List.map (fn x679 => 
                                                                                 cvtINIT_STEP x679
                                                                          ) ls680))]))
     | cvtSTMT (ClassBlock{ns=opt700, ident=x704, name=opt706, block=x710}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x699 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x699))
       )), ("ident", cvtIDENT x704), ("name", 
       (case opt706 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x705 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x705))
       )), ("block", cvtBLOCK x710)]))
     | cvtSTMT (ForInStmt x722) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x722))
     | cvtSTMT (ThrowStmt x725) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x725))
     | cvtSTMT (ReturnStmt x728) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x728))
     | cvtSTMT (BreakStmt opt732) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x731))
       ))
     | cvtSTMT (ContinueStmt opt739) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x738))
       ))
     | cvtSTMT (BlockStmt x745) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x745))
     | cvtSTMT (LabeledStmt(x748, x749)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x748, 
          cvtSTMT x749]))
     | cvtSTMT (LetStmt x753) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x753))
     | cvtSTMT (WhileStmt x756) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x756))
     | cvtSTMT (DoWhileStmt x759) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x759))
     | cvtSTMT (ForStmt x762) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x762))
     | cvtSTMT (IfStmt{cnd=x765, thn=x766, els=x767}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x765), ("thn", cvtSTMT x766), 
          ("els", cvtSTMT x767)]))
     | cvtSTMT (WithStmt{obj=x777, ty=x778, body=x779}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x777), ("ty", cvtTY x778), ("body", 
          cvtSTMT x779)]))
     | cvtSTMT (TryStmt{block=x789, catches=ls791, finally=opt796}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x789), ("catches", PrettyRep.List (List.map (fn x790 => 
                                                                                                     cvtCATCH_CLAUSE x790
                                                                                              ) ls791)), 
          ("finally", 
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x795))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt810, cond=x814, labels=ls816, cases=ls821}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x809))
       )), ("cond", cvtEXPR x814), ("labels", PrettyRep.List (List.map (fn x815 => 
                                                                              cvtIDENT x815
                                                                       ) ls816)), 
          ("cases", PrettyRep.List (List.map (fn x820 => cvtCASE x820
                                             ) ls821))]))
     | cvtSTMT (SwitchTypeStmt{cond=x836, ty=x837, cases=ls839}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x836), ("ty", cvtTY x837), 
          ("cases", PrettyRep.List (List.map (fn x838 => cvtCATCH_CLAUSE x838
                                             ) ls839))]))
     | cvtSTMT (DXNStmt{expr=x852}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x852)]))
   and cvtEXPR (TernaryExpr(x858, x859, x860)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x858, cvtEXPR x859, cvtEXPR x860]))
     | cvtEXPR (BinaryExpr(x864, x865, x866)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x864, cvtEXPR x865, cvtEXPR x866]))
     | cvtEXPR (BinaryTypeExpr(x870, x871, x872)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x870, cvtEXPR x871, cvtTY x872]))
     | cvtEXPR (ExpectedTypeExpr(x876, x877)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTY x876, cvtEXPR x877]))
     | cvtEXPR (UnaryExpr(x881, x882)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x881, 
          cvtEXPR x882]))
     | cvtEXPR (TypeExpr x886) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x886))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt891) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt891 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x890 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x890))
       ))
     | cvtEXPR (SuperExpr opt898) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt898 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x897 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x897))
       ))
     | cvtEXPR (LiteralExpr x904) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x904))
     | cvtEXPR (CallExpr{func=x907, actuals=ls909}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x907), ("actuals", PrettyRep.List (List.map (fn x908 => 
                                                                                                   cvtEXPR x908
                                                                                            ) ls909))]))
     | cvtEXPR (ApplyTypeExpr{expr=x920, actuals=ls922}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x920), ("actuals", PrettyRep.List (List.map (fn x921 => 
                                                                                                   cvtTY x921
                                                                                            ) ls922))]))
     | cvtEXPR (LetExpr{defs=x933, body=x934, head=opt936}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x933), ("body", cvtEXPR x934), 
          ("head", 
       (case opt936 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x935 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x935))
       ))]))
     | cvtEXPR (NewExpr{obj=x949, actuals=ls951}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x949), ("actuals", PrettyRep.List (List.map (fn x950 => 
                                                                                                  cvtEXPR x950
                                                                                           ) ls951))]))
     | cvtEXPR (ObjectRef{base=x962, ident=x963, loc=opt965}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x962), ("ident", cvtIDENT_EXPR x963), 
          ("loc", 
       (case opt965 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x964 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x964))
       ))]))
     | cvtEXPR (LexicalRef{ident=x978, loc=opt980}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x978), ("loc", 
       (case opt980 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x979 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x979))
       ))]))
     | cvtEXPR (SetExpr(x991, x992, x993)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x991, 
          cvtEXPR x992, cvtEXPR x993]))
     | cvtEXPR (ListExpr ls998) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x997 => 
                                                                                                    cvtEXPR x997
                                                                                             ) ls998)))
     | cvtEXPR (InitExpr(x1004, x1005, x1006)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x1004, cvtHEAD x1005, cvtINITS x1006]))
     | cvtEXPR (SliceExpr(x1010, x1011, x1012)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x1010, cvtEXPR x1011, cvtEXPR x1012]))
     | cvtEXPR (GetTemp n1016) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n1016))
     | cvtEXPR (GetParam n1019) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n1019))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1025) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1025))
     | cvtFIXTURE_NAME (PropName x1028) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1028))
   and cvtIDENT_EXPR (Identifier{ident=x1031, openNamespaces=ls1037}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1031), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1033 => PrettyRep.List (List.map (fn x1032 => 
                                                                                 cvtNAMESPACE x1032
                                                                          ) ls1033)
                                   ) ls1037))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1048, expr=x1049}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1048), ("expr", cvtEXPR x1049)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1057) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1057))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1060, openNamespaces=ls1066}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1060), ("openNamespaces", PrettyRep.List (List.map (fn ls1062 => 
                                                                             PrettyRep.List (List.map (fn x1061 => 
                                                                                                             cvtNAMESPACE x1061
                                                                                                      ) ls1062)
                                                                      ) ls1066))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1077, ident=s1078}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1077), ("ident", PrettyRep.UniStr s1078)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1087, x1091)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1086 => cvtIDENT x1086
                                                          ) ls1087), cvtIDENT_EXPR x1091]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1098) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1098))
     | cvtLITERAL (LiteralContextualDecimalInteger s1101) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1101))
     | cvtLITERAL (LiteralContextualHexInteger s1104) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1104))
     | cvtLITERAL (LiteralDouble r1107) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1107))
     | cvtLITERAL (LiteralDecimal d1110) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1110))
     | cvtLITERAL (LiteralInt i1113) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1113))
     | cvtLITERAL (LiteralUInt u1116) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1116))
     | cvtLITERAL (LiteralBoolean b1119) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1119))
     | cvtLITERAL (LiteralString s1122) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1122))
     | cvtLITERAL (LiteralArray{exprs=ls1126, ty=opt1131}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1125 => 
                                                                         cvtEXPR x1125
                                                                  ) ls1126)), 
          ("ty", 
       (case opt1131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1130 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1130))
       ))]))
     | cvtLITERAL (LiteralXML ls1143) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1142 => 
                                                                                                            cvtEXPR x1142
                                                                                                     ) ls1143)))
     | cvtLITERAL (LiteralNamespace x1149) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1149))
     | cvtLITERAL (LiteralObject{expr=ls1153, ty=opt1158}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1152 => 
                                                                        cvtFIELD x1152
                                                                 ) ls1153)), 
          ("ty", 
       (case opt1158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1157 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1157))
       ))]))
     | cvtLITERAL (LiteralFunction x1169) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1169))
     | cvtLITERAL (LiteralRegExp{str=s1172}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1172)]))
   and cvtBLOCK (Block x1178) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1178))
   and cvtFIXTURE (NamespaceFixture x1181) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1181))
     | cvtFIXTURE (ClassFixture x1184) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1184))
     | cvtFIXTURE (InterfaceFixture x1187) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1187))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1191) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1191))
     | cvtFIXTURE (MethodFixture{func=x1194, ty=x1195, readOnly=b1196, override=b1197, 
          final=b1198, abstract=b1199}) = PrettyRep.Ctor ("MethodFixture", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1194), ("ty", cvtTY x1195), 
          ("readOnly", PrettyRep.Bool b1196), ("override", PrettyRep.Bool b1197), 
          ("final", PrettyRep.Bool b1198), ("abstract", PrettyRep.Bool b1199)]))
     | cvtFIXTURE (ValFixture{ty=x1215, readOnly=b1216}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1215), ("readOnly", PrettyRep.Bool b1216)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1224, getter=opt1226, setter=opt1231}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1224), ("getter", 
       (case opt1226 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1225 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1225))
       )), ("setter", 
       (case opt1231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1230 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1230))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1244, baseTypeArgs=ls1246}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1244), ("baseTypeArgs", PrettyRep.List (List.map (fn x1245 => 
                                                                           cvtTY x1245
                                                                    ) ls1246))]))
   and cvtHEAD (Head(x1257, x1258)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1257, 
          cvtINITS x1258]))
   and cvtBINDINGS (ls1263, ls1268) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1262 => 
                                                                                       cvtBINDING x1262
                                                                                ) ls1263), 
          PrettyRep.List (List.map (fn x1267 => cvtINIT_STEP x1267
                                   ) ls1268)]
   and cvtRIB ls1276 = PrettyRep.List (List.map (fn (x1273, x1274) => PrettyRep.Tuple [cvtFIXTURE_NAME x1273, 
                                                       cvtFIXTURE x1274]
                                                ) ls1276)
   and cvtRIBS ls1281 = PrettyRep.List (List.map (fn x1280 => cvtRIB x1280
                                                 ) ls1281)
   and cvtINITS ls1288 = PrettyRep.List (List.map (fn (x1285, x1286) => PrettyRep.Tuple [cvtFIXTURE_NAME x1285, 
                                                         cvtEXPR x1286]
                                                  ) ls1288)
   and cvtINSTANCE_TYPE {name=x1292, typeArgs=ls1294, nonnullable=b1298, superTypes=ls1300, 
          ty=x1304, conversionTy=opt1306, dynamic=b1310} = PrettyRep.Rec [("name", 
          cvtNAME x1292), ("typeArgs", PrettyRep.List (List.map (fn x1293 => 
                                                                       cvtTYPE_EXPR x1293
                                                                ) ls1294)), 
          ("nonnullable", PrettyRep.Bool b1298), ("superTypes", PrettyRep.List (List.map (fn x1299 => 
                                                                                                cvtTYPE_EXPR x1299
                                                                                         ) ls1300)), 
          ("ty", cvtTYPE_EXPR x1304), ("conversionTy", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1305))
       )), ("dynamic", PrettyRep.Bool b1310)]
   and cvtFIELD {kind=x1326, name=x1327, init=x1328} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1326), ("name", cvtIDENT_EXPR x1327), ("init", cvtEXPR x1328)]
   and cvtFIELD_TYPE {name=x1336, ty=x1337} = PrettyRep.Rec [("name", cvtIDENT x1336), 
          ("ty", cvtTYPE_EXPR x1337)]
   and cvtFUNC_TYPE {params=ls1344, result=x1348, thisType=opt1350, hasRest=b1354, 
          minArgs=n1355} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1343 => 
                                                                                     cvtTYPE_EXPR x1343
                                                                              ) ls1344)), 
          ("result", cvtTYPE_EXPR x1348), ("thisType", 
       (case opt1350 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1349 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1349))
       )), ("hasRest", PrettyRep.Bool b1354), ("minArgs", PrettyRep.Int n1355)]
   and cvtFUNC_DEFN {kind=x1367, ns=opt1369, final=b1373, override=b1374, prototype=b1375, 
          static=b1376, abstract=b1377, func=x1378} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1367), ("ns", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1368))
       )), ("final", PrettyRep.Bool b1373), ("override", PrettyRep.Bool b1374), 
          ("prototype", PrettyRep.Bool b1375), ("static", PrettyRep.Bool b1376), 
          ("abstract", PrettyRep.Bool b1377), ("func", cvtFUNC x1378)]
   and cvtCTOR_DEFN x1396 = cvtCTOR x1396
   and cvtVAR_DEFN {kind=x1397, ns=opt1399, static=b1403, prototype=b1404, 
          bindings=(ls1406, ls1411)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1397), 
          ("ns", 
       (case opt1399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1398 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1398))
       )), ("static", PrettyRep.Bool b1403), ("prototype", PrettyRep.Bool b1404), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1405 => 
                                                                        cvtBINDING x1405
                                                                 ) ls1406), 
          PrettyRep.List (List.map (fn x1410 => cvtINIT_STEP x1410
                                   ) ls1411)])]
   and cvtNAMESPACE_DEFN {ident=x1427, ns=opt1429, init=opt1434} = PrettyRep.Rec [("ident", 
          cvtIDENT x1427), ("ns", 
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1428 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1428))
       )), ("init", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1433))
       ))]
   and cvtCLASS_DEFN {ident=x1445, ns=opt1447, nonnullable=b1451, dynamic=b1452, 
          final=b1453, params=ls1455, extends=opt1460, implements=ls1465, classDefns=ls1470, 
          instanceDefns=ls1475, instanceStmts=ls1480, ctorDefn=opt1485} = PrettyRep.Rec [("ident", 
          cvtIDENT x1445), ("ns", 
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1446 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1446))
       )), ("nonnullable", PrettyRep.Bool b1451), ("dynamic", PrettyRep.Bool b1452), 
          ("final", PrettyRep.Bool b1453), ("params", PrettyRep.List (List.map (fn x1454 => 
                                                                                      cvtIDENT x1454
                                                                               ) ls1455)), 
          ("extends", 
       (case opt1460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1459))
       )), ("implements", PrettyRep.List (List.map (fn x1464 => cvtIDENT_EXPR x1464
                                                   ) ls1465)), ("classDefns", 
          PrettyRep.List (List.map (fn x1469 => cvtDEFN x1469
                                   ) ls1470)), ("instanceDefns", PrettyRep.List (List.map (fn x1474 => 
                                                                                                 cvtDEFN x1474
                                                                                          ) ls1475)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1479 => cvtSTMT x1479
                                                     ) ls1480)), ("ctorDefn", 
          
       (case opt1485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1484 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1484))
       ))]
   and cvtINTERFACE_DEFN {ident=x1514, ns=opt1516, nonnullable=b1520, params=ls1522, 
          extends=ls1527, instanceDefns=ls1532} = PrettyRep.Rec [("ident", 
          cvtIDENT x1514), ("ns", 
       (case opt1516 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1515 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1515))
       )), ("nonnullable", PrettyRep.Bool b1520), ("params", PrettyRep.List (List.map (fn x1521 => 
                                                                                             cvtIDENT x1521
                                                                                      ) ls1522)), 
          ("extends", PrettyRep.List (List.map (fn x1526 => cvtIDENT_EXPR x1526
                                               ) ls1527)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1531 => cvtDEFN x1531
                                   ) ls1532))]
   and cvtTYPE_DEFN {ident=x1549, ns=opt1551, init=x1555} = PrettyRep.Rec [("ident", 
          cvtIDENT x1549), ("ns", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1550))
       )), ("init", cvtTYPE_EXPR x1555)]
   and cvtFOR_ENUM_STMT {isEach=b1563, defn=opt1594, obj=x1598, rib=opt1606, 
          next=x1610, labels=ls1612, body=x1616} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1563), ("defn", 
       (case opt1594 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1564, ns=opt1566, static=b1570, prototype=b1571, bindings=(ls1573, 
            ls1578)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1564), ("ns", 
         (case opt1566 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1565))
         )), ("static", PrettyRep.Bool b1570), ("prototype", PrettyRep.Bool b1571), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1572 => 
                                                                          cvtBINDING x1572
                                                                   ) ls1573), 
            PrettyRep.List (List.map (fn x1577 => cvtINIT_STEP x1577
                                     ) ls1578)])]))
       )), ("obj", cvtEXPR x1598), ("rib", 
       (case opt1606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1602 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1599, 
                                                                                      x1600) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1599, 
                                                                                      cvtFIXTURE x1600]
                                                                               ) ls1602)))
       )), ("next", cvtSTMT x1610), ("labels", PrettyRep.List (List.map (fn x1611 => 
                                                                               cvtIDENT x1611
                                                                        ) ls1612)), 
          ("body", cvtSTMT x1616)]
   and cvtFOR_STMT {rib=opt1639, defn=opt1673, init=ls1678, cond=x1682, update=x1683, 
          labels=ls1685, body=x1689} = PrettyRep.Rec [("rib", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1635 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1632, 
                                                                                      x1633) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1632, 
                                                                                      cvtFIXTURE x1633]
                                                                               ) ls1635)))
       )), ("defn", 
       (case opt1673 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1643, ns=opt1645, static=b1649, prototype=b1650, bindings=(ls1652, 
            ls1657)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1643), ("ns", 
         (case opt1645 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1644 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1644))
         )), ("static", PrettyRep.Bool b1649), ("prototype", PrettyRep.Bool b1650), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1651 => 
                                                                          cvtBINDING x1651
                                                                   ) ls1652), 
            PrettyRep.List (List.map (fn x1656 => cvtINIT_STEP x1656
                                     ) ls1657)])]))
       )), ("init", PrettyRep.List (List.map (fn x1677 => cvtSTMT x1677
                                             ) ls1678)), ("cond", cvtEXPR x1682), 
          ("update", cvtEXPR x1683), ("labels", PrettyRep.List (List.map (fn x1684 => 
                                                                                cvtIDENT x1684
                                                                         ) ls1685)), 
          ("body", cvtSTMT x1689)]
   and cvtWHILE_STMT {cond=x1705, rib=opt1713, body=x1717, labels=ls1719} = 
          PrettyRep.Rec [("cond", cvtEXPR x1705), ("rib", 
       (case opt1713 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1709 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1706, 
                                                                                      x1707) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1706, 
                                                                                      cvtFIXTURE x1707]
                                                                               ) ls1709)))
       )), ("body", cvtSTMT x1717), ("labels", PrettyRep.List (List.map (fn x1718 => 
                                                                               cvtIDENT x1718
                                                                        ) ls1719))]
   and cvtDIRECTIVES {pragmas=ls1733, defns=ls1738, head=opt1743, body=ls1748, 
          loc=opt1753} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1732 => 
                                                                                    cvtPRAGMA x1732
                                                                             ) ls1733)), 
          ("defns", PrettyRep.List (List.map (fn x1737 => cvtDEFN x1737
                                             ) ls1738)), ("head", 
       (case opt1743 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1742 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1742))
       )), ("body", PrettyRep.List (List.map (fn x1747 => cvtSTMT x1747
                                             ) ls1748)), ("loc", 
       (case opt1753 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1752 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1752))
       ))]
   and cvtCASE {label=opt1769, inits=opt1780, body=x1784} = PrettyRep.Rec [("label", 
          
       (case opt1769 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1768 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1768))
       )), ("inits", 
       (case opt1780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1776 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1773, 
                                                                                      x1774) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1773, 
                                                                                      cvtEXPR x1774]
                                                                               ) ls1776)))
       )), ("body", cvtBLOCK x1784)]
   and cvtCATCH_CLAUSE {bindings=(ls1793, ls1798), ty=x1803, rib=opt1811, inits=opt1822, 
          block=x1826} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1792 => 
                                                                                                      cvtBINDING x1792
                                                                                               ) ls1793), 
          PrettyRep.List (List.map (fn x1797 => cvtINIT_STEP x1797
                                   ) ls1798)]), ("ty", cvtTY x1803), ("rib", 
          
       (case opt1811 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1807 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1804, 
                                                                                      x1805) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1804, 
                                                                                      cvtFIXTURE x1805]
                                                                               ) ls1807)))
       )), ("inits", 
       (case opt1822 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1818 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1815, 
                                                                                      x1816) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1815, 
                                                                                      cvtEXPR x1816]
                                                                               ) ls1818)))
       )), ("block", cvtBLOCK x1826)]
   and cvtFUNC_NAME {kind=x1838, ident=x1839} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1838), 
          ("ident", cvtIDENT x1839)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1845, getter=opt1847, setter=opt1852} = 
          PrettyRep.Rec [("ty", cvtTY x1845), ("getter", 
       (case opt1847 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1846 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1846))
       )), ("setter", 
       (case opt1852 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1851 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1851))
       ))]
   and cvtFRAGMENT (Unit{name=opt1864, fragments=ls1869}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1864 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1863 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1863))
       )), ("fragments", PrettyRep.List (List.map (fn x1868 => cvtFRAGMENT x1868
                                                  ) ls1869))]))
     | cvtFRAGMENT (Package{name=ls1881, fragments=ls1886}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1880 => 
                                                                        cvtIDENT x1880
                                                                 ) ls1881)), 
          ("fragments", PrettyRep.List (List.map (fn x1885 => cvtFRAGMENT x1885
                                                 ) ls1886))]))
     | cvtFRAGMENT (Anon x1897) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1897))
end

