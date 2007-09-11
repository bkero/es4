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
   and cvtFUNC (Func{name=x451, fsig=x452, native=b453, block=opt455, param=x459, 
          defaults=ls461, ty=x465, loc=opt467}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x451), ("fsig", cvtFUNC_SIG x452), ("native", PrettyRep.Bool b453), 
          ("block", 
       (case opt455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x454 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x454))
       )), ("param", cvtHEAD x459), ("defaults", PrettyRep.List (List.map (fn x460 => 
                                                                                 cvtEXPR x460
                                                                          ) ls461)), 
          ("ty", cvtTY x465), ("loc", 
       (case opt467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x466 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x466))
       ))]))
   and cvtDEFN (ClassDefn x490) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x490))
     | cvtDEFN (VariableDefn x493) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x493))
     | cvtDEFN (FunctionDefn x496) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x496))
     | cvtDEFN (ConstructorDefn x499) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x499))
     | cvtDEFN (InterfaceDefn x502) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x502))
     | cvtDEFN (NamespaceDefn x505) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x505))
     | cvtDEFN (TypeDefn x508) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x508))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls512, params=x516, paramTypes=ls518, 
          defaults=ls523, ctorInits=opt534, returnType=x538, thisType=opt540, 
          hasRest=b544}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x511 => cvtIDENT x511
                                   ) ls512)), ("params", cvtBINDINGS x516), 
          ("paramTypes", PrettyRep.List (List.map (fn x517 => cvtTYPE_EXPR x517
                                                  ) ls518)), ("defaults", PrettyRep.List (List.map (fn x522 => 
                                                                                                          cvtEXPR x522
                                                                                                   ) ls523)), 
          ("ctorInits", 
       (case opt534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x527, ls529) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x527, 
            PrettyRep.List (List.map (fn x528 => cvtEXPR x528
                                     ) ls529)]))
       )), ("returnType", cvtTYPE_EXPR x538), ("thisType", 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x539 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x539))
       )), ("hasRest", PrettyRep.Bool b544)]))
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
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1090, typeArgs=ls1092}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1090), ("typeArgs", 
          PrettyRep.List (List.map (fn x1091 => cvtTY x1091
                                   ) ls1092))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1104, x1108)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1103 => cvtIDENT x1103
                                                          ) ls1104), cvtIDENT_EXPR x1108]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1115) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1115))
     | cvtLITERAL (LiteralContextualDecimalInteger s1118) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1118))
     | cvtLITERAL (LiteralContextualHexInteger s1121) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1121))
     | cvtLITERAL (LiteralDouble r1124) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1124))
     | cvtLITERAL (LiteralDecimal d1127) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1127))
     | cvtLITERAL (LiteralInt i1130) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1130))
     | cvtLITERAL (LiteralUInt u1133) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1133))
     | cvtLITERAL (LiteralBoolean b1136) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1136))
     | cvtLITERAL (LiteralString s1139) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1139))
     | cvtLITERAL (LiteralArray{exprs=ls1143, ty=opt1148}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1142 => 
                                                                         cvtEXPR x1142
                                                                  ) ls1143)), 
          ("ty", 
       (case opt1148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1147 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1147))
       ))]))
     | cvtLITERAL (LiteralXML ls1160) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1159 => 
                                                                                                            cvtEXPR x1159
                                                                                                     ) ls1160)))
     | cvtLITERAL (LiteralNamespace x1166) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1166))
     | cvtLITERAL (LiteralObject{expr=ls1170, ty=opt1175}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1169 => 
                                                                        cvtFIELD x1169
                                                                 ) ls1170)), 
          ("ty", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1174))
       ))]))
     | cvtLITERAL (LiteralFunction x1186) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1186))
     | cvtLITERAL (LiteralRegExp{str=s1189}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1189)]))
   and cvtBLOCK (Block x1195) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1195))
   and cvtFIXTURE (NamespaceFixture x1198) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1198))
     | cvtFIXTURE (ClassFixture x1201) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1201))
     | cvtFIXTURE (InterfaceFixture x1204) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1204))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1208) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1208))
     | cvtFIXTURE (MethodFixture{func=x1211, ty=x1212, readOnly=b1213, override=b1214, 
          final=b1215}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1211), ("ty", cvtTY x1212), ("readOnly", PrettyRep.Bool b1213), 
          ("override", PrettyRep.Bool b1214), ("final", PrettyRep.Bool b1215)]))
     | cvtFIXTURE (ValFixture{ty=x1229, readOnly=b1230}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1229), ("readOnly", PrettyRep.Bool b1230)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1238, getter=opt1240, setter=opt1245}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1238), ("getter", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1239))
       )), ("setter", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1244))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1258, baseTypeArgs=ls1260}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1258), ("baseTypeArgs", PrettyRep.List (List.map (fn x1259 => 
                                                                           cvtTY x1259
                                                                    ) ls1260))]))
   and cvtHEAD (Head(x1271, x1272)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1271, 
          cvtINITS x1272]))
   and cvtBINDINGS (ls1277, ls1282) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1276 => 
                                                                                       cvtBINDING x1276
                                                                                ) ls1277), 
          PrettyRep.List (List.map (fn x1281 => cvtINIT_STEP x1281
                                   ) ls1282)]
   and cvtRIB ls1290 = PrettyRep.List (List.map (fn (x1287, x1288) => PrettyRep.Tuple [cvtFIXTURE_NAME x1287, 
                                                       cvtFIXTURE x1288]
                                                ) ls1290)
   and cvtRIBS ls1295 = PrettyRep.List (List.map (fn x1294 => cvtRIB x1294
                                                 ) ls1295)
   and cvtINITS ls1302 = PrettyRep.List (List.map (fn (x1299, x1300) => PrettyRep.Tuple [cvtFIXTURE_NAME x1299, 
                                                         cvtEXPR x1300]
                                                  ) ls1302)
   and cvtINSTANCE_TYPE {name=x1306, typeArgs=ls1308, nonnullable=b1312, superTypes=ls1314, 
          ty=x1318, conversionTy=opt1320, dynamic=b1324} = PrettyRep.Rec [("name", 
          cvtNAME x1306), ("typeArgs", PrettyRep.List (List.map (fn x1307 => 
                                                                       cvtTYPE_EXPR x1307
                                                                ) ls1308)), 
          ("nonnullable", PrettyRep.Bool b1312), ("superTypes", PrettyRep.List (List.map (fn x1313 => 
                                                                                                cvtTYPE_EXPR x1313
                                                                                         ) ls1314)), 
          ("ty", cvtTYPE_EXPR x1318), ("conversionTy", 
       (case opt1320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1319 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1319))
       )), ("dynamic", PrettyRep.Bool b1324)]
   and cvtFIELD {kind=x1340, name=x1341, init=x1342} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1340), ("name", cvtIDENT_EXPR x1341), ("init", cvtEXPR x1342)]
   and cvtFIELD_TYPE {name=x1350, ty=x1351} = PrettyRep.Rec [("name", cvtIDENT x1350), 
          ("ty", cvtTYPE_EXPR x1351)]
   and cvtFUNC_TYPE {params=ls1358, result=x1362, thisType=opt1364, hasRest=b1368, 
          minArgs=n1369} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1357 => 
                                                                                     cvtTYPE_EXPR x1357
                                                                              ) ls1358)), 
          ("result", cvtTYPE_EXPR x1362), ("thisType", 
       (case opt1364 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1363 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1363))
       )), ("hasRest", PrettyRep.Bool b1368), ("minArgs", PrettyRep.Int n1369)]
   and cvtFUNC_DEFN {kind=x1381, ns=opt1383, final=b1387, override=b1388, prototype=b1389, 
          static=b1390, func=x1391} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1381), 
          ("ns", 
       (case opt1383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1382 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1382))
       )), ("final", PrettyRep.Bool b1387), ("override", PrettyRep.Bool b1388), 
          ("prototype", PrettyRep.Bool b1389), ("static", PrettyRep.Bool b1390), 
          ("func", cvtFUNC x1391)]
   and cvtCTOR_DEFN x1407 = cvtCTOR x1407
   and cvtVAR_DEFN {kind=x1408, ns=opt1410, static=b1414, prototype=b1415, 
          bindings=(ls1417, ls1422)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1408), 
          ("ns", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1409 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1409))
       )), ("static", PrettyRep.Bool b1414), ("prototype", PrettyRep.Bool b1415), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1416 => 
                                                                        cvtBINDING x1416
                                                                 ) ls1417), 
          PrettyRep.List (List.map (fn x1421 => cvtINIT_STEP x1421
                                   ) ls1422)])]
   and cvtNAMESPACE_DEFN {ident=x1438, ns=opt1440, init=opt1445} = PrettyRep.Rec [("ident", 
          cvtIDENT x1438), ("ns", 
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1439 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1439))
       )), ("init", 
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1444))
       ))]
   and cvtCLASS_DEFN {ns=opt1457, ident=x1461, nonnullable=b1462, dynamic=b1463, 
          final=b1464, params=ls1466, extends=opt1471, implements=ls1476, classDefns=ls1481, 
          instanceDefns=ls1486, instanceStmts=ls1491, ctorDefn=opt1496} = PrettyRep.Rec [("ns", 
          
       (case opt1457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1456 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1456))
       )), ("ident", cvtIDENT x1461), ("nonnullable", PrettyRep.Bool b1462), 
          ("dynamic", PrettyRep.Bool b1463), ("final", PrettyRep.Bool b1464), 
          ("params", PrettyRep.List (List.map (fn x1465 => cvtIDENT x1465
                                              ) ls1466)), ("extends", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1470))
       )), ("implements", PrettyRep.List (List.map (fn x1475 => cvtTYPE_EXPR x1475
                                                   ) ls1476)), ("classDefns", 
          PrettyRep.List (List.map (fn x1480 => cvtDEFN x1480
                                   ) ls1481)), ("instanceDefns", PrettyRep.List (List.map (fn x1485 => 
                                                                                                 cvtDEFN x1485
                                                                                          ) ls1486)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1490 => cvtSTMT x1490
                                                     ) ls1491)), ("ctorDefn", 
          
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1495 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1495))
       ))]
   and cvtINTERFACE_DEFN {ident=x1525, ns=opt1527, nonnullable=b1531, params=ls1533, 
          extends=ls1538, instanceDefns=ls1543} = PrettyRep.Rec [("ident", 
          cvtIDENT x1525), ("ns", 
       (case opt1527 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1526 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1526))
       )), ("nonnullable", PrettyRep.Bool b1531), ("params", PrettyRep.List (List.map (fn x1532 => 
                                                                                             cvtIDENT x1532
                                                                                      ) ls1533)), 
          ("extends", PrettyRep.List (List.map (fn x1537 => cvtTYPE_EXPR x1537
                                               ) ls1538)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1542 => cvtDEFN x1542
                                   ) ls1543))]
   and cvtTYPE_DEFN {ident=x1560, ns=opt1562, init=x1566} = PrettyRep.Rec [("ident", 
          cvtIDENT x1560), ("ns", 
       (case opt1562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1561 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1561))
       )), ("init", cvtTYPE_EXPR x1566)]
   and cvtFOR_ENUM_STMT {isEach=b1574, defn=opt1605, obj=x1609, rib=opt1617, 
          next=x1621, labels=ls1623, body=x1627} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1574), ("defn", 
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1575, ns=opt1577, static=b1581, prototype=b1582, bindings=(ls1584, 
            ls1589)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1575), ("ns", 
         (case opt1577 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1576))
         )), ("static", PrettyRep.Bool b1581), ("prototype", PrettyRep.Bool b1582), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1583 => 
                                                                          cvtBINDING x1583
                                                                   ) ls1584), 
            PrettyRep.List (List.map (fn x1588 => cvtINIT_STEP x1588
                                     ) ls1589)])]))
       )), ("obj", cvtEXPR x1609), ("rib", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1613 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1610, 
                                                                                      x1611) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1610, 
                                                                                      cvtFIXTURE x1611]
                                                                               ) ls1613)))
       )), ("next", cvtSTMT x1621), ("labels", PrettyRep.List (List.map (fn x1622 => 
                                                                               cvtIDENT x1622
                                                                        ) ls1623)), 
          ("body", cvtSTMT x1627)]
   and cvtFOR_STMT {rib=opt1650, defn=opt1684, init=ls1689, cond=x1693, update=x1694, 
          labels=ls1696, body=x1700} = PrettyRep.Rec [("rib", 
       (case opt1650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1646 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1643, 
                                                                                      x1644) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1643, 
                                                                                      cvtFIXTURE x1644]
                                                                               ) ls1646)))
       )), ("defn", 
       (case opt1684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1654, ns=opt1656, static=b1660, prototype=b1661, bindings=(ls1663, 
            ls1668)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1654), ("ns", 
         (case opt1656 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1655 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1655))
         )), ("static", PrettyRep.Bool b1660), ("prototype", PrettyRep.Bool b1661), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1662 => 
                                                                          cvtBINDING x1662
                                                                   ) ls1663), 
            PrettyRep.List (List.map (fn x1667 => cvtINIT_STEP x1667
                                     ) ls1668)])]))
       )), ("init", PrettyRep.List (List.map (fn x1688 => cvtSTMT x1688
                                             ) ls1689)), ("cond", cvtEXPR x1693), 
          ("update", cvtEXPR x1694), ("labels", PrettyRep.List (List.map (fn x1695 => 
                                                                                cvtIDENT x1695
                                                                         ) ls1696)), 
          ("body", cvtSTMT x1700)]
   and cvtWHILE_STMT {cond=x1716, rib=opt1724, body=x1728, labels=ls1730} = 
          PrettyRep.Rec [("cond", cvtEXPR x1716), ("rib", 
       (case opt1724 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1720 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1717, 
                                                                                      x1718) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1717, 
                                                                                      cvtFIXTURE x1718]
                                                                               ) ls1720)))
       )), ("body", cvtSTMT x1728), ("labels", PrettyRep.List (List.map (fn x1729 => 
                                                                               cvtIDENT x1729
                                                                        ) ls1730))]
   and cvtDIRECTIVES {pragmas=ls1744, defns=ls1749, head=opt1754, body=ls1759, 
          loc=opt1764} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1743 => 
                                                                                    cvtPRAGMA x1743
                                                                             ) ls1744)), 
          ("defns", PrettyRep.List (List.map (fn x1748 => cvtDEFN x1748
                                             ) ls1749)), ("head", 
       (case opt1754 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1753 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1753))
       )), ("body", PrettyRep.List (List.map (fn x1758 => cvtSTMT x1758
                                             ) ls1759)), ("loc", 
       (case opt1764 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1763 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1763))
       ))]
   and cvtCASE {label=opt1780, inits=opt1791, body=x1795} = PrettyRep.Rec [("label", 
          
       (case opt1780 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1779 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1779))
       )), ("inits", 
       (case opt1791 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1787 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1784, 
                                                                                      x1785) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1784, 
                                                                                      cvtEXPR x1785]
                                                                               ) ls1787)))
       )), ("body", cvtBLOCK x1795)]
   and cvtCATCH_CLAUSE {bindings=(ls1804, ls1809), ty=x1814, rib=opt1822, inits=opt1833, 
          block=x1837} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1803 => 
                                                                                                      cvtBINDING x1803
                                                                                               ) ls1804), 
          PrettyRep.List (List.map (fn x1808 => cvtINIT_STEP x1808
                                   ) ls1809)]), ("ty", cvtTY x1814), ("rib", 
          
       (case opt1822 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1818 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1815, 
                                                                                      x1816) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1815, 
                                                                                      cvtFIXTURE x1816]
                                                                               ) ls1818)))
       )), ("inits", 
       (case opt1833 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1829 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1826, 
                                                                                      x1827) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1826, 
                                                                                      cvtEXPR x1827]
                                                                               ) ls1829)))
       )), ("block", cvtBLOCK x1837)]
   and cvtFUNC_NAME {kind=x1849, ident=x1850} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1849), 
          ("ident", cvtIDENT x1850)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1856, getter=opt1858, setter=opt1863} = 
          PrettyRep.Rec [("ty", cvtTY x1856), ("getter", 
       (case opt1858 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1857 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1857))
       )), ("setter", 
       (case opt1863 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1862 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1862))
       ))]
   and cvtFRAGMENT (Unit{name=opt1875, fragments=ls1880}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1875 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1874 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1874))
       )), ("fragments", PrettyRep.List (List.map (fn x1879 => cvtFRAGMENT x1879
                                                  ) ls1880))]))
     | cvtFRAGMENT (Package{name=ls1892, fragments=ls1897}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1891 => 
                                                                        cvtIDENT x1891
                                                                 ) ls1892)), 
          ("fragments", PrettyRep.List (List.map (fn x1896 => cvtFRAGMENT x1896
                                                 ) ls1897))]))
     | cvtFRAGMENT (Anon{block=x1908, rib=opt1910}) = PrettyRep.Ctor ("Anon", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x1908), ("rib", 
       (case opt1910 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1909 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x1909))
       ))]))
end

