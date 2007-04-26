structure PrettyCvt = struct
   open Ast
   fun cvtPOS {file=s0, span=s1, sm=s2, post_newline=b3} = PrettyRep.Rec [("file", 
          PrettyRep.String s0), ("span", PrettyRep.StrmPosSpan s1), ("sm", 
          PrettyRep.StrmPosSM s2), ("post_newline", PrettyRep.Bool b3)]
   and cvtIDENT s13 = PrettyRep.UniStr s13
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x16) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x16))
     | cvtNAMESPACE (Protected x19) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x19))
     | cvtNAMESPACE (Public x22) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x22))
     | cvtNAMESPACE (Internal x25) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x25))
     | cvtNAMESPACE (UserNamespace s28) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s28))
     | cvtNAMESPACE (AnonUserNamespace n31) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n31))
     | cvtNAMESPACE (LimitedNamespace(x34, x35)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x34, cvtNAMESPACE x35]))
   and cvtNAME {ns=x39, id=x40} = PrettyRep.Rec [("ns", cvtNAMESPACE x39), 
          ("id", cvtIDENT x40)]
   and cvtMULTINAME {nss=ls51, id=x55} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls47 => 
                                                                                                PrettyRep.List (List.map (fn x46 => 
                                                                                                                                cvtNAMESPACE x46
                                                                                                                         ) ls47)
                                                                                         ) ls51)), 
          ("id", cvtIDENT x55)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x66, roundingMode=r67, precision=n68} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x66), ("roundingMode", 
          PrettyRep.DecRm r67), ("precision", PrettyRep.Int n68)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt81) = PrettyRep.Ctor ("Plus", SOME 
       (case opt81 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x80 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x80))
       ))
     | cvtBINOP (Minus opt88) = PrettyRep.Ctor ("Minus", SOME 
       (case opt88 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x87 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x87))
       ))
     | cvtBINOP (Times opt95) = PrettyRep.Ctor ("Times", SOME 
       (case opt95 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x94 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x94))
       ))
     | cvtBINOP (Divide opt102) = PrettyRep.Ctor ("Divide", SOME 
       (case opt102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x101 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x101))
       ))
     | cvtBINOP (Remainder opt109) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt109 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x108 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x108))
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
     | cvtBINOP (Equals opt126) = PrettyRep.Ctor ("Equals", SOME 
       (case opt126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x125 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x125))
       ))
     | cvtBINOP (NotEquals opt133) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt133 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x132 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x132))
       ))
     | cvtBINOP (StrictEquals opt140) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x139 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x139))
       ))
     | cvtBINOP (StrictNotEquals opt147) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x146 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x146))
       ))
     | cvtBINOP (Less opt154) = PrettyRep.Ctor ("Less", SOME 
       (case opt154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x153 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x153))
       ))
     | cvtBINOP (LessOrEqual opt161) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x160 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x160))
       ))
     | cvtBINOP (Greater opt168) = PrettyRep.Ctor ("Greater", SOME 
       (case opt168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x167 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x167))
       ))
     | cvtBINOP (GreaterOrEqual opt175) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x174 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x174))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt184) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x183 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x183))
       ))
     | cvtASSIGNOP (AssignMinus opt191) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x190 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x190))
       ))
     | cvtASSIGNOP (AssignTimes opt198) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x197 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x197))
       ))
     | cvtASSIGNOP (AssignDivide opt205) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt205 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x204 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x204))
       ))
     | cvtASSIGNOP (AssignRemainder opt212) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt212 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x211 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x211))
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
     | cvtUNOP (PreIncrement opt230) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x229 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x229))
       ))
     | cvtUNOP (PreDecrement opt237) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x236 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x236))
       ))
     | cvtUNOP (PostIncrement opt244) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x243 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x243))
       ))
     | cvtUNOP (PostDecrement opt251) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x250 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x250))
       ))
     | cvtUNOP (UnaryPlus opt258) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x257 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x257))
       ))
     | cvtUNOP (UnaryMinus opt265) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x264 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x264))
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
   and cvtPRAGMA (UseNamespace x282) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x282))
     | cvtPRAGMA (UseDefaultNamespace x285) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x285))
     | cvtPRAGMA (UseNumber x288) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x288))
     | cvtPRAGMA (UseRounding r291) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r291))
     | cvtPRAGMA (UsePrecision n294) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n294))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls300, name=x304, alias=opt306}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x299 => 
                                                                           cvtIDENT x299
                                                                    ) ls300)), 
          ("name", cvtIDENT x304), ("alias", 
       (case opt306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x305 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x305))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x325, extends=opt327, implements=ls332, classFixtures=x336, 
          instanceFixtures=x337, instanceInits=x338, constructor=opt340, classType=x344, 
          instanceType=x345}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x325), ("extends", 
       (case opt327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x326 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x326))
       )), ("implements", PrettyRep.List (List.map (fn x331 => cvtNAME x331
                                                   ) ls332)), ("classFixtures", 
          cvtFIXTURES x336), ("instanceFixtures", cvtFIXTURES x337), ("instanceInits", 
          cvtHEAD x338), ("constructor", 
       (case opt340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x339 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x339))
       )), ("classType", cvtTYPE_EXPR x344), ("instanceType", cvtTYPE_EXPR x345)]))
   and cvtCTOR (Ctor{settings=x367, superArgs=ls369, func=x373}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x367), ("superArgs", PrettyRep.List (List.map (fn x368 => 
                                                                                                         cvtEXPR x368
                                                                                                  ) ls369)), 
          ("func", cvtFUNC x373)]))
   and cvtFUNC (Func{name=x383, fsig=x384, isNative=b385, block=x386, param=x387, 
          defaults=ls389, ty=x393}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x383), ("fsig", cvtFUNC_SIG x384), ("isNative", PrettyRep.Bool b385), 
          ("block", cvtBLOCK x386), ("param", cvtHEAD x387), ("defaults", PrettyRep.List (List.map (fn x388 => 
                                                                                                          cvtEXPR x388
                                                                                                   ) ls389)), 
          ("ty", cvtFUNC_TYPE x393)]))
   and cvtDEFN (ClassDefn x411) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x411))
     | cvtDEFN (VariableDefn x414) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x414))
     | cvtDEFN (FunctionDefn x417) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x417))
     | cvtDEFN (ConstructorDefn x420) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x420))
     | cvtDEFN (InterfaceDefn x423) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x423))
     | cvtDEFN (NamespaceDefn x426) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x426))
     | cvtDEFN (TypeDefn x429) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x429))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls433, params=x437, paramTypes=ls439, 
          defaults=ls444, ctorInits=opt455, returnType=x459, thisType=opt461, 
          hasRest=b465}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x432 => cvtIDENT x432
                                   ) ls433)), ("params", cvtBINDINGS x437), 
          ("paramTypes", PrettyRep.List (List.map (fn x438 => cvtTYPE_EXPR x438
                                                  ) ls439)), ("defaults", PrettyRep.List (List.map (fn x443 => 
                                                                                                          cvtEXPR x443
                                                                                                   ) ls444)), 
          ("ctorInits", 
       (case opt455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x448, ls450) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x448, 
            PrettyRep.List (List.map (fn x449 => cvtEXPR x449
                                     ) ls450)]))
       )), ("returnType", cvtTYPE_EXPR x459), ("thisType", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x460 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x460))
       )), ("hasRest", PrettyRep.Bool b465)]))
   and cvtBINDING (Binding{ident=x485, ty=x486}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x485), ("ty", cvtTYPE_EXPR x486)]))
   and cvtBINDING_IDENT (TempIdent n494) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n494))
     | cvtBINDING_IDENT (ParamIdent n497) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n497))
     | cvtBINDING_IDENT (PropIdent x500) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x500))
   and cvtINIT_STEP (InitStep(x503, x504)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x503, 
          cvtEXPR x504]))
     | cvtINIT_STEP (AssignStep(x508, x509)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x508, cvtEXPR x509]))
   and cvtTYPE_EXPR (SpecialType x513) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x513))
     | cvtTYPE_EXPR (UnionType ls517) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x516 => 
                                                                                                           cvtTYPE_EXPR x516
                                                                                                    ) ls517)))
     | cvtTYPE_EXPR (ArrayType ls524) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x523 => 
                                                                                                           cvtTYPE_EXPR x523
                                                                                                    ) ls524)))
     | cvtTYPE_EXPR (TypeName x530) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x530))
     | cvtTYPE_EXPR (ElementTypeRef(x533, n534)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x533, PrettyRep.Int n534]))
     | cvtTYPE_EXPR (FieldTypeRef(x538, x539)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x538, cvtIDENT x539]))
     | cvtTYPE_EXPR (FunctionType x543) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x543))
     | cvtTYPE_EXPR (ObjectType ls547) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x546 => 
                                                                                                             cvtFIELD_TYPE x546
                                                                                                      ) ls547)))
     | cvtTYPE_EXPR (AppType{base=x553, args=ls555}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x553), ("args", PrettyRep.List (List.map (fn x554 => 
                                                                                                     cvtTYPE_EXPR x554
                                                                                              ) ls555))]))
     | cvtTYPE_EXPR (NullableType{expr=x566, nullable=b567}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x566), ("nullable", PrettyRep.Bool b567)]))
     | cvtTYPE_EXPR (InstanceType{name=x575, typeParams=ls577, ty=x581, isDynamic=b582}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x575), 
          ("typeParams", PrettyRep.List (List.map (fn x576 => cvtIDENT x576
                                                  ) ls577)), ("ty", cvtTYPE_EXPR x581), 
          ("isDynamic", PrettyRep.Bool b582)]))
     | cvtTYPE_EXPR (NominalType x594) = PrettyRep.Ctor ("NominalType", SOME (cvtNAME x594))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x598) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x598))
     | cvtSTMT (InitStmt{kind=x601, ns=opt603, prototype=b607, static=b608, 
          temps=x609, inits=ls611}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x601), ("ns", 
       (case opt603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x602 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x602))
       )), ("prototype", PrettyRep.Bool b607), ("static", PrettyRep.Bool b608), 
          ("temps", cvtBINDINGS x609), ("inits", PrettyRep.List (List.map (fn x610 => 
                                                                                 cvtINIT_STEP x610
                                                                          ) ls611))]))
     | cvtSTMT (ClassBlock{ns=opt631, ident=x635, name=opt637, block=x641}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt631 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x630 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x630))
       )), ("ident", cvtIDENT x635), ("name", 
       (case opt637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x636 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x636))
       )), ("block", cvtBLOCK x641)]))
     | cvtSTMT (ForInStmt x653) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x653))
     | cvtSTMT (ThrowStmt x656) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x656))
     | cvtSTMT (ReturnStmt x659) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x659))
     | cvtSTMT (BreakStmt opt663) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt663 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x662 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x662))
       ))
     | cvtSTMT (ContinueStmt opt670) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x669 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x669))
       ))
     | cvtSTMT (BlockStmt x676) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x676))
     | cvtSTMT (LabeledStmt(x679, x680)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x679, 
          cvtSTMT x680]))
     | cvtSTMT (LetStmt x684) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x684))
     | cvtSTMT (WhileStmt x687) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x687))
     | cvtSTMT (DoWhileStmt x690) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x690))
     | cvtSTMT (ForStmt x693) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x693))
     | cvtSTMT (IfStmt{cnd=x696, thn=x697, els=x698}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x696), ("thn", cvtSTMT x697), 
          ("els", cvtSTMT x698)]))
     | cvtSTMT (WithStmt{obj=x708, ty=x709, body=x710}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x708), ("ty", cvtTYPE_EXPR x709), 
          ("body", cvtSTMT x710)]))
     | cvtSTMT (TryStmt{block=x720, catches=ls722, finally=opt727}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x720), ("catches", PrettyRep.List (List.map (fn x721 => 
                                                                                                     cvtCATCH_CLAUSE x721
                                                                                              ) ls722)), 
          ("finally", 
       (case opt727 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x726 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x726))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt741, cond=x745, labels=ls747, cases=ls752}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt741 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x740 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x740))
       )), ("cond", cvtEXPR x745), ("labels", PrettyRep.List (List.map (fn x746 => 
                                                                              cvtIDENT x746
                                                                       ) ls747)), 
          ("cases", PrettyRep.List (List.map (fn x751 => cvtCASE x751
                                             ) ls752))]))
     | cvtSTMT (SwitchTypeStmt{cond=x767, ty=x768, cases=ls770}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x767), ("ty", cvtTYPE_EXPR x768), 
          ("cases", PrettyRep.List (List.map (fn x769 => cvtTYPE_CASE x769
                                             ) ls770))]))
     | cvtSTMT (Dxns{expr=x783}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x783)]))
   and cvtEXPR (TernaryExpr(x789, x790, x791, x792)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x789, cvtEXPR x790, cvtEXPR x791, 
          cvtEXPR x792]))
     | cvtEXPR (BinaryExpr(x796, x797, x798)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x796, cvtEXPR x797, cvtEXPR x798]))
     | cvtEXPR (BinaryTypeExpr(x802, x803, x804)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x802, cvtEXPR x803, cvtTYPE_EXPR x804]))
     | cvtEXPR (ExpectedTypeExpr(x808, x809)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x808, cvtEXPR x809]))
     | cvtEXPR (UnaryExpr(x813, x814)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x813, 
          cvtEXPR x814]))
     | cvtEXPR (TypeExpr x818) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x818))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt823) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt823 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x822 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x822))
       ))
     | cvtEXPR (SuperExpr opt830) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt830 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x829 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x829))
       ))
     | cvtEXPR (LiteralExpr x836) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x836))
     | cvtEXPR (CallExpr{func=x839, actuals=ls841}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x839), ("actuals", PrettyRep.List (List.map (fn x840 => 
                                                                                                   cvtEXPR x840
                                                                                            ) ls841))]))
     | cvtEXPR (ApplyTypeExpr{expr=x852, actuals=ls854}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x852), ("actuals", PrettyRep.List (List.map (fn x853 => 
                                                                                                   cvtTYPE_EXPR x853
                                                                                            ) ls854))]))
     | cvtEXPR (LetExpr{defs=x865, body=x866, head=opt868}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x865), ("body", cvtEXPR x866), 
          ("head", 
       (case opt868 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x867 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x867))
       ))]))
     | cvtEXPR (NewExpr{obj=x881, actuals=ls883}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x881), ("actuals", PrettyRep.List (List.map (fn x882 => 
                                                                                                  cvtEXPR x882
                                                                                           ) ls883))]))
     | cvtEXPR (ObjectRef{base=x894, ident=x895, pos=opt897}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x894), ("ident", cvtIDENT_EXPR x895), 
          ("pos", 
       (case opt897 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x896 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x896))
       ))]))
     | cvtEXPR (LexicalRef{ident=x910, pos=opt912}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x910), ("pos", 
       (case opt912 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x911 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x911))
       ))]))
     | cvtEXPR (SetExpr(x923, x924, x925)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x923, 
          cvtEXPR x924, cvtEXPR x925]))
     | cvtEXPR (ListExpr ls930) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x929 => 
                                                                                                    cvtEXPR x929
                                                                                             ) ls930)))
     | cvtEXPR (InitExpr(x936, x937, x938)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x936, 
          cvtHEAD x937, cvtINITS x938]))
     | cvtEXPR (SliceExpr(x942, x943, x944)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x942, cvtEXPR x943, cvtEXPR x944]))
     | cvtEXPR (GetTemp n948) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n948))
     | cvtEXPR (GetParam n951) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n951))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n957) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n957))
     | cvtFIXTURE_NAME (PropName x960) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x960))
   and cvtIDENT_EXPR (Identifier{ident=x963, openNamespaces=ls969}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x963), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls965 => PrettyRep.List (List.map (fn x964 => 
                                                                                cvtNAMESPACE x964
                                                                         ) ls965)
                                   ) ls969))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x980, expr=x981}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x980), ("expr", cvtEXPR x981)]))
     | cvtIDENT_EXPR (AttributeIdentifier x989) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x989))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x992, openNamespaces=ls998}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x992), ("openNamespaces", PrettyRep.List (List.map (fn ls994 => 
                                                                            PrettyRep.List (List.map (fn x993 => 
                                                                                                            cvtNAMESPACE x993
                                                                                                     ) ls994)
                                                                     ) ls998))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1009, ident=s1010}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1009), ("ident", PrettyRep.UniStr s1010)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1018, typeArgs=ls1020}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1018), ("typeArgs", 
          PrettyRep.List (List.map (fn x1019 => cvtTYPE_EXPR x1019
                                   ) ls1020))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1032, x1036)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1031 => cvtIDENT x1031
                                                          ) ls1032), cvtIDENT_EXPR x1036]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1043) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1043))
     | cvtLITERAL (LiteralContextualDecimalInteger s1046) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1046))
     | cvtLITERAL (LiteralContextualHexInteger s1049) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1049))
     | cvtLITERAL (LiteralDouble r1052) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1052))
     | cvtLITERAL (LiteralDecimal d1055) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1055))
     | cvtLITERAL (LiteralInt i1058) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1058))
     | cvtLITERAL (LiteralUInt u1061) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1061))
     | cvtLITERAL (LiteralBoolean b1064) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1064))
     | cvtLITERAL (LiteralString s1067) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1067))
     | cvtLITERAL (LiteralArray{exprs=ls1071, ty=opt1076}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1070 => 
                                                                         cvtEXPR x1070
                                                                  ) ls1071)), 
          ("ty", 
       (case opt1076 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1075 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1075))
       ))]))
     | cvtLITERAL (LiteralXML ls1088) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1087 => 
                                                                                                            cvtEXPR x1087
                                                                                                     ) ls1088)))
     | cvtLITERAL (LiteralNamespace x1094) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1094))
     | cvtLITERAL (LiteralObject{expr=ls1098, ty=opt1103}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1097 => 
                                                                        cvtFIELD x1097
                                                                 ) ls1098)), 
          ("ty", 
       (case opt1103 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1102 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1102))
       ))]))
     | cvtLITERAL (LiteralFunction x1114) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1114))
     | cvtLITERAL (LiteralRegExp{str=s1117}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1117)]))
   and cvtBLOCK (Block x1123) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1123))
   and cvtFIXTURE (NamespaceFixture x1126) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1126))
     | cvtFIXTURE (ClassFixture x1129) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1129))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1134) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1134))
     | cvtFIXTURE (MethodFixture{func=x1137, ty=x1138, readOnly=b1139, override=b1140, 
          final=b1141}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1137), ("ty", cvtTYPE_EXPR x1138), ("readOnly", PrettyRep.Bool b1139), 
          ("override", PrettyRep.Bool b1140), ("final", PrettyRep.Bool b1141)]))
     | cvtFIXTURE (ValFixture{ty=x1155, readOnly=b1156}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1155), ("readOnly", PrettyRep.Bool b1156)]))
     | cvtFIXTURE (VirtualValFixture x1164) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1164))
   and cvtBINDINGS (ls1168, ls1173) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1167 => 
                                                                                       cvtBINDING x1167
                                                                                ) ls1168), 
          PrettyRep.List (List.map (fn x1172 => cvtINIT_STEP x1172
                                   ) ls1173)]
   and cvtFIXTURES ls1181 = PrettyRep.List (List.map (fn (x1178, x1179) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1178, 
                                                            cvtFIXTURE x1179]
                                                     ) ls1181)
   and cvtINITS ls1188 = PrettyRep.List (List.map (fn (x1185, x1186) => PrettyRep.Tuple [cvtFIXTURE_NAME x1185, 
                                                         cvtEXPR x1186]
                                                  ) ls1188)
   and cvtHEAD (x1192, x1193) = PrettyRep.Tuple [cvtFIXTURES x1192, cvtINITS x1193]
   and cvtFIELD {kind=x1195, name=x1196, init=x1197} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1195), ("name", cvtIDENT_EXPR x1196), ("init", cvtEXPR x1197)]
   and cvtFIELD_TYPE {name=x1205, ty=x1206} = PrettyRep.Rec [("name", cvtIDENT x1205), 
          ("ty", cvtTYPE_EXPR x1206)]
   and cvtTYPED_IDENT {name=x1212, ty=opt1214} = PrettyRep.Rec [("name", cvtIDENT x1212), 
          ("ty", 
       (case opt1214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1213 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1213))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1224, params=ls1229, result=x1233, thisType=opt1235, 
          hasRest=b1239, minArgs=n1240} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1223 => 
                                                                                                        cvtIDENT x1223
                                                                                                 ) ls1224)), 
          ("params", PrettyRep.List (List.map (fn x1228 => cvtTYPE_EXPR x1228
                                              ) ls1229)), ("result", cvtTYPE_EXPR x1233), 
          ("thisType", 
       (case opt1235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1234 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1234))
       )), ("hasRest", PrettyRep.Bool b1239), ("minArgs", PrettyRep.Int n1240)]
   and cvtFUNC_DEFN {kind=x1254, ns=opt1256, final=b1260, override=b1261, prototype=b1262, 
          static=b1263, func=x1264} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1254), 
          ("ns", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1255))
       )), ("final", PrettyRep.Bool b1260), ("override", PrettyRep.Bool b1261), 
          ("prototype", PrettyRep.Bool b1262), ("static", PrettyRep.Bool b1263), 
          ("func", cvtFUNC x1264)]
   and cvtCTOR_DEFN x1280 = cvtCTOR x1280
   and cvtVAR_DEFN {kind=x1281, ns=opt1283, static=b1287, prototype=b1288, 
          bindings=x1289} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1281), 
          ("ns", 
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1282))
       )), ("static", PrettyRep.Bool b1287), ("prototype", PrettyRep.Bool b1288), 
          ("bindings", cvtBINDINGS x1289)]
   and cvtNAMESPACE_DEFN {ident=x1301, ns=opt1303, init=opt1308} = PrettyRep.Rec [("ident", 
          cvtIDENT x1301), ("ns", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1302))
       )), ("init", 
       (case opt1308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1307 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1307))
       ))]
   and cvtCLASS_DEFN {ident=x1319, ns=opt1321, nonnullable=b1325, dynamic=b1326, 
          final=b1327, params=ls1329, extends=opt1334, implements=ls1339, classDefns=ls1344, 
          instanceDefns=ls1349, instanceStmts=ls1354, ctorDefn=opt1359} = PrettyRep.Rec [("ident", 
          cvtIDENT x1319), ("ns", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1320))
       )), ("nonnullable", PrettyRep.Bool b1325), ("dynamic", PrettyRep.Bool b1326), 
          ("final", PrettyRep.Bool b1327), ("params", PrettyRep.List (List.map (fn x1328 => 
                                                                                      cvtIDENT x1328
                                                                               ) ls1329)), 
          ("extends", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1333))
       )), ("implements", PrettyRep.List (List.map (fn x1338 => cvtIDENT_EXPR x1338
                                                   ) ls1339)), ("classDefns", 
          PrettyRep.List (List.map (fn x1343 => cvtDEFN x1343
                                   ) ls1344)), ("instanceDefns", PrettyRep.List (List.map (fn x1348 => 
                                                                                                 cvtDEFN x1348
                                                                                          ) ls1349)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1353 => cvtSTMT x1353
                                                     ) ls1354)), ("ctorDefn", 
          
       (case opt1359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1358))
       ))]
   and cvtINTERFACE_DEFN {ident=x1388, ns=opt1390, nonnullable=b1394, params=ls1396, 
          extends=ls1401, block=x1405} = PrettyRep.Rec [("ident", cvtIDENT x1388), 
          ("ns", 
       (case opt1390 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1389 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1389))
       )), ("nonnullable", PrettyRep.Bool b1394), ("params", PrettyRep.List (List.map (fn x1395 => 
                                                                                             cvtIDENT x1395
                                                                                      ) ls1396)), 
          ("extends", PrettyRep.List (List.map (fn x1400 => cvtIDENT_EXPR x1400
                                               ) ls1401)), ("block", cvtBLOCK x1405)]
   and cvtTYPE_DEFN {ident=x1419, ns=opt1421, init=x1425} = PrettyRep.Rec [("ident", 
          cvtIDENT x1419), ("ns", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1420 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1420))
       )), ("init", cvtTYPE_EXPR x1425)]
   and cvtFOR_ENUM_STMT {isEach=b1433, defn=opt1435, obj=x1439, fixtures=opt1441, 
          next=x1445, labels=ls1447, body=x1451} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1433), ("defn", 
       (case opt1435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1434))
       )), ("obj", cvtEXPR x1439), ("fixtures", 
       (case opt1441 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1440))
       )), ("next", cvtSTMT x1445), ("labels", PrettyRep.List (List.map (fn x1446 => 
                                                                               cvtIDENT x1446
                                                                        ) ls1447)), 
          ("body", cvtSTMT x1451)]
   and cvtFOR_STMT {fixtures=opt1468, defn=opt1473, init=ls1478, cond=x1482, 
          update=x1483, labels=ls1485, body=x1489} = PrettyRep.Rec [("fixtures", 
          
       (case opt1468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1467))
       )), ("defn", 
       (case opt1473 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1472 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1472))
       )), ("init", PrettyRep.List (List.map (fn x1477 => cvtSTMT x1477
                                             ) ls1478)), ("cond", cvtEXPR x1482), 
          ("update", cvtEXPR x1483), ("labels", PrettyRep.List (List.map (fn x1484 => 
                                                                                cvtIDENT x1484
                                                                         ) ls1485)), 
          ("body", cvtSTMT x1489)]
   and cvtWHILE_STMT {cond=x1505, fixtures=opt1507, body=x1511, labels=ls1513} = 
          PrettyRep.Rec [("cond", cvtEXPR x1505), ("fixtures", 
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1506 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1506))
       )), ("body", cvtSTMT x1511), ("labels", PrettyRep.List (List.map (fn x1512 => 
                                                                               cvtIDENT x1512
                                                                        ) ls1513))]
   and cvtDIRECTIVES {pragmas=ls1527, defns=ls1532, head=opt1537, body=ls1542, 
          pos=opt1547} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1526 => 
                                                                                    cvtPRAGMA x1526
                                                                             ) ls1527)), 
          ("defns", PrettyRep.List (List.map (fn x1531 => cvtDEFN x1531
                                             ) ls1532)), ("head", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1536))
       )), ("body", PrettyRep.List (List.map (fn x1541 => cvtSTMT x1541
                                             ) ls1542)), ("pos", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1546))
       ))]
   and cvtCASE {label=opt1563, inits=opt1568, body=x1572} = PrettyRep.Rec [("label", 
          
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1562))
       )), ("inits", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1567))
       )), ("body", cvtBLOCK x1572)]
   and cvtTYPE_CASE {ty=opt1581, body=x1585} = PrettyRep.Rec [("ty", 
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1580))
       )), ("body", cvtSTMT x1585)]
   and cvtCATCH_CLAUSE {bindings=x1591, ty=x1592, fixtures=opt1594, block=x1598} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1591), ("ty", cvtTYPE_EXPR x1592), 
          ("fixtures", 
       (case opt1594 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1593 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1593))
       )), ("block", cvtBLOCK x1598)]
   and cvtFUNC_NAME {kind=x1608, ident=x1609} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1608), 
          ("ident", cvtIDENT x1609)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1615, getter=opt1617, setter=opt1622} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1615), ("getter", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1616 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1616))
       )), ("setter", 
       (case opt1622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1621 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1621))
       ))]
   and cvtPACKAGE {name=ls1634, block=x1638} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1633 => 
                                                                                                       cvtIDENT x1633
                                                                                                ) ls1634)), 
          ("block", cvtBLOCK x1638)]
   and cvtPROGRAM {packages=ls1645, fixtures=opt1650, block=x1654} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1644 => cvtPACKAGE x1644
                                   ) ls1645)), ("fixtures", 
       (case opt1650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1649 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1649))
       )), ("block", cvtBLOCK x1654)]
end

