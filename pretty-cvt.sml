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
     | cvtNAMESPACE (Imported(x19, x20)) = PrettyRep.Ctor ("Imported", SOME (PrettyRep.Tuple [cvtIDENT x19, 
          cvtIDENT x20]))
   and cvtNAME {ns=x24, id=x25} = PrettyRep.Rec [("ns", cvtNAMESPACE x24), 
          ("id", cvtIDENT x25)]
   and cvtMULTINAME {nss=ls36, id=x40} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls32 => 
                                                                                                PrettyRep.List (List.map (fn x31 => 
                                                                                                                                cvtNAMESPACE x31
                                                                                                                         ) ls32)
                                                                                         ) ls36)), 
          ("id", cvtIDENT x40)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x51, roundingMode=r52, precision=n53} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x51), ("roundingMode", 
          PrettyRep.DecRm r52), ("precision", PrettyRep.Int n53)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt66) = PrettyRep.Ctor ("Plus", SOME 
       (case opt66 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x65 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x65))
       ))
     | cvtBINOP (Minus opt73) = PrettyRep.Ctor ("Minus", SOME 
       (case opt73 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x72 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x72))
       ))
     | cvtBINOP (Times opt80) = PrettyRep.Ctor ("Times", SOME 
       (case opt80 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x79 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x79))
       ))
     | cvtBINOP (Divide opt87) = PrettyRep.Ctor ("Divide", SOME 
       (case opt87 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
       ))
     | cvtBINOP (Remainder opt94) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt94 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
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
     | cvtBINOP (Equals opt111) = PrettyRep.Ctor ("Equals", SOME 
       (case opt111 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x110 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x110))
       ))
     | cvtBINOP (NotEquals opt118) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt118 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x117 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x117))
       ))
     | cvtBINOP (StrictEquals opt125) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x124 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x124))
       ))
     | cvtBINOP (StrictNotEquals opt132) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
       ))
     | cvtBINOP (Less opt139) = PrettyRep.Ctor ("Less", SOME 
       (case opt139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
       ))
     | cvtBINOP (LessOrEqual opt146) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
       ))
     | cvtBINOP (Greater opt153) = PrettyRep.Ctor ("Greater", SOME 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
       ))
     | cvtBINOP (GreaterOrEqual opt160) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt169) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x168 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x168))
       ))
     | cvtASSIGNOP (AssignMinus opt176) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x175 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x175))
       ))
     | cvtASSIGNOP (AssignTimes opt183) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x182))
       ))
     | cvtASSIGNOP (AssignDivide opt190) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
       ))
     | cvtASSIGNOP (AssignRemainder opt197) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
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
     | cvtUNOP (PreIncrement opt215) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x214 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x214))
       ))
     | cvtUNOP (PreDecrement opt222) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x221 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x221))
       ))
     | cvtUNOP (PostIncrement opt229) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x228 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x228))
       ))
     | cvtUNOP (PostDecrement opt236) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
       ))
     | cvtUNOP (UnaryPlus opt243) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
       ))
     | cvtUNOP (UnaryMinus opt250) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
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
   and cvtPRAGMA (UseNamespace x267) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x267))
     | cvtPRAGMA (UseDefaultNamespace x270) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x270))
     | cvtPRAGMA (UseNumber x273) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x273))
     | cvtPRAGMA (UseRounding r276) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r276))
     | cvtPRAGMA (UsePrecision n279) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n279))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls285, name=x289, alias=opt291}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x284 => 
                                                                           cvtIDENT x284
                                                                    ) ls285)), 
          ("name", cvtIDENT x289), ("alias", 
       (case opt291 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x290 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x290))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtCLS (Cls{name=x310, extends=opt312, implements=ls317, classFixtures=x321, 
          instanceFixtures=x322, instanceInits=x323, constructor=opt325, classType=x329, 
          instanceType=x330}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x310), ("extends", 
       (case opt312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x311 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x311))
       )), ("implements", PrettyRep.List (List.map (fn x316 => cvtNAME x316
                                                   ) ls317)), ("classFixtures", 
          cvtFIXTURES x321), ("instanceFixtures", cvtFIXTURES x322), ("instanceInits", 
          cvtHEAD x323), ("constructor", 
       (case opt325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x324 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x324))
       )), ("classType", cvtTYPE_EXPR x329), ("instanceType", cvtTYPE_EXPR x330)]))
   and cvtCTOR (Ctor{settings=x352, superArgs=ls354, func=x358}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x352), ("superArgs", PrettyRep.List (List.map (fn x353 => 
                                                                                                         cvtEXPR x353
                                                                                                  ) ls354)), 
          ("func", cvtFUNC x358)]))
   and cvtFUNC (Func{name=x368, fsig=x369, isNative=b370, block=x371, param=x372, 
          defaults=ls374, ty=x378}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x368), ("fsig", cvtFUNC_SIG x369), ("isNative", PrettyRep.Bool b370), 
          ("block", cvtBLOCK x371), ("param", cvtHEAD x372), ("defaults", PrettyRep.List (List.map (fn x373 => 
                                                                                                          cvtEXPR x373
                                                                                                   ) ls374)), 
          ("ty", cvtFUNC_TYPE x378)]))
   and cvtDEFN (ClassDefn x396) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x396))
     | cvtDEFN (VariableDefn x399) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x399))
     | cvtDEFN (FunctionDefn x402) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x402))
     | cvtDEFN (ConstructorDefn x405) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x405))
     | cvtDEFN (InterfaceDefn x408) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x408))
     | cvtDEFN (NamespaceDefn x411) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x411))
     | cvtDEFN (TypeDefn x414) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x414))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls418, params=x422, paramTypes=ls424, 
          defaults=ls429, ctorInits=opt440, returnType=x444, thisType=opt446, 
          hasRest=b450}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x417 => cvtIDENT x417
                                   ) ls418)), ("params", cvtBINDINGS x422), 
          ("paramTypes", PrettyRep.List (List.map (fn x423 => cvtTYPE_EXPR x423
                                                  ) ls424)), ("defaults", PrettyRep.List (List.map (fn x428 => 
                                                                                                          cvtEXPR x428
                                                                                                   ) ls429)), 
          ("ctorInits", 
       (case opt440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x433, ls435) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x433, 
            PrettyRep.List (List.map (fn x434 => cvtEXPR x434
                                     ) ls435)]))
       )), ("returnType", cvtTYPE_EXPR x444), ("thisType", 
       (case opt446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x445 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x445))
       )), ("hasRest", PrettyRep.Bool b450)]))
   and cvtBINDING (Binding{ident=x470, ty=x471}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x470), ("ty", cvtTYPE_EXPR x471)]))
   and cvtBINDING_IDENT (TempIdent n479) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n479))
     | cvtBINDING_IDENT (ParamIdent n482) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n482))
     | cvtBINDING_IDENT (PropIdent x485) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x485))
   and cvtINIT_STEP (InitStep(x488, x489)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x488, 
          cvtEXPR x489]))
     | cvtINIT_STEP (AssignStep(x493, x494)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x493, cvtEXPR x494]))
   and cvtTYPE_EXPR (SpecialType x498) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x498))
     | cvtTYPE_EXPR (UnionType ls502) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x501 => 
                                                                                                           cvtTYPE_EXPR x501
                                                                                                    ) ls502)))
     | cvtTYPE_EXPR (ArrayType ls509) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x508 => 
                                                                                                           cvtTYPE_EXPR x508
                                                                                                    ) ls509)))
     | cvtTYPE_EXPR (TypeName x515) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x515))
     | cvtTYPE_EXPR (ElementTypeRef(x518, n519)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x518, PrettyRep.Int n519]))
     | cvtTYPE_EXPR (FieldTypeRef(x523, x524)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x523, cvtIDENT x524]))
     | cvtTYPE_EXPR (FunctionType x528) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x528))
     | cvtTYPE_EXPR (ObjectType ls532) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x531 => 
                                                                                                             cvtFIELD_TYPE x531
                                                                                                      ) ls532)))
     | cvtTYPE_EXPR (AppType{base=x538, args=ls540}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x538), ("args", PrettyRep.List (List.map (fn x539 => 
                                                                                                     cvtTYPE_EXPR x539
                                                                                              ) ls540))]))
     | cvtTYPE_EXPR (NullableType{expr=x551, nullable=b552}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x551), ("nullable", PrettyRep.Bool b552)]))
     | cvtTYPE_EXPR (InstanceType{name=x560, typeParams=ls562, ty=x566, isDynamic=b567}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x560), 
          ("typeParams", PrettyRep.List (List.map (fn x561 => cvtIDENT x561
                                                  ) ls562)), ("ty", cvtTYPE_EXPR x566), 
          ("isDynamic", PrettyRep.Bool b567)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x580) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x580))
     | cvtSTMT (InitStmt{kind=x583, ns=opt585, prototype=b589, static=b590, 
          temps=x591, inits=ls593}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x583), ("ns", 
       (case opt585 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x584 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x584))
       )), ("prototype", PrettyRep.Bool b589), ("static", PrettyRep.Bool b590), 
          ("temps", cvtBINDINGS x591), ("inits", PrettyRep.List (List.map (fn x592 => 
                                                                                 cvtINIT_STEP x592
                                                                          ) ls593))]))
     | cvtSTMT (ClassBlock{ns=opt613, ident=x617, name=opt619, block=x623}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt613 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x612 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x612))
       )), ("ident", cvtIDENT x617), ("name", 
       (case opt619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x618 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x618))
       )), ("block", cvtBLOCK x623)]))
     | cvtSTMT (ForEachStmt x635) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x635))
     | cvtSTMT (ForInStmt x638) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x638))
     | cvtSTMT (ThrowStmt x641) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x641))
     | cvtSTMT (ReturnStmt x644) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x644))
     | cvtSTMT (BreakStmt opt648) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x647 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x647))
       ))
     | cvtSTMT (ContinueStmt opt655) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x654 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x654))
       ))
     | cvtSTMT (BlockStmt x661) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x661))
     | cvtSTMT (LabeledStmt(x664, x665)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x664, 
          cvtSTMT x665]))
     | cvtSTMT (LetStmt x669) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x669))
     | cvtSTMT (WhileStmt x672) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x672))
     | cvtSTMT (DoWhileStmt x675) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x675))
     | cvtSTMT (ForStmt x678) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x678))
     | cvtSTMT (IfStmt{cnd=x681, thn=x682, els=x683}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x681), ("thn", cvtSTMT x682), 
          ("els", cvtSTMT x683)]))
     | cvtSTMT (WithStmt{obj=x693, ty=x694, body=x695}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x693), ("ty", cvtTYPE_EXPR x694), 
          ("body", cvtSTMT x695)]))
     | cvtSTMT (TryStmt{block=x705, catches=ls723, finally=opt728}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x705), ("catches", PrettyRep.List (List.map (fn {bindings=x706, 
                                                                                                     ty=x707, 
                                                                                                     fixtures=opt709, 
                                                                                                     block=x713} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x706), 
                                                                                                     ("ty", 
                                                                                                     cvtTYPE_EXPR x707), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt709 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x708 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x708))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x713)]
                                                                                              ) ls723)), 
          ("finally", 
       (case opt728 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x727 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x727))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt742, cond=x746, cases=ls748}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt742 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x741 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x741))
       )), ("cond", cvtEXPR x746), ("cases", PrettyRep.List (List.map (fn x747 => 
                                                                             cvtCASE x747
                                                                      ) ls748))]))
     | cvtSTMT (SwitchTypeStmt{cond=x761, ty=x762, cases=ls764}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x761), ("ty", cvtTYPE_EXPR x762), 
          ("cases", PrettyRep.List (List.map (fn x763 => cvtTYPE_CASE x763
                                             ) ls764))]))
     | cvtSTMT (Dxns{expr=x777}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x777)]))
   and cvtEXPR (TrinaryExpr(x783, x784, x785, x786)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x783, cvtEXPR x784, cvtEXPR x785, 
          cvtEXPR x786]))
     | cvtEXPR (BinaryExpr(x790, x791, x792)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x790, cvtEXPR x791, cvtEXPR x792]))
     | cvtEXPR (BinaryTypeExpr(x796, x797, x798)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x796, cvtEXPR x797, cvtTYPE_EXPR x798]))
     | cvtEXPR (UnaryExpr(x802, x803)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x802, 
          cvtEXPR x803]))
     | cvtEXPR (TypeExpr x807) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x807))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt812) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt812 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x811 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x811))
       ))
     | cvtEXPR (SuperExpr opt819) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt819 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x818 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x818))
       ))
     | cvtEXPR (LiteralExpr x825) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x825))
     | cvtEXPR (CallExpr{func=x828, actuals=ls830}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x828), ("actuals", PrettyRep.List (List.map (fn x829 => 
                                                                                                   cvtEXPR x829
                                                                                            ) ls830))]))
     | cvtEXPR (ApplyTypeExpr{expr=x841, actuals=ls843}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x841), ("actuals", PrettyRep.List (List.map (fn x842 => 
                                                                                                   cvtTYPE_EXPR x842
                                                                                            ) ls843))]))
     | cvtEXPR (LetExpr{defs=x854, body=x855, head=opt857}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x854), ("body", cvtEXPR x855), 
          ("head", 
       (case opt857 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x856 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x856))
       ))]))
     | cvtEXPR (NewExpr{obj=x870, actuals=ls872}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x870), ("actuals", PrettyRep.List (List.map (fn x871 => 
                                                                                                  cvtEXPR x871
                                                                                           ) ls872))]))
     | cvtEXPR (ObjectRef{base=x883, ident=x884}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x883), ("ident", cvtIDENT_EXPR x884)]))
     | cvtEXPR (LexicalRef{ident=x892}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x892)]))
     | cvtEXPR (SetExpr(x898, x899, x900)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x898, 
          cvtEXPR x899, cvtEXPR x900]))
     | cvtEXPR (ListExpr ls905) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x904 => 
                                                                                                    cvtEXPR x904
                                                                                             ) ls905)))
     | cvtEXPR (InitExpr(x911, x912, x913)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x911, 
          cvtHEAD x912, cvtINITS x913]))
     | cvtEXPR (SliceExpr(x917, x918, x919)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x917, cvtEXPR x918, cvtEXPR x919]))
     | cvtEXPR (GetTemp n923) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n923))
     | cvtEXPR (GetParam n926) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n926))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n932) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n932))
     | cvtFIXTURE_NAME (PropName x935) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x935))
   and cvtIDENT_EXPR (Identifier{ident=x938, openNamespaces=ls944}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x938), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls940 => PrettyRep.List (List.map (fn x939 => 
                                                                                cvtNAMESPACE x939
                                                                         ) ls940)
                                   ) ls944))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x955, expr=x956}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x955), ("expr", cvtEXPR x956)]))
     | cvtIDENT_EXPR (AttributeIdentifier x964) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x964))
     | cvtIDENT_EXPR (ExpressionIdentifier x967) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x967))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x970, ident=x971}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x970), ("ident", cvtUSTRING x971)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x979, typeArgs=ls981}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x979), ("typeArgs", 
          PrettyRep.List (List.map (fn x980 => cvtTYPE_EXPR x980
                                   ) ls981))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls993, x997)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x992 => cvtIDENT x992
                                                          ) ls993), cvtIDENT_EXPR x997]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1003) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1003))
     | cvtLITERAL (LiteralContextualDecimalInteger s1006) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1006))
     | cvtLITERAL (LiteralContextualHexInteger s1009) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1009))
     | cvtLITERAL (LiteralDouble r1012) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1012))
     | cvtLITERAL (LiteralDecimal d1015) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1015))
     | cvtLITERAL (LiteralInt i1018) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1018))
     | cvtLITERAL (LiteralUInt u1021) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1021))
     | cvtLITERAL (LiteralBoolean b1024) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1024))
     | cvtLITERAL (LiteralString x1027) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1027))
     | cvtLITERAL (LiteralArray{exprs=ls1031, ty=opt1036}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1030 => 
                                                                         cvtEXPR x1030
                                                                  ) ls1031)), 
          ("ty", 
       (case opt1036 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1035 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1035))
       ))]))
     | cvtLITERAL (LiteralXML ls1048) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1047 => 
                                                                                                            cvtEXPR x1047
                                                                                                     ) ls1048)))
     | cvtLITERAL (LiteralNamespace x1054) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1054))
     | cvtLITERAL (LiteralObject{expr=ls1058, ty=opt1063}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1057 => 
                                                                        cvtFIELD x1057
                                                                 ) ls1058)), 
          ("ty", 
       (case opt1063 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1062 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1062))
       ))]))
     | cvtLITERAL (LiteralFunction x1074) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1074))
     | cvtLITERAL (LiteralRegExp{str=x1077}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1077)]))
   and cvtBLOCK (Block x1083) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1083))
   and cvtFIXTURE (NamespaceFixture x1086) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1086))
     | cvtFIXTURE (ClassFixture x1089) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1089))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1093) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1093))
     | cvtFIXTURE (MethodFixture{func=x1096, ty=x1097, readOnly=b1098, override=b1099, 
          final=b1100}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1096), ("ty", cvtTYPE_EXPR x1097), ("readOnly", PrettyRep.Bool b1098), 
          ("override", PrettyRep.Bool b1099), ("final", PrettyRep.Bool b1100)]))
     | cvtFIXTURE (ValFixture{ty=x1114, readOnly=b1115}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1114), ("readOnly", PrettyRep.Bool b1115)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1123, getter=opt1125, setter=opt1130}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1123), ("getter", 
       (case opt1125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1124 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1124))
       )), ("setter", 
       (case opt1130 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1129 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1129))
       ))]))
   and cvtBINDINGS (ls1144, ls1149) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1143 => 
                                                                                       cvtBINDING x1143
                                                                                ) ls1144), 
          PrettyRep.List (List.map (fn x1148 => cvtINIT_STEP x1148
                                   ) ls1149)]
   and cvtFIXTURES ls1157 = PrettyRep.List (List.map (fn (x1154, x1155) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1154, 
                                                            cvtFIXTURE x1155]
                                                     ) ls1157)
   and cvtINITS ls1164 = PrettyRep.List (List.map (fn (x1161, x1162) => PrettyRep.Tuple [cvtFIXTURE_NAME x1161, 
                                                         cvtEXPR x1162]
                                                  ) ls1164)
   and cvtHEAD (x1168, x1169) = PrettyRep.Tuple [cvtFIXTURES x1168, cvtINITS x1169]
   and cvtFIELD {kind=x1171, name=x1172, init=x1173} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1171), ("name", cvtIDENT_EXPR x1172), ("init", cvtEXPR x1173)]
   and cvtFIELD_TYPE {name=x1181, ty=x1182} = PrettyRep.Rec [("name", cvtIDENT x1181), 
          ("ty", cvtTYPE_EXPR x1182)]
   and cvtTYPED_IDENT {name=x1188, ty=opt1190} = PrettyRep.Rec [("name", cvtIDENT x1188), 
          ("ty", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1189 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1189))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1200, params=ls1205, result=x1209, thisType=opt1211, 
          hasRest=b1215, minArgs=n1216} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1199 => 
                                                                                                        cvtIDENT x1199
                                                                                                 ) ls1200)), 
          ("params", PrettyRep.List (List.map (fn x1204 => cvtTYPE_EXPR x1204
                                              ) ls1205)), ("result", cvtTYPE_EXPR x1209), 
          ("thisType", 
       (case opt1211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1210 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1210))
       )), ("hasRest", PrettyRep.Bool b1215), ("minArgs", PrettyRep.Int n1216)]
   and cvtFUNC_DEFN {kind=x1230, ns=opt1232, final=b1236, override=b1237, prototype=b1238, 
          static=b1239, func=x1240} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1230), 
          ("ns", 
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1231))
       )), ("final", PrettyRep.Bool b1236), ("override", PrettyRep.Bool b1237), 
          ("prototype", PrettyRep.Bool b1238), ("static", PrettyRep.Bool b1239), 
          ("func", cvtFUNC x1240)]
   and cvtCTOR_DEFN x1256 = cvtCTOR x1256
   and cvtVAR_DEFN {kind=x1257, ns=opt1259, static=b1263, prototype=b1264, 
          bindings=x1265} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1257), 
          ("ns", 
       (case opt1259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1258 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1258))
       )), ("static", PrettyRep.Bool b1263), ("prototype", PrettyRep.Bool b1264), 
          ("bindings", cvtBINDINGS x1265)]
   and cvtNAMESPACE_DEFN {ident=x1277, ns=opt1279, init=opt1284} = PrettyRep.Rec [("ident", 
          cvtIDENT x1277), ("ns", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1278))
       )), ("init", 
       (case opt1284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1283 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1283))
       ))]
   and cvtCLASS_DEFN {ident=x1295, ns=opt1297, nonnullable=b1301, dynamic=b1302, 
          final=b1303, params=ls1305, extends=opt1310, implements=ls1315, classDefns=ls1320, 
          instanceDefns=ls1325, instanceStmts=ls1330, ctorDefn=opt1335} = PrettyRep.Rec [("ident", 
          cvtIDENT x1295), ("ns", 
       (case opt1297 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1296 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1296))
       )), ("nonnullable", PrettyRep.Bool b1301), ("dynamic", PrettyRep.Bool b1302), 
          ("final", PrettyRep.Bool b1303), ("params", PrettyRep.List (List.map (fn x1304 => 
                                                                                      cvtIDENT x1304
                                                                               ) ls1305)), 
          ("extends", 
       (case opt1310 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1309 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1309))
       )), ("implements", PrettyRep.List (List.map (fn x1314 => cvtIDENT_EXPR x1314
                                                   ) ls1315)), ("classDefns", 
          PrettyRep.List (List.map (fn x1319 => cvtDEFN x1319
                                   ) ls1320)), ("instanceDefns", PrettyRep.List (List.map (fn x1324 => 
                                                                                                 cvtDEFN x1324
                                                                                          ) ls1325)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1329 => cvtSTMT x1329
                                                     ) ls1330)), ("ctorDefn", 
          
       (case opt1335 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1334 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1334))
       ))]
   and cvtINTERFACE_DEFN {ident=x1364, ns=opt1366, nonnullable=b1370, params=ls1372, 
          extends=ls1377, block=x1381} = PrettyRep.Rec [("ident", cvtIDENT x1364), 
          ("ns", 
       (case opt1366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1365 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1365))
       )), ("nonnullable", PrettyRep.Bool b1370), ("params", PrettyRep.List (List.map (fn x1371 => 
                                                                                             cvtIDENT x1371
                                                                                      ) ls1372)), 
          ("extends", PrettyRep.List (List.map (fn x1376 => cvtIDENT_EXPR x1376
                                               ) ls1377)), ("block", cvtBLOCK x1381)]
   and cvtTYPE_DEFN {ident=x1395, ns=opt1397, init=x1401} = PrettyRep.Rec [("ident", 
          cvtIDENT x1395), ("ns", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1396))
       )), ("init", cvtTYPE_EXPR x1401)]
   and cvtFOR_ENUM_STMT {defn=opt1410, obj=x1414, fixtures=opt1416, inits=opt1421, 
          labels=ls1426, body=x1430} = PrettyRep.Rec [("defn", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1409 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1409))
       )), ("obj", cvtEXPR x1414), ("fixtures", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1415))
       )), ("inits", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1420 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1420))
       )), ("labels", PrettyRep.List (List.map (fn x1425 => cvtIDENT x1425
                                               ) ls1426)), ("body", cvtSTMT x1430)]
   and cvtFOR_STMT {fixtures=opt1445, defn=opt1450, init=x1454, cond=x1455, 
          update=x1456, labels=ls1458, body=x1462} = PrettyRep.Rec [("fixtures", 
          
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1444))
       )), ("defn", 
       (case opt1450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1449 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1449))
       )), ("init", cvtSTMT x1454), ("cond", cvtEXPR x1455), ("update", cvtEXPR x1456), 
          ("labels", PrettyRep.List (List.map (fn x1457 => cvtIDENT x1457
                                              ) ls1458)), ("body", cvtSTMT x1462)]
   and cvtWHILE_STMT {cond=x1478, fixtures=opt1480, body=x1484, labels=ls1486} = 
          PrettyRep.Rec [("cond", cvtEXPR x1478), ("fixtures", 
       (case opt1480 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1479 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1479))
       )), ("body", cvtSTMT x1484), ("labels", PrettyRep.List (List.map (fn x1485 => 
                                                                               cvtIDENT x1485
                                                                        ) ls1486))]
   and cvtDIRECTIVES {pragmas=ls1500, defns=ls1505, head=opt1510, body=ls1515} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1499 => 
                                                                     cvtPRAGMA x1499
                                                              ) ls1500)), ("defns", 
          PrettyRep.List (List.map (fn x1504 => cvtDEFN x1504
                                   ) ls1505)), ("head", 
       (case opt1510 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1509 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1509))
       )), ("body", PrettyRep.List (List.map (fn x1514 => cvtSTMT x1514
                                             ) ls1515))]
   and cvtCASE {label=opt1529, inits=opt1534, body=x1538} = PrettyRep.Rec [("label", 
          
       (case opt1529 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1528 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1528))
       )), ("inits", 
       (case opt1534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1533))
       )), ("body", cvtBLOCK x1538)]
   and cvtTYPE_CASE {ty=opt1547, bindings=x1551, inits=opt1553, body=x1557} = 
          PrettyRep.Rec [("ty", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1546))
       )), ("bindings", cvtBINDINGS x1551), ("inits", 
       (case opt1553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1552 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1552))
       )), ("body", cvtBLOCK x1557)]
   and cvtFUNC_NAME {kind=x1567, ident=x1568} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1567), 
          ("ident", cvtIDENT x1568)]
   and cvtPACKAGE {name=ls1575, block=x1579} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1574 => 
                                                                                                       cvtIDENT x1574
                                                                                                ) ls1575)), 
          ("block", cvtBLOCK x1579)]
   and cvtPROGRAM {packages=ls1586, fixtures=opt1591, block=x1595} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1585 => cvtPACKAGE x1585
                                   ) ls1586)), ("fixtures", 
       (case opt1591 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1590 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1590))
       )), ("block", cvtBLOCK x1595)]
end

