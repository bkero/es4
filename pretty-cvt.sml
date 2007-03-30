structure PrettyCvt = struct
   open Ast
   fun cvtPOS {file=s0, line=n1} = PrettyRep.Rec [("file", PrettyRep.String s0), 
          ("line", PrettyRep.Int n1)]
   and cvtUSTRING s7 = PrettyRep.String s7
   and cvtIDENT x8 = cvtUSTRING x8
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x11) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x11))
     | cvtNAMESPACE (Protected x14) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x14))
     | cvtNAMESPACE (Public x17) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x17))
     | cvtNAMESPACE (Internal x20) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x20))
     | cvtNAMESPACE (UserNamespace x23) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x23))
     | cvtNAMESPACE (Imported(x26, x27)) = PrettyRep.Ctor ("Imported", SOME (PrettyRep.Tuple [cvtIDENT x26, 
          cvtIDENT x27]))
   and cvtNAME {ns=x31, id=x32} = PrettyRep.Rec [("ns", cvtNAMESPACE x31), 
          ("id", cvtIDENT x32)]
   and cvtMULTINAME {nss=ls43, id=x47} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls39 => 
                                                                                                PrettyRep.List (List.map (fn x38 => 
                                                                                                                                cvtNAMESPACE x38
                                                                                                                         ) ls39)
                                                                                         ) ls43)), 
          ("id", cvtIDENT x47)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x58, roundingMode=r59, precision=n60} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x58), ("roundingMode", 
          PrettyRep.DecRm r59), ("precision", PrettyRep.Int n60)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt73) = PrettyRep.Ctor ("Plus", SOME 
       (case opt73 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x72 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x72))
       ))
     | cvtBINOP (Minus opt80) = PrettyRep.Ctor ("Minus", SOME 
       (case opt80 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x79 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x79))
       ))
     | cvtBINOP (Times opt87) = PrettyRep.Ctor ("Times", SOME 
       (case opt87 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
       ))
     | cvtBINOP (Divide opt94) = PrettyRep.Ctor ("Divide", SOME 
       (case opt94 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
       ))
     | cvtBINOP (Remainder opt101) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x100 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x100))
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
     | cvtBINOP (Equals opt118) = PrettyRep.Ctor ("Equals", SOME 
       (case opt118 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x117 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x117))
       ))
     | cvtBINOP (NotEquals opt125) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x124 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x124))
       ))
     | cvtBINOP (StrictEquals opt132) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
       ))
     | cvtBINOP (StrictNotEquals opt139) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
       ))
     | cvtBINOP (Less opt146) = PrettyRep.Ctor ("Less", SOME 
       (case opt146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
       ))
     | cvtBINOP (LessOrEqual opt153) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
       ))
     | cvtBINOP (Greater opt160) = PrettyRep.Ctor ("Greater", SOME 
       (case opt160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
       ))
     | cvtBINOP (GreaterOrEqual opt167) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x166))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt176) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x175 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x175))
       ))
     | cvtASSIGNOP (AssignMinus opt183) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x182))
       ))
     | cvtASSIGNOP (AssignTimes opt190) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
       ))
     | cvtASSIGNOP (AssignDivide opt197) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
       ))
     | cvtASSIGNOP (AssignRemainder opt204) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt204 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x203 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x203))
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
     | cvtUNOP (PreIncrement opt222) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x221 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x221))
       ))
     | cvtUNOP (PreDecrement opt229) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x228 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x228))
       ))
     | cvtUNOP (PostIncrement opt236) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
       ))
     | cvtUNOP (PostDecrement opt243) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
       ))
     | cvtUNOP (UnaryPlus opt250) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
       ))
     | cvtUNOP (UnaryMinus opt257) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x256))
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
   and cvtPRAGMA (UseNamespace x274) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x274))
     | cvtPRAGMA (UseDefaultNamespace x277) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x277))
     | cvtPRAGMA (UseNumber x280) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x280))
     | cvtPRAGMA (UseRounding r283) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r283))
     | cvtPRAGMA (UsePrecision n286) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n286))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls292, name=x296, alias=opt298}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x291 => 
                                                                           cvtIDENT x291
                                                                    ) ls292)), 
          ("name", cvtIDENT x296), ("alias", 
       (case opt298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x297 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x297))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x317, extends=opt319, implements=ls324, classFixtures=x328, 
          instanceFixtures=x329, instanceInits=x330, constructor=opt332, classType=x336, 
          instanceType=x337}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x317), ("extends", 
       (case opt319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x318 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x318))
       )), ("implements", PrettyRep.List (List.map (fn x323 => cvtNAME x323
                                                   ) ls324)), ("classFixtures", 
          cvtFIXTURES x328), ("instanceFixtures", cvtFIXTURES x329), ("instanceInits", 
          cvtHEAD x330), ("constructor", 
       (case opt332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x331 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x331))
       )), ("classType", cvtTYPE_EXPR x336), ("instanceType", cvtTYPE_EXPR x337)]))
   and cvtCTOR (Ctor{settings=x359, superArgs=ls361, func=x365}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x359), ("superArgs", PrettyRep.List (List.map (fn x360 => 
                                                                                                         cvtEXPR x360
                                                                                                  ) ls361)), 
          ("func", cvtFUNC x365)]))
   and cvtFUNC (Func{name=x375, fsig=x376, isNative=b377, block=x378, param=x379, 
          defaults=ls381, ty=x385}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x375), ("fsig", cvtFUNC_SIG x376), ("isNative", PrettyRep.Bool b377), 
          ("block", cvtBLOCK x378), ("param", cvtHEAD x379), ("defaults", PrettyRep.List (List.map (fn x380 => 
                                                                                                          cvtEXPR x380
                                                                                                   ) ls381)), 
          ("ty", cvtFUNC_TYPE x385)]))
   and cvtDEFN (ClassDefn x403) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x403))
     | cvtDEFN (VariableDefn x406) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x406))
     | cvtDEFN (FunctionDefn x409) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x409))
     | cvtDEFN (ConstructorDefn x412) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x412))
     | cvtDEFN (InterfaceDefn x415) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x415))
     | cvtDEFN (NamespaceDefn x418) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x418))
     | cvtDEFN (TypeDefn x421) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x421))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls425, params=x429, paramTypes=ls431, 
          defaults=ls436, ctorInits=opt447, returnType=x451, thisType=opt453, 
          hasRest=b457}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x424 => cvtIDENT x424
                                   ) ls425)), ("params", cvtBINDINGS x429), 
          ("paramTypes", PrettyRep.List (List.map (fn x430 => cvtTYPE_EXPR x430
                                                  ) ls431)), ("defaults", PrettyRep.List (List.map (fn x435 => 
                                                                                                          cvtEXPR x435
                                                                                                   ) ls436)), 
          ("ctorInits", 
       (case opt447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x440, ls442) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x440, 
            PrettyRep.List (List.map (fn x441 => cvtEXPR x441
                                     ) ls442)]))
       )), ("returnType", cvtTYPE_EXPR x451), ("thisType", 
       (case opt453 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x452 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x452))
       )), ("hasRest", PrettyRep.Bool b457)]))
   and cvtBINDING (Binding{ident=x477, ty=x478}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x477), ("ty", cvtTYPE_EXPR x478)]))
   and cvtBINDING_IDENT (TempIdent n486) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n486))
     | cvtBINDING_IDENT (ParamIdent n489) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n489))
     | cvtBINDING_IDENT (PropIdent x492) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x492))
   and cvtINIT_STEP (InitStep(x495, x496)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x495, 
          cvtEXPR x496]))
     | cvtINIT_STEP (AssignStep(x500, x501)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x500, cvtEXPR x501]))
   and cvtTYPE_EXPR (SpecialType x505) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x505))
     | cvtTYPE_EXPR (UnionType ls509) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x508 => 
                                                                                                           cvtTYPE_EXPR x508
                                                                                                    ) ls509)))
     | cvtTYPE_EXPR (ArrayType ls516) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x515 => 
                                                                                                           cvtTYPE_EXPR x515
                                                                                                    ) ls516)))
     | cvtTYPE_EXPR (TypeName x522) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x522))
     | cvtTYPE_EXPR (ElementTypeRef(x525, n526)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x525, PrettyRep.Int n526]))
     | cvtTYPE_EXPR (FieldTypeRef(x530, x531)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x530, cvtIDENT x531]))
     | cvtTYPE_EXPR (FunctionType x535) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x535))
     | cvtTYPE_EXPR (ObjectType ls539) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x538 => 
                                                                                                             cvtFIELD_TYPE x538
                                                                                                      ) ls539)))
     | cvtTYPE_EXPR (AppType{base=x545, args=ls547}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x545), ("args", PrettyRep.List (List.map (fn x546 => 
                                                                                                     cvtTYPE_EXPR x546
                                                                                              ) ls547))]))
     | cvtTYPE_EXPR (NullableType{expr=x558, nullable=b559}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x558), ("nullable", PrettyRep.Bool b559)]))
     | cvtTYPE_EXPR (InstanceType{name=x567, typeParams=ls569, ty=x573, isDynamic=b574}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x567), 
          ("typeParams", PrettyRep.List (List.map (fn x568 => cvtIDENT x568
                                                  ) ls569)), ("ty", cvtTYPE_EXPR x573), 
          ("isDynamic", PrettyRep.Bool b574)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x587) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x587))
     | cvtSTMT (InitStmt{kind=x590, ns=opt592, prototype=b596, static=b597, 
          temps=x598, inits=ls600}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x590), ("ns", 
       (case opt592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x591 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x591))
       )), ("prototype", PrettyRep.Bool b596), ("static", PrettyRep.Bool b597), 
          ("temps", cvtBINDINGS x598), ("inits", PrettyRep.List (List.map (fn x599 => 
                                                                                 cvtINIT_STEP x599
                                                                          ) ls600))]))
     | cvtSTMT (ClassBlock{ns=opt620, ident=x624, name=opt626, block=x630}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt620 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x619 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x619))
       )), ("ident", cvtIDENT x624), ("name", 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x625))
       )), ("block", cvtBLOCK x630)]))
     | cvtSTMT (ForInStmt x642) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x642))
     | cvtSTMT (ThrowStmt x645) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x645))
     | cvtSTMT (ReturnStmt x648) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x648))
     | cvtSTMT (BreakStmt opt652) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt652 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x651 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x651))
       ))
     | cvtSTMT (ContinueStmt opt659) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x658 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x658))
       ))
     | cvtSTMT (BlockStmt x665) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x665))
     | cvtSTMT (LabeledStmt(x668, x669)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x668, 
          cvtSTMT x669]))
     | cvtSTMT (LetStmt x673) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x673))
     | cvtSTMT (WhileStmt x676) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x676))
     | cvtSTMT (DoWhileStmt x679) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x679))
     | cvtSTMT (ForStmt x682) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x682))
     | cvtSTMT (IfStmt{cnd=x685, thn=x686, els=x687}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x685), ("thn", cvtSTMT x686), 
          ("els", cvtSTMT x687)]))
     | cvtSTMT (WithStmt{obj=x697, ty=x698, body=x699}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x697), ("ty", cvtTYPE_EXPR x698), 
          ("body", cvtSTMT x699)]))
     | cvtSTMT (TryStmt{block=x709, catches=ls711, finally=opt716}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x709), ("catches", PrettyRep.List (List.map (fn x710 => 
                                                                                                     cvtCATCH_CLAUSE x710
                                                                                              ) ls711)), 
          ("finally", 
       (case opt716 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x715 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x715))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt730, cond=x734, cases=ls736}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt730 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x729 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x729))
       )), ("cond", cvtEXPR x734), ("cases", PrettyRep.List (List.map (fn x735 => 
                                                                             cvtCASE x735
                                                                      ) ls736))]))
     | cvtSTMT (SwitchTypeStmt{cond=x749, ty=x750, cases=ls752}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x749), ("ty", cvtTYPE_EXPR x750), 
          ("cases", PrettyRep.List (List.map (fn x751 => cvtTYPE_CASE x751
                                             ) ls752))]))
     | cvtSTMT (Dxns{expr=x765}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x765)]))
   and cvtEXPR (TrinaryExpr(x771, x772, x773, x774)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x771, cvtEXPR x772, cvtEXPR x773, 
          cvtEXPR x774]))
     | cvtEXPR (BinaryExpr(x778, x779, x780)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x778, cvtEXPR x779, cvtEXPR x780]))
     | cvtEXPR (BinaryTypeExpr(x784, x785, x786)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x784, cvtEXPR x785, cvtTYPE_EXPR x786]))
     | cvtEXPR (UnaryExpr(x790, x791)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x790, 
          cvtEXPR x791]))
     | cvtEXPR (TypeExpr x795) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x795))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt800) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt800 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x799 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x799))
       ))
     | cvtEXPR (SuperExpr opt807) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt807 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x806 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x806))
       ))
     | cvtEXPR (LiteralExpr x813) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x813))
     | cvtEXPR (CallExpr{func=x816, actuals=ls818}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x816), ("actuals", PrettyRep.List (List.map (fn x817 => 
                                                                                                   cvtEXPR x817
                                                                                            ) ls818))]))
     | cvtEXPR (ApplyTypeExpr{expr=x829, actuals=ls831}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x829), ("actuals", PrettyRep.List (List.map (fn x830 => 
                                                                                                   cvtTYPE_EXPR x830
                                                                                            ) ls831))]))
     | cvtEXPR (LetExpr{defs=x842, body=x843, head=opt845}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x842), ("body", cvtEXPR x843), 
          ("head", 
       (case opt845 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x844 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x844))
       ))]))
     | cvtEXPR (NewExpr{obj=x858, actuals=ls860}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x858), ("actuals", PrettyRep.List (List.map (fn x859 => 
                                                                                                  cvtEXPR x859
                                                                                           ) ls860))]))
     | cvtEXPR (ObjectRef{base=x871, ident=x872, pos=opt874}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x871), ("ident", cvtIDENT_EXPR x872), 
          ("pos", 
       (case opt874 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x873 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x873))
       ))]))
     | cvtEXPR (LexicalRef{ident=x887, pos=opt889}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x887), ("pos", 
       (case opt889 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x888 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x888))
       ))]))
     | cvtEXPR (SetExpr(x900, x901, x902)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x900, 
          cvtEXPR x901, cvtEXPR x902]))
     | cvtEXPR (ListExpr ls907) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x906 => 
                                                                                                    cvtEXPR x906
                                                                                             ) ls907)))
     | cvtEXPR (InitExpr(x913, x914, x915)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x913, 
          cvtHEAD x914, cvtINITS x915]))
     | cvtEXPR (SliceExpr(x919, x920, x921)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x919, cvtEXPR x920, cvtEXPR x921]))
     | cvtEXPR (GetTemp n925) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n925))
     | cvtEXPR (GetParam n928) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n928))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n934) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n934))
     | cvtFIXTURE_NAME (PropName x937) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x937))
   and cvtIDENT_EXPR (Identifier{ident=x940, openNamespaces=ls946}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x940), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls942 => PrettyRep.List (List.map (fn x941 => 
                                                                                cvtNAMESPACE x941
                                                                         ) ls942)
                                   ) ls946))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x957, expr=x958}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x957), ("expr", cvtEXPR x958)]))
     | cvtIDENT_EXPR (AttributeIdentifier x966) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x966))
     | cvtIDENT_EXPR (ExpressionIdentifier x969) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x969))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x972, ident=x973}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x972), ("ident", cvtUSTRING x973)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x981, typeArgs=ls983}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x981), ("typeArgs", 
          PrettyRep.List (List.map (fn x982 => cvtTYPE_EXPR x982
                                   ) ls983))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls995, x999)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x994 => cvtIDENT x994
                                                          ) ls995), cvtIDENT_EXPR x999]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1005) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1005))
     | cvtLITERAL (LiteralContextualDecimalInteger s1008) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1008))
     | cvtLITERAL (LiteralContextualHexInteger s1011) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1011))
     | cvtLITERAL (LiteralDouble r1014) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1014))
     | cvtLITERAL (LiteralDecimal d1017) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1017))
     | cvtLITERAL (LiteralInt i1020) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1020))
     | cvtLITERAL (LiteralUInt u1023) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1023))
     | cvtLITERAL (LiteralBoolean b1026) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1026))
     | cvtLITERAL (LiteralString x1029) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1029))
     | cvtLITERAL (LiteralArray{exprs=ls1033, ty=opt1038}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1032 => 
                                                                         cvtEXPR x1032
                                                                  ) ls1033)), 
          ("ty", 
       (case opt1038 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1037 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1037))
       ))]))
     | cvtLITERAL (LiteralXML ls1050) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1049 => 
                                                                                                            cvtEXPR x1049
                                                                                                     ) ls1050)))
     | cvtLITERAL (LiteralNamespace x1056) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1056))
     | cvtLITERAL (LiteralObject{expr=ls1060, ty=opt1065}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1059 => 
                                                                        cvtFIELD x1059
                                                                 ) ls1060)), 
          ("ty", 
       (case opt1065 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1064 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1064))
       ))]))
     | cvtLITERAL (LiteralFunction x1076) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1076))
     | cvtLITERAL (LiteralRegExp{str=x1079}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1079)]))
   and cvtBLOCK (Block x1085) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1085))
   and cvtFIXTURE (NamespaceFixture x1088) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1088))
     | cvtFIXTURE (ClassFixture x1091) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1091))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1096) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1096))
     | cvtFIXTURE (MethodFixture{func=x1099, ty=x1100, readOnly=b1101, override=b1102, 
          final=b1103}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1099), ("ty", cvtTYPE_EXPR x1100), ("readOnly", PrettyRep.Bool b1101), 
          ("override", PrettyRep.Bool b1102), ("final", PrettyRep.Bool b1103)]))
     | cvtFIXTURE (ValFixture{ty=x1117, readOnly=b1118}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1117), ("readOnly", PrettyRep.Bool b1118)]))
     | cvtFIXTURE (VirtualValFixture x1126) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1126))
   and cvtBINDINGS (ls1130, ls1135) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1129 => 
                                                                                       cvtBINDING x1129
                                                                                ) ls1130), 
          PrettyRep.List (List.map (fn x1134 => cvtINIT_STEP x1134
                                   ) ls1135)]
   and cvtFIXTURES ls1143 = PrettyRep.List (List.map (fn (x1140, x1141) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1140, 
                                                            cvtFIXTURE x1141]
                                                     ) ls1143)
   and cvtINITS ls1150 = PrettyRep.List (List.map (fn (x1147, x1148) => PrettyRep.Tuple [cvtFIXTURE_NAME x1147, 
                                                         cvtEXPR x1148]
                                                  ) ls1150)
   and cvtHEAD (x1154, x1155) = PrettyRep.Tuple [cvtFIXTURES x1154, cvtINITS x1155]
   and cvtFIELD {kind=x1157, name=x1158, init=x1159} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1157), ("name", cvtIDENT_EXPR x1158), ("init", cvtEXPR x1159)]
   and cvtFIELD_TYPE {name=x1167, ty=x1168} = PrettyRep.Rec [("name", cvtIDENT x1167), 
          ("ty", cvtTYPE_EXPR x1168)]
   and cvtTYPED_IDENT {name=x1174, ty=opt1176} = PrettyRep.Rec [("name", cvtIDENT x1174), 
          ("ty", 
       (case opt1176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1175 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1175))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1186, params=ls1191, result=x1195, thisType=opt1197, 
          hasRest=b1201, minArgs=n1202} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1185 => 
                                                                                                        cvtIDENT x1185
                                                                                                 ) ls1186)), 
          ("params", PrettyRep.List (List.map (fn x1190 => cvtTYPE_EXPR x1190
                                              ) ls1191)), ("result", cvtTYPE_EXPR x1195), 
          ("thisType", 
       (case opt1197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1196 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1196))
       )), ("hasRest", PrettyRep.Bool b1201), ("minArgs", PrettyRep.Int n1202)]
   and cvtFUNC_DEFN {kind=x1216, ns=opt1218, final=b1222, override=b1223, prototype=b1224, 
          static=b1225, func=x1226} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1216), 
          ("ns", 
       (case opt1218 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1217 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1217))
       )), ("final", PrettyRep.Bool b1222), ("override", PrettyRep.Bool b1223), 
          ("prototype", PrettyRep.Bool b1224), ("static", PrettyRep.Bool b1225), 
          ("func", cvtFUNC x1226)]
   and cvtCTOR_DEFN x1242 = cvtCTOR x1242
   and cvtVAR_DEFN {kind=x1243, ns=opt1245, static=b1249, prototype=b1250, 
          bindings=x1251} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1243), 
          ("ns", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1244))
       )), ("static", PrettyRep.Bool b1249), ("prototype", PrettyRep.Bool b1250), 
          ("bindings", cvtBINDINGS x1251)]
   and cvtNAMESPACE_DEFN {ident=x1263, ns=opt1265, init=opt1270} = PrettyRep.Rec [("ident", 
          cvtIDENT x1263), ("ns", 
       (case opt1265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1264 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1264))
       )), ("init", 
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1269))
       ))]
   and cvtCLASS_DEFN {ident=x1281, ns=opt1283, nonnullable=b1287, dynamic=b1288, 
          final=b1289, params=ls1291, extends=opt1296, implements=ls1301, classDefns=ls1306, 
          instanceDefns=ls1311, instanceStmts=ls1316, ctorDefn=opt1321} = PrettyRep.Rec [("ident", 
          cvtIDENT x1281), ("ns", 
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1282))
       )), ("nonnullable", PrettyRep.Bool b1287), ("dynamic", PrettyRep.Bool b1288), 
          ("final", PrettyRep.Bool b1289), ("params", PrettyRep.List (List.map (fn x1290 => 
                                                                                      cvtIDENT x1290
                                                                               ) ls1291)), 
          ("extends", 
       (case opt1296 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1295 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1295))
       )), ("implements", PrettyRep.List (List.map (fn x1300 => cvtIDENT_EXPR x1300
                                                   ) ls1301)), ("classDefns", 
          PrettyRep.List (List.map (fn x1305 => cvtDEFN x1305
                                   ) ls1306)), ("instanceDefns", PrettyRep.List (List.map (fn x1310 => 
                                                                                                 cvtDEFN x1310
                                                                                          ) ls1311)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1315 => cvtSTMT x1315
                                                     ) ls1316)), ("ctorDefn", 
          
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1320))
       ))]
   and cvtINTERFACE_DEFN {ident=x1350, ns=opt1352, nonnullable=b1356, params=ls1358, 
          extends=ls1363, block=x1367} = PrettyRep.Rec [("ident", cvtIDENT x1350), 
          ("ns", 
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1351))
       )), ("nonnullable", PrettyRep.Bool b1356), ("params", PrettyRep.List (List.map (fn x1357 => 
                                                                                             cvtIDENT x1357
                                                                                      ) ls1358)), 
          ("extends", PrettyRep.List (List.map (fn x1362 => cvtIDENT_EXPR x1362
                                               ) ls1363)), ("block", cvtBLOCK x1367)]
   and cvtTYPE_DEFN {ident=x1381, ns=opt1383, init=x1387} = PrettyRep.Rec [("ident", 
          cvtIDENT x1381), ("ns", 
       (case opt1383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1382 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1382))
       )), ("init", cvtTYPE_EXPR x1387)]
   and cvtFOR_ENUM_STMT {isEach=b1395, defn=opt1397, obj=x1401, fixtures=opt1403, 
          init=ls1408, labels=ls1413, body=x1417} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1395), ("defn", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1396))
       )), ("obj", cvtEXPR x1401), ("fixtures", 
       (case opt1403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1402 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1402))
       )), ("init", PrettyRep.List (List.map (fn x1407 => cvtSTMT x1407
                                             ) ls1408)), ("labels", PrettyRep.List (List.map (fn x1412 => 
                                                                                                    cvtIDENT x1412
                                                                                             ) ls1413)), 
          ("body", cvtSTMT x1417)]
   and cvtFOR_STMT {fixtures=opt1434, defn=opt1439, init=ls1444, cond=x1448, 
          update=x1449, labels=ls1451, body=x1455} = PrettyRep.Rec [("fixtures", 
          
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1433))
       )), ("defn", 
       (case opt1439 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1438))
       )), ("init", PrettyRep.List (List.map (fn x1443 => cvtSTMT x1443
                                             ) ls1444)), ("cond", cvtEXPR x1448), 
          ("update", cvtEXPR x1449), ("labels", PrettyRep.List (List.map (fn x1450 => 
                                                                                cvtIDENT x1450
                                                                         ) ls1451)), 
          ("body", cvtSTMT x1455)]
   and cvtWHILE_STMT {cond=x1471, fixtures=opt1473, body=x1477, labels=ls1479} = 
          PrettyRep.Rec [("cond", cvtEXPR x1471), ("fixtures", 
       (case opt1473 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1472 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1472))
       )), ("body", cvtSTMT x1477), ("labels", PrettyRep.List (List.map (fn x1478 => 
                                                                               cvtIDENT x1478
                                                                        ) ls1479))]
   and cvtDIRECTIVES {pragmas=ls1493, defns=ls1498, head=opt1503, body=ls1508, 
          pos=opt1513} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1492 => 
                                                                                    cvtPRAGMA x1492
                                                                             ) ls1493)), 
          ("defns", PrettyRep.List (List.map (fn x1497 => cvtDEFN x1497
                                             ) ls1498)), ("head", 
       (case opt1503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1502 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1502))
       )), ("body", PrettyRep.List (List.map (fn x1507 => cvtSTMT x1507
                                             ) ls1508)), ("pos", 
       (case opt1513 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1512 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1512))
       ))]
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
   and cvtCATCH_CLAUSE {bindings=x1567, ty=x1568, fixtures=opt1570, block=x1574} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1567), ("ty", cvtTYPE_EXPR x1568), 
          ("fixtures", 
       (case opt1570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1569 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1569))
       )), ("block", cvtBLOCK x1574)]
   and cvtFUNC_NAME {kind=x1584, ident=x1585} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1584), 
          ("ident", cvtIDENT x1585)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1591, getter=opt1593, setter=opt1598} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1591), ("getter", 
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1592 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1592))
       )), ("setter", 
       (case opt1598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1597 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1597))
       ))]
   and cvtPACKAGE {name=ls1610, block=x1614} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1609 => 
                                                                                                       cvtIDENT x1609
                                                                                                ) ls1610)), 
          ("block", cvtBLOCK x1614)]
   and cvtPROGRAM {packages=ls1621, fixtures=opt1626, block=x1630} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1620 => cvtPACKAGE x1620
                                   ) ls1621)), ("fixtures", 
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1625 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1625))
       )), ("block", cvtBLOCK x1630)]
end

