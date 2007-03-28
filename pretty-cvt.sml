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
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
   and cvtCLS (Cls{name=x314, extends=opt316, implements=ls321, classFixtures=x325, 
          instanceFixtures=x326, instanceInits=x327, constructor=opt329, classType=x333, 
          instanceType=x334}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x314), ("extends", 
       (case opt316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x315 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x315))
       )), ("implements", PrettyRep.List (List.map (fn x320 => cvtNAME x320
                                                   ) ls321)), ("classFixtures", 
          cvtFIXTURES x325), ("instanceFixtures", cvtFIXTURES x326), ("instanceInits", 
          cvtHEAD x327), ("constructor", 
       (case opt329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x328 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x328))
       )), ("classType", cvtTYPE_EXPR x333), ("instanceType", cvtTYPE_EXPR x334)]))
   and cvtCTOR (Ctor{settings=x356, superArgs=ls358, func=x362}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x356), ("superArgs", PrettyRep.List (List.map (fn x357 => 
                                                                                                         cvtEXPR x357
                                                                                                  ) ls358)), 
          ("func", cvtFUNC x362)]))
   and cvtFUNC (Func{name=x372, fsig=x373, isNative=b374, block=x375, param=x376, 
          defaults=ls378, ty=x382}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x372), ("fsig", cvtFUNC_SIG x373), ("isNative", PrettyRep.Bool b374), 
          ("block", cvtBLOCK x375), ("param", cvtHEAD x376), ("defaults", PrettyRep.List (List.map (fn x377 => 
                                                                                                          cvtEXPR x377
                                                                                                   ) ls378)), 
          ("ty", cvtFUNC_TYPE x382)]))
   and cvtDEFN (ClassDefn x400) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x400))
     | cvtDEFN (VariableDefn x403) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x403))
     | cvtDEFN (FunctionDefn x406) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x406))
     | cvtDEFN (ConstructorDefn x409) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x409))
     | cvtDEFN (InterfaceDefn x412) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x412))
     | cvtDEFN (NamespaceDefn x415) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x415))
     | cvtDEFN (TypeDefn x418) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x418))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls422, params=x426, paramTypes=ls428, 
          defaults=ls433, ctorInits=opt444, returnType=x448, thisType=opt450, 
          hasRest=b454}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x421 => cvtIDENT x421
                                   ) ls422)), ("params", cvtBINDINGS x426), 
          ("paramTypes", PrettyRep.List (List.map (fn x427 => cvtTYPE_EXPR x427
                                                  ) ls428)), ("defaults", PrettyRep.List (List.map (fn x432 => 
                                                                                                          cvtEXPR x432
                                                                                                   ) ls433)), 
          ("ctorInits", 
       (case opt444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x437, ls439) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x437, 
            PrettyRep.List (List.map (fn x438 => cvtEXPR x438
                                     ) ls439)]))
       )), ("returnType", cvtTYPE_EXPR x448), ("thisType", 
       (case opt450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x449 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x449))
       )), ("hasRest", PrettyRep.Bool b454)]))
   and cvtBINDING (Binding{ident=x474, ty=x475}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x474), ("ty", cvtTYPE_EXPR x475)]))
   and cvtBINDING_IDENT (TempIdent n483) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n483))
     | cvtBINDING_IDENT (ParamIdent n486) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n486))
     | cvtBINDING_IDENT (PropIdent x489) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x489))
   and cvtINIT_STEP (InitStep(x492, x493)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x492, 
          cvtEXPR x493]))
     | cvtINIT_STEP (AssignStep(x497, x498)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x497, cvtEXPR x498]))
   and cvtTYPE_EXPR (SpecialType x502) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x502))
     | cvtTYPE_EXPR (UnionType ls506) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x505 => 
                                                                                                           cvtTYPE_EXPR x505
                                                                                                    ) ls506)))
     | cvtTYPE_EXPR (ArrayType ls513) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x512 => 
                                                                                                           cvtTYPE_EXPR x512
                                                                                                    ) ls513)))
     | cvtTYPE_EXPR (TypeName x519) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x519))
     | cvtTYPE_EXPR (ElementTypeRef(x522, n523)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x522, PrettyRep.Int n523]))
     | cvtTYPE_EXPR (FieldTypeRef(x527, x528)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x527, cvtIDENT x528]))
     | cvtTYPE_EXPR (FunctionType x532) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x532))
     | cvtTYPE_EXPR (ObjectType ls536) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x535 => 
                                                                                                             cvtFIELD_TYPE x535
                                                                                                      ) ls536)))
     | cvtTYPE_EXPR (AppType{base=x542, args=ls544}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x542), ("args", PrettyRep.List (List.map (fn x543 => 
                                                                                                     cvtTYPE_EXPR x543
                                                                                              ) ls544))]))
     | cvtTYPE_EXPR (NullableType{expr=x555, nullable=b556}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x555), ("nullable", PrettyRep.Bool b556)]))
     | cvtTYPE_EXPR (InstanceType{name=x564, typeParams=ls566, ty=x570, isDynamic=b571}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x564), 
          ("typeParams", PrettyRep.List (List.map (fn x565 => cvtIDENT x565
                                                  ) ls566)), ("ty", cvtTYPE_EXPR x570), 
          ("isDynamic", PrettyRep.Bool b571)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x584) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x584))
     | cvtSTMT (InitStmt{kind=x587, ns=opt589, prototype=b593, static=b594, 
          temps=x595, inits=ls597}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x587), ("ns", 
       (case opt589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x588 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x588))
       )), ("prototype", PrettyRep.Bool b593), ("static", PrettyRep.Bool b594), 
          ("temps", cvtBINDINGS x595), ("inits", PrettyRep.List (List.map (fn x596 => 
                                                                                 cvtINIT_STEP x596
                                                                          ) ls597))]))
     | cvtSTMT (ClassBlock{ns=opt617, ident=x621, name=opt623, block=x627}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x616 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x616))
       )), ("ident", cvtIDENT x621), ("name", 
       (case opt623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x622 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x622))
       )), ("block", cvtBLOCK x627)]))
     | cvtSTMT (ForEachStmt x639) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x639))
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
     | cvtSTMT (TryStmt{block=x709, catches=ls727, finally=opt732}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x709), ("catches", PrettyRep.List (List.map (fn {bindings=x710, 
                                                                                                     ty=x711, 
                                                                                                     fixtures=opt713, 
                                                                                                     block=x717} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x710), 
                                                                                                     ("ty", 
                                                                                                     cvtTYPE_EXPR x711), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt713 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x712 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x712))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x717)]
                                                                                              ) ls727)), 
          ("finally", 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x731))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt746, cond=x750, cases=ls752}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x745 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x745))
       )), ("cond", cvtEXPR x750), ("cases", PrettyRep.List (List.map (fn x751 => 
                                                                             cvtCASE x751
                                                                      ) ls752))]))
     | cvtSTMT (SwitchTypeStmt{cond=x765, ty=x766, cases=ls768}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x765), ("ty", cvtTYPE_EXPR x766), 
          ("cases", PrettyRep.List (List.map (fn x767 => cvtTYPE_CASE x767
                                             ) ls768))]))
     | cvtSTMT (Dxns{expr=x781}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x781)]))
   and cvtEXPR (TrinaryExpr(x787, x788, x789, x790)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x787, cvtEXPR x788, cvtEXPR x789, 
          cvtEXPR x790]))
     | cvtEXPR (BinaryExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x794, cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (BinaryTypeExpr(x800, x801, x802)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x800, cvtEXPR x801, cvtTYPE_EXPR x802]))
     | cvtEXPR (UnaryExpr(x806, x807)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x806, 
          cvtEXPR x807]))
     | cvtEXPR (TypeExpr x811) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x811))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt816) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt816 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x815 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x815))
       ))
     | cvtEXPR (SuperExpr opt823) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt823 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x822 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x822))
       ))
     | cvtEXPR (LiteralExpr x829) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x829))
     | cvtEXPR (CallExpr{func=x832, actuals=ls834}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x832), ("actuals", PrettyRep.List (List.map (fn x833 => 
                                                                                                   cvtEXPR x833
                                                                                            ) ls834))]))
     | cvtEXPR (ApplyTypeExpr{expr=x845, actuals=ls847}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x845), ("actuals", PrettyRep.List (List.map (fn x846 => 
                                                                                                   cvtTYPE_EXPR x846
                                                                                            ) ls847))]))
     | cvtEXPR (LetExpr{defs=x858, body=x859, head=opt861}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x858), ("body", cvtEXPR x859), 
          ("head", 
       (case opt861 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x860 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x860))
       ))]))
     | cvtEXPR (NewExpr{obj=x874, actuals=ls876}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x874), ("actuals", PrettyRep.List (List.map (fn x875 => 
                                                                                                  cvtEXPR x875
                                                                                           ) ls876))]))
     | cvtEXPR (ObjectRef{base=x887, ident=x888, pos=opt890}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x887), ("ident", cvtIDENT_EXPR x888), 
          ("pos", 
       (case opt890 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x889 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x889))
       ))]))
     | cvtEXPR (LexicalRef{ident=x903, pos=opt905}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x903), ("pos", 
       (case opt905 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x904 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x904))
       ))]))
     | cvtEXPR (SetExpr(x916, x917, x918)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x916, 
          cvtEXPR x917, cvtEXPR x918]))
     | cvtEXPR (ListExpr ls923) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x922 => 
                                                                                                    cvtEXPR x922
                                                                                             ) ls923)))
     | cvtEXPR (InitExpr(x929, x930, x931)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x929, 
          cvtHEAD x930, cvtINITS x931]))
     | cvtEXPR (SliceExpr(x935, x936, x937)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x935, cvtEXPR x936, cvtEXPR x937]))
     | cvtEXPR (GetTemp n941) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n941))
     | cvtEXPR (GetParam n944) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n944))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n950) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n950))
     | cvtFIXTURE_NAME (PropName x953) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x953))
   and cvtIDENT_EXPR (Identifier{ident=x956, openNamespaces=ls962}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x956), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls958 => PrettyRep.List (List.map (fn x957 => 
                                                                                cvtNAMESPACE x957
                                                                         ) ls958)
                                   ) ls962))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x973, expr=x974}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x973), ("expr", cvtEXPR x974)]))
     | cvtIDENT_EXPR (AttributeIdentifier x982) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x982))
     | cvtIDENT_EXPR (ExpressionIdentifier x985) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x985))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x988, ident=x989}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x988), ("ident", cvtUSTRING x989)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x997, typeArgs=ls999}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x997), ("typeArgs", 
          PrettyRep.List (List.map (fn x998 => cvtTYPE_EXPR x998
                                   ) ls999))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1011, x1015)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1010 => cvtIDENT x1010
                                                          ) ls1011), cvtIDENT_EXPR x1015]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1021) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1021))
     | cvtLITERAL (LiteralContextualDecimalInteger s1024) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1024))
     | cvtLITERAL (LiteralContextualHexInteger s1027) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1027))
     | cvtLITERAL (LiteralDouble r1030) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1030))
     | cvtLITERAL (LiteralDecimal d1033) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1033))
     | cvtLITERAL (LiteralInt i1036) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1036))
     | cvtLITERAL (LiteralUInt u1039) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1039))
     | cvtLITERAL (LiteralBoolean b1042) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1042))
     | cvtLITERAL (LiteralString x1045) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1045))
     | cvtLITERAL (LiteralArray{exprs=ls1049, ty=opt1054}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1048 => 
                                                                         cvtEXPR x1048
                                                                  ) ls1049)), 
          ("ty", 
       (case opt1054 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1053 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1053))
       ))]))
     | cvtLITERAL (LiteralXML ls1066) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1065 => 
                                                                                                            cvtEXPR x1065
                                                                                                     ) ls1066)))
     | cvtLITERAL (LiteralNamespace x1072) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1072))
     | cvtLITERAL (LiteralObject{expr=ls1076, ty=opt1081}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1075 => 
                                                                        cvtFIELD x1075
                                                                 ) ls1076)), 
          ("ty", 
       (case opt1081 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1080 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1080))
       ))]))
     | cvtLITERAL (LiteralFunction x1092) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1092))
     | cvtLITERAL (LiteralRegExp{str=x1095}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1095)]))
   and cvtBLOCK (Block x1101) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1101))
   and cvtFIXTURE (NamespaceFixture x1104) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1104))
     | cvtFIXTURE (ClassFixture x1107) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1107))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1111) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1111))
     | cvtFIXTURE (MethodFixture{func=x1114, ty=x1115, readOnly=b1116, override=b1117, 
          final=b1118}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1114), ("ty", cvtTYPE_EXPR x1115), ("readOnly", PrettyRep.Bool b1116), 
          ("override", PrettyRep.Bool b1117), ("final", PrettyRep.Bool b1118)]))
     | cvtFIXTURE (ValFixture{ty=x1132, readOnly=b1133}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1132), ("readOnly", PrettyRep.Bool b1133)]))
     | cvtFIXTURE (VirtualValFixture x1141) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1141))
   and cvtBINDINGS (ls1145, ls1150) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1144 => 
                                                                                       cvtBINDING x1144
                                                                                ) ls1145), 
          PrettyRep.List (List.map (fn x1149 => cvtINIT_STEP x1149
                                   ) ls1150)]
   and cvtFIXTURES ls1158 = PrettyRep.List (List.map (fn (x1155, x1156) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1155, 
                                                            cvtFIXTURE x1156]
                                                     ) ls1158)
   and cvtINITS ls1165 = PrettyRep.List (List.map (fn (x1162, x1163) => PrettyRep.Tuple [cvtFIXTURE_NAME x1162, 
                                                         cvtEXPR x1163]
                                                  ) ls1165)
   and cvtHEAD (x1169, x1170) = PrettyRep.Tuple [cvtFIXTURES x1169, cvtINITS x1170]
   and cvtFIELD {kind=x1172, name=x1173, init=x1174} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1172), ("name", cvtIDENT_EXPR x1173), ("init", cvtEXPR x1174)]
   and cvtFIELD_TYPE {name=x1182, ty=x1183} = PrettyRep.Rec [("name", cvtIDENT x1182), 
          ("ty", cvtTYPE_EXPR x1183)]
   and cvtTYPED_IDENT {name=x1189, ty=opt1191} = PrettyRep.Rec [("name", cvtIDENT x1189), 
          ("ty", 
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1190))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1201, params=ls1206, result=x1210, thisType=opt1212, 
          hasRest=b1216, minArgs=n1217} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1200 => 
                                                                                                        cvtIDENT x1200
                                                                                                 ) ls1201)), 
          ("params", PrettyRep.List (List.map (fn x1205 => cvtTYPE_EXPR x1205
                                              ) ls1206)), ("result", cvtTYPE_EXPR x1210), 
          ("thisType", 
       (case opt1212 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1211 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1211))
       )), ("hasRest", PrettyRep.Bool b1216), ("minArgs", PrettyRep.Int n1217)]
   and cvtFUNC_DEFN {kind=x1231, ns=opt1233, final=b1237, override=b1238, prototype=b1239, 
          static=b1240, func=x1241} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1231), 
          ("ns", 
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1232))
       )), ("final", PrettyRep.Bool b1237), ("override", PrettyRep.Bool b1238), 
          ("prototype", PrettyRep.Bool b1239), ("static", PrettyRep.Bool b1240), 
          ("func", cvtFUNC x1241)]
   and cvtCTOR_DEFN x1257 = cvtCTOR x1257
   and cvtVAR_DEFN {kind=x1258, ns=opt1260, static=b1264, prototype=b1265, 
          bindings=x1266} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1258), 
          ("ns", 
       (case opt1260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1259 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1259))
       )), ("static", PrettyRep.Bool b1264), ("prototype", PrettyRep.Bool b1265), 
          ("bindings", cvtBINDINGS x1266)]
   and cvtNAMESPACE_DEFN {ident=x1278, ns=opt1280, init=opt1285} = PrettyRep.Rec [("ident", 
          cvtIDENT x1278), ("ns", 
       (case opt1280 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1279 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1279))
       )), ("init", 
       (case opt1285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1284 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1284))
       ))]
   and cvtCLASS_DEFN {ident=x1296, ns=opt1298, nonnullable=b1302, dynamic=b1303, 
          final=b1304, params=ls1306, extends=opt1311, implements=ls1316, classDefns=ls1321, 
          instanceDefns=ls1326, instanceStmts=ls1331, ctorDefn=opt1336} = PrettyRep.Rec [("ident", 
          cvtIDENT x1296), ("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1297))
       )), ("nonnullable", PrettyRep.Bool b1302), ("dynamic", PrettyRep.Bool b1303), 
          ("final", PrettyRep.Bool b1304), ("params", PrettyRep.List (List.map (fn x1305 => 
                                                                                      cvtIDENT x1305
                                                                               ) ls1306)), 
          ("extends", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1310))
       )), ("implements", PrettyRep.List (List.map (fn x1315 => cvtIDENT_EXPR x1315
                                                   ) ls1316)), ("classDefns", 
          PrettyRep.List (List.map (fn x1320 => cvtDEFN x1320
                                   ) ls1321)), ("instanceDefns", PrettyRep.List (List.map (fn x1325 => 
                                                                                                 cvtDEFN x1325
                                                                                          ) ls1326)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1330 => cvtSTMT x1330
                                                     ) ls1331)), ("ctorDefn", 
          
       (case opt1336 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1335 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1335))
       ))]
   and cvtINTERFACE_DEFN {ident=x1365, ns=opt1367, nonnullable=b1371, params=ls1373, 
          extends=ls1378, block=x1382} = PrettyRep.Rec [("ident", cvtIDENT x1365), 
          ("ns", 
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1366))
       )), ("nonnullable", PrettyRep.Bool b1371), ("params", PrettyRep.List (List.map (fn x1372 => 
                                                                                             cvtIDENT x1372
                                                                                      ) ls1373)), 
          ("extends", PrettyRep.List (List.map (fn x1377 => cvtIDENT_EXPR x1377
                                               ) ls1378)), ("block", cvtBLOCK x1382)]
   and cvtTYPE_DEFN {ident=x1396, ns=opt1398, init=x1402} = PrettyRep.Rec [("ident", 
          cvtIDENT x1396), ("ns", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1397))
       )), ("init", cvtTYPE_EXPR x1402)]
   and cvtFOR_ENUM_STMT {defn=opt1411, obj=x1415, fixtures=opt1417, inits=opt1422, 
          labels=ls1427, body=x1431} = PrettyRep.Rec [("defn", 
       (case opt1411 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1410 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1410))
       )), ("obj", cvtEXPR x1415), ("fixtures", 
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1416 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1416))
       )), ("inits", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1421))
       )), ("labels", PrettyRep.List (List.map (fn x1426 => cvtIDENT x1426
                                               ) ls1427)), ("body", cvtSTMT x1431)]
   and cvtFOR_STMT {fixtures=opt1446, defn=opt1451, init=x1455, cond=x1456, 
          update=x1457, labels=ls1459, body=x1463} = PrettyRep.Rec [("fixtures", 
          
       (case opt1446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1445 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1445))
       )), ("defn", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1450))
       )), ("init", cvtSTMT x1455), ("cond", cvtEXPR x1456), ("update", cvtEXPR x1457), 
          ("labels", PrettyRep.List (List.map (fn x1458 => cvtIDENT x1458
                                              ) ls1459)), ("body", cvtSTMT x1463)]
   and cvtWHILE_STMT {cond=x1479, fixtures=opt1481, body=x1485, labels=ls1487} = 
          PrettyRep.Rec [("cond", cvtEXPR x1479), ("fixtures", 
       (case opt1481 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1480 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1480))
       )), ("body", cvtSTMT x1485), ("labels", PrettyRep.List (List.map (fn x1486 => 
                                                                               cvtIDENT x1486
                                                                        ) ls1487))]
   and cvtDIRECTIVES {pragmas=ls1501, defns=ls1506, head=opt1511, body=ls1516, 
          pos=opt1521} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1500 => 
                                                                                    cvtPRAGMA x1500
                                                                             ) ls1501)), 
          ("defns", PrettyRep.List (List.map (fn x1505 => cvtDEFN x1505
                                             ) ls1506)), ("head", 
       (case opt1511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1510 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1510))
       )), ("body", PrettyRep.List (List.map (fn x1515 => cvtSTMT x1515
                                             ) ls1516)), ("pos", 
       (case opt1521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1520 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1520))
       ))]
   and cvtCASE {label=opt1537, inits=opt1542, body=x1546} = PrettyRep.Rec [("label", 
          
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1536))
       )), ("inits", 
       (case opt1542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1541 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1541))
       )), ("body", cvtBLOCK x1546)]
   and cvtTYPE_CASE {ty=opt1555, bindings=x1559, inits=opt1561, body=x1565} = 
          PrettyRep.Rec [("ty", 
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1554))
       )), ("bindings", cvtBINDINGS x1559), ("inits", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1560))
       )), ("body", cvtBLOCK x1565)]
   and cvtFUNC_NAME {kind=x1575, ident=x1576} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1575), 
          ("ident", cvtIDENT x1576)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1582, getter=opt1584, setter=opt1589} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1582), ("getter", 
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1583 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1583))
       )), ("setter", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1588))
       ))]
   and cvtPACKAGE {name=ls1601, block=x1605} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1600 => 
                                                                                                       cvtIDENT x1600
                                                                                                ) ls1601)), 
          ("block", cvtBLOCK x1605)]
   and cvtPROGRAM {packages=ls1612, fixtures=opt1617, block=x1621} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1611 => cvtPACKAGE x1611
                                   ) ls1612)), ("fixtures", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1616 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1616))
       )), ("block", cvtBLOCK x1621)]
end

