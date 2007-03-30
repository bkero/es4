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
     | cvtSTMT (ForInStmt x639) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x639))
     | cvtSTMT (ThrowStmt x642) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x642))
     | cvtSTMT (ReturnStmt x645) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x645))
     | cvtSTMT (BreakStmt opt649) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt649 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x648 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x648))
       ))
     | cvtSTMT (ContinueStmt opt656) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x655 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x655))
       ))
     | cvtSTMT (BlockStmt x662) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x662))
     | cvtSTMT (LabeledStmt(x665, x666)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x665, 
          cvtSTMT x666]))
     | cvtSTMT (LetStmt x670) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x670))
     | cvtSTMT (WhileStmt x673) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x673))
     | cvtSTMT (DoWhileStmt x676) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x676))
     | cvtSTMT (ForStmt x679) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x679))
     | cvtSTMT (IfStmt{cnd=x682, thn=x683, els=x684}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x682), ("thn", cvtSTMT x683), 
          ("els", cvtSTMT x684)]))
     | cvtSTMT (WithStmt{obj=x694, ty=x695, body=x696}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x694), ("ty", cvtTYPE_EXPR x695), 
          ("body", cvtSTMT x696)]))
     | cvtSTMT (TryStmt{block=x706, catches=ls708, finally=opt713}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x706), ("catches", PrettyRep.List (List.map (fn x707 => 
                                                                                                     cvtCATCH_CLAUSE x707
                                                                                              ) ls708)), 
          ("finally", 
       (case opt713 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x712 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x712))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt727, cond=x731, cases=ls733}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt727 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x726 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x726))
       )), ("cond", cvtEXPR x731), ("cases", PrettyRep.List (List.map (fn x732 => 
                                                                             cvtCASE x732
                                                                      ) ls733))]))
     | cvtSTMT (SwitchTypeStmt{cond=x746, ty=x747, cases=ls749}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x746), ("ty", cvtTYPE_EXPR x747), 
          ("cases", PrettyRep.List (List.map (fn x748 => cvtTYPE_CASE x748
                                             ) ls749))]))
     | cvtSTMT (Dxns{expr=x762}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x762)]))
   and cvtEXPR (TrinaryExpr(x768, x769, x770, x771)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x768, cvtEXPR x769, cvtEXPR x770, 
          cvtEXPR x771]))
     | cvtEXPR (BinaryExpr(x775, x776, x777)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x775, cvtEXPR x776, cvtEXPR x777]))
     | cvtEXPR (BinaryTypeExpr(x781, x782, x783)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x781, cvtEXPR x782, cvtTYPE_EXPR x783]))
     | cvtEXPR (UnaryExpr(x787, x788)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x787, 
          cvtEXPR x788]))
     | cvtEXPR (TypeExpr x792) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x792))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt797) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x796 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x796))
       ))
     | cvtEXPR (SuperExpr opt804) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt804 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x803 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x803))
       ))
     | cvtEXPR (LiteralExpr x810) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x810))
     | cvtEXPR (CallExpr{func=x813, actuals=ls815}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x813), ("actuals", PrettyRep.List (List.map (fn x814 => 
                                                                                                   cvtEXPR x814
                                                                                            ) ls815))]))
     | cvtEXPR (ApplyTypeExpr{expr=x826, actuals=ls828}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x826), ("actuals", PrettyRep.List (List.map (fn x827 => 
                                                                                                   cvtTYPE_EXPR x827
                                                                                            ) ls828))]))
     | cvtEXPR (LetExpr{defs=x839, body=x840, head=opt842}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x839), ("body", cvtEXPR x840), 
          ("head", 
       (case opt842 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x841 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x841))
       ))]))
     | cvtEXPR (NewExpr{obj=x855, actuals=ls857}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x855), ("actuals", PrettyRep.List (List.map (fn x856 => 
                                                                                                  cvtEXPR x856
                                                                                           ) ls857))]))
     | cvtEXPR (ObjectRef{base=x868, ident=x869, pos=opt871}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x868), ("ident", cvtIDENT_EXPR x869), 
          ("pos", 
       (case opt871 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x870 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x870))
       ))]))
     | cvtEXPR (LexicalRef{ident=x884, pos=opt886}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x884), ("pos", 
       (case opt886 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x885 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x885))
       ))]))
     | cvtEXPR (SetExpr(x897, x898, x899)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x897, 
          cvtEXPR x898, cvtEXPR x899]))
     | cvtEXPR (ListExpr ls904) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x903 => 
                                                                                                    cvtEXPR x903
                                                                                             ) ls904)))
     | cvtEXPR (InitExpr(x910, x911, x912)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x910, 
          cvtHEAD x911, cvtINITS x912]))
     | cvtEXPR (SliceExpr(x916, x917, x918)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x916, cvtEXPR x917, cvtEXPR x918]))
     | cvtEXPR (GetTemp n922) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n922))
     | cvtEXPR (GetParam n925) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n925))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n931) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n931))
     | cvtFIXTURE_NAME (PropName x934) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x934))
   and cvtIDENT_EXPR (Identifier{ident=x937, openNamespaces=ls943}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x937), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls939 => PrettyRep.List (List.map (fn x938 => 
                                                                                cvtNAMESPACE x938
                                                                         ) ls939)
                                   ) ls943))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x954, expr=x955}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x954), ("expr", cvtEXPR x955)]))
     | cvtIDENT_EXPR (AttributeIdentifier x963) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x963))
     | cvtIDENT_EXPR (ExpressionIdentifier x966) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x966))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x969, ident=x970}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x969), ("ident", cvtUSTRING x970)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x978, typeArgs=ls980}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x978), ("typeArgs", 
          PrettyRep.List (List.map (fn x979 => cvtTYPE_EXPR x979
                                   ) ls980))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls992, x996)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x991 => cvtIDENT x991
                                                          ) ls992), cvtIDENT_EXPR x996]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1002) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1002))
     | cvtLITERAL (LiteralContextualDecimalInteger s1005) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1005))
     | cvtLITERAL (LiteralContextualHexInteger s1008) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1008))
     | cvtLITERAL (LiteralDouble r1011) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1011))
     | cvtLITERAL (LiteralDecimal d1014) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1014))
     | cvtLITERAL (LiteralInt i1017) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1017))
     | cvtLITERAL (LiteralUInt u1020) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1020))
     | cvtLITERAL (LiteralBoolean b1023) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1023))
     | cvtLITERAL (LiteralString x1026) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1026))
     | cvtLITERAL (LiteralArray{exprs=ls1030, ty=opt1035}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1029 => 
                                                                         cvtEXPR x1029
                                                                  ) ls1030)), 
          ("ty", 
       (case opt1035 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1034 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1034))
       ))]))
     | cvtLITERAL (LiteralXML ls1047) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1046 => 
                                                                                                            cvtEXPR x1046
                                                                                                     ) ls1047)))
     | cvtLITERAL (LiteralNamespace x1053) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1053))
     | cvtLITERAL (LiteralObject{expr=ls1057, ty=opt1062}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1056 => 
                                                                        cvtFIELD x1056
                                                                 ) ls1057)), 
          ("ty", 
       (case opt1062 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1061 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1061))
       ))]))
     | cvtLITERAL (LiteralFunction x1073) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1073))
     | cvtLITERAL (LiteralRegExp{str=x1076}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1076)]))
   and cvtBLOCK (Block x1082) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1082))
   and cvtFIXTURE (NamespaceFixture x1085) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1085))
     | cvtFIXTURE (ClassFixture x1088) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1088))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1092) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1092))
     | cvtFIXTURE (MethodFixture{func=x1095, ty=x1096, readOnly=b1097, override=b1098, 
          final=b1099}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1095), ("ty", cvtTYPE_EXPR x1096), ("readOnly", PrettyRep.Bool b1097), 
          ("override", PrettyRep.Bool b1098), ("final", PrettyRep.Bool b1099)]))
     | cvtFIXTURE (ValFixture{ty=x1113, readOnly=b1114}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1113), ("readOnly", PrettyRep.Bool b1114)]))
     | cvtFIXTURE (VirtualValFixture x1122) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1122))
   and cvtBINDINGS (ls1126, ls1131) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1125 => 
                                                                                       cvtBINDING x1125
                                                                                ) ls1126), 
          PrettyRep.List (List.map (fn x1130 => cvtINIT_STEP x1130
                                   ) ls1131)]
   and cvtFIXTURES ls1139 = PrettyRep.List (List.map (fn (x1136, x1137) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1136, 
                                                            cvtFIXTURE x1137]
                                                     ) ls1139)
   and cvtINITS ls1146 = PrettyRep.List (List.map (fn (x1143, x1144) => PrettyRep.Tuple [cvtFIXTURE_NAME x1143, 
                                                         cvtEXPR x1144]
                                                  ) ls1146)
   and cvtHEAD (x1150, x1151) = PrettyRep.Tuple [cvtFIXTURES x1150, cvtINITS x1151]
   and cvtFIELD {kind=x1153, name=x1154, init=x1155} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1153), ("name", cvtIDENT_EXPR x1154), ("init", cvtEXPR x1155)]
   and cvtFIELD_TYPE {name=x1163, ty=x1164} = PrettyRep.Rec [("name", cvtIDENT x1163), 
          ("ty", cvtTYPE_EXPR x1164)]
   and cvtTYPED_IDENT {name=x1170, ty=opt1172} = PrettyRep.Rec [("name", cvtIDENT x1170), 
          ("ty", 
       (case opt1172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1171 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1171))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1182, params=ls1187, result=x1191, thisType=opt1193, 
          hasRest=b1197, minArgs=n1198} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1181 => 
                                                                                                        cvtIDENT x1181
                                                                                                 ) ls1182)), 
          ("params", PrettyRep.List (List.map (fn x1186 => cvtTYPE_EXPR x1186
                                              ) ls1187)), ("result", cvtTYPE_EXPR x1191), 
          ("thisType", 
       (case opt1193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1192 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1192))
       )), ("hasRest", PrettyRep.Bool b1197), ("minArgs", PrettyRep.Int n1198)]
   and cvtFUNC_DEFN {kind=x1212, ns=opt1214, final=b1218, override=b1219, prototype=b1220, 
          static=b1221, func=x1222} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1212), 
          ("ns", 
       (case opt1214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1213 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1213))
       )), ("final", PrettyRep.Bool b1218), ("override", PrettyRep.Bool b1219), 
          ("prototype", PrettyRep.Bool b1220), ("static", PrettyRep.Bool b1221), 
          ("func", cvtFUNC x1222)]
   and cvtCTOR_DEFN x1238 = cvtCTOR x1238
   and cvtVAR_DEFN {kind=x1239, ns=opt1241, static=b1245, prototype=b1246, 
          bindings=x1247} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1239), 
          ("ns", 
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1240))
       )), ("static", PrettyRep.Bool b1245), ("prototype", PrettyRep.Bool b1246), 
          ("bindings", cvtBINDINGS x1247)]
   and cvtNAMESPACE_DEFN {ident=x1259, ns=opt1261, init=opt1266} = PrettyRep.Rec [("ident", 
          cvtIDENT x1259), ("ns", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1260))
       )), ("init", 
       (case opt1266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1265 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1265))
       ))]
   and cvtCLASS_DEFN {ident=x1277, ns=opt1279, nonnullable=b1283, dynamic=b1284, 
          final=b1285, params=ls1287, extends=opt1292, implements=ls1297, classDefns=ls1302, 
          instanceDefns=ls1307, instanceStmts=ls1312, ctorDefn=opt1317} = PrettyRep.Rec [("ident", 
          cvtIDENT x1277), ("ns", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1278))
       )), ("nonnullable", PrettyRep.Bool b1283), ("dynamic", PrettyRep.Bool b1284), 
          ("final", PrettyRep.Bool b1285), ("params", PrettyRep.List (List.map (fn x1286 => 
                                                                                      cvtIDENT x1286
                                                                               ) ls1287)), 
          ("extends", 
       (case opt1292 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1291 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1291))
       )), ("implements", PrettyRep.List (List.map (fn x1296 => cvtIDENT_EXPR x1296
                                                   ) ls1297)), ("classDefns", 
          PrettyRep.List (List.map (fn x1301 => cvtDEFN x1301
                                   ) ls1302)), ("instanceDefns", PrettyRep.List (List.map (fn x1306 => 
                                                                                                 cvtDEFN x1306
                                                                                          ) ls1307)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1311 => cvtSTMT x1311
                                                     ) ls1312)), ("ctorDefn", 
          
       (case opt1317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1316 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1316))
       ))]
   and cvtINTERFACE_DEFN {ident=x1346, ns=opt1348, nonnullable=b1352, params=ls1354, 
          extends=ls1359, block=x1363} = PrettyRep.Rec [("ident", cvtIDENT x1346), 
          ("ns", 
       (case opt1348 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1347 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1347))
       )), ("nonnullable", PrettyRep.Bool b1352), ("params", PrettyRep.List (List.map (fn x1353 => 
                                                                                             cvtIDENT x1353
                                                                                      ) ls1354)), 
          ("extends", PrettyRep.List (List.map (fn x1358 => cvtIDENT_EXPR x1358
                                               ) ls1359)), ("block", cvtBLOCK x1363)]
   and cvtTYPE_DEFN {ident=x1377, ns=opt1379, init=x1383} = PrettyRep.Rec [("ident", 
          cvtIDENT x1377), ("ns", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1378 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1378))
       )), ("init", cvtTYPE_EXPR x1383)]
   and cvtFOR_ENUM_STMT {isEach=b1391, defn=opt1393, obj=x1397, fixtures=opt1399, 
          init=ls1404, labels=ls1409, body=x1413} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1391), ("defn", 
       (case opt1393 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1392 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1392))
       )), ("obj", cvtEXPR x1397), ("fixtures", 
       (case opt1399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1398 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1398))
       )), ("init", PrettyRep.List (List.map (fn x1403 => cvtSTMT x1403
                                             ) ls1404)), ("labels", PrettyRep.List (List.map (fn x1408 => 
                                                                                                    cvtIDENT x1408
                                                                                             ) ls1409)), 
          ("body", cvtSTMT x1413)]
   and cvtFOR_STMT {fixtures=opt1430, defn=opt1435, init=ls1440, cond=x1444, 
          update=x1445, labels=ls1447, body=x1451} = PrettyRep.Rec [("fixtures", 
          
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1429 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1429))
       )), ("defn", 
       (case opt1435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1434))
       )), ("init", PrettyRep.List (List.map (fn x1439 => cvtSTMT x1439
                                             ) ls1440)), ("cond", cvtEXPR x1444), 
          ("update", cvtEXPR x1445), ("labels", PrettyRep.List (List.map (fn x1446 => 
                                                                                cvtIDENT x1446
                                                                         ) ls1447)), 
          ("body", cvtSTMT x1451)]
   and cvtWHILE_STMT {cond=x1467, fixtures=opt1469, body=x1473, labels=ls1475} = 
          PrettyRep.Rec [("cond", cvtEXPR x1467), ("fixtures", 
       (case opt1469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1468 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1468))
       )), ("body", cvtSTMT x1473), ("labels", PrettyRep.List (List.map (fn x1474 => 
                                                                               cvtIDENT x1474
                                                                        ) ls1475))]
   and cvtDIRECTIVES {pragmas=ls1489, defns=ls1494, head=opt1499, body=ls1504, 
          pos=opt1509} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1488 => 
                                                                                    cvtPRAGMA x1488
                                                                             ) ls1489)), 
          ("defns", PrettyRep.List (List.map (fn x1493 => cvtDEFN x1493
                                             ) ls1494)), ("head", 
       (case opt1499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1498 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1498))
       )), ("body", PrettyRep.List (List.map (fn x1503 => cvtSTMT x1503
                                             ) ls1504)), ("pos", 
       (case opt1509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1508 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1508))
       ))]
   and cvtCASE {label=opt1525, inits=opt1530, body=x1534} = PrettyRep.Rec [("label", 
          
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1524 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1524))
       )), ("inits", 
       (case opt1530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1529 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1529))
       )), ("body", cvtBLOCK x1534)]
   and cvtTYPE_CASE {ty=opt1543, bindings=x1547, inits=opt1549, body=x1553} = 
          PrettyRep.Rec [("ty", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1542))
       )), ("bindings", cvtBINDINGS x1547), ("inits", 
       (case opt1549 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1548 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1548))
       )), ("body", cvtBLOCK x1553)]
   and cvtCATCH_CLAUSE {bindings=x1563, ty=x1564, fixtures=opt1566, block=x1570} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1563), ("ty", cvtTYPE_EXPR x1564), 
          ("fixtures", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1565))
       )), ("block", cvtBLOCK x1570)]
   and cvtFUNC_NAME {kind=x1580, ident=x1581} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1580), 
          ("ident", cvtIDENT x1581)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1587, getter=opt1589, setter=opt1594} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1587), ("getter", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1588))
       )), ("setter", 
       (case opt1594 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1593 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1593))
       ))]
   and cvtPACKAGE {name=ls1606, block=x1610} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1605 => 
                                                                                                       cvtIDENT x1605
                                                                                                ) ls1606)), 
          ("block", cvtBLOCK x1610)]
   and cvtPROGRAM {packages=ls1617, fixtures=opt1622, block=x1626} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1616 => cvtPACKAGE x1616
                                   ) ls1617)), ("fixtures", 
       (case opt1622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1621 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1621))
       )), ("block", cvtBLOCK x1626)]
end

