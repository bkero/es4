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
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1095) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1095))
     | cvtFIXTURE (MethodFixture{func=x1098, ty=x1099, readOnly=b1100, override=b1101, 
          final=b1102}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1098), ("ty", cvtTYPE_EXPR x1099), ("readOnly", PrettyRep.Bool b1100), 
          ("override", PrettyRep.Bool b1101), ("final", PrettyRep.Bool b1102)]))
     | cvtFIXTURE (ValFixture{ty=x1116, readOnly=b1117}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1116), ("readOnly", PrettyRep.Bool b1117)]))
     | cvtFIXTURE (VirtualValFixture x1125) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1125))
   and cvtBINDINGS (ls1129, ls1134) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1128 => 
                                                                                       cvtBINDING x1128
                                                                                ) ls1129), 
          PrettyRep.List (List.map (fn x1133 => cvtINIT_STEP x1133
                                   ) ls1134)]
   and cvtFIXTURES ls1142 = PrettyRep.List (List.map (fn (x1139, x1140) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1139, 
                                                            cvtFIXTURE x1140]
                                                     ) ls1142)
   and cvtINITS ls1149 = PrettyRep.List (List.map (fn (x1146, x1147) => PrettyRep.Tuple [cvtFIXTURE_NAME x1146, 
                                                         cvtEXPR x1147]
                                                  ) ls1149)
   and cvtHEAD (x1153, x1154) = PrettyRep.Tuple [cvtFIXTURES x1153, cvtINITS x1154]
   and cvtFIELD {kind=x1156, name=x1157, init=x1158} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1156), ("name", cvtIDENT_EXPR x1157), ("init", cvtEXPR x1158)]
   and cvtFIELD_TYPE {name=x1166, ty=x1167} = PrettyRep.Rec [("name", cvtIDENT x1166), 
          ("ty", cvtTYPE_EXPR x1167)]
   and cvtTYPED_IDENT {name=x1173, ty=opt1175} = PrettyRep.Rec [("name", cvtIDENT x1173), 
          ("ty", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1174))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1185, params=ls1190, result=x1194, thisType=opt1196, 
          hasRest=b1200, minArgs=n1201} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1184 => 
                                                                                                        cvtIDENT x1184
                                                                                                 ) ls1185)), 
          ("params", PrettyRep.List (List.map (fn x1189 => cvtTYPE_EXPR x1189
                                              ) ls1190)), ("result", cvtTYPE_EXPR x1194), 
          ("thisType", 
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1195))
       )), ("hasRest", PrettyRep.Bool b1200), ("minArgs", PrettyRep.Int n1201)]
   and cvtFUNC_DEFN {kind=x1215, ns=opt1217, final=b1221, override=b1222, prototype=b1223, 
          static=b1224, func=x1225} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1215), 
          ("ns", 
       (case opt1217 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1216 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1216))
       )), ("final", PrettyRep.Bool b1221), ("override", PrettyRep.Bool b1222), 
          ("prototype", PrettyRep.Bool b1223), ("static", PrettyRep.Bool b1224), 
          ("func", cvtFUNC x1225)]
   and cvtCTOR_DEFN x1241 = cvtCTOR x1241
   and cvtVAR_DEFN {kind=x1242, ns=opt1244, static=b1248, prototype=b1249, 
          bindings=x1250} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1242), 
          ("ns", 
       (case opt1244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1243 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1243))
       )), ("static", PrettyRep.Bool b1248), ("prototype", PrettyRep.Bool b1249), 
          ("bindings", cvtBINDINGS x1250)]
   and cvtNAMESPACE_DEFN {ident=x1262, ns=opt1264, init=opt1269} = PrettyRep.Rec [("ident", 
          cvtIDENT x1262), ("ns", 
       (case opt1264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1263 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1263))
       )), ("init", 
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1268))
       ))]
   and cvtCLASS_DEFN {ident=x1280, ns=opt1282, nonnullable=b1286, dynamic=b1287, 
          final=b1288, params=ls1290, extends=opt1295, implements=ls1300, classDefns=ls1305, 
          instanceDefns=ls1310, instanceStmts=ls1315, ctorDefn=opt1320} = PrettyRep.Rec [("ident", 
          cvtIDENT x1280), ("ns", 
       (case opt1282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1281 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1281))
       )), ("nonnullable", PrettyRep.Bool b1286), ("dynamic", PrettyRep.Bool b1287), 
          ("final", PrettyRep.Bool b1288), ("params", PrettyRep.List (List.map (fn x1289 => 
                                                                                      cvtIDENT x1289
                                                                               ) ls1290)), 
          ("extends", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1294))
       )), ("implements", PrettyRep.List (List.map (fn x1299 => cvtIDENT_EXPR x1299
                                                   ) ls1300)), ("classDefns", 
          PrettyRep.List (List.map (fn x1304 => cvtDEFN x1304
                                   ) ls1305)), ("instanceDefns", PrettyRep.List (List.map (fn x1309 => 
                                                                                                 cvtDEFN x1309
                                                                                          ) ls1310)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1314 => cvtSTMT x1314
                                                     ) ls1315)), ("ctorDefn", 
          
       (case opt1320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1319 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1319))
       ))]
   and cvtINTERFACE_DEFN {ident=x1349, ns=opt1351, nonnullable=b1355, params=ls1357, 
          extends=ls1362, block=x1366} = PrettyRep.Rec [("ident", cvtIDENT x1349), 
          ("ns", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1350))
       )), ("nonnullable", PrettyRep.Bool b1355), ("params", PrettyRep.List (List.map (fn x1356 => 
                                                                                             cvtIDENT x1356
                                                                                      ) ls1357)), 
          ("extends", PrettyRep.List (List.map (fn x1361 => cvtIDENT_EXPR x1361
                                               ) ls1362)), ("block", cvtBLOCK x1366)]
   and cvtTYPE_DEFN {ident=x1380, ns=opt1382, init=x1386} = PrettyRep.Rec [("ident", 
          cvtIDENT x1380), ("ns", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1381))
       )), ("init", cvtTYPE_EXPR x1386)]
   and cvtFOR_ENUM_STMT {defn=opt1395, obj=x1399, fixtures=opt1401, inits=opt1406, 
          labels=ls1411, body=x1415} = PrettyRep.Rec [("defn", 
       (case opt1395 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1394 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1394))
       )), ("obj", cvtEXPR x1399), ("fixtures", 
       (case opt1401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1400 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1400))
       )), ("inits", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1405))
       )), ("labels", PrettyRep.List (List.map (fn x1410 => cvtIDENT x1410
                                               ) ls1411)), ("body", cvtSTMT x1415)]
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

