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
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt80) = PrettyRep.Ctor ("Plus", SOME 
       (case opt80 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x79 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x79))
       ))
     | cvtBINOP (Minus opt87) = PrettyRep.Ctor ("Minus", SOME 
       (case opt87 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
       ))
     | cvtBINOP (Times opt94) = PrettyRep.Ctor ("Times", SOME 
       (case opt94 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
       ))
     | cvtBINOP (Divide opt101) = PrettyRep.Ctor ("Divide", SOME 
       (case opt101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x100 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x100))
       ))
     | cvtBINOP (Remainder opt108) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt108 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x107 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x107))
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
     | cvtBINOP (Equals opt125) = PrettyRep.Ctor ("Equals", SOME 
       (case opt125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x124 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x124))
       ))
     | cvtBINOP (NotEquals opt132) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
       ))
     | cvtBINOP (StrictEquals opt139) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
       ))
     | cvtBINOP (StrictNotEquals opt146) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
       ))
     | cvtBINOP (Less opt153) = PrettyRep.Ctor ("Less", SOME 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
       ))
     | cvtBINOP (LessOrEqual opt160) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
       ))
     | cvtBINOP (Greater opt167) = PrettyRep.Ctor ("Greater", SOME 
       (case opt167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x166))
       ))
     | cvtBINOP (GreaterOrEqual opt174) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x173 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x173))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt183) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x182))
       ))
     | cvtASSIGNOP (AssignMinus opt190) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
       ))
     | cvtASSIGNOP (AssignTimes opt197) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
       ))
     | cvtASSIGNOP (AssignDivide opt204) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt204 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x203 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x203))
       ))
     | cvtASSIGNOP (AssignRemainder opt211) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x210 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x210))
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
     | cvtUNOP (PreIncrement opt229) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x228 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x228))
       ))
     | cvtUNOP (PreDecrement opt236) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
       ))
     | cvtUNOP (PostIncrement opt243) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
       ))
     | cvtUNOP (PostDecrement opt250) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
       ))
     | cvtUNOP (UnaryPlus opt257) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x256))
       ))
     | cvtUNOP (UnaryMinus opt264) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x263 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x263))
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
   and cvtPRAGMA (UseNamespace x281) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x281))
     | cvtPRAGMA (UseDefaultNamespace x284) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x284))
     | cvtPRAGMA (UseNumber x287) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x287))
     | cvtPRAGMA (UseRounding r290) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r290))
     | cvtPRAGMA (UsePrecision n293) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n293))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls299, name=x303, alias=opt305}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x298 => 
                                                                           cvtIDENT x298
                                                                    ) ls299)), 
          ("name", cvtIDENT x303), ("alias", 
       (case opt305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x304 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x304))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x324, nonnullable=b325, extends=opt327, implements=ls332, 
          classFixtures=x336, instanceFixtures=x337, instanceInits=x338, constructor=opt340, 
          classType=x344, instanceType=x345}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x324), ("nonnullable", PrettyRep.Bool b325), ("extends", 
          
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
   and cvtCTOR (Ctor{settings=x369, superArgs=ls371, func=x375}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x369), ("superArgs", PrettyRep.List (List.map (fn x370 => 
                                                                                                         cvtEXPR x370
                                                                                                  ) ls371)), 
          ("func", cvtFUNC x375)]))
   and cvtFUNC (Func{name=x385, fsig=x386, isNative=b387, block=x388, param=x389, 
          defaults=ls391, ty=x395}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x385), ("fsig", cvtFUNC_SIG x386), ("isNative", PrettyRep.Bool b387), 
          ("block", cvtBLOCK x388), ("param", cvtHEAD x389), ("defaults", PrettyRep.List (List.map (fn x390 => 
                                                                                                          cvtEXPR x390
                                                                                                   ) ls391)), 
          ("ty", cvtFUNC_TYPE x395)]))
   and cvtDEFN (ClassDefn x413) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x413))
     | cvtDEFN (VariableDefn x416) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x416))
     | cvtDEFN (FunctionDefn x419) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x419))
     | cvtDEFN (ConstructorDefn x422) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x422))
     | cvtDEFN (InterfaceDefn x425) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x425))
     | cvtDEFN (NamespaceDefn x428) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x428))
     | cvtDEFN (TypeDefn x431) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x431))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls435, params=x439, paramTypes=ls441, 
          defaults=ls446, ctorInits=opt457, returnType=x461, thisType=opt463, 
          hasRest=b467}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x434 => cvtIDENT x434
                                   ) ls435)), ("params", cvtBINDINGS x439), 
          ("paramTypes", PrettyRep.List (List.map (fn x440 => cvtTYPE_EXPR x440
                                                  ) ls441)), ("defaults", PrettyRep.List (List.map (fn x445 => 
                                                                                                          cvtEXPR x445
                                                                                                   ) ls446)), 
          ("ctorInits", 
       (case opt457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x450, ls452) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x450, 
            PrettyRep.List (List.map (fn x451 => cvtEXPR x451
                                     ) ls452)]))
       )), ("returnType", cvtTYPE_EXPR x461), ("thisType", 
       (case opt463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x462 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x462))
       )), ("hasRest", PrettyRep.Bool b467)]))
   and cvtBINDING (Binding{ident=x487, ty=x488}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x487), ("ty", cvtTYPE_EXPR x488)]))
   and cvtBINDING_IDENT (TempIdent n496) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n496))
     | cvtBINDING_IDENT (ParamIdent n499) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n499))
     | cvtBINDING_IDENT (PropIdent x502) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x502))
   and cvtINIT_STEP (InitStep(x505, x506)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x505, 
          cvtEXPR x506]))
     | cvtINIT_STEP (AssignStep(x510, x511)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x510, cvtEXPR x511]))
   and cvtTYPE_EXPR (SpecialType x515) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x515))
     | cvtTYPE_EXPR (UnionType ls519) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x518 => 
                                                                                                           cvtTYPE_EXPR x518
                                                                                                    ) ls519)))
     | cvtTYPE_EXPR (ArrayType ls526) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x525 => 
                                                                                                           cvtTYPE_EXPR x525
                                                                                                    ) ls526)))
     | cvtTYPE_EXPR (TypeName x532) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x532))
     | cvtTYPE_EXPR (ElementTypeRef(x535, n536)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x535, PrettyRep.Int n536]))
     | cvtTYPE_EXPR (FieldTypeRef(x540, x541)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x540, cvtIDENT x541]))
     | cvtTYPE_EXPR (FunctionType x545) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x545))
     | cvtTYPE_EXPR (ObjectType ls549) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x548 => 
                                                                                                             cvtFIELD_TYPE x548
                                                                                                      ) ls549)))
     | cvtTYPE_EXPR (AppType{base=x555, args=ls557}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x555), ("args", PrettyRep.List (List.map (fn x556 => 
                                                                                                     cvtTYPE_EXPR x556
                                                                                              ) ls557))]))
     | cvtTYPE_EXPR (NullableType{expr=x568, nullable=b569}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x568), ("nullable", PrettyRep.Bool b569)]))
     | cvtTYPE_EXPR (InstanceType{name=x577, typeParams=ls579, ty=x583, isDynamic=b584}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x577), 
          ("typeParams", PrettyRep.List (List.map (fn x578 => cvtIDENT x578
                                                  ) ls579)), ("ty", cvtTYPE_EXPR x583), 
          ("isDynamic", PrettyRep.Bool b584)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x597) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x597))
     | cvtSTMT (InitStmt{kind=x600, ns=opt602, prototype=b606, static=b607, 
          temps=x608, inits=ls610}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x600), ("ns", 
       (case opt602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x601))
       )), ("prototype", PrettyRep.Bool b606), ("static", PrettyRep.Bool b607), 
          ("temps", cvtBINDINGS x608), ("inits", PrettyRep.List (List.map (fn x609 => 
                                                                                 cvtINIT_STEP x609
                                                                          ) ls610))]))
     | cvtSTMT (ClassBlock{ns=opt630, ident=x634, name=opt636, block=x640}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x629 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x629))
       )), ("ident", cvtIDENT x634), ("name", 
       (case opt636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x635 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x635))
       )), ("block", cvtBLOCK x640)]))
     | cvtSTMT (ForInStmt x652) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x652))
     | cvtSTMT (ThrowStmt x655) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x655))
     | cvtSTMT (ReturnStmt x658) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x658))
     | cvtSTMT (BreakStmt opt662) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x661))
       ))
     | cvtSTMT (ContinueStmt opt669) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x668))
       ))
     | cvtSTMT (BlockStmt x675) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x675))
     | cvtSTMT (LabeledStmt(x678, x679)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x678, 
          cvtSTMT x679]))
     | cvtSTMT (LetStmt x683) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x683))
     | cvtSTMT (WhileStmt x686) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x686))
     | cvtSTMT (DoWhileStmt x689) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x689))
     | cvtSTMT (ForStmt x692) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x692))
     | cvtSTMT (IfStmt{cnd=x695, thn=x696, els=x697}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x695), ("thn", cvtSTMT x696), 
          ("els", cvtSTMT x697)]))
     | cvtSTMT (WithStmt{obj=x707, ty=x708, body=x709}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x707), ("ty", cvtTYPE_EXPR x708), 
          ("body", cvtSTMT x709)]))
     | cvtSTMT (TryStmt{block=x719, catches=ls721, finally=opt726}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x719), ("catches", PrettyRep.List (List.map (fn x720 => 
                                                                                                     cvtCATCH_CLAUSE x720
                                                                                              ) ls721)), 
          ("finally", 
       (case opt726 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x725 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x725))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt740, cond=x744, labels=ls746, cases=ls751}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt740 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x739 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x739))
       )), ("cond", cvtEXPR x744), ("labels", PrettyRep.List (List.map (fn x745 => 
                                                                              cvtIDENT x745
                                                                       ) ls746)), 
          ("cases", PrettyRep.List (List.map (fn x750 => cvtCASE x750
                                             ) ls751))]))
     | cvtSTMT (SwitchTypeStmt{cond=x766, ty=x767, cases=ls769}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x766), ("ty", cvtTYPE_EXPR x767), 
          ("cases", PrettyRep.List (List.map (fn x768 => cvtTYPE_CASE x768
                                             ) ls769))]))
     | cvtSTMT (DXNStmt{expr=x782}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x782)]))
   and cvtEXPR (TernaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x794, cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (BinaryTypeExpr(x800, x801, x802)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x800, cvtEXPR x801, cvtTYPE_EXPR x802]))
     | cvtEXPR (ExpectedTypeExpr(x806, x807)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x806, cvtEXPR x807]))
     | cvtEXPR (UnaryExpr(x811, x812)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x811, 
          cvtEXPR x812]))
     | cvtEXPR (TypeExpr x816) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x816))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt821) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x820))
       ))
     | cvtEXPR (SuperExpr opt828) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt828 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x827 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x827))
       ))
     | cvtEXPR (LiteralExpr x834) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x834))
     | cvtEXPR (CallExpr{func=x837, actuals=ls839}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x837), ("actuals", PrettyRep.List (List.map (fn x838 => 
                                                                                                   cvtEXPR x838
                                                                                            ) ls839))]))
     | cvtEXPR (ApplyTypeExpr{expr=x850, actuals=ls852}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x850), ("actuals", PrettyRep.List (List.map (fn x851 => 
                                                                                                   cvtTYPE_EXPR x851
                                                                                            ) ls852))]))
     | cvtEXPR (LetExpr{defs=x863, body=x864, head=opt866}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x863), ("body", cvtEXPR x864), 
          ("head", 
       (case opt866 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x865 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x865))
       ))]))
     | cvtEXPR (NewExpr{obj=x879, actuals=ls881}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x879), ("actuals", PrettyRep.List (List.map (fn x880 => 
                                                                                                  cvtEXPR x880
                                                                                           ) ls881))]))
     | cvtEXPR (ObjectRef{base=x892, ident=x893, pos=opt895}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x892), ("ident", cvtIDENT_EXPR x893), 
          ("pos", 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x894))
       ))]))
     | cvtEXPR (LexicalRef{ident=x908, pos=opt910}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x908), ("pos", 
       (case opt910 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x909 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x909))
       ))]))
     | cvtEXPR (SetExpr(x921, x922, x923)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x921, 
          cvtEXPR x922, cvtEXPR x923]))
     | cvtEXPR (ListExpr ls928) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x927 => 
                                                                                                    cvtEXPR x927
                                                                                             ) ls928)))
     | cvtEXPR (InitExpr(x934, x935, x936)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x934, 
          cvtHEAD x935, cvtINITS x936]))
     | cvtEXPR (SliceExpr(x940, x941, x942)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x940, cvtEXPR x941, cvtEXPR x942]))
     | cvtEXPR (GetTemp n946) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n946))
     | cvtEXPR (GetParam n949) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n949))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n955) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n955))
     | cvtFIXTURE_NAME (PropName x958) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x958))
   and cvtIDENT_EXPR (Identifier{ident=x961, openNamespaces=ls967}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x961), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls963 => PrettyRep.List (List.map (fn x962 => 
                                                                                cvtNAMESPACE x962
                                                                         ) ls963)
                                   ) ls967))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x978, expr=x979}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x978), ("expr", cvtEXPR x979)]))
     | cvtIDENT_EXPR (AttributeIdentifier x987) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x987))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x990, openNamespaces=ls996}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x990), ("openNamespaces", PrettyRep.List (List.map (fn ls992 => 
                                                                            PrettyRep.List (List.map (fn x991 => 
                                                                                                            cvtNAMESPACE x991
                                                                                                     ) ls992)
                                                                     ) ls996))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1007, ident=s1008}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1007), ("ident", PrettyRep.UniStr s1008)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1016, typeArgs=ls1018}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1016), ("typeArgs", 
          PrettyRep.List (List.map (fn x1017 => cvtTYPE_EXPR x1017
                                   ) ls1018))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1030, x1034)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1029 => cvtIDENT x1029
                                                          ) ls1030), cvtIDENT_EXPR x1034]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1041) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1041))
     | cvtLITERAL (LiteralContextualDecimalInteger s1044) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1044))
     | cvtLITERAL (LiteralContextualHexInteger s1047) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1047))
     | cvtLITERAL (LiteralDouble r1050) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1050))
     | cvtLITERAL (LiteralDecimal d1053) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1053))
     | cvtLITERAL (LiteralInt i1056) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1056))
     | cvtLITERAL (LiteralUInt u1059) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1059))
     | cvtLITERAL (LiteralBoolean b1062) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1062))
     | cvtLITERAL (LiteralString s1065) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1065))
     | cvtLITERAL (LiteralArray{exprs=ls1069, ty=opt1074}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1068 => 
                                                                         cvtEXPR x1068
                                                                  ) ls1069)), 
          ("ty", 
       (case opt1074 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1073))
       ))]))
     | cvtLITERAL (LiteralXML ls1086) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1085 => 
                                                                                                            cvtEXPR x1085
                                                                                                     ) ls1086)))
     | cvtLITERAL (LiteralNamespace x1092) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1092))
     | cvtLITERAL (LiteralObject{expr=ls1096, ty=opt1101}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1095 => 
                                                                        cvtFIELD x1095
                                                                 ) ls1096)), 
          ("ty", 
       (case opt1101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1100 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1100))
       ))]))
     | cvtLITERAL (LiteralFunction x1112) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1112))
     | cvtLITERAL (LiteralRegExp{str=s1115}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1115)]))
   and cvtBLOCK (Block x1121) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1121))
   and cvtFIXTURE (NamespaceFixture x1124) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1124))
     | cvtFIXTURE (ClassFixture x1127) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1127))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1132) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1132))
     | cvtFIXTURE (MethodFixture{func=x1135, ty=x1136, readOnly=b1137, override=b1138, 
          final=b1139}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1135), ("ty", cvtTYPE_EXPR x1136), ("readOnly", PrettyRep.Bool b1137), 
          ("override", PrettyRep.Bool b1138), ("final", PrettyRep.Bool b1139)]))
     | cvtFIXTURE (ValFixture{ty=x1153, readOnly=b1154}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1153), ("readOnly", PrettyRep.Bool b1154)]))
     | cvtFIXTURE (VirtualValFixture x1162) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1162))
   and cvtBINDINGS (ls1166, ls1171) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1165 => 
                                                                                       cvtBINDING x1165
                                                                                ) ls1166), 
          PrettyRep.List (List.map (fn x1170 => cvtINIT_STEP x1170
                                   ) ls1171)]
   and cvtFIXTURES ls1179 = PrettyRep.List (List.map (fn (x1176, x1177) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1176, 
                                                            cvtFIXTURE x1177]
                                                     ) ls1179)
   and cvtINITS ls1186 = PrettyRep.List (List.map (fn (x1183, x1184) => PrettyRep.Tuple [cvtFIXTURE_NAME x1183, 
                                                         cvtEXPR x1184]
                                                  ) ls1186)
   and cvtHEAD (x1190, x1191) = PrettyRep.Tuple [cvtFIXTURES x1190, cvtINITS x1191]
   and cvtFIELD {kind=x1193, name=x1194, init=x1195} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1193), ("name", cvtIDENT_EXPR x1194), ("init", cvtEXPR x1195)]
   and cvtFIELD_TYPE {name=x1203, ty=x1204} = PrettyRep.Rec [("name", cvtIDENT x1203), 
          ("ty", cvtTYPE_EXPR x1204)]
   and cvtFUNC_TYPE {typeParams=ls1211, params=ls1216, result=x1220, thisType=opt1222, 
          hasRest=b1226, minArgs=n1227} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1210 => 
                                                                                                        cvtIDENT x1210
                                                                                                 ) ls1211)), 
          ("params", PrettyRep.List (List.map (fn x1215 => cvtTYPE_EXPR x1215
                                              ) ls1216)), ("result", cvtTYPE_EXPR x1220), 
          ("thisType", 
       (case opt1222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1221))
       )), ("hasRest", PrettyRep.Bool b1226), ("minArgs", PrettyRep.Int n1227)]
   and cvtFUNC_DEFN {kind=x1241, ns=opt1243, final=b1247, override=b1248, prototype=b1249, 
          static=b1250, func=x1251} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1241), 
          ("ns", 
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       )), ("final", PrettyRep.Bool b1247), ("override", PrettyRep.Bool b1248), 
          ("prototype", PrettyRep.Bool b1249), ("static", PrettyRep.Bool b1250), 
          ("func", cvtFUNC x1251)]
   and cvtCTOR_DEFN x1267 = cvtCTOR x1267
   and cvtVAR_DEFN {kind=x1268, ns=opt1270, static=b1274, prototype=b1275, 
          bindings=x1276} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1268), 
          ("ns", 
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1269))
       )), ("static", PrettyRep.Bool b1274), ("prototype", PrettyRep.Bool b1275), 
          ("bindings", cvtBINDINGS x1276)]
   and cvtNAMESPACE_DEFN {ident=x1288, ns=opt1290, init=opt1295} = PrettyRep.Rec [("ident", 
          cvtIDENT x1288), ("ns", 
       (case opt1290 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1289 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1289))
       )), ("init", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1294))
       ))]
   and cvtCLASS_DEFN {ident=x1306, ns=opt1308, nonnullable=b1312, dynamic=b1313, 
          final=b1314, params=ls1316, extends=opt1321, implements=ls1326, classDefns=ls1331, 
          instanceDefns=ls1336, instanceStmts=ls1341, ctorDefn=opt1346} = PrettyRep.Rec [("ident", 
          cvtIDENT x1306), ("ns", 
       (case opt1308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1307 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1307))
       )), ("nonnullable", PrettyRep.Bool b1312), ("dynamic", PrettyRep.Bool b1313), 
          ("final", PrettyRep.Bool b1314), ("params", PrettyRep.List (List.map (fn x1315 => 
                                                                                      cvtIDENT x1315
                                                                               ) ls1316)), 
          ("extends", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1320))
       )), ("implements", PrettyRep.List (List.map (fn x1325 => cvtIDENT_EXPR x1325
                                                   ) ls1326)), ("classDefns", 
          PrettyRep.List (List.map (fn x1330 => cvtDEFN x1330
                                   ) ls1331)), ("instanceDefns", PrettyRep.List (List.map (fn x1335 => 
                                                                                                 cvtDEFN x1335
                                                                                          ) ls1336)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1340 => cvtSTMT x1340
                                                     ) ls1341)), ("ctorDefn", 
          
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1345))
       ))]
   and cvtINTERFACE_DEFN {ident=x1375, ns=opt1377, nonnullable=b1381, params=ls1383, 
          extends=ls1388, block=x1392} = PrettyRep.Rec [("ident", cvtIDENT x1375), 
          ("ns", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1376))
       )), ("nonnullable", PrettyRep.Bool b1381), ("params", PrettyRep.List (List.map (fn x1382 => 
                                                                                             cvtIDENT x1382
                                                                                      ) ls1383)), 
          ("extends", PrettyRep.List (List.map (fn x1387 => cvtIDENT_EXPR x1387
                                               ) ls1388)), ("block", cvtBLOCK x1392)]
   and cvtTYPE_DEFN {ident=x1406, ns=opt1408, init=x1412} = PrettyRep.Rec [("ident", 
          cvtIDENT x1406), ("ns", 
       (case opt1408 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1407 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1407))
       )), ("init", cvtTYPE_EXPR x1412)]
   and cvtFOR_ENUM_STMT {isEach=b1420, defn=opt1422, obj=x1426, fixtures=opt1428, 
          next=x1432, labels=ls1434, body=x1438} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1420), ("defn", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1421))
       )), ("obj", cvtEXPR x1426), ("fixtures", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1427 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1427))
       )), ("next", cvtSTMT x1432), ("labels", PrettyRep.List (List.map (fn x1433 => 
                                                                               cvtIDENT x1433
                                                                        ) ls1434)), 
          ("body", cvtSTMT x1438)]
   and cvtFOR_STMT {fixtures=opt1455, defn=opt1460, init=ls1465, cond=x1469, 
          update=x1470, labels=ls1472, body=x1476} = PrettyRep.Rec [("fixtures", 
          
       (case opt1455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1454 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1454))
       )), ("defn", 
       (case opt1460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1459))
       )), ("init", PrettyRep.List (List.map (fn x1464 => cvtSTMT x1464
                                             ) ls1465)), ("cond", cvtEXPR x1469), 
          ("update", cvtEXPR x1470), ("labels", PrettyRep.List (List.map (fn x1471 => 
                                                                                cvtIDENT x1471
                                                                         ) ls1472)), 
          ("body", cvtSTMT x1476)]
   and cvtWHILE_STMT {cond=x1492, fixtures=opt1494, body=x1498, labels=ls1500} = 
          PrettyRep.Rec [("cond", cvtEXPR x1492), ("fixtures", 
       (case opt1494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1493))
       )), ("body", cvtSTMT x1498), ("labels", PrettyRep.List (List.map (fn x1499 => 
                                                                               cvtIDENT x1499
                                                                        ) ls1500))]
   and cvtDIRECTIVES {pragmas=ls1514, defns=ls1519, head=opt1524, body=ls1529, 
          pos=opt1534} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1513 => 
                                                                                    cvtPRAGMA x1513
                                                                             ) ls1514)), 
          ("defns", PrettyRep.List (List.map (fn x1518 => cvtDEFN x1518
                                             ) ls1519)), ("head", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1523))
       )), ("body", PrettyRep.List (List.map (fn x1528 => cvtSTMT x1528
                                             ) ls1529)), ("pos", 
       (case opt1534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1533))
       ))]
   and cvtCASE {label=opt1550, inits=opt1555, body=x1559} = PrettyRep.Rec [("label", 
          
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1549))
       )), ("inits", 
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1554))
       )), ("body", cvtBLOCK x1559)]
   and cvtTYPE_CASE {ty=opt1568, body=x1572} = PrettyRep.Rec [("ty", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1567))
       )), ("body", cvtSTMT x1572)]
   and cvtCATCH_CLAUSE {bindings=x1578, ty=x1579, fixtures=opt1581, block=x1585} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1578), ("ty", cvtTYPE_EXPR x1579), 
          ("fixtures", 
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1580))
       )), ("block", cvtBLOCK x1585)]
   and cvtFUNC_NAME {kind=x1595, ident=x1596} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1595), 
          ("ident", cvtIDENT x1596)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1602, getter=opt1604, setter=opt1609} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1602), ("getter", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1603 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1603))
       )), ("setter", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1608 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1608))
       ))]
   and cvtPACKAGE {name=ls1621, block=x1625} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1620 => 
                                                                                                       cvtIDENT x1620
                                                                                                ) ls1621)), 
          ("block", cvtBLOCK x1625)]
   and cvtPROGRAM {packages=ls1632, fixtures=opt1637, block=x1641} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1631 => cvtPACKAGE x1631
                                   ) ls1632)), ("fixtures", 
       (case opt1637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1636 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1636))
       )), ("block", cvtBLOCK x1641)]
end

