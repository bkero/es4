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
          SOME (cvtUSTRING x23))
     | cvtNAMESPACE (AnonUserNamespace n26) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n26))
     | cvtNAMESPACE (LimitedNamespace(x29, x30)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x29, cvtNAMESPACE x30]))
   and cvtNAME {ns=x34, id=x35} = PrettyRep.Rec [("ns", cvtNAMESPACE x34), 
          ("id", cvtIDENT x35)]
   and cvtMULTINAME {nss=ls46, id=x50} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls42 => 
                                                                                                PrettyRep.List (List.map (fn x41 => 
                                                                                                                                cvtNAMESPACE x41
                                                                                                                         ) ls42)
                                                                                         ) ls46)), 
          ("id", cvtIDENT x50)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x61, roundingMode=r62, precision=n63} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x61), ("roundingMode", 
          PrettyRep.DecRm r62), ("precision", PrettyRep.Int n63)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt76) = PrettyRep.Ctor ("Plus", SOME 
       (case opt76 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x75 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x75))
       ))
     | cvtBINOP (Minus opt83) = PrettyRep.Ctor ("Minus", SOME 
       (case opt83 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x82 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x82))
       ))
     | cvtBINOP (Times opt90) = PrettyRep.Ctor ("Times", SOME 
       (case opt90 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x89 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x89))
       ))
     | cvtBINOP (Divide opt97) = PrettyRep.Ctor ("Divide", SOME 
       (case opt97 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x96 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x96))
       ))
     | cvtBINOP (Remainder opt104) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x103 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x103))
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
     | cvtBINOP (Equals opt121) = PrettyRep.Ctor ("Equals", SOME 
       (case opt121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x120 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x120))
       ))
     | cvtBINOP (NotEquals opt128) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt128 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x127 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x127))
       ))
     | cvtBINOP (StrictEquals opt135) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x134 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x134))
       ))
     | cvtBINOP (StrictNotEquals opt142) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt142 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x141 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x141))
       ))
     | cvtBINOP (Less opt149) = PrettyRep.Ctor ("Less", SOME 
       (case opt149 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x148 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x148))
       ))
     | cvtBINOP (LessOrEqual opt156) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x155 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x155))
       ))
     | cvtBINOP (Greater opt163) = PrettyRep.Ctor ("Greater", SOME 
       (case opt163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x162 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x162))
       ))
     | cvtBINOP (GreaterOrEqual opt170) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x169 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x169))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt179) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x178))
       ))
     | cvtASSIGNOP (AssignMinus opt186) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x185 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x185))
       ))
     | cvtASSIGNOP (AssignTimes opt193) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x192 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x192))
       ))
     | cvtASSIGNOP (AssignDivide opt200) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x199 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x199))
       ))
     | cvtASSIGNOP (AssignRemainder opt207) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x206 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x206))
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
     | cvtUNOP (PreIncrement opt225) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x224 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x224))
       ))
     | cvtUNOP (PreDecrement opt232) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x231 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x231))
       ))
     | cvtUNOP (PostIncrement opt239) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x238 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x238))
       ))
     | cvtUNOP (PostDecrement opt246) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt246 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x245 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x245))
       ))
     | cvtUNOP (UnaryPlus opt253) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x252 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x252))
       ))
     | cvtUNOP (UnaryMinus opt260) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x259 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x259))
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
   and cvtPRAGMA (UseNamespace x277) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x277))
     | cvtPRAGMA (UseDefaultNamespace x280) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x280))
     | cvtPRAGMA (UseNumber x283) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x283))
     | cvtPRAGMA (UseRounding r286) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r286))
     | cvtPRAGMA (UsePrecision n289) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n289))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls295, name=x299, alias=opt301}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x294 => 
                                                                           cvtIDENT x294
                                                                    ) ls295)), 
          ("name", cvtIDENT x299), ("alias", 
       (case opt301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x300 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x300))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x320, extends=opt322, implements=ls327, classFixtures=x331, 
          instanceFixtures=x332, instanceInits=x333, constructor=opt335, classType=x339, 
          instanceType=x340}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x320), ("extends", 
       (case opt322 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x321 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x321))
       )), ("implements", PrettyRep.List (List.map (fn x326 => cvtNAME x326
                                                   ) ls327)), ("classFixtures", 
          cvtFIXTURES x331), ("instanceFixtures", cvtFIXTURES x332), ("instanceInits", 
          cvtHEAD x333), ("constructor", 
       (case opt335 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x334 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x334))
       )), ("classType", cvtTYPE_EXPR x339), ("instanceType", cvtTYPE_EXPR x340)]))
   and cvtCTOR (Ctor{settings=x362, superArgs=ls364, func=x368}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x362), ("superArgs", PrettyRep.List (List.map (fn x363 => 
                                                                                                         cvtEXPR x363
                                                                                                  ) ls364)), 
          ("func", cvtFUNC x368)]))
   and cvtFUNC (Func{name=x378, fsig=x379, isNative=b380, block=x381, param=x382, 
          defaults=ls384, ty=x388}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x378), ("fsig", cvtFUNC_SIG x379), ("isNative", PrettyRep.Bool b380), 
          ("block", cvtBLOCK x381), ("param", cvtHEAD x382), ("defaults", PrettyRep.List (List.map (fn x383 => 
                                                                                                          cvtEXPR x383
                                                                                                   ) ls384)), 
          ("ty", cvtFUNC_TYPE x388)]))
   and cvtDEFN (ClassDefn x406) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x406))
     | cvtDEFN (VariableDefn x409) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x409))
     | cvtDEFN (FunctionDefn x412) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x412))
     | cvtDEFN (ConstructorDefn x415) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x415))
     | cvtDEFN (InterfaceDefn x418) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x418))
     | cvtDEFN (NamespaceDefn x421) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x421))
     | cvtDEFN (TypeDefn x424) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x424))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls428, params=x432, paramTypes=ls434, 
          defaults=ls439, ctorInits=opt450, returnType=x454, thisType=opt456, 
          hasRest=b460}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x427 => cvtIDENT x427
                                   ) ls428)), ("params", cvtBINDINGS x432), 
          ("paramTypes", PrettyRep.List (List.map (fn x433 => cvtTYPE_EXPR x433
                                                  ) ls434)), ("defaults", PrettyRep.List (List.map (fn x438 => 
                                                                                                          cvtEXPR x438
                                                                                                   ) ls439)), 
          ("ctorInits", 
       (case opt450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x443, ls445) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x443, 
            PrettyRep.List (List.map (fn x444 => cvtEXPR x444
                                     ) ls445)]))
       )), ("returnType", cvtTYPE_EXPR x454), ("thisType", 
       (case opt456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x455 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x455))
       )), ("hasRest", PrettyRep.Bool b460)]))
   and cvtBINDING (Binding{ident=x480, ty=x481}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x480), ("ty", cvtTYPE_EXPR x481)]))
   and cvtBINDING_IDENT (TempIdent n489) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n489))
     | cvtBINDING_IDENT (ParamIdent n492) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n492))
     | cvtBINDING_IDENT (PropIdent x495) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x495))
   and cvtINIT_STEP (InitStep(x498, x499)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x498, 
          cvtEXPR x499]))
     | cvtINIT_STEP (AssignStep(x503, x504)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x503, cvtEXPR x504]))
   and cvtTYPE_EXPR (SpecialType x508) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x508))
     | cvtTYPE_EXPR (UnionType ls512) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x511 => 
                                                                                                           cvtTYPE_EXPR x511
                                                                                                    ) ls512)))
     | cvtTYPE_EXPR (ArrayType ls519) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x518 => 
                                                                                                           cvtTYPE_EXPR x518
                                                                                                    ) ls519)))
     | cvtTYPE_EXPR (TypeName x525) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x525))
     | cvtTYPE_EXPR (ElementTypeRef(x528, n529)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x528, PrettyRep.Int n529]))
     | cvtTYPE_EXPR (FieldTypeRef(x533, x534)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x533, cvtIDENT x534]))
     | cvtTYPE_EXPR (FunctionType x538) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x538))
     | cvtTYPE_EXPR (ObjectType ls542) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x541 => 
                                                                                                             cvtFIELD_TYPE x541
                                                                                                      ) ls542)))
     | cvtTYPE_EXPR (AppType{base=x548, args=ls550}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x548), ("args", PrettyRep.List (List.map (fn x549 => 
                                                                                                     cvtTYPE_EXPR x549
                                                                                              ) ls550))]))
     | cvtTYPE_EXPR (NullableType{expr=x561, nullable=b562}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x561), ("nullable", PrettyRep.Bool b562)]))
     | cvtTYPE_EXPR (InstanceType{name=x570, typeParams=ls572, ty=x576, isDynamic=b577}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x570), 
          ("typeParams", PrettyRep.List (List.map (fn x571 => cvtIDENT x571
                                                  ) ls572)), ("ty", cvtTYPE_EXPR x576), 
          ("isDynamic", PrettyRep.Bool b577)]))
     | cvtTYPE_EXPR (NominalType x589) = PrettyRep.Ctor ("NominalType", SOME (cvtNAME x589))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x593) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x593))
     | cvtSTMT (InitStmt{kind=x596, ns=opt598, prototype=b602, static=b603, 
          temps=x604, inits=ls606}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x596), ("ns", 
       (case opt598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x597 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x597))
       )), ("prototype", PrettyRep.Bool b602), ("static", PrettyRep.Bool b603), 
          ("temps", cvtBINDINGS x604), ("inits", PrettyRep.List (List.map (fn x605 => 
                                                                                 cvtINIT_STEP x605
                                                                          ) ls606))]))
     | cvtSTMT (ClassBlock{ns=opt626, ident=x630, name=opt632, block=x636}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x625))
       )), ("ident", cvtIDENT x630), ("name", 
       (case opt632 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x631 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x631))
       )), ("block", cvtBLOCK x636)]))
     | cvtSTMT (ForInStmt x648) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x648))
     | cvtSTMT (ThrowStmt x651) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x651))
     | cvtSTMT (ReturnStmt x654) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x654))
     | cvtSTMT (BreakStmt opt658) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x657 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x657))
       ))
     | cvtSTMT (ContinueStmt opt665) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt665 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x664 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x664))
       ))
     | cvtSTMT (BlockStmt x671) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x671))
     | cvtSTMT (LabeledStmt(x674, x675)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x674, 
          cvtSTMT x675]))
     | cvtSTMT (LetStmt x679) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x679))
     | cvtSTMT (WhileStmt x682) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x682))
     | cvtSTMT (DoWhileStmt x685) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x685))
     | cvtSTMT (ForStmt x688) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x688))
     | cvtSTMT (IfStmt{cnd=x691, thn=x692, els=x693}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x691), ("thn", cvtSTMT x692), 
          ("els", cvtSTMT x693)]))
     | cvtSTMT (WithStmt{obj=x703, ty=x704, body=x705}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x703), ("ty", cvtTYPE_EXPR x704), 
          ("body", cvtSTMT x705)]))
     | cvtSTMT (TryStmt{block=x715, catches=ls717, finally=opt722}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x715), ("catches", PrettyRep.List (List.map (fn x716 => 
                                                                                                     cvtCATCH_CLAUSE x716
                                                                                              ) ls717)), 
          ("finally", 
       (case opt722 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x721 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x721))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt736, cond=x740, labels=ls742, cases=ls747}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x735))
       )), ("cond", cvtEXPR x740), ("labels", PrettyRep.List (List.map (fn x741 => 
                                                                              cvtIDENT x741
                                                                       ) ls742)), 
          ("cases", PrettyRep.List (List.map (fn x746 => cvtCASE x746
                                             ) ls747))]))
     | cvtSTMT (SwitchTypeStmt{cond=x762, ty=x763, cases=ls765}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x762), ("ty", cvtTYPE_EXPR x763), 
          ("cases", PrettyRep.List (List.map (fn x764 => cvtTYPE_CASE x764
                                             ) ls765))]))
     | cvtSTMT (Dxns{expr=x778}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x778)]))
   and cvtEXPR (TernaryExpr(x784, x785, x786, x787)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x784, cvtEXPR x785, cvtEXPR x786, 
          cvtEXPR x787]))
     | cvtEXPR (BinaryExpr(x791, x792, x793)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x791, cvtEXPR x792, cvtEXPR x793]))
     | cvtEXPR (BinaryTypeExpr(x797, x798, x799)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x797, cvtEXPR x798, cvtTYPE_EXPR x799]))
     | cvtEXPR (ExpectedTypeExpr(x803, x804)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x803, cvtEXPR x804]))
     | cvtEXPR (UnaryExpr(x808, x809)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x808, 
          cvtEXPR x809]))
     | cvtEXPR (TypeExpr x813) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x813))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt818) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x817 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x817))
       ))
     | cvtEXPR (SuperExpr opt825) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt825 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x824 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x824))
       ))
     | cvtEXPR (LiteralExpr x831) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x831))
     | cvtEXPR (CallExpr{func=x834, actuals=ls836}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x834), ("actuals", PrettyRep.List (List.map (fn x835 => 
                                                                                                   cvtEXPR x835
                                                                                            ) ls836))]))
     | cvtEXPR (ApplyTypeExpr{expr=x847, actuals=ls849}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x847), ("actuals", PrettyRep.List (List.map (fn x848 => 
                                                                                                   cvtTYPE_EXPR x848
                                                                                            ) ls849))]))
     | cvtEXPR (LetExpr{defs=x860, body=x861, head=opt863}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x860), ("body", cvtEXPR x861), 
          ("head", 
       (case opt863 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x862 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x862))
       ))]))
     | cvtEXPR (NewExpr{obj=x876, actuals=ls878}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x876), ("actuals", PrettyRep.List (List.map (fn x877 => 
                                                                                                  cvtEXPR x877
                                                                                           ) ls878))]))
     | cvtEXPR (ObjectRef{base=x889, ident=x890, pos=opt892}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x889), ("ident", cvtIDENT_EXPR x890), 
          ("pos", 
       (case opt892 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x891 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x891))
       ))]))
     | cvtEXPR (LexicalRef{ident=x905, pos=opt907}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x905), ("pos", 
       (case opt907 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x906 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x906))
       ))]))
     | cvtEXPR (SetExpr(x918, x919, x920)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x918, 
          cvtEXPR x919, cvtEXPR x920]))
     | cvtEXPR (ListExpr ls925) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x924 => 
                                                                                                    cvtEXPR x924
                                                                                             ) ls925)))
     | cvtEXPR (InitExpr(x931, x932, x933)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x931, 
          cvtHEAD x932, cvtINITS x933]))
     | cvtEXPR (SliceExpr(x937, x938, x939)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x937, cvtEXPR x938, cvtEXPR x939]))
     | cvtEXPR (GetTemp n943) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n943))
     | cvtEXPR (GetParam n946) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n946))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n952) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n952))
     | cvtFIXTURE_NAME (PropName x955) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x955))
   and cvtIDENT_EXPR (Identifier{ident=x958, openNamespaces=ls964}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x958), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls960 => PrettyRep.List (List.map (fn x959 => 
                                                                                cvtNAMESPACE x959
                                                                         ) ls960)
                                   ) ls964))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x975, expr=x976}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x975), ("expr", cvtEXPR x976)]))
     | cvtIDENT_EXPR (AttributeIdentifier x984) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x984))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x987, openNamespaces=ls993}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x987), ("openNamespaces", PrettyRep.List (List.map (fn ls989 => 
                                                                            PrettyRep.List (List.map (fn x988 => 
                                                                                                            cvtNAMESPACE x988
                                                                                                     ) ls989)
                                                                     ) ls993))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1004, ident=x1005}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1004), ("ident", cvtUSTRING x1005)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1013, typeArgs=ls1015}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1013), ("typeArgs", 
          PrettyRep.List (List.map (fn x1014 => cvtTYPE_EXPR x1014
                                   ) ls1015))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1027, x1031)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1026 => cvtIDENT x1026
                                                          ) ls1027), cvtIDENT_EXPR x1031]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1038) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1038))
     | cvtLITERAL (LiteralContextualDecimalInteger s1041) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1041))
     | cvtLITERAL (LiteralContextualHexInteger s1044) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1044))
     | cvtLITERAL (LiteralDouble r1047) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1047))
     | cvtLITERAL (LiteralDecimal d1050) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1050))
     | cvtLITERAL (LiteralInt i1053) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1053))
     | cvtLITERAL (LiteralUInt u1056) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1056))
     | cvtLITERAL (LiteralBoolean b1059) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1059))
     | cvtLITERAL (LiteralString x1062) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1062))
     | cvtLITERAL (LiteralArray{exprs=ls1066, ty=opt1071}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1065 => 
                                                                         cvtEXPR x1065
                                                                  ) ls1066)), 
          ("ty", 
       (case opt1071 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1070 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1070))
       ))]))
     | cvtLITERAL (LiteralXML ls1083) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1082 => 
                                                                                                            cvtEXPR x1082
                                                                                                     ) ls1083)))
     | cvtLITERAL (LiteralNamespace x1089) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1089))
     | cvtLITERAL (LiteralObject{expr=ls1093, ty=opt1098}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1092 => 
                                                                        cvtFIELD x1092
                                                                 ) ls1093)), 
          ("ty", 
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1097))
       ))]))
     | cvtLITERAL (LiteralFunction x1109) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1109))
     | cvtLITERAL (LiteralRegExp{str=x1112}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1112)]))
   and cvtBLOCK (Block x1118) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1118))
   and cvtFIXTURE (NamespaceFixture x1121) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1121))
     | cvtFIXTURE (ClassFixture x1124) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1124))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1129) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1129))
     | cvtFIXTURE (MethodFixture{func=x1132, ty=x1133, readOnly=b1134, override=b1135, 
          final=b1136}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1132), ("ty", cvtTYPE_EXPR x1133), ("readOnly", PrettyRep.Bool b1134), 
          ("override", PrettyRep.Bool b1135), ("final", PrettyRep.Bool b1136)]))
     | cvtFIXTURE (ValFixture{ty=x1150, readOnly=b1151}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1150), ("readOnly", PrettyRep.Bool b1151)]))
     | cvtFIXTURE (VirtualValFixture x1159) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1159))
   and cvtBINDINGS (ls1163, ls1168) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1162 => 
                                                                                       cvtBINDING x1162
                                                                                ) ls1163), 
          PrettyRep.List (List.map (fn x1167 => cvtINIT_STEP x1167
                                   ) ls1168)]
   and cvtFIXTURES ls1176 = PrettyRep.List (List.map (fn (x1173, x1174) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1173, 
                                                            cvtFIXTURE x1174]
                                                     ) ls1176)
   and cvtINITS ls1183 = PrettyRep.List (List.map (fn (x1180, x1181) => PrettyRep.Tuple [cvtFIXTURE_NAME x1180, 
                                                         cvtEXPR x1181]
                                                  ) ls1183)
   and cvtHEAD (x1187, x1188) = PrettyRep.Tuple [cvtFIXTURES x1187, cvtINITS x1188]
   and cvtFIELD {kind=x1190, name=x1191, init=x1192} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1190), ("name", cvtIDENT_EXPR x1191), ("init", cvtEXPR x1192)]
   and cvtFIELD_TYPE {name=x1200, ty=x1201} = PrettyRep.Rec [("name", cvtIDENT x1200), 
          ("ty", cvtTYPE_EXPR x1201)]
   and cvtTYPED_IDENT {name=x1207, ty=opt1209} = PrettyRep.Rec [("name", cvtIDENT x1207), 
          ("ty", 
       (case opt1209 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1208 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1208))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1219, params=ls1224, result=x1228, thisType=opt1230, 
          hasRest=b1234, minArgs=n1235} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1218 => 
                                                                                                        cvtIDENT x1218
                                                                                                 ) ls1219)), 
          ("params", PrettyRep.List (List.map (fn x1223 => cvtTYPE_EXPR x1223
                                              ) ls1224)), ("result", cvtTYPE_EXPR x1228), 
          ("thisType", 
       (case opt1230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1229 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1229))
       )), ("hasRest", PrettyRep.Bool b1234), ("minArgs", PrettyRep.Int n1235)]
   and cvtFUNC_DEFN {kind=x1249, ns=opt1251, final=b1255, override=b1256, prototype=b1257, 
          static=b1258, func=x1259} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1249), 
          ("ns", 
       (case opt1251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1250 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1250))
       )), ("final", PrettyRep.Bool b1255), ("override", PrettyRep.Bool b1256), 
          ("prototype", PrettyRep.Bool b1257), ("static", PrettyRep.Bool b1258), 
          ("func", cvtFUNC x1259)]
   and cvtCTOR_DEFN x1275 = cvtCTOR x1275
   and cvtVAR_DEFN {kind=x1276, ns=opt1278, static=b1282, prototype=b1283, 
          bindings=x1284} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1276), 
          ("ns", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1277))
       )), ("static", PrettyRep.Bool b1282), ("prototype", PrettyRep.Bool b1283), 
          ("bindings", cvtBINDINGS x1284)]
   and cvtNAMESPACE_DEFN {ident=x1296, ns=opt1298, init=opt1303} = PrettyRep.Rec [("ident", 
          cvtIDENT x1296), ("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1297))
       )), ("init", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1302))
       ))]
   and cvtCLASS_DEFN {ident=x1314, ns=opt1316, nonnullable=b1320, dynamic=b1321, 
          final=b1322, params=ls1324, extends=opt1329, implements=ls1334, classDefns=ls1339, 
          instanceDefns=ls1344, instanceStmts=ls1349, ctorDefn=opt1354} = PrettyRep.Rec [("ident", 
          cvtIDENT x1314), ("ns", 
       (case opt1316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1315 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1315))
       )), ("nonnullable", PrettyRep.Bool b1320), ("dynamic", PrettyRep.Bool b1321), 
          ("final", PrettyRep.Bool b1322), ("params", PrettyRep.List (List.map (fn x1323 => 
                                                                                      cvtIDENT x1323
                                                                               ) ls1324)), 
          ("extends", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1328))
       )), ("implements", PrettyRep.List (List.map (fn x1333 => cvtIDENT_EXPR x1333
                                                   ) ls1334)), ("classDefns", 
          PrettyRep.List (List.map (fn x1338 => cvtDEFN x1338
                                   ) ls1339)), ("instanceDefns", PrettyRep.List (List.map (fn x1343 => 
                                                                                                 cvtDEFN x1343
                                                                                          ) ls1344)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1348 => cvtSTMT x1348
                                                     ) ls1349)), ("ctorDefn", 
          
       (case opt1354 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1353 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1353))
       ))]
   and cvtINTERFACE_DEFN {ident=x1383, ns=opt1385, nonnullable=b1389, params=ls1391, 
          extends=ls1396, block=x1400} = PrettyRep.Rec [("ident", cvtIDENT x1383), 
          ("ns", 
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1384))
       )), ("nonnullable", PrettyRep.Bool b1389), ("params", PrettyRep.List (List.map (fn x1390 => 
                                                                                             cvtIDENT x1390
                                                                                      ) ls1391)), 
          ("extends", PrettyRep.List (List.map (fn x1395 => cvtIDENT_EXPR x1395
                                               ) ls1396)), ("block", cvtBLOCK x1400)]
   and cvtTYPE_DEFN {ident=x1414, ns=opt1416, init=x1420} = PrettyRep.Rec [("ident", 
          cvtIDENT x1414), ("ns", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1415))
       )), ("init", cvtTYPE_EXPR x1420)]
   and cvtFOR_ENUM_STMT {isEach=b1428, defn=opt1430, obj=x1434, fixtures=opt1436, 
          next=x1440, labels=ls1442, body=x1446} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1428), ("defn", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1429 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1429))
       )), ("obj", cvtEXPR x1434), ("fixtures", 
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1435))
       )), ("next", cvtSTMT x1440), ("labels", PrettyRep.List (List.map (fn x1441 => 
                                                                               cvtIDENT x1441
                                                                        ) ls1442)), 
          ("body", cvtSTMT x1446)]
   and cvtFOR_STMT {fixtures=opt1463, defn=opt1468, init=ls1473, cond=x1477, 
          update=x1478, labels=ls1480, body=x1484} = PrettyRep.Rec [("fixtures", 
          
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1462 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1462))
       )), ("defn", 
       (case opt1468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1467))
       )), ("init", PrettyRep.List (List.map (fn x1472 => cvtSTMT x1472
                                             ) ls1473)), ("cond", cvtEXPR x1477), 
          ("update", cvtEXPR x1478), ("labels", PrettyRep.List (List.map (fn x1479 => 
                                                                                cvtIDENT x1479
                                                                         ) ls1480)), 
          ("body", cvtSTMT x1484)]
   and cvtWHILE_STMT {cond=x1500, fixtures=opt1502, body=x1506, labels=ls1508} = 
          PrettyRep.Rec [("cond", cvtEXPR x1500), ("fixtures", 
       (case opt1502 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1501 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1501))
       )), ("body", cvtSTMT x1506), ("labels", PrettyRep.List (List.map (fn x1507 => 
                                                                               cvtIDENT x1507
                                                                        ) ls1508))]
   and cvtDIRECTIVES {pragmas=ls1522, defns=ls1527, head=opt1532, body=ls1537, 
          pos=opt1542} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1521 => 
                                                                                    cvtPRAGMA x1521
                                                                             ) ls1522)), 
          ("defns", PrettyRep.List (List.map (fn x1526 => cvtDEFN x1526
                                             ) ls1527)), ("head", 
       (case opt1532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1531 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1531))
       )), ("body", PrettyRep.List (List.map (fn x1536 => cvtSTMT x1536
                                             ) ls1537)), ("pos", 
       (case opt1542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1541 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1541))
       ))]
   and cvtCASE {label=opt1558, inits=opt1563, body=x1567} = PrettyRep.Rec [("label", 
          
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1557 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1557))
       )), ("inits", 
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1562))
       )), ("body", cvtBLOCK x1567)]
   and cvtTYPE_CASE {ty=opt1576, body=x1580} = PrettyRep.Rec [("ty", 
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1575 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1575))
       )), ("body", cvtSTMT x1580)]
   and cvtCATCH_CLAUSE {bindings=x1586, ty=x1587, fixtures=opt1589, block=x1593} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1586), ("ty", cvtTYPE_EXPR x1587), 
          ("fixtures", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1588))
       )), ("block", cvtBLOCK x1593)]
   and cvtFUNC_NAME {kind=x1603, ident=x1604} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1603), 
          ("ident", cvtIDENT x1604)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1610, getter=opt1612, setter=opt1617} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1610), ("getter", 
       (case opt1612 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1611 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1611))
       )), ("setter", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1616 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1616))
       ))]
   and cvtPACKAGE {name=ls1629, block=x1633} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1628 => 
                                                                                                       cvtIDENT x1628
                                                                                                ) ls1629)), 
          ("block", cvtBLOCK x1633)]
   and cvtPROGRAM {packages=ls1640, fixtures=opt1645, block=x1649} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1639 => cvtPACKAGE x1639
                                   ) ls1640)), ("fixtures", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1644 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1644))
       )), ("block", cvtBLOCK x1649)]
end

