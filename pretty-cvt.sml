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
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x590) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x590))
     | cvtSTMT (InitStmt{kind=x593, ns=opt595, prototype=b599, static=b600, 
          temps=x601, inits=ls603}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x593), ("ns", 
       (case opt595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x594 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x594))
       )), ("prototype", PrettyRep.Bool b599), ("static", PrettyRep.Bool b600), 
          ("temps", cvtBINDINGS x601), ("inits", PrettyRep.List (List.map (fn x602 => 
                                                                                 cvtINIT_STEP x602
                                                                          ) ls603))]))
     | cvtSTMT (ClassBlock{ns=opt623, ident=x627, name=opt629, block=x633}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x622 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x622))
       )), ("ident", cvtIDENT x627), ("name", 
       (case opt629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x628 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x628))
       )), ("block", cvtBLOCK x633)]))
     | cvtSTMT (ForInStmt x645) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x645))
     | cvtSTMT (ThrowStmt x648) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x648))
     | cvtSTMT (ReturnStmt x651) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x651))
     | cvtSTMT (BreakStmt opt655) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x654 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x654))
       ))
     | cvtSTMT (ContinueStmt opt662) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x661))
       ))
     | cvtSTMT (BlockStmt x668) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x668))
     | cvtSTMT (LabeledStmt(x671, x672)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x671, 
          cvtSTMT x672]))
     | cvtSTMT (LetStmt x676) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x676))
     | cvtSTMT (WhileStmt x679) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x679))
     | cvtSTMT (DoWhileStmt x682) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x682))
     | cvtSTMT (ForStmt x685) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x685))
     | cvtSTMT (IfStmt{cnd=x688, thn=x689, els=x690}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x688), ("thn", cvtSTMT x689), 
          ("els", cvtSTMT x690)]))
     | cvtSTMT (WithStmt{obj=x700, ty=x701, body=x702}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x700), ("ty", cvtTYPE_EXPR x701), 
          ("body", cvtSTMT x702)]))
     | cvtSTMT (TryStmt{block=x712, catches=ls714, finally=opt719}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x712), ("catches", PrettyRep.List (List.map (fn x713 => 
                                                                                                     cvtCATCH_CLAUSE x713
                                                                                              ) ls714)), 
          ("finally", 
       (case opt719 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x718 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x718))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt733, cond=x737, labels=ls739, cases=ls744}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x732))
       )), ("cond", cvtEXPR x737), ("labels", PrettyRep.List (List.map (fn x738 => 
                                                                              cvtIDENT x738
                                                                       ) ls739)), 
          ("cases", PrettyRep.List (List.map (fn x743 => cvtCASE x743
                                             ) ls744))]))
     | cvtSTMT (SwitchTypeStmt{cond=x759, ty=x760, cases=ls762}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x759), ("ty", cvtTYPE_EXPR x760), 
          ("cases", PrettyRep.List (List.map (fn x761 => cvtTYPE_CASE x761
                                             ) ls762))]))
     | cvtSTMT (Dxns{expr=x775}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x775)]))
   and cvtEXPR (TernaryExpr(x781, x782, x783, x784)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x781, cvtEXPR x782, cvtEXPR x783, 
          cvtEXPR x784]))
     | cvtEXPR (BinaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryTypeExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x794, cvtEXPR x795, cvtTYPE_EXPR x796]))
     | cvtEXPR (ExpectedTypeExpr(x800, x801)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x800, cvtEXPR x801]))
     | cvtEXPR (UnaryExpr(x805, x806)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x805, 
          cvtEXPR x806]))
     | cvtEXPR (TypeExpr x810) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x810))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt815) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt815 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x814 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x814))
       ))
     | cvtEXPR (SuperExpr opt822) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt822 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x821 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x821))
       ))
     | cvtEXPR (LiteralExpr x828) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x828))
     | cvtEXPR (CallExpr{func=x831, actuals=ls833}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x831), ("actuals", PrettyRep.List (List.map (fn x832 => 
                                                                                                   cvtEXPR x832
                                                                                            ) ls833))]))
     | cvtEXPR (ApplyTypeExpr{expr=x844, actuals=ls846}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x844), ("actuals", PrettyRep.List (List.map (fn x845 => 
                                                                                                   cvtTYPE_EXPR x845
                                                                                            ) ls846))]))
     | cvtEXPR (LetExpr{defs=x857, body=x858, head=opt860}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x857), ("body", cvtEXPR x858), 
          ("head", 
       (case opt860 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x859 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x859))
       ))]))
     | cvtEXPR (NewExpr{obj=x873, actuals=ls875}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x873), ("actuals", PrettyRep.List (List.map (fn x874 => 
                                                                                                  cvtEXPR x874
                                                                                           ) ls875))]))
     | cvtEXPR (ObjectRef{base=x886, ident=x887, pos=opt889}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x886), ("ident", cvtIDENT_EXPR x887), 
          ("pos", 
       (case opt889 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x888 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x888))
       ))]))
     | cvtEXPR (LexicalRef{ident=x902, pos=opt904}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x902), ("pos", 
       (case opt904 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x903 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x903))
       ))]))
     | cvtEXPR (SetExpr(x915, x916, x917)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x915, 
          cvtEXPR x916, cvtEXPR x917]))
     | cvtEXPR (ListExpr ls922) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x921 => 
                                                                                                    cvtEXPR x921
                                                                                             ) ls922)))
     | cvtEXPR (InitExpr(x928, x929, x930)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x928, 
          cvtHEAD x929, cvtINITS x930]))
     | cvtEXPR (SliceExpr(x934, x935, x936)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x934, cvtEXPR x935, cvtEXPR x936]))
     | cvtEXPR (GetTemp n940) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n940))
     | cvtEXPR (GetParam n943) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n943))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n949) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n949))
     | cvtFIXTURE_NAME (PropName x952) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x952))
   and cvtIDENT_EXPR (Identifier{ident=x955, openNamespaces=ls961}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x955), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls957 => PrettyRep.List (List.map (fn x956 => 
                                                                                cvtNAMESPACE x956
                                                                         ) ls957)
                                   ) ls961))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x972, expr=x973}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x972), ("expr", cvtEXPR x973)]))
     | cvtIDENT_EXPR (AttributeIdentifier x981) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x981))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x984, openNamespaces=ls990}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x984), ("openNamespaces", PrettyRep.List (List.map (fn ls986 => 
                                                                            PrettyRep.List (List.map (fn x985 => 
                                                                                                            cvtNAMESPACE x985
                                                                                                     ) ls986)
                                                                     ) ls990))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1001, ident=x1002}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1001), ("ident", cvtUSTRING x1002)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1010, typeArgs=ls1012}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1010), ("typeArgs", 
          PrettyRep.List (List.map (fn x1011 => cvtTYPE_EXPR x1011
                                   ) ls1012))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1024, x1028)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1023 => cvtIDENT x1023
                                                          ) ls1024), cvtIDENT_EXPR x1028]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1035) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1035))
     | cvtLITERAL (LiteralContextualDecimalInteger s1038) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1038))
     | cvtLITERAL (LiteralContextualHexInteger s1041) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1041))
     | cvtLITERAL (LiteralDouble r1044) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1044))
     | cvtLITERAL (LiteralDecimal d1047) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1047))
     | cvtLITERAL (LiteralInt i1050) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1050))
     | cvtLITERAL (LiteralUInt u1053) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1053))
     | cvtLITERAL (LiteralBoolean b1056) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1056))
     | cvtLITERAL (LiteralString x1059) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1059))
     | cvtLITERAL (LiteralArray{exprs=ls1063, ty=opt1068}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1062 => 
                                                                         cvtEXPR x1062
                                                                  ) ls1063)), 
          ("ty", 
       (case opt1068 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1067 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1067))
       ))]))
     | cvtLITERAL (LiteralXML ls1080) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1079 => 
                                                                                                            cvtEXPR x1079
                                                                                                     ) ls1080)))
     | cvtLITERAL (LiteralNamespace x1086) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1086))
     | cvtLITERAL (LiteralObject{expr=ls1090, ty=opt1095}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1089 => 
                                                                        cvtFIELD x1089
                                                                 ) ls1090)), 
          ("ty", 
       (case opt1095 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1094 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1094))
       ))]))
     | cvtLITERAL (LiteralFunction x1106) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1106))
     | cvtLITERAL (LiteralRegExp{str=x1109}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1109)]))
   and cvtBLOCK (Block x1115) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1115))
   and cvtFIXTURE (NamespaceFixture x1118) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1118))
     | cvtFIXTURE (ClassFixture x1121) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1121))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1126) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1126))
     | cvtFIXTURE (MethodFixture{func=x1129, ty=x1130, readOnly=b1131, override=b1132, 
          final=b1133}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1129), ("ty", cvtTYPE_EXPR x1130), ("readOnly", PrettyRep.Bool b1131), 
          ("override", PrettyRep.Bool b1132), ("final", PrettyRep.Bool b1133)]))
     | cvtFIXTURE (ValFixture{ty=x1147, readOnly=b1148}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1147), ("readOnly", PrettyRep.Bool b1148)]))
     | cvtFIXTURE (VirtualValFixture x1156) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1156))
   and cvtBINDINGS (ls1160, ls1165) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1159 => 
                                                                                       cvtBINDING x1159
                                                                                ) ls1160), 
          PrettyRep.List (List.map (fn x1164 => cvtINIT_STEP x1164
                                   ) ls1165)]
   and cvtFIXTURES ls1173 = PrettyRep.List (List.map (fn (x1170, x1171) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1170, 
                                                            cvtFIXTURE x1171]
                                                     ) ls1173)
   and cvtINITS ls1180 = PrettyRep.List (List.map (fn (x1177, x1178) => PrettyRep.Tuple [cvtFIXTURE_NAME x1177, 
                                                         cvtEXPR x1178]
                                                  ) ls1180)
   and cvtHEAD (x1184, x1185) = PrettyRep.Tuple [cvtFIXTURES x1184, cvtINITS x1185]
   and cvtFIELD {kind=x1187, name=x1188, init=x1189} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1187), ("name", cvtIDENT_EXPR x1188), ("init", cvtEXPR x1189)]
   and cvtFIELD_TYPE {name=x1197, ty=x1198} = PrettyRep.Rec [("name", cvtIDENT x1197), 
          ("ty", cvtTYPE_EXPR x1198)]
   and cvtTYPED_IDENT {name=x1204, ty=opt1206} = PrettyRep.Rec [("name", cvtIDENT x1204), 
          ("ty", 
       (case opt1206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1205 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1205))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1216, params=ls1221, result=x1225, thisType=opt1227, 
          hasRest=b1231, minArgs=n1232} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1215 => 
                                                                                                        cvtIDENT x1215
                                                                                                 ) ls1216)), 
          ("params", PrettyRep.List (List.map (fn x1220 => cvtTYPE_EXPR x1220
                                              ) ls1221)), ("result", cvtTYPE_EXPR x1225), 
          ("thisType", 
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1226))
       )), ("hasRest", PrettyRep.Bool b1231), ("minArgs", PrettyRep.Int n1232)]
   and cvtFUNC_DEFN {kind=x1246, ns=opt1248, final=b1252, override=b1253, prototype=b1254, 
          static=b1255, func=x1256} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1246), 
          ("ns", 
       (case opt1248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1247 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1247))
       )), ("final", PrettyRep.Bool b1252), ("override", PrettyRep.Bool b1253), 
          ("prototype", PrettyRep.Bool b1254), ("static", PrettyRep.Bool b1255), 
          ("func", cvtFUNC x1256)]
   and cvtCTOR_DEFN x1272 = cvtCTOR x1272
   and cvtVAR_DEFN {kind=x1273, ns=opt1275, static=b1279, prototype=b1280, 
          bindings=x1281} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1273), 
          ("ns", 
       (case opt1275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1274 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1274))
       )), ("static", PrettyRep.Bool b1279), ("prototype", PrettyRep.Bool b1280), 
          ("bindings", cvtBINDINGS x1281)]
   and cvtNAMESPACE_DEFN {ident=x1293, ns=opt1295, init=opt1300} = PrettyRep.Rec [("ident", 
          cvtIDENT x1293), ("ns", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1294))
       )), ("init", 
       (case opt1300 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1299 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1299))
       ))]
   and cvtCLASS_DEFN {ident=x1311, ns=opt1313, nonnullable=b1317, dynamic=b1318, 
          final=b1319, params=ls1321, extends=opt1326, implements=ls1331, classDefns=ls1336, 
          instanceDefns=ls1341, instanceStmts=ls1346, ctorDefn=opt1351} = PrettyRep.Rec [("ident", 
          cvtIDENT x1311), ("ns", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1312))
       )), ("nonnullable", PrettyRep.Bool b1317), ("dynamic", PrettyRep.Bool b1318), 
          ("final", PrettyRep.Bool b1319), ("params", PrettyRep.List (List.map (fn x1320 => 
                                                                                      cvtIDENT x1320
                                                                               ) ls1321)), 
          ("extends", 
       (case opt1326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1325 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1325))
       )), ("implements", PrettyRep.List (List.map (fn x1330 => cvtIDENT_EXPR x1330
                                                   ) ls1331)), ("classDefns", 
          PrettyRep.List (List.map (fn x1335 => cvtDEFN x1335
                                   ) ls1336)), ("instanceDefns", PrettyRep.List (List.map (fn x1340 => 
                                                                                                 cvtDEFN x1340
                                                                                          ) ls1341)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1345 => cvtSTMT x1345
                                                     ) ls1346)), ("ctorDefn", 
          
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1350))
       ))]
   and cvtINTERFACE_DEFN {ident=x1380, ns=opt1382, nonnullable=b1386, params=ls1388, 
          extends=ls1393, block=x1397} = PrettyRep.Rec [("ident", cvtIDENT x1380), 
          ("ns", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1381))
       )), ("nonnullable", PrettyRep.Bool b1386), ("params", PrettyRep.List (List.map (fn x1387 => 
                                                                                             cvtIDENT x1387
                                                                                      ) ls1388)), 
          ("extends", PrettyRep.List (List.map (fn x1392 => cvtIDENT_EXPR x1392
                                               ) ls1393)), ("block", cvtBLOCK x1397)]
   and cvtTYPE_DEFN {ident=x1411, ns=opt1413, init=x1417} = PrettyRep.Rec [("ident", 
          cvtIDENT x1411), ("ns", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1412))
       )), ("init", cvtTYPE_EXPR x1417)]
   and cvtFOR_ENUM_STMT {isEach=b1425, defn=opt1427, obj=x1431, fixtures=opt1433, 
          next=x1437, labels=ls1439, body=x1443} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1425), ("defn", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1426))
       )), ("obj", cvtEXPR x1431), ("fixtures", 
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1432))
       )), ("next", cvtSTMT x1437), ("labels", PrettyRep.List (List.map (fn x1438 => 
                                                                               cvtIDENT x1438
                                                                        ) ls1439)), 
          ("body", cvtSTMT x1443)]
   and cvtFOR_STMT {fixtures=opt1460, defn=opt1465, init=ls1470, cond=x1474, 
          update=x1475, labels=ls1477, body=x1481} = PrettyRep.Rec [("fixtures", 
          
       (case opt1460 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1459))
       )), ("defn", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1464))
       )), ("init", PrettyRep.List (List.map (fn x1469 => cvtSTMT x1469
                                             ) ls1470)), ("cond", cvtEXPR x1474), 
          ("update", cvtEXPR x1475), ("labels", PrettyRep.List (List.map (fn x1476 => 
                                                                                cvtIDENT x1476
                                                                         ) ls1477)), 
          ("body", cvtSTMT x1481)]
   and cvtWHILE_STMT {cond=x1497, fixtures=opt1499, body=x1503, labels=ls1505} = 
          PrettyRep.Rec [("cond", cvtEXPR x1497), ("fixtures", 
       (case opt1499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1498 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1498))
       )), ("body", cvtSTMT x1503), ("labels", PrettyRep.List (List.map (fn x1504 => 
                                                                               cvtIDENT x1504
                                                                        ) ls1505))]
   and cvtDIRECTIVES {pragmas=ls1519, defns=ls1524, head=opt1529, body=ls1534, 
          pos=opt1539} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1518 => 
                                                                                    cvtPRAGMA x1518
                                                                             ) ls1519)), 
          ("defns", PrettyRep.List (List.map (fn x1523 => cvtDEFN x1523
                                             ) ls1524)), ("head", 
       (case opt1529 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1528 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1528))
       )), ("body", PrettyRep.List (List.map (fn x1533 => cvtSTMT x1533
                                             ) ls1534)), ("pos", 
       (case opt1539 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1538 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1538))
       ))]
   and cvtCASE {label=opt1555, inits=opt1560, body=x1564} = PrettyRep.Rec [("label", 
          
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1554))
       )), ("inits", 
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1559 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1559))
       )), ("body", cvtBLOCK x1564)]
   and cvtTYPE_CASE {ty=opt1573, body=x1577} = PrettyRep.Rec [("ty", 
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1572 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1572))
       )), ("body", cvtSTMT x1577)]
   and cvtCATCH_CLAUSE {bindings=x1583, ty=x1584, fixtures=opt1586, block=x1590} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1583), ("ty", cvtTYPE_EXPR x1584), 
          ("fixtures", 
       (case opt1586 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1585 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1585))
       )), ("block", cvtBLOCK x1590)]
   and cvtFUNC_NAME {kind=x1600, ident=x1601} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1600), 
          ("ident", cvtIDENT x1601)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1607, getter=opt1609, setter=opt1614} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1607), ("getter", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1608 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1608))
       )), ("setter", 
       (case opt1614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1613 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1613))
       ))]
   and cvtPACKAGE {name=ls1626, block=x1630} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1625 => 
                                                                                                       cvtIDENT x1625
                                                                                                ) ls1626)), 
          ("block", cvtBLOCK x1630)]
   and cvtPROGRAM {packages=ls1637, fixtures=opt1642, block=x1646} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1636 => cvtPACKAGE x1636
                                   ) ls1637)), ("fixtures", 
       (case opt1642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1641 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1641))
       )), ("block", cvtBLOCK x1646)]
end

