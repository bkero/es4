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
   and cvtEXPR (TrinaryExpr(x781, x782, x783, x784)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x781, cvtEXPR x782, cvtEXPR x783, 
          cvtEXPR x784]))
     | cvtEXPR (BinaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryTypeExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x794, cvtEXPR x795, cvtTYPE_EXPR x796]))
     | cvtEXPR (UnaryExpr(x800, x801)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x800, 
          cvtEXPR x801]))
     | cvtEXPR (TypeExpr x805) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x805))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt810) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x809))
       ))
     | cvtEXPR (SuperExpr opt817) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt817 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x816 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x816))
       ))
     | cvtEXPR (LiteralExpr x823) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x823))
     | cvtEXPR (CallExpr{func=x826, actuals=ls828}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x826), ("actuals", PrettyRep.List (List.map (fn x827 => 
                                                                                                   cvtEXPR x827
                                                                                            ) ls828))]))
     | cvtEXPR (ApplyTypeExpr{expr=x839, actuals=ls841}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x839), ("actuals", PrettyRep.List (List.map (fn x840 => 
                                                                                                   cvtTYPE_EXPR x840
                                                                                            ) ls841))]))
     | cvtEXPR (LetExpr{defs=x852, body=x853, head=opt855}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x852), ("body", cvtEXPR x853), 
          ("head", 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x854))
       ))]))
     | cvtEXPR (NewExpr{obj=x868, actuals=ls870}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x868), ("actuals", PrettyRep.List (List.map (fn x869 => 
                                                                                                  cvtEXPR x869
                                                                                           ) ls870))]))
     | cvtEXPR (ObjectRef{base=x881, ident=x882, pos=opt884}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x881), ("ident", cvtIDENT_EXPR x882), 
          ("pos", 
       (case opt884 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x883 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x883))
       ))]))
     | cvtEXPR (LexicalRef{ident=x897, pos=opt899}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x897), ("pos", 
       (case opt899 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x898 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x898))
       ))]))
     | cvtEXPR (SetExpr(x910, x911, x912)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x910, 
          cvtEXPR x911, cvtEXPR x912]))
     | cvtEXPR (ListExpr ls917) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x916 => 
                                                                                                    cvtEXPR x916
                                                                                             ) ls917)))
     | cvtEXPR (InitExpr(x923, x924, x925)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x923, 
          cvtHEAD x924, cvtINITS x925]))
     | cvtEXPR (SliceExpr(x929, x930, x931)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x929, cvtEXPR x930, cvtEXPR x931]))
     | cvtEXPR (GetTemp n935) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n935))
     | cvtEXPR (GetParam n938) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n938))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n944) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n944))
     | cvtFIXTURE_NAME (PropName x947) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x947))
   and cvtIDENT_EXPR (Identifier{ident=x950, openNamespaces=ls956}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x950), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls952 => PrettyRep.List (List.map (fn x951 => 
                                                                                cvtNAMESPACE x951
                                                                         ) ls952)
                                   ) ls956))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x967, expr=x968}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x967), ("expr", cvtEXPR x968)]))
     | cvtIDENT_EXPR (AttributeIdentifier x976) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x976))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x979, openNamespaces=ls985}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x979), ("openNamespaces", PrettyRep.List (List.map (fn ls981 => 
                                                                            PrettyRep.List (List.map (fn x980 => 
                                                                                                            cvtNAMESPACE x980
                                                                                                     ) ls981)
                                                                     ) ls985))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x996, ident=x997}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x996), ("ident", cvtUSTRING x997)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1005, typeArgs=ls1007}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1005), ("typeArgs", 
          PrettyRep.List (List.map (fn x1006 => cvtTYPE_EXPR x1006
                                   ) ls1007))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1019, x1023)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1018 => cvtIDENT x1018
                                                          ) ls1019), cvtIDENT_EXPR x1023]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1030) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1030))
     | cvtLITERAL (LiteralContextualDecimalInteger s1033) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1033))
     | cvtLITERAL (LiteralContextualHexInteger s1036) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1036))
     | cvtLITERAL (LiteralDouble r1039) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1039))
     | cvtLITERAL (LiteralDecimal d1042) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1042))
     | cvtLITERAL (LiteralInt i1045) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1045))
     | cvtLITERAL (LiteralUInt u1048) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1048))
     | cvtLITERAL (LiteralBoolean b1051) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1051))
     | cvtLITERAL (LiteralString x1054) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1054))
     | cvtLITERAL (LiteralArray{exprs=ls1058, ty=opt1063}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1057 => 
                                                                         cvtEXPR x1057
                                                                  ) ls1058)), 
          ("ty", 
       (case opt1063 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1062 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1062))
       ))]))
     | cvtLITERAL (LiteralXML ls1075) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1074 => 
                                                                                                            cvtEXPR x1074
                                                                                                     ) ls1075)))
     | cvtLITERAL (LiteralNamespace x1081) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1081))
     | cvtLITERAL (LiteralObject{expr=ls1085, ty=opt1090}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1084 => 
                                                                        cvtFIELD x1084
                                                                 ) ls1085)), 
          ("ty", 
       (case opt1090 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1089 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1089))
       ))]))
     | cvtLITERAL (LiteralFunction x1101) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1101))
     | cvtLITERAL (LiteralRegExp{str=x1104}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1104)]))
   and cvtBLOCK (Block x1110) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1110))
   and cvtFIXTURE (NamespaceFixture x1113) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1113))
     | cvtFIXTURE (ClassFixture x1116) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1116))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1121) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1121))
     | cvtFIXTURE (MethodFixture{func=x1124, ty=x1125, readOnly=b1126, override=b1127, 
          final=b1128}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1124), ("ty", cvtTYPE_EXPR x1125), ("readOnly", PrettyRep.Bool b1126), 
          ("override", PrettyRep.Bool b1127), ("final", PrettyRep.Bool b1128)]))
     | cvtFIXTURE (ValFixture{ty=x1142, readOnly=b1143}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1142), ("readOnly", PrettyRep.Bool b1143)]))
     | cvtFIXTURE (VirtualValFixture x1151) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1151))
   and cvtBINDINGS (ls1155, ls1160) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1154 => 
                                                                                       cvtBINDING x1154
                                                                                ) ls1155), 
          PrettyRep.List (List.map (fn x1159 => cvtINIT_STEP x1159
                                   ) ls1160)]
   and cvtFIXTURES ls1168 = PrettyRep.List (List.map (fn (x1165, x1166) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1165, 
                                                            cvtFIXTURE x1166]
                                                     ) ls1168)
   and cvtINITS ls1175 = PrettyRep.List (List.map (fn (x1172, x1173) => PrettyRep.Tuple [cvtFIXTURE_NAME x1172, 
                                                         cvtEXPR x1173]
                                                  ) ls1175)
   and cvtHEAD (x1179, x1180) = PrettyRep.Tuple [cvtFIXTURES x1179, cvtINITS x1180]
   and cvtFIELD {kind=x1182, name=x1183, init=x1184} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1182), ("name", cvtIDENT_EXPR x1183), ("init", cvtEXPR x1184)]
   and cvtFIELD_TYPE {name=x1192, ty=x1193} = PrettyRep.Rec [("name", cvtIDENT x1192), 
          ("ty", cvtTYPE_EXPR x1193)]
   and cvtTYPED_IDENT {name=x1199, ty=opt1201} = PrettyRep.Rec [("name", cvtIDENT x1199), 
          ("ty", 
       (case opt1201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1200 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1200))
       ))]
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
          init=ls1433, next=ls1438, labels=ls1443, body=x1447} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1420), ("defn", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1421))
       )), ("obj", cvtEXPR x1426), ("fixtures", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1427 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1427))
       )), ("init", PrettyRep.List (List.map (fn x1432 => cvtSTMT x1432
                                             ) ls1433)), ("next", PrettyRep.List (List.map (fn x1437 => 
                                                                                                  cvtSTMT x1437
                                                                                           ) ls1438)), 
          ("labels", PrettyRep.List (List.map (fn x1442 => cvtIDENT x1442
                                              ) ls1443)), ("body", cvtSTMT x1447)]
   and cvtFOR_STMT {fixtures=opt1466, defn=opt1471, init=ls1476, cond=x1480, 
          update=x1481, labels=ls1483, body=x1487} = PrettyRep.Rec [("fixtures", 
          
       (case opt1466 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1465 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1465))
       )), ("defn", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1470))
       )), ("init", PrettyRep.List (List.map (fn x1475 => cvtSTMT x1475
                                             ) ls1476)), ("cond", cvtEXPR x1480), 
          ("update", cvtEXPR x1481), ("labels", PrettyRep.List (List.map (fn x1482 => 
                                                                                cvtIDENT x1482
                                                                         ) ls1483)), 
          ("body", cvtSTMT x1487)]
   and cvtWHILE_STMT {cond=x1503, fixtures=opt1505, body=x1509, labels=ls1511} = 
          PrettyRep.Rec [("cond", cvtEXPR x1503), ("fixtures", 
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1504 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1504))
       )), ("body", cvtSTMT x1509), ("labels", PrettyRep.List (List.map (fn x1510 => 
                                                                               cvtIDENT x1510
                                                                        ) ls1511))]
   and cvtDIRECTIVES {pragmas=ls1525, defns=ls1530, head=opt1535, body=ls1540, 
          pos=opt1545} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1524 => 
                                                                                    cvtPRAGMA x1524
                                                                             ) ls1525)), 
          ("defns", PrettyRep.List (List.map (fn x1529 => cvtDEFN x1529
                                             ) ls1530)), ("head", 
       (case opt1535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1534 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1534))
       )), ("body", PrettyRep.List (List.map (fn x1539 => cvtSTMT x1539
                                             ) ls1540)), ("pos", 
       (case opt1545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1544 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1544))
       ))]
   and cvtCASE {label=opt1561, inits=opt1566, body=x1570} = PrettyRep.Rec [("label", 
          
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1560))
       )), ("inits", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1565))
       )), ("body", cvtBLOCK x1570)]
   and cvtTYPE_CASE {ty=opt1579, bindings=x1583, inits=opt1585, body=x1589} = 
          PrettyRep.Rec [("ty", 
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1578 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1578))
       )), ("bindings", cvtBINDINGS x1583), ("inits", 
       (case opt1585 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1584 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1584))
       )), ("body", cvtBLOCK x1589)]
   and cvtCATCH_CLAUSE {bindings=x1599, ty=x1600, fixtures=opt1602, block=x1606} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1599), ("ty", cvtTYPE_EXPR x1600), 
          ("fixtures", 
       (case opt1602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1601 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1601))
       )), ("block", cvtBLOCK x1606)]
   and cvtFUNC_NAME {kind=x1616, ident=x1617} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1616), 
          ("ident", cvtIDENT x1617)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1623, getter=opt1625, setter=opt1630} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1623), ("getter", 
       (case opt1625 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1624 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1624))
       )), ("setter", 
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1629 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1629))
       ))]
   and cvtPACKAGE {name=ls1642, block=x1646} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1641 => 
                                                                                                       cvtIDENT x1641
                                                                                                ) ls1642)), 
          ("block", cvtBLOCK x1646)]
   and cvtPROGRAM {packages=ls1653, fixtures=opt1658, block=x1662} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1652 => cvtPACKAGE x1652
                                   ) ls1653)), ("fixtures", 
       (case opt1658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1657 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1657))
       )), ("block", cvtBLOCK x1662)]
end

