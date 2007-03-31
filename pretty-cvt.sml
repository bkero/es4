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
     | cvtNAMESPACE (Imported(x29, x30)) = PrettyRep.Ctor ("Imported", SOME (PrettyRep.Tuple [cvtIDENT x29, 
          cvtIDENT x30]))
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
     | cvtSTMT (SwitchStmt{mode=opt733, cond=x737, cases=ls739}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x732))
       )), ("cond", cvtEXPR x737), ("cases", PrettyRep.List (List.map (fn x738 => 
                                                                             cvtCASE x738
                                                                      ) ls739))]))
     | cvtSTMT (SwitchTypeStmt{cond=x752, ty=x753, cases=ls755}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x752), ("ty", cvtTYPE_EXPR x753), 
          ("cases", PrettyRep.List (List.map (fn x754 => cvtTYPE_CASE x754
                                             ) ls755))]))
     | cvtSTMT (Dxns{expr=x768}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x768)]))
   and cvtEXPR (TrinaryExpr(x774, x775, x776, x777)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x774, cvtEXPR x775, cvtEXPR x776, 
          cvtEXPR x777]))
     | cvtEXPR (BinaryExpr(x781, x782, x783)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x781, cvtEXPR x782, cvtEXPR x783]))
     | cvtEXPR (BinaryTypeExpr(x787, x788, x789)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x787, cvtEXPR x788, cvtTYPE_EXPR x789]))
     | cvtEXPR (UnaryExpr(x793, x794)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x793, 
          cvtEXPR x794]))
     | cvtEXPR (TypeExpr x798) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x798))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt803) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt803 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x802 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x802))
       ))
     | cvtEXPR (SuperExpr opt810) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x809))
       ))
     | cvtEXPR (LiteralExpr x816) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x816))
     | cvtEXPR (CallExpr{func=x819, actuals=ls821}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x819), ("actuals", PrettyRep.List (List.map (fn x820 => 
                                                                                                   cvtEXPR x820
                                                                                            ) ls821))]))
     | cvtEXPR (ApplyTypeExpr{expr=x832, actuals=ls834}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x832), ("actuals", PrettyRep.List (List.map (fn x833 => 
                                                                                                   cvtTYPE_EXPR x833
                                                                                            ) ls834))]))
     | cvtEXPR (LetExpr{defs=x845, body=x846, head=opt848}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x845), ("body", cvtEXPR x846), 
          ("head", 
       (case opt848 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x847 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x847))
       ))]))
     | cvtEXPR (NewExpr{obj=x861, actuals=ls863}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x861), ("actuals", PrettyRep.List (List.map (fn x862 => 
                                                                                                  cvtEXPR x862
                                                                                           ) ls863))]))
     | cvtEXPR (ObjectRef{base=x874, ident=x875, pos=opt877}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x874), ("ident", cvtIDENT_EXPR x875), 
          ("pos", 
       (case opt877 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x876 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x876))
       ))]))
     | cvtEXPR (LexicalRef{ident=x890, pos=opt892}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x890), ("pos", 
       (case opt892 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x891 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x891))
       ))]))
     | cvtEXPR (SetExpr(x903, x904, x905)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x903, 
          cvtEXPR x904, cvtEXPR x905]))
     | cvtEXPR (ListExpr ls910) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x909 => 
                                                                                                    cvtEXPR x909
                                                                                             ) ls910)))
     | cvtEXPR (InitExpr(x916, x917, x918)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x916, 
          cvtHEAD x917, cvtINITS x918]))
     | cvtEXPR (SliceExpr(x922, x923, x924)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x922, cvtEXPR x923, cvtEXPR x924]))
     | cvtEXPR (GetTemp n928) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n928))
     | cvtEXPR (GetParam n931) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n931))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n937) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n937))
     | cvtFIXTURE_NAME (PropName x940) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x940))
   and cvtIDENT_EXPR (Identifier{ident=x943, openNamespaces=ls949}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x943), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls945 => PrettyRep.List (List.map (fn x944 => 
                                                                                cvtNAMESPACE x944
                                                                         ) ls945)
                                   ) ls949))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x960, expr=x961}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x960), ("expr", cvtEXPR x961)]))
     | cvtIDENT_EXPR (AttributeIdentifier x969) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x969))
     | cvtIDENT_EXPR (ExpressionIdentifier x972) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x972))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x975, ident=x976}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x975), ("ident", cvtUSTRING x976)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x984, typeArgs=ls986}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x984), ("typeArgs", 
          PrettyRep.List (List.map (fn x985 => cvtTYPE_EXPR x985
                                   ) ls986))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls998, x1002)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x997 => cvtIDENT x997
                                                          ) ls998), cvtIDENT_EXPR x1002]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1009) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1009))
     | cvtLITERAL (LiteralContextualDecimalInteger s1012) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1012))
     | cvtLITERAL (LiteralContextualHexInteger s1015) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1015))
     | cvtLITERAL (LiteralDouble r1018) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1018))
     | cvtLITERAL (LiteralDecimal d1021) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1021))
     | cvtLITERAL (LiteralInt i1024) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1024))
     | cvtLITERAL (LiteralUInt u1027) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1027))
     | cvtLITERAL (LiteralBoolean b1030) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1030))
     | cvtLITERAL (LiteralString x1033) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1033))
     | cvtLITERAL (LiteralArray{exprs=ls1037, ty=opt1042}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1036 => 
                                                                         cvtEXPR x1036
                                                                  ) ls1037)), 
          ("ty", 
       (case opt1042 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1041 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1041))
       ))]))
     | cvtLITERAL (LiteralXML ls1054) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1053 => 
                                                                                                            cvtEXPR x1053
                                                                                                     ) ls1054)))
     | cvtLITERAL (LiteralNamespace x1060) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1060))
     | cvtLITERAL (LiteralObject{expr=ls1064, ty=opt1069}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1063 => 
                                                                        cvtFIELD x1063
                                                                 ) ls1064)), 
          ("ty", 
       (case opt1069 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1068 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1068))
       ))]))
     | cvtLITERAL (LiteralFunction x1080) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1080))
     | cvtLITERAL (LiteralRegExp{str=x1083}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1083)]))
   and cvtBLOCK (Block x1089) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1089))
   and cvtFIXTURE (NamespaceFixture x1092) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1092))
     | cvtFIXTURE (ClassFixture x1095) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1095))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1100) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1100))
     | cvtFIXTURE (MethodFixture{func=x1103, ty=x1104, readOnly=b1105, override=b1106, 
          final=b1107}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1103), ("ty", cvtTYPE_EXPR x1104), ("readOnly", PrettyRep.Bool b1105), 
          ("override", PrettyRep.Bool b1106), ("final", PrettyRep.Bool b1107)]))
     | cvtFIXTURE (ValFixture{ty=x1121, readOnly=b1122}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1121), ("readOnly", PrettyRep.Bool b1122)]))
     | cvtFIXTURE (VirtualValFixture x1130) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1130))
   and cvtBINDINGS (ls1134, ls1139) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1133 => 
                                                                                       cvtBINDING x1133
                                                                                ) ls1134), 
          PrettyRep.List (List.map (fn x1138 => cvtINIT_STEP x1138
                                   ) ls1139)]
   and cvtFIXTURES ls1147 = PrettyRep.List (List.map (fn (x1144, x1145) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1144, 
                                                            cvtFIXTURE x1145]
                                                     ) ls1147)
   and cvtINITS ls1154 = PrettyRep.List (List.map (fn (x1151, x1152) => PrettyRep.Tuple [cvtFIXTURE_NAME x1151, 
                                                         cvtEXPR x1152]
                                                  ) ls1154)
   and cvtHEAD (x1158, x1159) = PrettyRep.Tuple [cvtFIXTURES x1158, cvtINITS x1159]
   and cvtFIELD {kind=x1161, name=x1162, init=x1163} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1161), ("name", cvtIDENT_EXPR x1162), ("init", cvtEXPR x1163)]
   and cvtFIELD_TYPE {name=x1171, ty=x1172} = PrettyRep.Rec [("name", cvtIDENT x1171), 
          ("ty", cvtTYPE_EXPR x1172)]
   and cvtTYPED_IDENT {name=x1178, ty=opt1180} = PrettyRep.Rec [("name", cvtIDENT x1178), 
          ("ty", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1179))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1190, params=ls1195, result=x1199, thisType=opt1201, 
          hasRest=b1205, minArgs=n1206} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1189 => 
                                                                                                        cvtIDENT x1189
                                                                                                 ) ls1190)), 
          ("params", PrettyRep.List (List.map (fn x1194 => cvtTYPE_EXPR x1194
                                              ) ls1195)), ("result", cvtTYPE_EXPR x1199), 
          ("thisType", 
       (case opt1201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1200 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1200))
       )), ("hasRest", PrettyRep.Bool b1205), ("minArgs", PrettyRep.Int n1206)]
   and cvtFUNC_DEFN {kind=x1220, ns=opt1222, final=b1226, override=b1227, prototype=b1228, 
          static=b1229, func=x1230} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1220), 
          ("ns", 
       (case opt1222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1221))
       )), ("final", PrettyRep.Bool b1226), ("override", PrettyRep.Bool b1227), 
          ("prototype", PrettyRep.Bool b1228), ("static", PrettyRep.Bool b1229), 
          ("func", cvtFUNC x1230)]
   and cvtCTOR_DEFN x1246 = cvtCTOR x1246
   and cvtVAR_DEFN {kind=x1247, ns=opt1249, static=b1253, prototype=b1254, 
          bindings=x1255} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1247), 
          ("ns", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1248))
       )), ("static", PrettyRep.Bool b1253), ("prototype", PrettyRep.Bool b1254), 
          ("bindings", cvtBINDINGS x1255)]
   and cvtNAMESPACE_DEFN {ident=x1267, ns=opt1269, init=opt1274} = PrettyRep.Rec [("ident", 
          cvtIDENT x1267), ("ns", 
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1268))
       )), ("init", 
       (case opt1274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1273 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1273))
       ))]
   and cvtCLASS_DEFN {ident=x1285, ns=opt1287, nonnullable=b1291, dynamic=b1292, 
          final=b1293, params=ls1295, extends=opt1300, implements=ls1305, classDefns=ls1310, 
          instanceDefns=ls1315, instanceStmts=ls1320, ctorDefn=opt1325} = PrettyRep.Rec [("ident", 
          cvtIDENT x1285), ("ns", 
       (case opt1287 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1286 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1286))
       )), ("nonnullable", PrettyRep.Bool b1291), ("dynamic", PrettyRep.Bool b1292), 
          ("final", PrettyRep.Bool b1293), ("params", PrettyRep.List (List.map (fn x1294 => 
                                                                                      cvtIDENT x1294
                                                                               ) ls1295)), 
          ("extends", 
       (case opt1300 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1299 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1299))
       )), ("implements", PrettyRep.List (List.map (fn x1304 => cvtIDENT_EXPR x1304
                                                   ) ls1305)), ("classDefns", 
          PrettyRep.List (List.map (fn x1309 => cvtDEFN x1309
                                   ) ls1310)), ("instanceDefns", PrettyRep.List (List.map (fn x1314 => 
                                                                                                 cvtDEFN x1314
                                                                                          ) ls1315)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1319 => cvtSTMT x1319
                                                     ) ls1320)), ("ctorDefn", 
          
       (case opt1325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1324 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1324))
       ))]
   and cvtINTERFACE_DEFN {ident=x1354, ns=opt1356, nonnullable=b1360, params=ls1362, 
          extends=ls1367, block=x1371} = PrettyRep.Rec [("ident", cvtIDENT x1354), 
          ("ns", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1355))
       )), ("nonnullable", PrettyRep.Bool b1360), ("params", PrettyRep.List (List.map (fn x1361 => 
                                                                                             cvtIDENT x1361
                                                                                      ) ls1362)), 
          ("extends", PrettyRep.List (List.map (fn x1366 => cvtIDENT_EXPR x1366
                                               ) ls1367)), ("block", cvtBLOCK x1371)]
   and cvtTYPE_DEFN {ident=x1385, ns=opt1387, init=x1391} = PrettyRep.Rec [("ident", 
          cvtIDENT x1385), ("ns", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1386 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1386))
       )), ("init", cvtTYPE_EXPR x1391)]
   and cvtFOR_ENUM_STMT {isEach=b1399, defn=opt1401, obj=x1405, fixtures=opt1407, 
          init=ls1412, labels=ls1417, body=x1421} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1399), ("defn", 
       (case opt1401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1400 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1400))
       )), ("obj", cvtEXPR x1405), ("fixtures", 
       (case opt1407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1406 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1406))
       )), ("init", PrettyRep.List (List.map (fn x1411 => cvtSTMT x1411
                                             ) ls1412)), ("labels", PrettyRep.List (List.map (fn x1416 => 
                                                                                                    cvtIDENT x1416
                                                                                             ) ls1417)), 
          ("body", cvtSTMT x1421)]
   and cvtFOR_STMT {fixtures=opt1438, defn=opt1443, init=ls1448, cond=x1452, 
          update=x1453, labels=ls1455, body=x1459} = PrettyRep.Rec [("fixtures", 
          
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1437))
       )), ("defn", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1442 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1442))
       )), ("init", PrettyRep.List (List.map (fn x1447 => cvtSTMT x1447
                                             ) ls1448)), ("cond", cvtEXPR x1452), 
          ("update", cvtEXPR x1453), ("labels", PrettyRep.List (List.map (fn x1454 => 
                                                                                cvtIDENT x1454
                                                                         ) ls1455)), 
          ("body", cvtSTMT x1459)]
   and cvtWHILE_STMT {cond=x1475, fixtures=opt1477, body=x1481, labels=ls1483} = 
          PrettyRep.Rec [("cond", cvtEXPR x1475), ("fixtures", 
       (case opt1477 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1476 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1476))
       )), ("body", cvtSTMT x1481), ("labels", PrettyRep.List (List.map (fn x1482 => 
                                                                               cvtIDENT x1482
                                                                        ) ls1483))]
   and cvtDIRECTIVES {pragmas=ls1497, defns=ls1502, head=opt1507, body=ls1512, 
          pos=opt1517} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1496 => 
                                                                                    cvtPRAGMA x1496
                                                                             ) ls1497)), 
          ("defns", PrettyRep.List (List.map (fn x1501 => cvtDEFN x1501
                                             ) ls1502)), ("head", 
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1506 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1506))
       )), ("body", PrettyRep.List (List.map (fn x1511 => cvtSTMT x1511
                                             ) ls1512)), ("pos", 
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1516 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1516))
       ))]
   and cvtCASE {label=opt1533, inits=opt1538, body=x1542} = PrettyRep.Rec [("label", 
          
       (case opt1533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1532 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1532))
       )), ("inits", 
       (case opt1538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1537 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1537))
       )), ("body", cvtBLOCK x1542)]
   and cvtTYPE_CASE {ty=opt1551, bindings=x1555, inits=opt1557, body=x1561} = 
          PrettyRep.Rec [("ty", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1550))
       )), ("bindings", cvtBINDINGS x1555), ("inits", 
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1556))
       )), ("body", cvtBLOCK x1561)]
   and cvtCATCH_CLAUSE {bindings=x1571, ty=x1572, fixtures=opt1574, block=x1578} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1571), ("ty", cvtTYPE_EXPR x1572), 
          ("fixtures", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1573))
       )), ("block", cvtBLOCK x1578)]
   and cvtFUNC_NAME {kind=x1588, ident=x1589} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1588), 
          ("ident", cvtIDENT x1589)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1595, getter=opt1597, setter=opt1602} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1595), ("getter", 
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1596 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1596))
       )), ("setter", 
       (case opt1602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1601 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1601))
       ))]
   and cvtPACKAGE {name=ls1614, block=x1618} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1613 => 
                                                                                                       cvtIDENT x1613
                                                                                                ) ls1614)), 
          ("block", cvtBLOCK x1618)]
   and cvtPROGRAM {packages=ls1625, fixtures=opt1630, block=x1634} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1624 => cvtPACKAGE x1624
                                   ) ls1625)), ("fixtures", 
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1629 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1629))
       )), ("block", cvtBLOCK x1634)]
end

