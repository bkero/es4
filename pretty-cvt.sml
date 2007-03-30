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
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1008) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1008))
     | cvtLITERAL (LiteralContextualDecimalInteger s1011) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1011))
     | cvtLITERAL (LiteralContextualHexInteger s1014) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1014))
     | cvtLITERAL (LiteralDouble r1017) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1017))
     | cvtLITERAL (LiteralDecimal d1020) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1020))
     | cvtLITERAL (LiteralInt i1023) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1023))
     | cvtLITERAL (LiteralUInt u1026) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1026))
     | cvtLITERAL (LiteralBoolean b1029) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1029))
     | cvtLITERAL (LiteralString x1032) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1032))
     | cvtLITERAL (LiteralArray{exprs=ls1036, ty=opt1041}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1035 => 
                                                                         cvtEXPR x1035
                                                                  ) ls1036)), 
          ("ty", 
       (case opt1041 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1040 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1040))
       ))]))
     | cvtLITERAL (LiteralXML ls1053) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1052 => 
                                                                                                            cvtEXPR x1052
                                                                                                     ) ls1053)))
     | cvtLITERAL (LiteralNamespace x1059) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1059))
     | cvtLITERAL (LiteralObject{expr=ls1063, ty=opt1068}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1062 => 
                                                                        cvtFIELD x1062
                                                                 ) ls1063)), 
          ("ty", 
       (case opt1068 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1067 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1067))
       ))]))
     | cvtLITERAL (LiteralFunction x1079) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1079))
     | cvtLITERAL (LiteralRegExp{str=x1082}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1082)]))
   and cvtBLOCK (Block x1088) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1088))
   and cvtFIXTURE (NamespaceFixture x1091) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1091))
     | cvtFIXTURE (ClassFixture x1094) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1094))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1099) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1099))
     | cvtFIXTURE (MethodFixture{func=x1102, ty=x1103, readOnly=b1104, override=b1105, 
          final=b1106}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1102), ("ty", cvtTYPE_EXPR x1103), ("readOnly", PrettyRep.Bool b1104), 
          ("override", PrettyRep.Bool b1105), ("final", PrettyRep.Bool b1106)]))
     | cvtFIXTURE (ValFixture{ty=x1120, readOnly=b1121}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1120), ("readOnly", PrettyRep.Bool b1121)]))
     | cvtFIXTURE (VirtualValFixture x1129) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1129))
   and cvtBINDINGS (ls1133, ls1138) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1132 => 
                                                                                       cvtBINDING x1132
                                                                                ) ls1133), 
          PrettyRep.List (List.map (fn x1137 => cvtINIT_STEP x1137
                                   ) ls1138)]
   and cvtFIXTURES ls1146 = PrettyRep.List (List.map (fn (x1143, x1144) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1143, 
                                                            cvtFIXTURE x1144]
                                                     ) ls1146)
   and cvtINITS ls1153 = PrettyRep.List (List.map (fn (x1150, x1151) => PrettyRep.Tuple [cvtFIXTURE_NAME x1150, 
                                                         cvtEXPR x1151]
                                                  ) ls1153)
   and cvtHEAD (x1157, x1158) = PrettyRep.Tuple [cvtFIXTURES x1157, cvtINITS x1158]
   and cvtFIELD {kind=x1160, name=x1161, init=x1162} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1160), ("name", cvtIDENT_EXPR x1161), ("init", cvtEXPR x1162)]
   and cvtFIELD_TYPE {name=x1170, ty=x1171} = PrettyRep.Rec [("name", cvtIDENT x1170), 
          ("ty", cvtTYPE_EXPR x1171)]
   and cvtTYPED_IDENT {name=x1177, ty=opt1179} = PrettyRep.Rec [("name", cvtIDENT x1177), 
          ("ty", 
       (case opt1179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1178 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1178))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1189, params=ls1194, result=x1198, thisType=opt1200, 
          hasRest=b1204, minArgs=n1205} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1188 => 
                                                                                                        cvtIDENT x1188
                                                                                                 ) ls1189)), 
          ("params", PrettyRep.List (List.map (fn x1193 => cvtTYPE_EXPR x1193
                                              ) ls1194)), ("result", cvtTYPE_EXPR x1198), 
          ("thisType", 
       (case opt1200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1199 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1199))
       )), ("hasRest", PrettyRep.Bool b1204), ("minArgs", PrettyRep.Int n1205)]
   and cvtFUNC_DEFN {kind=x1219, ns=opt1221, final=b1225, override=b1226, prototype=b1227, 
          static=b1228, func=x1229} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1219), 
          ("ns", 
       (case opt1221 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1220 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1220))
       )), ("final", PrettyRep.Bool b1225), ("override", PrettyRep.Bool b1226), 
          ("prototype", PrettyRep.Bool b1227), ("static", PrettyRep.Bool b1228), 
          ("func", cvtFUNC x1229)]
   and cvtCTOR_DEFN x1245 = cvtCTOR x1245
   and cvtVAR_DEFN {kind=x1246, ns=opt1248, static=b1252, prototype=b1253, 
          bindings=x1254} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1246), 
          ("ns", 
       (case opt1248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1247 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1247))
       )), ("static", PrettyRep.Bool b1252), ("prototype", PrettyRep.Bool b1253), 
          ("bindings", cvtBINDINGS x1254)]
   and cvtNAMESPACE_DEFN {ident=x1266, ns=opt1268, init=opt1273} = PrettyRep.Rec [("ident", 
          cvtIDENT x1266), ("ns", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1267))
       )), ("init", 
       (case opt1273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1272 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1272))
       ))]
   and cvtCLASS_DEFN {ident=x1284, ns=opt1286, nonnullable=b1290, dynamic=b1291, 
          final=b1292, params=ls1294, extends=opt1299, implements=ls1304, classDefns=ls1309, 
          instanceDefns=ls1314, instanceStmts=ls1319, ctorDefn=opt1324} = PrettyRep.Rec [("ident", 
          cvtIDENT x1284), ("ns", 
       (case opt1286 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1285 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1285))
       )), ("nonnullable", PrettyRep.Bool b1290), ("dynamic", PrettyRep.Bool b1291), 
          ("final", PrettyRep.Bool b1292), ("params", PrettyRep.List (List.map (fn x1293 => 
                                                                                      cvtIDENT x1293
                                                                               ) ls1294)), 
          ("extends", 
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1298))
       )), ("implements", PrettyRep.List (List.map (fn x1303 => cvtIDENT_EXPR x1303
                                                   ) ls1304)), ("classDefns", 
          PrettyRep.List (List.map (fn x1308 => cvtDEFN x1308
                                   ) ls1309)), ("instanceDefns", PrettyRep.List (List.map (fn x1313 => 
                                                                                                 cvtDEFN x1313
                                                                                          ) ls1314)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1318 => cvtSTMT x1318
                                                     ) ls1319)), ("ctorDefn", 
          
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1323))
       ))]
   and cvtINTERFACE_DEFN {ident=x1353, ns=opt1355, nonnullable=b1359, params=ls1361, 
          extends=ls1366, block=x1370} = PrettyRep.Rec [("ident", cvtIDENT x1353), 
          ("ns", 
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1354))
       )), ("nonnullable", PrettyRep.Bool b1359), ("params", PrettyRep.List (List.map (fn x1360 => 
                                                                                             cvtIDENT x1360
                                                                                      ) ls1361)), 
          ("extends", PrettyRep.List (List.map (fn x1365 => cvtIDENT_EXPR x1365
                                               ) ls1366)), ("block", cvtBLOCK x1370)]
   and cvtTYPE_DEFN {ident=x1384, ns=opt1386, init=x1390} = PrettyRep.Rec [("ident", 
          cvtIDENT x1384), ("ns", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1385))
       )), ("init", cvtTYPE_EXPR x1390)]
   and cvtFOR_ENUM_STMT {isEach=b1398, defn=opt1400, obj=x1404, fixtures=opt1406, 
          init=ls1411, labels=ls1416, body=x1420} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1398), ("defn", 
       (case opt1400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1399 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1399))
       )), ("obj", cvtEXPR x1404), ("fixtures", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1405 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1405))
       )), ("init", PrettyRep.List (List.map (fn x1410 => cvtSTMT x1410
                                             ) ls1411)), ("labels", PrettyRep.List (List.map (fn x1415 => 
                                                                                                    cvtIDENT x1415
                                                                                             ) ls1416)), 
          ("body", cvtSTMT x1420)]
   and cvtFOR_STMT {fixtures=opt1437, defn=opt1442, init=ls1447, cond=x1451, 
          update=x1452, labels=ls1454, body=x1458} = PrettyRep.Rec [("fixtures", 
          
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1436))
       )), ("defn", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1441 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1441))
       )), ("init", PrettyRep.List (List.map (fn x1446 => cvtSTMT x1446
                                             ) ls1447)), ("cond", cvtEXPR x1451), 
          ("update", cvtEXPR x1452), ("labels", PrettyRep.List (List.map (fn x1453 => 
                                                                                cvtIDENT x1453
                                                                         ) ls1454)), 
          ("body", cvtSTMT x1458)]
   and cvtWHILE_STMT {cond=x1474, fixtures=opt1476, body=x1480, labels=ls1482} = 
          PrettyRep.Rec [("cond", cvtEXPR x1474), ("fixtures", 
       (case opt1476 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1475 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1475))
       )), ("body", cvtSTMT x1480), ("labels", PrettyRep.List (List.map (fn x1481 => 
                                                                               cvtIDENT x1481
                                                                        ) ls1482))]
   and cvtDIRECTIVES {pragmas=ls1496, defns=ls1501, head=opt1506, body=ls1511, 
          pos=opt1516} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1495 => 
                                                                                    cvtPRAGMA x1495
                                                                             ) ls1496)), 
          ("defns", PrettyRep.List (List.map (fn x1500 => cvtDEFN x1500
                                             ) ls1501)), ("head", 
       (case opt1506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1505 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1505))
       )), ("body", PrettyRep.List (List.map (fn x1510 => cvtSTMT x1510
                                             ) ls1511)), ("pos", 
       (case opt1516 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1515 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1515))
       ))]
   and cvtCASE {label=opt1532, inits=opt1537, body=x1541} = PrettyRep.Rec [("label", 
          
       (case opt1532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1531 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1531))
       )), ("inits", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1536))
       )), ("body", cvtBLOCK x1541)]
   and cvtTYPE_CASE {ty=opt1550, bindings=x1554, inits=opt1556, body=x1560} = 
          PrettyRep.Rec [("ty", 
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1549))
       )), ("bindings", cvtBINDINGS x1554), ("inits", 
       (case opt1556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1555 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1555))
       )), ("body", cvtBLOCK x1560)]
   and cvtCATCH_CLAUSE {bindings=x1570, ty=x1571, fixtures=opt1573, block=x1577} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1570), ("ty", cvtTYPE_EXPR x1571), 
          ("fixtures", 
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1572 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1572))
       )), ("block", cvtBLOCK x1577)]
   and cvtFUNC_NAME {kind=x1587, ident=x1588} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1587), 
          ("ident", cvtIDENT x1588)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1594, getter=opt1596, setter=opt1601} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1594), ("getter", 
       (case opt1596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1595 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1595))
       )), ("setter", 
       (case opt1601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1600 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1600))
       ))]
   and cvtPACKAGE {name=ls1613, block=x1617} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1612 => 
                                                                                                       cvtIDENT x1612
                                                                                                ) ls1613)), 
          ("block", cvtBLOCK x1617)]
   and cvtPROGRAM {packages=ls1624, fixtures=opt1629, block=x1633} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1623 => cvtPACKAGE x1623
                                   ) ls1624)), ("fixtures", 
       (case opt1629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1628 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1628))
       )), ("block", cvtBLOCK x1633)]
end

