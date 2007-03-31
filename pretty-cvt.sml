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
     | cvtIDENT_EXPR (ExpressionIdentifier x979) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x979))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x982, ident=x983}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x982), ("ident", cvtUSTRING x983)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x991, typeArgs=ls993}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x991), ("typeArgs", 
          PrettyRep.List (List.map (fn x992 => cvtTYPE_EXPR x992
                                   ) ls993))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1005, x1009)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1004 => cvtIDENT x1004
                                                          ) ls1005), cvtIDENT_EXPR x1009]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1016) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1016))
     | cvtLITERAL (LiteralContextualDecimalInteger s1019) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1019))
     | cvtLITERAL (LiteralContextualHexInteger s1022) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1022))
     | cvtLITERAL (LiteralDouble r1025) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1025))
     | cvtLITERAL (LiteralDecimal d1028) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1028))
     | cvtLITERAL (LiteralInt i1031) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1031))
     | cvtLITERAL (LiteralUInt u1034) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1034))
     | cvtLITERAL (LiteralBoolean b1037) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1037))
     | cvtLITERAL (LiteralString x1040) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1040))
     | cvtLITERAL (LiteralArray{exprs=ls1044, ty=opt1049}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1043 => 
                                                                         cvtEXPR x1043
                                                                  ) ls1044)), 
          ("ty", 
       (case opt1049 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1048 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1048))
       ))]))
     | cvtLITERAL (LiteralXML ls1061) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1060 => 
                                                                                                            cvtEXPR x1060
                                                                                                     ) ls1061)))
     | cvtLITERAL (LiteralNamespace x1067) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1067))
     | cvtLITERAL (LiteralObject{expr=ls1071, ty=opt1076}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1070 => 
                                                                        cvtFIELD x1070
                                                                 ) ls1071)), 
          ("ty", 
       (case opt1076 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1075 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1075))
       ))]))
     | cvtLITERAL (LiteralFunction x1087) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1087))
     | cvtLITERAL (LiteralRegExp{str=x1090}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1090)]))
   and cvtBLOCK (Block x1096) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1096))
   and cvtFIXTURE (NamespaceFixture x1099) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1099))
     | cvtFIXTURE (ClassFixture x1102) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1102))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1107) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1107))
     | cvtFIXTURE (MethodFixture{func=x1110, ty=x1111, readOnly=b1112, override=b1113, 
          final=b1114}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1110), ("ty", cvtTYPE_EXPR x1111), ("readOnly", PrettyRep.Bool b1112), 
          ("override", PrettyRep.Bool b1113), ("final", PrettyRep.Bool b1114)]))
     | cvtFIXTURE (ValFixture{ty=x1128, readOnly=b1129}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1128), ("readOnly", PrettyRep.Bool b1129)]))
     | cvtFIXTURE (VirtualValFixture x1137) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1137))
   and cvtBINDINGS (ls1141, ls1146) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1140 => 
                                                                                       cvtBINDING x1140
                                                                                ) ls1141), 
          PrettyRep.List (List.map (fn x1145 => cvtINIT_STEP x1145
                                   ) ls1146)]
   and cvtFIXTURES ls1154 = PrettyRep.List (List.map (fn (x1151, x1152) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1151, 
                                                            cvtFIXTURE x1152]
                                                     ) ls1154)
   and cvtINITS ls1161 = PrettyRep.List (List.map (fn (x1158, x1159) => PrettyRep.Tuple [cvtFIXTURE_NAME x1158, 
                                                         cvtEXPR x1159]
                                                  ) ls1161)
   and cvtHEAD (x1165, x1166) = PrettyRep.Tuple [cvtFIXTURES x1165, cvtINITS x1166]
   and cvtFIELD {kind=x1168, name=x1169, init=x1170} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1168), ("name", cvtIDENT_EXPR x1169), ("init", cvtEXPR x1170)]
   and cvtFIELD_TYPE {name=x1178, ty=x1179} = PrettyRep.Rec [("name", cvtIDENT x1178), 
          ("ty", cvtTYPE_EXPR x1179)]
   and cvtTYPED_IDENT {name=x1185, ty=opt1187} = PrettyRep.Rec [("name", cvtIDENT x1185), 
          ("ty", 
       (case opt1187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1186 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1186))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1197, params=ls1202, result=x1206, thisType=opt1208, 
          hasRest=b1212, minArgs=n1213} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1196 => 
                                                                                                        cvtIDENT x1196
                                                                                                 ) ls1197)), 
          ("params", PrettyRep.List (List.map (fn x1201 => cvtTYPE_EXPR x1201
                                              ) ls1202)), ("result", cvtTYPE_EXPR x1206), 
          ("thisType", 
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1207))
       )), ("hasRest", PrettyRep.Bool b1212), ("minArgs", PrettyRep.Int n1213)]
   and cvtFUNC_DEFN {kind=x1227, ns=opt1229, final=b1233, override=b1234, prototype=b1235, 
          static=b1236, func=x1237} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1227), 
          ("ns", 
       (case opt1229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1228 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1228))
       )), ("final", PrettyRep.Bool b1233), ("override", PrettyRep.Bool b1234), 
          ("prototype", PrettyRep.Bool b1235), ("static", PrettyRep.Bool b1236), 
          ("func", cvtFUNC x1237)]
   and cvtCTOR_DEFN x1253 = cvtCTOR x1253
   and cvtVAR_DEFN {kind=x1254, ns=opt1256, static=b1260, prototype=b1261, 
          bindings=x1262} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1254), 
          ("ns", 
       (case opt1256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1255 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1255))
       )), ("static", PrettyRep.Bool b1260), ("prototype", PrettyRep.Bool b1261), 
          ("bindings", cvtBINDINGS x1262)]
   and cvtNAMESPACE_DEFN {ident=x1274, ns=opt1276, init=opt1281} = PrettyRep.Rec [("ident", 
          cvtIDENT x1274), ("ns", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1275))
       )), ("init", 
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1280))
       ))]
   and cvtCLASS_DEFN {ident=x1292, ns=opt1294, nonnullable=b1298, dynamic=b1299, 
          final=b1300, params=ls1302, extends=opt1307, implements=ls1312, classDefns=ls1317, 
          instanceDefns=ls1322, instanceStmts=ls1327, ctorDefn=opt1332} = PrettyRep.Rec [("ident", 
          cvtIDENT x1292), ("ns", 
       (case opt1294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1293 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1293))
       )), ("nonnullable", PrettyRep.Bool b1298), ("dynamic", PrettyRep.Bool b1299), 
          ("final", PrettyRep.Bool b1300), ("params", PrettyRep.List (List.map (fn x1301 => 
                                                                                      cvtIDENT x1301
                                                                               ) ls1302)), 
          ("extends", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1306))
       )), ("implements", PrettyRep.List (List.map (fn x1311 => cvtIDENT_EXPR x1311
                                                   ) ls1312)), ("classDefns", 
          PrettyRep.List (List.map (fn x1316 => cvtDEFN x1316
                                   ) ls1317)), ("instanceDefns", PrettyRep.List (List.map (fn x1321 => 
                                                                                                 cvtDEFN x1321
                                                                                          ) ls1322)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1326 => cvtSTMT x1326
                                                     ) ls1327)), ("ctorDefn", 
          
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1331))
       ))]
   and cvtINTERFACE_DEFN {ident=x1361, ns=opt1363, nonnullable=b1367, params=ls1369, 
          extends=ls1374, block=x1378} = PrettyRep.Rec [("ident", cvtIDENT x1361), 
          ("ns", 
       (case opt1363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1362))
       )), ("nonnullable", PrettyRep.Bool b1367), ("params", PrettyRep.List (List.map (fn x1368 => 
                                                                                             cvtIDENT x1368
                                                                                      ) ls1369)), 
          ("extends", PrettyRep.List (List.map (fn x1373 => cvtIDENT_EXPR x1373
                                               ) ls1374)), ("block", cvtBLOCK x1378)]
   and cvtTYPE_DEFN {ident=x1392, ns=opt1394, init=x1398} = PrettyRep.Rec [("ident", 
          cvtIDENT x1392), ("ns", 
       (case opt1394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1393 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1393))
       )), ("init", cvtTYPE_EXPR x1398)]
   and cvtFOR_ENUM_STMT {isEach=b1406, defn=opt1408, obj=x1412, fixtures=opt1414, 
          init=ls1419, labels=ls1424, body=x1428} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1406), ("defn", 
       (case opt1408 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1407 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1407))
       )), ("obj", cvtEXPR x1412), ("fixtures", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1413))
       )), ("init", PrettyRep.List (List.map (fn x1418 => cvtSTMT x1418
                                             ) ls1419)), ("labels", PrettyRep.List (List.map (fn x1423 => 
                                                                                                    cvtIDENT x1423
                                                                                             ) ls1424)), 
          ("body", cvtSTMT x1428)]
   and cvtFOR_STMT {fixtures=opt1445, defn=opt1450, init=ls1455, cond=x1459, 
          update=x1460, labels=ls1462, body=x1466} = PrettyRep.Rec [("fixtures", 
          
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1444))
       )), ("defn", 
       (case opt1450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1449 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1449))
       )), ("init", PrettyRep.List (List.map (fn x1454 => cvtSTMT x1454
                                             ) ls1455)), ("cond", cvtEXPR x1459), 
          ("update", cvtEXPR x1460), ("labels", PrettyRep.List (List.map (fn x1461 => 
                                                                                cvtIDENT x1461
                                                                         ) ls1462)), 
          ("body", cvtSTMT x1466)]
   and cvtWHILE_STMT {cond=x1482, fixtures=opt1484, body=x1488, labels=ls1490} = 
          PrettyRep.Rec [("cond", cvtEXPR x1482), ("fixtures", 
       (case opt1484 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1483 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1483))
       )), ("body", cvtSTMT x1488), ("labels", PrettyRep.List (List.map (fn x1489 => 
                                                                               cvtIDENT x1489
                                                                        ) ls1490))]
   and cvtDIRECTIVES {pragmas=ls1504, defns=ls1509, head=opt1514, body=ls1519, 
          pos=opt1524} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1503 => 
                                                                                    cvtPRAGMA x1503
                                                                             ) ls1504)), 
          ("defns", PrettyRep.List (List.map (fn x1508 => cvtDEFN x1508
                                             ) ls1509)), ("head", 
       (case opt1514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1513 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1513))
       )), ("body", PrettyRep.List (List.map (fn x1518 => cvtSTMT x1518
                                             ) ls1519)), ("pos", 
       (case opt1524 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1523))
       ))]
   and cvtCASE {label=opt1540, inits=opt1545, body=x1549} = PrettyRep.Rec [("label", 
          
       (case opt1540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1539 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1539))
       )), ("inits", 
       (case opt1545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1544 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1544))
       )), ("body", cvtBLOCK x1549)]
   and cvtTYPE_CASE {ty=opt1558, bindings=x1562, inits=opt1564, body=x1568} = 
          PrettyRep.Rec [("ty", 
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1557 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1557))
       )), ("bindings", cvtBINDINGS x1562), ("inits", 
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1563))
       )), ("body", cvtBLOCK x1568)]
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

