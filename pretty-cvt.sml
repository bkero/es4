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
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
   and cvtCLS (Cls{name=x317, extends=opt319, implements=ls324, classFixtures=x328, 
          instanceFixtures=x329, instanceInits=x330, constructor=opt332, classType=x336, 
          instanceType=x337}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x317), ("extends", 
       (case opt319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x318 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x318))
       )), ("implements", PrettyRep.List (List.map (fn x323 => cvtNAME x323
                                                   ) ls324)), ("classFixtures", 
          cvtFIXTURES x328), ("instanceFixtures", cvtFIXTURES x329), ("instanceInits", 
          cvtHEAD x330), ("constructor", 
       (case opt332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x331 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x331))
       )), ("classType", cvtTYPE_EXPR x336), ("instanceType", cvtTYPE_EXPR x337)]))
   and cvtCTOR (Ctor{settings=x359, superArgs=ls361, func=x365}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x359), ("superArgs", PrettyRep.List (List.map (fn x360 => 
                                                                                                         cvtEXPR x360
                                                                                                  ) ls361)), 
          ("func", cvtFUNC x365)]))
   and cvtFUNC (Func{name=x375, fsig=x376, isNative=b377, block=x378, param=x379, 
          defaults=ls381, ty=x385}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x375), ("fsig", cvtFUNC_SIG x376), ("isNative", PrettyRep.Bool b377), 
          ("block", cvtBLOCK x378), ("param", cvtHEAD x379), ("defaults", PrettyRep.List (List.map (fn x380 => 
                                                                                                          cvtEXPR x380
                                                                                                   ) ls381)), 
          ("ty", cvtFUNC_TYPE x385)]))
   and cvtDEFN (ClassDefn x403) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x403))
     | cvtDEFN (VariableDefn x406) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x406))
     | cvtDEFN (FunctionDefn x409) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x409))
     | cvtDEFN (ConstructorDefn x412) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x412))
     | cvtDEFN (InterfaceDefn x415) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x415))
     | cvtDEFN (NamespaceDefn x418) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x418))
     | cvtDEFN (TypeDefn x421) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x421))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls425, params=x429, paramTypes=ls431, 
          defaults=ls436, ctorInits=opt447, returnType=x451, thisType=opt453, 
          hasRest=b457}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x424 => cvtIDENT x424
                                   ) ls425)), ("params", cvtBINDINGS x429), 
          ("paramTypes", PrettyRep.List (List.map (fn x430 => cvtTYPE_EXPR x430
                                                  ) ls431)), ("defaults", PrettyRep.List (List.map (fn x435 => 
                                                                                                          cvtEXPR x435
                                                                                                   ) ls436)), 
          ("ctorInits", 
       (case opt447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x440, ls442) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x440, 
            PrettyRep.List (List.map (fn x441 => cvtEXPR x441
                                     ) ls442)]))
       )), ("returnType", cvtTYPE_EXPR x451), ("thisType", 
       (case opt453 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x452 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x452))
       )), ("hasRest", PrettyRep.Bool b457)]))
   and cvtBINDING (Binding{ident=x477, ty=x478}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x477), ("ty", cvtTYPE_EXPR x478)]))
   and cvtBINDING_IDENT (TempIdent n486) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n486))
     | cvtBINDING_IDENT (ParamIdent n489) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n489))
     | cvtBINDING_IDENT (PropIdent x492) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x492))
   and cvtINIT_STEP (InitStep(x495, x496)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x495, 
          cvtEXPR x496]))
     | cvtINIT_STEP (AssignStep(x500, x501)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x500, cvtEXPR x501]))
   and cvtTYPE_EXPR (SpecialType x505) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x505))
     | cvtTYPE_EXPR (UnionType ls509) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x508 => 
                                                                                                           cvtTYPE_EXPR x508
                                                                                                    ) ls509)))
     | cvtTYPE_EXPR (ArrayType ls516) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x515 => 
                                                                                                           cvtTYPE_EXPR x515
                                                                                                    ) ls516)))
     | cvtTYPE_EXPR (TypeName x522) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x522))
     | cvtTYPE_EXPR (ElementTypeRef(x525, n526)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x525, PrettyRep.Int n526]))
     | cvtTYPE_EXPR (FieldTypeRef(x530, x531)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x530, cvtIDENT x531]))
     | cvtTYPE_EXPR (FunctionType x535) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x535))
     | cvtTYPE_EXPR (ObjectType ls539) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x538 => 
                                                                                                             cvtFIELD_TYPE x538
                                                                                                      ) ls539)))
     | cvtTYPE_EXPR (AppType{base=x545, args=ls547}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x545), ("args", PrettyRep.List (List.map (fn x546 => 
                                                                                                     cvtTYPE_EXPR x546
                                                                                              ) ls547))]))
     | cvtTYPE_EXPR (NullableType{expr=x558, nullable=b559}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x558), ("nullable", PrettyRep.Bool b559)]))
     | cvtTYPE_EXPR (InstanceType{name=x567, typeParams=ls569, ty=x573, isDynamic=b574}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x567), 
          ("typeParams", PrettyRep.List (List.map (fn x568 => cvtIDENT x568
                                                  ) ls569)), ("ty", cvtTYPE_EXPR x573), 
          ("isDynamic", PrettyRep.Bool b574)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x587) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x587))
     | cvtSTMT (InitStmt{kind=x590, ns=opt592, prototype=b596, static=b597, 
          temps=x598, inits=ls600}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x590), ("ns", 
       (case opt592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x591 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x591))
       )), ("prototype", PrettyRep.Bool b596), ("static", PrettyRep.Bool b597), 
          ("temps", cvtBINDINGS x598), ("inits", PrettyRep.List (List.map (fn x599 => 
                                                                                 cvtINIT_STEP x599
                                                                          ) ls600))]))
     | cvtSTMT (ClassBlock{ns=opt620, ident=x624, name=opt626, block=x630}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt620 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x619 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x619))
       )), ("ident", cvtIDENT x624), ("name", 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x625))
       )), ("block", cvtBLOCK x630)]))
     | cvtSTMT (ForEachStmt x642) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x642))
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
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1098) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1098))
     | cvtFIXTURE (MethodFixture{func=x1101, ty=x1102, readOnly=b1103, override=b1104, 
          final=b1105}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1101), ("ty", cvtTYPE_EXPR x1102), ("readOnly", PrettyRep.Bool b1103), 
          ("override", PrettyRep.Bool b1104), ("final", PrettyRep.Bool b1105)]))
     | cvtFIXTURE (ValFixture{ty=x1119, readOnly=b1120}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1119), ("readOnly", PrettyRep.Bool b1120)]))
     | cvtFIXTURE (VirtualValFixture x1128) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1128))
   and cvtBINDINGS (ls1132, ls1137) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1131 => 
                                                                                       cvtBINDING x1131
                                                                                ) ls1132), 
          PrettyRep.List (List.map (fn x1136 => cvtINIT_STEP x1136
                                   ) ls1137)]
   and cvtFIXTURES ls1145 = PrettyRep.List (List.map (fn (x1142, x1143) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1142, 
                                                            cvtFIXTURE x1143]
                                                     ) ls1145)
   and cvtINITS ls1152 = PrettyRep.List (List.map (fn (x1149, x1150) => PrettyRep.Tuple [cvtFIXTURE_NAME x1149, 
                                                         cvtEXPR x1150]
                                                  ) ls1152)
   and cvtHEAD (x1156, x1157) = PrettyRep.Tuple [cvtFIXTURES x1156, cvtINITS x1157]
   and cvtFIELD {kind=x1159, name=x1160, init=x1161} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1159), ("name", cvtIDENT_EXPR x1160), ("init", cvtEXPR x1161)]
   and cvtFIELD_TYPE {name=x1169, ty=x1170} = PrettyRep.Rec [("name", cvtIDENT x1169), 
          ("ty", cvtTYPE_EXPR x1170)]
   and cvtTYPED_IDENT {name=x1176, ty=opt1178} = PrettyRep.Rec [("name", cvtIDENT x1176), 
          ("ty", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1177))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1188, params=ls1193, result=x1197, thisType=opt1199, 
          hasRest=b1203, minArgs=n1204} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1187 => 
                                                                                                        cvtIDENT x1187
                                                                                                 ) ls1188)), 
          ("params", PrettyRep.List (List.map (fn x1192 => cvtTYPE_EXPR x1192
                                              ) ls1193)), ("result", cvtTYPE_EXPR x1197), 
          ("thisType", 
       (case opt1199 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1198 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1198))
       )), ("hasRest", PrettyRep.Bool b1203), ("minArgs", PrettyRep.Int n1204)]
   and cvtFUNC_DEFN {kind=x1218, ns=opt1220, final=b1224, override=b1225, prototype=b1226, 
          static=b1227, func=x1228} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1218), 
          ("ns", 
       (case opt1220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1219 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1219))
       )), ("final", PrettyRep.Bool b1224), ("override", PrettyRep.Bool b1225), 
          ("prototype", PrettyRep.Bool b1226), ("static", PrettyRep.Bool b1227), 
          ("func", cvtFUNC x1228)]
   and cvtCTOR_DEFN x1244 = cvtCTOR x1244
   and cvtVAR_DEFN {kind=x1245, ns=opt1247, static=b1251, prototype=b1252, 
          bindings=x1253} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1245), 
          ("ns", 
       (case opt1247 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1246 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1246))
       )), ("static", PrettyRep.Bool b1251), ("prototype", PrettyRep.Bool b1252), 
          ("bindings", cvtBINDINGS x1253)]
   and cvtNAMESPACE_DEFN {ident=x1265, ns=opt1267, init=opt1272} = PrettyRep.Rec [("ident", 
          cvtIDENT x1265), ("ns", 
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1266))
       )), ("init", 
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1271))
       ))]
   and cvtCLASS_DEFN {ident=x1283, ns=opt1285, nonnullable=b1289, dynamic=b1290, 
          final=b1291, params=ls1293, extends=opt1298, implements=ls1303, classDefns=ls1308, 
          instanceDefns=ls1313, instanceStmts=ls1318, ctorDefn=opt1323} = PrettyRep.Rec [("ident", 
          cvtIDENT x1283), ("ns", 
       (case opt1285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1284 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1284))
       )), ("nonnullable", PrettyRep.Bool b1289), ("dynamic", PrettyRep.Bool b1290), 
          ("final", PrettyRep.Bool b1291), ("params", PrettyRep.List (List.map (fn x1292 => 
                                                                                      cvtIDENT x1292
                                                                               ) ls1293)), 
          ("extends", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1297))
       )), ("implements", PrettyRep.List (List.map (fn x1302 => cvtIDENT_EXPR x1302
                                                   ) ls1303)), ("classDefns", 
          PrettyRep.List (List.map (fn x1307 => cvtDEFN x1307
                                   ) ls1308)), ("instanceDefns", PrettyRep.List (List.map (fn x1312 => 
                                                                                                 cvtDEFN x1312
                                                                                          ) ls1313)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1317 => cvtSTMT x1317
                                                     ) ls1318)), ("ctorDefn", 
          
       (case opt1323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1322 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1322))
       ))]
   and cvtINTERFACE_DEFN {ident=x1352, ns=opt1354, nonnullable=b1358, params=ls1360, 
          extends=ls1365, block=x1369} = PrettyRep.Rec [("ident", cvtIDENT x1352), 
          ("ns", 
       (case opt1354 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1353 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1353))
       )), ("nonnullable", PrettyRep.Bool b1358), ("params", PrettyRep.List (List.map (fn x1359 => 
                                                                                             cvtIDENT x1359
                                                                                      ) ls1360)), 
          ("extends", PrettyRep.List (List.map (fn x1364 => cvtIDENT_EXPR x1364
                                               ) ls1365)), ("block", cvtBLOCK x1369)]
   and cvtTYPE_DEFN {ident=x1383, ns=opt1385, init=x1389} = PrettyRep.Rec [("ident", 
          cvtIDENT x1383), ("ns", 
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1384))
       )), ("init", cvtTYPE_EXPR x1389)]
   and cvtFOR_ENUM_STMT {defn=opt1398, obj=x1402, fixtures=opt1404, inits=opt1409, 
          labels=ls1414, body=x1418} = PrettyRep.Rec [("defn", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1397 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1397))
       )), ("obj", cvtEXPR x1402), ("fixtures", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1403 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1403))
       )), ("inits", 
       (case opt1409 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1408 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1408))
       )), ("labels", PrettyRep.List (List.map (fn x1413 => cvtIDENT x1413
                                               ) ls1414)), ("body", cvtSTMT x1418)]
   and cvtFOR_STMT {fixtures=opt1433, defn=opt1438, init=ls1443, cond=x1447, 
          update=x1448, labels=ls1450, body=x1454} = PrettyRep.Rec [("fixtures", 
          
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1432))
       )), ("defn", 
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1437))
       )), ("init", PrettyRep.List (List.map (fn x1442 => cvtSTMT x1442
                                             ) ls1443)), ("cond", cvtEXPR x1447), 
          ("update", cvtEXPR x1448), ("labels", PrettyRep.List (List.map (fn x1449 => 
                                                                                cvtIDENT x1449
                                                                         ) ls1450)), 
          ("body", cvtSTMT x1454)]
   and cvtWHILE_STMT {cond=x1470, fixtures=opt1472, body=x1476, labels=ls1478} = 
          PrettyRep.Rec [("cond", cvtEXPR x1470), ("fixtures", 
       (case opt1472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1471 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1471))
       )), ("body", cvtSTMT x1476), ("labels", PrettyRep.List (List.map (fn x1477 => 
                                                                               cvtIDENT x1477
                                                                        ) ls1478))]
   and cvtDIRECTIVES {pragmas=ls1492, defns=ls1497, head=opt1502, body=ls1507, 
          pos=opt1512} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1491 => 
                                                                                    cvtPRAGMA x1491
                                                                             ) ls1492)), 
          ("defns", PrettyRep.List (List.map (fn x1496 => cvtDEFN x1496
                                             ) ls1497)), ("head", 
       (case opt1502 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1501 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1501))
       )), ("body", PrettyRep.List (List.map (fn x1506 => cvtSTMT x1506
                                             ) ls1507)), ("pos", 
       (case opt1512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1511 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1511))
       ))]
   and cvtCASE {label=opt1528, inits=opt1533, body=x1537} = PrettyRep.Rec [("label", 
          
       (case opt1528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1527 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1527))
       )), ("inits", 
       (case opt1533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1532 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1532))
       )), ("body", cvtBLOCK x1537)]
   and cvtTYPE_CASE {ty=opt1546, bindings=x1550, inits=opt1552, body=x1556} = 
          PrettyRep.Rec [("ty", 
       (case opt1546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1545 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1545))
       )), ("bindings", cvtBINDINGS x1550), ("inits", 
       (case opt1552 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1551 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1551))
       )), ("body", cvtBLOCK x1556)]
   and cvtCATCH_CLAUSE {bindings=x1566, ty=x1567, fixtures=opt1569, block=x1573} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1566), ("ty", cvtTYPE_EXPR x1567), 
          ("fixtures", 
       (case opt1569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1568))
       )), ("block", cvtBLOCK x1573)]
   and cvtFUNC_NAME {kind=x1583, ident=x1584} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1583), 
          ("ident", cvtIDENT x1584)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1590, getter=opt1592, setter=opt1597} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1590), ("getter", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1591))
       )), ("setter", 
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1596 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1596))
       ))]
   and cvtPACKAGE {name=ls1609, block=x1613} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1608 => 
                                                                                                       cvtIDENT x1608
                                                                                                ) ls1609)), 
          ("block", cvtBLOCK x1613)]
   and cvtPROGRAM {packages=ls1620, fixtures=opt1625, block=x1629} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1619 => cvtPACKAGE x1619
                                   ) ls1620)), ("fixtures", 
       (case opt1625 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1624 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1624))
       )), ("block", cvtBLOCK x1629)]
end

