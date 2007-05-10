structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x22) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x22))
     | cvtNAMESPACE (Protected x25) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x25))
     | cvtNAMESPACE (Public x28) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Internal x31) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x31))
     | cvtNAMESPACE (UserNamespace s34) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s34))
     | cvtNAMESPACE (AnonUserNamespace n37) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n37))
     | cvtNAMESPACE (LimitedNamespace(x40, x41)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x40, cvtNAMESPACE x41]))
   and cvtNAME {ns=x45, id=x46} = PrettyRep.Rec [("ns", cvtNAMESPACE x45), 
          ("id", cvtIDENT x46)]
   and cvtMULTINAME {nss=ls57, id=x61} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls53 => 
                                                                                                PrettyRep.List (List.map (fn x52 => 
                                                                                                                                cvtNAMESPACE x52
                                                                                                                         ) ls53)
                                                                                         ) ls57)), 
          ("id", cvtIDENT x61)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x72, roundingMode=r73, precision=n74} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x72), ("roundingMode", 
          PrettyRep.DecRm r73), ("precision", PrettyRep.Int n74)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt87) = PrettyRep.Ctor ("Plus", SOME 
       (case opt87 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
       ))
     | cvtBINOP (Minus opt94) = PrettyRep.Ctor ("Minus", SOME 
       (case opt94 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
       ))
     | cvtBINOP (Times opt101) = PrettyRep.Ctor ("Times", SOME 
       (case opt101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x100 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x100))
       ))
     | cvtBINOP (Divide opt108) = PrettyRep.Ctor ("Divide", SOME 
       (case opt108 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x107 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x107))
       ))
     | cvtBINOP (Remainder opt115) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt115 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x114 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x114))
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
     | cvtBINOP (Equals opt132) = PrettyRep.Ctor ("Equals", SOME 
       (case opt132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
       ))
     | cvtBINOP (NotEquals opt139) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
       ))
     | cvtBINOP (StrictEquals opt146) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
       ))
     | cvtBINOP (StrictNotEquals opt153) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
       ))
     | cvtBINOP (Less opt160) = PrettyRep.Ctor ("Less", SOME 
       (case opt160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
       ))
     | cvtBINOP (LessOrEqual opt167) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x166))
       ))
     | cvtBINOP (Greater opt174) = PrettyRep.Ctor ("Greater", SOME 
       (case opt174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x173 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x173))
       ))
     | cvtBINOP (GreaterOrEqual opt181) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x180 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x180))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt190) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
       ))
     | cvtASSIGNOP (AssignMinus opt197) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
       ))
     | cvtASSIGNOP (AssignTimes opt204) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt204 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x203 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x203))
       ))
     | cvtASSIGNOP (AssignDivide opt211) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x210 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x210))
       ))
     | cvtASSIGNOP (AssignRemainder opt218) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt218 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x217 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x217))
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
     | cvtUNOP (PreIncrement opt236) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
       ))
     | cvtUNOP (PreDecrement opt243) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
       ))
     | cvtUNOP (PostIncrement opt250) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
       ))
     | cvtUNOP (PostDecrement opt257) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x256))
       ))
     | cvtUNOP (UnaryPlus opt264) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x263 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x263))
       ))
     | cvtUNOP (UnaryMinus opt271) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt271 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x270 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x270))
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
   and cvtPRAGMA (UseNamespace x288) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x288))
     | cvtPRAGMA (UseDefaultNamespace x291) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x291))
     | cvtPRAGMA (UseNumber x294) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x294))
     | cvtPRAGMA (UseRounding r297) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r297))
     | cvtPRAGMA (UsePrecision n300) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n300))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls306, name=x310, alias=opt312}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x305 => 
                                                                           cvtIDENT x305
                                                                    ) ls306)), 
          ("name", cvtIDENT x310), ("alias", 
       (case opt312 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x311 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x311))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x331, extends=opt333, implements=ls338, classFixtures=x342, 
          instanceFixtures=x343, instanceInits=x344, constructor=opt346, classType=x350, 
          instanceType=x351}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x331), ("extends", 
       (case opt333 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x332 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x332))
       )), ("implements", PrettyRep.List (List.map (fn x337 => cvtNAME x337
                                                   ) ls338)), ("classFixtures", 
          cvtFIXTURES x342), ("instanceFixtures", cvtFIXTURES x343), ("instanceInits", 
          cvtHEAD x344), ("constructor", 
       (case opt346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x345 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x345))
       )), ("classType", cvtTYPE_EXPR x350), ("instanceType", cvtTYPE_EXPR x351)]))
   and cvtCTOR (Ctor{settings=x373, superArgs=ls375, func=x379}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x373), ("superArgs", PrettyRep.List (List.map (fn x374 => 
                                                                                                         cvtEXPR x374
                                                                                                  ) ls375)), 
          ("func", cvtFUNC x379)]))
   and cvtFUNC (Func{name=x389, fsig=x390, isNative=b391, block=x392, param=x393, 
          defaults=ls395, ty=x399}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x389), ("fsig", cvtFUNC_SIG x390), ("isNative", PrettyRep.Bool b391), 
          ("block", cvtBLOCK x392), ("param", cvtHEAD x393), ("defaults", PrettyRep.List (List.map (fn x394 => 
                                                                                                          cvtEXPR x394
                                                                                                   ) ls395)), 
          ("ty", cvtFUNC_TYPE x399)]))
   and cvtDEFN (ClassDefn x417) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x417))
     | cvtDEFN (VariableDefn x420) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x420))
     | cvtDEFN (FunctionDefn x423) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x423))
     | cvtDEFN (ConstructorDefn x426) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x426))
     | cvtDEFN (InterfaceDefn x429) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x429))
     | cvtDEFN (NamespaceDefn x432) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x432))
     | cvtDEFN (TypeDefn x435) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x435))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls439, params=x443, paramTypes=ls445, 
          defaults=ls450, ctorInits=opt461, returnType=x465, thisType=opt467, 
          hasRest=b471}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x438 => cvtIDENT x438
                                   ) ls439)), ("params", cvtBINDINGS x443), 
          ("paramTypes", PrettyRep.List (List.map (fn x444 => cvtTYPE_EXPR x444
                                                  ) ls445)), ("defaults", PrettyRep.List (List.map (fn x449 => 
                                                                                                          cvtEXPR x449
                                                                                                   ) ls450)), 
          ("ctorInits", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x454, ls456) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x454, 
            PrettyRep.List (List.map (fn x455 => cvtEXPR x455
                                     ) ls456)]))
       )), ("returnType", cvtTYPE_EXPR x465), ("thisType", 
       (case opt467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x466 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x466))
       )), ("hasRest", PrettyRep.Bool b471)]))
   and cvtBINDING (Binding{ident=x491, ty=x492}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x491), ("ty", cvtTYPE_EXPR x492)]))
   and cvtBINDING_IDENT (TempIdent n500) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n500))
     | cvtBINDING_IDENT (ParamIdent n503) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n503))
     | cvtBINDING_IDENT (PropIdent x506) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x506))
   and cvtINIT_STEP (InitStep(x509, x510)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x509, 
          cvtEXPR x510]))
     | cvtINIT_STEP (AssignStep(x514, x515)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x514, cvtEXPR x515]))
   and cvtTYPE_EXPR (SpecialType x519) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x519))
     | cvtTYPE_EXPR (UnionType ls523) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x522 => 
                                                                                                           cvtTYPE_EXPR x522
                                                                                                    ) ls523)))
     | cvtTYPE_EXPR (ArrayType ls530) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x529 => 
                                                                                                           cvtTYPE_EXPR x529
                                                                                                    ) ls530)))
     | cvtTYPE_EXPR (TypeName x536) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x536))
     | cvtTYPE_EXPR (ElementTypeRef(x539, n540)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x539, PrettyRep.Int n540]))
     | cvtTYPE_EXPR (FieldTypeRef(x544, x545)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x544, cvtIDENT x545]))
     | cvtTYPE_EXPR (FunctionType x549) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x549))
     | cvtTYPE_EXPR (ObjectType ls553) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x552 => 
                                                                                                             cvtFIELD_TYPE x552
                                                                                                      ) ls553)))
     | cvtTYPE_EXPR (AppType{base=x559, args=ls561}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x559), ("args", PrettyRep.List (List.map (fn x560 => 
                                                                                                     cvtTYPE_EXPR x560
                                                                                              ) ls561))]))
     | cvtTYPE_EXPR (NullableType{expr=x572, nullable=b573}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x572), ("nullable", PrettyRep.Bool b573)]))
     | cvtTYPE_EXPR (InstanceType{name=x581, typeParams=ls583, ty=x587, isDynamic=b588}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x581), 
          ("typeParams", PrettyRep.List (List.map (fn x582 => cvtIDENT x582
                                                  ) ls583)), ("ty", cvtTYPE_EXPR x587), 
          ("isDynamic", PrettyRep.Bool b588)]))
     | cvtTYPE_EXPR (NominalType x600) = PrettyRep.Ctor ("NominalType", SOME (cvtNAME x600))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x604) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x604))
     | cvtSTMT (InitStmt{kind=x607, ns=opt609, prototype=b613, static=b614, 
          temps=x615, inits=ls617}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x607), ("ns", 
       (case opt609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x608 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x608))
       )), ("prototype", PrettyRep.Bool b613), ("static", PrettyRep.Bool b614), 
          ("temps", cvtBINDINGS x615), ("inits", PrettyRep.List (List.map (fn x616 => 
                                                                                 cvtINIT_STEP x616
                                                                          ) ls617))]))
     | cvtSTMT (ClassBlock{ns=opt637, ident=x641, name=opt643, block=x647}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x636 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x636))
       )), ("ident", cvtIDENT x641), ("name", 
       (case opt643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x642 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x642))
       )), ("block", cvtBLOCK x647)]))
     | cvtSTMT (ForInStmt x659) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x659))
     | cvtSTMT (ThrowStmt x662) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x662))
     | cvtSTMT (ReturnStmt x665) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x665))
     | cvtSTMT (BreakStmt opt669) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x668))
       ))
     | cvtSTMT (ContinueStmt opt676) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x675 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x675))
       ))
     | cvtSTMT (BlockStmt x682) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x682))
     | cvtSTMT (LabeledStmt(x685, x686)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x685, 
          cvtSTMT x686]))
     | cvtSTMT (LetStmt x690) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x690))
     | cvtSTMT (WhileStmt x693) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x693))
     | cvtSTMT (DoWhileStmt x696) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x696))
     | cvtSTMT (ForStmt x699) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x699))
     | cvtSTMT (IfStmt{cnd=x702, thn=x703, els=x704}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x702), ("thn", cvtSTMT x703), 
          ("els", cvtSTMT x704)]))
     | cvtSTMT (WithStmt{obj=x714, ty=x715, body=x716}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x714), ("ty", cvtTYPE_EXPR x715), 
          ("body", cvtSTMT x716)]))
     | cvtSTMT (TryStmt{block=x726, catches=ls728, finally=opt733}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x726), ("catches", PrettyRep.List (List.map (fn x727 => 
                                                                                                     cvtCATCH_CLAUSE x727
                                                                                              ) ls728)), 
          ("finally", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x732))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt747, cond=x751, labels=ls753, cases=ls758}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt747 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x746 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x746))
       )), ("cond", cvtEXPR x751), ("labels", PrettyRep.List (List.map (fn x752 => 
                                                                              cvtIDENT x752
                                                                       ) ls753)), 
          ("cases", PrettyRep.List (List.map (fn x757 => cvtCASE x757
                                             ) ls758))]))
     | cvtSTMT (SwitchTypeStmt{cond=x773, ty=x774, cases=ls776}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x773), ("ty", cvtTYPE_EXPR x774), 
          ("cases", PrettyRep.List (List.map (fn x775 => cvtTYPE_CASE x775
                                             ) ls776))]))
     | cvtSTMT (Dxns{expr=x789}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x789)]))
   and cvtEXPR (TernaryExpr(x795, x796, x797, x798)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x795, cvtEXPR x796, cvtEXPR x797, 
          cvtEXPR x798]))
     | cvtEXPR (BinaryExpr(x802, x803, x804)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x802, cvtEXPR x803, cvtEXPR x804]))
     | cvtEXPR (BinaryTypeExpr(x808, x809, x810)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x808, cvtEXPR x809, cvtTYPE_EXPR x810]))
     | cvtEXPR (ExpectedTypeExpr(x814, x815)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x814, cvtEXPR x815]))
     | cvtEXPR (UnaryExpr(x819, x820)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x819, 
          cvtEXPR x820]))
     | cvtEXPR (TypeExpr x824) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x824))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt829) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt829 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x828 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x828))
       ))
     | cvtEXPR (SuperExpr opt836) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt836 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x835 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x835))
       ))
     | cvtEXPR (LiteralExpr x842) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x842))
     | cvtEXPR (CallExpr{func=x845, actuals=ls847}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x845), ("actuals", PrettyRep.List (List.map (fn x846 => 
                                                                                                   cvtEXPR x846
                                                                                            ) ls847))]))
     | cvtEXPR (ApplyTypeExpr{expr=x858, actuals=ls860}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x858), ("actuals", PrettyRep.List (List.map (fn x859 => 
                                                                                                   cvtTYPE_EXPR x859
                                                                                            ) ls860))]))
     | cvtEXPR (LetExpr{defs=x871, body=x872, head=opt874}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x871), ("body", cvtEXPR x872), 
          ("head", 
       (case opt874 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x873 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x873))
       ))]))
     | cvtEXPR (NewExpr{obj=x887, actuals=ls889}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x887), ("actuals", PrettyRep.List (List.map (fn x888 => 
                                                                                                  cvtEXPR x888
                                                                                           ) ls889))]))
     | cvtEXPR (ObjectRef{base=x900, ident=x901, loc=opt903}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x900), ("ident", cvtIDENT_EXPR x901), 
          ("loc", 
       (case opt903 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x902 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x902))
       ))]))
     | cvtEXPR (LexicalRef{ident=x916, loc=opt918}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x916), ("loc", 
       (case opt918 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x917 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x917))
       ))]))
     | cvtEXPR (SetExpr(x929, x930, x931)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x929, 
          cvtEXPR x930, cvtEXPR x931]))
     | cvtEXPR (ListExpr ls936) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x935 => 
                                                                                                    cvtEXPR x935
                                                                                             ) ls936)))
     | cvtEXPR (InitExpr(x942, x943, x944)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x942, 
          cvtHEAD x943, cvtINITS x944]))
     | cvtEXPR (SliceExpr(x948, x949, x950)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x948, cvtEXPR x949, cvtEXPR x950]))
     | cvtEXPR (GetTemp n954) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n954))
     | cvtEXPR (GetParam n957) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n957))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n963) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n963))
     | cvtFIXTURE_NAME (PropName x966) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x966))
   and cvtIDENT_EXPR (Identifier{ident=x969, openNamespaces=ls975}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x969), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls971 => PrettyRep.List (List.map (fn x970 => 
                                                                                cvtNAMESPACE x970
                                                                         ) ls971)
                                   ) ls975))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x986, expr=x987}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x986), ("expr", cvtEXPR x987)]))
     | cvtIDENT_EXPR (AttributeIdentifier x995) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x995))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x998, openNamespaces=ls1004}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x998), ("openNamespaces", PrettyRep.List (List.map (fn ls1000 => 
                                                                            PrettyRep.List (List.map (fn x999 => 
                                                                                                            cvtNAMESPACE x999
                                                                                                     ) ls1000)
                                                                     ) ls1004))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1015, ident=s1016}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1015), ("ident", PrettyRep.UniStr s1016)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1024, typeArgs=ls1026}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1024), ("typeArgs", 
          PrettyRep.List (List.map (fn x1025 => cvtTYPE_EXPR x1025
                                   ) ls1026))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1038, x1042)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1037 => cvtIDENT x1037
                                                          ) ls1038), cvtIDENT_EXPR x1042]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1049) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1049))
     | cvtLITERAL (LiteralContextualDecimalInteger s1052) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1052))
     | cvtLITERAL (LiteralContextualHexInteger s1055) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1055))
     | cvtLITERAL (LiteralDouble r1058) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1058))
     | cvtLITERAL (LiteralDecimal d1061) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1061))
     | cvtLITERAL (LiteralInt i1064) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1064))
     | cvtLITERAL (LiteralUInt u1067) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1067))
     | cvtLITERAL (LiteralBoolean b1070) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1070))
     | cvtLITERAL (LiteralString s1073) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1073))
     | cvtLITERAL (LiteralArray{exprs=ls1077, ty=opt1082}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1076 => 
                                                                         cvtEXPR x1076
                                                                  ) ls1077)), 
          ("ty", 
       (case opt1082 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1081 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1081))
       ))]))
     | cvtLITERAL (LiteralXML ls1094) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1093 => 
                                                                                                            cvtEXPR x1093
                                                                                                     ) ls1094)))
     | cvtLITERAL (LiteralNamespace x1100) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1100))
     | cvtLITERAL (LiteralObject{expr=ls1104, ty=opt1109}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1103 => 
                                                                        cvtFIELD x1103
                                                                 ) ls1104)), 
          ("ty", 
       (case opt1109 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1108 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1108))
       ))]))
     | cvtLITERAL (LiteralFunction x1120) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1120))
     | cvtLITERAL (LiteralRegExp{str=s1123}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1123)]))
   and cvtBLOCK (Block x1129) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1129))
   and cvtFIXTURE (NamespaceFixture x1132) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1132))
     | cvtFIXTURE (ClassFixture x1135) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1135))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1140) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1140))
     | cvtFIXTURE (MethodFixture{func=x1143, ty=x1144, readOnly=b1145, override=b1146, 
          final=b1147}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1143), ("ty", cvtTYPE_EXPR x1144), ("readOnly", PrettyRep.Bool b1145), 
          ("override", PrettyRep.Bool b1146), ("final", PrettyRep.Bool b1147)]))
     | cvtFIXTURE (ValFixture{ty=x1161, readOnly=b1162}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1161), ("readOnly", PrettyRep.Bool b1162)]))
     | cvtFIXTURE (VirtualValFixture x1170) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1170))
   and cvtBINDINGS (ls1174, ls1179) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1173 => 
                                                                                       cvtBINDING x1173
                                                                                ) ls1174), 
          PrettyRep.List (List.map (fn x1178 => cvtINIT_STEP x1178
                                   ) ls1179)]
   and cvtFIXTURES ls1187 = PrettyRep.List (List.map (fn (x1184, x1185) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1184, 
                                                            cvtFIXTURE x1185]
                                                     ) ls1187)
   and cvtINITS ls1194 = PrettyRep.List (List.map (fn (x1191, x1192) => PrettyRep.Tuple [cvtFIXTURE_NAME x1191, 
                                                         cvtEXPR x1192]
                                                  ) ls1194)
   and cvtHEAD (x1198, x1199) = PrettyRep.Tuple [cvtFIXTURES x1198, cvtINITS x1199]
   and cvtFIELD {kind=x1201, name=x1202, init=x1203} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1201), ("name", cvtIDENT_EXPR x1202), ("init", cvtEXPR x1203)]
   and cvtFIELD_TYPE {name=x1211, ty=x1212} = PrettyRep.Rec [("name", cvtIDENT x1211), 
          ("ty", cvtTYPE_EXPR x1212)]
   and cvtTYPED_IDENT {name=x1218, ty=opt1220} = PrettyRep.Rec [("name", cvtIDENT x1218), 
          ("ty", 
       (case opt1220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1219 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1219))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1230, params=ls1235, result=x1239, thisType=opt1241, 
          hasRest=b1245, minArgs=n1246} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1229 => 
                                                                                                        cvtIDENT x1229
                                                                                                 ) ls1230)), 
          ("params", PrettyRep.List (List.map (fn x1234 => cvtTYPE_EXPR x1234
                                              ) ls1235)), ("result", cvtTYPE_EXPR x1239), 
          ("thisType", 
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1240))
       )), ("hasRest", PrettyRep.Bool b1245), ("minArgs", PrettyRep.Int n1246)]
   and cvtFUNC_DEFN {kind=x1260, ns=opt1262, final=b1266, override=b1267, prototype=b1268, 
          static=b1269, func=x1270} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1260), 
          ("ns", 
       (case opt1262 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1261 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1261))
       )), ("final", PrettyRep.Bool b1266), ("override", PrettyRep.Bool b1267), 
          ("prototype", PrettyRep.Bool b1268), ("static", PrettyRep.Bool b1269), 
          ("func", cvtFUNC x1270)]
   and cvtCTOR_DEFN x1286 = cvtCTOR x1286
   and cvtVAR_DEFN {kind=x1287, ns=opt1289, static=b1293, prototype=b1294, 
          bindings=x1295} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1287), 
          ("ns", 
       (case opt1289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1288 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1288))
       )), ("static", PrettyRep.Bool b1293), ("prototype", PrettyRep.Bool b1294), 
          ("bindings", cvtBINDINGS x1295)]
   and cvtNAMESPACE_DEFN {ident=x1307, ns=opt1309, init=opt1314} = PrettyRep.Rec [("ident", 
          cvtIDENT x1307), ("ns", 
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1308))
       )), ("init", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1313))
       ))]
   and cvtCLASS_DEFN {ident=x1325, ns=opt1327, nonnullable=b1331, dynamic=b1332, 
          final=b1333, params=ls1335, extends=opt1340, implements=ls1345, classDefns=ls1350, 
          instanceDefns=ls1355, instanceStmts=ls1360, ctorDefn=opt1365} = PrettyRep.Rec [("ident", 
          cvtIDENT x1325), ("ns", 
       (case opt1327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1326 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1326))
       )), ("nonnullable", PrettyRep.Bool b1331), ("dynamic", PrettyRep.Bool b1332), 
          ("final", PrettyRep.Bool b1333), ("params", PrettyRep.List (List.map (fn x1334 => 
                                                                                      cvtIDENT x1334
                                                                               ) ls1335)), 
          ("extends", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1339))
       )), ("implements", PrettyRep.List (List.map (fn x1344 => cvtIDENT_EXPR x1344
                                                   ) ls1345)), ("classDefns", 
          PrettyRep.List (List.map (fn x1349 => cvtDEFN x1349
                                   ) ls1350)), ("instanceDefns", PrettyRep.List (List.map (fn x1354 => 
                                                                                                 cvtDEFN x1354
                                                                                          ) ls1355)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1359 => cvtSTMT x1359
                                                     ) ls1360)), ("ctorDefn", 
          
       (case opt1365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1364))
       ))]
   and cvtINTERFACE_DEFN {ident=x1394, ns=opt1396, nonnullable=b1400, params=ls1402, 
          extends=ls1407, block=x1411} = PrettyRep.Rec [("ident", cvtIDENT x1394), 
          ("ns", 
       (case opt1396 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1395 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1395))
       )), ("nonnullable", PrettyRep.Bool b1400), ("params", PrettyRep.List (List.map (fn x1401 => 
                                                                                             cvtIDENT x1401
                                                                                      ) ls1402)), 
          ("extends", PrettyRep.List (List.map (fn x1406 => cvtIDENT_EXPR x1406
                                               ) ls1407)), ("block", cvtBLOCK x1411)]
   and cvtTYPE_DEFN {ident=x1425, ns=opt1427, init=x1431} = PrettyRep.Rec [("ident", 
          cvtIDENT x1425), ("ns", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1426))
       )), ("init", cvtTYPE_EXPR x1431)]
   and cvtFOR_ENUM_STMT {isEach=b1439, defn=opt1441, obj=x1445, fixtures=opt1447, 
          next=x1451, labels=ls1453, body=x1457} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1439), ("defn", 
       (case opt1441 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1440))
       )), ("obj", cvtEXPR x1445), ("fixtures", 
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1446 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1446))
       )), ("next", cvtSTMT x1451), ("labels", PrettyRep.List (List.map (fn x1452 => 
                                                                               cvtIDENT x1452
                                                                        ) ls1453)), 
          ("body", cvtSTMT x1457)]
   and cvtFOR_STMT {fixtures=opt1474, defn=opt1479, init=ls1484, cond=x1488, 
          update=x1489, labels=ls1491, body=x1495} = PrettyRep.Rec [("fixtures", 
          
       (case opt1474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1473 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1473))
       )), ("defn", 
       (case opt1479 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1478 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1478))
       )), ("init", PrettyRep.List (List.map (fn x1483 => cvtSTMT x1483
                                             ) ls1484)), ("cond", cvtEXPR x1488), 
          ("update", cvtEXPR x1489), ("labels", PrettyRep.List (List.map (fn x1490 => 
                                                                                cvtIDENT x1490
                                                                         ) ls1491)), 
          ("body", cvtSTMT x1495)]
   and cvtWHILE_STMT {cond=x1511, fixtures=opt1513, body=x1517, labels=ls1519} = 
          PrettyRep.Rec [("cond", cvtEXPR x1511), ("fixtures", 
       (case opt1513 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1512 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1512))
       )), ("body", cvtSTMT x1517), ("labels", PrettyRep.List (List.map (fn x1518 => 
                                                                               cvtIDENT x1518
                                                                        ) ls1519))]
   and cvtDIRECTIVES {pragmas=ls1533, defns=ls1538, head=opt1543, body=ls1548, 
          loc=opt1553} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1532 => 
                                                                                    cvtPRAGMA x1532
                                                                             ) ls1533)), 
          ("defns", PrettyRep.List (List.map (fn x1537 => cvtDEFN x1537
                                             ) ls1538)), ("head", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1542))
       )), ("body", PrettyRep.List (List.map (fn x1547 => cvtSTMT x1547
                                             ) ls1548)), ("loc", 
       (case opt1553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1552 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1552))
       ))]
   and cvtCASE {label=opt1569, inits=opt1574, body=x1578} = PrettyRep.Rec [("label", 
          
       (case opt1569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1568))
       )), ("inits", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1573))
       )), ("body", cvtBLOCK x1578)]
   and cvtTYPE_CASE {ty=opt1587, body=x1591} = PrettyRep.Rec [("ty", 
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1586))
       )), ("body", cvtSTMT x1591)]
   and cvtCATCH_CLAUSE {bindings=x1597, ty=x1598, fixtures=opt1600, block=x1604} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1597), ("ty", cvtTYPE_EXPR x1598), 
          ("fixtures", 
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1599 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1599))
       )), ("block", cvtBLOCK x1604)]
   and cvtFUNC_NAME {kind=x1614, ident=x1615} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1614), 
          ("ident", cvtIDENT x1615)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1621, getter=opt1623, setter=opt1628} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1621), ("getter", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1622 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1622))
       )), ("setter", 
       (case opt1628 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1627 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1627))
       ))]
   and cvtPACKAGE {name=ls1640, block=x1644} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1639 => 
                                                                                                       cvtIDENT x1639
                                                                                                ) ls1640)), 
          ("block", cvtBLOCK x1644)]
   and cvtPROGRAM {packages=ls1651, fixtures=opt1656, block=x1660} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1650 => cvtPACKAGE x1650
                                   ) ls1651)), ("fixtures", 
       (case opt1656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1655 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1655))
       )), ("block", cvtBLOCK x1660)]
end

