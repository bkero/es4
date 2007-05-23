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
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt86) = PrettyRep.Ctor ("Plus", SOME 
       (case opt86 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x85 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x85))
       ))
     | cvtBINOP (Minus opt93) = PrettyRep.Ctor ("Minus", SOME 
       (case opt93 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x92 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x92))
       ))
     | cvtBINOP (Times opt100) = PrettyRep.Ctor ("Times", SOME 
       (case opt100 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x99 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x99))
       ))
     | cvtBINOP (Divide opt107) = PrettyRep.Ctor ("Divide", SOME 
       (case opt107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x106 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x106))
       ))
     | cvtBINOP (Remainder opt114) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x113 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x113))
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
     | cvtBINOP (Equals opt131) = PrettyRep.Ctor ("Equals", SOME 
       (case opt131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x130 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x130))
       ))
     | cvtBINOP (NotEquals opt138) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt138 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x137 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x137))
       ))
     | cvtBINOP (StrictEquals opt145) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt145 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x144 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x144))
       ))
     | cvtBINOP (StrictNotEquals opt152) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt152 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x151 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x151))
       ))
     | cvtBINOP (Less opt159) = PrettyRep.Ctor ("Less", SOME 
       (case opt159 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x158 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x158))
       ))
     | cvtBINOP (LessOrEqual opt166) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x165 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x165))
       ))
     | cvtBINOP (Greater opt173) = PrettyRep.Ctor ("Greater", SOME 
       (case opt173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x172 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x172))
       ))
     | cvtBINOP (GreaterOrEqual opt180) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x179 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x179))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt189) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt189 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x188 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x188))
       ))
     | cvtASSIGNOP (AssignMinus opt196) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x195 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x195))
       ))
     | cvtASSIGNOP (AssignTimes opt203) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x202 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x202))
       ))
     | cvtASSIGNOP (AssignDivide opt210) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x209 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x209))
       ))
     | cvtASSIGNOP (AssignRemainder opt217) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt217 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x216 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x216))
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
     | cvtUNOP (PreIncrement opt235) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x234 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x234))
       ))
     | cvtUNOP (PreDecrement opt242) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x241 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x241))
       ))
     | cvtUNOP (PostIncrement opt249) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x248 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x248))
       ))
     | cvtUNOP (PostDecrement opt256) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x255 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x255))
       ))
     | cvtUNOP (UnaryPlus opt263) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x262 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x262))
       ))
     | cvtUNOP (UnaryMinus opt270) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x269))
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
   and cvtPRAGMA (UseNamespace x287) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x287))
     | cvtPRAGMA (UseDefaultNamespace x290) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x290))
     | cvtPRAGMA (UseNumber x293) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x293))
     | cvtPRAGMA (UseRounding r296) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r296))
     | cvtPRAGMA (UsePrecision n299) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n299))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls305, name=x309, alias=opt311}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x304 => 
                                                                           cvtIDENT x304
                                                                    ) ls305)), 
          ("name", cvtIDENT x309), ("alias", 
       (case opt311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x310 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x310))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x330, nonnullable=b331, extends=opt333, implements=ls338, 
          classFixtures=x342, instanceFixtures=x343, instanceInits=x344, constructor=opt346, 
          classType=x350, instanceType=x351}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x330), ("nonnullable", PrettyRep.Bool b331), ("extends", 
          
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
   and cvtCTOR (Ctor{settings=x375, superArgs=ls377, func=x381}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x375), ("superArgs", PrettyRep.List (List.map (fn x376 => 
                                                                                                         cvtEXPR x376
                                                                                                  ) ls377)), 
          ("func", cvtFUNC x381)]))
   and cvtFUNC (Func{name=x391, fsig=x392, isNative=b393, block=x394, param=x395, 
          defaults=ls397, ty=x401}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x391), ("fsig", cvtFUNC_SIG x392), ("isNative", PrettyRep.Bool b393), 
          ("block", cvtBLOCK x394), ("param", cvtHEAD x395), ("defaults", PrettyRep.List (List.map (fn x396 => 
                                                                                                          cvtEXPR x396
                                                                                                   ) ls397)), 
          ("ty", cvtFUNC_TYPE x401)]))
   and cvtDEFN (ClassDefn x419) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x419))
     | cvtDEFN (VariableDefn x422) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x422))
     | cvtDEFN (FunctionDefn x425) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x425))
     | cvtDEFN (ConstructorDefn x428) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x428))
     | cvtDEFN (InterfaceDefn x431) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x431))
     | cvtDEFN (NamespaceDefn x434) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x434))
     | cvtDEFN (TypeDefn x437) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x437))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls441, params=x445, paramTypes=ls447, 
          defaults=ls452, ctorInits=opt463, returnType=x467, thisType=opt469, 
          hasRest=b473}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x440 => cvtIDENT x440
                                   ) ls441)), ("params", cvtBINDINGS x445), 
          ("paramTypes", PrettyRep.List (List.map (fn x446 => cvtTYPE_EXPR x446
                                                  ) ls447)), ("defaults", PrettyRep.List (List.map (fn x451 => 
                                                                                                          cvtEXPR x451
                                                                                                   ) ls452)), 
          ("ctorInits", 
       (case opt463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x456, ls458) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x456, 
            PrettyRep.List (List.map (fn x457 => cvtEXPR x457
                                     ) ls458)]))
       )), ("returnType", cvtTYPE_EXPR x467), ("thisType", 
       (case opt469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x468 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x468))
       )), ("hasRest", PrettyRep.Bool b473)]))
   and cvtBINDING (Binding{ident=x493, ty=x494}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x493), ("ty", cvtTYPE_EXPR x494)]))
   and cvtBINDING_IDENT (TempIdent n502) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n502))
     | cvtBINDING_IDENT (ParamIdent n505) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n505))
     | cvtBINDING_IDENT (PropIdent x508) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x508))
   and cvtINIT_STEP (InitStep(x511, x512)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x511, 
          cvtEXPR x512]))
     | cvtINIT_STEP (AssignStep(x516, x517)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x516, cvtEXPR x517]))
   and cvtTYPE_EXPR (SpecialType x521) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x521))
     | cvtTYPE_EXPR (UnionType ls525) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x524 => 
                                                                                                           cvtTYPE_EXPR x524
                                                                                                    ) ls525)))
     | cvtTYPE_EXPR (ArrayType ls532) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x531 => 
                                                                                                           cvtTYPE_EXPR x531
                                                                                                    ) ls532)))
     | cvtTYPE_EXPR (TypeName x538) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x538))
     | cvtTYPE_EXPR (ElementTypeRef(x541, n542)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x541, PrettyRep.Int n542]))
     | cvtTYPE_EXPR (FieldTypeRef(x546, x547)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x546, cvtIDENT x547]))
     | cvtTYPE_EXPR (FunctionType x551) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x551))
     | cvtTYPE_EXPR (ObjectType ls555) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x554 => 
                                                                                                             cvtFIELD_TYPE x554
                                                                                                      ) ls555)))
     | cvtTYPE_EXPR (AppType{base=x561, args=ls563}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x561), ("args", PrettyRep.List (List.map (fn x562 => 
                                                                                                     cvtTYPE_EXPR x562
                                                                                              ) ls563))]))
     | cvtTYPE_EXPR (NullableType{expr=x574, nullable=b575}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x574), ("nullable", PrettyRep.Bool b575)]))
     | cvtTYPE_EXPR (InstanceType{name=x583, typeParams=ls585, ty=x589, isDynamic=b590}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x583), 
          ("typeParams", PrettyRep.List (List.map (fn x584 => cvtIDENT x584
                                                  ) ls585)), ("ty", cvtTYPE_EXPR x589), 
          ("isDynamic", PrettyRep.Bool b590)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x603) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x603))
     | cvtSTMT (InitStmt{kind=x606, ns=opt608, prototype=b612, static=b613, 
          temps=x614, inits=ls616}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x606), ("ns", 
       (case opt608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x607 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x607))
       )), ("prototype", PrettyRep.Bool b612), ("static", PrettyRep.Bool b613), 
          ("temps", cvtBINDINGS x614), ("inits", PrettyRep.List (List.map (fn x615 => 
                                                                                 cvtINIT_STEP x615
                                                                          ) ls616))]))
     | cvtSTMT (ClassBlock{ns=opt636, ident=x640, name=opt642, block=x646}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x635 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x635))
       )), ("ident", cvtIDENT x640), ("name", 
       (case opt642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x641 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x641))
       )), ("block", cvtBLOCK x646)]))
     | cvtSTMT (ForInStmt x658) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x658))
     | cvtSTMT (ThrowStmt x661) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x661))
     | cvtSTMT (ReturnStmt x664) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x664))
     | cvtSTMT (BreakStmt opt668) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x667 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x667))
       ))
     | cvtSTMT (ContinueStmt opt675) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x674 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x674))
       ))
     | cvtSTMT (BlockStmt x681) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x681))
     | cvtSTMT (LabeledStmt(x684, x685)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x684, 
          cvtSTMT x685]))
     | cvtSTMT (LetStmt x689) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x689))
     | cvtSTMT (WhileStmt x692) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x692))
     | cvtSTMT (DoWhileStmt x695) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x695))
     | cvtSTMT (ForStmt x698) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x698))
     | cvtSTMT (IfStmt{cnd=x701, thn=x702, els=x703}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x701), ("thn", cvtSTMT x702), 
          ("els", cvtSTMT x703)]))
     | cvtSTMT (WithStmt{obj=x713, ty=x714, body=x715}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x713), ("ty", cvtTYPE_EXPR x714), 
          ("body", cvtSTMT x715)]))
     | cvtSTMT (TryStmt{block=x725, catches=ls727, finally=opt732}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x725), ("catches", PrettyRep.List (List.map (fn x726 => 
                                                                                                     cvtCATCH_CLAUSE x726
                                                                                              ) ls727)), 
          ("finally", 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x731))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt746, cond=x750, labels=ls752, cases=ls757}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x745 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x745))
       )), ("cond", cvtEXPR x750), ("labels", PrettyRep.List (List.map (fn x751 => 
                                                                              cvtIDENT x751
                                                                       ) ls752)), 
          ("cases", PrettyRep.List (List.map (fn x756 => cvtCASE x756
                                             ) ls757))]))
     | cvtSTMT (SwitchTypeStmt{cond=x772, ty=x773, cases=ls775}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x772), ("ty", cvtTYPE_EXPR x773), 
          ("cases", PrettyRep.List (List.map (fn x774 => cvtTYPE_CASE x774
                                             ) ls775))]))
     | cvtSTMT (DXNStmt{expr=x788}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x788)]))
   and cvtEXPR (TernaryExpr(x794, x795, x796)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x794, cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (BinaryExpr(x800, x801, x802)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x800, cvtEXPR x801, cvtEXPR x802]))
     | cvtEXPR (BinaryTypeExpr(x806, x807, x808)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x806, cvtEXPR x807, cvtTYPE_EXPR x808]))
     | cvtEXPR (ExpectedTypeExpr(x812, x813)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x812, cvtEXPR x813]))
     | cvtEXPR (UnaryExpr(x817, x818)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x817, 
          cvtEXPR x818]))
     | cvtEXPR (TypeExpr x822) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x822))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt827) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt827 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x826 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x826))
       ))
     | cvtEXPR (SuperExpr opt834) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt834 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x833 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x833))
       ))
     | cvtEXPR (LiteralExpr x840) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x840))
     | cvtEXPR (CallExpr{func=x843, actuals=ls845}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x843), ("actuals", PrettyRep.List (List.map (fn x844 => 
                                                                                                   cvtEXPR x844
                                                                                            ) ls845))]))
     | cvtEXPR (ApplyTypeExpr{expr=x856, actuals=ls858}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x856), ("actuals", PrettyRep.List (List.map (fn x857 => 
                                                                                                   cvtTYPE_EXPR x857
                                                                                            ) ls858))]))
     | cvtEXPR (LetExpr{defs=x869, body=x870, head=opt872}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x869), ("body", cvtEXPR x870), 
          ("head", 
       (case opt872 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x871 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x871))
       ))]))
     | cvtEXPR (NewExpr{obj=x885, actuals=ls887}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x885), ("actuals", PrettyRep.List (List.map (fn x886 => 
                                                                                                  cvtEXPR x886
                                                                                           ) ls887))]))
     | cvtEXPR (ObjectRef{base=x898, ident=x899, loc=opt901}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x898), ("ident", cvtIDENT_EXPR x899), 
          ("loc", 
       (case opt901 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x900 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x900))
       ))]))
     | cvtEXPR (LexicalRef{ident=x914, loc=opt916}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x914), ("loc", 
       (case opt916 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x915 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x915))
       ))]))
     | cvtEXPR (SetExpr(x927, x928, x929)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x927, 
          cvtEXPR x928, cvtEXPR x929]))
     | cvtEXPR (ListExpr ls934) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x933 => 
                                                                                                    cvtEXPR x933
                                                                                             ) ls934)))
     | cvtEXPR (InitExpr(x940, x941, x942)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x940, 
          cvtHEAD x941, cvtINITS x942]))
     | cvtEXPR (SliceExpr(x946, x947, x948)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x946, cvtEXPR x947, cvtEXPR x948]))
     | cvtEXPR (GetTemp n952) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n952))
     | cvtEXPR (GetParam n955) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n955))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n961) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n961))
     | cvtFIXTURE_NAME (PropName x964) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x964))
   and cvtIDENT_EXPR (Identifier{ident=x967, openNamespaces=ls973}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x967), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls969 => PrettyRep.List (List.map (fn x968 => 
                                                                                cvtNAMESPACE x968
                                                                         ) ls969)
                                   ) ls973))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x984, expr=x985}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x984), ("expr", cvtEXPR x985)]))
     | cvtIDENT_EXPR (AttributeIdentifier x993) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x993))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x996, openNamespaces=ls1002}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x996), ("openNamespaces", PrettyRep.List (List.map (fn ls998 => 
                                                                            PrettyRep.List (List.map (fn x997 => 
                                                                                                            cvtNAMESPACE x997
                                                                                                     ) ls998)
                                                                     ) ls1002))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1013, ident=s1014}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1013), ("ident", PrettyRep.UniStr s1014)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1022, typeArgs=ls1024}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1022), ("typeArgs", 
          PrettyRep.List (List.map (fn x1023 => cvtTYPE_EXPR x1023
                                   ) ls1024))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1036, x1040)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1035 => cvtIDENT x1035
                                                          ) ls1036), cvtIDENT_EXPR x1040]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1047) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1047))
     | cvtLITERAL (LiteralContextualDecimalInteger s1050) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1050))
     | cvtLITERAL (LiteralContextualHexInteger s1053) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1053))
     | cvtLITERAL (LiteralDouble r1056) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1056))
     | cvtLITERAL (LiteralDecimal d1059) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1059))
     | cvtLITERAL (LiteralInt i1062) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1062))
     | cvtLITERAL (LiteralUInt u1065) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1065))
     | cvtLITERAL (LiteralBoolean b1068) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1068))
     | cvtLITERAL (LiteralString s1071) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1071))
     | cvtLITERAL (LiteralArray{exprs=ls1075, ty=opt1080}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1074 => 
                                                                         cvtEXPR x1074
                                                                  ) ls1075)), 
          ("ty", 
       (case opt1080 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1079 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1079))
       ))]))
     | cvtLITERAL (LiteralXML ls1092) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1091 => 
                                                                                                            cvtEXPR x1091
                                                                                                     ) ls1092)))
     | cvtLITERAL (LiteralNamespace x1098) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1098))
     | cvtLITERAL (LiteralObject{expr=ls1102, ty=opt1107}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1101 => 
                                                                        cvtFIELD x1101
                                                                 ) ls1102)), 
          ("ty", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1106))
       ))]))
     | cvtLITERAL (LiteralFunction x1118) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1118))
     | cvtLITERAL (LiteralRegExp{str=s1121}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1121)]))
   and cvtBLOCK (Block x1127) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1127))
   and cvtFIXTURE (NamespaceFixture x1130) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1130))
     | cvtFIXTURE (ClassFixture x1133) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1133))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1138) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1138))
     | cvtFIXTURE (MethodFixture{func=x1141, ty=x1142, readOnly=b1143, override=b1144, 
          final=b1145}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1141), ("ty", cvtTYPE_EXPR x1142), ("readOnly", PrettyRep.Bool b1143), 
          ("override", PrettyRep.Bool b1144), ("final", PrettyRep.Bool b1145)]))
     | cvtFIXTURE (ValFixture{ty=x1159, readOnly=b1160}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1159), ("readOnly", PrettyRep.Bool b1160)]))
     | cvtFIXTURE (VirtualValFixture x1168) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1168))
   and cvtBINDINGS (ls1172, ls1177) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1171 => 
                                                                                       cvtBINDING x1171
                                                                                ) ls1172), 
          PrettyRep.List (List.map (fn x1176 => cvtINIT_STEP x1176
                                   ) ls1177)]
   and cvtFIXTURES ls1185 = PrettyRep.List (List.map (fn (x1182, x1183) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1182, 
                                                            cvtFIXTURE x1183]
                                                     ) ls1185)
   and cvtINITS ls1192 = PrettyRep.List (List.map (fn (x1189, x1190) => PrettyRep.Tuple [cvtFIXTURE_NAME x1189, 
                                                         cvtEXPR x1190]
                                                  ) ls1192)
   and cvtHEAD (x1196, x1197) = PrettyRep.Tuple [cvtFIXTURES x1196, cvtINITS x1197]
   and cvtFIELD {kind=x1199, name=x1200, init=x1201} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1199), ("name", cvtIDENT_EXPR x1200), ("init", cvtEXPR x1201)]
   and cvtFIELD_TYPE {name=x1209, ty=x1210} = PrettyRep.Rec [("name", cvtIDENT x1209), 
          ("ty", cvtTYPE_EXPR x1210)]
   and cvtFUNC_TYPE {typeParams=ls1217, params=ls1222, result=x1226, thisType=opt1228, 
          hasRest=b1232, minArgs=n1233} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1216 => 
                                                                                                        cvtIDENT x1216
                                                                                                 ) ls1217)), 
          ("params", PrettyRep.List (List.map (fn x1221 => cvtTYPE_EXPR x1221
                                              ) ls1222)), ("result", cvtTYPE_EXPR x1226), 
          ("thisType", 
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1227))
       )), ("hasRest", PrettyRep.Bool b1232), ("minArgs", PrettyRep.Int n1233)]
   and cvtFUNC_DEFN {kind=x1247, ns=opt1249, final=b1253, override=b1254, prototype=b1255, 
          static=b1256, func=x1257} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1247), 
          ("ns", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1248))
       )), ("final", PrettyRep.Bool b1253), ("override", PrettyRep.Bool b1254), 
          ("prototype", PrettyRep.Bool b1255), ("static", PrettyRep.Bool b1256), 
          ("func", cvtFUNC x1257)]
   and cvtCTOR_DEFN x1273 = cvtCTOR x1273
   and cvtVAR_DEFN {kind=x1274, ns=opt1276, static=b1280, prototype=b1281, 
          bindings=x1282} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1274), 
          ("ns", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1275))
       )), ("static", PrettyRep.Bool b1280), ("prototype", PrettyRep.Bool b1281), 
          ("bindings", cvtBINDINGS x1282)]
   and cvtNAMESPACE_DEFN {ident=x1294, ns=opt1296, init=opt1301} = PrettyRep.Rec [("ident", 
          cvtIDENT x1294), ("ns", 
       (case opt1296 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1295 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1295))
       )), ("init", 
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1300))
       ))]
   and cvtCLASS_DEFN {ident=x1312, ns=opt1314, nonnullable=b1318, dynamic=b1319, 
          final=b1320, params=ls1322, extends=opt1327, implements=ls1332, classDefns=ls1337, 
          instanceDefns=ls1342, instanceStmts=ls1347, ctorDefn=opt1352} = PrettyRep.Rec [("ident", 
          cvtIDENT x1312), ("ns", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1313))
       )), ("nonnullable", PrettyRep.Bool b1318), ("dynamic", PrettyRep.Bool b1319), 
          ("final", PrettyRep.Bool b1320), ("params", PrettyRep.List (List.map (fn x1321 => 
                                                                                      cvtIDENT x1321
                                                                               ) ls1322)), 
          ("extends", 
       (case opt1327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1326 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1326))
       )), ("implements", PrettyRep.List (List.map (fn x1331 => cvtIDENT_EXPR x1331
                                                   ) ls1332)), ("classDefns", 
          PrettyRep.List (List.map (fn x1336 => cvtDEFN x1336
                                   ) ls1337)), ("instanceDefns", PrettyRep.List (List.map (fn x1341 => 
                                                                                                 cvtDEFN x1341
                                                                                          ) ls1342)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1346 => cvtSTMT x1346
                                                     ) ls1347)), ("ctorDefn", 
          
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1351))
       ))]
   and cvtINTERFACE_DEFN {ident=x1381, ns=opt1383, nonnullable=b1387, params=ls1389, 
          extends=ls1394, block=x1398} = PrettyRep.Rec [("ident", cvtIDENT x1381), 
          ("ns", 
       (case opt1383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1382 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1382))
       )), ("nonnullable", PrettyRep.Bool b1387), ("params", PrettyRep.List (List.map (fn x1388 => 
                                                                                             cvtIDENT x1388
                                                                                      ) ls1389)), 
          ("extends", PrettyRep.List (List.map (fn x1393 => cvtIDENT_EXPR x1393
                                               ) ls1394)), ("block", cvtBLOCK x1398)]
   and cvtTYPE_DEFN {ident=x1412, ns=opt1414, init=x1418} = PrettyRep.Rec [("ident", 
          cvtIDENT x1412), ("ns", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1413))
       )), ("init", cvtTYPE_EXPR x1418)]
   and cvtFOR_ENUM_STMT {isEach=b1426, defn=opt1428, obj=x1432, fixtures=opt1434, 
          next=x1438, labels=ls1440, body=x1444} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1426), ("defn", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1427 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1427))
       )), ("obj", cvtEXPR x1432), ("fixtures", 
       (case opt1434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1433))
       )), ("next", cvtSTMT x1438), ("labels", PrettyRep.List (List.map (fn x1439 => 
                                                                               cvtIDENT x1439
                                                                        ) ls1440)), 
          ("body", cvtSTMT x1444)]
   and cvtFOR_STMT {fixtures=opt1461, defn=opt1466, init=ls1471, cond=x1475, 
          update=x1476, labels=ls1478, body=x1482} = PrettyRep.Rec [("fixtures", 
          
       (case opt1461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1460 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1460))
       )), ("defn", 
       (case opt1466 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1465 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1465))
       )), ("init", PrettyRep.List (List.map (fn x1470 => cvtSTMT x1470
                                             ) ls1471)), ("cond", cvtEXPR x1475), 
          ("update", cvtEXPR x1476), ("labels", PrettyRep.List (List.map (fn x1477 => 
                                                                                cvtIDENT x1477
                                                                         ) ls1478)), 
          ("body", cvtSTMT x1482)]
   and cvtWHILE_STMT {cond=x1498, fixtures=opt1500, body=x1504, labels=ls1506} = 
          PrettyRep.Rec [("cond", cvtEXPR x1498), ("fixtures", 
       (case opt1500 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1499 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1499))
       )), ("body", cvtSTMT x1504), ("labels", PrettyRep.List (List.map (fn x1505 => 
                                                                               cvtIDENT x1505
                                                                        ) ls1506))]
   and cvtDIRECTIVES {pragmas=ls1520, defns=ls1525, head=opt1530, body=ls1535, 
          loc=opt1540} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1519 => 
                                                                                    cvtPRAGMA x1519
                                                                             ) ls1520)), 
          ("defns", PrettyRep.List (List.map (fn x1524 => cvtDEFN x1524
                                             ) ls1525)), ("head", 
       (case opt1530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1529 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1529))
       )), ("body", PrettyRep.List (List.map (fn x1534 => cvtSTMT x1534
                                             ) ls1535)), ("loc", 
       (case opt1540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1539 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1539))
       ))]
   and cvtCASE {label=opt1556, inits=opt1561, body=x1565} = PrettyRep.Rec [("label", 
          
       (case opt1556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1555 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1555))
       )), ("inits", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1560))
       )), ("body", cvtBLOCK x1565)]
   and cvtTYPE_CASE {ty=opt1574, body=x1578} = PrettyRep.Rec [("ty", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1573))
       )), ("body", cvtSTMT x1578)]
   and cvtCATCH_CLAUSE {bindings=x1584, ty=x1585, fixtures=opt1587, inits=opt1592, 
          block=x1596} = PrettyRep.Rec [("bindings", cvtBINDINGS x1584), ("ty", 
          cvtTYPE_EXPR x1585), ("fixtures", 
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1586))
       )), ("inits", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1591))
       )), ("block", cvtBLOCK x1596)]
   and cvtFUNC_NAME {kind=x1608, ident=x1609} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1608), 
          ("ident", cvtIDENT x1609)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1615, getter=opt1617, setter=opt1622} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1615), ("getter", 
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1616 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1616))
       )), ("setter", 
       (case opt1622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1621 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1621))
       ))]
   and cvtPACKAGE {name=ls1634, block=x1638} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1633 => 
                                                                                                       cvtIDENT x1633
                                                                                                ) ls1634)), 
          ("block", cvtBLOCK x1638)]
   and cvtPROGRAM {packages=ls1645, fixtures=opt1650, block=x1654} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1644 => cvtPACKAGE x1644
                                   ) ls1645)), ("fixtures", 
       (case opt1650 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1649 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1649))
       )), ("block", cvtBLOCK x1654)]
end

