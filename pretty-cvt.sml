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
     | cvtLITERAL (LiteralContextualOctInteger s1056) = PrettyRep.Ctor ("LiteralContextualOctInteger", 
          SOME (PrettyRep.String s1056))
     | cvtLITERAL (LiteralDouble r1059) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1059))
     | cvtLITERAL (LiteralDecimal d1062) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1062))
     | cvtLITERAL (LiteralInt i1065) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1065))
     | cvtLITERAL (LiteralUInt u1068) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1068))
     | cvtLITERAL (LiteralBoolean b1071) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1071))
     | cvtLITERAL (LiteralString s1074) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1074))
     | cvtLITERAL (LiteralArray{exprs=ls1078, ty=opt1083}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1077 => 
                                                                         cvtEXPR x1077
                                                                  ) ls1078)), 
          ("ty", 
       (case opt1083 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1082 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1082))
       ))]))
     | cvtLITERAL (LiteralXML ls1095) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1094 => 
                                                                                                            cvtEXPR x1094
                                                                                                     ) ls1095)))
     | cvtLITERAL (LiteralNamespace x1101) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1101))
     | cvtLITERAL (LiteralObject{expr=ls1105, ty=opt1110}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1104 => 
                                                                        cvtFIELD x1104
                                                                 ) ls1105)), 
          ("ty", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1109 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1109))
       ))]))
     | cvtLITERAL (LiteralFunction x1121) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1121))
     | cvtLITERAL (LiteralRegExp{str=s1124}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1124)]))
   and cvtBLOCK (Block x1130) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1130))
   and cvtFIXTURE (NamespaceFixture x1133) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1133))
     | cvtFIXTURE (ClassFixture x1136) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1136))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1141) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1141))
     | cvtFIXTURE (MethodFixture{func=x1144, ty=x1145, readOnly=b1146, override=b1147, 
          final=b1148}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1144), ("ty", cvtTYPE_EXPR x1145), ("readOnly", PrettyRep.Bool b1146), 
          ("override", PrettyRep.Bool b1147), ("final", PrettyRep.Bool b1148)]))
     | cvtFIXTURE (ValFixture{ty=x1162, readOnly=b1163}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1162), ("readOnly", PrettyRep.Bool b1163)]))
     | cvtFIXTURE (VirtualValFixture x1171) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1171))
   and cvtBINDINGS (ls1175, ls1180) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1174 => 
                                                                                       cvtBINDING x1174
                                                                                ) ls1175), 
          PrettyRep.List (List.map (fn x1179 => cvtINIT_STEP x1179
                                   ) ls1180)]
   and cvtFIXTURES ls1188 = PrettyRep.List (List.map (fn (x1185, x1186) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1185, 
                                                            cvtFIXTURE x1186]
                                                     ) ls1188)
   and cvtINITS ls1195 = PrettyRep.List (List.map (fn (x1192, x1193) => PrettyRep.Tuple [cvtFIXTURE_NAME x1192, 
                                                         cvtEXPR x1193]
                                                  ) ls1195)
   and cvtHEAD (x1199, x1200) = PrettyRep.Tuple [cvtFIXTURES x1199, cvtINITS x1200]
   and cvtFIELD {kind=x1202, name=x1203, init=x1204} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1202), ("name", cvtIDENT_EXPR x1203), ("init", cvtEXPR x1204)]
   and cvtFIELD_TYPE {name=x1212, ty=x1213} = PrettyRep.Rec [("name", cvtIDENT x1212), 
          ("ty", cvtTYPE_EXPR x1213)]
   and cvtFUNC_TYPE {typeParams=ls1220, params=ls1225, result=x1229, thisType=opt1231, 
          hasRest=b1235, minArgs=n1236} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1219 => 
                                                                                                        cvtIDENT x1219
                                                                                                 ) ls1220)), 
          ("params", PrettyRep.List (List.map (fn x1224 => cvtTYPE_EXPR x1224
                                              ) ls1225)), ("result", cvtTYPE_EXPR x1229), 
          ("thisType", 
       (case opt1231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1230 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1230))
       )), ("hasRest", PrettyRep.Bool b1235), ("minArgs", PrettyRep.Int n1236)]
   and cvtFUNC_DEFN {kind=x1250, ns=opt1252, final=b1256, override=b1257, prototype=b1258, 
          static=b1259, func=x1260} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1250), 
          ("ns", 
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1251))
       )), ("final", PrettyRep.Bool b1256), ("override", PrettyRep.Bool b1257), 
          ("prototype", PrettyRep.Bool b1258), ("static", PrettyRep.Bool b1259), 
          ("func", cvtFUNC x1260)]
   and cvtCTOR_DEFN x1276 = cvtCTOR x1276
   and cvtVAR_DEFN {kind=x1277, ns=opt1279, static=b1283, prototype=b1284, 
          bindings=x1285} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1277), 
          ("ns", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1278))
       )), ("static", PrettyRep.Bool b1283), ("prototype", PrettyRep.Bool b1284), 
          ("bindings", cvtBINDINGS x1285)]
   and cvtNAMESPACE_DEFN {ident=x1297, ns=opt1299, init=opt1304} = PrettyRep.Rec [("ident", 
          cvtIDENT x1297), ("ns", 
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1298))
       )), ("init", 
       (case opt1304 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1303 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1303))
       ))]
   and cvtCLASS_DEFN {ident=x1315, ns=opt1317, nonnullable=b1321, dynamic=b1322, 
          final=b1323, params=ls1325, extends=opt1330, implements=ls1335, classDefns=ls1340, 
          instanceDefns=ls1345, instanceStmts=ls1350, ctorDefn=opt1355} = PrettyRep.Rec [("ident", 
          cvtIDENT x1315), ("ns", 
       (case opt1317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1316 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1316))
       )), ("nonnullable", PrettyRep.Bool b1321), ("dynamic", PrettyRep.Bool b1322), 
          ("final", PrettyRep.Bool b1323), ("params", PrettyRep.List (List.map (fn x1324 => 
                                                                                      cvtIDENT x1324
                                                                               ) ls1325)), 
          ("extends", 
       (case opt1330 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1329 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1329))
       )), ("implements", PrettyRep.List (List.map (fn x1334 => cvtIDENT_EXPR x1334
                                                   ) ls1335)), ("classDefns", 
          PrettyRep.List (List.map (fn x1339 => cvtDEFN x1339
                                   ) ls1340)), ("instanceDefns", PrettyRep.List (List.map (fn x1344 => 
                                                                                                 cvtDEFN x1344
                                                                                          ) ls1345)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1349 => cvtSTMT x1349
                                                     ) ls1350)), ("ctorDefn", 
          
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1354))
       ))]
   and cvtINTERFACE_DEFN {ident=x1384, ns=opt1386, nonnullable=b1390, params=ls1392, 
          extends=ls1397, block=x1401} = PrettyRep.Rec [("ident", cvtIDENT x1384), 
          ("ns", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1385))
       )), ("nonnullable", PrettyRep.Bool b1390), ("params", PrettyRep.List (List.map (fn x1391 => 
                                                                                             cvtIDENT x1391
                                                                                      ) ls1392)), 
          ("extends", PrettyRep.List (List.map (fn x1396 => cvtIDENT_EXPR x1396
                                               ) ls1397)), ("block", cvtBLOCK x1401)]
   and cvtTYPE_DEFN {ident=x1415, ns=opt1417, init=x1421} = PrettyRep.Rec [("ident", 
          cvtIDENT x1415), ("ns", 
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1416 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1416))
       )), ("init", cvtTYPE_EXPR x1421)]
   and cvtFOR_ENUM_STMT {isEach=b1429, defn=opt1431, obj=x1435, fixtures=opt1437, 
          next=x1441, labels=ls1443, body=x1447} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1429), ("defn", 
       (case opt1431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1430 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1430))
       )), ("obj", cvtEXPR x1435), ("fixtures", 
       (case opt1437 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1436 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1436))
       )), ("next", cvtSTMT x1441), ("labels", PrettyRep.List (List.map (fn x1442 => 
                                                                               cvtIDENT x1442
                                                                        ) ls1443)), 
          ("body", cvtSTMT x1447)]
   and cvtFOR_STMT {fixtures=opt1464, defn=opt1469, init=ls1474, cond=x1478, 
          update=x1479, labels=ls1481, body=x1485} = PrettyRep.Rec [("fixtures", 
          
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1463 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1463))
       )), ("defn", 
       (case opt1469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1468 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1468))
       )), ("init", PrettyRep.List (List.map (fn x1473 => cvtSTMT x1473
                                             ) ls1474)), ("cond", cvtEXPR x1478), 
          ("update", cvtEXPR x1479), ("labels", PrettyRep.List (List.map (fn x1480 => 
                                                                                cvtIDENT x1480
                                                                         ) ls1481)), 
          ("body", cvtSTMT x1485)]
   and cvtWHILE_STMT {cond=x1501, fixtures=opt1503, body=x1507, labels=ls1509} = 
          PrettyRep.Rec [("cond", cvtEXPR x1501), ("fixtures", 
       (case opt1503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1502 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1502))
       )), ("body", cvtSTMT x1507), ("labels", PrettyRep.List (List.map (fn x1508 => 
                                                                               cvtIDENT x1508
                                                                        ) ls1509))]
   and cvtDIRECTIVES {pragmas=ls1523, defns=ls1528, head=opt1533, body=ls1538, 
          loc=opt1543} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1522 => 
                                                                                    cvtPRAGMA x1522
                                                                             ) ls1523)), 
          ("defns", PrettyRep.List (List.map (fn x1527 => cvtDEFN x1527
                                             ) ls1528)), ("head", 
       (case opt1533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1532 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1532))
       )), ("body", PrettyRep.List (List.map (fn x1537 => cvtSTMT x1537
                                             ) ls1538)), ("loc", 
       (case opt1543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1542))
       ))]
   and cvtCASE {label=opt1559, inits=opt1564, body=x1568} = PrettyRep.Rec [("label", 
          
       (case opt1559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1558 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1558))
       )), ("inits", 
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1563))
       )), ("body", cvtBLOCK x1568)]
   and cvtTYPE_CASE {ty=opt1577, body=x1581} = PrettyRep.Rec [("ty", 
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1576))
       )), ("body", cvtSTMT x1581)]
   and cvtCATCH_CLAUSE {bindings=x1587, ty=x1588, fixtures=opt1590, inits=opt1595, 
          block=x1599} = PrettyRep.Rec [("bindings", cvtBINDINGS x1587), ("ty", 
          cvtTYPE_EXPR x1588), ("fixtures", 
       (case opt1590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1589 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1589))
       )), ("inits", 
       (case opt1595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1594 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1594))
       )), ("block", cvtBLOCK x1599)]
   and cvtFUNC_NAME {kind=x1611, ident=x1612} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1611), 
          ("ident", cvtIDENT x1612)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1618, getter=opt1620, setter=opt1625} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1618), ("getter", 
       (case opt1620 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1619 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1619))
       )), ("setter", 
       (case opt1625 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1624 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1624))
       ))]
   and cvtPACKAGE {name=ls1637, block=x1641} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1636 => 
                                                                                                       cvtIDENT x1636
                                                                                                ) ls1637)), 
          ("block", cvtBLOCK x1641)]
   and cvtPROGRAM {packages=ls1648, fixtures=opt1653, block=x1657} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1647 => cvtPACKAGE x1647
                                   ) ls1648)), ("fixtures", 
       (case opt1653 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1652 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1652))
       )), ("block", cvtBLOCK x1657)]
end

