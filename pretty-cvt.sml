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
   and cvtIFACE (Iface{name=x375, extends=ls377, instanceFixtures=x381, instanceType=x382}) = 
          PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", cvtNAME x375), 
          ("extends", PrettyRep.List (List.map (fn x376 => cvtNAME x376
                                               ) ls377)), ("instanceFixtures", 
          cvtFIXTURES x381), ("instanceType", cvtTYPE_EXPR x382)]))
   and cvtCTOR (Ctor{settings=x394, superArgs=ls396, func=x400}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x394), ("superArgs", PrettyRep.List (List.map (fn x395 => 
                                                                                                         cvtEXPR x395
                                                                                                  ) ls396)), 
          ("func", cvtFUNC x400)]))
   and cvtFUNC (Func{name=x410, fsig=x411, isNative=b412, block=x413, param=x414, 
          defaults=ls416, ty=x420}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x410), ("fsig", cvtFUNC_SIG x411), ("isNative", PrettyRep.Bool b412), 
          ("block", cvtBLOCK x413), ("param", cvtHEAD x414), ("defaults", PrettyRep.List (List.map (fn x415 => 
                                                                                                          cvtEXPR x415
                                                                                                   ) ls416)), 
          ("ty", cvtFUNC_TYPE x420)]))
   and cvtDEFN (ClassDefn x438) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x438))
     | cvtDEFN (VariableDefn x441) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x441))
     | cvtDEFN (FunctionDefn x444) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x444))
     | cvtDEFN (ConstructorDefn x447) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x447))
     | cvtDEFN (InterfaceDefn x450) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x450))
     | cvtDEFN (NamespaceDefn x453) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x453))
     | cvtDEFN (TypeDefn x456) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x456))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls460, params=x464, paramTypes=ls466, 
          defaults=ls471, ctorInits=opt482, returnType=x486, thisType=opt488, 
          hasRest=b492}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x459 => cvtIDENT x459
                                   ) ls460)), ("params", cvtBINDINGS x464), 
          ("paramTypes", PrettyRep.List (List.map (fn x465 => cvtTYPE_EXPR x465
                                                  ) ls466)), ("defaults", PrettyRep.List (List.map (fn x470 => 
                                                                                                          cvtEXPR x470
                                                                                                   ) ls471)), 
          ("ctorInits", 
       (case opt482 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x475, ls477) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x475, 
            PrettyRep.List (List.map (fn x476 => cvtEXPR x476
                                     ) ls477)]))
       )), ("returnType", cvtTYPE_EXPR x486), ("thisType", 
       (case opt488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x487 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x487))
       )), ("hasRest", PrettyRep.Bool b492)]))
   and cvtBINDING (Binding{ident=x512, ty=x513}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x512), ("ty", cvtTYPE_EXPR x513)]))
   and cvtBINDING_IDENT (TempIdent n521) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n521))
     | cvtBINDING_IDENT (ParamIdent n524) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n524))
     | cvtBINDING_IDENT (PropIdent x527) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x527))
   and cvtINIT_STEP (InitStep(x530, x531)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x530, 
          cvtEXPR x531]))
     | cvtINIT_STEP (AssignStep(x535, x536)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x535, cvtEXPR x536]))
   and cvtTYPE_EXPR (SpecialType x540) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x540))
     | cvtTYPE_EXPR (UnionType ls544) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x543 => 
                                                                                                           cvtTYPE_EXPR x543
                                                                                                    ) ls544)))
     | cvtTYPE_EXPR (ArrayType ls551) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x550 => 
                                                                                                           cvtTYPE_EXPR x550
                                                                                                    ) ls551)))
     | cvtTYPE_EXPR (TypeName x557) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x557))
     | cvtTYPE_EXPR (ElementTypeRef(x560, n561)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x560, PrettyRep.Int n561]))
     | cvtTYPE_EXPR (FieldTypeRef(x565, x566)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x565, cvtIDENT x566]))
     | cvtTYPE_EXPR (FunctionType x570) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x570))
     | cvtTYPE_EXPR (ObjectType ls574) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x573 => 
                                                                                                             cvtFIELD_TYPE x573
                                                                                                      ) ls574)))
     | cvtTYPE_EXPR (AppType{base=x580, args=ls582}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x580), ("args", PrettyRep.List (List.map (fn x581 => 
                                                                                                     cvtTYPE_EXPR x581
                                                                                              ) ls582))]))
     | cvtTYPE_EXPR (NullableType{expr=x593, nullable=b594}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x593), ("nullable", PrettyRep.Bool b594)]))
     | cvtTYPE_EXPR (InstanceType{name=x602, typeParams=ls604, ty=x608, isDynamic=b609}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x602), 
          ("typeParams", PrettyRep.List (List.map (fn x603 => cvtIDENT x603
                                                  ) ls604)), ("ty", cvtTYPE_EXPR x608), 
          ("isDynamic", PrettyRep.Bool b609)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x622) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x622))
     | cvtSTMT (InitStmt{kind=x625, ns=opt627, prototype=b631, static=b632, 
          temps=x633, inits=ls635}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x625), ("ns", 
       (case opt627 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x626 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x626))
       )), ("prototype", PrettyRep.Bool b631), ("static", PrettyRep.Bool b632), 
          ("temps", cvtBINDINGS x633), ("inits", PrettyRep.List (List.map (fn x634 => 
                                                                                 cvtINIT_STEP x634
                                                                          ) ls635))]))
     | cvtSTMT (ClassBlock{ns=opt655, ident=x659, name=opt661, block=x665}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x654 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x654))
       )), ("ident", cvtIDENT x659), ("name", 
       (case opt661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x660 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x660))
       )), ("block", cvtBLOCK x665)]))
     | cvtSTMT (ForInStmt x677) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x677))
     | cvtSTMT (ThrowStmt x680) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x680))
     | cvtSTMT (ReturnStmt x683) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x683))
     | cvtSTMT (BreakStmt opt687) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt687 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x686 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x686))
       ))
     | cvtSTMT (ContinueStmt opt694) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x693 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x693))
       ))
     | cvtSTMT (BlockStmt x700) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x700))
     | cvtSTMT (LabeledStmt(x703, x704)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x703, 
          cvtSTMT x704]))
     | cvtSTMT (LetStmt x708) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x708))
     | cvtSTMT (WhileStmt x711) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x711))
     | cvtSTMT (DoWhileStmt x714) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x714))
     | cvtSTMT (ForStmt x717) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x717))
     | cvtSTMT (IfStmt{cnd=x720, thn=x721, els=x722}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x720), ("thn", cvtSTMT x721), 
          ("els", cvtSTMT x722)]))
     | cvtSTMT (WithStmt{obj=x732, ty=x733, body=x734}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x732), ("ty", cvtTYPE_EXPR x733), 
          ("body", cvtSTMT x734)]))
     | cvtSTMT (TryStmt{block=x744, catches=ls746, finally=opt751}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x744), ("catches", PrettyRep.List (List.map (fn x745 => 
                                                                                                     cvtCATCH_CLAUSE x745
                                                                                              ) ls746)), 
          ("finally", 
       (case opt751 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x750 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x750))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt765, cond=x769, labels=ls771, cases=ls776}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt765 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x764 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x764))
       )), ("cond", cvtEXPR x769), ("labels", PrettyRep.List (List.map (fn x770 => 
                                                                              cvtIDENT x770
                                                                       ) ls771)), 
          ("cases", PrettyRep.List (List.map (fn x775 => cvtCASE x775
                                             ) ls776))]))
     | cvtSTMT (SwitchTypeStmt{cond=x791, ty=x792, cases=ls794}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x791), ("ty", cvtTYPE_EXPR x792), 
          ("cases", PrettyRep.List (List.map (fn x793 => cvtCATCH_CLAUSE x793
                                             ) ls794))]))
     | cvtSTMT (DXNStmt{expr=x807}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x807)]))
   and cvtEXPR (TernaryExpr(x813, x814, x815)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x813, cvtEXPR x814, cvtEXPR x815]))
     | cvtEXPR (BinaryExpr(x819, x820, x821)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x819, cvtEXPR x820, cvtEXPR x821]))
     | cvtEXPR (BinaryTypeExpr(x825, x826, x827)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x825, cvtEXPR x826, cvtTYPE_EXPR x827]))
     | cvtEXPR (ExpectedTypeExpr(x831, x832)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x831, cvtEXPR x832]))
     | cvtEXPR (UnaryExpr(x836, x837)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x836, 
          cvtEXPR x837]))
     | cvtEXPR (TypeExpr x841) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x841))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt846) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt846 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x845 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x845))
       ))
     | cvtEXPR (SuperExpr opt853) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt853 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x852 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x852))
       ))
     | cvtEXPR (LiteralExpr x859) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x859))
     | cvtEXPR (CallExpr{func=x862, actuals=ls864}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x862), ("actuals", PrettyRep.List (List.map (fn x863 => 
                                                                                                   cvtEXPR x863
                                                                                            ) ls864))]))
     | cvtEXPR (ApplyTypeExpr{expr=x875, actuals=ls877}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x875), ("actuals", PrettyRep.List (List.map (fn x876 => 
                                                                                                   cvtTYPE_EXPR x876
                                                                                            ) ls877))]))
     | cvtEXPR (LetExpr{defs=x888, body=x889, head=opt891}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x888), ("body", cvtEXPR x889), 
          ("head", 
       (case opt891 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x890 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x890))
       ))]))
     | cvtEXPR (NewExpr{obj=x904, actuals=ls906}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x904), ("actuals", PrettyRep.List (List.map (fn x905 => 
                                                                                                  cvtEXPR x905
                                                                                           ) ls906))]))
     | cvtEXPR (ObjectRef{base=x917, ident=x918, loc=opt920}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x917), ("ident", cvtIDENT_EXPR x918), 
          ("loc", 
       (case opt920 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x919 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x919))
       ))]))
     | cvtEXPR (LexicalRef{ident=x933, loc=opt935}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x933), ("loc", 
       (case opt935 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x934 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x934))
       ))]))
     | cvtEXPR (SetExpr(x946, x947, x948)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x946, 
          cvtEXPR x947, cvtEXPR x948]))
     | cvtEXPR (ListExpr ls953) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x952 => 
                                                                                                    cvtEXPR x952
                                                                                             ) ls953)))
     | cvtEXPR (InitExpr(x959, x960, x961)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x959, 
          cvtHEAD x960, cvtINITS x961]))
     | cvtEXPR (SliceExpr(x965, x966, x967)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x965, cvtEXPR x966, cvtEXPR x967]))
     | cvtEXPR (GetTemp n971) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n971))
     | cvtEXPR (GetParam n974) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n974))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n980) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n980))
     | cvtFIXTURE_NAME (PropName x983) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x983))
   and cvtIDENT_EXPR (Identifier{ident=x986, openNamespaces=ls992}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x986), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls988 => PrettyRep.List (List.map (fn x987 => 
                                                                                cvtNAMESPACE x987
                                                                         ) ls988)
                                   ) ls992))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1003, expr=x1004}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1003), ("expr", cvtEXPR x1004)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1012) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1012))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1015, openNamespaces=ls1021}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1015), ("openNamespaces", PrettyRep.List (List.map (fn ls1017 => 
                                                                             PrettyRep.List (List.map (fn x1016 => 
                                                                                                             cvtNAMESPACE x1016
                                                                                                      ) ls1017)
                                                                      ) ls1021))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1032, ident=s1033}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1032), ("ident", PrettyRep.UniStr s1033)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1041, typeArgs=ls1043}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1041), ("typeArgs", 
          PrettyRep.List (List.map (fn x1042 => cvtTYPE_EXPR x1042
                                   ) ls1043))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1055, x1059)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1054 => cvtIDENT x1054
                                                          ) ls1055), cvtIDENT_EXPR x1059]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1066) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1066))
     | cvtLITERAL (LiteralContextualDecimalInteger s1069) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1069))
     | cvtLITERAL (LiteralContextualHexInteger s1072) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1072))
     | cvtLITERAL (LiteralContextualOctInteger s1075) = PrettyRep.Ctor ("LiteralContextualOctInteger", 
          SOME (PrettyRep.String s1075))
     | cvtLITERAL (LiteralDouble r1078) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1078))
     | cvtLITERAL (LiteralDecimal d1081) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1081))
     | cvtLITERAL (LiteralInt i1084) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1084))
     | cvtLITERAL (LiteralUInt u1087) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1087))
     | cvtLITERAL (LiteralBoolean b1090) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1090))
     | cvtLITERAL (LiteralString s1093) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1093))
     | cvtLITERAL (LiteralArray{exprs=ls1097, ty=opt1102}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1096 => 
                                                                         cvtEXPR x1096
                                                                  ) ls1097)), 
          ("ty", 
       (case opt1102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1101 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1101))
       ))]))
     | cvtLITERAL (LiteralXML ls1114) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1113 => 
                                                                                                            cvtEXPR x1113
                                                                                                     ) ls1114)))
     | cvtLITERAL (LiteralNamespace x1120) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1120))
     | cvtLITERAL (LiteralObject{expr=ls1124, ty=opt1129}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1123 => 
                                                                        cvtFIELD x1123
                                                                 ) ls1124)), 
          ("ty", 
       (case opt1129 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1128 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1128))
       ))]))
     | cvtLITERAL (LiteralFunction x1140) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1140))
     | cvtLITERAL (LiteralRegExp{str=s1143}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1143)]))
   and cvtBLOCK (Block x1149) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1149))
   and cvtFIXTURE (NamespaceFixture x1152) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1152))
     | cvtFIXTURE (ClassFixture x1155) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1155))
     | cvtFIXTURE (InterfaceFixture x1158) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1158))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1162) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1162))
     | cvtFIXTURE (MethodFixture{func=x1165, ty=x1166, readOnly=b1167, override=b1168, 
          final=b1169}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1165), ("ty", cvtTYPE_EXPR x1166), ("readOnly", PrettyRep.Bool b1167), 
          ("override", PrettyRep.Bool b1168), ("final", PrettyRep.Bool b1169)]))
     | cvtFIXTURE (ValFixture{ty=x1183, readOnly=b1184}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1183), ("readOnly", PrettyRep.Bool b1184)]))
     | cvtFIXTURE (VirtualValFixture x1192) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1192))
   and cvtBINDINGS (ls1196, ls1201) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1195 => 
                                                                                       cvtBINDING x1195
                                                                                ) ls1196), 
          PrettyRep.List (List.map (fn x1200 => cvtINIT_STEP x1200
                                   ) ls1201)]
   and cvtFIXTURES ls1209 = PrettyRep.List (List.map (fn (x1206, x1207) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1206, 
                                                            cvtFIXTURE x1207]
                                                     ) ls1209)
   and cvtINITS ls1216 = PrettyRep.List (List.map (fn (x1213, x1214) => PrettyRep.Tuple [cvtFIXTURE_NAME x1213, 
                                                         cvtEXPR x1214]
                                                  ) ls1216)
   and cvtHEAD (x1220, x1221) = PrettyRep.Tuple [cvtFIXTURES x1220, cvtINITS x1221]
   and cvtFIELD {kind=x1223, name=x1224, init=x1225} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1223), ("name", cvtIDENT_EXPR x1224), ("init", cvtEXPR x1225)]
   and cvtFIELD_TYPE {name=x1233, ty=x1234} = PrettyRep.Rec [("name", cvtIDENT x1233), 
          ("ty", cvtTYPE_EXPR x1234)]
   and cvtFUNC_TYPE {typeParams=ls1241, params=ls1246, result=x1250, thisType=opt1252, 
          hasRest=b1256, minArgs=n1257} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1240 => 
                                                                                                        cvtIDENT x1240
                                                                                                 ) ls1241)), 
          ("params", PrettyRep.List (List.map (fn x1245 => cvtTYPE_EXPR x1245
                                              ) ls1246)), ("result", cvtTYPE_EXPR x1250), 
          ("thisType", 
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1251))
       )), ("hasRest", PrettyRep.Bool b1256), ("minArgs", PrettyRep.Int n1257)]
   and cvtFUNC_DEFN {kind=x1271, ns=opt1273, final=b1277, override=b1278, prototype=b1279, 
          static=b1280, func=x1281} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1271), 
          ("ns", 
       (case opt1273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1272 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1272))
       )), ("final", PrettyRep.Bool b1277), ("override", PrettyRep.Bool b1278), 
          ("prototype", PrettyRep.Bool b1279), ("static", PrettyRep.Bool b1280), 
          ("func", cvtFUNC x1281)]
   and cvtCTOR_DEFN x1297 = cvtCTOR x1297
   and cvtVAR_DEFN {kind=x1298, ns=opt1300, static=b1304, prototype=b1305, 
          bindings=x1306} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1298), 
          ("ns", 
       (case opt1300 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1299 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1299))
       )), ("static", PrettyRep.Bool b1304), ("prototype", PrettyRep.Bool b1305), 
          ("bindings", cvtBINDINGS x1306)]
   and cvtNAMESPACE_DEFN {ident=x1318, ns=opt1320, init=opt1325} = PrettyRep.Rec [("ident", 
          cvtIDENT x1318), ("ns", 
       (case opt1320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1319 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1319))
       )), ("init", 
       (case opt1325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1324 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1324))
       ))]
   and cvtCLASS_DEFN {ident=x1336, ns=opt1338, nonnullable=b1342, dynamic=b1343, 
          final=b1344, params=ls1346, extends=opt1351, implements=ls1356, classDefns=ls1361, 
          instanceDefns=ls1366, instanceStmts=ls1371, ctorDefn=opt1376} = PrettyRep.Rec [("ident", 
          cvtIDENT x1336), ("ns", 
       (case opt1338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1337 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1337))
       )), ("nonnullable", PrettyRep.Bool b1342), ("dynamic", PrettyRep.Bool b1343), 
          ("final", PrettyRep.Bool b1344), ("params", PrettyRep.List (List.map (fn x1345 => 
                                                                                      cvtIDENT x1345
                                                                               ) ls1346)), 
          ("extends", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1350))
       )), ("implements", PrettyRep.List (List.map (fn x1355 => cvtIDENT_EXPR x1355
                                                   ) ls1356)), ("classDefns", 
          PrettyRep.List (List.map (fn x1360 => cvtDEFN x1360
                                   ) ls1361)), ("instanceDefns", PrettyRep.List (List.map (fn x1365 => 
                                                                                                 cvtDEFN x1365
                                                                                          ) ls1366)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1370 => cvtSTMT x1370
                                                     ) ls1371)), ("ctorDefn", 
          
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1375))
       ))]
   and cvtINTERFACE_DEFN {ident=x1405, ns=opt1407, nonnullable=b1411, params=ls1413, 
          extends=ls1418, instanceDefns=ls1423} = PrettyRep.Rec [("ident", 
          cvtIDENT x1405), ("ns", 
       (case opt1407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1406 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1406))
       )), ("nonnullable", PrettyRep.Bool b1411), ("params", PrettyRep.List (List.map (fn x1412 => 
                                                                                             cvtIDENT x1412
                                                                                      ) ls1413)), 
          ("extends", PrettyRep.List (List.map (fn x1417 => cvtIDENT_EXPR x1417
                                               ) ls1418)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1422 => cvtDEFN x1422
                                   ) ls1423))]
   and cvtTYPE_DEFN {ident=x1440, ns=opt1442, init=x1446} = PrettyRep.Rec [("ident", 
          cvtIDENT x1440), ("ns", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1441 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1441))
       )), ("init", cvtTYPE_EXPR x1446)]
   and cvtFOR_ENUM_STMT {isEach=b1454, defn=opt1456, obj=x1460, fixtures=opt1462, 
          next=x1466, labels=ls1468, body=x1472} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1454), ("defn", 
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1455 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1455))
       )), ("obj", cvtEXPR x1460), ("fixtures", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1461 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1461))
       )), ("next", cvtSTMT x1466), ("labels", PrettyRep.List (List.map (fn x1467 => 
                                                                               cvtIDENT x1467
                                                                        ) ls1468)), 
          ("body", cvtSTMT x1472)]
   and cvtFOR_STMT {fixtures=opt1489, defn=opt1494, init=ls1499, cond=x1503, 
          update=x1504, labels=ls1506, body=x1510} = PrettyRep.Rec [("fixtures", 
          
       (case opt1489 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1488 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1488))
       )), ("defn", 
       (case opt1494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1493))
       )), ("init", PrettyRep.List (List.map (fn x1498 => cvtSTMT x1498
                                             ) ls1499)), ("cond", cvtEXPR x1503), 
          ("update", cvtEXPR x1504), ("labels", PrettyRep.List (List.map (fn x1505 => 
                                                                                cvtIDENT x1505
                                                                         ) ls1506)), 
          ("body", cvtSTMT x1510)]
   and cvtWHILE_STMT {cond=x1526, fixtures=opt1528, body=x1532, labels=ls1534} = 
          PrettyRep.Rec [("cond", cvtEXPR x1526), ("fixtures", 
       (case opt1528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1527 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1527))
       )), ("body", cvtSTMT x1532), ("labels", PrettyRep.List (List.map (fn x1533 => 
                                                                               cvtIDENT x1533
                                                                        ) ls1534))]
   and cvtDIRECTIVES {pragmas=ls1548, defns=ls1553, head=opt1558, body=ls1563, 
          loc=opt1568} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1547 => 
                                                                                    cvtPRAGMA x1547
                                                                             ) ls1548)), 
          ("defns", PrettyRep.List (List.map (fn x1552 => cvtDEFN x1552
                                             ) ls1553)), ("head", 
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1557 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1557))
       )), ("body", PrettyRep.List (List.map (fn x1562 => cvtSTMT x1562
                                             ) ls1563)), ("loc", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1567))
       ))]
   and cvtCASE {label=opt1584, inits=opt1589, body=x1593} = PrettyRep.Rec [("label", 
          
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1583 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1583))
       )), ("inits", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1588))
       )), ("body", cvtBLOCK x1593)]
   and cvtCATCH_CLAUSE {bindings=x1601, ty=x1602, fixtures=opt1604, inits=opt1609, 
          block=x1613} = PrettyRep.Rec [("bindings", cvtBINDINGS x1601), ("ty", 
          cvtTYPE_EXPR x1602), ("fixtures", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1603 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1603))
       )), ("inits", 
       (case opt1609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1608 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1608))
       )), ("block", cvtBLOCK x1613)]
   and cvtFUNC_NAME {kind=x1625, ident=x1626} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1625), 
          ("ident", cvtIDENT x1626)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1632, getter=opt1634, setter=opt1639} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1632), ("getter", 
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1633 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1633))
       )), ("setter", 
       (case opt1639 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1638 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1638))
       ))]
   and cvtPACKAGE {name=ls1651, block=x1655} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1650 => 
                                                                                                       cvtIDENT x1650
                                                                                                ) ls1651)), 
          ("block", cvtBLOCK x1655)]
   and cvtPROGRAM {packages=ls1662, fixtures=opt1667, block=x1671} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1661 => cvtPACKAGE x1661
                                   ) ls1662)), ("fixtures", 
       (case opt1667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1666 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1666))
       )), ("block", cvtBLOCK x1671)]
end

