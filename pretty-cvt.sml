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
   and cvtCLS (Cls{name=x330, nonnullable=b331, dynamic=b332, extends=opt334, 
          implements=ls339, classFixtures=x343, instanceFixtures=x344, instanceInits=x345, 
          constructor=opt347, classType=x351, instanceType=x352}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x330), ("nonnullable", PrettyRep.Bool b331), 
          ("dynamic", PrettyRep.Bool b332), ("extends", 
       (case opt334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x333 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x333))
       )), ("implements", PrettyRep.List (List.map (fn x338 => cvtNAME x338
                                                   ) ls339)), ("classFixtures", 
          cvtFIXTURES x343), ("instanceFixtures", cvtFIXTURES x344), ("instanceInits", 
          cvtHEAD x345), ("constructor", 
       (case opt347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x346 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x346))
       )), ("classType", cvtTYPE_EXPR x351), ("instanceType", cvtTYPE_EXPR x352)]))
   and cvtIFACE (Iface{name=x378, nonnullable=b379, extends=ls381, instanceFixtures=x385, 
          instanceType=x386}) = PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x378), ("nonnullable", PrettyRep.Bool b379), ("extends", 
          PrettyRep.List (List.map (fn x380 => cvtNAME x380
                                   ) ls381)), ("instanceFixtures", cvtFIXTURES x385), 
          ("instanceType", cvtTYPE_EXPR x386)]))
   and cvtCTOR (Ctor{settings=x400, superArgs=ls402, func=x406}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x400), ("superArgs", PrettyRep.List (List.map (fn x401 => 
                                                                                                         cvtEXPR x401
                                                                                                  ) ls402)), 
          ("func", cvtFUNC x406)]))
   and cvtFUNC (Func{name=x416, fsig=x417, isNative=b418, block=x419, param=x420, 
          defaults=ls422, ty=x426}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x416), ("fsig", cvtFUNC_SIG x417), ("isNative", PrettyRep.Bool b418), 
          ("block", cvtBLOCK x419), ("param", cvtHEAD x420), ("defaults", PrettyRep.List (List.map (fn x421 => 
                                                                                                          cvtEXPR x421
                                                                                                   ) ls422)), 
          ("ty", cvtFUNC_TYPE x426)]))
   and cvtDEFN (ClassDefn x444) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x444))
     | cvtDEFN (VariableDefn x447) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x447))
     | cvtDEFN (FunctionDefn x450) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x450))
     | cvtDEFN (ConstructorDefn x453) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x453))
     | cvtDEFN (InterfaceDefn x456) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x456))
     | cvtDEFN (NamespaceDefn x459) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x459))
     | cvtDEFN (TypeDefn x462) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x462))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls466, params=x470, paramTypes=ls472, 
          defaults=ls477, ctorInits=opt488, returnType=x492, thisType=opt494, 
          hasRest=b498}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x465 => cvtIDENT x465
                                   ) ls466)), ("params", cvtBINDINGS x470), 
          ("paramTypes", PrettyRep.List (List.map (fn x471 => cvtTYPE_EXPR x471
                                                  ) ls472)), ("defaults", PrettyRep.List (List.map (fn x476 => 
                                                                                                          cvtEXPR x476
                                                                                                   ) ls477)), 
          ("ctorInits", 
       (case opt488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x481, ls483) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x481, 
            PrettyRep.List (List.map (fn x482 => cvtEXPR x482
                                     ) ls483)]))
       )), ("returnType", cvtTYPE_EXPR x492), ("thisType", 
       (case opt494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x493 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x493))
       )), ("hasRest", PrettyRep.Bool b498)]))
   and cvtBINDING (Binding{ident=x518, ty=x519}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x518), ("ty", cvtTYPE_EXPR x519)]))
   and cvtBINDING_IDENT (TempIdent n527) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n527))
     | cvtBINDING_IDENT (ParamIdent n530) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n530))
     | cvtBINDING_IDENT (PropIdent x533) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x533))
   and cvtINIT_STEP (InitStep(x536, x537)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x536, 
          cvtEXPR x537]))
     | cvtINIT_STEP (AssignStep(x541, x542)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x541, cvtEXPR x542]))
   and cvtTYPE_EXPR (SpecialType x546) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x546))
     | cvtTYPE_EXPR (UnionType ls550) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x549 => 
                                                                                                           cvtTYPE_EXPR x549
                                                                                                    ) ls550)))
     | cvtTYPE_EXPR (ArrayType ls557) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x556 => 
                                                                                                           cvtTYPE_EXPR x556
                                                                                                    ) ls557)))
     | cvtTYPE_EXPR (TypeName x563) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x563))
     | cvtTYPE_EXPR (ElementTypeRef(x566, n567)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x566, PrettyRep.Int n567]))
     | cvtTYPE_EXPR (FieldTypeRef(x571, x572)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x571, cvtIDENT x572]))
     | cvtTYPE_EXPR (FunctionType x576) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x576))
     | cvtTYPE_EXPR (ObjectType ls580) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x579 => 
                                                                                                             cvtFIELD_TYPE x579
                                                                                                      ) ls580)))
     | cvtTYPE_EXPR (AppType{base=x586, args=ls588}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x586), ("args", PrettyRep.List (List.map (fn x587 => 
                                                                                                     cvtTYPE_EXPR x587
                                                                                              ) ls588))]))
     | cvtTYPE_EXPR (NullableType{expr=x599, nullable=b600}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x599), ("nullable", PrettyRep.Bool b600)]))
     | cvtTYPE_EXPR (InstanceType{name=x608, nonnullable=b609, typeParams=ls611, 
          ty=x615, dynamic=b616}) = PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", 
          cvtNAME x608), ("nonnullable", PrettyRep.Bool b609), ("typeParams", 
          PrettyRep.List (List.map (fn x610 => cvtIDENT x610
                                   ) ls611)), ("ty", cvtTYPE_EXPR x615), ("dynamic", 
          PrettyRep.Bool b616)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x631) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x631))
     | cvtSTMT (InitStmt{kind=x634, ns=opt636, prototype=b640, static=b641, 
          temps=x642, inits=ls644}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x634), ("ns", 
       (case opt636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x635 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x635))
       )), ("prototype", PrettyRep.Bool b640), ("static", PrettyRep.Bool b641), 
          ("temps", cvtBINDINGS x642), ("inits", PrettyRep.List (List.map (fn x643 => 
                                                                                 cvtINIT_STEP x643
                                                                          ) ls644))]))
     | cvtSTMT (ClassBlock{ns=opt664, ident=x668, name=opt670, block=x674}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt664 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x663 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x663))
       )), ("ident", cvtIDENT x668), ("name", 
       (case opt670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x669 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x669))
       )), ("block", cvtBLOCK x674)]))
     | cvtSTMT (ForInStmt x686) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x686))
     | cvtSTMT (ThrowStmt x689) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x689))
     | cvtSTMT (ReturnStmt x692) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x692))
     | cvtSTMT (BreakStmt opt696) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt696 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x695 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x695))
       ))
     | cvtSTMT (ContinueStmt opt703) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt703 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x702 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x702))
       ))
     | cvtSTMT (BlockStmt x709) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x709))
     | cvtSTMT (LabeledStmt(x712, x713)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x712, 
          cvtSTMT x713]))
     | cvtSTMT (LetStmt x717) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x717))
     | cvtSTMT (WhileStmt x720) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x720))
     | cvtSTMT (DoWhileStmt x723) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x723))
     | cvtSTMT (ForStmt x726) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x726))
     | cvtSTMT (IfStmt{cnd=x729, thn=x730, els=x731}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x729), ("thn", cvtSTMT x730), 
          ("els", cvtSTMT x731)]))
     | cvtSTMT (WithStmt{obj=x741, ty=x742, body=x743}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x741), ("ty", cvtTYPE_EXPR x742), 
          ("body", cvtSTMT x743)]))
     | cvtSTMT (TryStmt{block=x753, catches=ls755, finally=opt760}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x753), ("catches", PrettyRep.List (List.map (fn x754 => 
                                                                                                     cvtCATCH_CLAUSE x754
                                                                                              ) ls755)), 
          ("finally", 
       (case opt760 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x759 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x759))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt774, cond=x778, labels=ls780, cases=ls785}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt774 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x773 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x773))
       )), ("cond", cvtEXPR x778), ("labels", PrettyRep.List (List.map (fn x779 => 
                                                                              cvtIDENT x779
                                                                       ) ls780)), 
          ("cases", PrettyRep.List (List.map (fn x784 => cvtCASE x784
                                             ) ls785))]))
     | cvtSTMT (SwitchTypeStmt{cond=x800, ty=x801, cases=ls803}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x800), ("ty", cvtTYPE_EXPR x801), 
          ("cases", PrettyRep.List (List.map (fn x802 => cvtCATCH_CLAUSE x802
                                             ) ls803))]))
     | cvtSTMT (DXNStmt{expr=x816}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x816)]))
   and cvtEXPR (TernaryExpr(x822, x823, x824)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x822, cvtEXPR x823, cvtEXPR x824]))
     | cvtEXPR (BinaryExpr(x828, x829, x830)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x828, cvtEXPR x829, cvtEXPR x830]))
     | cvtEXPR (BinaryTypeExpr(x834, x835, x836)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x834, cvtEXPR x835, cvtTYPE_EXPR x836]))
     | cvtEXPR (ExpectedTypeExpr(x840, x841)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x840, cvtEXPR x841]))
     | cvtEXPR (UnaryExpr(x845, x846)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x845, 
          cvtEXPR x846]))
     | cvtEXPR (TypeExpr x850) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x850))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt855) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x854))
       ))
     | cvtEXPR (SuperExpr opt862) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt862 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x861 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x861))
       ))
     | cvtEXPR (LiteralExpr x868) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x868))
     | cvtEXPR (CallExpr{func=x871, actuals=ls873}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x871), ("actuals", PrettyRep.List (List.map (fn x872 => 
                                                                                                   cvtEXPR x872
                                                                                            ) ls873))]))
     | cvtEXPR (ApplyTypeExpr{expr=x884, actuals=ls886}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x884), ("actuals", PrettyRep.List (List.map (fn x885 => 
                                                                                                   cvtTYPE_EXPR x885
                                                                                            ) ls886))]))
     | cvtEXPR (LetExpr{defs=x897, body=x898, head=opt900}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x897), ("body", cvtEXPR x898), 
          ("head", 
       (case opt900 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x899 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x899))
       ))]))
     | cvtEXPR (NewExpr{obj=x913, actuals=ls915}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x913), ("actuals", PrettyRep.List (List.map (fn x914 => 
                                                                                                  cvtEXPR x914
                                                                                           ) ls915))]))
     | cvtEXPR (ObjectRef{base=x926, ident=x927, loc=opt929}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x926), ("ident", cvtIDENT_EXPR x927), 
          ("loc", 
       (case opt929 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x928 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x928))
       ))]))
     | cvtEXPR (LexicalRef{ident=x942, loc=opt944}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x942), ("loc", 
       (case opt944 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x943 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x943))
       ))]))
     | cvtEXPR (SetExpr(x955, x956, x957)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x955, 
          cvtEXPR x956, cvtEXPR x957]))
     | cvtEXPR (ListExpr ls962) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x961 => 
                                                                                                    cvtEXPR x961
                                                                                             ) ls962)))
     | cvtEXPR (InitExpr(x968, x969, x970)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x968, 
          cvtHEAD x969, cvtINITS x970]))
     | cvtEXPR (SliceExpr(x974, x975, x976)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x974, cvtEXPR x975, cvtEXPR x976]))
     | cvtEXPR (GetTemp n980) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n980))
     | cvtEXPR (GetParam n983) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n983))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n989) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n989))
     | cvtFIXTURE_NAME (PropName x992) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x992))
   and cvtIDENT_EXPR (Identifier{ident=x995, openNamespaces=ls1001}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x995), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls997 => PrettyRep.List (List.map (fn x996 => 
                                                                                cvtNAMESPACE x996
                                                                         ) ls997)
                                   ) ls1001))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1012, expr=x1013}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1012), ("expr", cvtEXPR x1013)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1021) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1021))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1024, openNamespaces=ls1030}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1024), ("openNamespaces", PrettyRep.List (List.map (fn ls1026 => 
                                                                             PrettyRep.List (List.map (fn x1025 => 
                                                                                                             cvtNAMESPACE x1025
                                                                                                      ) ls1026)
                                                                      ) ls1030))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1041, ident=s1042}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1041), ("ident", PrettyRep.UniStr s1042)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1050, typeArgs=ls1052}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1050), ("typeArgs", 
          PrettyRep.List (List.map (fn x1051 => cvtTYPE_EXPR x1051
                                   ) ls1052))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1064, x1068)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1063 => cvtIDENT x1063
                                                          ) ls1064), cvtIDENT_EXPR x1068]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1075) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1075))
     | cvtLITERAL (LiteralContextualDecimalInteger s1078) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1078))
     | cvtLITERAL (LiteralContextualHexInteger s1081) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1081))
     | cvtLITERAL (LiteralContextualOctInteger s1084) = PrettyRep.Ctor ("LiteralContextualOctInteger", 
          SOME (PrettyRep.String s1084))
     | cvtLITERAL (LiteralDouble r1087) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1087))
     | cvtLITERAL (LiteralDecimal d1090) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1090))
     | cvtLITERAL (LiteralInt i1093) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1093))
     | cvtLITERAL (LiteralUInt u1096) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1096))
     | cvtLITERAL (LiteralBoolean b1099) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1099))
     | cvtLITERAL (LiteralString s1102) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1102))
     | cvtLITERAL (LiteralArray{exprs=ls1106, ty=opt1111}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1105 => 
                                                                         cvtEXPR x1105
                                                                  ) ls1106)), 
          ("ty", 
       (case opt1111 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1110 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1110))
       ))]))
     | cvtLITERAL (LiteralXML ls1123) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1122 => 
                                                                                                            cvtEXPR x1122
                                                                                                     ) ls1123)))
     | cvtLITERAL (LiteralNamespace x1129) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1129))
     | cvtLITERAL (LiteralObject{expr=ls1133, ty=opt1138}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1132 => 
                                                                        cvtFIELD x1132
                                                                 ) ls1133)), 
          ("ty", 
       (case opt1138 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1137 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1137))
       ))]))
     | cvtLITERAL (LiteralFunction x1149) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1149))
     | cvtLITERAL (LiteralRegExp{str=s1152}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1152)]))
   and cvtBLOCK (Block x1158) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1158))
   and cvtFIXTURE (NamespaceFixture x1161) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1161))
     | cvtFIXTURE (ClassFixture x1164) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1164))
     | cvtFIXTURE (InterfaceFixture x1167) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1167))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1171) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1171))
     | cvtFIXTURE (MethodFixture{func=x1174, ty=x1175, readOnly=b1176, override=b1177, 
          final=b1178}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1174), ("ty", cvtTYPE_EXPR x1175), ("readOnly", PrettyRep.Bool b1176), 
          ("override", PrettyRep.Bool b1177), ("final", PrettyRep.Bool b1178)]))
     | cvtFIXTURE (ValFixture{ty=x1192, readOnly=b1193}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1192), ("readOnly", PrettyRep.Bool b1193)]))
     | cvtFIXTURE (VirtualValFixture x1201) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1201))
   and cvtBINDINGS (ls1205, ls1210) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1204 => 
                                                                                       cvtBINDING x1204
                                                                                ) ls1205), 
          PrettyRep.List (List.map (fn x1209 => cvtINIT_STEP x1209
                                   ) ls1210)]
   and cvtFIXTURES ls1218 = PrettyRep.List (List.map (fn (x1215, x1216) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1215, 
                                                            cvtFIXTURE x1216]
                                                     ) ls1218)
   and cvtINITS ls1225 = PrettyRep.List (List.map (fn (x1222, x1223) => PrettyRep.Tuple [cvtFIXTURE_NAME x1222, 
                                                         cvtEXPR x1223]
                                                  ) ls1225)
   and cvtHEAD (x1229, x1230) = PrettyRep.Tuple [cvtFIXTURES x1229, cvtINITS x1230]
   and cvtFIELD {kind=x1232, name=x1233, init=x1234} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1232), ("name", cvtIDENT_EXPR x1233), ("init", cvtEXPR x1234)]
   and cvtFIELD_TYPE {name=x1242, ty=x1243} = PrettyRep.Rec [("name", cvtIDENT x1242), 
          ("ty", cvtTYPE_EXPR x1243)]
   and cvtFUNC_TYPE {typeParams=ls1250, params=ls1255, result=x1259, thisType=opt1261, 
          hasRest=b1265, minArgs=n1266} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1249 => 
                                                                                                        cvtIDENT x1249
                                                                                                 ) ls1250)), 
          ("params", PrettyRep.List (List.map (fn x1254 => cvtTYPE_EXPR x1254
                                              ) ls1255)), ("result", cvtTYPE_EXPR x1259), 
          ("thisType", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1260))
       )), ("hasRest", PrettyRep.Bool b1265), ("minArgs", PrettyRep.Int n1266)]
   and cvtFUNC_DEFN {kind=x1280, ns=opt1282, final=b1286, override=b1287, prototype=b1288, 
          static=b1289, func=x1290} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1280), 
          ("ns", 
       (case opt1282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1281 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1281))
       )), ("final", PrettyRep.Bool b1286), ("override", PrettyRep.Bool b1287), 
          ("prototype", PrettyRep.Bool b1288), ("static", PrettyRep.Bool b1289), 
          ("func", cvtFUNC x1290)]
   and cvtCTOR_DEFN x1306 = cvtCTOR x1306
   and cvtVAR_DEFN {kind=x1307, ns=opt1309, static=b1313, prototype=b1314, 
          bindings=x1315} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1307), 
          ("ns", 
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1308))
       )), ("static", PrettyRep.Bool b1313), ("prototype", PrettyRep.Bool b1314), 
          ("bindings", cvtBINDINGS x1315)]
   and cvtNAMESPACE_DEFN {ident=x1327, ns=opt1329, init=opt1334} = PrettyRep.Rec [("ident", 
          cvtIDENT x1327), ("ns", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1328))
       )), ("init", 
       (case opt1334 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1333 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1333))
       ))]
   and cvtCLASS_DEFN {ident=x1345, ns=opt1347, nonnullable=b1351, dynamic=b1352, 
          final=b1353, params=ls1355, extends=opt1360, implements=ls1365, classDefns=ls1370, 
          instanceDefns=ls1375, instanceStmts=ls1380, ctorDefn=opt1385} = PrettyRep.Rec [("ident", 
          cvtIDENT x1345), ("ns", 
       (case opt1347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1346))
       )), ("nonnullable", PrettyRep.Bool b1351), ("dynamic", PrettyRep.Bool b1352), 
          ("final", PrettyRep.Bool b1353), ("params", PrettyRep.List (List.map (fn x1354 => 
                                                                                      cvtIDENT x1354
                                                                               ) ls1355)), 
          ("extends", 
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1359))
       )), ("implements", PrettyRep.List (List.map (fn x1364 => cvtIDENT_EXPR x1364
                                                   ) ls1365)), ("classDefns", 
          PrettyRep.List (List.map (fn x1369 => cvtDEFN x1369
                                   ) ls1370)), ("instanceDefns", PrettyRep.List (List.map (fn x1374 => 
                                                                                                 cvtDEFN x1374
                                                                                          ) ls1375)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1379 => cvtSTMT x1379
                                                     ) ls1380)), ("ctorDefn", 
          
       (case opt1385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1384))
       ))]
   and cvtINTERFACE_DEFN {ident=x1414, ns=opt1416, nonnullable=b1420, params=ls1422, 
          extends=ls1427, instanceDefns=ls1432} = PrettyRep.Rec [("ident", 
          cvtIDENT x1414), ("ns", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1415))
       )), ("nonnullable", PrettyRep.Bool b1420), ("params", PrettyRep.List (List.map (fn x1421 => 
                                                                                             cvtIDENT x1421
                                                                                      ) ls1422)), 
          ("extends", PrettyRep.List (List.map (fn x1426 => cvtIDENT_EXPR x1426
                                               ) ls1427)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1431 => cvtDEFN x1431
                                   ) ls1432))]
   and cvtTYPE_DEFN {ident=x1449, ns=opt1451, init=x1455} = PrettyRep.Rec [("ident", 
          cvtIDENT x1449), ("ns", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1450))
       )), ("init", cvtTYPE_EXPR x1455)]
   and cvtFOR_ENUM_STMT {isEach=b1463, defn=opt1465, obj=x1469, fixtures=opt1471, 
          next=x1475, labels=ls1477, body=x1481} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1463), ("defn", 
       (case opt1465 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1464))
       )), ("obj", cvtEXPR x1469), ("fixtures", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1470))
       )), ("next", cvtSTMT x1475), ("labels", PrettyRep.List (List.map (fn x1476 => 
                                                                               cvtIDENT x1476
                                                                        ) ls1477)), 
          ("body", cvtSTMT x1481)]
   and cvtFOR_STMT {fixtures=opt1498, defn=opt1503, init=ls1508, cond=x1512, 
          update=x1513, labels=ls1515, body=x1519} = PrettyRep.Rec [("fixtures", 
          
       (case opt1498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1497 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1497))
       )), ("defn", 
       (case opt1503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1502 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1502))
       )), ("init", PrettyRep.List (List.map (fn x1507 => cvtSTMT x1507
                                             ) ls1508)), ("cond", cvtEXPR x1512), 
          ("update", cvtEXPR x1513), ("labels", PrettyRep.List (List.map (fn x1514 => 
                                                                                cvtIDENT x1514
                                                                         ) ls1515)), 
          ("body", cvtSTMT x1519)]
   and cvtWHILE_STMT {cond=x1535, fixtures=opt1537, body=x1541, labels=ls1543} = 
          PrettyRep.Rec [("cond", cvtEXPR x1535), ("fixtures", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1536 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1536))
       )), ("body", cvtSTMT x1541), ("labels", PrettyRep.List (List.map (fn x1542 => 
                                                                               cvtIDENT x1542
                                                                        ) ls1543))]
   and cvtDIRECTIVES {pragmas=ls1557, defns=ls1562, head=opt1567, body=ls1572, 
          loc=opt1577} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1556 => 
                                                                                    cvtPRAGMA x1556
                                                                             ) ls1557)), 
          ("defns", PrettyRep.List (List.map (fn x1561 => cvtDEFN x1561
                                             ) ls1562)), ("head", 
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1566 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1566))
       )), ("body", PrettyRep.List (List.map (fn x1571 => cvtSTMT x1571
                                             ) ls1572)), ("loc", 
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1576))
       ))]
   and cvtCASE {label=opt1593, inits=opt1598, body=x1602} = PrettyRep.Rec [("label", 
          
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1592 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1592))
       )), ("inits", 
       (case opt1598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1597 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1597))
       )), ("body", cvtBLOCK x1602)]
   and cvtCATCH_CLAUSE {bindings=x1610, ty=x1611, fixtures=opt1613, inits=opt1618, 
          block=x1622} = PrettyRep.Rec [("bindings", cvtBINDINGS x1610), ("ty", 
          cvtTYPE_EXPR x1611), ("fixtures", 
       (case opt1613 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1612 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1612))
       )), ("inits", 
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1617 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1617))
       )), ("block", cvtBLOCK x1622)]
   and cvtFUNC_NAME {kind=x1634, ident=x1635} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1634), 
          ("ident", cvtIDENT x1635)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1641, getter=opt1643, setter=opt1648} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1641), ("getter", 
       (case opt1643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1642 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1642))
       )), ("setter", 
       (case opt1648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1647 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1647))
       ))]
   and cvtPACKAGE {name=ls1660, block=x1664} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1659 => 
                                                                                                       cvtIDENT x1659
                                                                                                ) ls1660)), 
          ("block", cvtBLOCK x1664)]
   and cvtPROGRAM {packages=ls1671, fixtures=opt1676, block=x1680} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1670 => cvtPACKAGE x1670
                                   ) ls1671)), ("fixtures", 
       (case opt1676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1675 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1675))
       )), ("block", cvtBLOCK x1680)]
end

