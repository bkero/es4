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
       )), ("classType", cvtTYPE_EXPR x351), ("instanceType", cvtINSTANCE_TYPE x352)]))
   and cvtIFACE (Iface{name=x378, nonnullable=b379, extends=ls381, instanceFixtures=x385, 
          instanceType=x386}) = PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x378), ("nonnullable", PrettyRep.Bool b379), ("extends", 
          PrettyRep.List (List.map (fn x380 => cvtNAME x380
                                   ) ls381)), ("instanceFixtures", cvtFIXTURES x385), 
          ("instanceType", cvtINSTANCE_TYPE x386)]))
   and cvtCTOR (Ctor{settings=x400, superArgs=ls402, func=x406}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x400), ("superArgs", PrettyRep.List (List.map (fn x401 => 
                                                                                                         cvtEXPR x401
                                                                                                  ) ls402)), 
          ("func", cvtFUNC x406)]))
   and cvtFUNC (Func{name=x416, fsig=x417, native=b418, block=x419, param=x420, 
          defaults=ls422, ty=x426}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x416), ("fsig", cvtFUNC_SIG x417), ("native", PrettyRep.Bool b418), 
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
     | cvtTYPE_EXPR (InstanceType x608) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x608))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x612) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x612))
     | cvtSTMT (InitStmt{kind=x615, ns=opt617, prototype=b621, static=b622, 
          temps=x623, inits=ls625}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x615), ("ns", 
       (case opt617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x616 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x616))
       )), ("prototype", PrettyRep.Bool b621), ("static", PrettyRep.Bool b622), 
          ("temps", cvtBINDINGS x623), ("inits", PrettyRep.List (List.map (fn x624 => 
                                                                                 cvtINIT_STEP x624
                                                                          ) ls625))]))
     | cvtSTMT (ClassBlock{ns=opt645, ident=x649, name=opt651, block=x655}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x644 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x644))
       )), ("ident", cvtIDENT x649), ("name", 
       (case opt651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x650 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x650))
       )), ("block", cvtBLOCK x655)]))
     | cvtSTMT (ForInStmt x667) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x667))
     | cvtSTMT (ThrowStmt x670) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x670))
     | cvtSTMT (ReturnStmt x673) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x673))
     | cvtSTMT (BreakStmt opt677) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt677 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x676 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x676))
       ))
     | cvtSTMT (ContinueStmt opt684) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x683 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x683))
       ))
     | cvtSTMT (BlockStmt x690) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x690))
     | cvtSTMT (LabeledStmt(x693, x694)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x693, 
          cvtSTMT x694]))
     | cvtSTMT (LetStmt x698) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x698))
     | cvtSTMT (WhileStmt x701) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x701))
     | cvtSTMT (DoWhileStmt x704) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x704))
     | cvtSTMT (ForStmt x707) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x707))
     | cvtSTMT (IfStmt{cnd=x710, thn=x711, els=x712}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x710), ("thn", cvtSTMT x711), 
          ("els", cvtSTMT x712)]))
     | cvtSTMT (WithStmt{obj=x722, ty=x723, body=x724}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x722), ("ty", cvtTYPE_EXPR x723), 
          ("body", cvtSTMT x724)]))
     | cvtSTMT (TryStmt{block=x734, catches=ls736, finally=opt741}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x734), ("catches", PrettyRep.List (List.map (fn x735 => 
                                                                                                     cvtCATCH_CLAUSE x735
                                                                                              ) ls736)), 
          ("finally", 
       (case opt741 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x740 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x740))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt755, cond=x759, labels=ls761, cases=ls766}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt755 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x754 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x754))
       )), ("cond", cvtEXPR x759), ("labels", PrettyRep.List (List.map (fn x760 => 
                                                                              cvtIDENT x760
                                                                       ) ls761)), 
          ("cases", PrettyRep.List (List.map (fn x765 => cvtCASE x765
                                             ) ls766))]))
     | cvtSTMT (SwitchTypeStmt{cond=x781, ty=x782, cases=ls784}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x781), ("ty", cvtTYPE_EXPR x782), 
          ("cases", PrettyRep.List (List.map (fn x783 => cvtCATCH_CLAUSE x783
                                             ) ls784))]))
     | cvtSTMT (DXNStmt{expr=x797}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x797)]))
   and cvtEXPR (TernaryExpr(x803, x804, x805)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x803, cvtEXPR x804, cvtEXPR x805]))
     | cvtEXPR (BinaryExpr(x809, x810, x811)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x809, cvtEXPR x810, cvtEXPR x811]))
     | cvtEXPR (BinaryTypeExpr(x815, x816, x817)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x815, cvtEXPR x816, cvtTYPE_EXPR x817]))
     | cvtEXPR (ExpectedTypeExpr(x821, x822)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x821, cvtEXPR x822]))
     | cvtEXPR (UnaryExpr(x826, x827)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x826, 
          cvtEXPR x827]))
     | cvtEXPR (TypeExpr x831) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x831))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt836) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt836 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x835 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x835))
       ))
     | cvtEXPR (SuperExpr opt843) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt843 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x842 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x842))
       ))
     | cvtEXPR (LiteralExpr x849) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x849))
     | cvtEXPR (CallExpr{func=x852, actuals=ls854}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x852), ("actuals", PrettyRep.List (List.map (fn x853 => 
                                                                                                   cvtEXPR x853
                                                                                            ) ls854))]))
     | cvtEXPR (ApplyTypeExpr{expr=x865, actuals=ls867}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x865), ("actuals", PrettyRep.List (List.map (fn x866 => 
                                                                                                   cvtTYPE_EXPR x866
                                                                                            ) ls867))]))
     | cvtEXPR (LetExpr{defs=x878, body=x879, head=opt881}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x878), ("body", cvtEXPR x879), 
          ("head", 
       (case opt881 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x880 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x880))
       ))]))
     | cvtEXPR (NewExpr{obj=x894, actuals=ls896}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x894), ("actuals", PrettyRep.List (List.map (fn x895 => 
                                                                                                  cvtEXPR x895
                                                                                           ) ls896))]))
     | cvtEXPR (ObjectRef{base=x907, ident=x908, loc=opt910}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x907), ("ident", cvtIDENT_EXPR x908), 
          ("loc", 
       (case opt910 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x909 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x909))
       ))]))
     | cvtEXPR (LexicalRef{ident=x923, loc=opt925}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x923), ("loc", 
       (case opt925 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x924 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x924))
       ))]))
     | cvtEXPR (SetExpr(x936, x937, x938)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x936, 
          cvtEXPR x937, cvtEXPR x938]))
     | cvtEXPR (ListExpr ls943) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x942 => 
                                                                                                    cvtEXPR x942
                                                                                             ) ls943)))
     | cvtEXPR (InitExpr(x949, x950, x951)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x949, 
          cvtHEAD x950, cvtINITS x951]))
     | cvtEXPR (SliceExpr(x955, x956, x957)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x955, cvtEXPR x956, cvtEXPR x957]))
     | cvtEXPR (GetTemp n961) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n961))
     | cvtEXPR (GetParam n964) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n964))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n970) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n970))
     | cvtFIXTURE_NAME (PropName x973) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x973))
   and cvtIDENT_EXPR (Identifier{ident=x976, openNamespaces=ls982}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x976), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls978 => PrettyRep.List (List.map (fn x977 => 
                                                                                cvtNAMESPACE x977
                                                                         ) ls978)
                                   ) ls982))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x993, expr=x994}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x993), ("expr", cvtEXPR x994)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1002) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1002))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1005, openNamespaces=ls1011}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1005), ("openNamespaces", PrettyRep.List (List.map (fn ls1007 => 
                                                                             PrettyRep.List (List.map (fn x1006 => 
                                                                                                             cvtNAMESPACE x1006
                                                                                                      ) ls1007)
                                                                      ) ls1011))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1022, ident=s1023}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1022), ("ident", PrettyRep.UniStr s1023)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1031, typeArgs=ls1033}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1031), ("typeArgs", 
          PrettyRep.List (List.map (fn x1032 => cvtTYPE_EXPR x1032
                                   ) ls1033))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1045, x1049)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1044 => cvtIDENT x1044
                                                          ) ls1045), cvtIDENT_EXPR x1049]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1056) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1056))
     | cvtLITERAL (LiteralContextualDecimalInteger s1059) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1059))
     | cvtLITERAL (LiteralContextualHexInteger s1062) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1062))
     | cvtLITERAL (LiteralContextualOctInteger s1065) = PrettyRep.Ctor ("LiteralContextualOctInteger", 
          SOME (PrettyRep.String s1065))
     | cvtLITERAL (LiteralDouble r1068) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1068))
     | cvtLITERAL (LiteralDecimal d1071) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1071))
     | cvtLITERAL (LiteralInt i1074) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1074))
     | cvtLITERAL (LiteralUInt u1077) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1077))
     | cvtLITERAL (LiteralBoolean b1080) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1080))
     | cvtLITERAL (LiteralString s1083) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1083))
     | cvtLITERAL (LiteralArray{exprs=ls1087, ty=opt1092}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1086 => 
                                                                         cvtEXPR x1086
                                                                  ) ls1087)), 
          ("ty", 
       (case opt1092 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1091 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1091))
       ))]))
     | cvtLITERAL (LiteralXML ls1104) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1103 => 
                                                                                                            cvtEXPR x1103
                                                                                                     ) ls1104)))
     | cvtLITERAL (LiteralNamespace x1110) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1110))
     | cvtLITERAL (LiteralObject{expr=ls1114, ty=opt1119}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1113 => 
                                                                        cvtFIELD x1113
                                                                 ) ls1114)), 
          ("ty", 
       (case opt1119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1118 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1118))
       ))]))
     | cvtLITERAL (LiteralFunction x1130) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1130))
     | cvtLITERAL (LiteralRegExp{str=s1133}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1133)]))
   and cvtBLOCK (Block x1139) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1139))
   and cvtFIXTURE (NamespaceFixture x1142) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1142))
     | cvtFIXTURE (ClassFixture x1145) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1145))
     | cvtFIXTURE (InterfaceFixture x1148) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1148))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1152) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1152))
     | cvtFIXTURE (MethodFixture{func=x1155, ty=x1156, readOnly=b1157, override=b1158, 
          final=b1159, abstract=b1160}) = PrettyRep.Ctor ("MethodFixture", 
          SOME (PrettyRep.Rec [("func", cvtFUNC x1155), ("ty", cvtTYPE_EXPR x1156), 
          ("readOnly", PrettyRep.Bool b1157), ("override", PrettyRep.Bool b1158), 
          ("final", PrettyRep.Bool b1159), ("abstract", PrettyRep.Bool b1160)]))
     | cvtFIXTURE (ValFixture{ty=x1176, readOnly=b1177}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1176), ("readOnly", PrettyRep.Bool b1177)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1185, getter=opt1187, setter=opt1192}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1185), ("getter", 
       (case opt1187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1186 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1186))
       )), ("setter", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1191))
       ))]))
   and cvtHEAD (Head(x1205, x1206)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtFIXTURES x1205, 
          cvtINITS x1206]))
   and cvtBINDINGS (ls1211, ls1216) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1210 => 
                                                                                       cvtBINDING x1210
                                                                                ) ls1211), 
          PrettyRep.List (List.map (fn x1215 => cvtINIT_STEP x1215
                                   ) ls1216)]
   and cvtFIXTURES ls1224 = PrettyRep.List (List.map (fn (x1221, x1222) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1221, 
                                                            cvtFIXTURE x1222]
                                                     ) ls1224)
   and cvtINITS ls1231 = PrettyRep.List (List.map (fn (x1228, x1229) => PrettyRep.Tuple [cvtFIXTURE_NAME x1228, 
                                                         cvtEXPR x1229]
                                                  ) ls1231)
   and cvtINSTANCE_TYPE {name=x1235, nonnullable=b1236, typeParams=ls1238, 
          superTypes=ls1243, ty=x1247, conversionTy=opt1249, dynamic=b1253} = 
          PrettyRep.Rec [("name", cvtNAME x1235), ("nonnullable", PrettyRep.Bool b1236), 
          ("typeParams", PrettyRep.List (List.map (fn x1237 => cvtIDENT x1237
                                                  ) ls1238)), ("superTypes", 
          PrettyRep.List (List.map (fn x1242 => cvtNAME x1242
                                   ) ls1243)), ("ty", cvtTYPE_EXPR x1247), 
          ("conversionTy", 
       (case opt1249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1248 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1248))
       )), ("dynamic", PrettyRep.Bool b1253)]
   and cvtFIELD {kind=x1269, name=x1270, init=x1271} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1269), ("name", cvtIDENT_EXPR x1270), ("init", cvtEXPR x1271)]
   and cvtFIELD_TYPE {name=x1279, ty=x1280} = PrettyRep.Rec [("name", cvtIDENT x1279), 
          ("ty", cvtTYPE_EXPR x1280)]
   and cvtFUNC_TYPE {typeParams=ls1287, params=ls1292, result=x1296, thisType=opt1298, 
          hasRest=b1302, minArgs=n1303} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1286 => 
                                                                                                        cvtIDENT x1286
                                                                                                 ) ls1287)), 
          ("params", PrettyRep.List (List.map (fn x1291 => cvtTYPE_EXPR x1291
                                              ) ls1292)), ("result", cvtTYPE_EXPR x1296), 
          ("thisType", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1297))
       )), ("hasRest", PrettyRep.Bool b1302), ("minArgs", PrettyRep.Int n1303)]
   and cvtFUNC_DEFN {kind=x1317, ns=opt1319, final=b1323, override=b1324, prototype=b1325, 
          static=b1326, abstract=b1327, func=x1328} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1317), ("ns", 
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1318))
       )), ("final", PrettyRep.Bool b1323), ("override", PrettyRep.Bool b1324), 
          ("prototype", PrettyRep.Bool b1325), ("static", PrettyRep.Bool b1326), 
          ("abstract", PrettyRep.Bool b1327), ("func", cvtFUNC x1328)]
   and cvtCTOR_DEFN x1346 = cvtCTOR x1346
   and cvtVAR_DEFN {kind=x1347, ns=opt1349, static=b1353, prototype=b1354, 
          bindings=(ls1356, ls1361)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1347), 
          ("ns", 
       (case opt1349 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1348 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1348))
       )), ("static", PrettyRep.Bool b1353), ("prototype", PrettyRep.Bool b1354), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1355 => 
                                                                        cvtBINDING x1355
                                                                 ) ls1356), 
          PrettyRep.List (List.map (fn x1360 => cvtINIT_STEP x1360
                                   ) ls1361)])]
   and cvtNAMESPACE_DEFN {ident=x1377, ns=opt1379, init=opt1384} = PrettyRep.Rec [("ident", 
          cvtIDENT x1377), ("ns", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1378 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1378))
       )), ("init", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1383 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1383))
       ))]
   and cvtCLASS_DEFN {ident=x1395, ns=opt1397, nonnullable=b1401, dynamic=b1402, 
          final=b1403, params=ls1405, extends=opt1410, implements=ls1415, classDefns=ls1420, 
          instanceDefns=ls1425, instanceStmts=ls1430, ctorDefn=opt1435} = PrettyRep.Rec [("ident", 
          cvtIDENT x1395), ("ns", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1396))
       )), ("nonnullable", PrettyRep.Bool b1401), ("dynamic", PrettyRep.Bool b1402), 
          ("final", PrettyRep.Bool b1403), ("params", PrettyRep.List (List.map (fn x1404 => 
                                                                                      cvtIDENT x1404
                                                                               ) ls1405)), 
          ("extends", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1409 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1409))
       )), ("implements", PrettyRep.List (List.map (fn x1414 => cvtIDENT_EXPR x1414
                                                   ) ls1415)), ("classDefns", 
          PrettyRep.List (List.map (fn x1419 => cvtDEFN x1419
                                   ) ls1420)), ("instanceDefns", PrettyRep.List (List.map (fn x1424 => 
                                                                                                 cvtDEFN x1424
                                                                                          ) ls1425)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1429 => cvtSTMT x1429
                                                     ) ls1430)), ("ctorDefn", 
          
       (case opt1435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1434 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1434))
       ))]
   and cvtINTERFACE_DEFN {ident=x1464, ns=opt1466, nonnullable=b1470, params=ls1472, 
          extends=ls1477, instanceDefns=ls1482} = PrettyRep.Rec [("ident", 
          cvtIDENT x1464), ("ns", 
       (case opt1466 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1465 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1465))
       )), ("nonnullable", PrettyRep.Bool b1470), ("params", PrettyRep.List (List.map (fn x1471 => 
                                                                                             cvtIDENT x1471
                                                                                      ) ls1472)), 
          ("extends", PrettyRep.List (List.map (fn x1476 => cvtIDENT_EXPR x1476
                                               ) ls1477)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1481 => cvtDEFN x1481
                                   ) ls1482))]
   and cvtTYPE_DEFN {ident=x1499, ns=opt1501, init=x1505} = PrettyRep.Rec [("ident", 
          cvtIDENT x1499), ("ns", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1500 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1500))
       )), ("init", cvtTYPE_EXPR x1505)]
   and cvtFOR_ENUM_STMT {isEach=b1513, defn=opt1544, obj=x1548, fixtures=opt1556, 
          next=x1560, labels=ls1562, body=x1566} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1513), ("defn", 
       (case opt1544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1514, ns=opt1516, static=b1520, prototype=b1521, bindings=(ls1523, 
            ls1528)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1514), ("ns", 
         (case opt1516 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1515 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1515))
         )), ("static", PrettyRep.Bool b1520), ("prototype", PrettyRep.Bool b1521), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1522 => 
                                                                          cvtBINDING x1522
                                                                   ) ls1523), 
            PrettyRep.List (List.map (fn x1527 => cvtINIT_STEP x1527
                                     ) ls1528)])]))
       )), ("obj", cvtEXPR x1548), ("fixtures", 
       (case opt1556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1552 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1549, 
                                                                                      x1550) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1549, 
                                                                                      cvtFIXTURE x1550]
                                                                               ) ls1552)))
       )), ("next", cvtSTMT x1560), ("labels", PrettyRep.List (List.map (fn x1561 => 
                                                                               cvtIDENT x1561
                                                                        ) ls1562)), 
          ("body", cvtSTMT x1566)]
   and cvtFOR_STMT {fixtures=opt1589, defn=opt1623, init=ls1628, cond=x1632, 
          update=x1633, labels=ls1635, body=x1639} = PrettyRep.Rec [("fixtures", 
          
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1585 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1582, 
                                                                                      x1583) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1582, 
                                                                                      cvtFIXTURE x1583]
                                                                               ) ls1585)))
       )), ("defn", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1593, ns=opt1595, static=b1599, prototype=b1600, bindings=(ls1602, 
            ls1607)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1593), ("ns", 
         (case opt1595 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1594 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1594))
         )), ("static", PrettyRep.Bool b1599), ("prototype", PrettyRep.Bool b1600), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1601 => 
                                                                          cvtBINDING x1601
                                                                   ) ls1602), 
            PrettyRep.List (List.map (fn x1606 => cvtINIT_STEP x1606
                                     ) ls1607)])]))
       )), ("init", PrettyRep.List (List.map (fn x1627 => cvtSTMT x1627
                                             ) ls1628)), ("cond", cvtEXPR x1632), 
          ("update", cvtEXPR x1633), ("labels", PrettyRep.List (List.map (fn x1634 => 
                                                                                cvtIDENT x1634
                                                                         ) ls1635)), 
          ("body", cvtSTMT x1639)]
   and cvtWHILE_STMT {cond=x1655, fixtures=opt1663, body=x1667, labels=ls1669} = 
          PrettyRep.Rec [("cond", cvtEXPR x1655), ("fixtures", 
       (case opt1663 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1659 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1656, 
                                                                                      x1657) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1656, 
                                                                                      cvtFIXTURE x1657]
                                                                               ) ls1659)))
       )), ("body", cvtSTMT x1667), ("labels", PrettyRep.List (List.map (fn x1668 => 
                                                                               cvtIDENT x1668
                                                                        ) ls1669))]
   and cvtDIRECTIVES {pragmas=ls1683, defns=ls1688, head=opt1693, body=ls1698, 
          loc=opt1703} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1682 => 
                                                                                    cvtPRAGMA x1682
                                                                             ) ls1683)), 
          ("defns", PrettyRep.List (List.map (fn x1687 => cvtDEFN x1687
                                             ) ls1688)), ("head", 
       (case opt1693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1692 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1692))
       )), ("body", PrettyRep.List (List.map (fn x1697 => cvtSTMT x1697
                                             ) ls1698)), ("loc", 
       (case opt1703 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1702 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1702))
       ))]
   and cvtCASE {label=opt1719, inits=opt1730, body=x1734} = PrettyRep.Rec [("label", 
          
       (case opt1719 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1718 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1718))
       )), ("inits", 
       (case opt1730 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1726 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1723, 
                                                                                      x1724) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1723, 
                                                                                      cvtEXPR x1724]
                                                                               ) ls1726)))
       )), ("body", cvtBLOCK x1734)]
   and cvtCATCH_CLAUSE {bindings=(ls1743, ls1748), ty=x1753, fixtures=opt1761, 
          inits=opt1772, block=x1776} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1742 => 
                                                                                                                     cvtBINDING x1742
                                                                                                              ) ls1743), 
          PrettyRep.List (List.map (fn x1747 => cvtINIT_STEP x1747
                                   ) ls1748)]), ("ty", cvtTYPE_EXPR x1753), 
          ("fixtures", 
       (case opt1761 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1757 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1754, 
                                                                                      x1755) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1754, 
                                                                                      cvtFIXTURE x1755]
                                                                               ) ls1757)))
       )), ("inits", 
       (case opt1772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1768 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1765, 
                                                                                      x1766) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1765, 
                                                                                      cvtEXPR x1766]
                                                                               ) ls1768)))
       )), ("block", cvtBLOCK x1776)]
   and cvtFUNC_NAME {kind=x1788, ident=x1789} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1788), 
          ("ident", cvtIDENT x1789)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1795, getter=opt1797, setter=opt1802} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1795), ("getter", 
       (case opt1797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1796 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1796))
       )), ("setter", 
       (case opt1802 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1801 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1801))
       ))]
   and cvtPACKAGE {name=ls1814, block=x1818} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1813 => 
                                                                                                       cvtIDENT x1813
                                                                                                ) ls1814)), 
          ("block", cvtBLOCK x1818)]
   and cvtPROGRAM {packages=ls1825, fixtures=opt1830, block=x1834} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1824 => cvtPACKAGE x1824
                                   ) ls1825)), ("fixtures", 
       (case opt1830 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1829 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1829))
       )), ("block", cvtBLOCK x1834)]
end

