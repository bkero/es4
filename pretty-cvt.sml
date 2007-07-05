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
          defaults=ls422, ty=x426, loc=opt428}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name",
          cvtFUNC_NAME x416), ("fsig", cvtFUNC_SIG x417), ("native", PrettyRep.Bool b418),
          ("block", cvtBLOCK x419), ("param", cvtHEAD x420), ("defaults", PrettyRep.List (List.map (fn x421 =>
                                                                                                          cvtEXPR x421
                                                                                                   ) ls422)),
          ("ty", cvtFUNC_TYPE x426), ("loc",
       (case opt428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x427 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x427))
       ))]))
   and cvtDEFN (ClassDefn x451) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x451))
     | cvtDEFN (VariableDefn x454) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x454))
     | cvtDEFN (FunctionDefn x457) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x457))
     | cvtDEFN (ConstructorDefn x460) = PrettyRep.Ctor ("ConstructorDefn",
          SOME (cvtCTOR_DEFN x460))
     | cvtDEFN (InterfaceDefn x463) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x463))
     | cvtDEFN (NamespaceDefn x466) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x466))
     | cvtDEFN (TypeDefn x469) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x469))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls473, params=x477, paramTypes=ls479,
          defaults=ls484, ctorInits=opt495, returnType=x499, thisType=opt501,
          hasRest=b505}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams",
          PrettyRep.List (List.map (fn x472 => cvtIDENT x472
                                   ) ls473)), ("params", cvtBINDINGS x477),
          ("paramTypes", PrettyRep.List (List.map (fn x478 => cvtTYPE_EXPR x478
                                                  ) ls479)), ("defaults", PrettyRep.List (List.map (fn x483 =>
                                                                                                          cvtEXPR x483
                                                                                                   ) ls484)),
          ("ctorInits",
       (case opt495 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x488, ls490) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x488,
            PrettyRep.List (List.map (fn x489 => cvtEXPR x489
                                     ) ls490)]))
       )), ("returnType", cvtTYPE_EXPR x499), ("thisType",
       (case opt501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x500 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x500))
       )), ("hasRest", PrettyRep.Bool b505)]))
   and cvtBINDING (Binding{ident=x525, ty=x526}) = PrettyRep.Ctor ("Binding",
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x525), ("ty", cvtTYPE_EXPR x526)]))
   and cvtBINDING_IDENT (TempIdent n534) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n534))
     | cvtBINDING_IDENT (ParamIdent n537) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n537))
     | cvtBINDING_IDENT (PropIdent x540) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x540))
   and cvtINIT_STEP (InitStep(x543, x544)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x543,
          cvtEXPR x544]))
     | cvtINIT_STEP (AssignStep(x548, x549)) = PrettyRep.Ctor ("AssignStep",
          SOME (PrettyRep.Tuple [cvtEXPR x548, cvtEXPR x549]))
   and cvtTYPE_EXPR (SpecialType x553) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x553))
     | cvtTYPE_EXPR (UnionType ls557) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x556 =>
                                                                                                           cvtTYPE_EXPR x556
                                                                                                    ) ls557)))
     | cvtTYPE_EXPR (ArrayType ls564) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x563 =>
                                                                                                           cvtTYPE_EXPR x563
                                                                                                    ) ls564)))
     | cvtTYPE_EXPR (TypeName x570) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x570))
     | cvtTYPE_EXPR (ElementTypeRef(x573, n574)) = PrettyRep.Ctor ("ElementTypeRef",
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x573, PrettyRep.Int n574]))
     | cvtTYPE_EXPR (FieldTypeRef(x578, x579)) = PrettyRep.Ctor ("FieldTypeRef",
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x578, cvtIDENT x579]))
     | cvtTYPE_EXPR (FunctionType x583) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x583))
     | cvtTYPE_EXPR (ObjectType ls587) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x586 =>
                                                                                                             cvtFIELD_TYPE x586
                                                                                                      ) ls587)))
     | cvtTYPE_EXPR (AppType{base=x593, args=ls595}) = PrettyRep.Ctor ("AppType",
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x593), ("args", PrettyRep.List (List.map (fn x594 =>
                                                                                                     cvtTYPE_EXPR x594
                                                                                              ) ls595))]))
     | cvtTYPE_EXPR (NullableType{expr=x606, nullable=b607}) = PrettyRep.Ctor ("NullableType",
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x606), ("nullable", PrettyRep.Bool b607)]))
     | cvtTYPE_EXPR (InstanceType x615) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x615))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x619) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x619))
     | cvtSTMT (InitStmt{kind=x622, ns=opt624, prototype=b628, static=b629,
          temps=x630, inits=ls632}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind",
          cvtVAR_DEFN_TAG x622), ("ns",
       (case opt624 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x623 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x623))
       )), ("prototype", PrettyRep.Bool b628), ("static", PrettyRep.Bool b629),
          ("temps", cvtBINDINGS x630), ("inits", PrettyRep.List (List.map (fn x631 =>
                                                                                 cvtINIT_STEP x631
                                                                          ) ls632))]))
     | cvtSTMT (ClassBlock{ns=opt652, ident=x656, name=opt658, block=x662}) =
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns",
       (case opt652 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x651 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x651))
       )), ("ident", cvtIDENT x656), ("name",
       (case opt658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x657 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x657))
       )), ("block", cvtBLOCK x662)]))
     | cvtSTMT (ForInStmt x674) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x674))
     | cvtSTMT (ThrowStmt x677) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x677))
     | cvtSTMT (ReturnStmt x680) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x680))
     | cvtSTMT (BreakStmt opt684) = PrettyRep.Ctor ("BreakStmt", SOME
       (case opt684 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x683 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x683))
       ))
     | cvtSTMT (ContinueStmt opt691) = PrettyRep.Ctor ("ContinueStmt", SOME
       (case opt691 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x690 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x690))
       ))
     | cvtSTMT (BlockStmt x697) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x697))
     | cvtSTMT (LabeledStmt(x700, x701)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x700,
          cvtSTMT x701]))
     | cvtSTMT (LetStmt x705) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x705))
     | cvtSTMT (WhileStmt x708) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x708))
     | cvtSTMT (DoWhileStmt x711) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x711))
     | cvtSTMT (ForStmt x714) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x714))
     | cvtSTMT (IfStmt{cnd=x717, thn=x718, els=x719}) = PrettyRep.Ctor ("IfStmt",
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x717), ("thn", cvtSTMT x718),
          ("els", cvtSTMT x719)]))
     | cvtSTMT (WithStmt{obj=x729, ty=x730, body=x731}) = PrettyRep.Ctor ("WithStmt",
          SOME (PrettyRep.Rec [("obj", cvtEXPR x729), ("ty", cvtTYPE_EXPR x730),
          ("body", cvtSTMT x731)]))
     | cvtSTMT (TryStmt{block=x741, catches=ls743, finally=opt748}) = PrettyRep.Ctor ("TryStmt",
          SOME (PrettyRep.Rec [("block", cvtBLOCK x741), ("catches", PrettyRep.List (List.map (fn x742 =>
                                                                                                     cvtCATCH_CLAUSE x742
                                                                                              ) ls743)),
          ("finally",
       (case opt748 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x747 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x747))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt762, cond=x766, labels=ls768, cases=ls773}) =
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode",
       (case opt762 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x761 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x761))
       )), ("cond", cvtEXPR x766), ("labels", PrettyRep.List (List.map (fn x767 =>
                                                                              cvtIDENT x767
                                                                       ) ls768)),
          ("cases", PrettyRep.List (List.map (fn x772 => cvtCASE x772
                                             ) ls773))]))
     | cvtSTMT (SwitchTypeStmt{cond=x788, ty=x789, cases=ls791}) = PrettyRep.Ctor ("SwitchTypeStmt",
          SOME (PrettyRep.Rec [("cond", cvtEXPR x788), ("ty", cvtTYPE_EXPR x789),
          ("cases", PrettyRep.List (List.map (fn x790 => cvtCATCH_CLAUSE x790
                                             ) ls791))]))
     | cvtSTMT (DXNStmt{expr=x804}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr",
          cvtEXPR x804)]))
   and cvtEXPR (TernaryExpr(x810, x811, x812)) = PrettyRep.Ctor ("TernaryExpr",
          SOME (PrettyRep.Tuple [cvtEXPR x810, cvtEXPR x811, cvtEXPR x812]))
     | cvtEXPR (BinaryExpr(x816, x817, x818)) = PrettyRep.Ctor ("BinaryExpr",
          SOME (PrettyRep.Tuple [cvtBINOP x816, cvtEXPR x817, cvtEXPR x818]))
     | cvtEXPR (BinaryTypeExpr(x822, x823, x824)) = PrettyRep.Ctor ("BinaryTypeExpr",
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x822, cvtEXPR x823, cvtTYPE_EXPR x824]))
     | cvtEXPR (ExpectedTypeExpr(x828, x829)) = PrettyRep.Ctor ("ExpectedTypeExpr",
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x828, cvtEXPR x829]))
     | cvtEXPR (UnaryExpr(x833, x834)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x833,
          cvtEXPR x834]))
     | cvtEXPR (TypeExpr x838) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x838))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt843) = PrettyRep.Ctor ("YieldExpr", SOME
       (case opt843 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x842 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x842))
       ))
     | cvtEXPR (SuperExpr opt850) = PrettyRep.Ctor ("SuperExpr", SOME
       (case opt850 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x849 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x849))
       ))
     | cvtEXPR (LiteralExpr x856) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x856))
     | cvtEXPR (CallExpr{func=x859, actuals=ls861}) = PrettyRep.Ctor ("CallExpr",
          SOME (PrettyRep.Rec [("func", cvtEXPR x859), ("actuals", PrettyRep.List (List.map (fn x860 =>
                                                                                                   cvtEXPR x860
                                                                                            ) ls861))]))
     | cvtEXPR (ApplyTypeExpr{expr=x872, actuals=ls874}) = PrettyRep.Ctor ("ApplyTypeExpr",
          SOME (PrettyRep.Rec [("expr", cvtEXPR x872), ("actuals", PrettyRep.List (List.map (fn x873 =>
                                                                                                   cvtTYPE_EXPR x873
                                                                                            ) ls874))]))
     | cvtEXPR (LetExpr{defs=x885, body=x886, head=opt888}) = PrettyRep.Ctor ("LetExpr",
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x885), ("body", cvtEXPR x886),
          ("head",
       (case opt888 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x887 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x887))
       ))]))
     | cvtEXPR (NewExpr{obj=x901, actuals=ls903}) = PrettyRep.Ctor ("NewExpr",
          SOME (PrettyRep.Rec [("obj", cvtEXPR x901), ("actuals", PrettyRep.List (List.map (fn x902 =>
                                                                                                  cvtEXPR x902
                                                                                           ) ls903))]))
     | cvtEXPR (ObjectRef{base=x914, ident=x915, loc=opt917}) = PrettyRep.Ctor ("ObjectRef",
          SOME (PrettyRep.Rec [("base", cvtEXPR x914), ("ident", cvtIDENT_EXPR x915),
          ("loc",
       (case opt917 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x916 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x916))
       ))]))
     | cvtEXPR (LexicalRef{ident=x930, loc=opt932}) = PrettyRep.Ctor ("LexicalRef",
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x930), ("loc",
       (case opt932 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x931 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x931))
       ))]))
     | cvtEXPR (SetExpr(x943, x944, x945)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x943,
          cvtEXPR x944, cvtEXPR x945]))
     | cvtEXPR (ListExpr ls950) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x949 =>
                                                                                                    cvtEXPR x949
                                                                                             ) ls950)))
     | cvtEXPR (InitExpr(x956, x957, x958)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x956,
          cvtHEAD x957, cvtINITS x958]))
     | cvtEXPR (SliceExpr(x962, x963, x964)) = PrettyRep.Ctor ("SliceExpr",
          SOME (PrettyRep.Tuple [cvtEXPR x962, cvtEXPR x963, cvtEXPR x964]))
     | cvtEXPR (GetTemp n968) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n968))
     | cvtEXPR (GetParam n971) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n971))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n977) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n977))
     | cvtFIXTURE_NAME (PropName x980) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x980))
   and cvtIDENT_EXPR (Identifier{ident=x983, openNamespaces=ls989}) = PrettyRep.Ctor ("Identifier",
          SOME (PrettyRep.Rec [("ident", cvtIDENT x983), ("openNamespaces",
          PrettyRep.List (List.map (fn ls985 => PrettyRep.List (List.map (fn x984 =>
                                                                                cvtNAMESPACE x984
                                                                         ) ls985)
                                   ) ls989))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1000, expr=x1001}) = PrettyRep.Ctor ("QualifiedExpression",
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1000), ("expr", cvtEXPR x1001)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1009) = PrettyRep.Ctor ("AttributeIdentifier",
          SOME (cvtIDENT_EXPR x1009))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1012, openNamespaces=ls1018}) =
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr",
          cvtEXPR x1012), ("openNamespaces", PrettyRep.List (List.map (fn ls1014 =>
                                                                             PrettyRep.List (List.map (fn x1013 =>
                                                                                                             cvtNAMESPACE x1013
                                                                                                      ) ls1014)
                                                                      ) ls1018))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1029, ident=s1030}) = PrettyRep.Ctor ("QualifiedIdentifier",
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1029), ("ident", PrettyRep.UniStr s1030)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1038, typeArgs=ls1040}) = PrettyRep.Ctor ("TypeIdentifier",
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1038), ("typeArgs",
          PrettyRep.List (List.map (fn x1039 => cvtTYPE_EXPR x1039
                                   ) ls1040))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1052, x1056)) = PrettyRep.Ctor ("UnresolvedPath",
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1051 => cvtIDENT x1051
                                                          ) ls1052), cvtIDENT_EXPR x1056]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier",
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined",
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1063) = PrettyRep.Ctor ("LiteralContextualDecimal",
          SOME (PrettyRep.String s1063))
     | cvtLITERAL (LiteralContextualDecimalInteger s1066) = PrettyRep.Ctor ("LiteralContextualDecimalInteger",
          SOME (PrettyRep.String s1066))
     | cvtLITERAL (LiteralContextualHexInteger s1069) = PrettyRep.Ctor ("LiteralContextualHexInteger",
          SOME (PrettyRep.String s1069))
     | cvtLITERAL (LiteralDouble r1072) = PrettyRep.Ctor ("LiteralDouble",
          SOME (PrettyRep.Real64 r1072))
     | cvtLITERAL (LiteralDecimal d1075) = PrettyRep.Ctor ("LiteralDecimal",
          SOME (PrettyRep.Dec d1075))
     | cvtLITERAL (LiteralInt i1078) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1078))
     | cvtLITERAL (LiteralUInt u1081) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1081))
     | cvtLITERAL (LiteralBoolean b1084) = PrettyRep.Ctor ("LiteralBoolean",
          SOME (PrettyRep.Bool b1084))
     | cvtLITERAL (LiteralString s1087) = PrettyRep.Ctor ("LiteralString",
          SOME (PrettyRep.UniStr s1087))
     | cvtLITERAL (LiteralArray{exprs=ls1091, ty=opt1096}) = PrettyRep.Ctor ("LiteralArray",
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1090 =>
                                                                         cvtEXPR x1090
                                                                  ) ls1091)),
          ("ty",
       (case opt1096 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1095 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1095))
       ))]))
     | cvtLITERAL (LiteralXML ls1108) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1107 =>
                                                                                                            cvtEXPR x1107
                                                                                                     ) ls1108)))
     | cvtLITERAL (LiteralNamespace x1114) = PrettyRep.Ctor ("LiteralNamespace",
          SOME (cvtNAMESPACE x1114))
     | cvtLITERAL (LiteralObject{expr=ls1118, ty=opt1123}) = PrettyRep.Ctor ("LiteralObject",
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1117 =>
                                                                        cvtFIELD x1117
                                                                 ) ls1118)),
          ("ty",
       (case opt1123 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1122 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1122))
       ))]))
     | cvtLITERAL (LiteralFunction x1134) = PrettyRep.Ctor ("LiteralFunction",
          SOME (cvtFUNC x1134))
     | cvtLITERAL (LiteralRegExp{str=s1137}) = PrettyRep.Ctor ("LiteralRegExp",
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1137)]))
   and cvtBLOCK (Block x1143) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1143))
   and cvtFIXTURE (NamespaceFixture x1146) = PrettyRep.Ctor ("NamespaceFixture",
          SOME (cvtNAMESPACE x1146))
     | cvtFIXTURE (ClassFixture x1149) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1149))
     | cvtFIXTURE (InterfaceFixture x1152) = PrettyRep.Ctor ("InterfaceFixture",
          SOME (cvtIFACE x1152))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1156) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1156))
     | cvtFIXTURE (MethodFixture{func=x1159, ty=x1160, readOnly=b1161, override=b1162,
          final=b1163, abstract=b1164}) = PrettyRep.Ctor ("MethodFixture",
          SOME (PrettyRep.Rec [("func", cvtFUNC x1159), ("ty", cvtTYPE_EXPR x1160),
          ("readOnly", PrettyRep.Bool b1161), ("override", PrettyRep.Bool b1162),
          ("final", PrettyRep.Bool b1163), ("abstract", PrettyRep.Bool b1164)]))
     | cvtFIXTURE (ValFixture{ty=x1180, readOnly=b1181}) = PrettyRep.Ctor ("ValFixture",
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1180), ("readOnly", PrettyRep.Bool b1181)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1189, getter=opt1191, setter=opt1196}) =
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty",
          cvtTYPE_EXPR x1189), ("getter",
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1190))
       )), ("setter",
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1195))
       ))]))
   and cvtHEAD (Head(x1209, x1210)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtFIXTURES x1209,
          cvtINITS x1210]))
   and cvtBINDINGS (ls1215, ls1220) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1214 =>
                                                                                       cvtBINDING x1214
                                                                                ) ls1215),
          PrettyRep.List (List.map (fn x1219 => cvtINIT_STEP x1219
                                   ) ls1220)]
   and cvtFIXTURES ls1228 = PrettyRep.List (List.map (fn (x1225, x1226) =>
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1225,
                                                            cvtFIXTURE x1226]
                                                     ) ls1228)
   and cvtINITS ls1235 = PrettyRep.List (List.map (fn (x1232, x1233) => PrettyRep.Tuple [cvtFIXTURE_NAME x1232,
                                                         cvtEXPR x1233]
                                                  ) ls1235)
   and cvtINSTANCE_TYPE {name=x1239, nonnullable=b1240, typeParams=ls1242,
          superTypes=ls1247, ty=x1251, conversionTy=opt1253, dynamic=b1257} =
          PrettyRep.Rec [("name", cvtNAME x1239), ("nonnullable", PrettyRep.Bool b1240),
          ("typeParams", PrettyRep.List (List.map (fn x1241 => cvtIDENT x1241
                                                  ) ls1242)), ("superTypes",
          PrettyRep.List (List.map (fn x1246 => cvtNAME x1246
                                   ) ls1247)), ("ty", cvtTYPE_EXPR x1251),
          ("conversionTy",
       (case opt1253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1252 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1252))
       )), ("dynamic", PrettyRep.Bool b1257)]
   and cvtFIELD {kind=x1273, name=x1274, init=x1275} = PrettyRep.Rec [("kind",
          cvtVAR_DEFN_TAG x1273), ("name", cvtIDENT_EXPR x1274), ("init", cvtEXPR x1275)]
   and cvtFIELD_TYPE {name=x1283, ty=x1284} = PrettyRep.Rec [("name", cvtIDENT x1283),
          ("ty", cvtTYPE_EXPR x1284)]
   and cvtFUNC_TYPE {typeParams=ls1291, params=ls1296, result=x1300, thisType=opt1302,
          hasRest=b1306, minArgs=n1307} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1290 =>
                                                                                                        cvtIDENT x1290
                                                                                                 ) ls1291)),
          ("params", PrettyRep.List (List.map (fn x1295 => cvtTYPE_EXPR x1295
                                              ) ls1296)), ("result", cvtTYPE_EXPR x1300),
          ("thisType",
       (case opt1302 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1301 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1301))
       )), ("hasRest", PrettyRep.Bool b1306), ("minArgs", PrettyRep.Int n1307)]
   and cvtFUNC_DEFN {kind=x1321, ns=opt1323, final=b1327, override=b1328, prototype=b1329,
          static=b1330, abstract=b1331, func=x1332} = PrettyRep.Rec [("kind",
          cvtVAR_DEFN_TAG x1321), ("ns",
       (case opt1323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1322 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1322))
       )), ("final", PrettyRep.Bool b1327), ("override", PrettyRep.Bool b1328),
          ("prototype", PrettyRep.Bool b1329), ("static", PrettyRep.Bool b1330),
          ("abstract", PrettyRep.Bool b1331), ("func", cvtFUNC x1332)]
   and cvtCTOR_DEFN x1350 = cvtCTOR x1350
   and cvtVAR_DEFN {kind=x1351, ns=opt1353, static=b1357, prototype=b1358,
          bindings=(ls1360, ls1365)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1351),
          ("ns",
       (case opt1353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1352))
       )), ("static", PrettyRep.Bool b1357), ("prototype", PrettyRep.Bool b1358),
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1359 =>
                                                                        cvtBINDING x1359
                                                                 ) ls1360),
          PrettyRep.List (List.map (fn x1364 => cvtINIT_STEP x1364
                                   ) ls1365)])]
   and cvtNAMESPACE_DEFN {ident=x1381, ns=opt1383, init=opt1388} = PrettyRep.Rec [("ident",
          cvtIDENT x1381), ("ns",
       (case opt1383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1382 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1382))
       )), ("init",
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1387 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1387))
       ))]
   and cvtCLASS_DEFN {ident=x1399, ns=opt1401, nonnullable=b1405, dynamic=b1406,
          final=b1407, params=ls1409, extends=opt1414, implements=ls1419, classDefns=ls1424,
          instanceDefns=ls1429, instanceStmts=ls1434, ctorDefn=opt1439} = PrettyRep.Rec [("ident",
          cvtIDENT x1399), ("ns",
       (case opt1401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1400 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1400))
       )), ("nonnullable", PrettyRep.Bool b1405), ("dynamic", PrettyRep.Bool b1406),
          ("final", PrettyRep.Bool b1407), ("params", PrettyRep.List (List.map (fn x1408 =>
                                                                                      cvtIDENT x1408
                                                                               ) ls1409)),
          ("extends",
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1413))
       )), ("implements", PrettyRep.List (List.map (fn x1418 => cvtIDENT_EXPR x1418
                                                   ) ls1419)), ("classDefns",
          PrettyRep.List (List.map (fn x1423 => cvtDEFN x1423
                                   ) ls1424)), ("instanceDefns", PrettyRep.List (List.map (fn x1428 =>
                                                                                                 cvtDEFN x1428
                                                                                          ) ls1429)),
          ("instanceStmts", PrettyRep.List (List.map (fn x1433 => cvtSTMT x1433
                                                     ) ls1434)), ("ctorDefn",

       (case opt1439 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1438))
       ))]
   and cvtINTERFACE_DEFN {ident=x1468, ns=opt1470, nonnullable=b1474, params=ls1476,
          extends=ls1481, instanceDefns=ls1486} = PrettyRep.Rec [("ident",
          cvtIDENT x1468), ("ns",
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1469))
       )), ("nonnullable", PrettyRep.Bool b1474), ("params", PrettyRep.List (List.map (fn x1475 =>
                                                                                             cvtIDENT x1475
                                                                                      ) ls1476)),
          ("extends", PrettyRep.List (List.map (fn x1480 => cvtIDENT_EXPR x1480
                                               ) ls1481)), ("instanceDefns",
          PrettyRep.List (List.map (fn x1485 => cvtDEFN x1485
                                   ) ls1486))]
   and cvtTYPE_DEFN {ident=x1503, ns=opt1505, init=x1509} = PrettyRep.Rec [("ident",
          cvtIDENT x1503), ("ns",
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1504 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1504))
       )), ("init", cvtTYPE_EXPR x1509)]
   and cvtFOR_ENUM_STMT {isEach=b1517, defn=opt1548, obj=x1552, fixtures=opt1560,
          next=x1564, labels=ls1566, body=x1570} = PrettyRep.Rec [("isEach",
          PrettyRep.Bool b1517), ("defn",
       (case opt1548 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1518, ns=opt1520, static=b1524, prototype=b1525, bindings=(ls1527,
            ls1532)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind",
            cvtVAR_DEFN_TAG x1518), ("ns",
         (case opt1520 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1519 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1519))
         )), ("static", PrettyRep.Bool b1524), ("prototype", PrettyRep.Bool b1525),
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1526 =>
                                                                          cvtBINDING x1526
                                                                   ) ls1527),
            PrettyRep.List (List.map (fn x1531 => cvtINIT_STEP x1531
                                     ) ls1532)])]))
       )), ("obj", cvtEXPR x1552), ("fixtures",
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1556 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1553,
                                                                                      x1554) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1553,
                                                                                      cvtFIXTURE x1554]
                                                                               ) ls1556)))
       )), ("next", cvtSTMT x1564), ("labels", PrettyRep.List (List.map (fn x1565 =>
                                                                               cvtIDENT x1565
                                                                        ) ls1566)),
          ("body", cvtSTMT x1570)]
   and cvtFOR_STMT {fixtures=opt1593, defn=opt1627, init=ls1632, cond=x1636,
          update=x1637, labels=ls1639, body=x1643} = PrettyRep.Rec [("fixtures",

       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1589 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1586,
                                                                                      x1587) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1586,
                                                                                      cvtFIXTURE x1587]
                                                                               ) ls1589)))
       )), ("defn",
       (case opt1627 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1597, ns=opt1599, static=b1603, prototype=b1604, bindings=(ls1606,
            ls1611)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind",
            cvtVAR_DEFN_TAG x1597), ("ns",
         (case opt1599 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1598 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1598))
         )), ("static", PrettyRep.Bool b1603), ("prototype", PrettyRep.Bool b1604),
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1605 =>
                                                                          cvtBINDING x1605
                                                                   ) ls1606),
            PrettyRep.List (List.map (fn x1610 => cvtINIT_STEP x1610
                                     ) ls1611)])]))
       )), ("init", PrettyRep.List (List.map (fn x1631 => cvtSTMT x1631
                                             ) ls1632)), ("cond", cvtEXPR x1636),
          ("update", cvtEXPR x1637), ("labels", PrettyRep.List (List.map (fn x1638 =>
                                                                                cvtIDENT x1638
                                                                         ) ls1639)),
          ("body", cvtSTMT x1643)]
   and cvtWHILE_STMT {cond=x1659, fixtures=opt1667, body=x1671, labels=ls1673} =
          PrettyRep.Rec [("cond", cvtEXPR x1659), ("fixtures",
       (case opt1667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1663 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1660,
                                                                                      x1661) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1660,
                                                                                      cvtFIXTURE x1661]
                                                                               ) ls1663)))
       )), ("body", cvtSTMT x1671), ("labels", PrettyRep.List (List.map (fn x1672 =>
                                                                               cvtIDENT x1672
                                                                        ) ls1673))]
   and cvtDIRECTIVES {pragmas=ls1687, defns=ls1692, head=opt1697, body=ls1702,
          loc=opt1707} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1686 =>
                                                                                    cvtPRAGMA x1686
                                                                             ) ls1687)),
          ("defns", PrettyRep.List (List.map (fn x1691 => cvtDEFN x1691
                                             ) ls1692)), ("head",
       (case opt1697 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1696 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1696))
       )), ("body", PrettyRep.List (List.map (fn x1701 => cvtSTMT x1701
                                             ) ls1702)), ("loc",
       (case opt1707 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1706 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1706))
       ))]
   and cvtCASE {label=opt1723, inits=opt1734, body=x1738} = PrettyRep.Rec [("label",

       (case opt1723 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1722 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1722))
       )), ("inits",
       (case opt1734 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1730 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1727,
                                                                                      x1728) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1727,
                                                                                      cvtEXPR x1728]
                                                                               ) ls1730)))
       )), ("body", cvtBLOCK x1738)]
   and cvtCATCH_CLAUSE {bindings=(ls1747, ls1752), ty=x1757, fixtures=opt1765,
          inits=opt1776, block=x1780} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1746 =>
                                                                                                                     cvtBINDING x1746
                                                                                                              ) ls1747),
          PrettyRep.List (List.map (fn x1751 => cvtINIT_STEP x1751
                                   ) ls1752)]), ("ty", cvtTYPE_EXPR x1757),
          ("fixtures",
       (case opt1765 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1761 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1758,
                                                                                      x1759) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1758,
                                                                                      cvtFIXTURE x1759]
                                                                               ) ls1761)))
       )), ("inits",
       (case opt1776 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1772 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1769,
                                                                                      x1770) =>
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1769,
                                                                                      cvtEXPR x1770]
                                                                               ) ls1772)))
       )), ("block", cvtBLOCK x1780)]
   and cvtFUNC_NAME {kind=x1792, ident=x1793} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1792),
          ("ident", cvtIDENT x1793)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1799, getter=opt1801, setter=opt1806} =
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1799), ("getter",
       (case opt1801 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1800 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1800))
       )), ("setter",
       (case opt1806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1805 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1805))
       ))]
   and cvtPACKAGE {name=ls1818, block=x1822} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1817 =>
                                                                                                       cvtIDENT x1817
                                                                                                ) ls1818)),
          ("block", cvtBLOCK x1822)]
   and cvtPROGRAM {packages=ls1829, fixtures=opt1834, block=x1838} = PrettyRep.Rec [("packages",
          PrettyRep.List (List.map (fn x1828 => cvtPACKAGE x1828
                                   ) ls1829)), ("fixtures",
       (case opt1834 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1833 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1833))
       )), ("block", cvtBLOCK x1838)]
end

