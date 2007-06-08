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
   and cvtIFACE (Iface{name=x375, nonnullable=b376, extends=ls378, instanceFixtures=x382, 
          instanceType=x383}) = PrettyRep.Ctor ("Iface", SOME (PrettyRep.Rec [("name", 
          cvtNAME x375), ("nonnullable", PrettyRep.Bool b376), ("extends", 
          PrettyRep.List (List.map (fn x377 => cvtNAME x377
                                   ) ls378)), ("instanceFixtures", cvtFIXTURES x382), 
          ("instanceType", cvtTYPE_EXPR x383)]))
   and cvtCTOR (Ctor{settings=x397, superArgs=ls399, func=x403}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x397), ("superArgs", PrettyRep.List (List.map (fn x398 => 
                                                                                                         cvtEXPR x398
                                                                                                  ) ls399)), 
          ("func", cvtFUNC x403)]))
   and cvtFUNC (Func{name=x413, fsig=x414, isNative=b415, block=x416, param=x417, 
          defaults=ls419, ty=x423}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x413), ("fsig", cvtFUNC_SIG x414), ("isNative", PrettyRep.Bool b415), 
          ("block", cvtBLOCK x416), ("param", cvtHEAD x417), ("defaults", PrettyRep.List (List.map (fn x418 => 
                                                                                                          cvtEXPR x418
                                                                                                   ) ls419)), 
          ("ty", cvtFUNC_TYPE x423)]))
   and cvtDEFN (ClassDefn x441) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x441))
     | cvtDEFN (VariableDefn x444) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x444))
     | cvtDEFN (FunctionDefn x447) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x447))
     | cvtDEFN (ConstructorDefn x450) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x450))
     | cvtDEFN (InterfaceDefn x453) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x453))
     | cvtDEFN (NamespaceDefn x456) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x456))
     | cvtDEFN (TypeDefn x459) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x459))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls463, params=x467, paramTypes=ls469, 
          defaults=ls474, ctorInits=opt485, returnType=x489, thisType=opt491, 
          hasRest=b495}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x462 => cvtIDENT x462
                                   ) ls463)), ("params", cvtBINDINGS x467), 
          ("paramTypes", PrettyRep.List (List.map (fn x468 => cvtTYPE_EXPR x468
                                                  ) ls469)), ("defaults", PrettyRep.List (List.map (fn x473 => 
                                                                                                          cvtEXPR x473
                                                                                                   ) ls474)), 
          ("ctorInits", 
       (case opt485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x478, ls480) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x478, 
            PrettyRep.List (List.map (fn x479 => cvtEXPR x479
                                     ) ls480)]))
       )), ("returnType", cvtTYPE_EXPR x489), ("thisType", 
       (case opt491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x490 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x490))
       )), ("hasRest", PrettyRep.Bool b495)]))
   and cvtBINDING (Binding{ident=x515, ty=x516}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x515), ("ty", cvtTYPE_EXPR x516)]))
   and cvtBINDING_IDENT (TempIdent n524) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n524))
     | cvtBINDING_IDENT (ParamIdent n527) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n527))
     | cvtBINDING_IDENT (PropIdent x530) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x530))
   and cvtINIT_STEP (InitStep(x533, x534)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x533, 
          cvtEXPR x534]))
     | cvtINIT_STEP (AssignStep(x538, x539)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x538, cvtEXPR x539]))
   and cvtTYPE_EXPR (SpecialType x543) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x543))
     | cvtTYPE_EXPR (UnionType ls547) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x546 => 
                                                                                                           cvtTYPE_EXPR x546
                                                                                                    ) ls547)))
     | cvtTYPE_EXPR (ArrayType ls554) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x553 => 
                                                                                                           cvtTYPE_EXPR x553
                                                                                                    ) ls554)))
     | cvtTYPE_EXPR (TypeName x560) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x560))
     | cvtTYPE_EXPR (ElementTypeRef(x563, n564)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x563, PrettyRep.Int n564]))
     | cvtTYPE_EXPR (FieldTypeRef(x568, x569)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x568, cvtIDENT x569]))
     | cvtTYPE_EXPR (FunctionType x573) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x573))
     | cvtTYPE_EXPR (ObjectType ls577) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x576 => 
                                                                                                             cvtFIELD_TYPE x576
                                                                                                      ) ls577)))
     | cvtTYPE_EXPR (AppType{base=x583, args=ls585}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x583), ("args", PrettyRep.List (List.map (fn x584 => 
                                                                                                     cvtTYPE_EXPR x584
                                                                                              ) ls585))]))
     | cvtTYPE_EXPR (NullableType{expr=x596, nullable=b597}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x596), ("nullable", PrettyRep.Bool b597)]))
     | cvtTYPE_EXPR (InstanceType{name=x605, nonnullable=b606, typeParams=ls608, 
          ty=x612, isDynamic=b613}) = PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", 
          cvtNAME x605), ("nonnullable", PrettyRep.Bool b606), ("typeParams", 
          PrettyRep.List (List.map (fn x607 => cvtIDENT x607
                                   ) ls608)), ("ty", cvtTYPE_EXPR x612), ("isDynamic", 
          PrettyRep.Bool b613)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x628) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x628))
     | cvtSTMT (InitStmt{kind=x631, ns=opt633, prototype=b637, static=b638, 
          temps=x639, inits=ls641}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x631), ("ns", 
       (case opt633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x632 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x632))
       )), ("prototype", PrettyRep.Bool b637), ("static", PrettyRep.Bool b638), 
          ("temps", cvtBINDINGS x639), ("inits", PrettyRep.List (List.map (fn x640 => 
                                                                                 cvtINIT_STEP x640
                                                                          ) ls641))]))
     | cvtSTMT (ClassBlock{ns=opt661, ident=x665, name=opt667, block=x671}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x660 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x660))
       )), ("ident", cvtIDENT x665), ("name", 
       (case opt667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x666 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x666))
       )), ("block", cvtBLOCK x671)]))
     | cvtSTMT (ForInStmt x683) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x683))
     | cvtSTMT (ThrowStmt x686) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x686))
     | cvtSTMT (ReturnStmt x689) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x689))
     | cvtSTMT (BreakStmt opt693) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x692 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x692))
       ))
     | cvtSTMT (ContinueStmt opt700) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x699 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x699))
       ))
     | cvtSTMT (BlockStmt x706) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x706))
     | cvtSTMT (LabeledStmt(x709, x710)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x709, 
          cvtSTMT x710]))
     | cvtSTMT (LetStmt x714) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x714))
     | cvtSTMT (WhileStmt x717) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x717))
     | cvtSTMT (DoWhileStmt x720) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x720))
     | cvtSTMT (ForStmt x723) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x723))
     | cvtSTMT (IfStmt{cnd=x726, thn=x727, els=x728}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x726), ("thn", cvtSTMT x727), 
          ("els", cvtSTMT x728)]))
     | cvtSTMT (WithStmt{obj=x738, ty=x739, body=x740}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x738), ("ty", cvtTYPE_EXPR x739), 
          ("body", cvtSTMT x740)]))
     | cvtSTMT (TryStmt{block=x750, catches=ls752, finally=opt757}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x750), ("catches", PrettyRep.List (List.map (fn x751 => 
                                                                                                     cvtCATCH_CLAUSE x751
                                                                                              ) ls752)), 
          ("finally", 
       (case opt757 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x756 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x756))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt771, cond=x775, labels=ls777, cases=ls782}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt771 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x770 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x770))
       )), ("cond", cvtEXPR x775), ("labels", PrettyRep.List (List.map (fn x776 => 
                                                                              cvtIDENT x776
                                                                       ) ls777)), 
          ("cases", PrettyRep.List (List.map (fn x781 => cvtCASE x781
                                             ) ls782))]))
     | cvtSTMT (SwitchTypeStmt{cond=x797, ty=x798, cases=ls800}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x797), ("ty", cvtTYPE_EXPR x798), 
          ("cases", PrettyRep.List (List.map (fn x799 => cvtCATCH_CLAUSE x799
                                             ) ls800))]))
     | cvtSTMT (DXNStmt{expr=x813}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x813)]))
   and cvtEXPR (TernaryExpr(x819, x820, x821)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x819, cvtEXPR x820, cvtEXPR x821]))
     | cvtEXPR (BinaryExpr(x825, x826, x827)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x825, cvtEXPR x826, cvtEXPR x827]))
     | cvtEXPR (BinaryTypeExpr(x831, x832, x833)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x831, cvtEXPR x832, cvtTYPE_EXPR x833]))
     | cvtEXPR (ExpectedTypeExpr(x837, x838)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x837, cvtEXPR x838]))
     | cvtEXPR (UnaryExpr(x842, x843)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x842, 
          cvtEXPR x843]))
     | cvtEXPR (TypeExpr x847) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x847))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt852) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt852 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x851 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x851))
       ))
     | cvtEXPR (SuperExpr opt859) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt859 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x858 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x858))
       ))
     | cvtEXPR (LiteralExpr x865) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x865))
     | cvtEXPR (CallExpr{func=x868, actuals=ls870}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x868), ("actuals", PrettyRep.List (List.map (fn x869 => 
                                                                                                   cvtEXPR x869
                                                                                            ) ls870))]))
     | cvtEXPR (ApplyTypeExpr{expr=x881, actuals=ls883}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x881), ("actuals", PrettyRep.List (List.map (fn x882 => 
                                                                                                   cvtTYPE_EXPR x882
                                                                                            ) ls883))]))
     | cvtEXPR (LetExpr{defs=x894, body=x895, head=opt897}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x894), ("body", cvtEXPR x895), 
          ("head", 
       (case opt897 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x896 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x896))
       ))]))
     | cvtEXPR (NewExpr{obj=x910, actuals=ls912}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x910), ("actuals", PrettyRep.List (List.map (fn x911 => 
                                                                                                  cvtEXPR x911
                                                                                           ) ls912))]))
     | cvtEXPR (ObjectRef{base=x923, ident=x924, loc=opt926}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x923), ("ident", cvtIDENT_EXPR x924), 
          ("loc", 
       (case opt926 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x925 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x925))
       ))]))
     | cvtEXPR (LexicalRef{ident=x939, loc=opt941}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x939), ("loc", 
       (case opt941 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x940 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x940))
       ))]))
     | cvtEXPR (SetExpr(x952, x953, x954)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x952, 
          cvtEXPR x953, cvtEXPR x954]))
     | cvtEXPR (ListExpr ls959) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x958 => 
                                                                                                    cvtEXPR x958
                                                                                             ) ls959)))
     | cvtEXPR (InitExpr(x965, x966, x967)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x965, 
          cvtHEAD x966, cvtINITS x967]))
     | cvtEXPR (SliceExpr(x971, x972, x973)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x971, cvtEXPR x972, cvtEXPR x973]))
     | cvtEXPR (GetTemp n977) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n977))
     | cvtEXPR (GetParam n980) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n980))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n986) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n986))
     | cvtFIXTURE_NAME (PropName x989) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x989))
   and cvtIDENT_EXPR (Identifier{ident=x992, openNamespaces=ls998}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x992), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls994 => PrettyRep.List (List.map (fn x993 => 
                                                                                cvtNAMESPACE x993
                                                                         ) ls994)
                                   ) ls998))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1009, expr=x1010}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1009), ("expr", cvtEXPR x1010)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1018) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1018))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1021, openNamespaces=ls1027}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1021), ("openNamespaces", PrettyRep.List (List.map (fn ls1023 => 
                                                                             PrettyRep.List (List.map (fn x1022 => 
                                                                                                             cvtNAMESPACE x1022
                                                                                                      ) ls1023)
                                                                      ) ls1027))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1038, ident=s1039}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1038), ("ident", PrettyRep.UniStr s1039)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1047, typeArgs=ls1049}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1047), ("typeArgs", 
          PrettyRep.List (List.map (fn x1048 => cvtTYPE_EXPR x1048
                                   ) ls1049))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1061, x1065)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1060 => cvtIDENT x1060
                                                          ) ls1061), cvtIDENT_EXPR x1065]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1072) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1072))
     | cvtLITERAL (LiteralContextualDecimalInteger s1075) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1075))
     | cvtLITERAL (LiteralContextualHexInteger s1078) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1078))
     | cvtLITERAL (LiteralContextualOctInteger s1081) = PrettyRep.Ctor ("LiteralContextualOctInteger", 
          SOME (PrettyRep.String s1081))
     | cvtLITERAL (LiteralDouble r1084) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1084))
     | cvtLITERAL (LiteralDecimal d1087) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1087))
     | cvtLITERAL (LiteralInt i1090) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1090))
     | cvtLITERAL (LiteralUInt u1093) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1093))
     | cvtLITERAL (LiteralBoolean b1096) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1096))
     | cvtLITERAL (LiteralString s1099) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1099))
     | cvtLITERAL (LiteralArray{exprs=ls1103, ty=opt1108}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1102 => 
                                                                         cvtEXPR x1102
                                                                  ) ls1103)), 
          ("ty", 
       (case opt1108 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1107 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1107))
       ))]))
     | cvtLITERAL (LiteralXML ls1120) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1119 => 
                                                                                                            cvtEXPR x1119
                                                                                                     ) ls1120)))
     | cvtLITERAL (LiteralNamespace x1126) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1126))
     | cvtLITERAL (LiteralObject{expr=ls1130, ty=opt1135}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1129 => 
                                                                        cvtFIELD x1129
                                                                 ) ls1130)), 
          ("ty", 
       (case opt1135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1134 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1134))
       ))]))
     | cvtLITERAL (LiteralFunction x1146) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1146))
     | cvtLITERAL (LiteralRegExp{str=s1149}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1149)]))
   and cvtBLOCK (Block x1155) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1155))
   and cvtFIXTURE (NamespaceFixture x1158) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1158))
     | cvtFIXTURE (ClassFixture x1161) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1161))
     | cvtFIXTURE (InterfaceFixture x1164) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1164))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1168) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1168))
     | cvtFIXTURE (MethodFixture{func=x1171, ty=x1172, readOnly=b1173, override=b1174, 
          final=b1175}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1171), ("ty", cvtTYPE_EXPR x1172), ("readOnly", PrettyRep.Bool b1173), 
          ("override", PrettyRep.Bool b1174), ("final", PrettyRep.Bool b1175)]))
     | cvtFIXTURE (ValFixture{ty=x1189, readOnly=b1190}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1189), ("readOnly", PrettyRep.Bool b1190)]))
     | cvtFIXTURE (VirtualValFixture x1198) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1198))
   and cvtBINDINGS (ls1202, ls1207) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1201 => 
                                                                                       cvtBINDING x1201
                                                                                ) ls1202), 
          PrettyRep.List (List.map (fn x1206 => cvtINIT_STEP x1206
                                   ) ls1207)]
   and cvtFIXTURES ls1215 = PrettyRep.List (List.map (fn (x1212, x1213) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1212, 
                                                            cvtFIXTURE x1213]
                                                     ) ls1215)
   and cvtINITS ls1222 = PrettyRep.List (List.map (fn (x1219, x1220) => PrettyRep.Tuple [cvtFIXTURE_NAME x1219, 
                                                         cvtEXPR x1220]
                                                  ) ls1222)
   and cvtHEAD (x1226, x1227) = PrettyRep.Tuple [cvtFIXTURES x1226, cvtINITS x1227]
   and cvtFIELD {kind=x1229, name=x1230, init=x1231} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1229), ("name", cvtIDENT_EXPR x1230), ("init", cvtEXPR x1231)]
   and cvtFIELD_TYPE {name=x1239, ty=x1240} = PrettyRep.Rec [("name", cvtIDENT x1239), 
          ("ty", cvtTYPE_EXPR x1240)]
   and cvtFUNC_TYPE {typeParams=ls1247, params=ls1252, result=x1256, thisType=opt1258, 
          hasRest=b1262, minArgs=n1263} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1246 => 
                                                                                                        cvtIDENT x1246
                                                                                                 ) ls1247)), 
          ("params", PrettyRep.List (List.map (fn x1251 => cvtTYPE_EXPR x1251
                                              ) ls1252)), ("result", cvtTYPE_EXPR x1256), 
          ("thisType", 
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1257))
       )), ("hasRest", PrettyRep.Bool b1262), ("minArgs", PrettyRep.Int n1263)]
   and cvtFUNC_DEFN {kind=x1277, ns=opt1279, final=b1283, override=b1284, prototype=b1285, 
          static=b1286, func=x1287} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1277), 
          ("ns", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1278))
       )), ("final", PrettyRep.Bool b1283), ("override", PrettyRep.Bool b1284), 
          ("prototype", PrettyRep.Bool b1285), ("static", PrettyRep.Bool b1286), 
          ("func", cvtFUNC x1287)]
   and cvtCTOR_DEFN x1303 = cvtCTOR x1303
   and cvtVAR_DEFN {kind=x1304, ns=opt1306, static=b1310, prototype=b1311, 
          bindings=x1312} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1304), 
          ("ns", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1305))
       )), ("static", PrettyRep.Bool b1310), ("prototype", PrettyRep.Bool b1311), 
          ("bindings", cvtBINDINGS x1312)]
   and cvtNAMESPACE_DEFN {ident=x1324, ns=opt1326, init=opt1331} = PrettyRep.Rec [("ident", 
          cvtIDENT x1324), ("ns", 
       (case opt1326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1325 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1325))
       )), ("init", 
       (case opt1331 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1330 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1330))
       ))]
   and cvtCLASS_DEFN {ident=x1342, ns=opt1344, nonnullable=b1348, dynamic=b1349, 
          final=b1350, params=ls1352, extends=opt1357, implements=ls1362, classDefns=ls1367, 
          instanceDefns=ls1372, instanceStmts=ls1377, ctorDefn=opt1382} = PrettyRep.Rec [("ident", 
          cvtIDENT x1342), ("ns", 
       (case opt1344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1343 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1343))
       )), ("nonnullable", PrettyRep.Bool b1348), ("dynamic", PrettyRep.Bool b1349), 
          ("final", PrettyRep.Bool b1350), ("params", PrettyRep.List (List.map (fn x1351 => 
                                                                                      cvtIDENT x1351
                                                                               ) ls1352)), 
          ("extends", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1356))
       )), ("implements", PrettyRep.List (List.map (fn x1361 => cvtIDENT_EXPR x1361
                                                   ) ls1362)), ("classDefns", 
          PrettyRep.List (List.map (fn x1366 => cvtDEFN x1366
                                   ) ls1367)), ("instanceDefns", PrettyRep.List (List.map (fn x1371 => 
                                                                                                 cvtDEFN x1371
                                                                                          ) ls1372)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1376 => cvtSTMT x1376
                                                     ) ls1377)), ("ctorDefn", 
          
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1381))
       ))]
   and cvtINTERFACE_DEFN {ident=x1411, ns=opt1413, nonnullable=b1417, params=ls1419, 
          extends=ls1424, instanceDefns=ls1429} = PrettyRep.Rec [("ident", 
          cvtIDENT x1411), ("ns", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1412))
       )), ("nonnullable", PrettyRep.Bool b1417), ("params", PrettyRep.List (List.map (fn x1418 => 
                                                                                             cvtIDENT x1418
                                                                                      ) ls1419)), 
          ("extends", PrettyRep.List (List.map (fn x1423 => cvtIDENT_EXPR x1423
                                               ) ls1424)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1428 => cvtDEFN x1428
                                   ) ls1429))]
   and cvtTYPE_DEFN {ident=x1446, ns=opt1448, init=x1452} = PrettyRep.Rec [("ident", 
          cvtIDENT x1446), ("ns", 
       (case opt1448 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1447 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1447))
       )), ("init", cvtTYPE_EXPR x1452)]
   and cvtFOR_ENUM_STMT {isEach=b1460, defn=opt1462, obj=x1466, fixtures=opt1468, 
          next=x1472, labels=ls1474, body=x1478} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1460), ("defn", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1461 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1461))
       )), ("obj", cvtEXPR x1466), ("fixtures", 
       (case opt1468 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1467))
       )), ("next", cvtSTMT x1472), ("labels", PrettyRep.List (List.map (fn x1473 => 
                                                                               cvtIDENT x1473
                                                                        ) ls1474)), 
          ("body", cvtSTMT x1478)]
   and cvtFOR_STMT {fixtures=opt1495, defn=opt1500, init=ls1505, cond=x1509, 
          update=x1510, labels=ls1512, body=x1516} = PrettyRep.Rec [("fixtures", 
          
       (case opt1495 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1494 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1494))
       )), ("defn", 
       (case opt1500 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1499 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1499))
       )), ("init", PrettyRep.List (List.map (fn x1504 => cvtSTMT x1504
                                             ) ls1505)), ("cond", cvtEXPR x1509), 
          ("update", cvtEXPR x1510), ("labels", PrettyRep.List (List.map (fn x1511 => 
                                                                                cvtIDENT x1511
                                                                         ) ls1512)), 
          ("body", cvtSTMT x1516)]
   and cvtWHILE_STMT {cond=x1532, fixtures=opt1534, body=x1538, labels=ls1540} = 
          PrettyRep.Rec [("cond", cvtEXPR x1532), ("fixtures", 
       (case opt1534 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1533))
       )), ("body", cvtSTMT x1538), ("labels", PrettyRep.List (List.map (fn x1539 => 
                                                                               cvtIDENT x1539
                                                                        ) ls1540))]
   and cvtDIRECTIVES {pragmas=ls1554, defns=ls1559, head=opt1564, body=ls1569, 
          loc=opt1574} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1553 => 
                                                                                    cvtPRAGMA x1553
                                                                             ) ls1554)), 
          ("defns", PrettyRep.List (List.map (fn x1558 => cvtDEFN x1558
                                             ) ls1559)), ("head", 
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1563))
       )), ("body", PrettyRep.List (List.map (fn x1568 => cvtSTMT x1568
                                             ) ls1569)), ("loc", 
       (case opt1574 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1573))
       ))]
   and cvtCASE {label=opt1590, inits=opt1595, body=x1599} = PrettyRep.Rec [("label", 
          
       (case opt1590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1589 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1589))
       )), ("inits", 
       (case opt1595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1594 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1594))
       )), ("body", cvtBLOCK x1599)]
   and cvtCATCH_CLAUSE {bindings=x1607, ty=x1608, fixtures=opt1610, inits=opt1615, 
          block=x1619} = PrettyRep.Rec [("bindings", cvtBINDINGS x1607), ("ty", 
          cvtTYPE_EXPR x1608), ("fixtures", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1609 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1609))
       )), ("inits", 
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1614 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1614))
       )), ("block", cvtBLOCK x1619)]
   and cvtFUNC_NAME {kind=x1631, ident=x1632} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1631), 
          ("ident", cvtIDENT x1632)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1638, getter=opt1640, setter=opt1645} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1638), ("getter", 
       (case opt1640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1639 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1639))
       )), ("setter", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1644 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1644))
       ))]
   and cvtPACKAGE {name=ls1657, block=x1661} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1656 => 
                                                                                                       cvtIDENT x1656
                                                                                                ) ls1657)), 
          ("block", cvtBLOCK x1661)]
   and cvtPROGRAM {packages=ls1668, fixtures=opt1673, block=x1677} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1667 => cvtPACKAGE x1667
                                   ) ls1668)), ("fixtures", 
       (case opt1673 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1672 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1672))
       )), ("block", cvtBLOCK x1677)]
end

