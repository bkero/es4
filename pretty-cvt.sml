structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtUNIT_NAME ls21 = PrettyRep.List (List.map (fn x20 => cvtIDENT x20
                                                    ) ls21)
   and cvtFRAME_ID n25 = PrettyRep.Int n25
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x28) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Protected x31) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x31))
     | cvtNAMESPACE (Public x34) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x34))
     | cvtNAMESPACE (Internal x37) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x37))
     | cvtNAMESPACE (UserNamespace s40) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s40))
     | cvtNAMESPACE (AnonUserNamespace n43) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n43))
     | cvtNAMESPACE (LimitedNamespace(x46, x47)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x46, cvtNAMESPACE x47]))
   and cvtNAME {ns=x51, id=x52} = PrettyRep.Rec [("ns", cvtNAMESPACE x51), 
          ("id", cvtIDENT x52)]
   and cvtMULTINAME {nss=ls63, id=x67} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls59 => 
                                                                                                PrettyRep.List (List.map (fn x58 => 
                                                                                                                                cvtNAMESPACE x58
                                                                                                                         ) ls59)
                                                                                         ) ls63)), 
          ("id", cvtIDENT x67)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Byte) = PrettyRep.Ctor ("Byte", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x79, roundingMode=r80, precision=n81} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x79), ("roundingMode", 
          PrettyRep.DecRm r80), ("precision", PrettyRep.Int n81)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt93) = PrettyRep.Ctor ("Plus", SOME 
       (case opt93 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x92 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x92))
       ))
     | cvtBINOP (Minus opt100) = PrettyRep.Ctor ("Minus", SOME 
       (case opt100 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x99 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x99))
       ))
     | cvtBINOP (Times opt107) = PrettyRep.Ctor ("Times", SOME 
       (case opt107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x106 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x106))
       ))
     | cvtBINOP (Divide opt114) = PrettyRep.Ctor ("Divide", SOME 
       (case opt114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x113 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x113))
       ))
     | cvtBINOP (Remainder opt121) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x120 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x120))
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
     | cvtBINOP (Equals opt138) = PrettyRep.Ctor ("Equals", SOME 
       (case opt138 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x137 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x137))
       ))
     | cvtBINOP (NotEquals opt145) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt145 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x144 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x144))
       ))
     | cvtBINOP (StrictEquals opt152) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt152 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x151 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x151))
       ))
     | cvtBINOP (StrictNotEquals opt159) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt159 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x158 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x158))
       ))
     | cvtBINOP (Less opt166) = PrettyRep.Ctor ("Less", SOME 
       (case opt166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x165 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x165))
       ))
     | cvtBINOP (LessOrEqual opt173) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x172 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x172))
       ))
     | cvtBINOP (Greater opt180) = PrettyRep.Ctor ("Greater", SOME 
       (case opt180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x179 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x179))
       ))
     | cvtBINOP (GreaterOrEqual opt187) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x186 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x186))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt196) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x195 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x195))
       ))
     | cvtASSIGNOP (AssignMinus opt203) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x202 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x202))
       ))
     | cvtASSIGNOP (AssignTimes opt210) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x209 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x209))
       ))
     | cvtASSIGNOP (AssignDivide opt217) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt217 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x216 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x216))
       ))
     | cvtASSIGNOP (AssignRemainder opt224) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x223 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x223))
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
     | cvtUNOP (PreIncrement opt242) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x241 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x241))
       ))
     | cvtUNOP (PreDecrement opt249) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x248 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x248))
       ))
     | cvtUNOP (PostIncrement opt256) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt256 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x255 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x255))
       ))
     | cvtUNOP (PostDecrement opt263) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x262 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x262))
       ))
     | cvtUNOP (UnaryPlus opt270) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x269))
       ))
     | cvtUNOP (UnaryMinus opt277) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt277 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x276 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x276))
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
   and cvtPRAGMA (UseNamespace x294) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x294))
     | cvtPRAGMA (UseDefaultNamespace x297) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x297))
     | cvtPRAGMA (UseNumber x300) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x300))
     | cvtPRAGMA (UseRounding r303) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r303))
     | cvtPRAGMA (UsePrecision n306) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n306))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls312, name=x316, alias=opt318}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x311 => 
                                                                           cvtIDENT x311
                                                                    ) ls312)), 
          ("name", cvtIDENT x316), ("alias", 
       (case opt318 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x317 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x317))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x337, frameId=opt339, topUnit=opt344}) = PrettyRep.Ctor ("Ty", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x337), ("frameId", 
       (case opt339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x338 => PrettyRep.Ctor ("SOME", SOME (cvtFRAME_ID x338))
       )), ("topUnit", 
       (case opt344 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x343 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x343))
       ))]))
   and cvtCLS (Cls{name=x357, typeParams=ls359, nonnullable=b363, dynamic=b364, 
          extends=opt366, implements=ls371, classRib=x375, instanceRib=x376, 
          instanceInits=x377, constructor=opt379, classType=x383, instanceType=x384}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x357), 
          ("typeParams", PrettyRep.List (List.map (fn x358 => cvtIDENT x358
                                                  ) ls359)), ("nonnullable", 
          PrettyRep.Bool b363), ("dynamic", PrettyRep.Bool b364), ("extends", 
          
       (case opt366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x365 => PrettyRep.Ctor ("SOME", SOME (cvtTY x365))
       )), ("implements", PrettyRep.List (List.map (fn x370 => cvtTY x370
                                                   ) ls371)), ("classRib", 
          cvtRIB x375), ("instanceRib", cvtRIB x376), ("instanceInits", cvtHEAD x377), 
          ("constructor", 
       (case opt379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x378 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x378))
       )), ("classType", cvtTY x383), ("instanceType", cvtTY x384)]))
   and cvtIFACE (Iface{name=x412, typeParams=ls414, nonnullable=b418, extends=ls420, 
          instanceRib=x424, instanceType=x425}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x412), ("typeParams", PrettyRep.List (List.map (fn x413 => 
                                                                                                      cvtIDENT x413
                                                                                               ) ls414)), 
          ("nonnullable", PrettyRep.Bool b418), ("extends", PrettyRep.List (List.map (fn x419 => 
                                                                                            cvtTY x419
                                                                                     ) ls420)), 
          ("instanceRib", cvtRIB x424), ("instanceType", cvtTY x425)]))
   and cvtCTOR (Ctor{settings=x441, superArgs=ls443, func=x447}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x441), ("superArgs", PrettyRep.List (List.map (fn x442 => 
                                                                                                         cvtEXPR x442
                                                                                                  ) ls443)), 
          ("func", cvtFUNC x447)]))
   and cvtFUNC (Func{name=x457, fsig=x458, native=b459, block=opt461, param=x465, 
          defaults=ls467, ty=x471, loc=opt473}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x457), ("fsig", cvtFUNC_SIG x458), ("native", PrettyRep.Bool b459), 
          ("block", 
       (case opt461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x460 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x460))
       )), ("param", cvtHEAD x465), ("defaults", PrettyRep.List (List.map (fn x466 => 
                                                                                 cvtEXPR x466
                                                                          ) ls467)), 
          ("ty", cvtTY x471), ("loc", 
       (case opt473 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x472 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x472))
       ))]))
   and cvtDEFN (ClassDefn x496) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x496))
     | cvtDEFN (VariableDefn x499) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x499))
     | cvtDEFN (FunctionDefn x502) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x502))
     | cvtDEFN (ConstructorDefn x505) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x505))
     | cvtDEFN (InterfaceDefn x508) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x508))
     | cvtDEFN (NamespaceDefn x511) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x511))
     | cvtDEFN (TypeDefn x514) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x514))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls518, params=x522, paramTypes=ls524, 
          defaults=ls529, ctorInits=opt540, returnType=x544, thisType=opt546, 
          hasRest=b550}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x517 => cvtIDENT x517
                                   ) ls518)), ("params", cvtBINDINGS x522), 
          ("paramTypes", PrettyRep.List (List.map (fn x523 => cvtTYPE_EXPR x523
                                                  ) ls524)), ("defaults", PrettyRep.List (List.map (fn x528 => 
                                                                                                          cvtEXPR x528
                                                                                                   ) ls529)), 
          ("ctorInits", 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x533, ls535) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x533, 
            PrettyRep.List (List.map (fn x534 => cvtEXPR x534
                                     ) ls535)]))
       )), ("returnType", cvtTYPE_EXPR x544), ("thisType", 
       (case opt546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x545 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x545))
       )), ("hasRest", PrettyRep.Bool b550)]))
   and cvtBINDING (Binding{ident=x570, ty=x571}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x570), ("ty", cvtTYPE_EXPR x571)]))
   and cvtBINDING_IDENT (TempIdent n579) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n579))
     | cvtBINDING_IDENT (ParamIdent n582) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n582))
     | cvtBINDING_IDENT (PropIdent x585) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x585))
   and cvtINIT_STEP (InitStep(x588, x589)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x588, 
          cvtEXPR x589]))
     | cvtINIT_STEP (AssignStep(x593, x594)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x593, cvtEXPR x594]))
   and cvtTYPE_EXPR (SpecialType x598) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x598))
     | cvtTYPE_EXPR (UnionType ls602) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x601 => 
                                                                                                           cvtTYPE_EXPR x601
                                                                                                    ) ls602)))
     | cvtTYPE_EXPR (ArrayType ls609) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x608 => 
                                                                                                           cvtTYPE_EXPR x608
                                                                                                    ) ls609)))
     | cvtTYPE_EXPR (TypeName x615) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x615))
     | cvtTYPE_EXPR (ElementTypeRef(x618, n619)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x618, PrettyRep.Int n619]))
     | cvtTYPE_EXPR (FieldTypeRef(x623, x624)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x623, cvtIDENT x624]))
     | cvtTYPE_EXPR (FunctionType x628) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x628))
     | cvtTYPE_EXPR (ObjectType ls632) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x631 => 
                                                                                                             cvtFIELD_TYPE x631
                                                                                                      ) ls632)))
     | cvtTYPE_EXPR (AppType{base=x638, args=ls640}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x638), ("args", PrettyRep.List (List.map (fn x639 => 
                                                                                                     cvtTYPE_EXPR x639
                                                                                              ) ls640))]))
     | cvtTYPE_EXPR (LamType{params=ls652, body=x656}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x651 => 
                                                                          cvtIDENT x651
                                                                   ) ls652)), 
          ("body", cvtTYPE_EXPR x656)]))
     | cvtTYPE_EXPR (NullableType{expr=x664, nullable=b665}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x664), ("nullable", PrettyRep.Bool b665)]))
     | cvtTYPE_EXPR (InstanceType x673) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x673))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x677) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x677))
     | cvtSTMT (InitStmt{kind=x680, ns=opt682, prototype=b686, static=b687, 
          temps=x688, inits=ls690}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x680), ("ns", 
       (case opt682 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x681 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x681))
       )), ("prototype", PrettyRep.Bool b686), ("static", PrettyRep.Bool b687), 
          ("temps", cvtBINDINGS x688), ("inits", PrettyRep.List (List.map (fn x689 => 
                                                                                 cvtINIT_STEP x689
                                                                          ) ls690))]))
     | cvtSTMT (ClassBlock{ns=opt710, ident=x714, name=opt716, block=x720}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x709 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x709))
       )), ("ident", cvtIDENT x714), ("name", 
       (case opt716 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x715 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x715))
       )), ("block", cvtBLOCK x720)]))
     | cvtSTMT (ForInStmt x732) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x732))
     | cvtSTMT (ThrowStmt x735) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x735))
     | cvtSTMT (ReturnStmt x738) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x738))
     | cvtSTMT (BreakStmt opt742) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt742 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x741 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x741))
       ))
     | cvtSTMT (ContinueStmt opt749) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt749 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x748 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x748))
       ))
     | cvtSTMT (BlockStmt x755) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x755))
     | cvtSTMT (LabeledStmt(x758, x759)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x758, 
          cvtSTMT x759]))
     | cvtSTMT (LetStmt x763) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x763))
     | cvtSTMT (WhileStmt x766) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x766))
     | cvtSTMT (DoWhileStmt x769) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x769))
     | cvtSTMT (ForStmt x772) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x772))
     | cvtSTMT (IfStmt{cnd=x775, thn=x776, els=x777}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x775), ("thn", cvtSTMT x776), 
          ("els", cvtSTMT x777)]))
     | cvtSTMT (WithStmt{obj=x787, ty=x788, body=x789}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x787), ("ty", cvtTY x788), ("body", 
          cvtSTMT x789)]))
     | cvtSTMT (TryStmt{block=x799, catches=ls801, finally=opt806}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x799), ("catches", PrettyRep.List (List.map (fn x800 => 
                                                                                                     cvtCATCH_CLAUSE x800
                                                                                              ) ls801)), 
          ("finally", 
       (case opt806 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x805 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x805))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt820, cond=x824, labels=ls826, cases=ls831}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt820 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x819 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x819))
       )), ("cond", cvtEXPR x824), ("labels", PrettyRep.List (List.map (fn x825 => 
                                                                              cvtIDENT x825
                                                                       ) ls826)), 
          ("cases", PrettyRep.List (List.map (fn x830 => cvtCASE x830
                                             ) ls831))]))
     | cvtSTMT (SwitchTypeStmt{cond=x846, ty=x847, cases=ls849}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x846), ("ty", cvtTY x847), 
          ("cases", PrettyRep.List (List.map (fn x848 => cvtCATCH_CLAUSE x848
                                             ) ls849))]))
     | cvtSTMT (DXNStmt{expr=x862}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x862)]))
   and cvtEXPR (TernaryExpr(x868, x869, x870)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x868, cvtEXPR x869, cvtEXPR x870]))
     | cvtEXPR (BinaryExpr(x874, x875, x876)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x874, cvtEXPR x875, cvtEXPR x876]))
     | cvtEXPR (BinaryTypeExpr(x880, x881, x882)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x880, cvtEXPR x881, cvtTY x882]))
     | cvtEXPR (ExpectedTypeExpr(x886, x887)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x886, cvtEXPR x887]))
     | cvtEXPR (UnaryExpr(x891, x892)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x891, 
          cvtEXPR x892]))
     | cvtEXPR (TypeExpr x896) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x896))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt901) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt901 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x900 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x900))
       ))
     | cvtEXPR (SuperExpr opt908) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt908 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x907 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x907))
       ))
     | cvtEXPR (LiteralExpr x914) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x914))
     | cvtEXPR (CallExpr{func=x917, actuals=ls919}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x917), ("actuals", PrettyRep.List (List.map (fn x918 => 
                                                                                                   cvtEXPR x918
                                                                                            ) ls919))]))
     | cvtEXPR (ApplyTypeExpr{expr=x930, actuals=ls932}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x930), ("actuals", PrettyRep.List (List.map (fn x931 => 
                                                                                                   cvtTY x931
                                                                                            ) ls932))]))
     | cvtEXPR (LetExpr{defs=x943, body=x944, head=opt946}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x943), ("body", cvtEXPR x944), 
          ("head", 
       (case opt946 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x945 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x945))
       ))]))
     | cvtEXPR (NewExpr{obj=x959, actuals=ls961}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x959), ("actuals", PrettyRep.List (List.map (fn x960 => 
                                                                                                  cvtEXPR x960
                                                                                           ) ls961))]))
     | cvtEXPR (ObjectRef{base=x972, ident=x973, loc=opt975}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x972), ("ident", cvtIDENT_EXPR x973), 
          ("loc", 
       (case opt975 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x974 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x974))
       ))]))
     | cvtEXPR (LexicalRef{ident=x988, loc=opt990}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x988), ("loc", 
       (case opt990 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x989 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x989))
       ))]))
     | cvtEXPR (SetExpr(x1001, x1002, x1003)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x1001, cvtEXPR x1002, cvtEXPR x1003]))
     | cvtEXPR (ListExpr ls1008) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x1007 => 
                                                                                                     cvtEXPR x1007
                                                                                              ) ls1008)))
     | cvtEXPR (InitExpr(x1014, x1015, x1016)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x1014, cvtHEAD x1015, cvtINITS x1016]))
     | cvtEXPR (SliceExpr(x1020, x1021, x1022)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x1020, cvtEXPR x1021, cvtEXPR x1022]))
     | cvtEXPR (GetTemp n1026) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n1026))
     | cvtEXPR (GetParam n1029) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n1029))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n1035) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n1035))
     | cvtFIXTURE_NAME (PropName x1038) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x1038))
   and cvtIDENT_EXPR (Identifier{ident=x1041, openNamespaces=ls1047}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x1041), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls1043 => PrettyRep.List (List.map (fn x1042 => 
                                                                                 cvtNAMESPACE x1042
                                                                          ) ls1043)
                                   ) ls1047))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x1058, expr=x1059}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1058), ("expr", cvtEXPR x1059)]))
     | cvtIDENT_EXPR (AttributeIdentifier x1067) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x1067))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x1070, openNamespaces=ls1076}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x1070), ("openNamespaces", PrettyRep.List (List.map (fn ls1072 => 
                                                                             PrettyRep.List (List.map (fn x1071 => 
                                                                                                             cvtNAMESPACE x1071
                                                                                                      ) ls1072)
                                                                      ) ls1076))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1087, ident=s1088}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1087), ("ident", PrettyRep.UniStr s1088)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1097, x1101)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1096 => cvtIDENT x1096
                                                          ) ls1097), cvtIDENT_EXPR x1101]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1108) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1108))
     | cvtLITERAL (LiteralContextualDecimalInteger s1111) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1111))
     | cvtLITERAL (LiteralContextualHexInteger s1114) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1114))
     | cvtLITERAL (LiteralDouble r1117) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1117))
     | cvtLITERAL (LiteralDecimal d1120) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1120))
     | cvtLITERAL (LiteralInt i1123) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1123))
     | cvtLITERAL (LiteralUInt u1126) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1126))
     | cvtLITERAL (LiteralBoolean b1129) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1129))
     | cvtLITERAL (LiteralString s1132) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1132))
     | cvtLITERAL (LiteralArray{exprs=ls1136, ty=opt1141}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1135 => 
                                                                         cvtEXPR x1135
                                                                  ) ls1136)), 
          ("ty", 
       (case opt1141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1140 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1140))
       ))]))
     | cvtLITERAL (LiteralXML ls1153) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1152 => 
                                                                                                            cvtEXPR x1152
                                                                                                     ) ls1153)))
     | cvtLITERAL (LiteralNamespace x1159) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1159))
     | cvtLITERAL (LiteralObject{expr=ls1163, ty=opt1168}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1162 => 
                                                                        cvtFIELD x1162
                                                                 ) ls1163)), 
          ("ty", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtTY x1167))
       ))]))
     | cvtLITERAL (LiteralFunction x1179) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1179))
     | cvtLITERAL (LiteralRegExp{str=s1182}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1182)]))
   and cvtBLOCK (Block x1188) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1188))
   and cvtFIXTURE (NamespaceFixture x1191) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1191))
     | cvtFIXTURE (ClassFixture x1194) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1194))
     | cvtFIXTURE (InterfaceFixture x1197) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1197))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1201) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1201))
     | cvtFIXTURE (MethodFixture{func=x1204, ty=x1205, readOnly=b1206, override=b1207, 
          final=b1208}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1204), ("ty", cvtTY x1205), ("readOnly", PrettyRep.Bool b1206), 
          ("override", PrettyRep.Bool b1207), ("final", PrettyRep.Bool b1208)]))
     | cvtFIXTURE (ValFixture{ty=x1222, readOnly=b1223}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1222), ("readOnly", PrettyRep.Bool b1223)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1231, getter=opt1233, setter=opt1238}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1231), ("getter", 
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1232))
       )), ("setter", 
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1237))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1251, baseTypeArgs=ls1253}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1251), ("baseTypeArgs", PrettyRep.List (List.map (fn x1252 => 
                                                                           cvtTY x1252
                                                                    ) ls1253))]))
   and cvtHEAD (Head(x1264, x1265)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1264, 
          cvtINITS x1265]))
   and cvtBINDINGS (ls1270, ls1275) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1269 => 
                                                                                       cvtBINDING x1269
                                                                                ) ls1270), 
          PrettyRep.List (List.map (fn x1274 => cvtINIT_STEP x1274
                                   ) ls1275)]
   and cvtRIB ls1283 = PrettyRep.List (List.map (fn (x1280, x1281) => PrettyRep.Tuple [cvtFIXTURE_NAME x1280, 
                                                       cvtFIXTURE x1281]
                                                ) ls1283)
   and cvtRIBS ls1288 = PrettyRep.List (List.map (fn x1287 => cvtRIB x1287
                                                 ) ls1288)
   and cvtINITS ls1295 = PrettyRep.List (List.map (fn (x1292, x1293) => PrettyRep.Tuple [cvtFIXTURE_NAME x1292, 
                                                         cvtEXPR x1293]
                                                  ) ls1295)
   and cvtINSTANCE_TYPE {name=x1299, typeArgs=ls1301, nonnullable=b1305, superTypes=ls1307, 
          ty=x1311, conversionTy=opt1313, dynamic=b1317} = PrettyRep.Rec [("name", 
          cvtNAME x1299), ("typeArgs", PrettyRep.List (List.map (fn x1300 => 
                                                                       cvtTYPE_EXPR x1300
                                                                ) ls1301)), 
          ("nonnullable", PrettyRep.Bool b1305), ("superTypes", PrettyRep.List (List.map (fn x1306 => 
                                                                                                cvtTYPE_EXPR x1306
                                                                                         ) ls1307)), 
          ("ty", cvtTYPE_EXPR x1311), ("conversionTy", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1312))
       )), ("dynamic", PrettyRep.Bool b1317)]
   and cvtFIELD {kind=x1333, name=x1334, init=x1335} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1333), ("name", cvtIDENT_EXPR x1334), ("init", cvtEXPR x1335)]
   and cvtFIELD_TYPE {name=x1343, ty=x1344} = PrettyRep.Rec [("name", cvtIDENT x1343), 
          ("ty", cvtTYPE_EXPR x1344)]
   and cvtFUNC_TYPE {params=ls1351, result=x1355, thisType=opt1357, hasRest=b1361, 
          minArgs=n1362} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1350 => 
                                                                                     cvtTYPE_EXPR x1350
                                                                              ) ls1351)), 
          ("result", cvtTYPE_EXPR x1355), ("thisType", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1356))
       )), ("hasRest", PrettyRep.Bool b1361), ("minArgs", PrettyRep.Int n1362)]
   and cvtFUNC_DEFN {kind=x1374, ns=opt1376, final=b1380, override=b1381, prototype=b1382, 
          static=b1383, func=x1384} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1374), 
          ("ns", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1375))
       )), ("final", PrettyRep.Bool b1380), ("override", PrettyRep.Bool b1381), 
          ("prototype", PrettyRep.Bool b1382), ("static", PrettyRep.Bool b1383), 
          ("func", cvtFUNC x1384)]
   and cvtCTOR_DEFN x1400 = cvtCTOR x1400
   and cvtVAR_DEFN {kind=x1401, ns=opt1403, static=b1407, prototype=b1408, 
          bindings=(ls1410, ls1415)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1401), 
          ("ns", 
       (case opt1403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1402 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1402))
       )), ("static", PrettyRep.Bool b1407), ("prototype", PrettyRep.Bool b1408), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1409 => 
                                                                        cvtBINDING x1409
                                                                 ) ls1410), 
          PrettyRep.List (List.map (fn x1414 => cvtINIT_STEP x1414
                                   ) ls1415)])]
   and cvtNAMESPACE_DEFN {ident=x1431, ns=opt1433, init=opt1438} = PrettyRep.Rec [("ident", 
          cvtIDENT x1431), ("ns", 
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1432))
       )), ("init", 
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1437 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1437))
       ))]
   and cvtCLASS_DEFN {ns=opt1450, ident=x1454, nonnullable=b1455, dynamic=b1456, 
          final=b1457, params=ls1459, extends=opt1464, implements=ls1469, classDefns=ls1474, 
          instanceDefns=ls1479, instanceStmts=ls1484, ctorDefn=opt1489} = PrettyRep.Rec [("ns", 
          
       (case opt1450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1449 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1449))
       )), ("ident", cvtIDENT x1454), ("nonnullable", PrettyRep.Bool b1455), 
          ("dynamic", PrettyRep.Bool b1456), ("final", PrettyRep.Bool b1457), 
          ("params", PrettyRep.List (List.map (fn x1458 => cvtIDENT x1458
                                              ) ls1459)), ("extends", 
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1463 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1463))
       )), ("implements", PrettyRep.List (List.map (fn x1468 => cvtTYPE_EXPR x1468
                                                   ) ls1469)), ("classDefns", 
          PrettyRep.List (List.map (fn x1473 => cvtDEFN x1473
                                   ) ls1474)), ("instanceDefns", PrettyRep.List (List.map (fn x1478 => 
                                                                                                 cvtDEFN x1478
                                                                                          ) ls1479)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1483 => cvtSTMT x1483
                                                     ) ls1484)), ("ctorDefn", 
          
       (case opt1489 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1488 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1488))
       ))]
   and cvtINTERFACE_DEFN {ident=x1518, ns=opt1520, nonnullable=b1524, params=ls1526, 
          extends=ls1531, instanceDefns=ls1536} = PrettyRep.Rec [("ident", 
          cvtIDENT x1518), ("ns", 
       (case opt1520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1519 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1519))
       )), ("nonnullable", PrettyRep.Bool b1524), ("params", PrettyRep.List (List.map (fn x1525 => 
                                                                                             cvtIDENT x1525
                                                                                      ) ls1526)), 
          ("extends", PrettyRep.List (List.map (fn x1530 => cvtTYPE_EXPR x1530
                                               ) ls1531)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1535 => cvtDEFN x1535
                                   ) ls1536))]
   and cvtTYPE_DEFN {ident=x1553, ns=opt1555, init=x1559} = PrettyRep.Rec [("ident", 
          cvtIDENT x1553), ("ns", 
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1554))
       )), ("init", cvtTYPE_EXPR x1559)]
   and cvtFOR_ENUM_STMT {isEach=b1567, defn=opt1598, obj=x1602, rib=opt1610, 
          next=x1614, labels=ls1616, body=x1620} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1567), ("defn", 
       (case opt1598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1568, ns=opt1570, static=b1574, prototype=b1575, bindings=(ls1577, 
            ls1582)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1568), ("ns", 
         (case opt1570 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1569 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1569))
         )), ("static", PrettyRep.Bool b1574), ("prototype", PrettyRep.Bool b1575), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1576 => 
                                                                          cvtBINDING x1576
                                                                   ) ls1577), 
            PrettyRep.List (List.map (fn x1581 => cvtINIT_STEP x1581
                                     ) ls1582)])]))
       )), ("obj", cvtEXPR x1602), ("rib", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1606 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1603, 
                                                                                      x1604) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1603, 
                                                                                      cvtFIXTURE x1604]
                                                                               ) ls1606)))
       )), ("next", cvtSTMT x1614), ("labels", PrettyRep.List (List.map (fn x1615 => 
                                                                               cvtIDENT x1615
                                                                        ) ls1616)), 
          ("body", cvtSTMT x1620)]
   and cvtFOR_STMT {rib=opt1643, defn=opt1677, init=ls1682, cond=x1686, update=x1687, 
          labels=ls1689, body=x1693} = PrettyRep.Rec [("rib", 
       (case opt1643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1639 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1636, 
                                                                                      x1637) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1636, 
                                                                                      cvtFIXTURE x1637]
                                                                               ) ls1639)))
       )), ("defn", 
       (case opt1677 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1647, ns=opt1649, static=b1653, prototype=b1654, bindings=(ls1656, 
            ls1661)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1647), ("ns", 
         (case opt1649 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1648 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1648))
         )), ("static", PrettyRep.Bool b1653), ("prototype", PrettyRep.Bool b1654), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1655 => 
                                                                          cvtBINDING x1655
                                                                   ) ls1656), 
            PrettyRep.List (List.map (fn x1660 => cvtINIT_STEP x1660
                                     ) ls1661)])]))
       )), ("init", PrettyRep.List (List.map (fn x1681 => cvtSTMT x1681
                                             ) ls1682)), ("cond", cvtEXPR x1686), 
          ("update", cvtEXPR x1687), ("labels", PrettyRep.List (List.map (fn x1688 => 
                                                                                cvtIDENT x1688
                                                                         ) ls1689)), 
          ("body", cvtSTMT x1693)]
   and cvtWHILE_STMT {cond=x1709, rib=opt1717, body=x1721, labels=ls1723} = 
          PrettyRep.Rec [("cond", cvtEXPR x1709), ("rib", 
       (case opt1717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1713 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1710, 
                                                                                      x1711) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1710, 
                                                                                      cvtFIXTURE x1711]
                                                                               ) ls1713)))
       )), ("body", cvtSTMT x1721), ("labels", PrettyRep.List (List.map (fn x1722 => 
                                                                               cvtIDENT x1722
                                                                        ) ls1723))]
   and cvtDIRECTIVES {pragmas=ls1737, defns=ls1742, head=opt1747, body=ls1752, 
          loc=opt1757} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1736 => 
                                                                                    cvtPRAGMA x1736
                                                                             ) ls1737)), 
          ("defns", PrettyRep.List (List.map (fn x1741 => cvtDEFN x1741
                                             ) ls1742)), ("head", 
       (case opt1747 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1746 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1746))
       )), ("body", PrettyRep.List (List.map (fn x1751 => cvtSTMT x1751
                                             ) ls1752)), ("loc", 
       (case opt1757 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1756 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1756))
       ))]
   and cvtCASE {label=opt1773, inits=opt1784, body=x1788} = PrettyRep.Rec [("label", 
          
       (case opt1773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1772 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1772))
       )), ("inits", 
       (case opt1784 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1780 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1777, 
                                                                                      x1778) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1777, 
                                                                                      cvtEXPR x1778]
                                                                               ) ls1780)))
       )), ("body", cvtBLOCK x1788)]
   and cvtCATCH_CLAUSE {bindings=(ls1797, ls1802), ty=x1807, rib=opt1815, inits=opt1826, 
          block=x1830} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1796 => 
                                                                                                      cvtBINDING x1796
                                                                                               ) ls1797), 
          PrettyRep.List (List.map (fn x1801 => cvtINIT_STEP x1801
                                   ) ls1802)]), ("ty", cvtTY x1807), ("rib", 
          
       (case opt1815 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1811 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1808, 
                                                                                      x1809) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1808, 
                                                                                      cvtFIXTURE x1809]
                                                                               ) ls1811)))
       )), ("inits", 
       (case opt1826 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1822 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1819, 
                                                                                      x1820) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1819, 
                                                                                      cvtEXPR x1820]
                                                                               ) ls1822)))
       )), ("block", cvtBLOCK x1830)]
   and cvtFUNC_NAME {kind=x1842, ident=x1843} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1842), 
          ("ident", cvtIDENT x1843)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1849, getter=opt1851, setter=opt1856} = 
          PrettyRep.Rec [("ty", cvtTY x1849), ("getter", 
       (case opt1851 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1850 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1850))
       )), ("setter", 
       (case opt1856 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1855 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1855))
       ))]
   and cvtFRAGMENT (Unit{name=opt1868, fragments=ls1873}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1868 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1867 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1867))
       )), ("fragments", PrettyRep.List (List.map (fn x1872 => cvtFRAGMENT x1872
                                                  ) ls1873))]))
     | cvtFRAGMENT (Package{name=ls1885, fragments=ls1890}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1884 => 
                                                                        cvtIDENT x1884
                                                                 ) ls1885)), 
          ("fragments", PrettyRep.List (List.map (fn x1889 => cvtFRAGMENT x1889
                                                 ) ls1890))]))
     | cvtFRAGMENT (Anon x1901) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1901))
end

