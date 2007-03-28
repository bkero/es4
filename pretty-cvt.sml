structure PrettyCvt = struct
   open Ast
   fun cvtPOS n0 = PrettyRep.Int n0
   and cvtUSTRING s1 = PrettyRep.String s1
   and cvtIDENT x2 = cvtUSTRING x2
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x5) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x5))
     | cvtNAMESPACE (Protected x8) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x8))
     | cvtNAMESPACE (Public x11) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x11))
     | cvtNAMESPACE (Internal x14) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x14))
     | cvtNAMESPACE (UserNamespace x17) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x17))
   and cvtNAME {ns=x20, id=x21} = PrettyRep.Rec [("ns", cvtNAMESPACE x20), 
          ("id", cvtIDENT x21)]
   and cvtMULTINAME {nss=ls32, id=x36} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls28 => 
                                                                                                PrettyRep.List (List.map (fn x27 => 
                                                                                                                                cvtNAMESPACE x27
                                                                                                                         ) ls28)
                                                                                         ) ls32)), 
          ("id", cvtIDENT x36)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x47, roundingMode=r48, precision=n49} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x47), ("roundingMode", 
          PrettyRep.DecRm r48), ("precision", PrettyRep.Int n49)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt62) = PrettyRep.Ctor ("Plus", SOME 
       (case opt62 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x61 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x61))
       ))
     | cvtBINOP (Minus opt69) = PrettyRep.Ctor ("Minus", SOME 
       (case opt69 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x68 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x68))
       ))
     | cvtBINOP (Times opt76) = PrettyRep.Ctor ("Times", SOME 
       (case opt76 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x75 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x75))
       ))
     | cvtBINOP (Divide opt83) = PrettyRep.Ctor ("Divide", SOME 
       (case opt83 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x82 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x82))
       ))
     | cvtBINOP (Remainder opt90) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt90 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x89 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x89))
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
     | cvtBINOP (Equals opt107) = PrettyRep.Ctor ("Equals", SOME 
       (case opt107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x106 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x106))
       ))
     | cvtBINOP (NotEquals opt114) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x113 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x113))
       ))
     | cvtBINOP (StrictEquals opt121) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x120 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x120))
       ))
     | cvtBINOP (StrictNotEquals opt128) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt128 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x127 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x127))
       ))
     | cvtBINOP (Less opt135) = PrettyRep.Ctor ("Less", SOME 
       (case opt135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x134 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x134))
       ))
     | cvtBINOP (LessOrEqual opt142) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt142 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x141 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x141))
       ))
     | cvtBINOP (Greater opt149) = PrettyRep.Ctor ("Greater", SOME 
       (case opt149 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x148 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x148))
       ))
     | cvtBINOP (GreaterOrEqual opt156) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x155 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x155))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt165) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x164 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x164))
       ))
     | cvtASSIGNOP (AssignMinus opt172) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x171 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x171))
       ))
     | cvtASSIGNOP (AssignTimes opt179) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x178))
       ))
     | cvtASSIGNOP (AssignDivide opt186) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x185 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x185))
       ))
     | cvtASSIGNOP (AssignRemainder opt193) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x192 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x192))
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
     | cvtUNOP (PreIncrement opt211) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x210 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x210))
       ))
     | cvtUNOP (PreDecrement opt218) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt218 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x217 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x217))
       ))
     | cvtUNOP (PostIncrement opt225) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x224 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x224))
       ))
     | cvtUNOP (PostDecrement opt232) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x231 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x231))
       ))
     | cvtUNOP (UnaryPlus opt239) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x238 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x238))
       ))
     | cvtUNOP (UnaryMinus opt246) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt246 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x245 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x245))
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
   and cvtPRAGMA (UseNamespace x263) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x263))
     | cvtPRAGMA (UseDefaultNamespace x266) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x266))
     | cvtPRAGMA (UseNumber x269) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x269))
     | cvtPRAGMA (UseRounding r272) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r272))
     | cvtPRAGMA (UsePrecision n275) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n275))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls281, name=x285, alias=opt287}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x280 => 
                                                                           cvtIDENT x280
                                                                    ) ls281)), 
          ("name", cvtIDENT x285), ("alias", 
       (case opt287 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x286 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x286))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
   and cvtCLS (Cls{name=x303, extends=opt305, implements=ls310, classFixtures=x314, 
          instanceFixtures=x315, instanceInits=x316, constructor=opt318, classType=x322, 
          instanceType=x323}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x303), ("extends", 
       (case opt305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x304 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x304))
       )), ("implements", PrettyRep.List (List.map (fn x309 => cvtNAME x309
                                                   ) ls310)), ("classFixtures", 
          cvtFIXTURES x314), ("instanceFixtures", cvtFIXTURES x315), ("instanceInits", 
          cvtHEAD x316), ("constructor", 
       (case opt318 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x317 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x317))
       )), ("classType", cvtTYPE_EXPR x322), ("instanceType", cvtTYPE_EXPR x323)]))
   and cvtCTOR (Ctor{settings=x345, superArgs=ls347, func=x351}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x345), ("superArgs", PrettyRep.List (List.map (fn x346 => 
                                                                                                         cvtEXPR x346
                                                                                                  ) ls347)), 
          ("func", cvtFUNC x351)]))
   and cvtFUNC (Func{name=x361, fsig=x362, isNative=b363, block=x364, param=x365, 
          defaults=ls367, ty=x371}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x361), ("fsig", cvtFUNC_SIG x362), ("isNative", PrettyRep.Bool b363), 
          ("block", cvtBLOCK x364), ("param", cvtHEAD x365), ("defaults", PrettyRep.List (List.map (fn x366 => 
                                                                                                          cvtEXPR x366
                                                                                                   ) ls367)), 
          ("ty", cvtFUNC_TYPE x371)]))
   and cvtDEFN (ClassDefn x389) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x389))
     | cvtDEFN (VariableDefn x392) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x392))
     | cvtDEFN (FunctionDefn x395) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x395))
     | cvtDEFN (ConstructorDefn x398) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x398))
     | cvtDEFN (InterfaceDefn x401) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x401))
     | cvtDEFN (NamespaceDefn x404) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x404))
     | cvtDEFN (TypeDefn x407) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x407))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls411, params=x415, defaults=ls417, 
          ctorInits=opt428, returnType=x432, thisType=opt434, hasRest=b438}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x410 => cvtIDENT x410
                                   ) ls411)), ("params", cvtBINDINGS x415), 
          ("defaults", PrettyRep.List (List.map (fn x416 => cvtEXPR x416
                                                ) ls417)), ("ctorInits", 
       (case opt428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x421, ls423) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x421, 
            PrettyRep.List (List.map (fn x422 => cvtEXPR x422
                                     ) ls423)]))
       )), ("returnType", cvtTYPE_EXPR x432), ("thisType", 
       (case opt434 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x433 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x433))
       )), ("hasRest", PrettyRep.Bool b438)]))
   and cvtBINDING (Binding{ident=x456, ty=opt458}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x456), ("ty", 
       (case opt458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x457 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x457))
       ))]))
   and cvtBINDING_IDENT (TempIdent n469) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n469))
     | cvtBINDING_IDENT (PropIdent x472) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x472))
   and cvtINIT_STEP (InitStep(x475, x476)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x475, 
          cvtEXPR x476]))
     | cvtINIT_STEP (AssignStep(x480, x481)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x480, cvtEXPR x481]))
   and cvtTYPE_EXPR (SpecialType x485) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x485))
     | cvtTYPE_EXPR (UnionType ls489) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x488 => 
                                                                                                           cvtTYPE_EXPR x488
                                                                                                    ) ls489)))
     | cvtTYPE_EXPR (ArrayType ls496) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x495 => 
                                                                                                           cvtTYPE_EXPR x495
                                                                                                    ) ls496)))
     | cvtTYPE_EXPR (TypeName x502) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x502))
     | cvtTYPE_EXPR (ElementTypeRef(x505, n506)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x505, PrettyRep.Int n506]))
     | cvtTYPE_EXPR (FieldTypeRef(x510, x511)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x510, cvtIDENT x511]))
     | cvtTYPE_EXPR (FunctionType x515) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x515))
     | cvtTYPE_EXPR (ObjectType ls519) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x518 => 
                                                                                                             cvtFIELD_TYPE x518
                                                                                                      ) ls519)))
     | cvtTYPE_EXPR (AppType{base=x525, args=ls527}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x525), ("args", PrettyRep.List (List.map (fn x526 => 
                                                                                                     cvtTYPE_EXPR x526
                                                                                              ) ls527))]))
     | cvtTYPE_EXPR (NullableType{expr=x538, nullable=b539}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x538), ("nullable", PrettyRep.Bool b539)]))
     | cvtTYPE_EXPR (InstanceType{name=x547, typeParams=ls549, ty=x553, isDynamic=b554}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x547), 
          ("typeParams", PrettyRep.List (List.map (fn x548 => cvtIDENT x548
                                                  ) ls549)), ("ty", cvtTYPE_EXPR x553), 
          ("isDynamic", PrettyRep.Bool b554)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x567) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x567))
     | cvtSTMT (InitStmt{kind=x570, ns=opt572, prototype=b576, static=b577, 
          temps=x578, inits=ls580}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x570), ("ns", 
       (case opt572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x571 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x571))
       )), ("prototype", PrettyRep.Bool b576), ("static", PrettyRep.Bool b577), 
          ("temps", cvtBINDINGS x578), ("inits", PrettyRep.List (List.map (fn x579 => 
                                                                                 cvtINIT_STEP x579
                                                                          ) ls580))]))
     | cvtSTMT (ClassBlock{ns=opt600, ident=x604, name=opt606, block=x610}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x599 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x599))
       )), ("ident", cvtIDENT x604), ("name", 
       (case opt606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x605 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x605))
       )), ("block", cvtBLOCK x610)]))
     | cvtSTMT (ForEachStmt x622) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x622))
     | cvtSTMT (ForInStmt x625) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x625))
     | cvtSTMT (ThrowStmt x628) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x628))
     | cvtSTMT (ReturnStmt x631) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x631))
     | cvtSTMT (BreakStmt opt635) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt635 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x634 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x634))
       ))
     | cvtSTMT (ContinueStmt opt642) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x641 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x641))
       ))
     | cvtSTMT (BlockStmt x648) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x648))
     | cvtSTMT (LabeledStmt(x651, x652)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x651, 
          cvtSTMT x652]))
     | cvtSTMT (LetStmt x656) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x656))
     | cvtSTMT (WhileStmt x659) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x659))
     | cvtSTMT (DoWhileStmt x662) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x662))
     | cvtSTMT (ForStmt x665) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x665))
     | cvtSTMT (IfStmt{cnd=x668, thn=x669, els=x670}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x668), ("thn", cvtSTMT x669), 
          ("els", cvtSTMT x670)]))
     | cvtSTMT (WithStmt{obj=x680, ty=x681, body=x682}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x680), ("ty", cvtTYPE_EXPR x681), 
          ("body", cvtSTMT x682)]))
     | cvtSTMT (TryStmt{block=x692, catches=ls714, finally=opt719}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x692), ("catches", PrettyRep.List (List.map (fn {bindings=x693, 
                                                                                                     ty=opt695, 
                                                                                                     fixtures=opt700, 
                                                                                                     block=x704} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x693), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt695 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x694 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x694))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt700 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x699 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x699))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x704)]
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
     | cvtEXPR (ObjectRef{base=x874, ident=x875}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x874), ("ident", cvtIDENT_EXPR x875)]))
     | cvtEXPR (LexicalRef{ident=x883}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x883)]))
     | cvtEXPR (SetExpr(x889, x890, x891)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x889, 
          cvtEXPR x890, cvtEXPR x891]))
     | cvtEXPR (ListExpr ls896) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x895 => 
                                                                                                    cvtEXPR x895
                                                                                             ) ls896)))
     | cvtEXPR (InitExpr(x902, x903, x904)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x902, 
          cvtHEAD x903, cvtINITS x904]))
     | cvtEXPR (SliceExpr(x908, x909, x910)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x908, cvtEXPR x909, cvtEXPR x910]))
     | cvtEXPR (GetTemp n914) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n914))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n920) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n920))
     | cvtFIXTURE_NAME (PropName x923) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x923))
   and cvtIDENT_EXPR (Identifier{ident=x926, openNamespaces=ls932}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x926), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls928 => PrettyRep.List (List.map (fn x927 => 
                                                                                cvtNAMESPACE x927
                                                                         ) ls928)
                                   ) ls932))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x943, expr=x944}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x943), ("expr", cvtEXPR x944)]))
     | cvtIDENT_EXPR (AttributeIdentifier x952) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x952))
     | cvtIDENT_EXPR (ExpressionIdentifier x955) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x955))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x958, ident=x959}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x958), ("ident", cvtUSTRING x959)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x967, typeArgs=ls969}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x967), ("typeArgs", 
          PrettyRep.List (List.map (fn x968 => cvtTYPE_EXPR x968
                                   ) ls969))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls981, x985)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x980 => cvtIDENT x980
                                                          ) ls981), cvtIDENT_EXPR x985]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s991) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s991))
     | cvtLITERAL (LiteralContextualDecimalInteger s994) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s994))
     | cvtLITERAL (LiteralContextualHexInteger s997) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s997))
     | cvtLITERAL (LiteralDouble r1000) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1000))
     | cvtLITERAL (LiteralDecimal d1003) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1003))
     | cvtLITERAL (LiteralInt i1006) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1006))
     | cvtLITERAL (LiteralUInt u1009) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1009))
     | cvtLITERAL (LiteralBoolean b1012) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1012))
     | cvtLITERAL (LiteralString x1015) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1015))
     | cvtLITERAL (LiteralArray{exprs=ls1019, ty=opt1024}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1018 => 
                                                                         cvtEXPR x1018
                                                                  ) ls1019)), 
          ("ty", 
       (case opt1024 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1023 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1023))
       ))]))
     | cvtLITERAL (LiteralXML ls1036) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1035 => 
                                                                                                            cvtEXPR x1035
                                                                                                     ) ls1036)))
     | cvtLITERAL (LiteralNamespace x1042) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1042))
     | cvtLITERAL (LiteralObject{expr=ls1046, ty=opt1051}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1045 => 
                                                                        cvtFIELD x1045
                                                                 ) ls1046)), 
          ("ty", 
       (case opt1051 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1050 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1050))
       ))]))
     | cvtLITERAL (LiteralFunction x1062) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1062))
     | cvtLITERAL (LiteralRegExp{str=x1065}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1065)]))
   and cvtBLOCK (Block x1071) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1071))
   and cvtFIXTURE (NamespaceFixture x1074) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1074))
     | cvtFIXTURE (ClassFixture x1077) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1077))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1081) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1081))
     | cvtFIXTURE (MethodFixture{func=x1084, ty=x1085, readOnly=b1086, override=b1087, 
          final=b1088}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1084), ("ty", cvtTYPE_EXPR x1085), ("readOnly", PrettyRep.Bool b1086), 
          ("override", PrettyRep.Bool b1087), ("final", PrettyRep.Bool b1088)]))
     | cvtFIXTURE (ValFixture{ty=x1102, readOnly=b1103}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1102), ("readOnly", PrettyRep.Bool b1103)]))
     | cvtFIXTURE (VirtualValFixture x1111) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1111))
   and cvtBINDINGS (ls1115, ls1120) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1114 => 
                                                                                       cvtBINDING x1114
                                                                                ) ls1115), 
          PrettyRep.List (List.map (fn x1119 => cvtINIT_STEP x1119
                                   ) ls1120)]
   and cvtFIXTURES ls1128 = PrettyRep.List (List.map (fn (x1125, x1126) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1125, 
                                                            cvtFIXTURE x1126]
                                                     ) ls1128)
   and cvtINITS ls1135 = PrettyRep.List (List.map (fn (x1132, x1133) => PrettyRep.Tuple [cvtFIXTURE_NAME x1132, 
                                                         cvtEXPR x1133]
                                                  ) ls1135)
   and cvtHEAD (x1139, x1140) = PrettyRep.Tuple [cvtFIXTURES x1139, cvtINITS x1140]
   and cvtFIELD {kind=x1142, name=x1143, init=x1144} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1142), ("name", cvtIDENT_EXPR x1143), ("init", cvtEXPR x1144)]
   and cvtFIELD_TYPE {name=x1152, ty=x1153} = PrettyRep.Rec [("name", cvtIDENT x1152), 
          ("ty", cvtTYPE_EXPR x1153)]
   and cvtTYPED_IDENT {name=x1159, ty=opt1161} = PrettyRep.Rec [("name", cvtIDENT x1159), 
          ("ty", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1171, params=ls1176, result=x1180, thisType=opt1182, 
          hasRest=b1186, minArgs=n1187} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1170 => 
                                                                                                        cvtIDENT x1170
                                                                                                 ) ls1171)), 
          ("params", PrettyRep.List (List.map (fn x1175 => cvtTYPE_EXPR x1175
                                              ) ls1176)), ("result", cvtTYPE_EXPR x1180), 
          ("thisType", 
       (case opt1182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1181 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1181))
       )), ("hasRest", PrettyRep.Bool b1186), ("minArgs", PrettyRep.Int n1187)]
   and cvtFUNC_DEFN {kind=x1201, ns=opt1203, final=b1207, override=b1208, prototype=b1209, 
          static=b1210, func=x1211} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1201), 
          ("ns", 
       (case opt1203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1202 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1202))
       )), ("final", PrettyRep.Bool b1207), ("override", PrettyRep.Bool b1208), 
          ("prototype", PrettyRep.Bool b1209), ("static", PrettyRep.Bool b1210), 
          ("func", cvtFUNC x1211)]
   and cvtCTOR_DEFN x1227 = cvtCTOR x1227
   and cvtVAR_DEFN {kind=x1228, ns=opt1230, static=b1234, prototype=b1235, 
          bindings=x1236} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1228), 
          ("ns", 
       (case opt1230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1229 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1229))
       )), ("static", PrettyRep.Bool b1234), ("prototype", PrettyRep.Bool b1235), 
          ("bindings", cvtBINDINGS x1236)]
   and cvtNAMESPACE_DEFN {ident=x1248, ns=opt1250, init=opt1255} = PrettyRep.Rec [("ident", 
          cvtIDENT x1248), ("ns", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1249))
       )), ("init", 
       (case opt1255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1254 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1254))
       ))]
   and cvtCLASS_DEFN {ident=x1266, ns=opt1268, nonnullable=b1272, dynamic=b1273, 
          final=b1274, params=ls1276, extends=opt1281, implements=ls1286, classDefns=ls1291, 
          instanceDefns=ls1296, instanceStmts=ls1301, ctorDefn=opt1306} = PrettyRep.Rec [("ident", 
          cvtIDENT x1266), ("ns", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1267))
       )), ("nonnullable", PrettyRep.Bool b1272), ("dynamic", PrettyRep.Bool b1273), 
          ("final", PrettyRep.Bool b1274), ("params", PrettyRep.List (List.map (fn x1275 => 
                                                                                      cvtIDENT x1275
                                                                               ) ls1276)), 
          ("extends", 
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1280))
       )), ("implements", PrettyRep.List (List.map (fn x1285 => cvtIDENT_EXPR x1285
                                                   ) ls1286)), ("classDefns", 
          PrettyRep.List (List.map (fn x1290 => cvtDEFN x1290
                                   ) ls1291)), ("instanceDefns", PrettyRep.List (List.map (fn x1295 => 
                                                                                                 cvtDEFN x1295
                                                                                          ) ls1296)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1300 => cvtSTMT x1300
                                                     ) ls1301)), ("ctorDefn", 
          
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1305))
       ))]
   and cvtINTERFACE_DEFN {ident=x1335, ns=opt1337, nonnullable=b1341, params=ls1343, 
          extends=ls1348, block=x1352} = PrettyRep.Rec [("ident", cvtIDENT x1335), 
          ("ns", 
       (case opt1337 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1336 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1336))
       )), ("nonnullable", PrettyRep.Bool b1341), ("params", PrettyRep.List (List.map (fn x1342 => 
                                                                                             cvtIDENT x1342
                                                                                      ) ls1343)), 
          ("extends", PrettyRep.List (List.map (fn x1347 => cvtIDENT_EXPR x1347
                                               ) ls1348)), ("block", cvtBLOCK x1352)]
   and cvtTYPE_DEFN {ident=x1366, ns=opt1368, init=x1372} = PrettyRep.Rec [("ident", 
          cvtIDENT x1366), ("ns", 
       (case opt1368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1367 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1367))
       )), ("init", cvtTYPE_EXPR x1372)]
   and cvtFOR_ENUM_STMT {defn=opt1381, obj=x1385, fixtures=opt1387, inits=opt1392, 
          labels=ls1397, body=x1401} = PrettyRep.Rec [("defn", 
       (case opt1381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1380 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1380))
       )), ("obj", cvtEXPR x1385), ("fixtures", 
       (case opt1387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1386 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1386))
       )), ("inits", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1391 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1391))
       )), ("labels", PrettyRep.List (List.map (fn x1396 => cvtIDENT x1396
                                               ) ls1397)), ("body", cvtSTMT x1401)]
   and cvtFOR_STMT {fixtures=opt1416, defn=opt1421, init=x1425, cond=x1426, 
          update=x1427, labels=ls1429, body=x1433} = PrettyRep.Rec [("fixtures", 
          
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1415))
       )), ("defn", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1420 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1420))
       )), ("init", cvtSTMT x1425), ("cond", cvtEXPR x1426), ("update", cvtEXPR x1427), 
          ("labels", PrettyRep.List (List.map (fn x1428 => cvtIDENT x1428
                                              ) ls1429)), ("body", cvtSTMT x1433)]
   and cvtWHILE_STMT {cond=x1449, fixtures=opt1451, body=x1455, labels=ls1457} = 
          PrettyRep.Rec [("cond", cvtEXPR x1449), ("fixtures", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1450))
       )), ("body", cvtSTMT x1455), ("labels", PrettyRep.List (List.map (fn x1456 => 
                                                                               cvtIDENT x1456
                                                                        ) ls1457))]
   and cvtDIRECTIVES {pragmas=ls1471, defns=ls1476, head=opt1481, body=ls1486, 
          pos=opt1491} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1470 => 
                                                                                    cvtPRAGMA x1470
                                                                             ) ls1471)), 
          ("defns", PrettyRep.List (List.map (fn x1475 => cvtDEFN x1475
                                             ) ls1476)), ("head", 
       (case opt1481 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1480 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1480))
       )), ("body", PrettyRep.List (List.map (fn x1485 => cvtSTMT x1485
                                             ) ls1486)), ("pos", 
       (case opt1491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1490 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1490))
       ))]
   and cvtCASE {label=opt1507, inits=opt1512, body=x1516} = PrettyRep.Rec [("label", 
          
       (case opt1507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1506 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1506))
       )), ("inits", 
       (case opt1512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1511 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1511))
       )), ("body", cvtBLOCK x1516)]
   and cvtTYPE_CASE {ty=opt1525, bindings=x1529, inits=opt1531, body=x1535} = 
          PrettyRep.Rec [("ty", 
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1524 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1524))
       )), ("bindings", cvtBINDINGS x1529), ("inits", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1530))
       )), ("body", cvtBLOCK x1535)]
   and cvtFUNC_NAME {kind=x1545, ident=x1546} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1545), 
          ("ident", cvtIDENT x1546)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1552, getter=opt1554, setter=opt1559} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1552), ("getter", 
       (case opt1554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1553 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1553))
       )), ("setter", 
       (case opt1559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1558 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1558))
       ))]
   and cvtPACKAGE {name=ls1571, block=x1575} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1570 => 
                                                                                                       cvtIDENT x1570
                                                                                                ) ls1571)), 
          ("block", cvtBLOCK x1575)]
   and cvtPROGRAM {packages=ls1582, fixtures=opt1587, block=x1591} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1581 => cvtPACKAGE x1581
                                   ) ls1582)), ("fixtures", 
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1586))
       )), ("block", cvtBLOCK x1591)]
end

