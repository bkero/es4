structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x4) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x4))
     | cvtNAMESPACE (Protected x7) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x7))
     | cvtNAMESPACE (Public x10) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x10))
     | cvtNAMESPACE (Internal x13) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x13))
     | cvtNAMESPACE (UserNamespace x16) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x16))
   and cvtNAME {ns=x19, id=x20} = PrettyRep.Rec [("ns", cvtNAMESPACE x19), 
          ("id", cvtIDENT x20)]
   and cvtMULTINAME {nss=ls31, id=x35} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls27 => 
                                                                                                PrettyRep.List (List.map (fn x26 => 
                                                                                                                                cvtNAMESPACE x26
                                                                                                                         ) ls27)
                                                                                         ) ls31)), 
          ("id", cvtIDENT x35)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtROUNDING_MODE (Ceiling) = PrettyRep.Ctor ("Ceiling", NONE)
     | cvtROUNDING_MODE (Floor) = PrettyRep.Ctor ("Floor", NONE)
     | cvtROUNDING_MODE (Up) = PrettyRep.Ctor ("Up", NONE)
     | cvtROUNDING_MODE (Down) = PrettyRep.Ctor ("Down", NONE)
     | cvtROUNDING_MODE (HalfUp) = PrettyRep.Ctor ("HalfUp", NONE)
     | cvtROUNDING_MODE (HalfDown) = PrettyRep.Ctor ("HalfDown", NONE)
     | cvtROUNDING_MODE (HalfEven) = PrettyRep.Ctor ("HalfEven", NONE)
   and cvtNUMERIC_MODE {numberType=x53, roundingMode=x54, precision=n55} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x53), ("roundingMode", 
          cvtROUNDING_MODE x54), ("precision", PrettyRep.Int n55)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt68) = PrettyRep.Ctor ("Plus", SOME 
       (case opt68 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x67 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x67))
       ))
     | cvtBINOP (Minus opt75) = PrettyRep.Ctor ("Minus", SOME 
       (case opt75 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x74 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x74))
       ))
     | cvtBINOP (Times opt82) = PrettyRep.Ctor ("Times", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Divide opt89) = PrettyRep.Ctor ("Divide", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
       ))
     | cvtBINOP (Remainder opt96) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt96 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x95 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x95))
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
     | cvtBINOP (Equals opt113) = PrettyRep.Ctor ("Equals", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (NotEquals opt120) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
       ))
     | cvtBINOP (StrictEquals opt127) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (StrictNotEquals opt134) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (Less opt141) = PrettyRep.Ctor ("Less", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (LessOrEqual opt148) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (Greater opt155) = PrettyRep.Ctor ("Greater", SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (GreaterOrEqual opt162) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x161 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x161))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt172) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x171 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x171))
       ))
     | cvtASSIGNOP (AssignMinus opt179) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x178 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x178))
       ))
     | cvtASSIGNOP (AssignTimes opt186) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x185 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x185))
       ))
     | cvtASSIGNOP (AssignDivide opt193) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x192 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x192))
       ))
     | cvtASSIGNOP (AssignRemainder opt200) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x199 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x199))
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
     | cvtUNOP (PreIncrement) = PrettyRep.Ctor ("PreIncrement", NONE)
     | cvtUNOP (PreDecrement) = PrettyRep.Ctor ("PreDecrement", NONE)
     | cvtUNOP (PostIncrement) = PrettyRep.Ctor ("PostIncrement", NONE)
     | cvtUNOP (PostDecrement) = PrettyRep.Ctor ("PostDecrement", NONE)
     | cvtUNOP (UnaryPlus) = PrettyRep.Ctor ("UnaryPlus", NONE)
     | cvtUNOP (UnaryMinus) = PrettyRep.Ctor ("UnaryMinus", NONE)
     | cvtUNOP (BitwiseNot) = PrettyRep.Ctor ("BitwiseNot", NONE)
     | cvtUNOP (LogicalNot) = PrettyRep.Ctor ("LogicalNot", NONE)
     | cvtUNOP (MakeNamespace) = PrettyRep.Ctor ("MakeNamespace", NONE)
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x235) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x235))
     | cvtPRAGMA (UseDefaultNamespace x238) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x238))
     | cvtPRAGMA (UseNumber x241) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x241))
     | cvtPRAGMA (UseRounding x244) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x244))
     | cvtPRAGMA (UsePrecision x247) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x247))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x252, name=x253, alias=opt255}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x252), ("name", cvtIDENT x253), 
          ("alias", 
       (case opt255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x254 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x254))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtCLS (Cls{extends=opt276, implements=ls281, classFixtures=x285, instanceFixtures=x286, 
          instanceInits=x287, constructor=opt289, classType=x293, instanceType=x294}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("extends", 
       (case opt276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x275 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x275))
       )), ("implements", PrettyRep.List (List.map (fn x280 => cvtNAME x280
                                                   ) ls281)), ("classFixtures", 
          cvtFIXTURES x285), ("instanceFixtures", cvtFIXTURES x286), ("instanceInits", 
          cvtINITS x287), ("constructor", 
       (case opt289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x288 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x288))
       )), ("classType", cvtTYPE_EXPR x293), ("instanceType", cvtTYPE_EXPR x294)]))
   and cvtCTOR (Ctor{settings=x314, func=x315}) = PrettyRep.Ctor ("Ctor", SOME (PrettyRep.Rec [("settings", 
          cvtINITS x314), ("func", cvtFUNC x315)]))
   and cvtFUNC (Func{name=x323, fsig=x324, fixtures=opt326, inits=ls331, body=x335}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x323), 
          ("fsig", cvtFUNC_SIG x324), ("fixtures", 
       (case opt326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x325 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x325))
       )), ("inits", PrettyRep.List (List.map (fn x330 => cvtSTMT x330
                                              ) ls331)), ("body", cvtBLOCK x335)]))
   and cvtDEFN (ClassDefn x349) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x349))
     | cvtDEFN (VariableDefn x352) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x352))
     | cvtDEFN (FunctionDefn x355) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x355))
     | cvtDEFN (ConstructorDefn x358) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x358))
     | cvtDEFN (InterfaceDefn x361) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x361))
     | cvtDEFN (NamespaceDefn x364) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x364))
     | cvtDEFN (TypeDefn x367) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x367))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls371, params=ls376, inits=ls381, 
          returnType=x385, thisType=opt387, hasRest=b391}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x370 => 
                                                                              cvtIDENT x370
                                                                       ) ls371)), 
          ("params", PrettyRep.List (List.map (fn x375 => cvtVAR_BINDING x375
                                              ) ls376)), ("inits", PrettyRep.List (List.map (fn x380 => 
                                                                                                   cvtSTMT x380
                                                                                            ) ls381)), 
          ("returnType", cvtTYPE_EXPR x385), ("thisType", 
       (case opt387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x386 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x386))
       )), ("hasRest", PrettyRep.Bool b391)]))
   and cvtVAR_BINDING (Binding{pattern=x407, ty=opt409, init=opt414}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x407), ("ty", 
       (case opt409 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x408 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x408))
       )), ("init", 
       (case opt414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x413 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x413))
       ))]))
   and cvtTYPE_EXPR (SpecialType x427) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x427))
     | cvtTYPE_EXPR (UnionType ls431) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x430 => 
                                                                                                           cvtTYPE_EXPR x430
                                                                                                    ) ls431)))
     | cvtTYPE_EXPR (ArrayType ls438) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x437 => 
                                                                                                           cvtTYPE_EXPR x437
                                                                                                    ) ls438)))
     | cvtTYPE_EXPR (NominalType{ident=x444}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x444)]))
     | cvtTYPE_EXPR (FunctionType x450) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x450))
     | cvtTYPE_EXPR (ObjectType ls454) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x453 => 
                                                                                                             cvtFIELD_TYPE x453
                                                                                                      ) ls454)))
     | cvtTYPE_EXPR (AppType{base=x460, args=ls462}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x460), ("args", PrettyRep.List (List.map (fn x461 => 
                                                                                                     cvtTYPE_EXPR x461
                                                                                              ) ls462))]))
     | cvtTYPE_EXPR (NullableType{expr=x473, nullable=b474}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x473), ("nullable", PrettyRep.Bool b474)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls484) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x483 => 
                                                                                                    cvtEXPR x483
                                                                                             ) ls484)))
     | cvtSTMT (InitStmt{kind=x490, ns=x491, prototype=b492, static=b493, inits=ls495}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x490), 
          ("ns", cvtEXPR x491), ("prototype", PrettyRep.Bool b492), ("static", 
          PrettyRep.Bool b493), ("inits", PrettyRep.List (List.map (fn x494 => 
                                                                          cvtEXPR x494
                                                                   ) ls495))]))
     | cvtSTMT (ClassBlock{ns=x512, ident=x513, name=opt515, extends=opt520, 
          fixtures=opt525, block=x529}) = PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
          cvtEXPR x512), ("ident", cvtIDENT x513), ("name", 
       (case opt515 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x514 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x514))
       )), ("extends", 
       (case opt520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x519 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x519))
       )), ("fixtures", 
       (case opt525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x524 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x524))
       )), ("block", cvtBLOCK x529)]))
     | cvtSTMT (PackageBlock{name=x545, block=x546}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x545), ("block", cvtBLOCK x546)]))
     | cvtSTMT (ForEachStmt x554) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x554))
     | cvtSTMT (ForInStmt x557) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x557))
     | cvtSTMT (ThrowStmt ls561) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x560 => 
                                                                                                      cvtEXPR x560
                                                                                               ) ls561)))
     | cvtSTMT (ReturnStmt ls568) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x567 => 
                                                                                                        cvtEXPR x567
                                                                                                 ) ls568)))
     | cvtSTMT (BreakStmt opt575) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt575 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x574 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x574))
       ))
     | cvtSTMT (ContinueStmt opt582) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt582 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x581 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x581))
       ))
     | cvtSTMT (BlockStmt x588) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x588))
     | cvtSTMT (LabeledStmt(x591, x592)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x591, 
          cvtSTMT x592]))
     | cvtSTMT (LetStmt(ls597, x601)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x596 => 
                                                                                                                          cvtVAR_BINDING x596
                                                                                                                   ) ls597), 
          cvtSTMT x601]))
     | cvtSTMT (SuperStmt ls606) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x605 => 
                                                                                                      cvtEXPR x605
                                                                                               ) ls606)))
     | cvtSTMT (WhileStmt x612) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x612))
     | cvtSTMT (DoWhileStmt x615) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x615))
     | cvtSTMT (ForStmt{defns=ls619, fixtures=opt624, init=ls629, cond=ls634, 
          update=ls639, contLabel=opt644, body=x648}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x618 => 
                                                                         cvtVAR_BINDING x618
                                                                  ) ls619)), 
          ("fixtures", 
       (case opt624 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x623 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x623))
       )), ("init", PrettyRep.List (List.map (fn x628 => cvtEXPR x628
                                             ) ls629)), ("cond", PrettyRep.List (List.map (fn x633 => 
                                                                                                 cvtEXPR x633
                                                                                          ) ls634)), 
          ("update", PrettyRep.List (List.map (fn x638 => cvtEXPR x638
                                              ) ls639)), ("contLabel", 
       (case opt644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x643 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x643))
       )), ("body", cvtSTMT x648)]))
     | cvtSTMT (IfStmt{cnd=x666, thn=x667, els=x668}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x666), ("thn", cvtSTMT x667), 
          ("els", cvtSTMT x668)]))
     | cvtSTMT (WithStmt{obj=ls679, ty=x683, body=x684}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x678 => 
                                                                       cvtEXPR x678
                                                                ) ls679)), 
          ("ty", cvtTYPE_EXPR x683), ("body", cvtSTMT x684)]))
     | cvtSTMT (TryStmt{body=x694, catches=ls709, finally=opt714}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x694), ("catches", PrettyRep.List (List.map (fn {bind=x695, 
                                                                                                    fixtures=opt697, 
                                                                                                    body=x701} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x695), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt697 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x696 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x696))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x701)]
                                                                                             ) ls709)), 
          ("finally", 
       (case opt714 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x713 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x713))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls728, cases=ls733}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x727 => 
                                                                        cvtEXPR x727
                                                                 ) ls728)), 
          ("cases", PrettyRep.List (List.map (fn x732 => cvtCASE x732
                                             ) ls733))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls745, ty=x749, cases=ls751}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x744 => 
                                                                        cvtEXPR x744
                                                                 ) ls745)), 
          ("ty", cvtTYPE_EXPR x749), ("cases", PrettyRep.List (List.map (fn x750 => 
                                                                               cvtTYPE_CASE x750
                                                                        ) ls751))]))
     | cvtSTMT (Dxns{expr=x764}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x764)]))
   and cvtEXPR (TrinaryExpr(x770, x771, x772, x773)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x770, cvtEXPR x771, cvtEXPR x772, 
          cvtEXPR x773]))
     | cvtEXPR (BinaryExpr(x777, x778, x779)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x777, cvtEXPR x778, cvtEXPR x779]))
     | cvtEXPR (BinaryTypeExpr(x783, x784, x785)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x783, cvtEXPR x784, cvtTYPE_EXPR x785]))
     | cvtEXPR (UnaryExpr(x789, x790)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x789, 
          cvtEXPR x790]))
     | cvtEXPR (TypeExpr x794) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x794))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt803) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt803 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls799 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x798 => 
                                                                                     cvtEXPR x798
                                                                              ) ls799)))
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
     | cvtEXPR (LetExpr{defs=ls846, body=ls851, fixtures=opt856}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x845 => 
                                                                        cvtVAR_BINDING x845
                                                                 ) ls846)), 
          ("body", PrettyRep.List (List.map (fn x850 => cvtEXPR x850
                                            ) ls851)), ("fixtures", 
       (case opt856 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x855 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x855))
       ))]))
     | cvtEXPR (NewExpr{obj=x869, actuals=ls871}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x869), ("actuals", PrettyRep.List (List.map (fn x870 => 
                                                                                                  cvtEXPR x870
                                                                                           ) ls871))]))
     | cvtEXPR (FunExpr x882) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x882))
     | cvtEXPR (ObjectRef{base=x885, ident=x886}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x885), ("ident", cvtIDENT_EXPR x886)]))
     | cvtEXPR (LexicalRef{ident=x894}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x894)]))
     | cvtEXPR (SetExpr(x900, x901, x902)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x900, 
          cvtPATTERN x901, cvtEXPR x902]))
     | cvtEXPR (AllocTemp(n906, x907)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n906, 
          cvtEXPR x907]))
     | cvtEXPR (KillTemp n911) = PrettyRep.Ctor ("KillTemp", SOME (PrettyRep.Int n911))
     | cvtEXPR (GetTemp n914) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n914))
     | cvtEXPR (ListExpr ls918) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x917 => 
                                                                                                    cvtEXPR x917
                                                                                             ) ls918)))
     | cvtEXPR (SliceExpr(ls925, ls930, ls935)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x924 => cvtEXPR x924
                                                          ) ls925), PrettyRep.List (List.map (fn x929 => 
                                                                                                    cvtEXPR x929
                                                                                             ) ls930), 
          PrettyRep.List (List.map (fn x934 => cvtEXPR x934
                                   ) ls935)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x942, ident=x943}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x942), ("ident", cvtUSTRING x943)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x951, expr=x952}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x951), ("expr", cvtEXPR x952)]))
     | cvtIDENT_EXPR (AttributeIdentifier x960) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x960))
     | cvtIDENT_EXPR (Identifier{ident=x963, openNamespaces=ls969}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x963), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls965 => PrettyRep.List (List.map (fn x964 => 
                                                                                cvtNAMESPACE x964
                                                                         ) ls965)
                                   ) ls969))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x980) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x980))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x983, typeParams=ls985}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x983), ("typeParams", 
          PrettyRep.List (List.map (fn x984 => cvtTYPE_EXPR x984
                                   ) ls985))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r998) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r998))
     | cvtLITERAL (LiteralBoolean b1001) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1001))
     | cvtLITERAL (LiteralString x1004) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1004))
     | cvtLITERAL (LiteralArray{exprs=ls1008, ty=opt1013}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1007 => 
                                                                         cvtEXPR x1007
                                                                  ) ls1008)), 
          ("ty", 
       (case opt1013 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1012 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1012))
       ))]))
     | cvtLITERAL (LiteralXML ls1025) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1024 => 
                                                                                                            cvtEXPR x1024
                                                                                                     ) ls1025)))
     | cvtLITERAL (LiteralNamespace x1031) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1031))
     | cvtLITERAL (LiteralObject{expr=ls1035, ty=opt1040}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1034 => 
                                                                        cvtFIELD x1034
                                                                 ) ls1035)), 
          ("ty", 
       (case opt1040 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1039 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1039))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x1051}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1051)]))
   and cvtBLOCK (Block x1057) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1057))
   and cvtPATTERN (ObjectPattern ls1061) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x1060 => cvtFIELD_PATTERN x1060
                                         ) ls1061)))
     | cvtPATTERN (ArrayPattern ls1068) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x1067 => 
                                                                                                                cvtPATTERN x1067
                                                                                                         ) ls1068)))
     | cvtPATTERN (SimplePattern x1074) = PrettyRep.Ctor ("SimplePattern", 
          SOME (cvtEXPR x1074))
     | cvtPATTERN (IdentifierPattern x1077) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x1077))
   and cvtFIXTURE (NamespaceFixture x1080) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1080))
     | cvtFIXTURE (ClassFixture x1083) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1083))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1087) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1087))
     | cvtFIXTURE (ValFixture{ty=x1090, readOnly=b1091, isOverride=b1092, isFinal=b1093, 
          init=opt1095}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1090), ("readOnly", PrettyRep.Bool b1091), ("isOverride", 
          PrettyRep.Bool b1092), ("isFinal", PrettyRep.Bool b1093), ("init", 
          
       (case opt1095 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1094 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1094))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1112, getter=opt1114, setter=opt1119}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1112), ("getter", 
       (case opt1114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1113 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1113))
       )), ("setter", 
       (case opt1119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1118 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1118))
       ))]))
   and cvtFIELD {kind=x1132, name=x1133, init=x1134} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1132), ("name", cvtIDENT_EXPR x1133), ("init", cvtEXPR x1134)]
   and cvtFIELD_PATTERN {name=x1142, ptrn=x1143} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1142), ("ptrn", cvtPATTERN x1143)]
   and cvtFIELD_TYPE {name=x1149, ty=x1150} = PrettyRep.Rec [("name", cvtIDENT x1149), 
          ("ty", cvtTYPE_EXPR x1150)]
   and cvtTYPED_IDENT {name=x1156, ty=opt1158} = PrettyRep.Rec [("name", cvtIDENT x1156), 
          ("ty", 
       (case opt1158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1157 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1157))
       ))]
   and cvtATTRIBUTES {ns=x1167, override=b1168, static=b1169, final=b1170, 
          dynamic=b1171, prototype=b1172, native=b1173, rest=b1174} = PrettyRep.Rec [("ns", 
          cvtEXPR x1167), ("override", PrettyRep.Bool b1168), ("static", PrettyRep.Bool b1169), 
          ("final", PrettyRep.Bool b1170), ("dynamic", PrettyRep.Bool b1171), 
          ("prototype", PrettyRep.Bool b1172), ("native", PrettyRep.Bool b1173), 
          ("rest", PrettyRep.Bool b1174)]
   and cvtFUNC_DEFN {kind=x1192, ns=x1193, final=b1194, native=b1195, override=b1196, 
          prototype=b1197, static=b1198, func=x1199} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1192), ("ns", cvtEXPR x1193), ("final", PrettyRep.Bool b1194), 
          ("native", PrettyRep.Bool b1195), ("override", PrettyRep.Bool b1196), 
          ("prototype", PrettyRep.Bool b1197), ("static", PrettyRep.Bool b1198), 
          ("func", cvtFUNC x1199)]
   and cvtCTOR_DEFN {ns=x1217, native=b1218, ctor=x1219} = PrettyRep.Rec [("ns", 
          cvtEXPR x1217), ("native", PrettyRep.Bool b1218), ("ctor", cvtCTOR x1219)]
   and cvtVAR_DEFN {kind=x1227, ns=x1228, static=b1229, prototype=b1230, bindings=ls1232} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1227), ("ns", cvtEXPR x1228), 
          ("static", PrettyRep.Bool b1229), ("prototype", PrettyRep.Bool b1230), 
          ("bindings", PrettyRep.List (List.map (fn x1231 => cvtVAR_BINDING x1231
                                                ) ls1232))]
   and cvtFIXTURES ls1250 = PrettyRep.List (List.map (fn (x1247, x1248) => 
                                                            PrettyRep.Tuple [cvtNAME x1247, 
                                                            cvtFIXTURE x1248]
                                                     ) ls1250)
   and cvtINITS ls1255 = PrettyRep.List (List.map (fn x1254 => cvtSTMT x1254
                                                  ) ls1255)
   and cvtNAMESPACE_DEFN {ident=x1259, ns=x1260, init=opt1262} = PrettyRep.Rec [("ident", 
          cvtIDENT x1259), ("ns", cvtEXPR x1260), ("init", 
       (case opt1262 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1261 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1261))
       ))]
   and cvtCLASS_DEFN {ident=x1273, ns=x1274, nonnullable=b1275, dynamic=b1276, 
          final=b1277, params=ls1279, extends=opt1284, implements=ls1289, body=x1293} = 
          PrettyRep.Rec [("ident", cvtIDENT x1273), ("ns", cvtEXPR x1274), 
          ("nonnullable", PrettyRep.Bool b1275), ("dynamic", PrettyRep.Bool b1276), 
          ("final", PrettyRep.Bool b1277), ("params", PrettyRep.List (List.map (fn x1278 => 
                                                                                      cvtIDENT x1278
                                                                               ) ls1279)), 
          ("extends", 
       (case opt1284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1283 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1283))
       )), ("implements", PrettyRep.List (List.map (fn x1288 => cvtIDENT_EXPR x1288
                                                   ) ls1289)), ("body", cvtBLOCK x1293)]
   and cvtINTERFACE_DEFN {ident=x1313, ns=x1314, nonnullable=b1315, params=ls1317, 
          extends=ls1322, body=x1326} = PrettyRep.Rec [("ident", cvtIDENT x1313), 
          ("ns", cvtEXPR x1314), ("nonnullable", PrettyRep.Bool b1315), ("params", 
          PrettyRep.List (List.map (fn x1316 => cvtIDENT x1316
                                   ) ls1317)), ("extends", PrettyRep.List (List.map (fn x1321 => 
                                                                                           cvtIDENT_EXPR x1321
                                                                                    ) ls1322)), 
          ("body", cvtBLOCK x1326)]
   and cvtTYPE_DEFN {ident=x1340, ns=x1341, init=x1342} = PrettyRep.Rec [("ident", 
          cvtIDENT x1340), ("ns", cvtEXPR x1341), ("init", cvtTYPE_EXPR x1342)]
   and cvtFOR_ENUM_STMT {ptrn=opt1351, obj=ls1356, defns=ls1361, fixtures=opt1366, 
          contLabel=opt1371, body=x1375} = PrettyRep.Rec [("ptrn", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1350))
       )), ("obj", PrettyRep.List (List.map (fn x1355 => cvtEXPR x1355
                                            ) ls1356)), ("defns", PrettyRep.List (List.map (fn x1360 => 
                                                                                                  cvtVAR_BINDING x1360
                                                                                           ) ls1361)), 
          ("fixtures", 
       (case opt1366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1365 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1365))
       )), ("contLabel", 
       (case opt1371 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1370 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1370))
       )), ("body", cvtSTMT x1375)]
   and cvtWHILE_STMT {cond=x1389, body=x1390, contLabel=opt1392} = PrettyRep.Rec [("cond", 
          cvtEXPR x1389), ("body", cvtSTMT x1390), ("contLabel", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1391 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1391))
       ))]
   and cvtDIRECTIVES {pragmas=ls1404, defns=ls1409, stmts=ls1414, fixtures=opt1419, 
          inits=opt1428} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1403 => 
                                                                                      cvtPRAGMA x1403
                                                                               ) ls1404)), 
          ("defns", PrettyRep.List (List.map (fn x1408 => cvtDEFN x1408
                                             ) ls1409)), ("stmts", PrettyRep.List (List.map (fn x1413 => 
                                                                                                   cvtSTMT x1413
                                                                                            ) ls1414)), 
          ("fixtures", 
       (case opt1419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1418 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1418))
       )), ("inits", 
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1424 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1423 => 
                                                                                      cvtSTMT x1423
                                                                               ) ls1424)))
       ))]
   and cvtBINDINGS {b=ls1444, i=ls1449} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1443 => 
                                                                                               cvtVAR_BINDING x1443
                                                                                        ) ls1444)), 
          ("i", PrettyRep.List (List.map (fn x1448 => cvtEXPR x1448
                                         ) ls1449))]
   and cvtCASE {label=opt1463, body=x1467} = PrettyRep.Rec [("label", 
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1459 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1458 => 
                                                                                      cvtEXPR x1458
                                                                               ) ls1459)))
       )), ("body", cvtBLOCK x1467)]
   and cvtTYPE_CASE {ptrn=opt1474, body=x1478} = PrettyRep.Rec [("ptrn", 
       (case opt1474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1473 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1473))
       )), ("body", cvtBLOCK x1478)]
   and cvtFUNC_NAME {kind=x1484, ident=x1485} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1484), 
          ("ident", cvtIDENT x1485)]
   and cvtPACKAGE {name=x1491, body=x1492} = PrettyRep.Rec [("name", cvtUSTRING x1491), 
          ("body", cvtBLOCK x1492)]
   and cvtPROGRAM {packages=ls1499, fixtures=opt1504, body=x1508} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1498 => cvtPACKAGE x1498
                                   ) ls1499)), ("fixtures", 
       (case opt1504 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1503 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1503))
       )), ("body", cvtBLOCK x1508)]
end

