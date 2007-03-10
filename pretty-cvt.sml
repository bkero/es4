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
   and cvtNUMERIC_MODE {numberType=x46, roundingMode=r47, precision=n48} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x46), ("roundingMode", 
          PrettyRep.DecRm r47), ("precision", PrettyRep.Int n48)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt61) = PrettyRep.Ctor ("Plus", SOME 
       (case opt61 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x60 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x60))
       ))
     | cvtBINOP (Minus opt68) = PrettyRep.Ctor ("Minus", SOME 
       (case opt68 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x67 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x67))
       ))
     | cvtBINOP (Times opt75) = PrettyRep.Ctor ("Times", SOME 
       (case opt75 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x74 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x74))
       ))
     | cvtBINOP (Divide opt82) = PrettyRep.Ctor ("Divide", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Remainder opt89) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
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
     | cvtBINOP (Equals opt106) = PrettyRep.Ctor ("Equals", SOME 
       (case opt106 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x105 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x105))
       ))
     | cvtBINOP (NotEquals opt113) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (StrictEquals opt120) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
       ))
     | cvtBINOP (StrictNotEquals opt127) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (Less opt134) = PrettyRep.Ctor ("Less", SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (LessOrEqual opt141) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (Greater opt148) = PrettyRep.Ctor ("Greater", SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (GreaterOrEqual opt155) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
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
   and cvtPRAGMA (UseNamespace x228) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x228))
     | cvtPRAGMA (UseDefaultNamespace x231) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x231))
     | cvtPRAGMA (UseNumber x234) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x234))
     | cvtPRAGMA (UseRounding r237) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r237))
     | cvtPRAGMA (UsePrecision n240) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n240))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x245, name=x246, alias=opt248}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x245), ("name", cvtIDENT x246), 
          ("alias", 
       (case opt248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x247 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x247))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtCLS (Cls{name=x268, extends=opt270, implements=ls275, classFixtures=x279, 
          instanceFixtures=x280, instanceInits=x281, constructor=opt283, classType=x287, 
          instanceType=x288}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x268), ("extends", 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x269))
       )), ("implements", PrettyRep.List (List.map (fn x274 => cvtNAME x274
                                                   ) ls275)), ("classFixtures", 
          cvtFIXTURES x279), ("instanceFixtures", cvtFIXTURES x280), ("instanceInits", 
          cvtHEAD x281), ("constructor", 
       (case opt283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x282 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x282))
       )), ("classType", cvtTYPE_EXPR x287), ("instanceType", cvtTYPE_EXPR x288)]))
   and cvtCTOR (Ctor{settings=x310, superArgs=ls312, func=x316}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x310), ("superArgs", PrettyRep.List (List.map (fn x311 => 
                                                                                                         cvtEXPR x311
                                                                                                  ) ls312)), 
          ("func", cvtFUNC x316)]))
   and cvtFUNC (Func{name=x326, fsig=x327, isNative=b328, block=x329, param=x330, 
          defaults=ls332, ty=x336}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x326), ("fsig", cvtFUNC_SIG x327), ("isNative", PrettyRep.Bool b328), 
          ("block", cvtBLOCK x329), ("param", cvtHEAD x330), ("defaults", PrettyRep.List (List.map (fn x331 => 
                                                                                                          cvtEXPR x331
                                                                                                   ) ls332)), 
          ("ty", cvtFUNC_TYPE x336)]))
   and cvtDEFN (ClassDefn x354) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x354))
     | cvtDEFN (VariableDefn x357) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x357))
     | cvtDEFN (FunctionDefn x360) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x360))
     | cvtDEFN (ConstructorDefn x363) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x363))
     | cvtDEFN (InterfaceDefn x366) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x366))
     | cvtDEFN (NamespaceDefn x369) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x369))
     | cvtDEFN (TypeDefn x372) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x372))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls376, params=x380, defaults=ls382, 
          ctorInits=opt393, returnType=x397, thisType=opt399, hasRest=b403}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x375 => cvtIDENT x375
                                   ) ls376)), ("params", cvtBINDINGS x380), 
          ("defaults", PrettyRep.List (List.map (fn x381 => cvtEXPR x381
                                                ) ls382)), ("ctorInits", 
       (case opt393 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x386, ls388) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x386, 
            PrettyRep.List (List.map (fn x387 => cvtEXPR x387
                                     ) ls388)]))
       )), ("returnType", cvtTYPE_EXPR x397), ("thisType", 
       (case opt399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x398 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x398))
       )), ("hasRest", PrettyRep.Bool b403)]))
   and cvtBINDING (Binding{ident=x421, ty=opt423}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x421), ("ty", 
       (case opt423 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x422 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x422))
       ))]))
   and cvtBINDING_IDENT (TempIdent n434) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n434))
     | cvtBINDING_IDENT (PropIdent x437) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x437))
   and cvtINIT_STEP (InitStep(x440, x441)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x440, 
          cvtEXPR x441]))
     | cvtINIT_STEP (AssignStep(x445, x446)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x445, cvtEXPR x446]))
   and cvtTYPE_EXPR (SpecialType x450) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x450))
     | cvtTYPE_EXPR (UnionType ls454) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x453 => 
                                                                                                           cvtTYPE_EXPR x453
                                                                                                    ) ls454)))
     | cvtTYPE_EXPR (ArrayType ls461) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x460 => 
                                                                                                           cvtTYPE_EXPR x460
                                                                                                    ) ls461)))
     | cvtTYPE_EXPR (TypeName x467) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x467))
     | cvtTYPE_EXPR (ElementTypeRef(x470, n471)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x470, PrettyRep.Int n471]))
     | cvtTYPE_EXPR (FieldTypeRef(x475, x476)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x475, cvtIDENT x476]))
     | cvtTYPE_EXPR (FunctionType x480) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x480))
     | cvtTYPE_EXPR (ObjectType ls484) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x483 => 
                                                                                                             cvtFIELD_TYPE x483
                                                                                                      ) ls484)))
     | cvtTYPE_EXPR (AppType{base=x490, args=ls492}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x490), ("args", PrettyRep.List (List.map (fn x491 => 
                                                                                                     cvtTYPE_EXPR x491
                                                                                              ) ls492))]))
     | cvtTYPE_EXPR (NullableType{expr=x503, nullable=b504}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x503), ("nullable", PrettyRep.Bool b504)]))
     | cvtTYPE_EXPR (InstanceType{name=x512, typeParams=ls514, ty=x518}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x512), 
          ("typeParams", PrettyRep.List (List.map (fn x513 => cvtIDENT x513
                                                  ) ls514)), ("ty", cvtTYPE_EXPR x518)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x529) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x529))
     | cvtSTMT (InitStmt{kind=x532, ns=x533, prototype=b534, static=b535, temps=x536, 
          inits=ls538}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x532), ("ns", cvtEXPR x533), ("prototype", PrettyRep.Bool b534), 
          ("static", PrettyRep.Bool b535), ("temps", cvtBINDINGS x536), ("inits", 
          PrettyRep.List (List.map (fn x537 => cvtINIT_STEP x537
                                   ) ls538))]))
     | cvtSTMT (ClassBlock{ns=x557, ident=x558, name=opt560, block=x564}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", cvtEXPR x557), 
          ("ident", cvtIDENT x558), ("name", 
       (case opt560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x559 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x559))
       )), ("block", cvtBLOCK x564)]))
     | cvtSTMT (PackageBlock{name=x576, block=x577}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x576), ("block", cvtBLOCK x577)]))
     | cvtSTMT (ForEachStmt x585) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x585))
     | cvtSTMT (ForInStmt x588) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x588))
     | cvtSTMT (ThrowStmt x591) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x591))
     | cvtSTMT (ReturnStmt x594) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x594))
     | cvtSTMT (BreakStmt opt598) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x597 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x597))
       ))
     | cvtSTMT (ContinueStmt opt605) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x604 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x604))
       ))
     | cvtSTMT (BlockStmt x611) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x611))
     | cvtSTMT (LabeledStmt(x614, x615)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x614, 
          cvtSTMT x615]))
     | cvtSTMT (LetStmt x619) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x619))
     | cvtSTMT (WhileStmt x622) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x622))
     | cvtSTMT (DoWhileStmt x625) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x625))
     | cvtSTMT (ForStmt x628) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x628))
     | cvtSTMT (IfStmt{cnd=x631, thn=x632, els=x633}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x631), ("thn", cvtSTMT x632), 
          ("els", cvtSTMT x633)]))
     | cvtSTMT (WithStmt{obj=x643, ty=x644, body=x645}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x643), ("ty", cvtTYPE_EXPR x644), 
          ("body", cvtSTMT x645)]))
     | cvtSTMT (TryStmt{block=x655, catches=ls677, finally=opt682}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x655), ("catches", PrettyRep.List (List.map (fn {bindings=x656, 
                                                                                                     ty=opt658, 
                                                                                                     fixtures=opt663, 
                                                                                                     block=x667} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x656), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt658 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x657 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x657))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt663 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x662 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x662))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x667)]
                                                                                              ) ls677)), 
          ("finally", 
       (case opt682 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x681 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x681))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x695, cases=ls697}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x695), ("cases", PrettyRep.List (List.map (fn x696 => 
                                                                                                 cvtCASE x696
                                                                                          ) ls697))]))
     | cvtSTMT (SwitchTypeStmt{cond=x708, ty=x709, cases=ls711}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x708), ("ty", cvtTYPE_EXPR x709), 
          ("cases", PrettyRep.List (List.map (fn x710 => cvtTYPE_CASE x710
                                             ) ls711))]))
     | cvtSTMT (Dxns{expr=x724}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x724)]))
   and cvtEXPR (TrinaryExpr(x730, x731, x732, x733)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x730, cvtEXPR x731, cvtEXPR x732, 
          cvtEXPR x733]))
     | cvtEXPR (BinaryExpr(x737, x738, x739)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x737, cvtEXPR x738, cvtEXPR x739]))
     | cvtEXPR (BinaryTypeExpr(x743, x744, x745)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x743, cvtEXPR x744, cvtTYPE_EXPR x745]))
     | cvtEXPR (UnaryExpr(x749, x750)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x749, 
          cvtEXPR x750]))
     | cvtEXPR (TypeExpr x754) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x754))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt759) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt759 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x758 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x758))
       ))
     | cvtEXPR (SuperExpr opt766) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt766 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x765 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x765))
       ))
     | cvtEXPR (LiteralExpr x772) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x772))
     | cvtEXPR (CallExpr{func=x775, actuals=ls777}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x775), ("actuals", PrettyRep.List (List.map (fn x776 => 
                                                                                                   cvtEXPR x776
                                                                                            ) ls777))]))
     | cvtEXPR (ApplyTypeExpr{expr=x788, actuals=ls790}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x788), ("actuals", PrettyRep.List (List.map (fn x789 => 
                                                                                                   cvtTYPE_EXPR x789
                                                                                            ) ls790))]))
     | cvtEXPR (LetExpr{defs=x801, body=x802, head=opt804}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x801), ("body", cvtEXPR x802), 
          ("head", 
       (case opt804 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x803 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x803))
       ))]))
     | cvtEXPR (NewExpr{obj=x817, actuals=ls819}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x817), ("actuals", PrettyRep.List (List.map (fn x818 => 
                                                                                                  cvtEXPR x818
                                                                                           ) ls819))]))
     | cvtEXPR (ObjectRef{base=x830, ident=x831}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x830), ("ident", cvtIDENT_EXPR x831)]))
     | cvtEXPR (LexicalRef{ident=x839}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x839)]))
     | cvtEXPR (SetExpr(x845, x846, x847)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x845, 
          cvtEXPR x846, cvtEXPR x847]))
     | cvtEXPR (ListExpr ls852) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x851 => 
                                                                                                    cvtEXPR x851
                                                                                             ) ls852)))
     | cvtEXPR (InitExpr(x858, x859, x860)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x858, 
          cvtHEAD x859, cvtINITS x860]))
     | cvtEXPR (SliceExpr(x864, x865, x866)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x864, cvtEXPR x865, cvtEXPR x866]))
     | cvtEXPR (DefTemp(n870, x871)) = PrettyRep.Ctor ("DefTemp", SOME (PrettyRep.Tuple [PrettyRep.Int n870, 
          cvtEXPR x871]))
     | cvtEXPR (GetTemp n875) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n875))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n881) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n881))
     | cvtFIXTURE_NAME (PropName x884) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x884))
   and cvtIDENT_EXPR (Identifier{ident=x887, openNamespaces=ls893}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x887), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls889 => PrettyRep.List (List.map (fn x888 => 
                                                                                cvtNAMESPACE x888
                                                                         ) ls889)
                                   ) ls893))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x904, expr=x905}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x904), ("expr", cvtEXPR x905)]))
     | cvtIDENT_EXPR (AttributeIdentifier x913) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x913))
     | cvtIDENT_EXPR (ExpressionIdentifier x916) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x916))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x919, ident=x920}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x919), ("ident", cvtUSTRING x920)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x928, typeParams=ls930}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x928), ("typeParams", 
          PrettyRep.List (List.map (fn x929 => cvtTYPE_EXPR x929
                                   ) ls930))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s943) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s943))
     | cvtLITERAL (LiteralContextualDecimalInteger s946) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s946))
     | cvtLITERAL (LiteralContextualHexInteger s949) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s949))
     | cvtLITERAL (LiteralDouble r952) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r952))
     | cvtLITERAL (LiteralDecimal d955) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d955))
     | cvtLITERAL (LiteralInt i958) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i958))
     | cvtLITERAL (LiteralUInt u961) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u961))
     | cvtLITERAL (LiteralBoolean b964) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b964))
     | cvtLITERAL (LiteralString x967) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x967))
     | cvtLITERAL (LiteralArray{exprs=ls971, ty=opt976}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x970 => 
                                                                         cvtEXPR x970
                                                                  ) ls971)), 
          ("ty", 
       (case opt976 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x975 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x975))
       ))]))
     | cvtLITERAL (LiteralXML ls988) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x987 => 
                                                                                                           cvtEXPR x987
                                                                                                    ) ls988)))
     | cvtLITERAL (LiteralNamespace x994) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x994))
     | cvtLITERAL (LiteralObject{expr=ls998, ty=opt1003}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x997 => 
                                                                        cvtFIELD x997
                                                                 ) ls998)), 
          ("ty", 
       (case opt1003 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1002 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1002))
       ))]))
     | cvtLITERAL (LiteralFunction x1014) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1014))
     | cvtLITERAL (LiteralRegExp{str=x1017}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1017)]))
   and cvtBLOCK (Block x1023) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1023))
   and cvtFIXTURE (NamespaceFixture x1026) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1026))
     | cvtFIXTURE (ClassFixture x1029) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1029))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1033) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1033))
     | cvtFIXTURE (MethodFixture{func=x1036, ty=x1037, readOnly=b1038, override=b1039, 
          final=b1040}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1036), ("ty", cvtTYPE_EXPR x1037), ("readOnly", PrettyRep.Bool b1038), 
          ("override", PrettyRep.Bool b1039), ("final", PrettyRep.Bool b1040)]))
     | cvtFIXTURE (ValFixture{ty=x1054, readOnly=b1055}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1054), ("readOnly", PrettyRep.Bool b1055)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1063, getter=opt1065, setter=opt1070}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1063), ("getter", 
       (case opt1065 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1064 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1064))
       )), ("setter", 
       (case opt1070 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1069 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1069))
       ))]))
   and cvtBINDINGS (ls1084, ls1089) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1083 => 
                                                                                       cvtBINDING x1083
                                                                                ) ls1084), 
          PrettyRep.List (List.map (fn x1088 => cvtINIT_STEP x1088
                                   ) ls1089)]
   and cvtFIXTURES ls1097 = PrettyRep.List (List.map (fn (x1094, x1095) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1094, 
                                                            cvtFIXTURE x1095]
                                                     ) ls1097)
   and cvtINITS ls1104 = PrettyRep.List (List.map (fn (x1101, x1102) => PrettyRep.Tuple [cvtFIXTURE_NAME x1101, 
                                                         cvtEXPR x1102]
                                                  ) ls1104)
   and cvtHEAD (x1108, x1109) = PrettyRep.Tuple [cvtFIXTURES x1108, cvtINITS x1109]
   and cvtFIELD {kind=x1111, name=x1112, init=x1113} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1111), ("name", cvtIDENT_EXPR x1112), ("init", cvtEXPR x1113)]
   and cvtFIELD_TYPE {name=x1121, ty=x1122} = PrettyRep.Rec [("name", cvtIDENT x1121), 
          ("ty", cvtTYPE_EXPR x1122)]
   and cvtTYPED_IDENT {name=x1128, ty=opt1130} = PrettyRep.Rec [("name", cvtIDENT x1128), 
          ("ty", 
       (case opt1130 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1129 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1129))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1140, params=ls1145, result=x1149, thisType=opt1151, 
          hasRest=b1155, minArgs=n1156} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1139 => 
                                                                                                        cvtIDENT x1139
                                                                                                 ) ls1140)), 
          ("params", PrettyRep.List (List.map (fn x1144 => cvtTYPE_EXPR x1144
                                              ) ls1145)), ("result", cvtTYPE_EXPR x1149), 
          ("thisType", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1150))
       )), ("hasRest", PrettyRep.Bool b1155), ("minArgs", PrettyRep.Int n1156)]
   and cvtFUNC_DEFN {kind=x1170, ns=x1171, final=b1172, override=b1173, prototype=b1174, 
          static=b1175, func=x1176} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1170), 
          ("ns", cvtEXPR x1171), ("final", PrettyRep.Bool b1172), ("override", 
          PrettyRep.Bool b1173), ("prototype", PrettyRep.Bool b1174), ("static", 
          PrettyRep.Bool b1175), ("func", cvtFUNC x1176)]
   and cvtCTOR_DEFN {ns=x1192, ctor=x1193} = PrettyRep.Rec [("ns", cvtEXPR x1192), 
          ("ctor", cvtCTOR x1193)]
   and cvtVAR_DEFN {kind=x1199, ns=x1200, static=b1201, prototype=b1202, bindings=x1203} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1199), ("ns", cvtEXPR x1200), 
          ("static", PrettyRep.Bool b1201), ("prototype", PrettyRep.Bool b1202), 
          ("bindings", cvtBINDINGS x1203)]
   and cvtNAMESPACE_DEFN {ident=x1215, ns=x1216, init=opt1218} = PrettyRep.Rec [("ident", 
          cvtIDENT x1215), ("ns", cvtEXPR x1216), ("init", 
       (case opt1218 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1217 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1217))
       ))]
   and cvtCLASS_DEFN {ident=x1229, ns=x1230, nonnullable=b1231, dynamic=b1232, 
          final=b1233, params=ls1235, extends=opt1240, implements=ls1245, classDefns=ls1250, 
          instanceDefns=ls1255, instanceStmts=ls1260, ctorDefn=opt1265} = PrettyRep.Rec [("ident", 
          cvtIDENT x1229), ("ns", cvtEXPR x1230), ("nonnullable", PrettyRep.Bool b1231), 
          ("dynamic", PrettyRep.Bool b1232), ("final", PrettyRep.Bool b1233), 
          ("params", PrettyRep.List (List.map (fn x1234 => cvtIDENT x1234
                                              ) ls1235)), ("extends", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1239))
       )), ("implements", PrettyRep.List (List.map (fn x1244 => cvtIDENT_EXPR x1244
                                                   ) ls1245)), ("classDefns", 
          PrettyRep.List (List.map (fn x1249 => cvtDEFN x1249
                                   ) ls1250)), ("instanceDefns", PrettyRep.List (List.map (fn x1254 => 
                                                                                                 cvtDEFN x1254
                                                                                          ) ls1255)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1259 => cvtSTMT x1259
                                                     ) ls1260)), ("ctorDefn", 
          
       (case opt1265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1264 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR_DEFN x1264))
       ))]
   and cvtINTERFACE_DEFN {ident=x1294, ns=x1295, nonnullable=b1296, params=ls1298, 
          extends=ls1303, block=x1307} = PrettyRep.Rec [("ident", cvtIDENT x1294), 
          ("ns", cvtEXPR x1295), ("nonnullable", PrettyRep.Bool b1296), ("params", 
          PrettyRep.List (List.map (fn x1297 => cvtIDENT x1297
                                   ) ls1298)), ("extends", PrettyRep.List (List.map (fn x1302 => 
                                                                                           cvtIDENT_EXPR x1302
                                                                                    ) ls1303)), 
          ("block", cvtBLOCK x1307)]
   and cvtTYPE_DEFN {ident=x1321, ns=x1322, init=x1323} = PrettyRep.Rec [("ident", 
          cvtIDENT x1321), ("ns", cvtEXPR x1322), ("init", cvtTYPE_EXPR x1323)]
   and cvtFOR_ENUM_STMT {defn=opt1332, obj=x1336, fixtures=opt1338, inits=opt1343, 
          labels=ls1348, body=x1352} = PrettyRep.Rec [("defn", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1331))
       )), ("obj", cvtEXPR x1336), ("fixtures", 
       (case opt1338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1337 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1337))
       )), ("inits", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1342))
       )), ("labels", PrettyRep.List (List.map (fn x1347 => cvtIDENT x1347
                                               ) ls1348)), ("body", cvtSTMT x1352)]
   and cvtFOR_STMT {fixtures=opt1367, defn=opt1372, init=x1376, cond=x1377, 
          update=x1378, labels=ls1380, body=x1384} = PrettyRep.Rec [("fixtures", 
          
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1366))
       )), ("defn", 
       (case opt1372 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1371 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1371))
       )), ("init", cvtSTMT x1376), ("cond", cvtEXPR x1377), ("update", cvtEXPR x1378), 
          ("labels", PrettyRep.List (List.map (fn x1379 => cvtIDENT x1379
                                              ) ls1380)), ("body", cvtSTMT x1384)]
   and cvtWHILE_STMT {cond=x1400, fixtures=opt1402, body=x1406, labels=ls1408} = 
          PrettyRep.Rec [("cond", cvtEXPR x1400), ("fixtures", 
       (case opt1402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1401 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1401))
       )), ("body", cvtSTMT x1406), ("labels", PrettyRep.List (List.map (fn x1407 => 
                                                                               cvtIDENT x1407
                                                                        ) ls1408))]
   and cvtDIRECTIVES {pragmas=ls1422, defns=ls1427, head=opt1432, body=ls1437} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1421 => 
                                                                     cvtPRAGMA x1421
                                                              ) ls1422)), ("defns", 
          PrettyRep.List (List.map (fn x1426 => cvtDEFN x1426
                                   ) ls1427)), ("head", 
       (case opt1432 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1431 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1431))
       )), ("body", PrettyRep.List (List.map (fn x1436 => cvtSTMT x1436
                                             ) ls1437))]
   and cvtCASE {label=opt1451, inits=opt1456, body=x1460} = PrettyRep.Rec [("label", 
          
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1450))
       )), ("inits", 
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1455 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1455))
       )), ("body", cvtBLOCK x1460)]
   and cvtTYPE_CASE {ty=opt1469, bindings=x1473, inits=opt1475, body=x1479} = 
          PrettyRep.Rec [("ty", 
       (case opt1469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1468 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1468))
       )), ("bindings", cvtBINDINGS x1473), ("inits", 
       (case opt1475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1474 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1474))
       )), ("body", cvtBLOCK x1479)]
   and cvtFUNC_NAME {kind=x1489, ident=x1490} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1489), 
          ("ident", cvtIDENT x1490)]
   and cvtPACKAGE {name=x1496, block=x1497} = PrettyRep.Rec [("name", cvtUSTRING x1496), 
          ("block", cvtBLOCK x1497)]
   and cvtPROGRAM {packages=ls1504, fixtures=opt1509, block=x1513} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1503 => cvtPACKAGE x1503
                                   ) ls1504)), ("fixtures", 
       (case opt1509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1508 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1508))
       )), ("block", cvtBLOCK x1513)]
end

