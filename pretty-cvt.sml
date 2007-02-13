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
   and cvtNULOP (This) = PrettyRep.Ctor ("This", NONE)
     | cvtNULOP (Empty) = PrettyRep.Ctor ("Empty", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x237) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x237))
     | cvtPRAGMA (UseDefaultNamespace x240) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x240))
     | cvtPRAGMA (UseNumber x243) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x243))
     | cvtPRAGMA (UseRounding x246) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x246))
     | cvtPRAGMA (UsePrecision x249) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x249))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x254, name=x255, alias=opt257}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x254), ("name", cvtIDENT x255), 
          ("alias", 
       (case opt257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x256))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x277, fsig=x278, body=x279, fixtures=opt281, inits=ls286}) = 
          PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x277), 
          ("fsig", cvtFUNC_SIG x278), ("body", cvtBLOCK x279), ("fixtures", 
          
       (case opt281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x280 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x280))
       )), ("inits", PrettyRep.List (List.map (fn x285 => cvtSTMT x285
                                              ) ls286))]))
   and cvtDEFN (ClassDefn x303) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x303))
     | cvtDEFN (VariableDefn x306) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x306))
     | cvtDEFN (FunctionDefn x309) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x309))
     | cvtDEFN (InterfaceDefn x312) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x312))
     | cvtDEFN (NamespaceDefn x315) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x315))
     | cvtDEFN (TypeDefn x318) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x318))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls322, params=ls327, inits=ls332, 
          returnType=x336, thisType=opt338, hasRest=b342}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x321 => 
                                                                              cvtIDENT x321
                                                                       ) ls322)), 
          ("params", PrettyRep.List (List.map (fn x326 => cvtVAR_BINDING x326
                                              ) ls327)), ("inits", PrettyRep.List (List.map (fn x331 => 
                                                                                                   cvtSTMT x331
                                                                                            ) ls332)), 
          ("returnType", cvtTYPE_EXPR x336), ("thisType", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x337))
       )), ("hasRest", PrettyRep.Bool b342)]))
   and cvtVAR_BINDING (Binding{pattern=x358, ty=opt360, init=opt365}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x358), ("ty", 
       (case opt360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x359 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x359))
       )), ("init", 
       (case opt365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x364 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x364))
       ))]))
   and cvtTYPE_EXPR (SpecialType x378) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x378))
     | cvtTYPE_EXPR (UnionType ls382) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x381 => 
                                                                                                           cvtTYPE_EXPR x381
                                                                                                    ) ls382)))
     | cvtTYPE_EXPR (ArrayType ls389) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x388 => 
                                                                                                           cvtTYPE_EXPR x388
                                                                                                    ) ls389)))
     | cvtTYPE_EXPR (NominalType{ident=x395}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x395)]))
     | cvtTYPE_EXPR (FunctionType x401) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x401))
     | cvtTYPE_EXPR (ObjectType ls405) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x404 => 
                                                                                                             cvtFIELD_TYPE x404
                                                                                                      ) ls405)))
     | cvtTYPE_EXPR (AppType{base=x411, args=ls413}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x411), ("args", PrettyRep.List (List.map (fn x412 => 
                                                                                                     cvtTYPE_EXPR x412
                                                                                              ) ls413))]))
     | cvtTYPE_EXPR (NullableType{expr=x424, nullable=b425}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x424), ("nullable", PrettyRep.Bool b425)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls435) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x434 => 
                                                                                                    cvtEXPR x434
                                                                                             ) ls435)))
     | cvtSTMT (InitStmt{kind=x441, ns=x442, prototype=b443, static=b444, inits=ls446}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x441), 
          ("ns", cvtEXPR x442), ("prototype", PrettyRep.Bool b443), ("static", 
          PrettyRep.Bool b444), ("inits", PrettyRep.List (List.map (fn x445 => 
                                                                          cvtEXPR x445
                                                                   ) ls446))]))
     | cvtSTMT (ForEachStmt x463) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x463))
     | cvtSTMT (ForInStmt x466) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x466))
     | cvtSTMT (ThrowStmt ls470) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x469 => 
                                                                                                      cvtEXPR x469
                                                                                               ) ls470)))
     | cvtSTMT (ReturnStmt ls477) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x476 => 
                                                                                                        cvtEXPR x476
                                                                                                 ) ls477)))
     | cvtSTMT (BreakStmt opt484) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt484 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x483 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x483))
       ))
     | cvtSTMT (ContinueStmt opt491) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x490 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x490))
       ))
     | cvtSTMT (BlockStmt x497) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x497))
     | cvtSTMT (LabeledStmt(x500, x501)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x500, 
          cvtSTMT x501]))
     | cvtSTMT (LetStmt(ls506, x510)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x505 => 
                                                                                                                          cvtVAR_BINDING x505
                                                                                                                   ) ls506), 
          cvtSTMT x510]))
     | cvtSTMT (SuperStmt ls515) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x514 => 
                                                                                                      cvtEXPR x514
                                                                                               ) ls515)))
     | cvtSTMT (WhileStmt x521) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x521))
     | cvtSTMT (DoWhileStmt x524) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x524))
     | cvtSTMT (ForStmt{defns=ls528, fixtures=opt533, init=ls538, cond=ls543, 
          update=ls548, contLabel=opt553, body=x557}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x527 => 
                                                                         cvtVAR_BINDING x527
                                                                  ) ls528)), 
          ("fixtures", 
       (case opt533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x532 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x532))
       )), ("init", PrettyRep.List (List.map (fn x537 => cvtEXPR x537
                                             ) ls538)), ("cond", PrettyRep.List (List.map (fn x542 => 
                                                                                                 cvtEXPR x542
                                                                                          ) ls543)), 
          ("update", PrettyRep.List (List.map (fn x547 => cvtEXPR x547
                                              ) ls548)), ("contLabel", 
       (case opt553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x552 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x552))
       )), ("body", cvtSTMT x557)]))
     | cvtSTMT (IfStmt{cnd=x575, thn=x576, els=x577}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x575), ("thn", cvtSTMT x576), 
          ("els", cvtSTMT x577)]))
     | cvtSTMT (WithStmt{obj=ls588, ty=x592, body=x593}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x587 => 
                                                                       cvtEXPR x587
                                                                ) ls588)), 
          ("ty", cvtTYPE_EXPR x592), ("body", cvtSTMT x593)]))
     | cvtSTMT (TryStmt{body=x603, catches=ls618, finally=opt623}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x603), ("catches", PrettyRep.List (List.map (fn {bind=x604, 
                                                                                                    fixtures=opt606, 
                                                                                                    body=x610} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x604), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt606 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x605 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x605))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x610)]
                                                                                             ) ls618)), 
          ("finally", 
       (case opt623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x622 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x622))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls637, cases=ls642}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x636 => 
                                                                        cvtEXPR x636
                                                                 ) ls637)), 
          ("cases", PrettyRep.List (List.map (fn x641 => cvtCASE x641
                                             ) ls642))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls654, ty=x658, cases=ls660}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x653 => 
                                                                        cvtEXPR x653
                                                                 ) ls654)), 
          ("ty", cvtTYPE_EXPR x658), ("cases", PrettyRep.List (List.map (fn x659 => 
                                                                               cvtTYPE_CASE x659
                                                                        ) ls660))]))
     | cvtSTMT (Dxns{expr=x673}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x673)]))
   and cvtEXPR (TrinaryExpr(x679, x680, x681, x682)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x679, cvtEXPR x680, cvtEXPR x681, 
          cvtEXPR x682]))
     | cvtEXPR (BinaryExpr(x686, x687, x688)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x686, cvtEXPR x687, cvtEXPR x688]))
     | cvtEXPR (BinaryTypeExpr(x692, x693, x694)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x692, cvtEXPR x693, cvtTYPE_EXPR x694]))
     | cvtEXPR (UnaryExpr(x698, x699)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x698, 
          cvtEXPR x699]))
     | cvtEXPR (TypeExpr x703) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x703))
     | cvtEXPR (NullaryExpr x706) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x706))
     | cvtEXPR (YieldExpr opt714) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt714 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls710 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x709 => 
                                                                                     cvtEXPR x709
                                                                              ) ls710)))
       ))
     | cvtEXPR (SuperExpr opt721) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt721 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x720 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x720))
       ))
     | cvtEXPR (LiteralExpr x727) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x727))
     | cvtEXPR (CallExpr{func=x730, actuals=ls732}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x730), ("actuals", PrettyRep.List (List.map (fn x731 => 
                                                                                                   cvtEXPR x731
                                                                                            ) ls732))]))
     | cvtEXPR (ApplyTypeExpr{expr=x743, actuals=ls745}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x743), ("actuals", PrettyRep.List (List.map (fn x744 => 
                                                                                                   cvtTYPE_EXPR x744
                                                                                            ) ls745))]))
     | cvtEXPR (LetExpr{defs=ls757, body=ls762, fixtures=opt767}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x756 => 
                                                                        cvtVAR_BINDING x756
                                                                 ) ls757)), 
          ("body", PrettyRep.List (List.map (fn x761 => cvtEXPR x761
                                            ) ls762)), ("fixtures", 
       (case opt767 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x766 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x766))
       ))]))
     | cvtEXPR (NewExpr{obj=x780, actuals=ls782}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x780), ("actuals", PrettyRep.List (List.map (fn x781 => 
                                                                                                  cvtEXPR x781
                                                                                           ) ls782))]))
     | cvtEXPR (FunExpr x793) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x793))
     | cvtEXPR (ObjectRef{base=x796, ident=x797}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x796), ("ident", cvtIDENT_EXPR x797)]))
     | cvtEXPR (LexicalRef{ident=x805}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x805)]))
     | cvtEXPR (SetExpr(x811, x812, x813)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x811, 
          cvtPATTERN x812, cvtEXPR x813]))
     | cvtEXPR (AllocTemp(x817, x818)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [cvtIDENT_EXPR x817, 
          cvtEXPR x818]))
     | cvtEXPR (ListExpr ls823) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x822 => 
                                                                                                    cvtEXPR x822
                                                                                             ) ls823)))
     | cvtEXPR (SliceExpr(ls830, ls835, ls840)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x829 => cvtEXPR x829
                                                          ) ls830), PrettyRep.List (List.map (fn x834 => 
                                                                                                    cvtEXPR x834
                                                                                             ) ls835), 
          PrettyRep.List (List.map (fn x839 => cvtEXPR x839
                                   ) ls840)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x847, ident=x848}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x847), ("ident", cvtUSTRING x848)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x856, expr=x857}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x856), ("expr", cvtEXPR x857)]))
     | cvtIDENT_EXPR (AttributeIdentifier x865) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x865))
     | cvtIDENT_EXPR (Identifier{ident=x868, openNamespaces=ls874}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x868), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls870 => PrettyRep.List (List.map (fn x869 => 
                                                                                cvtNAMESPACE x869
                                                                         ) ls870)
                                   ) ls874))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x885) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x885))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x888, typeParams=ls890}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x888), ("typeParams", 
          PrettyRep.List (List.map (fn x889 => cvtTYPE_EXPR x889
                                   ) ls890))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r903) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r903))
     | cvtLITERAL (LiteralBoolean b906) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b906))
     | cvtLITERAL (LiteralString x909) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x909))
     | cvtLITERAL (LiteralArray{exprs=ls913, ty=opt918}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x912 => 
                                                                         cvtEXPR x912
                                                                  ) ls913)), 
          ("ty", 
       (case opt918 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x917 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x917))
       ))]))
     | cvtLITERAL (LiteralXML ls930) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x929 => 
                                                                                                           cvtEXPR x929
                                                                                                    ) ls930)))
     | cvtLITERAL (LiteralNamespace x936) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x936))
     | cvtLITERAL (LiteralObject{expr=ls940, ty=opt945}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x939 => 
                                                                        cvtFIELD x939
                                                                 ) ls940)), 
          ("ty", 
       (case opt945 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x944 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x944))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x956}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x956)]))
   and cvtBLOCK (Block x962) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x962))
   and cvtPATTERN (ObjectPattern ls966) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x965 => cvtFIELD_PATTERN x965
                                         ) ls966)))
     | cvtPATTERN (ArrayPattern ls973) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x972 => 
                                                                                                               cvtPATTERN x972
                                                                                                        ) ls973)))
     | cvtPATTERN (SimplePattern x979) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x979))
     | cvtPATTERN (IdentifierPattern x982) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x982))
   and cvtFIXTURE (NamespaceFixture x985) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x985))
     | cvtFIXTURE (ClassFixture{extends=opt989, implements=ls994, classBlock=x998, 
          instanceBlock=x999}) = PrettyRep.Ctor ("ClassFixture", SOME (PrettyRep.Rec [("extends", 
          
       (case opt989 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x988 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x988))
       )), ("implements", PrettyRep.List (List.map (fn x993 => cvtNAME x993
                                                   ) ls994)), ("classBlock", 
          cvtBLOCK x998), ("instanceBlock", cvtBLOCK x999)]))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1012) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1012))
     | cvtFIXTURE (ValFixture{ty=x1015, readOnly=b1016, isOverride=b1017, init=opt1019}) = 
          PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1015), 
          ("readOnly", PrettyRep.Bool b1016), ("isOverride", PrettyRep.Bool b1017), 
          ("init", 
       (case opt1019 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1018 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1018))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1034, getter=opt1036, setter=opt1041}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1034), ("getter", 
       (case opt1036 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1035 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1035))
       )), ("setter", 
       (case opt1041 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1040 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1040))
       ))]))
   and cvtFIELD {kind=x1054, name=x1055, init=x1056} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1054), ("name", cvtIDENT_EXPR x1055), ("init", cvtEXPR x1056)]
   and cvtFIELD_PATTERN {name=x1064, ptrn=x1065} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1064), ("ptrn", cvtPATTERN x1065)]
   and cvtFIELD_TYPE {name=x1071, ty=x1072} = PrettyRep.Rec [("name", cvtIDENT x1071), 
          ("ty", cvtTYPE_EXPR x1072)]
   and cvtTYPED_IDENT {name=x1078, ty=opt1080} = PrettyRep.Rec [("name", cvtIDENT x1078), 
          ("ty", 
       (case opt1080 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1079 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1079))
       ))]
   and cvtATTRIBUTES {ns=x1089, override=b1090, static=b1091, final=b1092, 
          dynamic=b1093, prototype=b1094, native=b1095, rest=b1096} = PrettyRep.Rec [("ns", 
          cvtEXPR x1089), ("override", PrettyRep.Bool b1090), ("static", PrettyRep.Bool b1091), 
          ("final", PrettyRep.Bool b1092), ("dynamic", PrettyRep.Bool b1093), 
          ("prototype", PrettyRep.Bool b1094), ("native", PrettyRep.Bool b1095), 
          ("rest", PrettyRep.Bool b1096)]
   and cvtFUNC_DEFN {kind=x1114, ns=x1115, final=b1116, native=b1117, override=b1118, 
          prototype=b1119, static=b1120, func=x1121} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1114), ("ns", cvtEXPR x1115), ("final", PrettyRep.Bool b1116), 
          ("native", PrettyRep.Bool b1117), ("override", PrettyRep.Bool b1118), 
          ("prototype", PrettyRep.Bool b1119), ("static", PrettyRep.Bool b1120), 
          ("func", cvtFUNC x1121)]
   and cvtVAR_DEFN {kind=x1139, ns=x1140, static=b1141, prototype=b1142, bindings=ls1144} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1139), ("ns", cvtEXPR x1140), 
          ("static", PrettyRep.Bool b1141), ("prototype", PrettyRep.Bool b1142), 
          ("bindings", PrettyRep.List (List.map (fn x1143 => cvtVAR_BINDING x1143
                                                ) ls1144))]
   and cvtFIXTURES ls1162 = PrettyRep.List (List.map (fn (x1159, x1160) => 
                                                            PrettyRep.Tuple [cvtNAME x1159, 
                                                            cvtFIXTURE x1160]
                                                     ) ls1162)
   and cvtNAMESPACE_DEFN {ident=x1166, ns=x1167, init=opt1169} = PrettyRep.Rec [("ident", 
          cvtIDENT x1166), ("ns", cvtEXPR x1167), ("init", 
       (case opt1169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1168 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1168))
       ))]
   and cvtCLASS_DEFN {ident=x1180, ns=x1181, nonnullable=b1182, dynamic=b1183, 
          final=b1184, params=ls1186, extends=opt1191, implements=ls1196, body=x1200} = 
          PrettyRep.Rec [("ident", cvtIDENT x1180), ("ns", cvtEXPR x1181), 
          ("nonnullable", PrettyRep.Bool b1182), ("dynamic", PrettyRep.Bool b1183), 
          ("final", PrettyRep.Bool b1184), ("params", PrettyRep.List (List.map (fn x1185 => 
                                                                                      cvtIDENT x1185
                                                                               ) ls1186)), 
          ("extends", 
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1190))
       )), ("implements", PrettyRep.List (List.map (fn x1195 => cvtIDENT_EXPR x1195
                                                   ) ls1196)), ("body", cvtBLOCK x1200)]
   and cvtINTERFACE_DEFN {ident=x1220, ns=x1221, nonnullable=b1222, params=ls1224, 
          extends=ls1229, body=x1233} = PrettyRep.Rec [("ident", cvtIDENT x1220), 
          ("ns", cvtEXPR x1221), ("nonnullable", PrettyRep.Bool b1222), ("params", 
          PrettyRep.List (List.map (fn x1223 => cvtIDENT x1223
                                   ) ls1224)), ("extends", PrettyRep.List (List.map (fn x1228 => 
                                                                                           cvtIDENT_EXPR x1228
                                                                                    ) ls1229)), 
          ("body", cvtBLOCK x1233)]
   and cvtTYPE_DEFN {ident=x1247, ns=x1248, init=x1249} = PrettyRep.Rec [("ident", 
          cvtIDENT x1247), ("ns", cvtEXPR x1248), ("init", cvtTYPE_EXPR x1249)]
   and cvtFOR_ENUM_STMT {ptrn=opt1258, obj=ls1263, defns=ls1268, fixtures=opt1273, 
          contLabel=opt1278, body=x1282} = PrettyRep.Rec [("ptrn", 
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1257))
       )), ("obj", PrettyRep.List (List.map (fn x1262 => cvtEXPR x1262
                                            ) ls1263)), ("defns", PrettyRep.List (List.map (fn x1267 => 
                                                                                                  cvtVAR_BINDING x1267
                                                                                           ) ls1268)), 
          ("fixtures", 
       (case opt1273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1272 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1272))
       )), ("contLabel", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1277))
       )), ("body", cvtSTMT x1282)]
   and cvtWHILE_STMT {cond=x1296, body=x1297, contLabel=opt1299} = PrettyRep.Rec [("cond", 
          cvtEXPR x1296), ("body", cvtSTMT x1297), ("contLabel", 
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1298))
       ))]
   and cvtDIRECTIVES {pragmas=ls1311, defns=ls1316, stmts=ls1321, fixtures=opt1326, 
          inits=opt1335} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1310 => 
                                                                                      cvtPRAGMA x1310
                                                                               ) ls1311)), 
          ("defns", PrettyRep.List (List.map (fn x1315 => cvtDEFN x1315
                                             ) ls1316)), ("stmts", PrettyRep.List (List.map (fn x1320 => 
                                                                                                   cvtSTMT x1320
                                                                                            ) ls1321)), 
          ("fixtures", 
       (case opt1326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1325 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1325))
       )), ("inits", 
       (case opt1335 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1331 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1330 => 
                                                                                      cvtSTMT x1330
                                                                               ) ls1331)))
       ))]
   and cvtBINDINGS {b=ls1351, i=ls1356} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1350 => 
                                                                                               cvtVAR_BINDING x1350
                                                                                        ) ls1351)), 
          ("i", PrettyRep.List (List.map (fn x1355 => cvtEXPR x1355
                                         ) ls1356))]
   and cvtCASE {label=opt1370, body=x1374} = PrettyRep.Rec [("label", 
       (case opt1370 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1366 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1365 => 
                                                                                      cvtEXPR x1365
                                                                               ) ls1366)))
       )), ("body", cvtBLOCK x1374)]
   and cvtTYPE_CASE {ptrn=opt1381, body=x1385} = PrettyRep.Rec [("ptrn", 
       (case opt1381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1380 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1380))
       )), ("body", cvtBLOCK x1385)]
   and cvtFUNC_NAME {kind=x1391, ident=x1392} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1391), 
          ("ident", cvtIDENT x1392)]
   and cvtPACKAGE {name=x1398, body=x1399} = PrettyRep.Rec [("name", cvtUSTRING x1398), 
          ("body", cvtBLOCK x1399)]
   and cvtPROGRAM {packages=ls1406, fixtures=opt1411, body=x1415} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1405 => cvtPACKAGE x1405
                                   ) ls1406)), ("fixtures", 
       (case opt1411 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1410 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1410))
       )), ("body", cvtBLOCK x1415)]
end

