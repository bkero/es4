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
     | cvtFIXTURE (ValFixture{ty=x1015, readOnly=b1016, isOverride=b1017, isFinal=b1018, 
          init=opt1020}) = PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1015), ("readOnly", PrettyRep.Bool b1016), ("isOverride", 
          PrettyRep.Bool b1017), ("isFinal", PrettyRep.Bool b1018), ("init", 
          
       (case opt1020 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1019 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1019))
       ))]))
     | cvtFIXTURE (VirtualValFixture{ty=x1037, getter=opt1039, setter=opt1044}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1037), ("getter", 
       (case opt1039 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1038 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1038))
       )), ("setter", 
       (case opt1044 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1043 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1043))
       ))]))
   and cvtFIELD {kind=x1057, name=x1058, init=x1059} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1057), ("name", cvtIDENT_EXPR x1058), ("init", cvtEXPR x1059)]
   and cvtFIELD_PATTERN {name=x1067, ptrn=x1068} = PrettyRep.Rec [("name", 
          cvtIDENT_EXPR x1067), ("ptrn", cvtPATTERN x1068)]
   and cvtFIELD_TYPE {name=x1074, ty=x1075} = PrettyRep.Rec [("name", cvtIDENT x1074), 
          ("ty", cvtTYPE_EXPR x1075)]
   and cvtTYPED_IDENT {name=x1081, ty=opt1083} = PrettyRep.Rec [("name", cvtIDENT x1081), 
          ("ty", 
       (case opt1083 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1082 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1082))
       ))]
   and cvtATTRIBUTES {ns=x1092, override=b1093, static=b1094, final=b1095, 
          dynamic=b1096, prototype=b1097, native=b1098, rest=b1099} = PrettyRep.Rec [("ns", 
          cvtEXPR x1092), ("override", PrettyRep.Bool b1093), ("static", PrettyRep.Bool b1094), 
          ("final", PrettyRep.Bool b1095), ("dynamic", PrettyRep.Bool b1096), 
          ("prototype", PrettyRep.Bool b1097), ("native", PrettyRep.Bool b1098), 
          ("rest", PrettyRep.Bool b1099)]
   and cvtFUNC_DEFN {kind=x1117, ns=x1118, final=b1119, native=b1120, override=b1121, 
          prototype=b1122, static=b1123, func=x1124} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1117), ("ns", cvtEXPR x1118), ("final", PrettyRep.Bool b1119), 
          ("native", PrettyRep.Bool b1120), ("override", PrettyRep.Bool b1121), 
          ("prototype", PrettyRep.Bool b1122), ("static", PrettyRep.Bool b1123), 
          ("func", cvtFUNC x1124)]
   and cvtVAR_DEFN {kind=x1142, ns=x1143, static=b1144, prototype=b1145, bindings=ls1147} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1142), ("ns", cvtEXPR x1143), 
          ("static", PrettyRep.Bool b1144), ("prototype", PrettyRep.Bool b1145), 
          ("bindings", PrettyRep.List (List.map (fn x1146 => cvtVAR_BINDING x1146
                                                ) ls1147))]
   and cvtFIXTURES ls1165 = PrettyRep.List (List.map (fn (x1162, x1163) => 
                                                            PrettyRep.Tuple [cvtNAME x1162, 
                                                            cvtFIXTURE x1163]
                                                     ) ls1165)
   and cvtNAMESPACE_DEFN {ident=x1169, ns=x1170, init=opt1172} = PrettyRep.Rec [("ident", 
          cvtIDENT x1169), ("ns", cvtEXPR x1170), ("init", 
       (case opt1172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1171 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1171))
       ))]
   and cvtCLASS_DEFN {ident=x1183, ns=x1184, nonnullable=b1185, dynamic=b1186, 
          final=b1187, params=ls1189, extends=opt1194, implements=ls1199, body=x1203} = 
          PrettyRep.Rec [("ident", cvtIDENT x1183), ("ns", cvtEXPR x1184), 
          ("nonnullable", PrettyRep.Bool b1185), ("dynamic", PrettyRep.Bool b1186), 
          ("final", PrettyRep.Bool b1187), ("params", PrettyRep.List (List.map (fn x1188 => 
                                                                                      cvtIDENT x1188
                                                                               ) ls1189)), 
          ("extends", 
       (case opt1194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1193 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1193))
       )), ("implements", PrettyRep.List (List.map (fn x1198 => cvtIDENT_EXPR x1198
                                                   ) ls1199)), ("body", cvtBLOCK x1203)]
   and cvtINTERFACE_DEFN {ident=x1223, ns=x1224, nonnullable=b1225, params=ls1227, 
          extends=ls1232, body=x1236} = PrettyRep.Rec [("ident", cvtIDENT x1223), 
          ("ns", cvtEXPR x1224), ("nonnullable", PrettyRep.Bool b1225), ("params", 
          PrettyRep.List (List.map (fn x1226 => cvtIDENT x1226
                                   ) ls1227)), ("extends", PrettyRep.List (List.map (fn x1231 => 
                                                                                           cvtIDENT_EXPR x1231
                                                                                    ) ls1232)), 
          ("body", cvtBLOCK x1236)]
   and cvtTYPE_DEFN {ident=x1250, ns=x1251, init=x1252} = PrettyRep.Rec [("ident", 
          cvtIDENT x1250), ("ns", cvtEXPR x1251), ("init", cvtTYPE_EXPR x1252)]
   and cvtFOR_ENUM_STMT {ptrn=opt1261, obj=ls1266, defns=ls1271, fixtures=opt1276, 
          contLabel=opt1281, body=x1285} = PrettyRep.Rec [("ptrn", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1260 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1260))
       )), ("obj", PrettyRep.List (List.map (fn x1265 => cvtEXPR x1265
                                            ) ls1266)), ("defns", PrettyRep.List (List.map (fn x1270 => 
                                                                                                  cvtVAR_BINDING x1270
                                                                                           ) ls1271)), 
          ("fixtures", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1275))
       )), ("contLabel", 
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1280))
       )), ("body", cvtSTMT x1285)]
   and cvtWHILE_STMT {cond=x1299, body=x1300, contLabel=opt1302} = PrettyRep.Rec [("cond", 
          cvtEXPR x1299), ("body", cvtSTMT x1300), ("contLabel", 
       (case opt1302 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1301 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1301))
       ))]
   and cvtDIRECTIVES {pragmas=ls1314, defns=ls1319, stmts=ls1324, fixtures=opt1329, 
          inits=opt1338} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1313 => 
                                                                                      cvtPRAGMA x1313
                                                                               ) ls1314)), 
          ("defns", PrettyRep.List (List.map (fn x1318 => cvtDEFN x1318
                                             ) ls1319)), ("stmts", PrettyRep.List (List.map (fn x1323 => 
                                                                                                   cvtSTMT x1323
                                                                                            ) ls1324)), 
          ("fixtures", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1328))
       )), ("inits", 
       (case opt1338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1334 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1333 => 
                                                                                      cvtSTMT x1333
                                                                               ) ls1334)))
       ))]
   and cvtBINDINGS {b=ls1354, i=ls1359} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1353 => 
                                                                                               cvtVAR_BINDING x1353
                                                                                        ) ls1354)), 
          ("i", PrettyRep.List (List.map (fn x1358 => cvtEXPR x1358
                                         ) ls1359))]
   and cvtCASE {label=opt1373, body=x1377} = PrettyRep.Rec [("label", 
       (case opt1373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1369 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1368 => 
                                                                                      cvtEXPR x1368
                                                                               ) ls1369)))
       )), ("body", cvtBLOCK x1377)]
   and cvtTYPE_CASE {ptrn=opt1384, body=x1388} = PrettyRep.Rec [("ptrn", 
       (case opt1384 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1383 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1383))
       )), ("body", cvtBLOCK x1388)]
   and cvtFUNC_NAME {kind=x1394, ident=x1395} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1394), 
          ("ident", cvtIDENT x1395)]
   and cvtPACKAGE {name=x1401, body=x1402} = PrettyRep.Rec [("name", cvtUSTRING x1401), 
          ("body", cvtBLOCK x1402)]
   and cvtPROGRAM {packages=ls1409, fixtures=opt1414, body=x1418} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1408 => cvtPACKAGE x1408
                                   ) ls1409)), ("fixtures", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1413))
       )), ("body", cvtBLOCK x1418)]
end

