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
          returnType=x336, thisType=opt338, hasBoundThis=b342, hasRest=b343}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x321 => cvtIDENT x321
                                   ) ls322)), ("params", PrettyRep.List (List.map (fn x326 => 
                                                                                         cvtVAR_BINDING x326
                                                                                  ) ls327)), 
          ("inits", PrettyRep.List (List.map (fn x331 => cvtSTMT x331
                                             ) ls332)), ("returnType", cvtTYPE_EXPR x336), 
          ("thisType", 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x337))
       )), ("hasBoundThis", PrettyRep.Bool b342), ("hasRest", PrettyRep.Bool b343)]))
   and cvtVAR_BINDING (Binding{pattern=x361, ty=opt363, init=opt368}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x361), ("ty", 
       (case opt363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x362 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x362))
       )), ("init", 
       (case opt368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x367 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x367))
       ))]))
   and cvtTYPE_EXPR (SpecialType x381) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x381))
     | cvtTYPE_EXPR (UnionType ls385) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x384 => 
                                                                                                           cvtTYPE_EXPR x384
                                                                                                    ) ls385)))
     | cvtTYPE_EXPR (ArrayType ls392) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x391 => 
                                                                                                           cvtTYPE_EXPR x391
                                                                                                    ) ls392)))
     | cvtTYPE_EXPR (NominalType{ident=x398}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x398)]))
     | cvtTYPE_EXPR (FunctionType x404) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x404))
     | cvtTYPE_EXPR (ObjectType ls408) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x407 => 
                                                                                                             cvtFIELD_TYPE x407
                                                                                                      ) ls408)))
     | cvtTYPE_EXPR (AppType{base=x414, args=ls416}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x414), ("args", PrettyRep.List (List.map (fn x415 => 
                                                                                                     cvtTYPE_EXPR x415
                                                                                              ) ls416))]))
     | cvtTYPE_EXPR (NullableType{expr=x427, nullable=b428}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x427), ("nullable", PrettyRep.Bool b428)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls438) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x437 => 
                                                                                                    cvtEXPR x437
                                                                                             ) ls438)))
     | cvtSTMT (InitStmt{kind=x444, ns=x445, prototype=b446, static=b447, inits=ls449}) = 
          PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x444), 
          ("ns", cvtEXPR x445), ("prototype", PrettyRep.Bool b446), ("static", 
          PrettyRep.Bool b447), ("inits", PrettyRep.List (List.map (fn x448 => 
                                                                          cvtEXPR x448
                                                                   ) ls449))]))
     | cvtSTMT (ForEachStmt x466) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x466))
     | cvtSTMT (ForInStmt x469) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x469))
     | cvtSTMT (ThrowStmt ls473) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x472 => 
                                                                                                      cvtEXPR x472
                                                                                               ) ls473)))
     | cvtSTMT (ReturnStmt ls480) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x479 => 
                                                                                                        cvtEXPR x479
                                                                                                 ) ls480)))
     | cvtSTMT (BreakStmt opt487) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt487 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x486 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x486))
       ))
     | cvtSTMT (ContinueStmt opt494) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x493 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x493))
       ))
     | cvtSTMT (BlockStmt x500) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x500))
     | cvtSTMT (LabeledStmt(x503, x504)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x503, 
          cvtSTMT x504]))
     | cvtSTMT (LetStmt(ls509, x513)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x508 => 
                                                                                                                          cvtVAR_BINDING x508
                                                                                                                   ) ls509), 
          cvtSTMT x513]))
     | cvtSTMT (SuperStmt ls518) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x517 => 
                                                                                                      cvtEXPR x517
                                                                                               ) ls518)))
     | cvtSTMT (WhileStmt x524) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x524))
     | cvtSTMT (DoWhileStmt x527) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x527))
     | cvtSTMT (ForStmt{defns=ls531, fixtures=opt536, init=ls541, cond=ls546, 
          update=ls551, contLabel=opt556, body=x560}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x530 => 
                                                                         cvtVAR_BINDING x530
                                                                  ) ls531)), 
          ("fixtures", 
       (case opt536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x535 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x535))
       )), ("init", PrettyRep.List (List.map (fn x540 => cvtEXPR x540
                                             ) ls541)), ("cond", PrettyRep.List (List.map (fn x545 => 
                                                                                                 cvtEXPR x545
                                                                                          ) ls546)), 
          ("update", PrettyRep.List (List.map (fn x550 => cvtEXPR x550
                                              ) ls551)), ("contLabel", 
       (case opt556 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x555 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x555))
       )), ("body", cvtSTMT x560)]))
     | cvtSTMT (IfStmt{cnd=x578, thn=x579, els=x580}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x578), ("thn", cvtSTMT x579), 
          ("els", cvtSTMT x580)]))
     | cvtSTMT (WithStmt{obj=ls591, ty=x595, body=x596}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x590 => 
                                                                       cvtEXPR x590
                                                                ) ls591)), 
          ("ty", cvtTYPE_EXPR x595), ("body", cvtSTMT x596)]))
     | cvtSTMT (TryStmt{body=x606, catches=ls621, finally=opt626}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x606), ("catches", PrettyRep.List (List.map (fn {bind=x607, 
                                                                                                    fixtures=opt609, 
                                                                                                    body=x613} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x607), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt609 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x608 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x608))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x613)]
                                                                                             ) ls621)), 
          ("finally", 
       (case opt626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x625 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x625))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls640, cases=ls645}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x639 => 
                                                                        cvtEXPR x639
                                                                 ) ls640)), 
          ("cases", PrettyRep.List (List.map (fn x644 => cvtCASE x644
                                             ) ls645))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls657, ty=x661, cases=ls663}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x656 => 
                                                                        cvtEXPR x656
                                                                 ) ls657)), 
          ("ty", cvtTYPE_EXPR x661), ("cases", PrettyRep.List (List.map (fn x662 => 
                                                                               cvtTYPE_CASE x662
                                                                        ) ls663))]))
     | cvtSTMT (Dxns{expr=x676}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x676)]))
   and cvtEXPR (TrinaryExpr(x682, x683, x684, x685)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x682, cvtEXPR x683, cvtEXPR x684, 
          cvtEXPR x685]))
     | cvtEXPR (BinaryExpr(x689, x690, x691)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x689, cvtEXPR x690, cvtEXPR x691]))
     | cvtEXPR (BinaryTypeExpr(x695, x696, x697)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x695, cvtEXPR x696, cvtTYPE_EXPR x697]))
     | cvtEXPR (UnaryExpr(x701, x702)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x701, 
          cvtEXPR x702]))
     | cvtEXPR (TypeExpr x706) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x706))
     | cvtEXPR (NullaryExpr x709) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x709))
     | cvtEXPR (YieldExpr opt717) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls713 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x712 => 
                                                                                     cvtEXPR x712
                                                                              ) ls713)))
       ))
     | cvtEXPR (SuperExpr opt724) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt724 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x723 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x723))
       ))
     | cvtEXPR (LiteralExpr x730) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x730))
     | cvtEXPR (CallExpr{func=x733, actuals=ls735}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x733), ("actuals", PrettyRep.List (List.map (fn x734 => 
                                                                                                   cvtEXPR x734
                                                                                            ) ls735))]))
     | cvtEXPR (ApplyTypeExpr{expr=x746, actuals=ls748}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x746), ("actuals", PrettyRep.List (List.map (fn x747 => 
                                                                                                   cvtTYPE_EXPR x747
                                                                                            ) ls748))]))
     | cvtEXPR (LetExpr{defs=ls760, body=ls765, fixtures=opt770}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x759 => 
                                                                        cvtVAR_BINDING x759
                                                                 ) ls760)), 
          ("body", PrettyRep.List (List.map (fn x764 => cvtEXPR x764
                                            ) ls765)), ("fixtures", 
       (case opt770 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x769 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x769))
       ))]))
     | cvtEXPR (NewExpr{obj=x783, actuals=ls785}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x783), ("actuals", PrettyRep.List (List.map (fn x784 => 
                                                                                                  cvtEXPR x784
                                                                                           ) ls785))]))
     | cvtEXPR (FunExpr x796) = PrettyRep.Ctor ("FunExpr", SOME (cvtFUNC x796))
     | cvtEXPR (ObjectRef{base=x799, ident=x800}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x799), ("ident", cvtIDENT_EXPR x800)]))
     | cvtEXPR (LexicalRef{ident=x808}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x808)]))
     | cvtEXPR (SetExpr(x814, x815, x816)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x814, 
          cvtPATTERN x815, cvtEXPR x816]))
     | cvtEXPR (AllocTemp(x820, x821)) = PrettyRep.Ctor ("AllocTemp", SOME (PrettyRep.Tuple [cvtIDENT_EXPR x820, 
          cvtEXPR x821]))
     | cvtEXPR (ListExpr ls826) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x825 => 
                                                                                                    cvtEXPR x825
                                                                                             ) ls826)))
     | cvtEXPR (SliceExpr(ls833, ls838, ls843)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x832 => cvtEXPR x832
                                                          ) ls833), PrettyRep.List (List.map (fn x837 => 
                                                                                                    cvtEXPR x837
                                                                                             ) ls838), 
          PrettyRep.List (List.map (fn x842 => cvtEXPR x842
                                   ) ls843)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x850, ident=x851}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x850), ("ident", cvtUSTRING x851)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x859, expr=x860}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x859), ("expr", cvtEXPR x860)]))
     | cvtIDENT_EXPR (AttributeIdentifier x868) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x868))
     | cvtIDENT_EXPR (Identifier{ident=x871, openNamespaces=ls877}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x871), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls873 => PrettyRep.List (List.map (fn x872 => 
                                                                                cvtNAMESPACE x872
                                                                         ) ls873)
                                   ) ls877))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x888) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x888))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x891, typeParams=ls893}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x891), ("typeParams", 
          PrettyRep.List (List.map (fn x892 => cvtTYPE_EXPR x892
                                   ) ls893))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r906) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r906))
     | cvtLITERAL (LiteralBoolean b909) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b909))
     | cvtLITERAL (LiteralString x912) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x912))
     | cvtLITERAL (LiteralArray{exprs=ls916, ty=opt921}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x915 => 
                                                                         cvtEXPR x915
                                                                  ) ls916)), 
          ("ty", 
       (case opt921 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x920 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x920))
       ))]))
     | cvtLITERAL (LiteralXML ls933) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x932 => 
                                                                                                           cvtEXPR x932
                                                                                                    ) ls933)))
     | cvtLITERAL (LiteralNamespace x939) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x939))
     | cvtLITERAL (LiteralObject{expr=ls943, ty=opt948}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x942 => 
                                                                        cvtFIELD x942
                                                                 ) ls943)), 
          ("ty", 
       (case opt948 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x947 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x947))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x959}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x959)]))
   and cvtBLOCK (Block x965) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x965))
   and cvtPATTERN (ObjectPattern ls969) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x968 => cvtFIELD_PATTERN x968
                                         ) ls969)))
     | cvtPATTERN (ArrayPattern ls976) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x975 => 
                                                                                                               cvtPATTERN x975
                                                                                                        ) ls976)))
     | cvtPATTERN (SimplePattern x982) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x982))
     | cvtPATTERN (IdentifierPattern x985) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT x985))
   and cvtFIXTURE (NamespaceFixture x988) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x988))
     | cvtFIXTURE (ClassFixture{extends=opt992, implements=ls997, classBlock=x1001, 
          instanceBlock=x1002}) = PrettyRep.Ctor ("ClassFixture", SOME (PrettyRep.Rec [("extends", 
          
       (case opt992 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x991 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x991))
       )), ("implements", PrettyRep.List (List.map (fn x996 => cvtNAME x996
                                                   ) ls997)), ("classBlock", 
          cvtBLOCK x1001), ("instanceBlock", cvtBLOCK x1002)]))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1015) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1015))
     | cvtFIXTURE (ValFixture{ty=x1018, readOnly=b1019, isOverride=b1020, init=opt1022}) = 
          PrettyRep.Ctor ("ValFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1018), 
          ("readOnly", PrettyRep.Bool b1019), ("isOverride", PrettyRep.Bool b1020), 
          ("init", 
       (case opt1022 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1021 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1021))
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

