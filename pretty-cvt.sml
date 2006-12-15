structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
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
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus) = PrettyRep.Ctor ("Plus", NONE)
     | cvtBINOP (Minus) = PrettyRep.Ctor ("Minus", NONE)
     | cvtBINOP (Times) = PrettyRep.Ctor ("Times", NONE)
     | cvtBINOP (Divide) = PrettyRep.Ctor ("Divide", NONE)
     | cvtBINOP (Remainder) = PrettyRep.Ctor ("Remainder", NONE)
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
     | cvtBINOP (Equals) = PrettyRep.Ctor ("Equals", NONE)
     | cvtBINOP (NotEquals) = PrettyRep.Ctor ("NotEquals", NONE)
     | cvtBINOP (StrictEquals) = PrettyRep.Ctor ("StrictEquals", NONE)
     | cvtBINOP (StrictNotEquals) = PrettyRep.Ctor ("StrictNotEquals", NONE)
     | cvtBINOP (Less) = PrettyRep.Ctor ("Less", NONE)
     | cvtBINOP (LessOrEqual) = PrettyRep.Ctor ("LessOrEqual", NONE)
     | cvtBINOP (Greater) = PrettyRep.Ctor ("Greater", NONE)
     | cvtBINOP (GreaterOrEqual) = PrettyRep.Ctor ("GreaterOrEqual", NONE)
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus) = PrettyRep.Ctor ("AssignPlus", NONE)
     | cvtASSIGNOP (AssignMinus) = PrettyRep.Ctor ("AssignMinus", NONE)
     | cvtASSIGNOP (AssignTimes) = PrettyRep.Ctor ("AssignTimes", NONE)
     | cvtASSIGNOP (AssignDivide) = PrettyRep.Ctor ("AssignDivide", NONE)
     | cvtASSIGNOP (AssignRemainder) = PrettyRep.Ctor ("AssignRemainder", NONE)
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
     | cvtVAR_DEFN_TAG (Rest) = PrettyRep.Ctor ("Rest", NONE)
   and cvtNAMESPACE (Private) = PrettyRep.Ctor ("Private", NONE)
     | cvtNAMESPACE (Protected) = PrettyRep.Ctor ("Protected", NONE)
     | cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Public x80) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x80))
     | cvtNAMESPACE (Internal x83) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x83))
     | cvtNAMESPACE (UserDefined x86) = PrettyRep.Ctor ("UserDefined", SOME (cvtIDENT x86))
   and cvtPRIM_KIND (Named) = PrettyRep.Ctor ("Named", NONE)
     | cvtPRIM_KIND (Nullable) = PrettyRep.Ctor ("Nullable", NONE)
     | cvtPRIM_KIND (NonNullable) = PrettyRep.Ctor ("NonNullable", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (NoType) = PrettyRep.Ctor ("NoType", NONE)
   and cvtPRAGMA (UseNamespace x96) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x96))
     | cvtPRAGMA (UseDefaultNamespace x99) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x99))
     | cvtPRAGMA (UseNumber x102) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x102))
     | cvtPRAGMA (UseRounding x105) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x105))
     | cvtPRAGMA (UsePrecision x108) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x108))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x113, name=x114, alias=opt116}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x113), ("name", cvtIDENT x114), 
          ("alias", 
       (case opt116 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x115 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x115))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x136, sign=x137, body=x138}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x136), ("sign", cvtFUNC_SIGN x137), 
          ("body", cvtBLOCK x138)]))
   and cvtDEFN (ClassDefn x148) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x148))
     | cvtDEFN (VariableDefn ls152) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x151 => 
                                                                                                            cvtVAR_BINDING x151
                                                                                                     ) ls152)))
     | cvtDEFN (FunctionDefn x158) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x158))
     | cvtDEFN (InterfaceDefn x161) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x161))
     | cvtDEFN (NamespaceDefn{name=x164, init=x165}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x164), ("init", cvtEXPR x165)]))
   and cvtFUNC_SIGN (FunctionSignature{typeparams=ls174, params=ls179, resulttype=x183}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeparams", 
          PrettyRep.List (List.map (fn x173 => cvtIDENT x173
                                   ) ls174)), ("params", PrettyRep.List (List.map (fn x178 => 
                                                                                         cvtVAR_BINDING x178
                                                                                  ) ls179)), 
          ("resulttype", cvtTYPE_EXPR x183)]))
   and cvtATTRIBUTES (Attributes{ns=x193, override=b194, static=b195, final=b196, 
          dynamic=b197, prototype=b198, native=b199, rest=b200}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x193), ("override", PrettyRep.Bool b194), 
          ("static", PrettyRep.Bool b195), ("final", PrettyRep.Bool b196), 
          ("dynamic", PrettyRep.Bool b197), ("prototype", PrettyRep.Bool b198), 
          ("native", PrettyRep.Bool b199), ("rest", PrettyRep.Bool b200)]))
   and cvtVAR_BINDING (Binding{kind=x220, init=opt222, attrs=x226, pattern=x227, 
          ty=opt229}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x220), ("init", 
       (case opt222 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x221 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x221))
       )), ("attrs", cvtATTRIBUTES x226), ("pattern", cvtPATTERN x227), ("ty", 
          
       (case opt229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x228 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x228))
       ))]))
   and cvtTYPE_EXPR (SpecialType x246) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x246))
     | cvtTYPE_EXPR (UnionType ls250) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x249 => 
                                                                                                           cvtTYPE_EXPR x249
                                                                                                    ) ls250)))
     | cvtTYPE_EXPR (ArrayType ls257) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x256 => 
                                                                                                           cvtTYPE_EXPR x256
                                                                                                    ) ls257)))
     | cvtTYPE_EXPR (PrimaryType{ident=x263, kind=x264}) = PrettyRep.Ctor ("PrimaryType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x263), ("kind", cvtPRIM_KIND x264)]))
     | cvtTYPE_EXPR (FunctionType x272) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TY x272))
     | cvtTYPE_EXPR (RecordType ls276) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn x275 => 
                                                                                                             cvtFIELD_TYPE x275
                                                                                                      ) ls276)))
     | cvtTYPE_EXPR (InstantiationType{base=x282, params=ls284}) = PrettyRep.Ctor ("InstantiationType", 
          SOME (PrettyRep.Rec [("base", cvtPRIM_TY x282), ("params", PrettyRep.List (List.map (fn x283 => 
                                                                                                     cvtTYPE_EXPR x283
                                                                                              ) ls284))]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls297) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x296 => 
                                                                                                    cvtEXPR x296
                                                                                             ) ls297)))
     | cvtSTMT (ForEachStmt x303) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x303))
     | cvtSTMT (ForInStmt x306) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x306))
     | cvtSTMT (ThrowStmt ls310) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x309 => 
                                                                                                      cvtEXPR x309
                                                                                               ) ls310)))
     | cvtSTMT (ReturnStmt ls317) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x316 => 
                                                                                                        cvtEXPR x316
                                                                                                 ) ls317)))
     | cvtSTMT (BreakStmt opt324) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x323 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x323))
       ))
     | cvtSTMT (ContinueStmt opt331) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt331 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x330 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x330))
       ))
     | cvtSTMT (BlockStmt x337) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x337))
     | cvtSTMT (LabeledStmt(x340, x341)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x340, 
          cvtSTMT x341]))
     | cvtSTMT (LetStmt(ls346, x350)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x345 => 
                                                                                                                          cvtVAR_BINDING x345
                                                                                                                   ) ls346), 
          cvtSTMT x350]))
     | cvtSTMT (SuperStmt ls355) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x354 => 
                                                                                                      cvtEXPR x354
                                                                                               ) ls355)))
     | cvtSTMT (WhileStmt x361) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x361))
     | cvtSTMT (DoWhileStmt x364) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x364))
     | cvtSTMT (ForStmt{defns=ls368, init=ls373, cond=ls378, update=ls383, 
          contLabel=opt388, body=x392}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x367 => cvtVAR_BINDING x367
                                   ) ls368)), ("init", PrettyRep.List (List.map (fn x372 => 
                                                                                       cvtEXPR x372
                                                                                ) ls373)), 
          ("cond", PrettyRep.List (List.map (fn x377 => cvtEXPR x377
                                            ) ls378)), ("update", PrettyRep.List (List.map (fn x382 => 
                                                                                                  cvtEXPR x382
                                                                                           ) ls383)), 
          ("contLabel", 
       (case opt388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x387 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x387))
       )), ("body", cvtSTMT x392)]))
     | cvtSTMT (IfStmt{cnd=x408, thn=x409, els=x410}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x408), ("thn", cvtSTMT x409), 
          ("els", cvtSTMT x410)]))
     | cvtSTMT (WithStmt{obj=ls421, ty=x425, body=x426}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x420 => 
                                                                       cvtEXPR x420
                                                                ) ls421)), 
          ("ty", cvtTYPE_EXPR x425), ("body", cvtSTMT x426)]))
     | cvtSTMT (TryStmt{body=x436, catches=ls444, finally=opt449}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x436), ("catches", PrettyRep.List (List.map (fn {bind=x437, 
                                                                                                    body=x438} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x437), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x438)]
                                                                                             ) ls444)), 
          ("finally", 
       (case opt449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x448 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x448))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls463, cases=ls468}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x462 => 
                                                                        cvtEXPR x462
                                                                 ) ls463)), 
          ("cases", PrettyRep.List (List.map (fn x467 => cvtCASE x467
                                             ) ls468))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls480, ty=x484, cases=ls486}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x479 => 
                                                                        cvtEXPR x479
                                                                 ) ls480)), 
          ("ty", cvtTYPE_EXPR x484), ("cases", PrettyRep.List (List.map (fn x485 => 
                                                                               cvtTYPE_CASE x485
                                                                        ) ls486))]))
     | cvtSTMT (Dxns{expr=x499}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x499)]))
   and cvtEXPR (TrinaryExpr(x505, x506, x507, x508)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x505, cvtEXPR x506, cvtEXPR x507, 
          cvtEXPR x508]))
     | cvtEXPR (BinaryExpr(x512, x513, x514)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x512, cvtEXPR x513, cvtEXPR x514]))
     | cvtEXPR (BinaryTypeExpr(x518, x519, x520)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x518, cvtEXPR x519, cvtTYPE_EXPR x520]))
     | cvtEXPR (UnaryExpr(x524, x525)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x524, 
          cvtEXPR x525]))
     | cvtEXPR (TypeExpr x529) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x529))
     | cvtEXPR (NullaryExpr x532) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x532))
     | cvtEXPR (YieldExpr opt540) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls536 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x535 => 
                                                                                     cvtEXPR x535
                                                                              ) ls536)))
       ))
     | cvtEXPR (SuperExpr opt547) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x546 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x546))
       ))
     | cvtEXPR (LiteralExpr x553) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x553))
     | cvtEXPR (CallExpr{func=x556, actuals=ls558}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x556), ("actuals", PrettyRep.List (List.map (fn x557 => 
                                                                                                   cvtEXPR x557
                                                                                            ) ls558))]))
     | cvtEXPR (LetExpr{defs=ls570, body=ls575}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x569 => 
                                                                        cvtVAR_BINDING x569
                                                                 ) ls570)), 
          ("body", PrettyRep.List (List.map (fn x574 => cvtEXPR x574
                                            ) ls575))]))
     | cvtEXPR (NewExpr{obj=x586, actuals=ls588}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x586), ("actuals", PrettyRep.List (List.map (fn x587 => 
                                                                                                  cvtEXPR x587
                                                                                           ) ls588))]))
     | cvtEXPR (FunExpr{ident=opt600, sign=x604, body=x605}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x599 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x599))
       )), ("sign", cvtFUNC_SIGN x604), ("body", cvtBLOCK x605)]))
     | cvtEXPR (ObjectRef{base=x615, ident=x616}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x615), ("ident", cvtIDENT_EXPR x616)]))
     | cvtEXPR (LexicalRef{ident=x624}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x624)]))
     | cvtEXPR (SetExpr(x630, x631, x632)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x630, 
          cvtPATTERN x631, cvtEXPR x632]))
     | cvtEXPR (ListExpr ls637) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x636 => 
                                                                                                    cvtEXPR x636
                                                                                             ) ls637)))
     | cvtEXPR (SliceExpr(ls644, ls649, ls654)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x643 => cvtEXPR x643
                                                          ) ls644), PrettyRep.List (List.map (fn x648 => 
                                                                                                    cvtEXPR x648
                                                                                             ) ls649), 
          PrettyRep.List (List.map (fn x653 => cvtEXPR x653
                                   ) ls654)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x661, ident=x662}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x661), ("ident", cvtUSTRING x662)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x670, expr=x671}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x670), ("expr", cvtEXPR x671)]))
     | cvtIDENT_EXPR (AttributeIdentifier x679) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x679))
     | cvtIDENT_EXPR (Identifier{ident=x682, openNamespaces=r688}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x682), ("openNamespaces", 
          
       (case ! r688 of
         ls684 => PrettyRep.Ref (PrettyRep.List (List.map (fn x683 => cvtNAMESPACE x683
                                                          ) ls684))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x699) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x699))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x702, typeParams=ls704}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x702), ("typeParams", 
          PrettyRep.List (List.map (fn x703 => cvtTYPE_EXPR x703
                                   ) ls704))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r717) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r717))
     | cvtLITERAL (LiteralBoolean b720) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b720))
     | cvtLITERAL (LiteralString x723) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x723))
     | cvtLITERAL (LiteralArray{exprs=ls727, ty=opt732}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x726 => 
                                                                         cvtEXPR x726
                                                                  ) ls727)), 
          ("ty", 
       (case opt732 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x731 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x731))
       ))]))
     | cvtLITERAL (LiteralXML ls744) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x743 => 
                                                                                                           cvtEXPR x743
                                                                                                    ) ls744)))
     | cvtLITERAL (LiteralNamespace x750) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x750))
     | cvtLITERAL (LiteralObject{expr=ls754, ty=opt759}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x753 => 
                                                                        cvtFIELD x753
                                                                 ) ls754)), 
          ("ty", 
       (case opt759 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x758 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x758))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x770}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x770)]))
   and cvtBLOCK (Block{pragmas=ls777, defns=ls782, stmts=ls787}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x776 => 
                                                                           cvtPRAGMA x776
                                                                    ) ls777)), 
          ("defns", PrettyRep.List (List.map (fn x781 => cvtDEFN x781
                                             ) ls782)), ("stmts", PrettyRep.List (List.map (fn x786 => 
                                                                                                  cvtSTMT x786
                                                                                           ) ls787))]))
   and cvtPATTERN (ObjectPattern ls807) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x800, ptrn=x801} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x800), ("ptrn", 
                                                cvtPATTERN x801)]
                                         ) ls807)))
     | cvtPATTERN (ArrayPattern ls814) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x813 => 
                                                                                                               cvtPATTERN x813
                                                                                                        ) ls814)))
     | cvtPATTERN (SimplePattern x820) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x820))
     | cvtPATTERN (IdentifierPattern x823) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x823))
   and cvtFIELD {kind=x826, name=x827, init=x828} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x826), ("name", cvtIDENT_EXPR x827), ("init", cvtEXPR x828)]
   and cvtFIELD_TYPE {name=x836, ty=x837} = PrettyRep.Rec [("name", cvtIDENT_EXPR x836), 
          ("ty", cvtTYPE_EXPR x837)]
   and cvtFUNC_TY {paramTypes=ls848, returnType=x852, boundThisType=opt854, 
          hasRest=b858} = PrettyRep.Rec [("paramTypes", PrettyRep.List (List.map (fn opt844 => 
                                                                                        
                                                                                     (case opt844 of
                                                                                       NONE => 
                                                                                          PrettyRep.Ctor ("NONE", 
                                                                                          NONE)
                                                                                     | SOME x843 => 
                                                                                          PrettyRep.Ctor ("SOME", 
                                                                                          SOME (cvtTYPE_EXPR x843))
                                                                                     )
                                                                                 ) ls848)), 
          ("returnType", cvtTYPE_EXPR x852), ("boundThisType", 
       (case opt854 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x853 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x853))
       )), ("hasRest", PrettyRep.Bool b858)]
   and cvtTYPED_IDENT {name=x868, ty=opt870} = PrettyRep.Rec [("name", cvtIDENT x868), 
          ("ty", 
       (case opt870 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x869 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x869))
       ))]
   and cvtFUNC_DEFN {attrs=x879, kind=x880, func=x881} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x879), ("kind", cvtVAR_DEFN_TAG x880), ("func", cvtFUNC x881)]
   and cvtCLASS_DEFN {name=x889, nonnullable=b890, attrs=x891, params=ls893, 
          extends=opt898, implements=ls903, body=x907, instanceVars=ls909, 
          instanceMethods=ls914, vars=ls919, methods=ls924, constructor=opt929, 
          initializer=ls934} = PrettyRep.Rec [("name", cvtIDENT x889), ("nonnullable", 
          PrettyRep.Bool b890), ("attrs", cvtATTRIBUTES x891), ("params", PrettyRep.List (List.map (fn x892 => 
                                                                                                          cvtIDENT x892
                                                                                                   ) ls893)), 
          ("extends", 
       (case opt898 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x897 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x897))
       )), ("implements", PrettyRep.List (List.map (fn x902 => cvtIDENT_EXPR x902
                                                   ) ls903)), ("body", cvtBLOCK x907), 
          ("instanceVars", PrettyRep.List (List.map (fn x908 => cvtVAR_BINDING x908
                                                    ) ls909)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x913 => cvtFUNC x913
                                   ) ls914)), ("vars", PrettyRep.List (List.map (fn x918 => 
                                                                                       cvtVAR_BINDING x918
                                                                                ) ls919)), 
          ("methods", PrettyRep.List (List.map (fn x923 => cvtFUNC x923
                                               ) ls924)), ("constructor", 
       (case opt929 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x928 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x928))
       )), ("initializer", PrettyRep.List (List.map (fn x933 => cvtSTMT x933
                                                    ) ls934))]
   and cvtINTERFACE_DEFN {name=x965, nonnullable=b966, attrs=x967, params=ls969, 
          extends=ls974, body=x978} = PrettyRep.Rec [("name", cvtIDENT x965), 
          ("nonnullable", PrettyRep.Bool b966), ("attrs", cvtATTRIBUTES x967), 
          ("params", PrettyRep.List (List.map (fn x968 => cvtIDENT x968
                                              ) ls969)), ("extends", PrettyRep.List (List.map (fn x973 => 
                                                                                                     cvtIDENT_EXPR x973
                                                                                              ) ls974)), 
          ("body", cvtBLOCK x978)]
   and cvtPRIM_TY {name=x992, kind=x993} = PrettyRep.Rec [("name", cvtUSTRING x992), 
          ("kind", cvtPRIM_KIND x993)]
   and cvtFOR_ENUM_STMT {ptrn=opt1000, obj=ls1005, defns=ls1010, contLabel=opt1015, 
          body=x1019} = PrettyRep.Rec [("ptrn", 
       (case opt1000 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x999 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x999))
       )), ("obj", PrettyRep.List (List.map (fn x1004 => cvtEXPR x1004
                                            ) ls1005)), ("defns", PrettyRep.List (List.map (fn x1009 => 
                                                                                                  cvtVAR_BINDING x1009
                                                                                           ) ls1010)), 
          ("contLabel", 
       (case opt1015 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1014 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1014))
       )), ("body", cvtSTMT x1019)]
   and cvtWHILE_STMT {cond=x1031, body=x1032, contLabel=opt1034} = PrettyRep.Rec [("cond", 
          cvtEXPR x1031), ("body", cvtSTMT x1032), ("contLabel", 
       (case opt1034 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1033 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1033))
       ))]
   and cvtDIRECTIVES {pragmas=ls1046, defns=ls1051, stmts=ls1056} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1045 => cvtPRAGMA x1045
                                   ) ls1046)), ("defns", PrettyRep.List (List.map (fn x1050 => 
                                                                                         cvtDEFN x1050
                                                                                  ) ls1051)), 
          ("stmts", PrettyRep.List (List.map (fn x1055 => cvtSTMT x1055
                                             ) ls1056))]
   and cvtBINDINGS {defns=ls1068, inits=ls1073} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1067 => cvtVAR_BINDING x1067
                                   ) ls1068)), ("inits", PrettyRep.List (List.map (fn x1072 => 
                                                                                         cvtEXPR x1072
                                                                                  ) ls1073))]
   and cvtCASE {label=opt1087, stmts=x1091} = PrettyRep.Rec [("label", 
       (case opt1087 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1083 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1082 => 
                                                                                      cvtEXPR x1082
                                                                               ) ls1083)))
       )), ("stmts", cvtDIRECTIVES x1091)]
   and cvtTYPE_CASE {ptrn=opt1098, body=x1102} = PrettyRep.Rec [("ptrn", 
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1097))
       )), ("body", cvtBLOCK x1102)]
   and cvtFUNC_NAME {kind=x1108, ident=x1109} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1108), 
          ("ident", cvtIDENT x1109)]
   and cvtPACKAGE {names=ls1116, fullname=x1120, body=x1121} = PrettyRep.Rec [("names", 
          PrettyRep.List (List.map (fn x1115 => cvtIDENT x1115
                                   ) ls1116)), ("fullname", cvtUSTRING x1120), 
          ("body", cvtBLOCK x1121)]
   and cvtPROGRAM {packages=ls1130, body=x1134} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1129 => cvtPACKAGE x1129
                                   ) ls1130)), ("body", cvtBLOCK x1134)]
end

