structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Private) = PrettyRep.Ctor ("Private", NONE)
     | cvtNAMESPACE (Protected) = PrettyRep.Ctor ("Protected", NONE)
     | cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Public x5) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x5))
     | cvtNAMESPACE (Internal x8) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x8))
     | cvtNAMESPACE (UserDefined x11) = PrettyRep.Ctor ("UserDefined", SOME (cvtIDENT x11))
   and cvtNAME {ns=x14, id=x15} = PrettyRep.Rec [("ns", cvtNAMESPACE x14), 
          ("id", cvtIDENT x15)]
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
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x100) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x100))
     | cvtPRAGMA (UseDefaultNamespace x103) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x103))
     | cvtPRAGMA (UseNumber x106) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x106))
     | cvtPRAGMA (UseRounding x109) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x109))
     | cvtPRAGMA (UsePrecision x112) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x112))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x117, name=x118, alias=opt120}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x117), ("name", cvtIDENT x118), 
          ("alias", 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x119))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x140, fsig=x141, body=x142, fixtures=opt144}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x140), ("fsig", cvtFUNC_SIG x141), 
          ("body", cvtBLOCK x142), ("fixtures", 
       (case opt144 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x143 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x143))
       ))]))
   and cvtDEFN (ClassDefn x159) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x159))
     | cvtDEFN (VariableDefn ls163) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x162 => 
                                                                                                            cvtVAR_BINDING x162
                                                                                                     ) ls163)))
     | cvtDEFN (FunctionDefn x169) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x169))
     | cvtDEFN (InterfaceDefn x172) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x172))
     | cvtDEFN (NamespaceDefn x175) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x175))
     | cvtDEFN (TypeDefn{attrs=x178, ident=x179, init=x180}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x178), ("ident", cvtIDENT x179), 
          ("init", cvtTYPE_EXPR x180)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls191, params=ls196, inits=opt201, 
          returnType=x205, thisType=opt207, hasBoundThis=b211, hasRest=b212}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x190 => cvtIDENT x190
                                   ) ls191)), ("params", PrettyRep.List (List.map (fn x195 => 
                                                                                         cvtVAR_BINDING x195
                                                                                  ) ls196)), 
          ("inits", 
       (case opt201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x200 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x200))
       )), ("returnType", cvtTYPE_EXPR x205), ("thisType", 
       (case opt207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x206 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x206))
       )), ("hasBoundThis", PrettyRep.Bool b211), ("hasRest", PrettyRep.Bool b212)]))
   and cvtATTRIBUTES (Attributes{ns=x230, override=b231, static=b232, final=b233, 
          dynamic=b234, prototype=b235, native=b236, rest=b237}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x230), ("override", PrettyRep.Bool b231), 
          ("static", PrettyRep.Bool b232), ("final", PrettyRep.Bool b233), 
          ("dynamic", PrettyRep.Bool b234), ("prototype", PrettyRep.Bool b235), 
          ("native", PrettyRep.Bool b236), ("rest", PrettyRep.Bool b237)]))
   and cvtVAR_BINDING (Binding{kind=x257, init=opt259, attrs=x263, pattern=x264, 
          ty=opt266}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x257), ("init", 
       (case opt259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x258 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x258))
       )), ("attrs", cvtATTRIBUTES x263), ("pattern", cvtPATTERN x264), ("ty", 
          
       (case opt266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x265 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x265))
       ))]))
   and cvtTYPE_EXPR (SpecialType x283) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x283))
     | cvtTYPE_EXPR (UnionType ls287) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x286 => 
                                                                                                           cvtTYPE_EXPR x286
                                                                                                    ) ls287)))
     | cvtTYPE_EXPR (ArrayType ls294) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x293 => 
                                                                                                           cvtTYPE_EXPR x293
                                                                                                    ) ls294)))
     | cvtTYPE_EXPR (NominalType{ident=x300, nullable=opt302}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x300), ("nullable", 
          
       (case opt302 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b301 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b301))
       ))]))
     | cvtTYPE_EXPR (FunctionType x313) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x313))
     | cvtTYPE_EXPR (ObjectType ls317) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x316 => 
                                                                                                             cvtFIELD_TYPE x316
                                                                                                      ) ls317)))
     | cvtTYPE_EXPR (AppType{base=x323, args=ls325}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x323), ("args", PrettyRep.List (List.map (fn x324 => 
                                                                                                     cvtTYPE_EXPR x324
                                                                                              ) ls325))]))
     | cvtTYPE_EXPR (NullableType{expr=x336, nullable=b337}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x336), ("nullable", PrettyRep.Bool b337)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls347) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x346 => 
                                                                                                    cvtEXPR x346
                                                                                             ) ls347)))
     | cvtSTMT (ForEachStmt x353) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x353))
     | cvtSTMT (ForInStmt x356) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x356))
     | cvtSTMT (ThrowStmt ls360) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x359 => 
                                                                                                      cvtEXPR x359
                                                                                               ) ls360)))
     | cvtSTMT (ReturnStmt ls367) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x366 => 
                                                                                                        cvtEXPR x366
                                                                                                 ) ls367)))
     | cvtSTMT (BreakStmt opt374) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x373 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x373))
       ))
     | cvtSTMT (ContinueStmt opt381) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x380 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x380))
       ))
     | cvtSTMT (BlockStmt x387) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x387))
     | cvtSTMT (LabeledStmt(x390, x391)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x390, 
          cvtSTMT x391]))
     | cvtSTMT (LetStmt(ls396, x400)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x395 => 
                                                                                                                          cvtVAR_BINDING x395
                                                                                                                   ) ls396), 
          cvtSTMT x400]))
     | cvtSTMT (SuperStmt ls405) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x404 => 
                                                                                                      cvtEXPR x404
                                                                                               ) ls405)))
     | cvtSTMT (WhileStmt x411) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x411))
     | cvtSTMT (DoWhileStmt x414) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x414))
     | cvtSTMT (ForStmt{defns=ls418, init=ls423, cond=ls428, update=ls433, 
          contLabel=opt438, body=x442}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x417 => cvtVAR_BINDING x417
                                   ) ls418)), ("init", PrettyRep.List (List.map (fn x422 => 
                                                                                       cvtEXPR x422
                                                                                ) ls423)), 
          ("cond", PrettyRep.List (List.map (fn x427 => cvtEXPR x427
                                            ) ls428)), ("update", PrettyRep.List (List.map (fn x432 => 
                                                                                                  cvtEXPR x432
                                                                                           ) ls433)), 
          ("contLabel", 
       (case opt438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x437 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x437))
       )), ("body", cvtSTMT x442)]))
     | cvtSTMT (IfStmt{cnd=x458, thn=x459, els=x460}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x458), ("thn", cvtSTMT x459), 
          ("els", cvtSTMT x460)]))
     | cvtSTMT (WithStmt{obj=ls471, ty=x475, body=x476}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x470 => 
                                                                       cvtEXPR x470
                                                                ) ls471)), 
          ("ty", cvtTYPE_EXPR x475), ("body", cvtSTMT x476)]))
     | cvtSTMT (TryStmt{body=x486, catches=ls494, finally=opt499}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x486), ("catches", PrettyRep.List (List.map (fn {bind=x487, 
                                                                                                    body=x488} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x487), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x488)]
                                                                                             ) ls494)), 
          ("finally", 
       (case opt499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x498 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x498))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls513, cases=ls518}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x512 => 
                                                                        cvtEXPR x512
                                                                 ) ls513)), 
          ("cases", PrettyRep.List (List.map (fn x517 => cvtCASE x517
                                             ) ls518))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls530, ty=x534, cases=ls536}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x529 => 
                                                                        cvtEXPR x529
                                                                 ) ls530)), 
          ("ty", cvtTYPE_EXPR x534), ("cases", PrettyRep.List (List.map (fn x535 => 
                                                                               cvtTYPE_CASE x535
                                                                        ) ls536))]))
     | cvtSTMT (Dxns{expr=x549}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x549)]))
   and cvtEXPR (TrinaryExpr(x555, x556, x557, x558)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x555, cvtEXPR x556, cvtEXPR x557, 
          cvtEXPR x558]))
     | cvtEXPR (BinaryExpr(x562, x563, x564)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x562, cvtEXPR x563, cvtEXPR x564]))
     | cvtEXPR (BinaryTypeExpr(x568, x569, x570)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x568, cvtEXPR x569, cvtTYPE_EXPR x570]))
     | cvtEXPR (UnaryExpr(x574, x575)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x574, 
          cvtEXPR x575]))
     | cvtEXPR (TypeExpr x579) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x579))
     | cvtEXPR (NullaryExpr x582) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x582))
     | cvtEXPR (YieldExpr opt590) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls586 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x585 => 
                                                                                     cvtEXPR x585
                                                                              ) ls586)))
       ))
     | cvtEXPR (SuperExpr opt597) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x596 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x596))
       ))
     | cvtEXPR (LiteralExpr x603) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x603))
     | cvtEXPR (CallExpr{func=x606, actuals=ls608}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x606), ("actuals", PrettyRep.List (List.map (fn x607 => 
                                                                                                   cvtEXPR x607
                                                                                            ) ls608))]))
     | cvtEXPR (ApplyTypeExpr{expr=x619, actuals=ls621}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x619), ("actuals", PrettyRep.List (List.map (fn x620 => 
                                                                                                   cvtTYPE_EXPR x620
                                                                                            ) ls621))]))
     | cvtEXPR (LetExpr{defs=ls633, body=ls638, fixtures=opt643}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x632 => 
                                                                        cvtVAR_BINDING x632
                                                                 ) ls633)), 
          ("body", PrettyRep.List (List.map (fn x637 => cvtEXPR x637
                                            ) ls638)), ("fixtures", 
       (case opt643 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x642 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x642))
       ))]))
     | cvtEXPR (NewExpr{obj=x656, actuals=ls658}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x656), ("actuals", PrettyRep.List (List.map (fn x657 => 
                                                                                                  cvtEXPR x657
                                                                                           ) ls658))]))
     | cvtEXPR (FunExpr{ident=opt670, fsig=x674, body=x675, fixtures=opt677}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x669 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x669))
       )), ("fsig", cvtFUNC_SIG x674), ("body", cvtBLOCK x675), ("fixtures", 
          
       (case opt677 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x676 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x676))
       ))]))
     | cvtEXPR (ObjectRef{base=x692, ident=x693}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x692), ("ident", cvtIDENT_EXPR x693)]))
     | cvtEXPR (LexicalRef{ident=x701}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x701)]))
     | cvtEXPR (SetExpr(x707, x708, x709)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x707, 
          cvtPATTERN x708, cvtEXPR x709]))
     | cvtEXPR (ListExpr ls714) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x713 => 
                                                                                                    cvtEXPR x713
                                                                                             ) ls714)))
     | cvtEXPR (SliceExpr(ls721, ls726, ls731)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x720 => cvtEXPR x720
                                                          ) ls721), PrettyRep.List (List.map (fn x725 => 
                                                                                                    cvtEXPR x725
                                                                                             ) ls726), 
          PrettyRep.List (List.map (fn x730 => cvtEXPR x730
                                   ) ls731)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x738, ident=x739}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x738), ("ident", cvtUSTRING x739)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x747, expr=x748}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x747), ("expr", cvtEXPR x748)]))
     | cvtIDENT_EXPR (AttributeIdentifier x756) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x756))
     | cvtIDENT_EXPR (Identifier{ident=x759, openNamespaces=ls761}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x759), ("openNamespaces", 
          PrettyRep.List (List.map (fn x760 => cvtNAMESPACE x760
                                   ) ls761))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x772) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x772))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x775, typeParams=ls777}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x775), ("typeParams", 
          PrettyRep.List (List.map (fn x776 => cvtTYPE_EXPR x776
                                   ) ls777))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r790) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r790))
     | cvtLITERAL (LiteralBoolean b793) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b793))
     | cvtLITERAL (LiteralString x796) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x796))
     | cvtLITERAL (LiteralArray{exprs=ls800, ty=opt805}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x799 => 
                                                                         cvtEXPR x799
                                                                  ) ls800)), 
          ("ty", 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x804))
       ))]))
     | cvtLITERAL (LiteralXML ls817) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x816 => 
                                                                                                           cvtEXPR x816
                                                                                                    ) ls817)))
     | cvtLITERAL (LiteralNamespace x823) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x823))
     | cvtLITERAL (LiteralObject{expr=ls827, ty=opt832}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x826 => 
                                                                        cvtFIELD x826
                                                                 ) ls827)), 
          ("ty", 
       (case opt832 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x831 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x831))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x843}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x843)]))
   and cvtBLOCK (Block x849) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x849))
   and cvtPATTERN (ObjectPattern ls859) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x852, ptrn=x853} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x852), ("ptrn", 
                                                cvtPATTERN x853)]
                                         ) ls859)))
     | cvtPATTERN (ArrayPattern ls866) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x865 => 
                                                                                                               cvtPATTERN x865
                                                                                                        ) ls866)))
     | cvtPATTERN (SimplePattern x872) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x872))
     | cvtPATTERN (IdentifierPattern x875) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x875))
   and cvtFIXTURE (NamespaceFixture x878) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x878))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (ClassFixture x882) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x882))
     | cvtFIXTURE (TypeFixture x885) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x885))
     | cvtFIXTURE (ValFixture{ty=x888, readOnly=b889, isOverride=b890}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x888), ("readOnly", PrettyRep.Bool b889), 
          ("isOverride", PrettyRep.Bool b890)]))
   and cvtFIXTURES (Fixtures{bindings=x900, openNamespaces=ls902, numberType=x906, 
          roundingMode=x907}) = PrettyRep.Ctor ("Fixtures", SOME (PrettyRep.Rec [("bindings", 
          cvtFIXTURE_BINDINGS x900), ("openNamespaces", PrettyRep.List (List.map (fn x901 => 
                                                                                        cvtNAMESPACE x901
                                                                                 ) ls902)), 
          ("numberType", cvtNUMBER_TYPE x906), ("roundingMode", cvtROUNDING_MODE x907)]))
   and cvtFIELD {kind=x919, name=x920, init=x921} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x919), ("name", cvtIDENT_EXPR x920), ("init", cvtEXPR x921)]
   and cvtFIELD_TYPE {name=x929, ty=x930} = PrettyRep.Rec [("name", cvtIDENT_EXPR x929), 
          ("ty", cvtTYPE_EXPR x930)]
   and cvtTYPED_IDENT {name=x936, ty=opt938} = PrettyRep.Rec [("name", cvtIDENT x936), 
          ("ty", 
       (case opt938 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x937 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x937))
       ))]
   and cvtFUNC_DEFN {attrs=x947, kind=x948, func=x949} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x947), ("kind", cvtVAR_DEFN_TAG x948), ("func", cvtFUNC x949)]
   and cvtFIXTURE_BINDINGS ls960 = PrettyRep.List (List.map (fn (x957, x958) => 
                                                                   PrettyRep.Tuple [cvtNAME x957, 
                                                                   cvtFIXTURE x958]
                                                            ) ls960)
   and cvtNAMESPACE_DEFN {attrs=x964, ident=x965, init=opt967} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x964), ("ident", cvtIDENT x965), ("init", 
       (case opt967 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x966 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x966))
       ))]
   and cvtCLASS_DEFN {name=x978, nonnullable=b979, attrs=x980, params=ls982, 
          extends=opt987, implements=ls992, classFixtures=opt997, instanceFixtures=opt1002, 
          body=x1006, instanceVars=ls1008, instanceMethods=ls1013, vars=ls1018, 
          methods=ls1023, constructor=opt1028, initializer=ls1033} = PrettyRep.Rec [("name", 
          cvtIDENT x978), ("nonnullable", PrettyRep.Bool b979), ("attrs", cvtATTRIBUTES x980), 
          ("params", PrettyRep.List (List.map (fn x981 => cvtIDENT x981
                                              ) ls982)), ("extends", 
       (case opt987 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x986 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x986))
       )), ("implements", PrettyRep.List (List.map (fn x991 => cvtIDENT_EXPR x991
                                                   ) ls992)), ("classFixtures", 
          
       (case opt997 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x996 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x996))
       )), ("instanceFixtures", 
       (case opt1002 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1001 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1001))
       )), ("body", cvtBLOCK x1006), ("instanceVars", PrettyRep.List (List.map (fn x1007 => 
                                                                                      cvtVAR_BINDING x1007
                                                                               ) ls1008)), 
          ("instanceMethods", PrettyRep.List (List.map (fn x1012 => cvtFUNC x1012
                                                       ) ls1013)), ("vars", 
          PrettyRep.List (List.map (fn x1017 => cvtVAR_BINDING x1017
                                   ) ls1018)), ("methods", PrettyRep.List (List.map (fn x1022 => 
                                                                                           cvtFUNC x1022
                                                                                    ) ls1023)), 
          ("constructor", 
       (case opt1028 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1027 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1027))
       )), ("initializer", PrettyRep.List (List.map (fn x1032 => cvtSTMT x1032
                                                    ) ls1033))]
   and cvtINTERFACE_DEFN {name=x1068, nonnullable=b1069, attrs=x1070, params=ls1072, 
          extends=ls1077, body=x1081} = PrettyRep.Rec [("name", cvtIDENT x1068), 
          ("nonnullable", PrettyRep.Bool b1069), ("attrs", cvtATTRIBUTES x1070), 
          ("params", PrettyRep.List (List.map (fn x1071 => cvtIDENT x1071
                                              ) ls1072)), ("extends", PrettyRep.List (List.map (fn x1076 => 
                                                                                                      cvtIDENT_EXPR x1076
                                                                                               ) ls1077)), 
          ("body", cvtBLOCK x1081)]
   and cvtFOR_ENUM_STMT {ptrn=opt1096, obj=ls1101, defns=ls1106, contLabel=opt1111, 
          body=x1115} = PrettyRep.Rec [("ptrn", 
       (case opt1096 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1095 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1095))
       )), ("obj", PrettyRep.List (List.map (fn x1100 => cvtEXPR x1100
                                            ) ls1101)), ("defns", PrettyRep.List (List.map (fn x1105 => 
                                                                                                  cvtVAR_BINDING x1105
                                                                                           ) ls1106)), 
          ("contLabel", 
       (case opt1111 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1110 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1110))
       )), ("body", cvtSTMT x1115)]
   and cvtWHILE_STMT {cond=x1127, body=x1128, contLabel=opt1130} = PrettyRep.Rec [("cond", 
          cvtEXPR x1127), ("body", cvtSTMT x1128), ("contLabel", 
       (case opt1130 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1129 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1129))
       ))]
   and cvtDIRECTIVES {pragmas=ls1142, defns=ls1147, stmts=ls1152, fixtures=opt1157} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1141 => 
                                                                     cvtPRAGMA x1141
                                                              ) ls1142)), ("defns", 
          PrettyRep.List (List.map (fn x1146 => cvtDEFN x1146
                                   ) ls1147)), ("stmts", PrettyRep.List (List.map (fn x1151 => 
                                                                                         cvtSTMT x1151
                                                                                  ) ls1152)), 
          ("fixtures", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1156))
       ))]
   and cvtBINDINGS {defns=ls1171, inits=ls1176} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1170 => cvtVAR_BINDING x1170
                                   ) ls1171)), ("inits", PrettyRep.List (List.map (fn x1175 => 
                                                                                         cvtEXPR x1175
                                                                                  ) ls1176))]
   and cvtCASE {label=opt1190, body=x1194} = PrettyRep.Rec [("label", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1186 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1185 => 
                                                                                      cvtEXPR x1185
                                                                               ) ls1186)))
       )), ("body", cvtBLOCK x1194)]
   and cvtTYPE_CASE {ptrn=opt1201, body=x1205} = PrettyRep.Rec [("ptrn", 
       (case opt1201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1200 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1200))
       )), ("body", cvtBLOCK x1205)]
   and cvtFUNC_NAME {kind=x1211, ident=x1212} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1211), 
          ("ident", cvtIDENT x1212)]
   and cvtPACKAGE {name=x1218, body=x1219} = PrettyRep.Rec [("name", cvtUSTRING x1218), 
          ("body", cvtBLOCK x1219)]
   and cvtPROGRAM {packages=ls1226, body=x1230} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1225 => cvtPACKAGE x1225
                                   ) ls1226)), ("body", cvtBLOCK x1230)]
end

