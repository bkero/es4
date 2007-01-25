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
   and cvtMULTINAME {nss=ls22, id=x26} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn x21 => 
                                                                                                cvtNAMESPACE x21
                                                                                         ) ls22)), 
          ("id", cvtIDENT x26)]
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
   and cvtPRAGMA (UseNamespace x111) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x111))
     | cvtPRAGMA (UseDefaultNamespace x114) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x114))
     | cvtPRAGMA (UseNumber x117) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x117))
     | cvtPRAGMA (UseRounding x120) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x120))
     | cvtPRAGMA (UsePrecision x123) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x123))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x128, name=x129, alias=opt131}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x128), ("name", cvtIDENT x129), 
          ("alias", 
       (case opt131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x130 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x130))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x151, fsig=x152, body=x153, fixtures=x154}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x151), ("fsig", cvtFUNC_SIG x152), 
          ("body", cvtBLOCK x153), ("fixtures", cvtFIXTURES x154)]))
   and cvtDEFN (ClassDefn x166) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x166))
     | cvtDEFN (VariableDefn ls170) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x169 => 
                                                                                                            cvtVAR_BINDING x169
                                                                                                     ) ls170)))
     | cvtDEFN (FunctionDefn x176) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x176))
     | cvtDEFN (InterfaceDefn x179) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x179))
     | cvtDEFN (NamespaceDefn x182) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x182))
     | cvtDEFN (TypeDefn{attrs=x185, ident=x186, init=x187}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x185), ("ident", cvtIDENT x186), 
          ("init", cvtTYPE_EXPR x187)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls198, params=ls203, inits=opt208, 
          returnType=x212, thisType=opt214, hasBoundThis=b218, hasRest=b219}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x197 => cvtIDENT x197
                                   ) ls198)), ("params", PrettyRep.List (List.map (fn x202 => 
                                                                                         cvtVAR_BINDING x202
                                                                                  ) ls203)), 
          ("inits", 
       (case opt208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x207 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x207))
       )), ("returnType", cvtTYPE_EXPR x212), ("thisType", 
       (case opt214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x213 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x213))
       )), ("hasBoundThis", PrettyRep.Bool b218), ("hasRest", PrettyRep.Bool b219)]))
   and cvtATTRIBUTES (Attributes{ns=x237, override=b238, static=b239, final=b240, 
          dynamic=b241, prototype=b242, native=b243, rest=b244}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x237), ("override", PrettyRep.Bool b238), 
          ("static", PrettyRep.Bool b239), ("final", PrettyRep.Bool b240), 
          ("dynamic", PrettyRep.Bool b241), ("prototype", PrettyRep.Bool b242), 
          ("native", PrettyRep.Bool b243), ("rest", PrettyRep.Bool b244)]))
   and cvtVAR_BINDING (Binding{kind=x264, init=opt266, attrs=x270, pattern=x271, 
          ty=opt273}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x264), ("init", 
       (case opt266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x265 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x265))
       )), ("attrs", cvtATTRIBUTES x270), ("pattern", cvtPATTERN x271), ("ty", 
          
       (case opt273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x272 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x272))
       ))]))
   and cvtTYPE_EXPR (SpecialType x290) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x290))
     | cvtTYPE_EXPR (UnionType ls294) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x293 => 
                                                                                                           cvtTYPE_EXPR x293
                                                                                                    ) ls294)))
     | cvtTYPE_EXPR (ArrayType ls301) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x300 => 
                                                                                                           cvtTYPE_EXPR x300
                                                                                                    ) ls301)))
     | cvtTYPE_EXPR (NominalType{ident=x307, nullable=opt309}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x307), ("nullable", 
          
       (case opt309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b308 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b308))
       ))]))
     | cvtTYPE_EXPR (FunctionType x320) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x320))
     | cvtTYPE_EXPR (ObjectType ls324) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x323 => 
                                                                                                             cvtFIELD_TYPE x323
                                                                                                      ) ls324)))
     | cvtTYPE_EXPR (AppType{base=x330, args=ls332}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x330), ("args", PrettyRep.List (List.map (fn x331 => 
                                                                                                     cvtTYPE_EXPR x331
                                                                                              ) ls332))]))
     | cvtTYPE_EXPR (NullableType{expr=x343, nullable=b344}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x343), ("nullable", PrettyRep.Bool b344)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls354) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x353 => 
                                                                                                    cvtEXPR x353
                                                                                             ) ls354)))
     | cvtSTMT (ForEachStmt x360) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x360))
     | cvtSTMT (ForInStmt x363) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x363))
     | cvtSTMT (ThrowStmt ls367) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x366 => 
                                                                                                      cvtEXPR x366
                                                                                               ) ls367)))
     | cvtSTMT (ReturnStmt ls374) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x373 => 
                                                                                                        cvtEXPR x373
                                                                                                 ) ls374)))
     | cvtSTMT (BreakStmt opt381) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x380 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x380))
       ))
     | cvtSTMT (ContinueStmt opt388) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x387 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x387))
       ))
     | cvtSTMT (BlockStmt x394) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x394))
     | cvtSTMT (LabeledStmt(x397, x398)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x397, 
          cvtSTMT x398]))
     | cvtSTMT (LetStmt(ls403, x407)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x402 => 
                                                                                                                          cvtVAR_BINDING x402
                                                                                                                   ) ls403), 
          cvtSTMT x407]))
     | cvtSTMT (SuperStmt ls412) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x411 => 
                                                                                                      cvtEXPR x411
                                                                                               ) ls412)))
     | cvtSTMT (WhileStmt x418) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x418))
     | cvtSTMT (DoWhileStmt x421) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x421))
     | cvtSTMT (ForStmt{defns=ls425, init=ls430, cond=ls435, update=ls440, 
          contLabel=opt445, body=x449}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x424 => cvtVAR_BINDING x424
                                   ) ls425)), ("init", PrettyRep.List (List.map (fn x429 => 
                                                                                       cvtEXPR x429
                                                                                ) ls430)), 
          ("cond", PrettyRep.List (List.map (fn x434 => cvtEXPR x434
                                            ) ls435)), ("update", PrettyRep.List (List.map (fn x439 => 
                                                                                                  cvtEXPR x439
                                                                                           ) ls440)), 
          ("contLabel", 
       (case opt445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x444 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x444))
       )), ("body", cvtSTMT x449)]))
     | cvtSTMT (IfStmt{cnd=x465, thn=x466, els=x467}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x465), ("thn", cvtSTMT x466), 
          ("els", cvtSTMT x467)]))
     | cvtSTMT (WithStmt{obj=ls478, ty=x482, body=x483}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x477 => 
                                                                       cvtEXPR x477
                                                                ) ls478)), 
          ("ty", cvtTYPE_EXPR x482), ("body", cvtSTMT x483)]))
     | cvtSTMT (TryStmt{body=x493, catches=ls501, finally=opt506}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x493), ("catches", PrettyRep.List (List.map (fn {bind=x494, 
                                                                                                    body=x495} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x494), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x495)]
                                                                                             ) ls501)), 
          ("finally", 
       (case opt506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x505 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x505))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls520, cases=ls525}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x519 => 
                                                                        cvtEXPR x519
                                                                 ) ls520)), 
          ("cases", PrettyRep.List (List.map (fn x524 => cvtCASE x524
                                             ) ls525))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls537, ty=x541, cases=ls543}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x536 => 
                                                                        cvtEXPR x536
                                                                 ) ls537)), 
          ("ty", cvtTYPE_EXPR x541), ("cases", PrettyRep.List (List.map (fn x542 => 
                                                                               cvtTYPE_CASE x542
                                                                        ) ls543))]))
     | cvtSTMT (Dxns{expr=x556}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x556)]))
   and cvtEXPR (TrinaryExpr(x562, x563, x564, x565)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x562, cvtEXPR x563, cvtEXPR x564, 
          cvtEXPR x565]))
     | cvtEXPR (BinaryExpr(x569, x570, x571)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x569, cvtEXPR x570, cvtEXPR x571]))
     | cvtEXPR (BinaryTypeExpr(x575, x576, x577)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x575, cvtEXPR x576, cvtTYPE_EXPR x577]))
     | cvtEXPR (UnaryExpr(x581, x582)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x581, 
          cvtEXPR x582]))
     | cvtEXPR (TypeExpr x586) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x586))
     | cvtEXPR (NullaryExpr x589) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x589))
     | cvtEXPR (YieldExpr opt597) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls593 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x592 => 
                                                                                     cvtEXPR x592
                                                                              ) ls593)))
       ))
     | cvtEXPR (SuperExpr opt604) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x603 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x603))
       ))
     | cvtEXPR (LiteralExpr x610) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x610))
     | cvtEXPR (CallExpr{func=x613, actuals=ls615}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x613), ("actuals", PrettyRep.List (List.map (fn x614 => 
                                                                                                   cvtEXPR x614
                                                                                            ) ls615))]))
     | cvtEXPR (ApplyTypeExpr{expr=x626, actuals=ls628}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x626), ("actuals", PrettyRep.List (List.map (fn x627 => 
                                                                                                   cvtTYPE_EXPR x627
                                                                                            ) ls628))]))
     | cvtEXPR (LetExpr{defs=ls640, body=ls645, fixtures=x649}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x639 => 
                                                                        cvtVAR_BINDING x639
                                                                 ) ls640)), 
          ("body", PrettyRep.List (List.map (fn x644 => cvtEXPR x644
                                            ) ls645)), ("fixtures", cvtFIXTURES x649)]))
     | cvtEXPR (NewExpr{obj=x659, actuals=ls661}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x659), ("actuals", PrettyRep.List (List.map (fn x660 => 
                                                                                                  cvtEXPR x660
                                                                                           ) ls661))]))
     | cvtEXPR (FunExpr{ident=opt673, fsig=x677, body=x678, fixtures=x679}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt673 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x672 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x672))
       )), ("fsig", cvtFUNC_SIG x677), ("body", cvtBLOCK x678), ("fixtures", 
          cvtFIXTURES x679)]))
     | cvtEXPR (ObjectRef{base=x691, ident=x692}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x691), ("ident", cvtIDENT_EXPR x692)]))
     | cvtEXPR (LexicalRef{ident=x700}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x700)]))
     | cvtEXPR (SetExpr(x706, x707, x708)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x706, 
          cvtPATTERN x707, cvtEXPR x708]))
     | cvtEXPR (ListExpr ls713) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x712 => 
                                                                                                    cvtEXPR x712
                                                                                             ) ls713)))
     | cvtEXPR (SliceExpr(ls720, ls725, ls730)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x719 => cvtEXPR x719
                                                          ) ls720), PrettyRep.List (List.map (fn x724 => 
                                                                                                    cvtEXPR x724
                                                                                             ) ls725), 
          PrettyRep.List (List.map (fn x729 => cvtEXPR x729
                                   ) ls730)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x737, ident=x738}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x737), ("ident", cvtUSTRING x738)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x746, expr=x747}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x746), ("expr", cvtEXPR x747)]))
     | cvtIDENT_EXPR (AttributeIdentifier x755) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x755))
     | cvtIDENT_EXPR (Identifier{ident=x758, openNamespaces=ls760}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x758), ("openNamespaces", 
          PrettyRep.List (List.map (fn x759 => cvtNAMESPACE x759
                                   ) ls760))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x771) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x771))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x774, typeParams=ls776}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x774), ("typeParams", 
          PrettyRep.List (List.map (fn x775 => cvtTYPE_EXPR x775
                                   ) ls776))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r789) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r789))
     | cvtLITERAL (LiteralBoolean b792) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b792))
     | cvtLITERAL (LiteralString x795) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x795))
     | cvtLITERAL (LiteralArray{exprs=ls799, ty=opt804}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x798 => 
                                                                         cvtEXPR x798
                                                                  ) ls799)), 
          ("ty", 
       (case opt804 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x803 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x803))
       ))]))
     | cvtLITERAL (LiteralXML ls816) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x815 => 
                                                                                                           cvtEXPR x815
                                                                                                    ) ls816)))
     | cvtLITERAL (LiteralNamespace x822) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x822))
     | cvtLITERAL (LiteralObject{expr=ls826, ty=opt831}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x825 => 
                                                                        cvtFIELD x825
                                                                 ) ls826)), 
          ("ty", 
       (case opt831 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x830 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x830))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x842}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x842)]))
   and cvtBLOCK (Block x848) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x848))
   and cvtPATTERN (ObjectPattern ls858) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x851, ptrn=x852} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x851), ("ptrn", 
                                                cvtPATTERN x852)]
                                         ) ls858)))
     | cvtPATTERN (ArrayPattern ls865) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x864 => 
                                                                                                               cvtPATTERN x864
                                                                                                        ) ls865)))
     | cvtPATTERN (SimplePattern x871) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x871))
     | cvtPATTERN (IdentifierPattern x874) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x874))
   and cvtFIXTURE (NamespaceFixture x877) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x877))
     | cvtFIXTURE (ClassFixture x880) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x880))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x884) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x884))
     | cvtFIXTURE (ValFixture{ty=x887, readOnly=b888, isOverride=b889}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x887), ("readOnly", PrettyRep.Bool b888), 
          ("isOverride", PrettyRep.Bool b889)]))
     | cvtFIXTURE (VirtualValFixture{getter=opt900, setter=opt905}) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (PrettyRep.Rec [("getter", 
       (case opt900 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x899 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x899))
       )), ("setter", 
       (case opt905 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x904 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x904))
       ))]))
   and cvtFIELD {kind=x916, name=x917, init=x918} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x916), ("name", cvtIDENT_EXPR x917), ("init", cvtEXPR x918)]
   and cvtFIELD_TYPE {name=x926, ty=x927} = PrettyRep.Rec [("name", cvtIDENT_EXPR x926), 
          ("ty", cvtTYPE_EXPR x927)]
   and cvtTYPED_IDENT {name=x933, ty=opt935} = PrettyRep.Rec [("name", cvtIDENT x933), 
          ("ty", 
       (case opt935 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x934 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x934))
       ))]
   and cvtFUNC_DEFN {attrs=x944, kind=x945, func=x946} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x944), ("kind", cvtVAR_DEFN_TAG x945), ("func", cvtFUNC x946)]
   and cvtFIXTURES ls957 = PrettyRep.List (List.map (fn (x954, x955) => PrettyRep.Tuple [cvtNAME x954, 
                                                           cvtFIXTURE x955]
                                                    ) ls957)
   and cvtNAMESPACE_DEFN {attrs=x961, ident=x962, init=opt964} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x961), ("ident", cvtIDENT x962), ("init", 
       (case opt964 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x963 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x963))
       ))]
   and cvtCLASS_DEFN {name=x975, nonnullable=b976, attrs=x977, params=ls979, 
          extends=opt984, implements=ls989, classFixtures=x993, instanceFixtures=x994, 
          body=x995, protoVars=ls997, protoMethods=ls1002, instanceVars=ls1007, 
          instanceMethods=ls1012, vars=ls1017, methods=ls1022, constructor=opt1027, 
          initializer=ls1032} = PrettyRep.Rec [("name", cvtIDENT x975), ("nonnullable", 
          PrettyRep.Bool b976), ("attrs", cvtATTRIBUTES x977), ("params", PrettyRep.List (List.map (fn x978 => 
                                                                                                          cvtIDENT x978
                                                                                                   ) ls979)), 
          ("extends", 
       (case opt984 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x983 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x983))
       )), ("implements", PrettyRep.List (List.map (fn x988 => cvtIDENT_EXPR x988
                                                   ) ls989)), ("classFixtures", 
          cvtFIXTURES x993), ("instanceFixtures", cvtFIXTURES x994), ("body", 
          cvtBLOCK x995), ("protoVars", PrettyRep.List (List.map (fn x996 => 
                                                                        cvtVAR_BINDING x996
                                                                 ) ls997)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1001 => cvtFUNC_DEFN x1001
                                                    ) ls1002)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1006 => cvtVAR_BINDING x1006
                                   ) ls1007)), ("instanceMethods", PrettyRep.List (List.map (fn x1011 => 
                                                                                                   cvtFUNC_DEFN x1011
                                                                                            ) ls1012)), 
          ("vars", PrettyRep.List (List.map (fn x1016 => cvtVAR_BINDING x1016
                                            ) ls1017)), ("methods", PrettyRep.List (List.map (fn x1021 => 
                                                                                                    cvtFUNC_DEFN x1021
                                                                                             ) ls1022)), 
          ("constructor", 
       (case opt1027 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1026 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1026))
       )), ("initializer", PrettyRep.List (List.map (fn x1031 => cvtSTMT x1031
                                                    ) ls1032))]
   and cvtINTERFACE_DEFN {name=x1071, nonnullable=b1072, attrs=x1073, params=ls1075, 
          extends=ls1080, body=x1084} = PrettyRep.Rec [("name", cvtIDENT x1071), 
          ("nonnullable", PrettyRep.Bool b1072), ("attrs", cvtATTRIBUTES x1073), 
          ("params", PrettyRep.List (List.map (fn x1074 => cvtIDENT x1074
                                              ) ls1075)), ("extends", PrettyRep.List (List.map (fn x1079 => 
                                                                                                      cvtIDENT_EXPR x1079
                                                                                               ) ls1080)), 
          ("body", cvtBLOCK x1084)]
   and cvtFOR_ENUM_STMT {ptrn=opt1099, obj=ls1104, defns=ls1109, contLabel=opt1114, 
          body=x1118} = PrettyRep.Rec [("ptrn", 
       (case opt1099 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1098 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1098))
       )), ("obj", PrettyRep.List (List.map (fn x1103 => cvtEXPR x1103
                                            ) ls1104)), ("defns", PrettyRep.List (List.map (fn x1108 => 
                                                                                                  cvtVAR_BINDING x1108
                                                                                           ) ls1109)), 
          ("contLabel", 
       (case opt1114 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1113 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1113))
       )), ("body", cvtSTMT x1118)]
   and cvtWHILE_STMT {cond=x1130, body=x1131, contLabel=opt1133} = PrettyRep.Rec [("cond", 
          cvtEXPR x1130), ("body", cvtSTMT x1131), ("contLabel", 
       (case opt1133 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1132 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1132))
       ))]
   and cvtDIRECTIVES {pragmas=ls1145, defns=ls1150, stmts=ls1155, fixtures=x1159} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1144 => 
                                                                     cvtPRAGMA x1144
                                                              ) ls1145)), ("defns", 
          PrettyRep.List (List.map (fn x1149 => cvtDEFN x1149
                                   ) ls1150)), ("stmts", PrettyRep.List (List.map (fn x1154 => 
                                                                                         cvtSTMT x1154
                                                                                  ) ls1155)), 
          ("fixtures", cvtFIXTURES x1159)]
   and cvtBINDINGS {defns=ls1170, inits=ls1175} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1169 => cvtVAR_BINDING x1169
                                   ) ls1170)), ("inits", PrettyRep.List (List.map (fn x1174 => 
                                                                                         cvtEXPR x1174
                                                                                  ) ls1175))]
   and cvtCASE {label=opt1189, body=x1193} = PrettyRep.Rec [("label", 
       (case opt1189 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1185 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1184 => 
                                                                                      cvtEXPR x1184
                                                                               ) ls1185)))
       )), ("body", cvtBLOCK x1193)]
   and cvtTYPE_CASE {ptrn=opt1200, body=x1204} = PrettyRep.Rec [("ptrn", 
       (case opt1200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1199 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1199))
       )), ("body", cvtBLOCK x1204)]
   and cvtFUNC_NAME {kind=x1210, ident=x1211} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1210), 
          ("ident", cvtIDENT x1211)]
   and cvtPACKAGE {name=x1217, body=x1218} = PrettyRep.Rec [("name", cvtUSTRING x1217), 
          ("body", cvtBLOCK x1218)]
   and cvtPROGRAM {packages=ls1225, body=x1229} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1224 => cvtPACKAGE x1224
                                   ) ls1225)), ("body", cvtBLOCK x1229)]
end

