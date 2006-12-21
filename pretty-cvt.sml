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
   and cvtFUNC (Func{name=x140, fsig=x141, body=x142}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x140), ("fsig", cvtFUNC_SIG x141), 
          ("body", cvtBLOCK x142)]))
   and cvtDEFN (ClassDefn x152) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x152))
     | cvtDEFN (VariableDefn ls156) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x155 => 
                                                                                                            cvtVAR_BINDING x155
                                                                                                     ) ls156)))
     | cvtDEFN (FunctionDefn x162) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x162))
     | cvtDEFN (InterfaceDefn x165) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x165))
     | cvtDEFN (NamespaceDefn{attrs=x168, ident=x169, init=opt171}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x168), ("ident", cvtIDENT x169), 
          ("init", 
       (case opt171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x170 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x170))
       ))]))
     | cvtDEFN (TypeDefn{attrs=x184, ident=x185, init=x186}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x184), ("ident", cvtIDENT x185), 
          ("init", cvtTYPE_EXPR x186)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls197, params=ls202, inits=opt207, 
          returnType=x211, thisType=opt213, hasBoundThis=b217, hasRest=b218}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x196 => cvtIDENT x196
                                   ) ls197)), ("params", PrettyRep.List (List.map (fn x201 => 
                                                                                         cvtVAR_BINDING x201
                                                                                  ) ls202)), 
          ("inits", 
       (case opt207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x206 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x206))
       )), ("returnType", cvtTYPE_EXPR x211), ("thisType", 
       (case opt213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x212 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x212))
       )), ("hasBoundThis", PrettyRep.Bool b217), ("hasRest", PrettyRep.Bool b218)]))
   and cvtATTRIBUTES (Attributes{ns=x236, override=b237, static=b238, final=b239, 
          dynamic=b240, prototype=b241, native=b242, rest=b243}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x236), ("override", PrettyRep.Bool b237), 
          ("static", PrettyRep.Bool b238), ("final", PrettyRep.Bool b239), 
          ("dynamic", PrettyRep.Bool b240), ("prototype", PrettyRep.Bool b241), 
          ("native", PrettyRep.Bool b242), ("rest", PrettyRep.Bool b243)]))
   and cvtVAR_BINDING (Binding{kind=x263, init=opt265, attrs=x269, pattern=x270, 
          ty=opt272}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x263), ("init", 
       (case opt265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x264 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x264))
       )), ("attrs", cvtATTRIBUTES x269), ("pattern", cvtPATTERN x270), ("ty", 
          
       (case opt272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x271 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x271))
       ))]))
   and cvtTYPE_EXPR (SpecialType x289) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x289))
     | cvtTYPE_EXPR (UnionType ls293) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x292 => 
                                                                                                           cvtTYPE_EXPR x292
                                                                                                    ) ls293)))
     | cvtTYPE_EXPR (ArrayType ls300) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x299 => 
                                                                                                           cvtTYPE_EXPR x299
                                                                                                    ) ls300)))
     | cvtTYPE_EXPR (NominalType{ident=x306, nullable=opt308}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x306), ("nullable", 
          
       (case opt308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b307 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b307))
       ))]))
     | cvtTYPE_EXPR (FunctionType x319) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x319))
     | cvtTYPE_EXPR (ObjectType ls323) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x322 => 
                                                                                                             cvtFIELD_TYPE x322
                                                                                                      ) ls323)))
     | cvtTYPE_EXPR (AppType{base=x329, args=ls331}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x329), ("args", PrettyRep.List (List.map (fn x330 => 
                                                                                                     cvtTYPE_EXPR x330
                                                                                              ) ls331))]))
     | cvtTYPE_EXPR (NullableType{expr=x342, nullable=b343}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x342), ("nullable", PrettyRep.Bool b343)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls353) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x352 => 
                                                                                                    cvtEXPR x352
                                                                                             ) ls353)))
     | cvtSTMT (ForEachStmt x359) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x359))
     | cvtSTMT (ForInStmt x362) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x362))
     | cvtSTMT (ThrowStmt ls366) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x365 => 
                                                                                                      cvtEXPR x365
                                                                                               ) ls366)))
     | cvtSTMT (ReturnStmt ls373) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x372 => 
                                                                                                        cvtEXPR x372
                                                                                                 ) ls373)))
     | cvtSTMT (BreakStmt opt380) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt380 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x379 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x379))
       ))
     | cvtSTMT (ContinueStmt opt387) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x386 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x386))
       ))
     | cvtSTMT (BlockStmt x393) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x393))
     | cvtSTMT (LabeledStmt(x396, x397)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x396, 
          cvtSTMT x397]))
     | cvtSTMT (LetStmt(ls402, x406)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x401 => 
                                                                                                                          cvtVAR_BINDING x401
                                                                                                                   ) ls402), 
          cvtSTMT x406]))
     | cvtSTMT (SuperStmt ls411) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x410 => 
                                                                                                      cvtEXPR x410
                                                                                               ) ls411)))
     | cvtSTMT (WhileStmt x417) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x417))
     | cvtSTMT (DoWhileStmt x420) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x420))
     | cvtSTMT (ForStmt{defns=ls424, init=ls429, cond=ls434, update=ls439, 
          contLabel=opt444, body=x448}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x423 => cvtVAR_BINDING x423
                                   ) ls424)), ("init", PrettyRep.List (List.map (fn x428 => 
                                                                                       cvtEXPR x428
                                                                                ) ls429)), 
          ("cond", PrettyRep.List (List.map (fn x433 => cvtEXPR x433
                                            ) ls434)), ("update", PrettyRep.List (List.map (fn x438 => 
                                                                                                  cvtEXPR x438
                                                                                           ) ls439)), 
          ("contLabel", 
       (case opt444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x443 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x443))
       )), ("body", cvtSTMT x448)]))
     | cvtSTMT (IfStmt{cnd=x464, thn=x465, els=x466}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x464), ("thn", cvtSTMT x465), 
          ("els", cvtSTMT x466)]))
     | cvtSTMT (WithStmt{obj=ls477, ty=x481, body=x482}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x476 => 
                                                                       cvtEXPR x476
                                                                ) ls477)), 
          ("ty", cvtTYPE_EXPR x481), ("body", cvtSTMT x482)]))
     | cvtSTMT (TryStmt{body=x492, catches=ls500, finally=opt505}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x492), ("catches", PrettyRep.List (List.map (fn {bind=x493, 
                                                                                                    body=x494} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x493), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x494)]
                                                                                             ) ls500)), 
          ("finally", 
       (case opt505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x504 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x504))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls519, cases=ls524}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x518 => 
                                                                        cvtEXPR x518
                                                                 ) ls519)), 
          ("cases", PrettyRep.List (List.map (fn x523 => cvtCASE x523
                                             ) ls524))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls536, ty=x540, cases=ls542}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x535 => 
                                                                        cvtEXPR x535
                                                                 ) ls536)), 
          ("ty", cvtTYPE_EXPR x540), ("cases", PrettyRep.List (List.map (fn x541 => 
                                                                               cvtTYPE_CASE x541
                                                                        ) ls542))]))
     | cvtSTMT (Dxns{expr=x555}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x555)]))
   and cvtEXPR (TrinaryExpr(x561, x562, x563, x564)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x561, cvtEXPR x562, cvtEXPR x563, 
          cvtEXPR x564]))
     | cvtEXPR (BinaryExpr(x568, x569, x570)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x568, cvtEXPR x569, cvtEXPR x570]))
     | cvtEXPR (BinaryTypeExpr(x574, x575, x576)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x574, cvtEXPR x575, cvtTYPE_EXPR x576]))
     | cvtEXPR (UnaryExpr(x580, x581)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x580, 
          cvtEXPR x581]))
     | cvtEXPR (TypeExpr x585) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x585))
     | cvtEXPR (NullaryExpr x588) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x588))
     | cvtEXPR (YieldExpr opt596) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls592 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x591 => 
                                                                                     cvtEXPR x591
                                                                              ) ls592)))
       ))
     | cvtEXPR (SuperExpr opt603) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x602 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x602))
       ))
     | cvtEXPR (LiteralExpr x609) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x609))
     | cvtEXPR (CallExpr{func=x612, actuals=ls614}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x612), ("actuals", PrettyRep.List (List.map (fn x613 => 
                                                                                                   cvtEXPR x613
                                                                                            ) ls614))]))
     | cvtEXPR (ApplyTypeExpr{expr=x625, actuals=ls627}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x625), ("actuals", PrettyRep.List (List.map (fn x626 => 
                                                                                                   cvtTYPE_EXPR x626
                                                                                            ) ls627))]))
     | cvtEXPR (LetExpr{defs=ls639, body=ls644}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x638 => 
                                                                        cvtVAR_BINDING x638
                                                                 ) ls639)), 
          ("body", PrettyRep.List (List.map (fn x643 => cvtEXPR x643
                                            ) ls644))]))
     | cvtEXPR (NewExpr{obj=x655, actuals=ls657}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x655), ("actuals", PrettyRep.List (List.map (fn x656 => 
                                                                                                  cvtEXPR x656
                                                                                           ) ls657))]))
     | cvtEXPR (FunExpr{ident=opt669, fsig=x673, body=x674}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x668))
       )), ("fsig", cvtFUNC_SIG x673), ("body", cvtBLOCK x674)]))
     | cvtEXPR (ObjectRef{base=x684, ident=x685}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x684), ("ident", cvtIDENT_EXPR x685)]))
     | cvtEXPR (LexicalRef{ident=x693}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x693)]))
     | cvtEXPR (SetExpr(x699, x700, x701)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x699, 
          cvtPATTERN x700, cvtEXPR x701]))
     | cvtEXPR (ListExpr ls706) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x705 => 
                                                                                                    cvtEXPR x705
                                                                                             ) ls706)))
     | cvtEXPR (SliceExpr(ls713, ls718, ls723)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x712 => cvtEXPR x712
                                                          ) ls713), PrettyRep.List (List.map (fn x717 => 
                                                                                                    cvtEXPR x717
                                                                                             ) ls718), 
          PrettyRep.List (List.map (fn x722 => cvtEXPR x722
                                   ) ls723)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x730, ident=x731}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x730), ("ident", cvtUSTRING x731)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x739, expr=x740}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x739), ("expr", cvtEXPR x740)]))
     | cvtIDENT_EXPR (AttributeIdentifier x748) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x748))
     | cvtIDENT_EXPR (Identifier{ident=x751, openNamespaces=ls753}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x751), ("openNamespaces", 
          PrettyRep.List (List.map (fn x752 => cvtNAMESPACE x752
                                   ) ls753))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x764) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x764))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x767, typeParams=ls769}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x767), ("typeParams", 
          PrettyRep.List (List.map (fn x768 => cvtTYPE_EXPR x768
                                   ) ls769))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r782) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r782))
     | cvtLITERAL (LiteralBoolean b785) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b785))
     | cvtLITERAL (LiteralString x788) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x788))
     | cvtLITERAL (LiteralArray{exprs=ls792, ty=opt797}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x791 => 
                                                                         cvtEXPR x791
                                                                  ) ls792)), 
          ("ty", 
       (case opt797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x796 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x796))
       ))]))
     | cvtLITERAL (LiteralXML ls809) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x808 => 
                                                                                                           cvtEXPR x808
                                                                                                    ) ls809)))
     | cvtLITERAL (LiteralNamespace x815) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x815))
     | cvtLITERAL (LiteralObject{expr=ls819, ty=opt824}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x818 => 
                                                                        cvtFIELD x818
                                                                 ) ls819)), 
          ("ty", 
       (case opt824 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x823 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x823))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x835}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x835)]))
   and cvtBLOCK (Block x841) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x841))
   and cvtPATTERN (ObjectPattern ls851) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x844, ptrn=x845} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x844), ("ptrn", 
                                                cvtPATTERN x845)]
                                         ) ls851)))
     | cvtPATTERN (ArrayPattern ls858) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x857 => 
                                                                                                               cvtPATTERN x857
                                                                                                        ) ls858)))
     | cvtPATTERN (SimplePattern x864) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x864))
     | cvtPATTERN (IdentifierPattern x867) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x867))
   and cvtFIXTURES_TAG (ClassFixtures) = PrettyRep.Ctor ("ClassFixtures", NONE)
     | cvtFIXTURES_TAG (FrameFixtures) = PrettyRep.Ctor ("FrameFixtures", NONE)
     | cvtFIXTURES_TAG (GlobalFixtures) = PrettyRep.Ctor ("GlobalFixtures", 
          NONE)
   and cvtFIXTURES (Fixtures{tag=x873, bindings=x874, isExtensible=b875}) = 
          PrettyRep.Ctor ("Fixtures", SOME (PrettyRep.Rec [("tag", cvtFIXTURES_TAG x873), 
          ("bindings", cvtFIXTURE_BINDINGS x874), ("isExtensible", PrettyRep.Bool b875)]))
   and cvtFIXTURE (NamespaceFixture x885) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x885))
     | cvtFIXTURE (TypeFixture x888) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x888))
     | cvtFIXTURE (PropFixture{ty=x891, readOnly=b892, isOverride=b893, subFixtures=opt895}) = 
          PrettyRep.Ctor ("PropFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x891), 
          ("readOnly", PrettyRep.Bool b892), ("isOverride", PrettyRep.Bool b893), 
          ("subFixtures", 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x894))
       ))]))
   and cvtFIELD {kind=x910, name=x911, init=x912} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x910), ("name", cvtIDENT_EXPR x911), ("init", cvtEXPR x912)]
   and cvtFIELD_TYPE {name=x920, ty=x921} = PrettyRep.Rec [("name", cvtIDENT_EXPR x920), 
          ("ty", cvtTYPE_EXPR x921)]
   and cvtTYPED_IDENT {name=x927, ty=opt929} = PrettyRep.Rec [("name", cvtIDENT x927), 
          ("ty", 
       (case opt929 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x928 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x928))
       ))]
   and cvtFUNC_DEFN {attrs=x938, kind=x939, func=x940} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x938), ("kind", cvtVAR_DEFN_TAG x939), ("func", cvtFUNC x940)]
   and cvtFIXTURE_BINDINGS ls951 = PrettyRep.List (List.map (fn (x948, x949) => 
                                                                   PrettyRep.Tuple [cvtNAME x948, 
                                                                   cvtFIXTURE x949]
                                                            ) ls951)
   and cvtCLASS_DEFN {name=x955, nonnullable=b956, attrs=x957, params=ls959, 
          extends=opt964, implements=ls969, classFixtures=opt974, instanceFixtures=opt979, 
          body=x983, instanceVars=ls985, instanceMethods=ls990, vars=ls995, 
          methods=ls1000, constructor=opt1005, initializer=ls1010} = PrettyRep.Rec [("name", 
          cvtIDENT x955), ("nonnullable", PrettyRep.Bool b956), ("attrs", cvtATTRIBUTES x957), 
          ("params", PrettyRep.List (List.map (fn x958 => cvtIDENT x958
                                              ) ls959)), ("extends", 
       (case opt964 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x963 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x963))
       )), ("implements", PrettyRep.List (List.map (fn x968 => cvtIDENT_EXPR x968
                                                   ) ls969)), ("classFixtures", 
          
       (case opt974 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x973 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x973))
       )), ("instanceFixtures", 
       (case opt979 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x978 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x978))
       )), ("body", cvtBLOCK x983), ("instanceVars", PrettyRep.List (List.map (fn x984 => 
                                                                                     cvtVAR_BINDING x984
                                                                              ) ls985)), 
          ("instanceMethods", PrettyRep.List (List.map (fn x989 => cvtFUNC x989
                                                       ) ls990)), ("vars", 
          PrettyRep.List (List.map (fn x994 => cvtVAR_BINDING x994
                                   ) ls995)), ("methods", PrettyRep.List (List.map (fn x999 => 
                                                                                          cvtFUNC x999
                                                                                   ) ls1000)), 
          ("constructor", 
       (case opt1005 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1004 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1004))
       )), ("initializer", PrettyRep.List (List.map (fn x1009 => cvtSTMT x1009
                                                    ) ls1010))]
   and cvtINTERFACE_DEFN {name=x1045, nonnullable=b1046, attrs=x1047, params=ls1049, 
          extends=ls1054, body=x1058} = PrettyRep.Rec [("name", cvtIDENT x1045), 
          ("nonnullable", PrettyRep.Bool b1046), ("attrs", cvtATTRIBUTES x1047), 
          ("params", PrettyRep.List (List.map (fn x1048 => cvtIDENT x1048
                                              ) ls1049)), ("extends", PrettyRep.List (List.map (fn x1053 => 
                                                                                                      cvtIDENT_EXPR x1053
                                                                                               ) ls1054)), 
          ("body", cvtBLOCK x1058)]
   and cvtFOR_ENUM_STMT {ptrn=opt1073, obj=ls1078, defns=ls1083, contLabel=opt1088, 
          body=x1092} = PrettyRep.Rec [("ptrn", 
       (case opt1073 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1072 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1072))
       )), ("obj", PrettyRep.List (List.map (fn x1077 => cvtEXPR x1077
                                            ) ls1078)), ("defns", PrettyRep.List (List.map (fn x1082 => 
                                                                                                  cvtVAR_BINDING x1082
                                                                                           ) ls1083)), 
          ("contLabel", 
       (case opt1088 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1087 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1087))
       )), ("body", cvtSTMT x1092)]
   and cvtWHILE_STMT {cond=x1104, body=x1105, contLabel=opt1107} = PrettyRep.Rec [("cond", 
          cvtEXPR x1104), ("body", cvtSTMT x1105), ("contLabel", 
       (case opt1107 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1106 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1106))
       ))]
   and cvtDIRECTIVES {pragmas=ls1119, defns=ls1124, stmts=ls1129, fixtures=opt1134} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1118 => 
                                                                     cvtPRAGMA x1118
                                                              ) ls1119)), ("defns", 
          PrettyRep.List (List.map (fn x1123 => cvtDEFN x1123
                                   ) ls1124)), ("stmts", PrettyRep.List (List.map (fn x1128 => 
                                                                                         cvtSTMT x1128
                                                                                  ) ls1129)), 
          ("fixtures", 
       (case opt1134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1133 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1133))
       ))]
   and cvtBINDINGS {defns=ls1148, inits=ls1153} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1147 => cvtVAR_BINDING x1147
                                   ) ls1148)), ("inits", PrettyRep.List (List.map (fn x1152 => 
                                                                                         cvtEXPR x1152
                                                                                  ) ls1153))]
   and cvtCASE {label=opt1167, body=x1171} = PrettyRep.Rec [("label", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1163 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1162 => 
                                                                                      cvtEXPR x1162
                                                                               ) ls1163)))
       )), ("body", cvtBLOCK x1171)]
   and cvtTYPE_CASE {ptrn=opt1178, body=x1182} = PrettyRep.Rec [("ptrn", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1177))
       )), ("body", cvtBLOCK x1182)]
   and cvtFUNC_NAME {kind=x1188, ident=x1189} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1188), 
          ("ident", cvtIDENT x1189)]
   and cvtPACKAGE {name=x1195, body=x1196} = PrettyRep.Rec [("name", cvtUSTRING x1195), 
          ("body", cvtBLOCK x1196)]
   and cvtPROGRAM {packages=ls1203, body=x1207} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1202 => cvtPACKAGE x1202
                                   ) ls1203)), ("body", cvtBLOCK x1207)]
end

