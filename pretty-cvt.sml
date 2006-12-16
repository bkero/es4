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
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x93) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x93))
     | cvtPRAGMA (UseDefaultNamespace x96) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x96))
     | cvtPRAGMA (UseNumber x99) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x99))
     | cvtPRAGMA (UseRounding x102) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x102))
     | cvtPRAGMA (UsePrecision x105) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x105))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x110, name=x111, alias=opt113}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x110), ("name", cvtIDENT x111), 
          ("alias", 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x112))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x133, fsig=x134, body=x135}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x133), ("fsig", cvtFUNC_SIG x134), 
          ("body", cvtBLOCK x135)]))
   and cvtDEFN (ClassDefn x145) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x145))
     | cvtDEFN (VariableDefn ls149) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x148 => 
                                                                                                            cvtVAR_BINDING x148
                                                                                                     ) ls149)))
     | cvtDEFN (FunctionDefn x155) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x155))
     | cvtDEFN (InterfaceDefn x158) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x158))
     | cvtDEFN (NamespaceDefn{attrs=x161, ident=x162, init=opt164}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x161), ("ident", cvtIDENT x162), 
          ("init", 
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x163))
       ))]))
     | cvtDEFN (TypeDefn{attrs=x177, ident=x178, init=x179}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x177), ("ident", cvtIDENT x178), 
          ("init", cvtTYPE_EXPR x179)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls190, params=ls195, returnType=x199, 
          thisType=opt201, hasBoundThis=b205, hasRest=b206}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x189 => 
                                                                              cvtIDENT x189
                                                                       ) ls190)), 
          ("params", PrettyRep.List (List.map (fn x194 => cvtVAR_BINDING x194
                                              ) ls195)), ("returnType", cvtTYPE_EXPR x199), 
          ("thisType", 
       (case opt201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x200 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x200))
       )), ("hasBoundThis", PrettyRep.Bool b205), ("hasRest", PrettyRep.Bool b206)]))
   and cvtATTRIBUTES (Attributes{ns=x222, override=b223, static=b224, final=b225, 
          dynamic=b226, prototype=b227, native=b228, rest=b229}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x222), ("override", PrettyRep.Bool b223), 
          ("static", PrettyRep.Bool b224), ("final", PrettyRep.Bool b225), 
          ("dynamic", PrettyRep.Bool b226), ("prototype", PrettyRep.Bool b227), 
          ("native", PrettyRep.Bool b228), ("rest", PrettyRep.Bool b229)]))
   and cvtVAR_BINDING (Binding{kind=x249, init=opt251, attrs=x255, pattern=x256, 
          ty=opt258}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x249), ("init", 
       (case opt251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x250 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x250))
       )), ("attrs", cvtATTRIBUTES x255), ("pattern", cvtPATTERN x256), ("ty", 
          
       (case opt258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x257 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x257))
       ))]))
   and cvtTYPE_EXPR (SpecialType x275) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x275))
     | cvtTYPE_EXPR (UnionType ls279) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x278 => 
                                                                                                           cvtTYPE_EXPR x278
                                                                                                    ) ls279)))
     | cvtTYPE_EXPR (ArrayType ls286) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x285 => 
                                                                                                           cvtTYPE_EXPR x285
                                                                                                    ) ls286)))
     | cvtTYPE_EXPR (NominalType{ident=x292, nullable=opt294}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x292), ("nullable", 
          
       (case opt294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b293 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b293))
       ))]))
     | cvtTYPE_EXPR (FunctionType x305) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x305))
     | cvtTYPE_EXPR (ObjectType ls309) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x308 => 
                                                                                                             cvtFIELD_TYPE x308
                                                                                                      ) ls309)))
     | cvtTYPE_EXPR (AppType{base=x315, args=ls317}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x315), ("args", PrettyRep.List (List.map (fn x316 => 
                                                                                                     cvtTYPE_EXPR x316
                                                                                              ) ls317))]))
     | cvtTYPE_EXPR (NullableType{expr=x328, nullable=b329}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x328), ("nullable", PrettyRep.Bool b329)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls339) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x338 => 
                                                                                                    cvtEXPR x338
                                                                                             ) ls339)))
     | cvtSTMT (ForEachStmt x345) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x345))
     | cvtSTMT (ForInStmt x348) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x348))
     | cvtSTMT (ThrowStmt ls352) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x351 => 
                                                                                                      cvtEXPR x351
                                                                                               ) ls352)))
     | cvtSTMT (ReturnStmt ls359) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x358 => 
                                                                                                        cvtEXPR x358
                                                                                                 ) ls359)))
     | cvtSTMT (BreakStmt opt366) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt366 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x365 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x365))
       ))
     | cvtSTMT (ContinueStmt opt373) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x372 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x372))
       ))
     | cvtSTMT (BlockStmt x379) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x379))
     | cvtSTMT (LabeledStmt(x382, x383)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x382, 
          cvtSTMT x383]))
     | cvtSTMT (LetStmt(ls388, x392)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x387 => 
                                                                                                                          cvtVAR_BINDING x387
                                                                                                                   ) ls388), 
          cvtSTMT x392]))
     | cvtSTMT (SuperStmt ls397) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x396 => 
                                                                                                      cvtEXPR x396
                                                                                               ) ls397)))
     | cvtSTMT (WhileStmt x403) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x403))
     | cvtSTMT (DoWhileStmt x406) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x406))
     | cvtSTMT (ForStmt{defns=ls410, init=ls415, cond=ls420, update=ls425, 
          contLabel=opt430, body=x434}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x409 => cvtVAR_BINDING x409
                                   ) ls410)), ("init", PrettyRep.List (List.map (fn x414 => 
                                                                                       cvtEXPR x414
                                                                                ) ls415)), 
          ("cond", PrettyRep.List (List.map (fn x419 => cvtEXPR x419
                                            ) ls420)), ("update", PrettyRep.List (List.map (fn x424 => 
                                                                                                  cvtEXPR x424
                                                                                           ) ls425)), 
          ("contLabel", 
       (case opt430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x429 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x429))
       )), ("body", cvtSTMT x434)]))
     | cvtSTMT (IfStmt{cnd=x450, thn=x451, els=x452}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x450), ("thn", cvtSTMT x451), 
          ("els", cvtSTMT x452)]))
     | cvtSTMT (WithStmt{obj=ls463, ty=x467, body=x468}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x462 => 
                                                                       cvtEXPR x462
                                                                ) ls463)), 
          ("ty", cvtTYPE_EXPR x467), ("body", cvtSTMT x468)]))
     | cvtSTMT (TryStmt{body=x478, catches=ls486, finally=opt491}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x478), ("catches", PrettyRep.List (List.map (fn {bind=x479, 
                                                                                                    body=x480} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x479), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x480)]
                                                                                             ) ls486)), 
          ("finally", 
       (case opt491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x490 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x490))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls505, cases=ls510}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x504 => 
                                                                        cvtEXPR x504
                                                                 ) ls505)), 
          ("cases", PrettyRep.List (List.map (fn x509 => cvtCASE x509
                                             ) ls510))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls522, ty=x526, cases=ls528}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x521 => 
                                                                        cvtEXPR x521
                                                                 ) ls522)), 
          ("ty", cvtTYPE_EXPR x526), ("cases", PrettyRep.List (List.map (fn x527 => 
                                                                               cvtTYPE_CASE x527
                                                                        ) ls528))]))
     | cvtSTMT (Dxns{expr=x541}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x541)]))
   and cvtEXPR (TrinaryExpr(x547, x548, x549, x550)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x547, cvtEXPR x548, cvtEXPR x549, 
          cvtEXPR x550]))
     | cvtEXPR (BinaryExpr(x554, x555, x556)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x554, cvtEXPR x555, cvtEXPR x556]))
     | cvtEXPR (BinaryTypeExpr(x560, x561, x562)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x560, cvtEXPR x561, cvtTYPE_EXPR x562]))
     | cvtEXPR (UnaryExpr(x566, x567)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x566, 
          cvtEXPR x567]))
     | cvtEXPR (TypeExpr x571) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x571))
     | cvtEXPR (NullaryExpr x574) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x574))
     | cvtEXPR (YieldExpr opt582) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt582 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls578 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x577 => 
                                                                                     cvtEXPR x577
                                                                              ) ls578)))
       ))
     | cvtEXPR (SuperExpr opt589) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x588 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x588))
       ))
     | cvtEXPR (LiteralExpr x595) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x595))
     | cvtEXPR (CallExpr{func=x598, actuals=ls600}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x598), ("actuals", PrettyRep.List (List.map (fn x599 => 
                                                                                                   cvtEXPR x599
                                                                                            ) ls600))]))
     | cvtEXPR (ApplyTypeExpr{expr=x611, actuals=ls613}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x611), ("actuals", PrettyRep.List (List.map (fn x612 => 
                                                                                                   cvtTYPE_EXPR x612
                                                                                            ) ls613))]))
     | cvtEXPR (LetExpr{defs=ls625, body=ls630}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x624 => 
                                                                        cvtVAR_BINDING x624
                                                                 ) ls625)), 
          ("body", PrettyRep.List (List.map (fn x629 => cvtEXPR x629
                                            ) ls630))]))
     | cvtEXPR (NewExpr{obj=x641, actuals=ls643}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x641), ("actuals", PrettyRep.List (List.map (fn x642 => 
                                                                                                  cvtEXPR x642
                                                                                           ) ls643))]))
     | cvtEXPR (FunExpr{ident=opt655, fsig=x659, body=x660}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x654 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x654))
       )), ("fsig", cvtFUNC_SIG x659), ("body", cvtBLOCK x660)]))
     | cvtEXPR (ObjectRef{base=x670, ident=x671}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x670), ("ident", cvtIDENT_EXPR x671)]))
     | cvtEXPR (LexicalRef{ident=x679}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x679)]))
     | cvtEXPR (SetExpr(x685, x686, x687)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x685, 
          cvtPATTERN x686, cvtEXPR x687]))
     | cvtEXPR (ListExpr ls692) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x691 => 
                                                                                                    cvtEXPR x691
                                                                                             ) ls692)))
     | cvtEXPR (SliceExpr(ls699, ls704, ls709)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x698 => cvtEXPR x698
                                                          ) ls699), PrettyRep.List (List.map (fn x703 => 
                                                                                                    cvtEXPR x703
                                                                                             ) ls704), 
          PrettyRep.List (List.map (fn x708 => cvtEXPR x708
                                   ) ls709)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x716, ident=x717}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x716), ("ident", cvtUSTRING x717)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x725, expr=x726}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x725), ("expr", cvtEXPR x726)]))
     | cvtIDENT_EXPR (AttributeIdentifier x734) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x734))
     | cvtIDENT_EXPR (Identifier{ident=x737, openNamespaces=r743}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x737), ("openNamespaces", 
          
       (case ! r743 of
         ls739 => PrettyRep.Ref (PrettyRep.List (List.map (fn x738 => cvtNAMESPACE x738
                                                          ) ls739))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x754) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x754))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x757, typeParams=ls759}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x757), ("typeParams", 
          PrettyRep.List (List.map (fn x758 => cvtTYPE_EXPR x758
                                   ) ls759))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r772) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r772))
     | cvtLITERAL (LiteralBoolean b775) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b775))
     | cvtLITERAL (LiteralString x778) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x778))
     | cvtLITERAL (LiteralArray{exprs=ls782, ty=opt787}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x781 => 
                                                                         cvtEXPR x781
                                                                  ) ls782)), 
          ("ty", 
       (case opt787 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x786 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x786))
       ))]))
     | cvtLITERAL (LiteralXML ls799) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x798 => 
                                                                                                           cvtEXPR x798
                                                                                                    ) ls799)))
     | cvtLITERAL (LiteralNamespace x805) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x805))
     | cvtLITERAL (LiteralObject{expr=ls809, ty=opt814}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x808 => 
                                                                        cvtFIELD x808
                                                                 ) ls809)), 
          ("ty", 
       (case opt814 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x813 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x813))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x825}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x825)]))
   and cvtBLOCK (Block{pragmas=ls832, defns=ls837, stmts=ls842}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x831 => 
                                                                           cvtPRAGMA x831
                                                                    ) ls832)), 
          ("defns", PrettyRep.List (List.map (fn x836 => cvtDEFN x836
                                             ) ls837)), ("stmts", PrettyRep.List (List.map (fn x841 => 
                                                                                                  cvtSTMT x841
                                                                                           ) ls842))]))
   and cvtPATTERN (ObjectPattern ls862) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x855, ptrn=x856} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x855), ("ptrn", 
                                                cvtPATTERN x856)]
                                         ) ls862)))
     | cvtPATTERN (ArrayPattern ls869) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x868 => 
                                                                                                               cvtPATTERN x868
                                                                                                        ) ls869)))
     | cvtPATTERN (SimplePattern x875) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x875))
     | cvtPATTERN (IdentifierPattern x878) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x878))
   and cvtFIELD {kind=x881, name=x882, init=x883} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x881), ("name", cvtIDENT_EXPR x882), ("init", cvtEXPR x883)]
   and cvtFIELD_TYPE {name=x891, ty=x892} = PrettyRep.Rec [("name", cvtIDENT_EXPR x891), 
          ("ty", cvtTYPE_EXPR x892)]
   and cvtTYPED_IDENT {name=x898, ty=opt900} = PrettyRep.Rec [("name", cvtIDENT x898), 
          ("ty", 
       (case opt900 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x899 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x899))
       ))]
   and cvtFUNC_DEFN {attrs=x909, kind=x910, func=x911} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x909), ("kind", cvtVAR_DEFN_TAG x910), ("func", cvtFUNC x911)]
   and cvtCLASS_DEFN {name=x919, nonnullable=b920, attrs=x921, params=ls923, 
          extends=opt928, implements=ls933, body=x937, instanceVars=ls939, 
          instanceMethods=ls944, vars=ls949, methods=ls954, constructor=opt959, 
          initializer=ls964} = PrettyRep.Rec [("name", cvtIDENT x919), ("nonnullable", 
          PrettyRep.Bool b920), ("attrs", cvtATTRIBUTES x921), ("params", PrettyRep.List (List.map (fn x922 => 
                                                                                                          cvtIDENT x922
                                                                                                   ) ls923)), 
          ("extends", 
       (case opt928 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x927 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x927))
       )), ("implements", PrettyRep.List (List.map (fn x932 => cvtIDENT_EXPR x932
                                                   ) ls933)), ("body", cvtBLOCK x937), 
          ("instanceVars", PrettyRep.List (List.map (fn x938 => cvtVAR_BINDING x938
                                                    ) ls939)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x943 => cvtFUNC x943
                                   ) ls944)), ("vars", PrettyRep.List (List.map (fn x948 => 
                                                                                       cvtVAR_BINDING x948
                                                                                ) ls949)), 
          ("methods", PrettyRep.List (List.map (fn x953 => cvtFUNC x953
                                               ) ls954)), ("constructor", 
       (case opt959 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x958 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x958))
       )), ("initializer", PrettyRep.List (List.map (fn x963 => cvtSTMT x963
                                                    ) ls964))]
   and cvtINTERFACE_DEFN {name=x995, nonnullable=b996, attrs=x997, params=ls999, 
          extends=ls1004, body=x1008} = PrettyRep.Rec [("name", cvtIDENT x995), 
          ("nonnullable", PrettyRep.Bool b996), ("attrs", cvtATTRIBUTES x997), 
          ("params", PrettyRep.List (List.map (fn x998 => cvtIDENT x998
                                              ) ls999)), ("extends", PrettyRep.List (List.map (fn x1003 => 
                                                                                                     cvtIDENT_EXPR x1003
                                                                                              ) ls1004)), 
          ("body", cvtBLOCK x1008)]
   and cvtFOR_ENUM_STMT {ptrn=opt1023, obj=ls1028, defns=ls1033, contLabel=opt1038, 
          body=x1042} = PrettyRep.Rec [("ptrn", 
       (case opt1023 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1022 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1022))
       )), ("obj", PrettyRep.List (List.map (fn x1027 => cvtEXPR x1027
                                            ) ls1028)), ("defns", PrettyRep.List (List.map (fn x1032 => 
                                                                                                  cvtVAR_BINDING x1032
                                                                                           ) ls1033)), 
          ("contLabel", 
       (case opt1038 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1037 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1037))
       )), ("body", cvtSTMT x1042)]
   and cvtWHILE_STMT {cond=x1054, body=x1055, contLabel=opt1057} = PrettyRep.Rec [("cond", 
          cvtEXPR x1054), ("body", cvtSTMT x1055), ("contLabel", 
       (case opt1057 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1056 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1056))
       ))]
   and cvtDIRECTIVES {pragmas=ls1069, defns=ls1074, stmts=ls1079} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1068 => cvtPRAGMA x1068
                                   ) ls1069)), ("defns", PrettyRep.List (List.map (fn x1073 => 
                                                                                         cvtDEFN x1073
                                                                                  ) ls1074)), 
          ("stmts", PrettyRep.List (List.map (fn x1078 => cvtSTMT x1078
                                             ) ls1079))]
   and cvtBINDINGS {defns=ls1091, inits=ls1096} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1090 => cvtVAR_BINDING x1090
                                   ) ls1091)), ("inits", PrettyRep.List (List.map (fn x1095 => 
                                                                                         cvtEXPR x1095
                                                                                  ) ls1096))]
   and cvtCASE {label=opt1110, stmts=x1114} = PrettyRep.Rec [("label", 
       (case opt1110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1106 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1105 => 
                                                                                      cvtEXPR x1105
                                                                               ) ls1106)))
       )), ("stmts", cvtDIRECTIVES x1114)]
   and cvtTYPE_CASE {ptrn=opt1121, body=x1125} = PrettyRep.Rec [("ptrn", 
       (case opt1121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1120 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1120))
       )), ("body", cvtBLOCK x1125)]
   and cvtFUNC_NAME {kind=x1131, ident=x1132} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1131), 
          ("ident", cvtIDENT x1132)]
   and cvtPACKAGE {name=x1138, body=x1139} = PrettyRep.Rec [("name", cvtUSTRING x1138), 
          ("body", cvtBLOCK x1139)]
   and cvtPROGRAM {packages=ls1146, body=x1150} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1145 => cvtPACKAGE x1145
                                   ) ls1146)), ("body", cvtBLOCK x1150)]
end

