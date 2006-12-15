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
     | cvtDEFN (NamespaceDefn{name=x161, init=x162}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x161), ("init", cvtEXPR x162)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls171, params=ls176, returnType=x180, 
          thisType=opt182, hasBoundThis=b186, hasRest=b187}) = PrettyRep.Ctor ("FunctionSignature", 
          SOME (PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x170 => 
                                                                              cvtIDENT x170
                                                                       ) ls171)), 
          ("params", PrettyRep.List (List.map (fn x175 => cvtVAR_BINDING x175
                                              ) ls176)), ("returnType", cvtTYPE_EXPR x180), 
          ("thisType", 
       (case opt182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x181 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x181))
       )), ("hasBoundThis", PrettyRep.Bool b186), ("hasRest", PrettyRep.Bool b187)]))
   and cvtATTRIBUTES (Attributes{ns=x203, override=b204, static=b205, final=b206, 
          dynamic=b207, prototype=b208, native=b209, rest=b210}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x203), ("override", PrettyRep.Bool b204), 
          ("static", PrettyRep.Bool b205), ("final", PrettyRep.Bool b206), 
          ("dynamic", PrettyRep.Bool b207), ("prototype", PrettyRep.Bool b208), 
          ("native", PrettyRep.Bool b209), ("rest", PrettyRep.Bool b210)]))
   and cvtVAR_BINDING (Binding{kind=x230, init=opt232, attrs=x236, pattern=x237, 
          ty=opt239}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x230), ("init", 
       (case opt232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x231 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x231))
       )), ("attrs", cvtATTRIBUTES x236), ("pattern", cvtPATTERN x237), ("ty", 
          
       (case opt239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x238 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x238))
       ))]))
   and cvtTYPE_EXPR (SpecialType x256) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x256))
     | cvtTYPE_EXPR (UnionType ls260) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x259 => 
                                                                                                           cvtTYPE_EXPR x259
                                                                                                    ) ls260)))
     | cvtTYPE_EXPR (ArrayType ls267) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x266 => 
                                                                                                           cvtTYPE_EXPR x266
                                                                                                    ) ls267)))
     | cvtTYPE_EXPR (NominalType{ident=x273, nullable=opt275}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x273), ("nullable", 
          
       (case opt275 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b274 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b274))
       ))]))
     | cvtTYPE_EXPR (FunctionType x286) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x286))
     | cvtTYPE_EXPR (ObjectType ls290) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x289 => 
                                                                                                             cvtFIELD_TYPE x289
                                                                                                      ) ls290)))
     | cvtTYPE_EXPR (AppType{base=x296, args=ls298}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x296), ("args", PrettyRep.List (List.map (fn x297 => 
                                                                                                     cvtTYPE_EXPR x297
                                                                                              ) ls298))]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls311) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x310 => 
                                                                                                    cvtEXPR x310
                                                                                             ) ls311)))
     | cvtSTMT (ForEachStmt x317) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x317))
     | cvtSTMT (ForInStmt x320) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x320))
     | cvtSTMT (ThrowStmt ls324) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x323 => 
                                                                                                      cvtEXPR x323
                                                                                               ) ls324)))
     | cvtSTMT (ReturnStmt ls331) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x330 => 
                                                                                                        cvtEXPR x330
                                                                                                 ) ls331)))
     | cvtSTMT (BreakStmt opt338) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x337 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x337))
       ))
     | cvtSTMT (ContinueStmt opt345) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x344 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x344))
       ))
     | cvtSTMT (BlockStmt x351) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x351))
     | cvtSTMT (LabeledStmt(x354, x355)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x354, 
          cvtSTMT x355]))
     | cvtSTMT (LetStmt(ls360, x364)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x359 => 
                                                                                                                          cvtVAR_BINDING x359
                                                                                                                   ) ls360), 
          cvtSTMT x364]))
     | cvtSTMT (SuperStmt ls369) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x368 => 
                                                                                                      cvtEXPR x368
                                                                                               ) ls369)))
     | cvtSTMT (WhileStmt x375) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x375))
     | cvtSTMT (DoWhileStmt x378) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x378))
     | cvtSTMT (ForStmt{defns=ls382, init=ls387, cond=ls392, update=ls397, 
          contLabel=opt402, body=x406}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x381 => cvtVAR_BINDING x381
                                   ) ls382)), ("init", PrettyRep.List (List.map (fn x386 => 
                                                                                       cvtEXPR x386
                                                                                ) ls387)), 
          ("cond", PrettyRep.List (List.map (fn x391 => cvtEXPR x391
                                            ) ls392)), ("update", PrettyRep.List (List.map (fn x396 => 
                                                                                                  cvtEXPR x396
                                                                                           ) ls397)), 
          ("contLabel", 
       (case opt402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x401 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x401))
       )), ("body", cvtSTMT x406)]))
     | cvtSTMT (IfStmt{cnd=x422, thn=x423, els=x424}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x422), ("thn", cvtSTMT x423), 
          ("els", cvtSTMT x424)]))
     | cvtSTMT (WithStmt{obj=ls435, ty=x439, body=x440}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x434 => 
                                                                       cvtEXPR x434
                                                                ) ls435)), 
          ("ty", cvtTYPE_EXPR x439), ("body", cvtSTMT x440)]))
     | cvtSTMT (TryStmt{body=x450, catches=ls458, finally=opt463}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x450), ("catches", PrettyRep.List (List.map (fn {bind=x451, 
                                                                                                    body=x452} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x451), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x452)]
                                                                                             ) ls458)), 
          ("finally", 
       (case opt463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x462 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x462))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls477, cases=ls482}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x476 => 
                                                                        cvtEXPR x476
                                                                 ) ls477)), 
          ("cases", PrettyRep.List (List.map (fn x481 => cvtCASE x481
                                             ) ls482))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls494, ty=x498, cases=ls500}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x493 => 
                                                                        cvtEXPR x493
                                                                 ) ls494)), 
          ("ty", cvtTYPE_EXPR x498), ("cases", PrettyRep.List (List.map (fn x499 => 
                                                                               cvtTYPE_CASE x499
                                                                        ) ls500))]))
     | cvtSTMT (Dxns{expr=x513}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x513)]))
   and cvtEXPR (TrinaryExpr(x519, x520, x521, x522)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x519, cvtEXPR x520, cvtEXPR x521, 
          cvtEXPR x522]))
     | cvtEXPR (BinaryExpr(x526, x527, x528)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x526, cvtEXPR x527, cvtEXPR x528]))
     | cvtEXPR (BinaryTypeExpr(x532, x533, x534)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x532, cvtEXPR x533, cvtTYPE_EXPR x534]))
     | cvtEXPR (UnaryExpr(x538, x539)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x538, 
          cvtEXPR x539]))
     | cvtEXPR (TypeExpr x543) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x543))
     | cvtEXPR (NullaryExpr x546) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x546))
     | cvtEXPR (YieldExpr opt554) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls550 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x549 => 
                                                                                     cvtEXPR x549
                                                                              ) ls550)))
       ))
     | cvtEXPR (SuperExpr opt561) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x560 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x560))
       ))
     | cvtEXPR (LiteralExpr x567) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x567))
     | cvtEXPR (CallExpr{func=x570, actuals=ls572}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x570), ("actuals", PrettyRep.List (List.map (fn x571 => 
                                                                                                   cvtEXPR x571
                                                                                            ) ls572))]))
     | cvtEXPR (LetExpr{defs=ls584, body=ls589}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x583 => 
                                                                        cvtVAR_BINDING x583
                                                                 ) ls584)), 
          ("body", PrettyRep.List (List.map (fn x588 => cvtEXPR x588
                                            ) ls589))]))
     | cvtEXPR (NewExpr{obj=x600, actuals=ls602}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x600), ("actuals", PrettyRep.List (List.map (fn x601 => 
                                                                                                  cvtEXPR x601
                                                                                           ) ls602))]))
     | cvtEXPR (FunExpr{ident=opt614, fsig=x618, body=x619}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x613 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x613))
       )), ("fsig", cvtFUNC_SIG x618), ("body", cvtBLOCK x619)]))
     | cvtEXPR (ObjectRef{base=x629, ident=x630}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x629), ("ident", cvtIDENT_EXPR x630)]))
     | cvtEXPR (LexicalRef{ident=x638}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x638)]))
     | cvtEXPR (SetExpr(x644, x645, x646)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x644, 
          cvtPATTERN x645, cvtEXPR x646]))
     | cvtEXPR (ListExpr ls651) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x650 => 
                                                                                                    cvtEXPR x650
                                                                                             ) ls651)))
     | cvtEXPR (SliceExpr(ls658, ls663, ls668)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x657 => cvtEXPR x657
                                                          ) ls658), PrettyRep.List (List.map (fn x662 => 
                                                                                                    cvtEXPR x662
                                                                                             ) ls663), 
          PrettyRep.List (List.map (fn x667 => cvtEXPR x667
                                   ) ls668)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x675, ident=x676}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x675), ("ident", cvtUSTRING x676)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x684, expr=x685}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x684), ("expr", cvtEXPR x685)]))
     | cvtIDENT_EXPR (AttributeIdentifier x693) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x693))
     | cvtIDENT_EXPR (Identifier{ident=x696, openNamespaces=r702}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x696), ("openNamespaces", 
          
       (case ! r702 of
         ls698 => PrettyRep.Ref (PrettyRep.List (List.map (fn x697 => cvtNAMESPACE x697
                                                          ) ls698))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x713) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x713))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x716, typeParams=ls718}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x716), ("typeParams", 
          PrettyRep.List (List.map (fn x717 => cvtTYPE_EXPR x717
                                   ) ls718))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r731) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r731))
     | cvtLITERAL (LiteralBoolean b734) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b734))
     | cvtLITERAL (LiteralString x737) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x737))
     | cvtLITERAL (LiteralArray{exprs=ls741, ty=opt746}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x740 => 
                                                                         cvtEXPR x740
                                                                  ) ls741)), 
          ("ty", 
       (case opt746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x745 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x745))
       ))]))
     | cvtLITERAL (LiteralXML ls758) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x757 => 
                                                                                                           cvtEXPR x757
                                                                                                    ) ls758)))
     | cvtLITERAL (LiteralNamespace x764) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x764))
     | cvtLITERAL (LiteralObject{expr=ls768, ty=opt773}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x767 => 
                                                                        cvtFIELD x767
                                                                 ) ls768)), 
          ("ty", 
       (case opt773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x772 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x772))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x784}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x784)]))
   and cvtBLOCK (Block{pragmas=ls791, defns=ls796, stmts=ls801}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x790 => 
                                                                           cvtPRAGMA x790
                                                                    ) ls791)), 
          ("defns", PrettyRep.List (List.map (fn x795 => cvtDEFN x795
                                             ) ls796)), ("stmts", PrettyRep.List (List.map (fn x800 => 
                                                                                                  cvtSTMT x800
                                                                                           ) ls801))]))
   and cvtPATTERN (ObjectPattern ls821) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x814, ptrn=x815} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x814), ("ptrn", 
                                                cvtPATTERN x815)]
                                         ) ls821)))
     | cvtPATTERN (ArrayPattern ls828) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x827 => 
                                                                                                               cvtPATTERN x827
                                                                                                        ) ls828)))
     | cvtPATTERN (SimplePattern x834) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x834))
     | cvtPATTERN (IdentifierPattern x837) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x837))
   and cvtFIELD {kind=x840, name=x841, init=x842} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x840), ("name", cvtIDENT_EXPR x841), ("init", cvtEXPR x842)]
   and cvtFIELD_TYPE {name=x850, ty=x851} = PrettyRep.Rec [("name", cvtIDENT_EXPR x850), 
          ("ty", cvtTYPE_EXPR x851)]
   and cvtTYPED_IDENT {name=x857, ty=opt859} = PrettyRep.Rec [("name", cvtIDENT x857), 
          ("ty", 
       (case opt859 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x858 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x858))
       ))]
   and cvtFUNC_DEFN {attrs=x868, kind=x869, func=x870} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x868), ("kind", cvtVAR_DEFN_TAG x869), ("func", cvtFUNC x870)]
   and cvtCLASS_DEFN {name=x878, nonnullable=b879, attrs=x880, params=ls882, 
          extends=opt887, implements=ls892, body=x896, instanceVars=ls898, 
          instanceMethods=ls903, vars=ls908, methods=ls913, constructor=opt918, 
          initializer=ls923} = PrettyRep.Rec [("name", cvtIDENT x878), ("nonnullable", 
          PrettyRep.Bool b879), ("attrs", cvtATTRIBUTES x880), ("params", PrettyRep.List (List.map (fn x881 => 
                                                                                                          cvtIDENT x881
                                                                                                   ) ls882)), 
          ("extends", 
       (case opt887 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x886 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x886))
       )), ("implements", PrettyRep.List (List.map (fn x891 => cvtIDENT_EXPR x891
                                                   ) ls892)), ("body", cvtBLOCK x896), 
          ("instanceVars", PrettyRep.List (List.map (fn x897 => cvtVAR_BINDING x897
                                                    ) ls898)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x902 => cvtFUNC x902
                                   ) ls903)), ("vars", PrettyRep.List (List.map (fn x907 => 
                                                                                       cvtVAR_BINDING x907
                                                                                ) ls908)), 
          ("methods", PrettyRep.List (List.map (fn x912 => cvtFUNC x912
                                               ) ls913)), ("constructor", 
       (case opt918 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x917 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x917))
       )), ("initializer", PrettyRep.List (List.map (fn x922 => cvtSTMT x922
                                                    ) ls923))]
   and cvtINTERFACE_DEFN {name=x954, nonnullable=b955, attrs=x956, params=ls958, 
          extends=ls963, body=x967} = PrettyRep.Rec [("name", cvtIDENT x954), 
          ("nonnullable", PrettyRep.Bool b955), ("attrs", cvtATTRIBUTES x956), 
          ("params", PrettyRep.List (List.map (fn x957 => cvtIDENT x957
                                              ) ls958)), ("extends", PrettyRep.List (List.map (fn x962 => 
                                                                                                     cvtIDENT_EXPR x962
                                                                                              ) ls963)), 
          ("body", cvtBLOCK x967)]
   and cvtFOR_ENUM_STMT {ptrn=opt982, obj=ls987, defns=ls992, contLabel=opt997, 
          body=x1001} = PrettyRep.Rec [("ptrn", 
       (case opt982 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x981 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x981))
       )), ("obj", PrettyRep.List (List.map (fn x986 => cvtEXPR x986
                                            ) ls987)), ("defns", PrettyRep.List (List.map (fn x991 => 
                                                                                                 cvtVAR_BINDING x991
                                                                                          ) ls992)), 
          ("contLabel", 
       (case opt997 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x996 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x996))
       )), ("body", cvtSTMT x1001)]
   and cvtWHILE_STMT {cond=x1013, body=x1014, contLabel=opt1016} = PrettyRep.Rec [("cond", 
          cvtEXPR x1013), ("body", cvtSTMT x1014), ("contLabel", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1015))
       ))]
   and cvtDIRECTIVES {pragmas=ls1028, defns=ls1033, stmts=ls1038} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1027 => cvtPRAGMA x1027
                                   ) ls1028)), ("defns", PrettyRep.List (List.map (fn x1032 => 
                                                                                         cvtDEFN x1032
                                                                                  ) ls1033)), 
          ("stmts", PrettyRep.List (List.map (fn x1037 => cvtSTMT x1037
                                             ) ls1038))]
   and cvtBINDINGS {defns=ls1050, inits=ls1055} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1049 => cvtVAR_BINDING x1049
                                   ) ls1050)), ("inits", PrettyRep.List (List.map (fn x1054 => 
                                                                                         cvtEXPR x1054
                                                                                  ) ls1055))]
   and cvtCASE {label=opt1069, stmts=x1073} = PrettyRep.Rec [("label", 
       (case opt1069 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1065 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1064 => 
                                                                                      cvtEXPR x1064
                                                                               ) ls1065)))
       )), ("stmts", cvtDIRECTIVES x1073)]
   and cvtTYPE_CASE {ptrn=opt1080, body=x1084} = PrettyRep.Rec [("ptrn", 
       (case opt1080 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1079 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1079))
       )), ("body", cvtBLOCK x1084)]
   and cvtFUNC_NAME {kind=x1090, ident=x1091} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1090), 
          ("ident", cvtIDENT x1091)]
   and cvtPACKAGE {names=ls1098, fullname=x1102, body=x1103} = PrettyRep.Rec [("names", 
          PrettyRep.List (List.map (fn x1097 => cvtIDENT x1097
                                   ) ls1098)), ("fullname", cvtUSTRING x1102), 
          ("body", cvtBLOCK x1103)]
   and cvtPROGRAM {packages=ls1112, body=x1116} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1111 => cvtPACKAGE x1111
                                   ) ls1112)), ("body", cvtBLOCK x1116)]
end

