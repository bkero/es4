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
     | cvtDEFN (NamespaceDefn{attrs=x175, ident=x176, init=opt178}) = PrettyRep.Ctor ("NamespaceDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x175), ("ident", cvtIDENT x176), 
          ("init", 
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x177))
       ))]))
     | cvtDEFN (TypeDefn{attrs=x191, ident=x192, init=x193}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x191), ("ident", cvtIDENT x192), 
          ("init", cvtTYPE_EXPR x193)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls204, params=ls209, inits=opt214, 
          returnType=x218, thisType=opt220, hasBoundThis=b224, hasRest=b225}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x203 => cvtIDENT x203
                                   ) ls204)), ("params", PrettyRep.List (List.map (fn x208 => 
                                                                                         cvtVAR_BINDING x208
                                                                                  ) ls209)), 
          ("inits", 
       (case opt214 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x213 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x213))
       )), ("returnType", cvtTYPE_EXPR x218), ("thisType", 
       (case opt220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x219 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x219))
       )), ("hasBoundThis", PrettyRep.Bool b224), ("hasRest", PrettyRep.Bool b225)]))
   and cvtATTRIBUTES (Attributes{ns=x243, override=b244, static=b245, final=b246, 
          dynamic=b247, prototype=b248, native=b249, rest=b250}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x243), ("override", PrettyRep.Bool b244), 
          ("static", PrettyRep.Bool b245), ("final", PrettyRep.Bool b246), 
          ("dynamic", PrettyRep.Bool b247), ("prototype", PrettyRep.Bool b248), 
          ("native", PrettyRep.Bool b249), ("rest", PrettyRep.Bool b250)]))
   and cvtVAR_BINDING (Binding{kind=x270, init=opt272, attrs=x276, pattern=x277, 
          ty=opt279}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x270), ("init", 
       (case opt272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x271 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x271))
       )), ("attrs", cvtATTRIBUTES x276), ("pattern", cvtPATTERN x277), ("ty", 
          
       (case opt279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x278 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x278))
       ))]))
   and cvtTYPE_EXPR (SpecialType x296) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x296))
     | cvtTYPE_EXPR (UnionType ls300) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x299 => 
                                                                                                           cvtTYPE_EXPR x299
                                                                                                    ) ls300)))
     | cvtTYPE_EXPR (ArrayType ls307) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x306 => 
                                                                                                           cvtTYPE_EXPR x306
                                                                                                    ) ls307)))
     | cvtTYPE_EXPR (NominalType{ident=x313, nullable=opt315}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x313), ("nullable", 
          
       (case opt315 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b314 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b314))
       ))]))
     | cvtTYPE_EXPR (FunctionType x326) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x326))
     | cvtTYPE_EXPR (ObjectType ls330) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x329 => 
                                                                                                             cvtFIELD_TYPE x329
                                                                                                      ) ls330)))
     | cvtTYPE_EXPR (AppType{base=x336, args=ls338}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x336), ("args", PrettyRep.List (List.map (fn x337 => 
                                                                                                     cvtTYPE_EXPR x337
                                                                                              ) ls338))]))
     | cvtTYPE_EXPR (NullableType{expr=x349, nullable=b350}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x349), ("nullable", PrettyRep.Bool b350)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls360) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x359 => 
                                                                                                    cvtEXPR x359
                                                                                             ) ls360)))
     | cvtSTMT (ForEachStmt x366) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x366))
     | cvtSTMT (ForInStmt x369) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x369))
     | cvtSTMT (ThrowStmt ls373) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x372 => 
                                                                                                      cvtEXPR x372
                                                                                               ) ls373)))
     | cvtSTMT (ReturnStmt ls380) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x379 => 
                                                                                                        cvtEXPR x379
                                                                                                 ) ls380)))
     | cvtSTMT (BreakStmt opt387) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt387 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x386 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x386))
       ))
     | cvtSTMT (ContinueStmt opt394) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x393 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x393))
       ))
     | cvtSTMT (BlockStmt x400) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x400))
     | cvtSTMT (LabeledStmt(x403, x404)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x403, 
          cvtSTMT x404]))
     | cvtSTMT (LetStmt(ls409, x413)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x408 => 
                                                                                                                          cvtVAR_BINDING x408
                                                                                                                   ) ls409), 
          cvtSTMT x413]))
     | cvtSTMT (SuperStmt ls418) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x417 => 
                                                                                                      cvtEXPR x417
                                                                                               ) ls418)))
     | cvtSTMT (WhileStmt x424) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x424))
     | cvtSTMT (DoWhileStmt x427) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x427))
     | cvtSTMT (ForStmt{defns=ls431, init=ls436, cond=ls441, update=ls446, 
          contLabel=opt451, body=x455}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x430 => cvtVAR_BINDING x430
                                   ) ls431)), ("init", PrettyRep.List (List.map (fn x435 => 
                                                                                       cvtEXPR x435
                                                                                ) ls436)), 
          ("cond", PrettyRep.List (List.map (fn x440 => cvtEXPR x440
                                            ) ls441)), ("update", PrettyRep.List (List.map (fn x445 => 
                                                                                                  cvtEXPR x445
                                                                                           ) ls446)), 
          ("contLabel", 
       (case opt451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x450 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x450))
       )), ("body", cvtSTMT x455)]))
     | cvtSTMT (IfStmt{cnd=x471, thn=x472, els=x473}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x471), ("thn", cvtSTMT x472), 
          ("els", cvtSTMT x473)]))
     | cvtSTMT (WithStmt{obj=ls484, ty=x488, body=x489}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x483 => 
                                                                       cvtEXPR x483
                                                                ) ls484)), 
          ("ty", cvtTYPE_EXPR x488), ("body", cvtSTMT x489)]))
     | cvtSTMT (TryStmt{body=x499, catches=ls507, finally=opt512}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x499), ("catches", PrettyRep.List (List.map (fn {bind=x500, 
                                                                                                    body=x501} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x500), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x501)]
                                                                                             ) ls507)), 
          ("finally", 
       (case opt512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x511 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x511))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls526, cases=ls531}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x525 => 
                                                                        cvtEXPR x525
                                                                 ) ls526)), 
          ("cases", PrettyRep.List (List.map (fn x530 => cvtCASE x530
                                             ) ls531))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls543, ty=x547, cases=ls549}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x542 => 
                                                                        cvtEXPR x542
                                                                 ) ls543)), 
          ("ty", cvtTYPE_EXPR x547), ("cases", PrettyRep.List (List.map (fn x548 => 
                                                                               cvtTYPE_CASE x548
                                                                        ) ls549))]))
     | cvtSTMT (Dxns{expr=x562}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x562)]))
   and cvtEXPR (TrinaryExpr(x568, x569, x570, x571)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x568, cvtEXPR x569, cvtEXPR x570, 
          cvtEXPR x571]))
     | cvtEXPR (BinaryExpr(x575, x576, x577)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x575, cvtEXPR x576, cvtEXPR x577]))
     | cvtEXPR (BinaryTypeExpr(x581, x582, x583)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x581, cvtEXPR x582, cvtTYPE_EXPR x583]))
     | cvtEXPR (UnaryExpr(x587, x588)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x587, 
          cvtEXPR x588]))
     | cvtEXPR (TypeExpr x592) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x592))
     | cvtEXPR (NullaryExpr x595) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x595))
     | cvtEXPR (YieldExpr opt603) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls599 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x598 => 
                                                                                     cvtEXPR x598
                                                                              ) ls599)))
       ))
     | cvtEXPR (SuperExpr opt610) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x609 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x609))
       ))
     | cvtEXPR (LiteralExpr x616) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x616))
     | cvtEXPR (CallExpr{func=x619, actuals=ls621}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x619), ("actuals", PrettyRep.List (List.map (fn x620 => 
                                                                                                   cvtEXPR x620
                                                                                            ) ls621))]))
     | cvtEXPR (ApplyTypeExpr{expr=x632, actuals=ls634}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x632), ("actuals", PrettyRep.List (List.map (fn x633 => 
                                                                                                   cvtTYPE_EXPR x633
                                                                                            ) ls634))]))
     | cvtEXPR (LetExpr{defs=ls646, body=ls651}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x645 => 
                                                                        cvtVAR_BINDING x645
                                                                 ) ls646)), 
          ("body", PrettyRep.List (List.map (fn x650 => cvtEXPR x650
                                            ) ls651))]))
     | cvtEXPR (NewExpr{obj=x662, actuals=ls664}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x662), ("actuals", PrettyRep.List (List.map (fn x663 => 
                                                                                                  cvtEXPR x663
                                                                                           ) ls664))]))
     | cvtEXPR (FunExpr{ident=opt676, fsig=x680, body=x681}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x675 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x675))
       )), ("fsig", cvtFUNC_SIG x680), ("body", cvtBLOCK x681)]))
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
   and cvtFIXTURES_TAG (ClassFixtures) = PrettyRep.Ctor ("ClassFixtures", NONE)
     | cvtFIXTURES_TAG (FrameFixtures) = PrettyRep.Ctor ("FrameFixtures", NONE)
     | cvtFIXTURES_TAG (GlobalFixtures) = PrettyRep.Ctor ("GlobalFixtures", 
          NONE)
   and cvtFIXTURES (Fixtures{tag=x880, bindings=x881, isExtensible=b882, openNamespaces=ls884, 
          numberType=x888, roundingMode=x889}) = PrettyRep.Ctor ("Fixtures", 
          SOME (PrettyRep.Rec [("tag", cvtFIXTURES_TAG x880), ("bindings", 
          cvtFIXTURE_BINDINGS x881), ("isExtensible", PrettyRep.Bool b882), 
          ("openNamespaces", PrettyRep.List (List.map (fn x883 => cvtNAMESPACE x883
                                                      ) ls884)), ("numberType", 
          cvtNUMBER_TYPE x888), ("roundingMode", cvtROUNDING_MODE x889)]))
   and cvtFIXTURE (NamespaceFixture x905) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x905))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x909) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x909))
     | cvtFIXTURE (PropFixture{ty=x912, readOnly=b913, isOverride=b914}) = 
          PrettyRep.Ctor ("PropFixture", SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x912), 
          ("readOnly", PrettyRep.Bool b913), ("isOverride", PrettyRep.Bool b914)]))
   and cvtFIELD {kind=x924, name=x925, init=x926} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x924), ("name", cvtIDENT_EXPR x925), ("init", cvtEXPR x926)]
   and cvtFIELD_TYPE {name=x934, ty=x935} = PrettyRep.Rec [("name", cvtIDENT_EXPR x934), 
          ("ty", cvtTYPE_EXPR x935)]
   and cvtTYPED_IDENT {name=x941, ty=opt943} = PrettyRep.Rec [("name", cvtIDENT x941), 
          ("ty", 
       (case opt943 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x942 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x942))
       ))]
   and cvtFUNC_DEFN {attrs=x952, kind=x953, func=x954} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x952), ("kind", cvtVAR_DEFN_TAG x953), ("func", cvtFUNC x954)]
   and cvtFIXTURE_BINDINGS ls965 = PrettyRep.List (List.map (fn (x962, x963) => 
                                                                   PrettyRep.Tuple [cvtNAME x962, 
                                                                   cvtFIXTURE x963]
                                                            ) ls965)
   and cvtCLASS_DEFN {name=x969, nonnullable=b970, attrs=x971, params=ls973, 
          extends=opt978, implements=ls983, classFixtures=opt988, instanceFixtures=opt993, 
          body=x997, instanceVars=ls999, instanceMethods=ls1004, vars=ls1009, 
          methods=ls1014, constructor=opt1019, initializer=ls1024} = PrettyRep.Rec [("name", 
          cvtIDENT x969), ("nonnullable", PrettyRep.Bool b970), ("attrs", cvtATTRIBUTES x971), 
          ("params", PrettyRep.List (List.map (fn x972 => cvtIDENT x972
                                              ) ls973)), ("extends", 
       (case opt978 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x977 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x977))
       )), ("implements", PrettyRep.List (List.map (fn x982 => cvtIDENT_EXPR x982
                                                   ) ls983)), ("classFixtures", 
          
       (case opt988 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x987 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x987))
       )), ("instanceFixtures", 
       (case opt993 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x992 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x992))
       )), ("body", cvtBLOCK x997), ("instanceVars", PrettyRep.List (List.map (fn x998 => 
                                                                                     cvtVAR_BINDING x998
                                                                              ) ls999)), 
          ("instanceMethods", PrettyRep.List (List.map (fn x1003 => cvtFUNC x1003
                                                       ) ls1004)), ("vars", 
          PrettyRep.List (List.map (fn x1008 => cvtVAR_BINDING x1008
                                   ) ls1009)), ("methods", PrettyRep.List (List.map (fn x1013 => 
                                                                                           cvtFUNC x1013
                                                                                    ) ls1014)), 
          ("constructor", 
       (case opt1019 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1018 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1018))
       )), ("initializer", PrettyRep.List (List.map (fn x1023 => cvtSTMT x1023
                                                    ) ls1024))]
   and cvtINTERFACE_DEFN {name=x1059, nonnullable=b1060, attrs=x1061, params=ls1063, 
          extends=ls1068, body=x1072} = PrettyRep.Rec [("name", cvtIDENT x1059), 
          ("nonnullable", PrettyRep.Bool b1060), ("attrs", cvtATTRIBUTES x1061), 
          ("params", PrettyRep.List (List.map (fn x1062 => cvtIDENT x1062
                                              ) ls1063)), ("extends", PrettyRep.List (List.map (fn x1067 => 
                                                                                                      cvtIDENT_EXPR x1067
                                                                                               ) ls1068)), 
          ("body", cvtBLOCK x1072)]
   and cvtFOR_ENUM_STMT {ptrn=opt1087, obj=ls1092, defns=ls1097, contLabel=opt1102, 
          body=x1106} = PrettyRep.Rec [("ptrn", 
       (case opt1087 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1086 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1086))
       )), ("obj", PrettyRep.List (List.map (fn x1091 => cvtEXPR x1091
                                            ) ls1092)), ("defns", PrettyRep.List (List.map (fn x1096 => 
                                                                                                  cvtVAR_BINDING x1096
                                                                                           ) ls1097)), 
          ("contLabel", 
       (case opt1102 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1101 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1101))
       )), ("body", cvtSTMT x1106)]
   and cvtWHILE_STMT {cond=x1118, body=x1119, contLabel=opt1121} = PrettyRep.Rec [("cond", 
          cvtEXPR x1118), ("body", cvtSTMT x1119), ("contLabel", 
       (case opt1121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1120 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1120))
       ))]
   and cvtDIRECTIVES {pragmas=ls1133, defns=ls1138, stmts=ls1143, fixtures=opt1148} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1132 => 
                                                                     cvtPRAGMA x1132
                                                              ) ls1133)), ("defns", 
          PrettyRep.List (List.map (fn x1137 => cvtDEFN x1137
                                   ) ls1138)), ("stmts", PrettyRep.List (List.map (fn x1142 => 
                                                                                         cvtSTMT x1142
                                                                                  ) ls1143)), 
          ("fixtures", 
       (case opt1148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1147 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1147))
       ))]
   and cvtBINDINGS {defns=ls1162, inits=ls1167} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1161 => cvtVAR_BINDING x1161
                                   ) ls1162)), ("inits", PrettyRep.List (List.map (fn x1166 => 
                                                                                         cvtEXPR x1166
                                                                                  ) ls1167))]
   and cvtCASE {label=opt1181, body=x1185} = PrettyRep.Rec [("label", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1177 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1176 => 
                                                                                      cvtEXPR x1176
                                                                               ) ls1177)))
       )), ("body", cvtBLOCK x1185)]
   and cvtTYPE_CASE {ptrn=opt1192, body=x1196} = PrettyRep.Rec [("ptrn", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1191))
       )), ("body", cvtBLOCK x1196)]
   and cvtFUNC_NAME {kind=x1202, ident=x1203} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1202), 
          ("ident", cvtIDENT x1203)]
   and cvtPACKAGE {name=x1209, body=x1210} = PrettyRep.Rec [("name", cvtUSTRING x1209), 
          ("body", cvtBLOCK x1210)]
   and cvtPROGRAM {packages=ls1217, body=x1221} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1216 => cvtPACKAGE x1216
                                   ) ls1217)), ("body", cvtBLOCK x1221)]
end

