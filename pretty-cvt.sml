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
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls330) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x329 => 
                                                                                                    cvtEXPR x329
                                                                                             ) ls330)))
     | cvtSTMT (ForEachStmt x336) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x336))
     | cvtSTMT (ForInStmt x339) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x339))
     | cvtSTMT (ThrowStmt ls343) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x342 => 
                                                                                                      cvtEXPR x342
                                                                                               ) ls343)))
     | cvtSTMT (ReturnStmt ls350) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x349 => 
                                                                                                        cvtEXPR x349
                                                                                                 ) ls350)))
     | cvtSTMT (BreakStmt opt357) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x356 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x356))
       ))
     | cvtSTMT (ContinueStmt opt364) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt364 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x363 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x363))
       ))
     | cvtSTMT (BlockStmt x370) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x370))
     | cvtSTMT (LabeledStmt(x373, x374)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x373, 
          cvtSTMT x374]))
     | cvtSTMT (LetStmt(ls379, x383)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x378 => 
                                                                                                                          cvtVAR_BINDING x378
                                                                                                                   ) ls379), 
          cvtSTMT x383]))
     | cvtSTMT (SuperStmt ls388) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x387 => 
                                                                                                      cvtEXPR x387
                                                                                               ) ls388)))
     | cvtSTMT (WhileStmt x394) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x394))
     | cvtSTMT (DoWhileStmt x397) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x397))
     | cvtSTMT (ForStmt{defns=ls401, init=ls406, cond=ls411, update=ls416, 
          contLabel=opt421, body=x425}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x400 => cvtVAR_BINDING x400
                                   ) ls401)), ("init", PrettyRep.List (List.map (fn x405 => 
                                                                                       cvtEXPR x405
                                                                                ) ls406)), 
          ("cond", PrettyRep.List (List.map (fn x410 => cvtEXPR x410
                                            ) ls411)), ("update", PrettyRep.List (List.map (fn x415 => 
                                                                                                  cvtEXPR x415
                                                                                           ) ls416)), 
          ("contLabel", 
       (case opt421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x420 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x420))
       )), ("body", cvtSTMT x425)]))
     | cvtSTMT (IfStmt{cnd=x441, thn=x442, els=x443}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x441), ("thn", cvtSTMT x442), 
          ("els", cvtSTMT x443)]))
     | cvtSTMT (WithStmt{obj=ls454, ty=x458, body=x459}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x453 => 
                                                                       cvtEXPR x453
                                                                ) ls454)), 
          ("ty", cvtTYPE_EXPR x458), ("body", cvtSTMT x459)]))
     | cvtSTMT (TryStmt{body=x469, catches=ls477, finally=opt482}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x469), ("catches", PrettyRep.List (List.map (fn {bind=x470, 
                                                                                                    body=x471} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x470), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x471)]
                                                                                             ) ls477)), 
          ("finally", 
       (case opt482 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x481 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x481))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls496, cases=ls501}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x495 => 
                                                                        cvtEXPR x495
                                                                 ) ls496)), 
          ("cases", PrettyRep.List (List.map (fn x500 => cvtCASE x500
                                             ) ls501))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls513, ty=x517, cases=ls519}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x512 => 
                                                                        cvtEXPR x512
                                                                 ) ls513)), 
          ("ty", cvtTYPE_EXPR x517), ("cases", PrettyRep.List (List.map (fn x518 => 
                                                                               cvtTYPE_CASE x518
                                                                        ) ls519))]))
     | cvtSTMT (Dxns{expr=x532}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x532)]))
   and cvtEXPR (TrinaryExpr(x538, x539, x540, x541)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x538, cvtEXPR x539, cvtEXPR x540, 
          cvtEXPR x541]))
     | cvtEXPR (BinaryExpr(x545, x546, x547)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x545, cvtEXPR x546, cvtEXPR x547]))
     | cvtEXPR (BinaryTypeExpr(x551, x552, x553)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x551, cvtEXPR x552, cvtTYPE_EXPR x553]))
     | cvtEXPR (UnaryExpr(x557, x558)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x557, 
          cvtEXPR x558]))
     | cvtEXPR (TypeExpr x562) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x562))
     | cvtEXPR (NullaryExpr x565) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x565))
     | cvtEXPR (YieldExpr opt573) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls569 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x568 => 
                                                                                     cvtEXPR x568
                                                                              ) ls569)))
       ))
     | cvtEXPR (SuperExpr opt580) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt580 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x579 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x579))
       ))
     | cvtEXPR (LiteralExpr x586) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x586))
     | cvtEXPR (CallExpr{func=x589, actuals=ls591}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x589), ("actuals", PrettyRep.List (List.map (fn x590 => 
                                                                                                   cvtEXPR x590
                                                                                            ) ls591))]))
     | cvtEXPR (LetExpr{defs=ls603, body=ls608}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x602 => 
                                                                        cvtVAR_BINDING x602
                                                                 ) ls603)), 
          ("body", PrettyRep.List (List.map (fn x607 => cvtEXPR x607
                                            ) ls608))]))
     | cvtEXPR (NewExpr{obj=x619, actuals=ls621}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x619), ("actuals", PrettyRep.List (List.map (fn x620 => 
                                                                                                  cvtEXPR x620
                                                                                           ) ls621))]))
     | cvtEXPR (FunExpr{ident=opt633, fsig=x637, body=x638}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x632 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x632))
       )), ("fsig", cvtFUNC_SIG x637), ("body", cvtBLOCK x638)]))
     | cvtEXPR (ObjectRef{base=x648, ident=x649}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x648), ("ident", cvtIDENT_EXPR x649)]))
     | cvtEXPR (LexicalRef{ident=x657}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x657)]))
     | cvtEXPR (SetExpr(x663, x664, x665)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x663, 
          cvtPATTERN x664, cvtEXPR x665]))
     | cvtEXPR (ListExpr ls670) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x669 => 
                                                                                                    cvtEXPR x669
                                                                                             ) ls670)))
     | cvtEXPR (SliceExpr(ls677, ls682, ls687)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x676 => cvtEXPR x676
                                                          ) ls677), PrettyRep.List (List.map (fn x681 => 
                                                                                                    cvtEXPR x681
                                                                                             ) ls682), 
          PrettyRep.List (List.map (fn x686 => cvtEXPR x686
                                   ) ls687)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x694, ident=x695}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x694), ("ident", cvtUSTRING x695)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x703, expr=x704}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x703), ("expr", cvtEXPR x704)]))
     | cvtIDENT_EXPR (AttributeIdentifier x712) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x712))
     | cvtIDENT_EXPR (Identifier{ident=x715, openNamespaces=r721}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x715), ("openNamespaces", 
          
       (case ! r721 of
         ls717 => PrettyRep.Ref (PrettyRep.List (List.map (fn x716 => cvtNAMESPACE x716
                                                          ) ls717))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x732) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x732))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x735, typeParams=ls737}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x735), ("typeParams", 
          PrettyRep.List (List.map (fn x736 => cvtTYPE_EXPR x736
                                   ) ls737))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r750) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r750))
     | cvtLITERAL (LiteralBoolean b753) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b753))
     | cvtLITERAL (LiteralString x756) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x756))
     | cvtLITERAL (LiteralArray{exprs=ls760, ty=opt765}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x759 => 
                                                                         cvtEXPR x759
                                                                  ) ls760)), 
          ("ty", 
       (case opt765 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x764 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x764))
       ))]))
     | cvtLITERAL (LiteralXML ls777) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x776 => 
                                                                                                           cvtEXPR x776
                                                                                                    ) ls777)))
     | cvtLITERAL (LiteralNamespace x783) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x783))
     | cvtLITERAL (LiteralObject{expr=ls787, ty=opt792}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x786 => 
                                                                        cvtFIELD x786
                                                                 ) ls787)), 
          ("ty", 
       (case opt792 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x791 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x791))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x803}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x803)]))
   and cvtBLOCK (Block{pragmas=ls810, defns=ls815, stmts=ls820}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x809 => 
                                                                           cvtPRAGMA x809
                                                                    ) ls810)), 
          ("defns", PrettyRep.List (List.map (fn x814 => cvtDEFN x814
                                             ) ls815)), ("stmts", PrettyRep.List (List.map (fn x819 => 
                                                                                                  cvtSTMT x819
                                                                                           ) ls820))]))
   and cvtPATTERN (ObjectPattern ls840) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x833, ptrn=x834} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x833), ("ptrn", 
                                                cvtPATTERN x834)]
                                         ) ls840)))
     | cvtPATTERN (ArrayPattern ls847) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x846 => 
                                                                                                               cvtPATTERN x846
                                                                                                        ) ls847)))
     | cvtPATTERN (SimplePattern x853) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x853))
     | cvtPATTERN (IdentifierPattern x856) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x856))
   and cvtFIELD {kind=x859, name=x860, init=x861} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x859), ("name", cvtIDENT_EXPR x860), ("init", cvtEXPR x861)]
   and cvtFIELD_TYPE {name=x869, ty=x870} = PrettyRep.Rec [("name", cvtIDENT_EXPR x869), 
          ("ty", cvtTYPE_EXPR x870)]
   and cvtTYPED_IDENT {name=x876, ty=opt878} = PrettyRep.Rec [("name", cvtIDENT x876), 
          ("ty", 
       (case opt878 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x877 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x877))
       ))]
   and cvtFUNC_DEFN {attrs=x887, kind=x888, func=x889} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x887), ("kind", cvtVAR_DEFN_TAG x888), ("func", cvtFUNC x889)]
   and cvtCLASS_DEFN {name=x897, nonnullable=b898, attrs=x899, params=ls901, 
          extends=opt906, implements=ls911, body=x915, instanceVars=ls917, 
          instanceMethods=ls922, vars=ls927, methods=ls932, constructor=opt937, 
          initializer=ls942} = PrettyRep.Rec [("name", cvtIDENT x897), ("nonnullable", 
          PrettyRep.Bool b898), ("attrs", cvtATTRIBUTES x899), ("params", PrettyRep.List (List.map (fn x900 => 
                                                                                                          cvtIDENT x900
                                                                                                   ) ls901)), 
          ("extends", 
       (case opt906 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x905 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x905))
       )), ("implements", PrettyRep.List (List.map (fn x910 => cvtIDENT_EXPR x910
                                                   ) ls911)), ("body", cvtBLOCK x915), 
          ("instanceVars", PrettyRep.List (List.map (fn x916 => cvtVAR_BINDING x916
                                                    ) ls917)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x921 => cvtFUNC x921
                                   ) ls922)), ("vars", PrettyRep.List (List.map (fn x926 => 
                                                                                       cvtVAR_BINDING x926
                                                                                ) ls927)), 
          ("methods", PrettyRep.List (List.map (fn x931 => cvtFUNC x931
                                               ) ls932)), ("constructor", 
       (case opt937 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x936 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x936))
       )), ("initializer", PrettyRep.List (List.map (fn x941 => cvtSTMT x941
                                                    ) ls942))]
   and cvtINTERFACE_DEFN {name=x973, nonnullable=b974, attrs=x975, params=ls977, 
          extends=ls982, body=x986} = PrettyRep.Rec [("name", cvtIDENT x973), 
          ("nonnullable", PrettyRep.Bool b974), ("attrs", cvtATTRIBUTES x975), 
          ("params", PrettyRep.List (List.map (fn x976 => cvtIDENT x976
                                              ) ls977)), ("extends", PrettyRep.List (List.map (fn x981 => 
                                                                                                     cvtIDENT_EXPR x981
                                                                                              ) ls982)), 
          ("body", cvtBLOCK x986)]
   and cvtFOR_ENUM_STMT {ptrn=opt1001, obj=ls1006, defns=ls1011, contLabel=opt1016, 
          body=x1020} = PrettyRep.Rec [("ptrn", 
       (case opt1001 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1000 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1000))
       )), ("obj", PrettyRep.List (List.map (fn x1005 => cvtEXPR x1005
                                            ) ls1006)), ("defns", PrettyRep.List (List.map (fn x1010 => 
                                                                                                  cvtVAR_BINDING x1010
                                                                                           ) ls1011)), 
          ("contLabel", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1015))
       )), ("body", cvtSTMT x1020)]
   and cvtWHILE_STMT {cond=x1032, body=x1033, contLabel=opt1035} = PrettyRep.Rec [("cond", 
          cvtEXPR x1032), ("body", cvtSTMT x1033), ("contLabel", 
       (case opt1035 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1034 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1034))
       ))]
   and cvtDIRECTIVES {pragmas=ls1047, defns=ls1052, stmts=ls1057} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1046 => cvtPRAGMA x1046
                                   ) ls1047)), ("defns", PrettyRep.List (List.map (fn x1051 => 
                                                                                         cvtDEFN x1051
                                                                                  ) ls1052)), 
          ("stmts", PrettyRep.List (List.map (fn x1056 => cvtSTMT x1056
                                             ) ls1057))]
   and cvtBINDINGS {defns=ls1069, inits=ls1074} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1068 => cvtVAR_BINDING x1068
                                   ) ls1069)), ("inits", PrettyRep.List (List.map (fn x1073 => 
                                                                                         cvtEXPR x1073
                                                                                  ) ls1074))]
   and cvtCASE {label=opt1088, stmts=x1092} = PrettyRep.Rec [("label", 
       (case opt1088 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1084 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1083 => 
                                                                                      cvtEXPR x1083
                                                                               ) ls1084)))
       )), ("stmts", cvtDIRECTIVES x1092)]
   and cvtTYPE_CASE {ptrn=opt1099, body=x1103} = PrettyRep.Rec [("ptrn", 
       (case opt1099 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1098 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1098))
       )), ("body", cvtBLOCK x1103)]
   and cvtFUNC_NAME {kind=x1109, ident=x1110} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1109), 
          ("ident", cvtIDENT x1110)]
   and cvtPACKAGE {name=x1116, body=x1117} = PrettyRep.Rec [("name", cvtUSTRING x1116), 
          ("body", cvtBLOCK x1117)]
   and cvtPROGRAM {packages=ls1124, body=x1128} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1123 => cvtPACKAGE x1123
                                   ) ls1124)), ("body", cvtBLOCK x1128)]
end

