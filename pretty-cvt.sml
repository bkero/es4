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
     | cvtEXPR (ApplyTypeExpr{expr=x602, actuals=ls604}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x602), ("actuals", PrettyRep.List (List.map (fn x603 => 
                                                                                                   cvtTYPE_EXPR x603
                                                                                            ) ls604))]))
     | cvtEXPR (LetExpr{defs=ls616, body=ls621}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x615 => 
                                                                        cvtVAR_BINDING x615
                                                                 ) ls616)), 
          ("body", PrettyRep.List (List.map (fn x620 => cvtEXPR x620
                                            ) ls621))]))
     | cvtEXPR (NewExpr{obj=x632, actuals=ls634}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x632), ("actuals", PrettyRep.List (List.map (fn x633 => 
                                                                                                  cvtEXPR x633
                                                                                           ) ls634))]))
     | cvtEXPR (FunExpr{ident=opt646, fsig=x650, body=x651}) = PrettyRep.Ctor ("FunExpr", 
          SOME (PrettyRep.Rec [("ident", 
       (case opt646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x645 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x645))
       )), ("fsig", cvtFUNC_SIG x650), ("body", cvtBLOCK x651)]))
     | cvtEXPR (ObjectRef{base=x661, ident=x662}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x661), ("ident", cvtIDENT_EXPR x662)]))
     | cvtEXPR (LexicalRef{ident=x670}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x670)]))
     | cvtEXPR (SetExpr(x676, x677, x678)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x676, 
          cvtPATTERN x677, cvtEXPR x678]))
     | cvtEXPR (ListExpr ls683) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x682 => 
                                                                                                    cvtEXPR x682
                                                                                             ) ls683)))
     | cvtEXPR (SliceExpr(ls690, ls695, ls700)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x689 => cvtEXPR x689
                                                          ) ls690), PrettyRep.List (List.map (fn x694 => 
                                                                                                    cvtEXPR x694
                                                                                             ) ls695), 
          PrettyRep.List (List.map (fn x699 => cvtEXPR x699
                                   ) ls700)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x707, ident=x708}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x707), ("ident", cvtUSTRING x708)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x716, expr=x717}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x716), ("expr", cvtEXPR x717)]))
     | cvtIDENT_EXPR (AttributeIdentifier x725) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x725))
     | cvtIDENT_EXPR (Identifier{ident=x728, openNamespaces=r734}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x728), ("openNamespaces", 
          
       (case ! r734 of
         ls730 => PrettyRep.Ref (PrettyRep.List (List.map (fn x729 => cvtNAMESPACE x729
                                                          ) ls730))
       ))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x745) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x745))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x748, typeParams=ls750}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x748), ("typeParams", 
          PrettyRep.List (List.map (fn x749 => cvtTYPE_EXPR x749
                                   ) ls750))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r763) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r763))
     | cvtLITERAL (LiteralBoolean b766) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b766))
     | cvtLITERAL (LiteralString x769) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x769))
     | cvtLITERAL (LiteralArray{exprs=ls773, ty=opt778}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x772 => 
                                                                         cvtEXPR x772
                                                                  ) ls773)), 
          ("ty", 
       (case opt778 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x777 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x777))
       ))]))
     | cvtLITERAL (LiteralXML ls790) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x789 => 
                                                                                                           cvtEXPR x789
                                                                                                    ) ls790)))
     | cvtLITERAL (LiteralNamespace x796) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x796))
     | cvtLITERAL (LiteralObject{expr=ls800, ty=opt805}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x799 => 
                                                                        cvtFIELD x799
                                                                 ) ls800)), 
          ("ty", 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x804))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x816}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x816)]))
   and cvtBLOCK (Block{pragmas=ls823, defns=ls828, stmts=ls833}) = PrettyRep.Ctor ("Block", 
          SOME (PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x822 => 
                                                                           cvtPRAGMA x822
                                                                    ) ls823)), 
          ("defns", PrettyRep.List (List.map (fn x827 => cvtDEFN x827
                                             ) ls828)), ("stmts", PrettyRep.List (List.map (fn x832 => 
                                                                                                  cvtSTMT x832
                                                                                           ) ls833))]))
   and cvtPATTERN (ObjectPattern ls853) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x846, ptrn=x847} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x846), ("ptrn", 
                                                cvtPATTERN x847)]
                                         ) ls853)))
     | cvtPATTERN (ArrayPattern ls860) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x859 => 
                                                                                                               cvtPATTERN x859
                                                                                                        ) ls860)))
     | cvtPATTERN (SimplePattern x866) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x866))
     | cvtPATTERN (IdentifierPattern x869) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x869))
   and cvtFIELD {kind=x872, name=x873, init=x874} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x872), ("name", cvtIDENT_EXPR x873), ("init", cvtEXPR x874)]
   and cvtFIELD_TYPE {name=x882, ty=x883} = PrettyRep.Rec [("name", cvtIDENT_EXPR x882), 
          ("ty", cvtTYPE_EXPR x883)]
   and cvtTYPED_IDENT {name=x889, ty=opt891} = PrettyRep.Rec [("name", cvtIDENT x889), 
          ("ty", 
       (case opt891 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x890 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x890))
       ))]
   and cvtFUNC_DEFN {attrs=x900, kind=x901, func=x902} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x900), ("kind", cvtVAR_DEFN_TAG x901), ("func", cvtFUNC x902)]
   and cvtCLASS_DEFN {name=x910, nonnullable=b911, attrs=x912, params=ls914, 
          extends=opt919, implements=ls924, body=x928, instanceVars=ls930, 
          instanceMethods=ls935, vars=ls940, methods=ls945, constructor=opt950, 
          initializer=ls955} = PrettyRep.Rec [("name", cvtIDENT x910), ("nonnullable", 
          PrettyRep.Bool b911), ("attrs", cvtATTRIBUTES x912), ("params", PrettyRep.List (List.map (fn x913 => 
                                                                                                          cvtIDENT x913
                                                                                                   ) ls914)), 
          ("extends", 
       (case opt919 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x918 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x918))
       )), ("implements", PrettyRep.List (List.map (fn x923 => cvtIDENT_EXPR x923
                                                   ) ls924)), ("body", cvtBLOCK x928), 
          ("instanceVars", PrettyRep.List (List.map (fn x929 => cvtVAR_BINDING x929
                                                    ) ls930)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x934 => cvtFUNC x934
                                   ) ls935)), ("vars", PrettyRep.List (List.map (fn x939 => 
                                                                                       cvtVAR_BINDING x939
                                                                                ) ls940)), 
          ("methods", PrettyRep.List (List.map (fn x944 => cvtFUNC x944
                                               ) ls945)), ("constructor", 
       (case opt950 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x949 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x949))
       )), ("initializer", PrettyRep.List (List.map (fn x954 => cvtSTMT x954
                                                    ) ls955))]
   and cvtINTERFACE_DEFN {name=x986, nonnullable=b987, attrs=x988, params=ls990, 
          extends=ls995, body=x999} = PrettyRep.Rec [("name", cvtIDENT x986), 
          ("nonnullable", PrettyRep.Bool b987), ("attrs", cvtATTRIBUTES x988), 
          ("params", PrettyRep.List (List.map (fn x989 => cvtIDENT x989
                                              ) ls990)), ("extends", PrettyRep.List (List.map (fn x994 => 
                                                                                                     cvtIDENT_EXPR x994
                                                                                              ) ls995)), 
          ("body", cvtBLOCK x999)]
   and cvtFOR_ENUM_STMT {ptrn=opt1014, obj=ls1019, defns=ls1024, contLabel=opt1029, 
          body=x1033} = PrettyRep.Rec [("ptrn", 
       (case opt1014 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1013 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1013))
       )), ("obj", PrettyRep.List (List.map (fn x1018 => cvtEXPR x1018
                                            ) ls1019)), ("defns", PrettyRep.List (List.map (fn x1023 => 
                                                                                                  cvtVAR_BINDING x1023
                                                                                           ) ls1024)), 
          ("contLabel", 
       (case opt1029 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1028 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1028))
       )), ("body", cvtSTMT x1033)]
   and cvtWHILE_STMT {cond=x1045, body=x1046, contLabel=opt1048} = PrettyRep.Rec [("cond", 
          cvtEXPR x1045), ("body", cvtSTMT x1046), ("contLabel", 
       (case opt1048 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1047 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1047))
       ))]
   and cvtDIRECTIVES {pragmas=ls1060, defns=ls1065, stmts=ls1070} = PrettyRep.Rec [("pragmas", 
          PrettyRep.List (List.map (fn x1059 => cvtPRAGMA x1059
                                   ) ls1060)), ("defns", PrettyRep.List (List.map (fn x1064 => 
                                                                                         cvtDEFN x1064
                                                                                  ) ls1065)), 
          ("stmts", PrettyRep.List (List.map (fn x1069 => cvtSTMT x1069
                                             ) ls1070))]
   and cvtBINDINGS {defns=ls1082, inits=ls1087} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1081 => cvtVAR_BINDING x1081
                                   ) ls1082)), ("inits", PrettyRep.List (List.map (fn x1086 => 
                                                                                         cvtEXPR x1086
                                                                                  ) ls1087))]
   and cvtCASE {label=opt1101, stmts=x1105} = PrettyRep.Rec [("label", 
       (case opt1101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1097 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1096 => 
                                                                                      cvtEXPR x1096
                                                                               ) ls1097)))
       )), ("stmts", cvtDIRECTIVES x1105)]
   and cvtTYPE_CASE {ptrn=opt1112, body=x1116} = PrettyRep.Rec [("ptrn", 
       (case opt1112 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1111 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1111))
       )), ("body", cvtBLOCK x1116)]
   and cvtFUNC_NAME {kind=x1122, ident=x1123} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1122), 
          ("ident", cvtIDENT x1123)]
   and cvtPACKAGE {name=x1129, body=x1130} = PrettyRep.Rec [("name", cvtUSTRING x1129), 
          ("body", cvtBLOCK x1130)]
   and cvtPROGRAM {packages=ls1137, body=x1141} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1136 => cvtPACKAGE x1136
                                   ) ls1137)), ("body", cvtBLOCK x1141)]
end

