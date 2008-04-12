structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtUNIT_NAME ls21 = PrettyRep.List (List.map (fn x20 => cvtIDENT x20
                                                    ) ls21)
   and cvtRIB_ID n25 = PrettyRep.Int n25
   and cvtTYPEVAR_NONCE n26 = PrettyRep.Int n26
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Private x28) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Protected x31) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x31))
     | cvtNAMESPACE (Public x34) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x34))
     | cvtNAMESPACE (Internal x37) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x37))
     | cvtNAMESPACE (UserNamespace s40) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s40))
     | cvtNAMESPACE (AnonUserNamespace n43) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n43))
     | cvtNAMESPACE (LimitedNamespace(x46, x47)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x46, cvtNAMESPACE x47]))
   and cvtNAME {ns=x51, id=x52} = PrettyRep.Rec [("ns", cvtNAMESPACE x51), 
          ("id", cvtIDENT x52)]
   and cvtMULTINAME {nss=ls63, id=x67} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls59 => 
                                                                                                PrettyRep.List (List.map (fn x58 => 
                                                                                                                                cvtNAMESPACE x58
                                                                                                                         ) ls59)
                                                                                         ) ls63)), 
          ("id", cvtIDENT x67)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (Wrap) = PrettyRep.Ctor ("Wrap", NONE)
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
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
     | cvtUNOP (Splat) = PrettyRep.Ctor ("Splat", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x136) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x136))
     | cvtPRAGMA (UseDefaultNamespace x139) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x139))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls145, name=x149}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x144 => 
                                                                           cvtIDENT x144
                                                                    ) ls145)), 
          ("name", cvtIDENT x149)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x163, ribId=opt165}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x163), ("ribId", 
       (case opt165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x164 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x164))
       ))]))
   and cvtCLS (Cls{name=x176, typeParams=ls178, nonnullable=b182, dynamic=b183, 
          extends=opt185, implements=ls190, classRib=x194, instanceRib=x195, 
          instanceInits=x196, constructor=opt198, classType=x202, instanceType=x203}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x176), 
          ("typeParams", PrettyRep.List (List.map (fn x177 => cvtIDENT x177
                                                  ) ls178)), ("nonnullable", 
          PrettyRep.Bool b182), ("dynamic", PrettyRep.Bool b183), ("extends", 
          
       (case opt185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x184 => PrettyRep.Ctor ("SOME", SOME (cvtTY x184))
       )), ("implements", PrettyRep.List (List.map (fn x189 => cvtTY x189
                                                   ) ls190)), ("classRib", 
          cvtRIB x194), ("instanceRib", cvtRIB x195), ("instanceInits", cvtHEAD x196), 
          ("constructor", 
       (case opt198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x197 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x197))
       )), ("classType", cvtTY x202), ("instanceType", cvtTY x203)]))
   and cvtIFACE (Iface{name=x231, typeParams=ls233, nonnullable=b237, extends=ls239, 
          instanceRib=x243, instanceType=x244}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x231), ("typeParams", PrettyRep.List (List.map (fn x232 => 
                                                                                                      cvtIDENT x232
                                                                                               ) ls233)), 
          ("nonnullable", PrettyRep.Bool b237), ("extends", PrettyRep.List (List.map (fn x238 => 
                                                                                            cvtTY x238
                                                                                     ) ls239)), 
          ("instanceRib", cvtRIB x243), ("instanceType", cvtTY x244)]))
   and cvtCTOR (Ctor{settings=x260, superArgs=ls262, func=x266}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x260), ("superArgs", PrettyRep.List (List.map (fn x261 => 
                                                                                                         cvtEXPR x261
                                                                                                  ) ls262)), 
          ("func", cvtFUNC x266)]))
   and cvtFUNC (Func{name=x276, fsig=x277, native=b278, block=opt280, param=x284, 
          defaults=ls286, ty=x290, loc=opt292}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x276), ("fsig", cvtFUNC_SIG x277), ("native", PrettyRep.Bool b278), 
          ("block", 
       (case opt280 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x279 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x279))
       )), ("param", cvtHEAD x284), ("defaults", PrettyRep.List (List.map (fn x285 => 
                                                                                 cvtEXPR x285
                                                                          ) ls286)), 
          ("ty", cvtTY x290), ("loc", 
       (case opt292 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x291 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x291))
       ))]))
   and cvtDEFN (ClassDefn x315) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x315))
     | cvtDEFN (VariableDefn x318) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x318))
     | cvtDEFN (FunctionDefn x321) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x321))
     | cvtDEFN (ConstructorDefn x324) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x324))
     | cvtDEFN (InterfaceDefn x327) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x327))
     | cvtDEFN (NamespaceDefn x330) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x330))
     | cvtDEFN (TypeDefn x333) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x333))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls337, params=x341, paramTypes=ls343, 
          defaults=ls348, ctorInits=opt359, returnType=x363, thisType=opt365, 
          hasRest=b369}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x336 => cvtIDENT x336
                                   ) ls337)), ("params", cvtBINDINGS x341), 
          ("paramTypes", PrettyRep.List (List.map (fn x342 => cvtTYPE_EXPR x342
                                                  ) ls343)), ("defaults", PrettyRep.List (List.map (fn x347 => 
                                                                                                          cvtEXPR x347
                                                                                                   ) ls348)), 
          ("ctorInits", 
       (case opt359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x352, ls354) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x352, 
            PrettyRep.List (List.map (fn x353 => cvtEXPR x353
                                     ) ls354)]))
       )), ("returnType", cvtTYPE_EXPR x363), ("thisType", 
       (case opt365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x364 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x364))
       )), ("hasRest", PrettyRep.Bool b369)]))
   and cvtBINDING (Binding{ident=x389, ty=x390}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x389), ("ty", cvtTYPE_EXPR x390)]))
   and cvtBINDING_IDENT (TempIdent n398) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n398))
     | cvtBINDING_IDENT (ParamIdent n401) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n401))
     | cvtBINDING_IDENT (PropIdent x404) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x404))
   and cvtINIT_STEP (InitStep(x407, x408)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x407, 
          cvtEXPR x408]))
     | cvtINIT_STEP (AssignStep(x412, x413)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x412, cvtEXPR x413]))
   and cvtTYPE_EXPR (SpecialType x417) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x417))
     | cvtTYPE_EXPR (UnionType ls421) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x420 => 
                                                                                                           cvtTYPE_EXPR x420
                                                                                                    ) ls421)))
     | cvtTYPE_EXPR (ArrayType ls428) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x427 => 
                                                                                                           cvtTYPE_EXPR x427
                                                                                                    ) ls428)))
     | cvtTYPE_EXPR (TypeName x434) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x434))
     | cvtTYPE_EXPR (ElementTypeRef(x437, n438)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x437, PrettyRep.Int n438]))
     | cvtTYPE_EXPR (FieldTypeRef(x442, x443)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x442, cvtIDENT x443]))
     | cvtTYPE_EXPR (FunctionType x447) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x447))
     | cvtTYPE_EXPR (ObjectType ls451) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x450 => 
                                                                                                             cvtFIELD_TYPE x450
                                                                                                      ) ls451)))
     | cvtTYPE_EXPR (LikeType x457) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x457))
     | cvtTYPE_EXPR (WrapType x460) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x460))
     | cvtTYPE_EXPR (AppType{base=x463, args=ls465}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x463), ("args", PrettyRep.List (List.map (fn x464 => 
                                                                                                     cvtTYPE_EXPR x464
                                                                                              ) ls465))]))
     | cvtTYPE_EXPR (LamType{params=ls477, body=x481}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x476 => 
                                                                          cvtIDENT x476
                                                                   ) ls477)), 
          ("body", cvtTYPE_EXPR x481)]))
     | cvtTYPE_EXPR (NullableType{expr=x489, nullable=b490}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x489), ("nullable", PrettyRep.Bool b490)]))
     | cvtTYPE_EXPR (InstanceType x498) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x498))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x502) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x502))
     | cvtSTMT (InitStmt{kind=x505, ns=opt507, prototype=b511, static=b512, 
          temps=x513, inits=ls515}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x505), ("ns", 
       (case opt507 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x506 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x506))
       )), ("prototype", PrettyRep.Bool b511), ("static", PrettyRep.Bool b512), 
          ("temps", cvtBINDINGS x513), ("inits", PrettyRep.List (List.map (fn x514 => 
                                                                                 cvtINIT_STEP x514
                                                                          ) ls515))]))
     | cvtSTMT (ClassBlock x534) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x534))
     | cvtSTMT (ForInStmt x537) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x537))
     | cvtSTMT (ThrowStmt x540) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x540))
     | cvtSTMT (ReturnStmt x543) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x543))
     | cvtSTMT (BreakStmt opt547) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x546 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x546))
       ))
     | cvtSTMT (ContinueStmt opt554) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x553 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x553))
       ))
     | cvtSTMT (BlockStmt x560) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x560))
     | cvtSTMT (LabeledStmt(x563, x564)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x563, 
          cvtSTMT x564]))
     | cvtSTMT (LetStmt x568) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x568))
     | cvtSTMT (WhileStmt x571) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x571))
     | cvtSTMT (DoWhileStmt x574) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x574))
     | cvtSTMT (ForStmt x577) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x577))
     | cvtSTMT (IfStmt{cnd=x580, thn=x581, els=x582}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x580), ("thn", cvtSTMT x581), 
          ("els", cvtSTMT x582)]))
     | cvtSTMT (WithStmt{obj=x592, ty=x593, body=x594}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x592), ("ty", cvtTY x593), ("body", 
          cvtSTMT x594)]))
     | cvtSTMT (TryStmt{block=x604, catches=ls606, finally=opt611}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x604), ("catches", PrettyRep.List (List.map (fn x605 => 
                                                                                                     cvtCATCH_CLAUSE x605
                                                                                              ) ls606)), 
          ("finally", 
       (case opt611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x610 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x610))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x624, labels=ls626, cases=ls631}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x624), ("labels", PrettyRep.List (List.map (fn x625 => 
                                                                                                  cvtIDENT x625
                                                                                           ) ls626)), 
          ("cases", PrettyRep.List (List.map (fn x630 => cvtCASE x630
                                             ) ls631))]))
     | cvtSTMT (SwitchTypeStmt{cond=x644, ty=x645, cases=ls647}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x644), ("ty", cvtTY x645), 
          ("cases", PrettyRep.List (List.map (fn x646 => cvtCATCH_CLAUSE x646
                                             ) ls647))]))
     | cvtSTMT (DXNStmt{expr=x660}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x660)]))
   and cvtEXPR (TernaryExpr(x666, x667, x668)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x666, cvtEXPR x667, cvtEXPR x668]))
     | cvtEXPR (BinaryExpr(x672, x673, x674)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x672, cvtEXPR x673, cvtEXPR x674]))
     | cvtEXPR (BinaryTypeExpr(x678, x679, x680)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x678, cvtEXPR x679, cvtTY x680]))
     | cvtEXPR (UnaryExpr(x684, x685)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x684, 
          cvtEXPR x685]))
     | cvtEXPR (TypeExpr x689) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x689))
     | cvtEXPR (ThisExpr opt693) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x692 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x692))
       ))
     | cvtEXPR (YieldExpr opt700) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x699 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x699))
       ))
     | cvtEXPR (SuperExpr opt707) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt707 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x706 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x706))
       ))
     | cvtEXPR (LiteralExpr x713) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x713))
     | cvtEXPR (CallExpr{func=x716, actuals=ls718}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x716), ("actuals", PrettyRep.List (List.map (fn x717 => 
                                                                                                   cvtEXPR x717
                                                                                            ) ls718))]))
     | cvtEXPR (ApplyTypeExpr{expr=x729, actuals=ls731}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x729), ("actuals", PrettyRep.List (List.map (fn x730 => 
                                                                                                   cvtTY x730
                                                                                            ) ls731))]))
     | cvtEXPR (LetExpr{defs=x742, body=x743, head=opt745}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x742), ("body", cvtEXPR x743), 
          ("head", 
       (case opt745 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x744 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x744))
       ))]))
     | cvtEXPR (NewExpr{obj=x758, actuals=ls760}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x758), ("actuals", PrettyRep.List (List.map (fn x759 => 
                                                                                                  cvtEXPR x759
                                                                                           ) ls760))]))
     | cvtEXPR (ObjectRef{base=x771, ident=x772, loc=opt774}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x771), ("ident", cvtIDENT_EXPR x772), 
          ("loc", 
       (case opt774 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x773 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x773))
       ))]))
     | cvtEXPR (LexicalRef{ident=x787, loc=opt789}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x787), ("loc", 
       (case opt789 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x788 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x788))
       ))]))
     | cvtEXPR (SetExpr(x800, x801, x802)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x800, 
          cvtEXPR x801, cvtEXPR x802]))
     | cvtEXPR (ListExpr ls807) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x806 => 
                                                                                                    cvtEXPR x806
                                                                                             ) ls807)))
     | cvtEXPR (InitExpr(x813, x814, x815)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x813, 
          cvtHEAD x814, cvtINITS x815]))
     | cvtEXPR (GetTemp n819) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n819))
     | cvtEXPR (GetParam n822) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n822))
     | cvtEXPR (Comprehension(x825, ls827, opt832)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x825, PrettyRep.List (List.map (fn x826 => 
                                                                               cvtFOR_ENUM_HEAD x826
                                                                        ) ls827), 
          
       (case opt832 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x831 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x831))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n844) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n844))
     | cvtFIXTURE_NAME (PropName x847) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x847))
   and cvtIDENT_EXPR (Identifier{ident=x850, openNamespaces=ls856}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x850), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls852 => PrettyRep.List (List.map (fn x851 => 
                                                                                cvtNAMESPACE x851
                                                                         ) ls852)
                                   ) ls856))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x867, expr=x868}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x867), ("expr", cvtEXPR x868)]))
     | cvtIDENT_EXPR (AttributeIdentifier x876) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x876))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x879, openNamespaces=ls885}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x879), ("openNamespaces", PrettyRep.List (List.map (fn ls881 => 
                                                                            PrettyRep.List (List.map (fn x880 => 
                                                                                                            cvtNAMESPACE x880
                                                                                                     ) ls881)
                                                                     ) ls885))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x896, ident=s897}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x896), ("ident", PrettyRep.UniStr s897)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls906, x910)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x905 => cvtIDENT x905
                                                          ) ls906), cvtIDENT_EXPR x910]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r917) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r917))
     | cvtLITERAL (LiteralDecimal d920) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d920))
     | cvtLITERAL (LiteralInt i923) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i923))
     | cvtLITERAL (LiteralUInt u926) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u926))
     | cvtLITERAL (LiteralBoolean b929) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b929))
     | cvtLITERAL (LiteralString s932) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s932))
     | cvtLITERAL (LiteralArray{exprs=x935, ty=opt937}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x935), ("ty", 
       (case opt937 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x936 => PrettyRep.Ctor ("SOME", SOME (cvtTY x936))
       ))]))
     | cvtLITERAL (LiteralXML ls949) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x948 => 
                                                                                                           cvtEXPR x948
                                                                                                    ) ls949)))
     | cvtLITERAL (LiteralNamespace x955) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x955))
     | cvtLITERAL (LiteralObject{expr=ls959, ty=opt964}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x958 => 
                                                                        cvtFIELD x958
                                                                 ) ls959)), 
          ("ty", 
       (case opt964 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x963 => PrettyRep.Ctor ("SOME", SOME (cvtTY x963))
       ))]))
     | cvtLITERAL (LiteralFunction x975) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x975))
     | cvtLITERAL (LiteralRegExp{str=s978}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s978)]))
   and cvtBLOCK (Block x984) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x984))
   and cvtFIXTURE (NamespaceFixture x987) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x987))
     | cvtFIXTURE (ClassFixture x990) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x990))
     | cvtFIXTURE (InterfaceFixture x993) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x993))
     | cvtFIXTURE (TypeVarFixture x996) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x996))
     | cvtFIXTURE (TypeFixture x999) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x999))
     | cvtFIXTURE (MethodFixture{func=x1002, ty=x1003, readOnly=b1004, override=b1005, 
          final=b1006}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1002), ("ty", cvtTY x1003), ("readOnly", PrettyRep.Bool b1004), 
          ("override", PrettyRep.Bool b1005), ("final", PrettyRep.Bool b1006)]))
     | cvtFIXTURE (ValFixture{ty=x1020, readOnly=b1021}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1020), ("readOnly", PrettyRep.Bool b1021)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1029, getter=opt1031, setter=opt1036}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1029), ("getter", 
       (case opt1031 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1030 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1030))
       )), ("setter", 
       (case opt1036 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1035 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1035))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1049, baseTypeArgs=ls1051}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1049), ("baseTypeArgs", PrettyRep.List (List.map (fn x1050 => 
                                                                           cvtTY x1050
                                                                    ) ls1051))]))
   and cvtHEAD (Head(x1062, x1063)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1062, 
          cvtINITS x1063]))
   and cvtBINDINGS (ls1068, ls1073) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1067 => 
                                                                                       cvtBINDING x1067
                                                                                ) ls1068), 
          PrettyRep.List (List.map (fn x1072 => cvtINIT_STEP x1072
                                   ) ls1073)]
   and cvtRIB ls1081 = PrettyRep.List (List.map (fn (x1078, x1079) => PrettyRep.Tuple [cvtFIXTURE_NAME x1078, 
                                                       cvtFIXTURE x1079]
                                                ) ls1081)
   and cvtRIBS ls1092 = PrettyRep.List (List.map (fn ls1088 => PrettyRep.List (List.map (fn (x1085, 
                                                                                               x1086) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1085, 
                                                                                               cvtFIXTURE x1086]
                                                                                        ) ls1088)
                                                 ) ls1092)
   and cvtINITS ls1099 = PrettyRep.List (List.map (fn (x1096, x1097) => PrettyRep.Tuple [cvtFIXTURE_NAME x1096, 
                                                         cvtEXPR x1097]
                                                  ) ls1099)
   and cvtINSTANCE_TYPE {name=x1103, typeParams=ls1105, typeArgs=ls1110, nonnullable=b1114, 
          superTypes=ls1116, ty=x1120, dynamic=b1121} = PrettyRep.Rec [("name", 
          cvtNAME x1103), ("typeParams", PrettyRep.List (List.map (fn x1104 => 
                                                                         cvtIDENT x1104
                                                                  ) ls1105)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1109 => cvtTYPE_EXPR x1109
                                                ) ls1110)), ("nonnullable", 
          PrettyRep.Bool b1114), ("superTypes", PrettyRep.List (List.map (fn x1115 => 
                                                                                cvtTYPE_EXPR x1115
                                                                         ) ls1116)), 
          ("ty", cvtTYPE_EXPR x1120), ("dynamic", PrettyRep.Bool b1121)]
   and cvtFIELD {kind=x1137, name=x1138, init=x1139} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1137), ("name", cvtIDENT_EXPR x1138), ("init", cvtEXPR x1139)]
   and cvtFIELD_TYPE {name=x1147, ty=x1148} = PrettyRep.Rec [("name", cvtIDENT x1147), 
          ("ty", cvtTYPE_EXPR x1148)]
   and cvtFUNC_TYPE {params=ls1155, result=x1159, thisType=opt1161, hasRest=b1165, 
          minArgs=n1166} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1154 => 
                                                                                     cvtTYPE_EXPR x1154
                                                                              ) ls1155)), 
          ("result", cvtTYPE_EXPR x1159), ("thisType", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       )), ("hasRest", PrettyRep.Bool b1165), ("minArgs", PrettyRep.Int n1166)]
   and cvtFUNC_DEFN {kind=x1178, ns=opt1180, final=b1184, override=b1185, prototype=b1186, 
          static=b1187, func=x1188} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1178), 
          ("ns", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1179))
       )), ("final", PrettyRep.Bool b1184), ("override", PrettyRep.Bool b1185), 
          ("prototype", PrettyRep.Bool b1186), ("static", PrettyRep.Bool b1187), 
          ("func", cvtFUNC x1188)]
   and cvtCTOR_DEFN x1204 = cvtCTOR x1204
   and cvtVAR_DEFN {kind=x1205, ns=opt1207, static=b1211, prototype=b1212, 
          bindings=(ls1214, ls1219)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1205), 
          ("ns", 
       (case opt1207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1206 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1206))
       )), ("static", PrettyRep.Bool b1211), ("prototype", PrettyRep.Bool b1212), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1213 => 
                                                                        cvtBINDING x1213
                                                                 ) ls1214), 
          PrettyRep.List (List.map (fn x1218 => cvtINIT_STEP x1218
                                   ) ls1219)])]
   and cvtNAMESPACE_DEFN {ident=x1235, ns=opt1237, init=opt1242} = PrettyRep.Rec [("ident", 
          cvtIDENT x1235), ("ns", 
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1236))
       )), ("init", 
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1241))
       ))]
   and cvtCLASS_DEFN {ns=opt1254, ident=x1258, nonnullable=b1259, dynamic=b1260, 
          final=b1261, params=ls1263, extends=opt1268, implements=ls1273, classDefns=ls1278, 
          instanceDefns=ls1283, instanceStmts=ls1288, ctorDefn=opt1293} = PrettyRep.Rec [("ns", 
          
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       )), ("ident", cvtIDENT x1258), ("nonnullable", PrettyRep.Bool b1259), 
          ("dynamic", PrettyRep.Bool b1260), ("final", PrettyRep.Bool b1261), 
          ("params", PrettyRep.List (List.map (fn x1262 => cvtIDENT x1262
                                              ) ls1263)), ("extends", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1267))
       )), ("implements", PrettyRep.List (List.map (fn x1272 => cvtTYPE_EXPR x1272
                                                   ) ls1273)), ("classDefns", 
          PrettyRep.List (List.map (fn x1277 => cvtDEFN x1277
                                   ) ls1278)), ("instanceDefns", PrettyRep.List (List.map (fn x1282 => 
                                                                                                 cvtDEFN x1282
                                                                                          ) ls1283)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1287 => cvtSTMT x1287
                                                     ) ls1288)), ("ctorDefn", 
          
       (case opt1293 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1292 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1292))
       ))]
   and cvtINTERFACE_DEFN {ident=x1322, ns=opt1324, nonnullable=b1328, params=ls1330, 
          extends=ls1335, instanceDefns=ls1340} = PrettyRep.Rec [("ident", 
          cvtIDENT x1322), ("ns", 
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1323))
       )), ("nonnullable", PrettyRep.Bool b1328), ("params", PrettyRep.List (List.map (fn x1329 => 
                                                                                             cvtIDENT x1329
                                                                                      ) ls1330)), 
          ("extends", PrettyRep.List (List.map (fn x1334 => cvtTYPE_EXPR x1334
                                               ) ls1335)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1339 => cvtDEFN x1339
                                   ) ls1340))]
   and cvtTYPE_DEFN {ident=x1357, ns=opt1359, init=x1363} = PrettyRep.Rec [("ident", 
          cvtIDENT x1357), ("ns", 
       (case opt1359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1358))
       )), ("init", cvtTYPE_EXPR x1363)]
   and cvtCLASS_BLOCK {ns=opt1372, ident=x1376, name=opt1378, block=x1382} = 
          PrettyRep.Rec [("ns", 
       (case opt1372 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1371 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1371))
       )), ("ident", cvtIDENT x1376), ("name", 
       (case opt1378 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1377 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1377))
       )), ("block", cvtBLOCK x1382)]
   and cvtFOR_ENUM_HEAD {isEach=b1392, bindings=(ls1394, ls1399), expr=x1404} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1392), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1393 => 
                                                                                                                         cvtBINDING x1393
                                                                                                                  ) ls1394), 
          PrettyRep.List (List.map (fn x1398 => cvtINIT_STEP x1398
                                   ) ls1399)]), ("expr", cvtEXPR x1404)]
   and cvtFOR_ENUM_STMT {isEach=b1412, defn=opt1443, obj=x1447, rib=opt1455, 
          next=x1459, labels=ls1461, body=x1465} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1412), ("defn", 
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1413, ns=opt1415, static=b1419, prototype=b1420, bindings=(ls1422, 
            ls1427)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1413), ("ns", 
         (case opt1415 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1414 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1414))
         )), ("static", PrettyRep.Bool b1419), ("prototype", PrettyRep.Bool b1420), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1421 => 
                                                                          cvtBINDING x1421
                                                                   ) ls1422), 
            PrettyRep.List (List.map (fn x1426 => cvtINIT_STEP x1426
                                     ) ls1427)])]))
       )), ("obj", cvtEXPR x1447), ("rib", 
       (case opt1455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1451 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1448, 
                                                                                      x1449) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1448, 
                                                                                      cvtFIXTURE x1449]
                                                                               ) ls1451)))
       )), ("next", cvtSTMT x1459), ("labels", PrettyRep.List (List.map (fn x1460 => 
                                                                               cvtIDENT x1460
                                                                        ) ls1461)), 
          ("body", cvtSTMT x1465)]
   and cvtFOR_STMT {rib=opt1488, defn=opt1522, init=ls1527, cond=x1531, update=x1532, 
          labels=ls1534, body=x1538} = PrettyRep.Rec [("rib", 
       (case opt1488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1484 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1481, 
                                                                                      x1482) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1481, 
                                                                                      cvtFIXTURE x1482]
                                                                               ) ls1484)))
       )), ("defn", 
       (case opt1522 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1492, ns=opt1494, static=b1498, prototype=b1499, bindings=(ls1501, 
            ls1506)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1492), ("ns", 
         (case opt1494 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1493))
         )), ("static", PrettyRep.Bool b1498), ("prototype", PrettyRep.Bool b1499), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1500 => 
                                                                          cvtBINDING x1500
                                                                   ) ls1501), 
            PrettyRep.List (List.map (fn x1505 => cvtINIT_STEP x1505
                                     ) ls1506)])]))
       )), ("init", PrettyRep.List (List.map (fn x1526 => cvtSTMT x1526
                                             ) ls1527)), ("cond", cvtEXPR x1531), 
          ("update", cvtEXPR x1532), ("labels", PrettyRep.List (List.map (fn x1533 => 
                                                                                cvtIDENT x1533
                                                                         ) ls1534)), 
          ("body", cvtSTMT x1538)]
   and cvtWHILE_STMT {cond=x1554, rib=opt1562, body=x1566, labels=ls1568} = 
          PrettyRep.Rec [("cond", cvtEXPR x1554), ("rib", 
       (case opt1562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1558 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1555, 
                                                                                      x1556) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1555, 
                                                                                      cvtFIXTURE x1556]
                                                                               ) ls1558)))
       )), ("body", cvtSTMT x1566), ("labels", PrettyRep.List (List.map (fn x1567 => 
                                                                               cvtIDENT x1567
                                                                        ) ls1568))]
   and cvtDIRECTIVES {pragmas=ls1582, defns=ls1587, head=opt1592, body=ls1597, 
          loc=opt1602} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1581 => 
                                                                                    cvtPRAGMA x1581
                                                                             ) ls1582)), 
          ("defns", PrettyRep.List (List.map (fn x1586 => cvtDEFN x1586
                                             ) ls1587)), ("head", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1591))
       )), ("body", PrettyRep.List (List.map (fn x1596 => cvtSTMT x1596
                                             ) ls1597)), ("loc", 
       (case opt1602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1601 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1601))
       ))]
   and cvtCASE {label=opt1618, inits=opt1629, body=x1633} = PrettyRep.Rec [("label", 
          
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1617 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1617))
       )), ("inits", 
       (case opt1629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1625 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1622, 
                                                                                      x1623) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1622, 
                                                                                      cvtEXPR x1623]
                                                                               ) ls1625)))
       )), ("body", cvtBLOCK x1633)]
   and cvtCATCH_CLAUSE {bindings=(ls1642, ls1647), ty=x1652, rib=opt1660, inits=opt1671, 
          block=x1675} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1641 => 
                                                                                                      cvtBINDING x1641
                                                                                               ) ls1642), 
          PrettyRep.List (List.map (fn x1646 => cvtINIT_STEP x1646
                                   ) ls1647)]), ("ty", cvtTY x1652), ("rib", 
          
       (case opt1660 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1656 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1653, 
                                                                                      x1654) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1653, 
                                                                                      cvtFIXTURE x1654]
                                                                               ) ls1656)))
       )), ("inits", 
       (case opt1671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1667 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1664, 
                                                                                      x1665) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1664, 
                                                                                      cvtEXPR x1665]
                                                                               ) ls1667)))
       )), ("block", cvtBLOCK x1675)]
   and cvtFUNC_NAME {kind=x1687, ident=x1688} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1687), 
          ("ident", cvtIDENT x1688)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1694, getter=opt1696, setter=opt1701} = 
          PrettyRep.Rec [("ty", cvtTY x1694), ("getter", 
       (case opt1696 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1695 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1695))
       )), ("setter", 
       (case opt1701 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1700 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1700))
       ))]
   and cvtFRAGMENT (Unit{name=opt1713, fragments=ls1718}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1713 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1712 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1712))
       )), ("fragments", PrettyRep.List (List.map (fn x1717 => cvtFRAGMENT x1717
                                                  ) ls1718))]))
     | cvtFRAGMENT (Package{name=ls1730, fragments=ls1735}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1729 => 
                                                                        cvtIDENT x1729
                                                                 ) ls1730)), 
          ("fragments", PrettyRep.List (List.map (fn x1734 => cvtFRAGMENT x1734
                                                 ) ls1735))]))
     | cvtFRAGMENT (Anon x1746) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1746))
end

