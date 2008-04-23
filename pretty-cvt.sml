structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtRIB_ID n20 = PrettyRep.Int n20
   and cvtTYPEVAR_NONCE n21 = PrettyRep.Int n21
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Private x23) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x23))
     | cvtNAMESPACE (Protected x26) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x26))
     | cvtNAMESPACE (Public x29) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x29))
     | cvtNAMESPACE (Internal x32) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x32))
     | cvtNAMESPACE (UserNamespace s35) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s35))
     | cvtNAMESPACE (AnonUserNamespace n38) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n38))
     | cvtNAMESPACE (LimitedNamespace(x41, x42)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x41, cvtNAMESPACE x42]))
   and cvtNAME {ns=x46, id=x47} = PrettyRep.Rec [("ns", cvtNAMESPACE x46), 
          ("id", cvtIDENT x47)]
   and cvtMULTINAME {nss=ls58, id=x62} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls54 => 
                                                                                                PrettyRep.List (List.map (fn x53 => 
                                                                                                                                cvtNAMESPACE x53
                                                                                                                         ) ls54)
                                                                                         ) ls58)), 
          ("id", cvtIDENT x62)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
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
     | cvtUNOP (Spread) = PrettyRep.Ctor ("Spread", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x129) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x129))
     | cvtPRAGMA (UseDefaultNamespace x132) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x132))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls138, name=x142}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x137 => 
                                                                           cvtIDENT x137
                                                                    ) ls138)), 
          ("name", cvtIDENT x142)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x156, ribId=opt158}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x156), ("ribId", 
       (case opt158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x157 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x157))
       ))]))
   and cvtCLS (Cls{name=x169, typeParams=ls171, nonnullable=b175, dynamic=b176, 
          extends=opt178, implements=ls183, classRib=x187, instanceRib=x188, 
          instanceInits=x189, constructor=opt191, classType=x195, instanceType=x196}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x169), 
          ("typeParams", PrettyRep.List (List.map (fn x170 => cvtIDENT x170
                                                  ) ls171)), ("nonnullable", 
          PrettyRep.Bool b175), ("dynamic", PrettyRep.Bool b176), ("extends", 
          
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtTY x177))
       )), ("implements", PrettyRep.List (List.map (fn x182 => cvtTY x182
                                                   ) ls183)), ("classRib", 
          cvtRIB x187), ("instanceRib", cvtRIB x188), ("instanceInits", cvtHEAD x189), 
          ("constructor", 
       (case opt191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x190 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x190))
       )), ("classType", cvtTY x195), ("instanceType", cvtTY x196)]))
   and cvtIFACE (Iface{name=x224, typeParams=ls226, nonnullable=b230, extends=ls232, 
          instanceRib=x236, instanceType=x237}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x224), ("typeParams", PrettyRep.List (List.map (fn x225 => 
                                                                                                      cvtIDENT x225
                                                                                               ) ls226)), 
          ("nonnullable", PrettyRep.Bool b230), ("extends", PrettyRep.List (List.map (fn x231 => 
                                                                                            cvtTY x231
                                                                                     ) ls232)), 
          ("instanceRib", cvtRIB x236), ("instanceType", cvtTY x237)]))
   and cvtCTOR (Ctor{settings=x253, superArgs=ls255, func=x259}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x253), ("superArgs", PrettyRep.List (List.map (fn x254 => 
                                                                                                         cvtEXPR x254
                                                                                                  ) ls255)), 
          ("func", cvtFUNC x259)]))
   and cvtFUNC (Func{name=x269, fsig=x270, native=b271, block=opt273, param=x277, 
          defaults=ls279, ty=x283, loc=opt285}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x269), ("fsig", cvtFUNC_SIG x270), ("native", PrettyRep.Bool b271), 
          ("block", 
       (case opt273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x272 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x272))
       )), ("param", cvtHEAD x277), ("defaults", PrettyRep.List (List.map (fn x278 => 
                                                                                 cvtEXPR x278
                                                                          ) ls279)), 
          ("ty", cvtTY x283), ("loc", 
       (case opt285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x284 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x284))
       ))]))
   and cvtDEFN (ClassDefn x308) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x308))
     | cvtDEFN (VariableDefn x311) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x311))
     | cvtDEFN (FunctionDefn x314) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x314))
     | cvtDEFN (ConstructorDefn x317) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x317))
     | cvtDEFN (InterfaceDefn x320) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x320))
     | cvtDEFN (NamespaceDefn x323) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x323))
     | cvtDEFN (TypeDefn x326) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x326))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls330, params=x334, paramTypes=ls336, 
          defaults=ls341, ctorInits=opt352, returnType=x356, thisType=opt358, 
          hasRest=b362}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x329 => cvtIDENT x329
                                   ) ls330)), ("params", cvtBINDINGS x334), 
          ("paramTypes", PrettyRep.List (List.map (fn x335 => cvtTYPE_EXPR x335
                                                  ) ls336)), ("defaults", PrettyRep.List (List.map (fn x340 => 
                                                                                                          cvtEXPR x340
                                                                                                   ) ls341)), 
          ("ctorInits", 
       (case opt352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x345, ls347) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x345, 
            PrettyRep.List (List.map (fn x346 => cvtEXPR x346
                                     ) ls347)]))
       )), ("returnType", cvtTYPE_EXPR x356), ("thisType", 
       (case opt358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x357 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x357))
       )), ("hasRest", PrettyRep.Bool b362)]))
   and cvtBINDING (Binding{ident=x382, ty=x383}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x382), ("ty", cvtTYPE_EXPR x383)]))
   and cvtBINDING_IDENT (TempIdent n391) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n391))
     | cvtBINDING_IDENT (ParamIdent n394) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n394))
     | cvtBINDING_IDENT (PropIdent x397) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x397))
   and cvtINIT_STEP (InitStep(x400, x401)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x400, 
          cvtEXPR x401]))
     | cvtINIT_STEP (AssignStep(x405, x406)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x405, cvtEXPR x406]))
   and cvtTYPE_EXPR (SpecialType x410) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x410))
     | cvtTYPE_EXPR (UnionType ls414) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x413 => 
                                                                                                           cvtTYPE_EXPR x413
                                                                                                    ) ls414)))
     | cvtTYPE_EXPR (ArrayType ls421) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x420 => 
                                                                                                           cvtTYPE_EXPR x420
                                                                                                    ) ls421)))
     | cvtTYPE_EXPR (TypeName x427) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x427))
     | cvtTYPE_EXPR (ElementTypeRef(x430, n431)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x430, PrettyRep.Int n431]))
     | cvtTYPE_EXPR (FieldTypeRef(x435, x436)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x435, cvtIDENT x436]))
     | cvtTYPE_EXPR (FunctionType x440) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x440))
     | cvtTYPE_EXPR (ObjectType ls444) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x443 => 
                                                                                                             cvtFIELD_TYPE x443
                                                                                                      ) ls444)))
     | cvtTYPE_EXPR (LikeType x450) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x450))
     | cvtTYPE_EXPR (AppType{base=x453, args=ls455}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x453), ("args", PrettyRep.List (List.map (fn x454 => 
                                                                                                     cvtTYPE_EXPR x454
                                                                                              ) ls455))]))
     | cvtTYPE_EXPR (LamType{params=ls467, body=x471}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x466 => 
                                                                          cvtIDENT x466
                                                                   ) ls467)), 
          ("body", cvtTYPE_EXPR x471)]))
     | cvtTYPE_EXPR (NullableType{expr=x479, nullable=b480}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x479), ("nullable", PrettyRep.Bool b480)]))
     | cvtTYPE_EXPR (InstanceType x488) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x488))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x492) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x492))
     | cvtSTMT (InitStmt{kind=x495, ns=opt497, prototype=b501, static=b502, 
          temps=x503, inits=ls505}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x495), ("ns", 
       (case opt497 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x496 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x496))
       )), ("prototype", PrettyRep.Bool b501), ("static", PrettyRep.Bool b502), 
          ("temps", cvtBINDINGS x503), ("inits", PrettyRep.List (List.map (fn x504 => 
                                                                                 cvtINIT_STEP x504
                                                                          ) ls505))]))
     | cvtSTMT (ClassBlock x524) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x524))
     | cvtSTMT (ForInStmt x527) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x527))
     | cvtSTMT (ThrowStmt x530) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x530))
     | cvtSTMT (ReturnStmt x533) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x533))
     | cvtSTMT (BreakStmt opt537) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x536 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x536))
       ))
     | cvtSTMT (ContinueStmt opt544) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x543 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x543))
       ))
     | cvtSTMT (BlockStmt x550) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x550))
     | cvtSTMT (LabeledStmt(x553, x554)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x553, 
          cvtSTMT x554]))
     | cvtSTMT (LetStmt x558) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x558))
     | cvtSTMT (WhileStmt x561) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x561))
     | cvtSTMT (DoWhileStmt x564) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x564))
     | cvtSTMT (ForStmt x567) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x567))
     | cvtSTMT (IfStmt{cnd=x570, thn=x571, els=x572}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x570), ("thn", cvtSTMT x571), 
          ("els", cvtSTMT x572)]))
     | cvtSTMT (WithStmt{obj=x582, ty=x583, body=x584}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x582), ("ty", cvtTY x583), ("body", 
          cvtSTMT x584)]))
     | cvtSTMT (TryStmt{block=x594, catches=ls596, finally=opt601}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x594), ("catches", PrettyRep.List (List.map (fn x595 => 
                                                                                                     cvtCATCH_CLAUSE x595
                                                                                              ) ls596)), 
          ("finally", 
       (case opt601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x600 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x600))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x614, labels=ls616, cases=ls621}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x614), ("labels", PrettyRep.List (List.map (fn x615 => 
                                                                                                  cvtIDENT x615
                                                                                           ) ls616)), 
          ("cases", PrettyRep.List (List.map (fn x620 => cvtCASE x620
                                             ) ls621))]))
     | cvtSTMT (SwitchTypeStmt{cond=x634, ty=x635, cases=ls637}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x634), ("ty", cvtTY x635), 
          ("cases", PrettyRep.List (List.map (fn x636 => cvtCATCH_CLAUSE x636
                                             ) ls637))]))
     | cvtSTMT (DXNStmt{expr=x650}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x650)]))
   and cvtEXPR (TernaryExpr(x656, x657, x658)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x656, cvtEXPR x657, cvtEXPR x658]))
     | cvtEXPR (BinaryExpr(x662, x663, x664)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x662, cvtEXPR x663, cvtEXPR x664]))
     | cvtEXPR (BinaryTypeExpr(x668, x669, x670)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x668, cvtEXPR x669, cvtTY x670]))
     | cvtEXPR (UnaryExpr(x674, x675)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x674, 
          cvtEXPR x675]))
     | cvtEXPR (TypeExpr x679) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x679))
     | cvtEXPR (ThisExpr opt683) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x682 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x682))
       ))
     | cvtEXPR (YieldExpr opt690) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt690 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x689 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x689))
       ))
     | cvtEXPR (SuperExpr opt697) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt697 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x696 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x696))
       ))
     | cvtEXPR (LiteralExpr x703) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x703))
     | cvtEXPR (CallExpr{func=x706, actuals=ls708}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x706), ("actuals", PrettyRep.List (List.map (fn x707 => 
                                                                                                   cvtEXPR x707
                                                                                            ) ls708))]))
     | cvtEXPR (ApplyTypeExpr{expr=x719, actuals=ls721}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x719), ("actuals", PrettyRep.List (List.map (fn x720 => 
                                                                                                   cvtTY x720
                                                                                            ) ls721))]))
     | cvtEXPR (LetExpr{defs=x732, body=x733, head=opt735}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x732), ("body", cvtEXPR x733), 
          ("head", 
       (case opt735 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x734 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x734))
       ))]))
     | cvtEXPR (NewExpr{obj=x748, actuals=ls750}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x748), ("actuals", PrettyRep.List (List.map (fn x749 => 
                                                                                                  cvtEXPR x749
                                                                                           ) ls750))]))
     | cvtEXPR (ObjectRef{base=x761, ident=x762, loc=opt764}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x761), ("ident", cvtIDENT_EXPR x762), 
          ("loc", 
       (case opt764 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x763 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x763))
       ))]))
     | cvtEXPR (LexicalRef{ident=x777, loc=opt779}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x777), ("loc", 
       (case opt779 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x778 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x778))
       ))]))
     | cvtEXPR (SetExpr(x790, x791, x792)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x790, 
          cvtEXPR x791, cvtEXPR x792]))
     | cvtEXPR (ListExpr ls797) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x796 => 
                                                                                                    cvtEXPR x796
                                                                                             ) ls797)))
     | cvtEXPR (InitExpr(x803, x804, x805)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x803, 
          cvtHEAD x804, cvtINITS x805]))
     | cvtEXPR (GetTemp n809) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n809))
     | cvtEXPR (GetParam n812) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n812))
     | cvtEXPR (Comprehension(x815, ls817, opt822)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x815, PrettyRep.List (List.map (fn x816 => 
                                                                               cvtFOR_ENUM_HEAD x816
                                                                        ) ls817), 
          
       (case opt822 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x821 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x821))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n834) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n834))
     | cvtFIXTURE_NAME (PropName x837) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x837))
   and cvtIDENT_EXPR (Identifier{ident=x840, openNamespaces=ls846}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x840), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls842 => PrettyRep.List (List.map (fn x841 => 
                                                                                cvtNAMESPACE x841
                                                                         ) ls842)
                                   ) ls846))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x857, expr=x858}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x857), ("expr", cvtEXPR x858)]))
     | cvtIDENT_EXPR (AttributeIdentifier x866) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x866))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x869, openNamespaces=ls875}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x869), ("openNamespaces", PrettyRep.List (List.map (fn ls871 => 
                                                                            PrettyRep.List (List.map (fn x870 => 
                                                                                                            cvtNAMESPACE x870
                                                                                                     ) ls871)
                                                                     ) ls875))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x886, ident=s887}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x886), ("ident", PrettyRep.UniStr s887)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls896, x900)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x895 => cvtIDENT x895
                                                          ) ls896), cvtIDENT_EXPR x900]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r907) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r907))
     | cvtLITERAL (LiteralDecimal d910) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d910))
     | cvtLITERAL (LiteralBoolean b913) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b913))
     | cvtLITERAL (LiteralString s916) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s916))
     | cvtLITERAL (LiteralArray{exprs=x919, ty=opt921}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x919), ("ty", 
       (case opt921 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x920 => PrettyRep.Ctor ("SOME", SOME (cvtTY x920))
       ))]))
     | cvtLITERAL (LiteralXML ls933) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x932 => 
                                                                                                           cvtEXPR x932
                                                                                                    ) ls933)))
     | cvtLITERAL (LiteralNamespace x939) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x939))
     | cvtLITERAL (LiteralObject{expr=ls943, ty=opt948}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x942 => 
                                                                        cvtFIELD x942
                                                                 ) ls943)), 
          ("ty", 
       (case opt948 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x947 => PrettyRep.Ctor ("SOME", SOME (cvtTY x947))
       ))]))
     | cvtLITERAL (LiteralFunction x959) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x959))
     | cvtLITERAL (LiteralRegExp{str=s962}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s962)]))
   and cvtBLOCK (Block x968) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x968))
   and cvtFIXTURE (NamespaceFixture x971) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x971))
     | cvtFIXTURE (ClassFixture x974) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x974))
     | cvtFIXTURE (InterfaceFixture x977) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x977))
     | cvtFIXTURE (TypeVarFixture x980) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x980))
     | cvtFIXTURE (TypeFixture x983) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x983))
     | cvtFIXTURE (MethodFixture{func=x986, ty=x987, readOnly=b988, override=b989, 
          final=b990}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x986), ("ty", cvtTY x987), ("readOnly", PrettyRep.Bool b988), 
          ("override", PrettyRep.Bool b989), ("final", PrettyRep.Bool b990)]))
     | cvtFIXTURE (ValFixture{ty=x1004, readOnly=b1005}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1004), ("readOnly", PrettyRep.Bool b1005)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1013, getter=opt1015, setter=opt1020}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1013), ("getter", 
       (case opt1015 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1014 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1014))
       )), ("setter", 
       (case opt1020 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1019 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1019))
       ))]))
   and cvtHEAD (Head(x1033, x1034)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1033, 
          cvtINITS x1034]))
   and cvtBINDINGS (ls1039, ls1044) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1038 => 
                                                                                       cvtBINDING x1038
                                                                                ) ls1039), 
          PrettyRep.List (List.map (fn x1043 => cvtINIT_STEP x1043
                                   ) ls1044)]
   and cvtRIB ls1052 = PrettyRep.List (List.map (fn (x1049, x1050) => PrettyRep.Tuple [cvtFIXTURE_NAME x1049, 
                                                       cvtFIXTURE x1050]
                                                ) ls1052)
   and cvtRIBS ls1063 = PrettyRep.List (List.map (fn ls1059 => PrettyRep.List (List.map (fn (x1056, 
                                                                                               x1057) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1056, 
                                                                                               cvtFIXTURE x1057]
                                                                                        ) ls1059)
                                                 ) ls1063)
   and cvtINITS ls1070 = PrettyRep.List (List.map (fn (x1067, x1068) => PrettyRep.Tuple [cvtFIXTURE_NAME x1067, 
                                                         cvtEXPR x1068]
                                                  ) ls1070)
   and cvtINSTANCE_TYPE {name=x1074, typeParams=ls1076, typeArgs=ls1081, nonnullable=b1085, 
          superTypes=ls1087, ty=x1091, dynamic=b1092} = PrettyRep.Rec [("name", 
          cvtNAME x1074), ("typeParams", PrettyRep.List (List.map (fn x1075 => 
                                                                         cvtIDENT x1075
                                                                  ) ls1076)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1080 => cvtTYPE_EXPR x1080
                                                ) ls1081)), ("nonnullable", 
          PrettyRep.Bool b1085), ("superTypes", PrettyRep.List (List.map (fn x1086 => 
                                                                                cvtTYPE_EXPR x1086
                                                                         ) ls1087)), 
          ("ty", cvtTYPE_EXPR x1091), ("dynamic", PrettyRep.Bool b1092)]
   and cvtFIELD {kind=x1108, name=x1109, init=x1110} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1108), ("name", cvtIDENT_EXPR x1109), ("init", cvtEXPR x1110)]
   and cvtFIELD_TYPE {name=x1118, ty=x1119} = PrettyRep.Rec [("name", cvtIDENT x1118), 
          ("ty", cvtTYPE_EXPR x1119)]
   and cvtFUNC_TYPE {params=ls1126, result=x1130, thisType=opt1132, hasRest=b1136, 
          minArgs=n1137} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1125 => 
                                                                                     cvtTYPE_EXPR x1125
                                                                              ) ls1126)), 
          ("result", cvtTYPE_EXPR x1130), ("thisType", 
       (case opt1132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1131 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1131))
       )), ("hasRest", PrettyRep.Bool b1136), ("minArgs", PrettyRep.Int n1137)]
   and cvtFUNC_DEFN {kind=x1149, ns=opt1151, final=b1155, override=b1156, prototype=b1157, 
          static=b1158, func=x1159} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1149), 
          ("ns", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1150))
       )), ("final", PrettyRep.Bool b1155), ("override", PrettyRep.Bool b1156), 
          ("prototype", PrettyRep.Bool b1157), ("static", PrettyRep.Bool b1158), 
          ("func", cvtFUNC x1159)]
   and cvtCTOR_DEFN x1175 = cvtCTOR x1175
   and cvtVAR_DEFN {kind=x1176, ns=opt1178, static=b1182, prototype=b1183, 
          bindings=(ls1185, ls1190)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1176), 
          ("ns", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1177))
       )), ("static", PrettyRep.Bool b1182), ("prototype", PrettyRep.Bool b1183), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1184 => 
                                                                        cvtBINDING x1184
                                                                 ) ls1185), 
          PrettyRep.List (List.map (fn x1189 => cvtINIT_STEP x1189
                                   ) ls1190)])]
   and cvtNAMESPACE_DEFN {ident=x1206, ns=opt1208, init=opt1213} = PrettyRep.Rec [("ident", 
          cvtIDENT x1206), ("ns", 
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1207))
       )), ("init", 
       (case opt1213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1212 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1212))
       ))]
   and cvtCLASS_DEFN {ns=opt1225, ident=x1229, nonnullable=b1230, dynamic=b1231, 
          final=b1232, params=ls1234, extends=opt1239, implements=ls1244, classDefns=ls1249, 
          instanceDefns=ls1254, instanceStmts=ls1259, ctorDefn=opt1264} = PrettyRep.Rec [("ns", 
          
       (case opt1225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1224 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1224))
       )), ("ident", cvtIDENT x1229), ("nonnullable", PrettyRep.Bool b1230), 
          ("dynamic", PrettyRep.Bool b1231), ("final", PrettyRep.Bool b1232), 
          ("params", PrettyRep.List (List.map (fn x1233 => cvtIDENT x1233
                                              ) ls1234)), ("extends", 
       (case opt1239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1238 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1238))
       )), ("implements", PrettyRep.List (List.map (fn x1243 => cvtTYPE_EXPR x1243
                                                   ) ls1244)), ("classDefns", 
          PrettyRep.List (List.map (fn x1248 => cvtDEFN x1248
                                   ) ls1249)), ("instanceDefns", PrettyRep.List (List.map (fn x1253 => 
                                                                                                 cvtDEFN x1253
                                                                                          ) ls1254)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1258 => cvtSTMT x1258
                                                     ) ls1259)), ("ctorDefn", 
          
       (case opt1264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1263 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1263))
       ))]
   and cvtINTERFACE_DEFN {ident=x1293, ns=opt1295, nonnullable=b1299, params=ls1301, 
          extends=ls1306, instanceDefns=ls1311} = PrettyRep.Rec [("ident", 
          cvtIDENT x1293), ("ns", 
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1294))
       )), ("nonnullable", PrettyRep.Bool b1299), ("params", PrettyRep.List (List.map (fn x1300 => 
                                                                                             cvtIDENT x1300
                                                                                      ) ls1301)), 
          ("extends", PrettyRep.List (List.map (fn x1305 => cvtTYPE_EXPR x1305
                                               ) ls1306)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1310 => cvtDEFN x1310
                                   ) ls1311))]
   and cvtTYPE_DEFN {ident=x1328, ns=opt1330, init=x1334} = PrettyRep.Rec [("ident", 
          cvtIDENT x1328), ("ns", 
       (case opt1330 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1329 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1329))
       )), ("init", cvtTYPE_EXPR x1334)]
   and cvtCLASS_BLOCK {ns=opt1343, ident=x1347, name=opt1349, block=x1353} = 
          PrettyRep.Rec [("ns", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1342))
       )), ("ident", cvtIDENT x1347), ("name", 
       (case opt1349 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1348 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1348))
       )), ("block", cvtBLOCK x1353)]
   and cvtFOR_ENUM_HEAD {isEach=b1363, bindings=(ls1365, ls1370), expr=x1375} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1363), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1364 => 
                                                                                                                         cvtBINDING x1364
                                                                                                                  ) ls1365), 
          PrettyRep.List (List.map (fn x1369 => cvtINIT_STEP x1369
                                   ) ls1370)]), ("expr", cvtEXPR x1375)]
   and cvtFOR_ENUM_STMT {isEach=b1383, defn=opt1414, obj=x1418, rib=opt1426, 
          next=x1430, labels=ls1432, body=x1436} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1383), ("defn", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1384, ns=opt1386, static=b1390, prototype=b1391, bindings=(ls1393, 
            ls1398)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1384), ("ns", 
         (case opt1386 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1385))
         )), ("static", PrettyRep.Bool b1390), ("prototype", PrettyRep.Bool b1391), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1392 => 
                                                                          cvtBINDING x1392
                                                                   ) ls1393), 
            PrettyRep.List (List.map (fn x1397 => cvtINIT_STEP x1397
                                     ) ls1398)])]))
       )), ("obj", cvtEXPR x1418), ("rib", 
       (case opt1426 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1422 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1419, 
                                                                                      x1420) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1419, 
                                                                                      cvtFIXTURE x1420]
                                                                               ) ls1422)))
       )), ("next", cvtSTMT x1430), ("labels", PrettyRep.List (List.map (fn x1431 => 
                                                                               cvtIDENT x1431
                                                                        ) ls1432)), 
          ("body", cvtSTMT x1436)]
   and cvtFOR_STMT {rib=opt1459, defn=opt1493, init=ls1498, cond=x1502, update=x1503, 
          labels=ls1505, body=x1509} = PrettyRep.Rec [("rib", 
       (case opt1459 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1455 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1452, 
                                                                                      x1453) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1452, 
                                                                                      cvtFIXTURE x1453]
                                                                               ) ls1455)))
       )), ("defn", 
       (case opt1493 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1463, ns=opt1465, static=b1469, prototype=b1470, bindings=(ls1472, 
            ls1477)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1463), ("ns", 
         (case opt1465 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1464 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1464))
         )), ("static", PrettyRep.Bool b1469), ("prototype", PrettyRep.Bool b1470), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1471 => 
                                                                          cvtBINDING x1471
                                                                   ) ls1472), 
            PrettyRep.List (List.map (fn x1476 => cvtINIT_STEP x1476
                                     ) ls1477)])]))
       )), ("init", PrettyRep.List (List.map (fn x1497 => cvtSTMT x1497
                                             ) ls1498)), ("cond", cvtEXPR x1502), 
          ("update", cvtEXPR x1503), ("labels", PrettyRep.List (List.map (fn x1504 => 
                                                                                cvtIDENT x1504
                                                                         ) ls1505)), 
          ("body", cvtSTMT x1509)]
   and cvtWHILE_STMT {cond=x1525, rib=opt1533, body=x1537, labels=ls1539} = 
          PrettyRep.Rec [("cond", cvtEXPR x1525), ("rib", 
       (case opt1533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1529 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1526, 
                                                                                      x1527) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1526, 
                                                                                      cvtFIXTURE x1527]
                                                                               ) ls1529)))
       )), ("body", cvtSTMT x1537), ("labels", PrettyRep.List (List.map (fn x1538 => 
                                                                               cvtIDENT x1538
                                                                        ) ls1539))]
   and cvtDIRECTIVES {pragmas=ls1553, defns=ls1558, head=opt1563, body=ls1568, 
          loc=opt1573} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1552 => 
                                                                                    cvtPRAGMA x1552
                                                                             ) ls1553)), 
          ("defns", PrettyRep.List (List.map (fn x1557 => cvtDEFN x1557
                                             ) ls1558)), ("head", 
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1562))
       )), ("body", PrettyRep.List (List.map (fn x1567 => cvtSTMT x1567
                                             ) ls1568)), ("loc", 
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1572 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1572))
       ))]
   and cvtCASE {label=opt1589, inits=opt1600, body=x1604} = PrettyRep.Rec [("label", 
          
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1588))
       )), ("inits", 
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1596 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1593, 
                                                                                      x1594) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1593, 
                                                                                      cvtEXPR x1594]
                                                                               ) ls1596)))
       )), ("body", cvtBLOCK x1604)]
   and cvtCATCH_CLAUSE {bindings=(ls1613, ls1618), ty=x1623, rib=opt1631, inits=opt1642, 
          block=x1646} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1612 => 
                                                                                                      cvtBINDING x1612
                                                                                               ) ls1613), 
          PrettyRep.List (List.map (fn x1617 => cvtINIT_STEP x1617
                                   ) ls1618)]), ("ty", cvtTY x1623), ("rib", 
          
       (case opt1631 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1627 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1624, 
                                                                                      x1625) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1624, 
                                                                                      cvtFIXTURE x1625]
                                                                               ) ls1627)))
       )), ("inits", 
       (case opt1642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1638 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1635, 
                                                                                      x1636) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1635, 
                                                                                      cvtEXPR x1636]
                                                                               ) ls1638)))
       )), ("block", cvtBLOCK x1646)]
   and cvtFUNC_NAME {kind=x1658, ident=x1659} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1658), 
          ("ident", cvtIDENT x1659)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1665, getter=opt1667, setter=opt1672} = 
          PrettyRep.Rec [("ty", cvtTY x1665), ("getter", 
       (case opt1667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1666 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1666))
       )), ("setter", 
       (case opt1672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1671 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1671))
       ))]
   and cvtFRAGMENT (Package{name=ls1684, fragments=ls1689}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1683 => 
                                                                        cvtIDENT x1683
                                                                 ) ls1684)), 
          ("fragments", PrettyRep.List (List.map (fn x1688 => cvtFRAGMENT x1688
                                                 ) ls1689))]))
     | cvtFRAGMENT (Anon x1700) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1700))
end

