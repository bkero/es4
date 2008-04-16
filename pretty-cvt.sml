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
     | cvtUNOP (Splat) = PrettyRep.Ctor ("Splat", NONE)
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
     | cvtFIXTURE (InheritedFixture{baseName=x1033, baseTypeArgs=ls1035}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1033), ("baseTypeArgs", PrettyRep.List (List.map (fn x1034 => 
                                                                           cvtTY x1034
                                                                    ) ls1035))]))
   and cvtHEAD (Head(x1046, x1047)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1046, 
          cvtINITS x1047]))
   and cvtBINDINGS (ls1052, ls1057) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1051 => 
                                                                                       cvtBINDING x1051
                                                                                ) ls1052), 
          PrettyRep.List (List.map (fn x1056 => cvtINIT_STEP x1056
                                   ) ls1057)]
   and cvtRIB ls1065 = PrettyRep.List (List.map (fn (x1062, x1063) => PrettyRep.Tuple [cvtFIXTURE_NAME x1062, 
                                                       cvtFIXTURE x1063]
                                                ) ls1065)
   and cvtRIBS ls1076 = PrettyRep.List (List.map (fn ls1072 => PrettyRep.List (List.map (fn (x1069, 
                                                                                               x1070) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1069, 
                                                                                               cvtFIXTURE x1070]
                                                                                        ) ls1072)
                                                 ) ls1076)
   and cvtINITS ls1083 = PrettyRep.List (List.map (fn (x1080, x1081) => PrettyRep.Tuple [cvtFIXTURE_NAME x1080, 
                                                         cvtEXPR x1081]
                                                  ) ls1083)
   and cvtINSTANCE_TYPE {name=x1087, typeParams=ls1089, typeArgs=ls1094, nonnullable=b1098, 
          superTypes=ls1100, ty=x1104, dynamic=b1105} = PrettyRep.Rec [("name", 
          cvtNAME x1087), ("typeParams", PrettyRep.List (List.map (fn x1088 => 
                                                                         cvtIDENT x1088
                                                                  ) ls1089)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1093 => cvtTYPE_EXPR x1093
                                                ) ls1094)), ("nonnullable", 
          PrettyRep.Bool b1098), ("superTypes", PrettyRep.List (List.map (fn x1099 => 
                                                                                cvtTYPE_EXPR x1099
                                                                         ) ls1100)), 
          ("ty", cvtTYPE_EXPR x1104), ("dynamic", PrettyRep.Bool b1105)]
   and cvtFIELD {kind=x1121, name=x1122, init=x1123} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1121), ("name", cvtIDENT_EXPR x1122), ("init", cvtEXPR x1123)]
   and cvtFIELD_TYPE {name=x1131, ty=x1132} = PrettyRep.Rec [("name", cvtIDENT x1131), 
          ("ty", cvtTYPE_EXPR x1132)]
   and cvtFUNC_TYPE {params=ls1139, result=x1143, thisType=opt1145, hasRest=b1149, 
          minArgs=n1150} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1138 => 
                                                                                     cvtTYPE_EXPR x1138
                                                                              ) ls1139)), 
          ("result", cvtTYPE_EXPR x1143), ("thisType", 
       (case opt1145 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1144 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1144))
       )), ("hasRest", PrettyRep.Bool b1149), ("minArgs", PrettyRep.Int n1150)]
   and cvtFUNC_DEFN {kind=x1162, ns=opt1164, final=b1168, override=b1169, prototype=b1170, 
          static=b1171, func=x1172} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1162), 
          ("ns", 
       (case opt1164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1163 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1163))
       )), ("final", PrettyRep.Bool b1168), ("override", PrettyRep.Bool b1169), 
          ("prototype", PrettyRep.Bool b1170), ("static", PrettyRep.Bool b1171), 
          ("func", cvtFUNC x1172)]
   and cvtCTOR_DEFN x1188 = cvtCTOR x1188
   and cvtVAR_DEFN {kind=x1189, ns=opt1191, static=b1195, prototype=b1196, 
          bindings=(ls1198, ls1203)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1189), 
          ("ns", 
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1190))
       )), ("static", PrettyRep.Bool b1195), ("prototype", PrettyRep.Bool b1196), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1197 => 
                                                                        cvtBINDING x1197
                                                                 ) ls1198), 
          PrettyRep.List (List.map (fn x1202 => cvtINIT_STEP x1202
                                   ) ls1203)])]
   and cvtNAMESPACE_DEFN {ident=x1219, ns=opt1221, init=opt1226} = PrettyRep.Rec [("ident", 
          cvtIDENT x1219), ("ns", 
       (case opt1221 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1220 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1220))
       )), ("init", 
       (case opt1226 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1225 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1225))
       ))]
   and cvtCLASS_DEFN {ns=opt1238, ident=x1242, nonnullable=b1243, dynamic=b1244, 
          final=b1245, params=ls1247, extends=opt1252, implements=ls1257, classDefns=ls1262, 
          instanceDefns=ls1267, instanceStmts=ls1272, ctorDefn=opt1277} = PrettyRep.Rec [("ns", 
          
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1237))
       )), ("ident", cvtIDENT x1242), ("nonnullable", PrettyRep.Bool b1243), 
          ("dynamic", PrettyRep.Bool b1244), ("final", PrettyRep.Bool b1245), 
          ("params", PrettyRep.List (List.map (fn x1246 => cvtIDENT x1246
                                              ) ls1247)), ("extends", 
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1251))
       )), ("implements", PrettyRep.List (List.map (fn x1256 => cvtTYPE_EXPR x1256
                                                   ) ls1257)), ("classDefns", 
          PrettyRep.List (List.map (fn x1261 => cvtDEFN x1261
                                   ) ls1262)), ("instanceDefns", PrettyRep.List (List.map (fn x1266 => 
                                                                                                 cvtDEFN x1266
                                                                                          ) ls1267)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1271 => cvtSTMT x1271
                                                     ) ls1272)), ("ctorDefn", 
          
       (case opt1277 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1276 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1276))
       ))]
   and cvtINTERFACE_DEFN {ident=x1306, ns=opt1308, nonnullable=b1312, params=ls1314, 
          extends=ls1319, instanceDefns=ls1324} = PrettyRep.Rec [("ident", 
          cvtIDENT x1306), ("ns", 
       (case opt1308 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1307 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1307))
       )), ("nonnullable", PrettyRep.Bool b1312), ("params", PrettyRep.List (List.map (fn x1313 => 
                                                                                             cvtIDENT x1313
                                                                                      ) ls1314)), 
          ("extends", PrettyRep.List (List.map (fn x1318 => cvtTYPE_EXPR x1318
                                               ) ls1319)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1323 => cvtDEFN x1323
                                   ) ls1324))]
   and cvtTYPE_DEFN {ident=x1341, ns=opt1343, init=x1347} = PrettyRep.Rec [("ident", 
          cvtIDENT x1341), ("ns", 
       (case opt1343 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1342 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1342))
       )), ("init", cvtTYPE_EXPR x1347)]
   and cvtCLASS_BLOCK {ns=opt1356, ident=x1360, name=opt1362, block=x1366} = 
          PrettyRep.Rec [("ns", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1355))
       )), ("ident", cvtIDENT x1360), ("name", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1361))
       )), ("block", cvtBLOCK x1366)]
   and cvtFOR_ENUM_HEAD {isEach=b1376, bindings=(ls1378, ls1383), expr=x1388} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1376), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1377 => 
                                                                                                                         cvtBINDING x1377
                                                                                                                  ) ls1378), 
          PrettyRep.List (List.map (fn x1382 => cvtINIT_STEP x1382
                                   ) ls1383)]), ("expr", cvtEXPR x1388)]
   and cvtFOR_ENUM_STMT {isEach=b1396, defn=opt1427, obj=x1431, rib=opt1439, 
          next=x1443, labels=ls1445, body=x1449} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1396), ("defn", 
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1397, ns=opt1399, static=b1403, prototype=b1404, bindings=(ls1406, 
            ls1411)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1397), ("ns", 
         (case opt1399 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1398 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1398))
         )), ("static", PrettyRep.Bool b1403), ("prototype", PrettyRep.Bool b1404), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1405 => 
                                                                          cvtBINDING x1405
                                                                   ) ls1406), 
            PrettyRep.List (List.map (fn x1410 => cvtINIT_STEP x1410
                                     ) ls1411)])]))
       )), ("obj", cvtEXPR x1431), ("rib", 
       (case opt1439 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1435 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1432, 
                                                                                      x1433) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1432, 
                                                                                      cvtFIXTURE x1433]
                                                                               ) ls1435)))
       )), ("next", cvtSTMT x1443), ("labels", PrettyRep.List (List.map (fn x1444 => 
                                                                               cvtIDENT x1444
                                                                        ) ls1445)), 
          ("body", cvtSTMT x1449)]
   and cvtFOR_STMT {rib=opt1472, defn=opt1506, init=ls1511, cond=x1515, update=x1516, 
          labels=ls1518, body=x1522} = PrettyRep.Rec [("rib", 
       (case opt1472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1468 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1465, 
                                                                                      x1466) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1465, 
                                                                                      cvtFIXTURE x1466]
                                                                               ) ls1468)))
       )), ("defn", 
       (case opt1506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1476, ns=opt1478, static=b1482, prototype=b1483, bindings=(ls1485, 
            ls1490)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1476), ("ns", 
         (case opt1478 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1477 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1477))
         )), ("static", PrettyRep.Bool b1482), ("prototype", PrettyRep.Bool b1483), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1484 => 
                                                                          cvtBINDING x1484
                                                                   ) ls1485), 
            PrettyRep.List (List.map (fn x1489 => cvtINIT_STEP x1489
                                     ) ls1490)])]))
       )), ("init", PrettyRep.List (List.map (fn x1510 => cvtSTMT x1510
                                             ) ls1511)), ("cond", cvtEXPR x1515), 
          ("update", cvtEXPR x1516), ("labels", PrettyRep.List (List.map (fn x1517 => 
                                                                                cvtIDENT x1517
                                                                         ) ls1518)), 
          ("body", cvtSTMT x1522)]
   and cvtWHILE_STMT {cond=x1538, rib=opt1546, body=x1550, labels=ls1552} = 
          PrettyRep.Rec [("cond", cvtEXPR x1538), ("rib", 
       (case opt1546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1542 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1539, 
                                                                                      x1540) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1539, 
                                                                                      cvtFIXTURE x1540]
                                                                               ) ls1542)))
       )), ("body", cvtSTMT x1550), ("labels", PrettyRep.List (List.map (fn x1551 => 
                                                                               cvtIDENT x1551
                                                                        ) ls1552))]
   and cvtDIRECTIVES {pragmas=ls1566, defns=ls1571, head=opt1576, body=ls1581, 
          loc=opt1586} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1565 => 
                                                                                    cvtPRAGMA x1565
                                                                             ) ls1566)), 
          ("defns", PrettyRep.List (List.map (fn x1570 => cvtDEFN x1570
                                             ) ls1571)), ("head", 
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1575 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1575))
       )), ("body", PrettyRep.List (List.map (fn x1580 => cvtSTMT x1580
                                             ) ls1581)), ("loc", 
       (case opt1586 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1585 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1585))
       ))]
   and cvtCASE {label=opt1602, inits=opt1613, body=x1617} = PrettyRep.Rec [("label", 
          
       (case opt1602 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1601 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1601))
       )), ("inits", 
       (case opt1613 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1609 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1606, 
                                                                                      x1607) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1606, 
                                                                                      cvtEXPR x1607]
                                                                               ) ls1609)))
       )), ("body", cvtBLOCK x1617)]
   and cvtCATCH_CLAUSE {bindings=(ls1626, ls1631), ty=x1636, rib=opt1644, inits=opt1655, 
          block=x1659} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1625 => 
                                                                                                      cvtBINDING x1625
                                                                                               ) ls1626), 
          PrettyRep.List (List.map (fn x1630 => cvtINIT_STEP x1630
                                   ) ls1631)]), ("ty", cvtTY x1636), ("rib", 
          
       (case opt1644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1640 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1637, 
                                                                                      x1638) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1637, 
                                                                                      cvtFIXTURE x1638]
                                                                               ) ls1640)))
       )), ("inits", 
       (case opt1655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1651 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1648, 
                                                                                      x1649) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1648, 
                                                                                      cvtEXPR x1649]
                                                                               ) ls1651)))
       )), ("block", cvtBLOCK x1659)]
   and cvtFUNC_NAME {kind=x1671, ident=x1672} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1671), 
          ("ident", cvtIDENT x1672)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1678, getter=opt1680, setter=opt1685} = 
          PrettyRep.Rec [("ty", cvtTY x1678), ("getter", 
       (case opt1680 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1679 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1679))
       )), ("setter", 
       (case opt1685 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1684 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1684))
       ))]
   and cvtFRAGMENT (Package{name=ls1697, fragments=ls1702}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1696 => 
                                                                        cvtIDENT x1696
                                                                 ) ls1697)), 
          ("fragments", PrettyRep.List (List.map (fn x1701 => cvtFRAGMENT x1701
                                                 ) ls1702))]))
     | cvtFRAGMENT (Anon x1713) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1713))
end

