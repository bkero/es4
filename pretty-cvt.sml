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
   and cvtFUNC (Func{name=x269, fsig=x270, native=b271, generator=b272, block=opt274, 
          param=x278, defaults=ls280, ty=x284, loc=opt286}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x269), ("fsig", cvtFUNC_SIG x270), 
          ("native", PrettyRep.Bool b271), ("generator", PrettyRep.Bool b272), 
          ("block", 
       (case opt274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x273 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x273))
       )), ("param", cvtHEAD x278), ("defaults", PrettyRep.List (List.map (fn x279 => 
                                                                                 cvtEXPR x279
                                                                          ) ls280)), 
          ("ty", cvtTY x284), ("loc", 
       (case opt286 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x285 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x285))
       ))]))
   and cvtDEFN (ClassDefn x311) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x311))
     | cvtDEFN (VariableDefn x314) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x314))
     | cvtDEFN (FunctionDefn x317) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x317))
     | cvtDEFN (ConstructorDefn x320) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x320))
     | cvtDEFN (InterfaceDefn x323) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x323))
     | cvtDEFN (NamespaceDefn x326) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x326))
     | cvtDEFN (TypeDefn x329) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x329))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls333, params=x337, paramTypes=ls339, 
          defaults=ls344, ctorInits=opt355, returnType=x359, thisType=opt361, 
          hasRest=b365}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x332 => cvtIDENT x332
                                   ) ls333)), ("params", cvtBINDINGS x337), 
          ("paramTypes", PrettyRep.List (List.map (fn x338 => cvtTYPE_EXPR x338
                                                  ) ls339)), ("defaults", PrettyRep.List (List.map (fn x343 => 
                                                                                                          cvtEXPR x343
                                                                                                   ) ls344)), 
          ("ctorInits", 
       (case opt355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x348, ls350) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x348, 
            PrettyRep.List (List.map (fn x349 => cvtEXPR x349
                                     ) ls350)]))
       )), ("returnType", cvtTYPE_EXPR x359), ("thisType", 
       (case opt361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x360 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x360))
       )), ("hasRest", PrettyRep.Bool b365)]))
   and cvtBINDING (Binding{ident=x385, ty=x386}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x385), ("ty", cvtTYPE_EXPR x386)]))
   and cvtBINDING_IDENT (TempIdent n394) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n394))
     | cvtBINDING_IDENT (ParamIdent n397) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n397))
     | cvtBINDING_IDENT (PropIdent x400) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x400))
   and cvtINIT_STEP (InitStep(x403, x404)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x403, 
          cvtEXPR x404]))
     | cvtINIT_STEP (AssignStep(x408, x409)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x408, cvtEXPR x409]))
   and cvtTYPE_EXPR (SpecialType x413) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x413))
     | cvtTYPE_EXPR (UnionType ls417) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x416 => 
                                                                                                           cvtTYPE_EXPR x416
                                                                                                    ) ls417)))
     | cvtTYPE_EXPR (ArrayType ls424) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                           cvtTYPE_EXPR x423
                                                                                                    ) ls424)))
     | cvtTYPE_EXPR (TypeName x430) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x430))
     | cvtTYPE_EXPR (ElementTypeRef(x433, n434)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x433, PrettyRep.Int n434]))
     | cvtTYPE_EXPR (FieldTypeRef(x438, x439)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x438, cvtIDENT x439]))
     | cvtTYPE_EXPR (FunctionType x443) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x443))
     | cvtTYPE_EXPR (ObjectType ls447) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x446 => 
                                                                                                             cvtFIELD_TYPE x446
                                                                                                      ) ls447)))
     | cvtTYPE_EXPR (LikeType x453) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x453))
     | cvtTYPE_EXPR (AppType{base=x456, args=ls458}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x456), ("args", PrettyRep.List (List.map (fn x457 => 
                                                                                                     cvtTYPE_EXPR x457
                                                                                              ) ls458))]))
     | cvtTYPE_EXPR (LamType{params=ls470, body=x474}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x469 => 
                                                                          cvtIDENT x469
                                                                   ) ls470)), 
          ("body", cvtTYPE_EXPR x474)]))
     | cvtTYPE_EXPR (NullableType{expr=x482, nullable=b483}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x482), ("nullable", PrettyRep.Bool b483)]))
     | cvtTYPE_EXPR (InstanceType x491) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x491))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x495) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x495))
     | cvtSTMT (InitStmt{kind=x498, ns=opt500, prototype=b504, static=b505, 
          temps=x506, inits=ls508}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x498), ("ns", 
       (case opt500 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x499 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x499))
       )), ("prototype", PrettyRep.Bool b504), ("static", PrettyRep.Bool b505), 
          ("temps", cvtBINDINGS x506), ("inits", PrettyRep.List (List.map (fn x507 => 
                                                                                 cvtINIT_STEP x507
                                                                          ) ls508))]))
     | cvtSTMT (ClassBlock x527) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x527))
     | cvtSTMT (ForInStmt x530) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x530))
     | cvtSTMT (ThrowStmt x533) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x533))
     | cvtSTMT (ReturnStmt x536) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x536))
     | cvtSTMT (BreakStmt opt540) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x539 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x539))
       ))
     | cvtSTMT (ContinueStmt opt547) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x546 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x546))
       ))
     | cvtSTMT (BlockStmt x553) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x553))
     | cvtSTMT (LabeledStmt(x556, x557)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x556, 
          cvtSTMT x557]))
     | cvtSTMT (LetStmt x561) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x561))
     | cvtSTMT (WhileStmt x564) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x564))
     | cvtSTMT (DoWhileStmt x567) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x567))
     | cvtSTMT (ForStmt x570) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x570))
     | cvtSTMT (IfStmt{cnd=x573, thn=x574, els=x575}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x573), ("thn", cvtSTMT x574), 
          ("els", cvtSTMT x575)]))
     | cvtSTMT (WithStmt{obj=x585, ty=x586, body=x587}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x585), ("ty", cvtTY x586), ("body", 
          cvtSTMT x587)]))
     | cvtSTMT (TryStmt{block=x597, catches=ls599, finally=opt604}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x597), ("catches", PrettyRep.List (List.map (fn x598 => 
                                                                                                     cvtCATCH_CLAUSE x598
                                                                                              ) ls599)), 
          ("finally", 
       (case opt604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x603 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x603))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x617, labels=ls619, cases=ls624}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x617), ("labels", PrettyRep.List (List.map (fn x618 => 
                                                                                                  cvtIDENT x618
                                                                                           ) ls619)), 
          ("cases", PrettyRep.List (List.map (fn x623 => cvtCASE x623
                                             ) ls624))]))
     | cvtSTMT (SwitchTypeStmt{cond=x637, ty=x638, cases=ls640}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x637), ("ty", cvtTY x638), 
          ("cases", PrettyRep.List (List.map (fn x639 => cvtCATCH_CLAUSE x639
                                             ) ls640))]))
     | cvtSTMT (DXNStmt{expr=x653}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x653)]))
   and cvtEXPR (TernaryExpr(x659, x660, x661)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x659, cvtEXPR x660, cvtEXPR x661]))
     | cvtEXPR (BinaryExpr(x665, x666, x667)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x665, cvtEXPR x666, cvtEXPR x667]))
     | cvtEXPR (BinaryTypeExpr(x671, x672, x673)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x671, cvtEXPR x672, cvtTY x673]))
     | cvtEXPR (UnaryExpr(x677, x678)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x677, 
          cvtEXPR x678]))
     | cvtEXPR (TypeExpr x682) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x682))
     | cvtEXPR (ThisExpr opt686) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt686 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x685 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x685))
       ))
     | cvtEXPR (YieldExpr opt693) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x692 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x692))
       ))
     | cvtEXPR (SuperExpr opt700) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x699 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x699))
       ))
     | cvtEXPR (LiteralExpr x706) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x706))
     | cvtEXPR (CallExpr{func=x709, actuals=ls711}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x709), ("actuals", PrettyRep.List (List.map (fn x710 => 
                                                                                                   cvtEXPR x710
                                                                                            ) ls711))]))
     | cvtEXPR (ApplyTypeExpr{expr=x722, actuals=ls724}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x722), ("actuals", PrettyRep.List (List.map (fn x723 => 
                                                                                                   cvtTY x723
                                                                                            ) ls724))]))
     | cvtEXPR (LetExpr{defs=x735, body=x736, head=opt738}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x735), ("body", cvtEXPR x736), 
          ("head", 
       (case opt738 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x737 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x737))
       ))]))
     | cvtEXPR (NewExpr{obj=x751, actuals=ls753}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x751), ("actuals", PrettyRep.List (List.map (fn x752 => 
                                                                                                  cvtEXPR x752
                                                                                           ) ls753))]))
     | cvtEXPR (ObjectRef{base=x764, ident=x765, loc=opt767}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x764), ("ident", cvtIDENT_EXPR x765), 
          ("loc", 
       (case opt767 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x766 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x766))
       ))]))
     | cvtEXPR (LexicalRef{ident=x780, loc=opt782}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x780), ("loc", 
       (case opt782 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x781 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x781))
       ))]))
     | cvtEXPR (SetExpr(x793, x794, x795)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x793, 
          cvtEXPR x794, cvtEXPR x795]))
     | cvtEXPR (ListExpr ls800) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x799 => 
                                                                                                    cvtEXPR x799
                                                                                             ) ls800)))
     | cvtEXPR (InitExpr(x806, x807, x808)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x806, 
          cvtHEAD x807, cvtINITS x808]))
     | cvtEXPR (GetTemp n812) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n812))
     | cvtEXPR (GetParam n815) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n815))
     | cvtEXPR (Comprehension(x818, ls820, opt825)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x818, PrettyRep.List (List.map (fn x819 => 
                                                                               cvtFOR_ENUM_HEAD x819
                                                                        ) ls820), 
          
       (case opt825 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x824 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x824))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n837) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n837))
     | cvtFIXTURE_NAME (PropName x840) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x840))
   and cvtIDENT_EXPR (Identifier{ident=x843, openNamespaces=ls849}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x843), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls845 => PrettyRep.List (List.map (fn x844 => 
                                                                                cvtNAMESPACE x844
                                                                         ) ls845)
                                   ) ls849))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x860, expr=x861}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x860), ("expr", cvtEXPR x861)]))
     | cvtIDENT_EXPR (AttributeIdentifier x869) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x869))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x872, openNamespaces=ls878}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x872), ("openNamespaces", PrettyRep.List (List.map (fn ls874 => 
                                                                            PrettyRep.List (List.map (fn x873 => 
                                                                                                            cvtNAMESPACE x873
                                                                                                     ) ls874)
                                                                     ) ls878))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x889, ident=s890}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x889), ("ident", PrettyRep.UniStr s890)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls899, x903)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x898 => cvtIDENT x898
                                                          ) ls899), cvtIDENT_EXPR x903]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r910) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r910))
     | cvtLITERAL (LiteralDecimal d913) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d913))
     | cvtLITERAL (LiteralBoolean b916) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b916))
     | cvtLITERAL (LiteralString s919) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s919))
     | cvtLITERAL (LiteralArray{exprs=x922, ty=opt924}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x922), ("ty", 
       (case opt924 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x923 => PrettyRep.Ctor ("SOME", SOME (cvtTY x923))
       ))]))
     | cvtLITERAL (LiteralXML ls936) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x935 => 
                                                                                                           cvtEXPR x935
                                                                                                    ) ls936)))
     | cvtLITERAL (LiteralNamespace x942) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x942))
     | cvtLITERAL (LiteralObject{expr=ls946, ty=opt951}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x945 => 
                                                                        cvtFIELD x945
                                                                 ) ls946)), 
          ("ty", 
       (case opt951 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x950 => PrettyRep.Ctor ("SOME", SOME (cvtTY x950))
       ))]))
     | cvtLITERAL (LiteralFunction x962) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x962))
     | cvtLITERAL (LiteralRegExp{str=s965}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s965)]))
   and cvtBLOCK (Block x971) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x971))
   and cvtFIXTURE (NamespaceFixture x974) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x974))
     | cvtFIXTURE (ClassFixture x977) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x977))
     | cvtFIXTURE (InterfaceFixture x980) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x980))
     | cvtFIXTURE (TypeVarFixture x983) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x983))
     | cvtFIXTURE (TypeFixture x986) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x986))
     | cvtFIXTURE (MethodFixture{func=x989, ty=x990, readOnly=b991, override=b992, 
          final=b993}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x989), ("ty", cvtTY x990), ("readOnly", PrettyRep.Bool b991), 
          ("override", PrettyRep.Bool b992), ("final", PrettyRep.Bool b993)]))
     | cvtFIXTURE (ValFixture{ty=x1007, readOnly=b1008}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1007), ("readOnly", PrettyRep.Bool b1008)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1016, getter=opt1018, setter=opt1023}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1016), ("getter", 
       (case opt1018 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1017 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1017))
       )), ("setter", 
       (case opt1023 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1022 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1022))
       ))]))
   and cvtHEAD (Head(x1036, x1037)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1036, 
          cvtINITS x1037]))
   and cvtBINDINGS (ls1042, ls1047) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1041 => 
                                                                                       cvtBINDING x1041
                                                                                ) ls1042), 
          PrettyRep.List (List.map (fn x1046 => cvtINIT_STEP x1046
                                   ) ls1047)]
   and cvtRIB ls1055 = PrettyRep.List (List.map (fn (x1052, x1053) => PrettyRep.Tuple [cvtFIXTURE_NAME x1052, 
                                                       cvtFIXTURE x1053]
                                                ) ls1055)
   and cvtRIBS ls1066 = PrettyRep.List (List.map (fn ls1062 => PrettyRep.List (List.map (fn (x1059, 
                                                                                               x1060) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1059, 
                                                                                               cvtFIXTURE x1060]
                                                                                        ) ls1062)
                                                 ) ls1066)
   and cvtINITS ls1073 = PrettyRep.List (List.map (fn (x1070, x1071) => PrettyRep.Tuple [cvtFIXTURE_NAME x1070, 
                                                         cvtEXPR x1071]
                                                  ) ls1073)
   and cvtINSTANCE_TYPE {name=x1077, typeParams=ls1079, typeArgs=ls1084, nonnullable=b1088, 
          superTypes=ls1090, ty=x1094, dynamic=b1095} = PrettyRep.Rec [("name", 
          cvtNAME x1077), ("typeParams", PrettyRep.List (List.map (fn x1078 => 
                                                                         cvtIDENT x1078
                                                                  ) ls1079)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1083 => cvtTYPE_EXPR x1083
                                                ) ls1084)), ("nonnullable", 
          PrettyRep.Bool b1088), ("superTypes", PrettyRep.List (List.map (fn x1089 => 
                                                                                cvtTYPE_EXPR x1089
                                                                         ) ls1090)), 
          ("ty", cvtTYPE_EXPR x1094), ("dynamic", PrettyRep.Bool b1095)]
   and cvtFIELD {kind=x1111, name=x1112, init=x1113} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1111), ("name", cvtIDENT_EXPR x1112), ("init", cvtEXPR x1113)]
   and cvtFIELD_TYPE {name=x1121, ty=x1122} = PrettyRep.Rec [("name", cvtIDENT x1121), 
          ("ty", cvtTYPE_EXPR x1122)]
   and cvtFUNC_TYPE {params=ls1129, result=x1133, thisType=opt1135, hasRest=b1139, 
          minArgs=n1140} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1128 => 
                                                                                     cvtTYPE_EXPR x1128
                                                                              ) ls1129)), 
          ("result", cvtTYPE_EXPR x1133), ("thisType", 
       (case opt1135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1134 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1134))
       )), ("hasRest", PrettyRep.Bool b1139), ("minArgs", PrettyRep.Int n1140)]
   and cvtFUNC_DEFN {kind=x1152, ns=opt1154, final=b1158, override=b1159, prototype=b1160, 
          static=b1161, func=x1162} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1152), 
          ("ns", 
       (case opt1154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1153 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1153))
       )), ("final", PrettyRep.Bool b1158), ("override", PrettyRep.Bool b1159), 
          ("prototype", PrettyRep.Bool b1160), ("static", PrettyRep.Bool b1161), 
          ("func", cvtFUNC x1162)]
   and cvtCTOR_DEFN x1178 = cvtCTOR x1178
   and cvtVAR_DEFN {kind=x1179, ns=opt1181, static=b1185, prototype=b1186, 
          bindings=(ls1188, ls1193)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1179), 
          ("ns", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1180))
       )), ("static", PrettyRep.Bool b1185), ("prototype", PrettyRep.Bool b1186), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1187 => 
                                                                        cvtBINDING x1187
                                                                 ) ls1188), 
          PrettyRep.List (List.map (fn x1192 => cvtINIT_STEP x1192
                                   ) ls1193)])]
   and cvtNAMESPACE_DEFN {ident=x1209, ns=opt1211, init=opt1216} = PrettyRep.Rec [("ident", 
          cvtIDENT x1209), ("ns", 
       (case opt1211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1210 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1210))
       )), ("init", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1215))
       ))]
   and cvtCLASS_DEFN {ns=opt1228, ident=x1232, nonnullable=b1233, dynamic=b1234, 
          final=b1235, params=ls1237, extends=opt1242, implements=ls1247, classDefns=ls1252, 
          instanceDefns=ls1257, instanceStmts=ls1262, ctorDefn=opt1267} = PrettyRep.Rec [("ns", 
          
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1227))
       )), ("ident", cvtIDENT x1232), ("nonnullable", PrettyRep.Bool b1233), 
          ("dynamic", PrettyRep.Bool b1234), ("final", PrettyRep.Bool b1235), 
          ("params", PrettyRep.List (List.map (fn x1236 => cvtIDENT x1236
                                              ) ls1237)), ("extends", 
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1241))
       )), ("implements", PrettyRep.List (List.map (fn x1246 => cvtTYPE_EXPR x1246
                                                   ) ls1247)), ("classDefns", 
          PrettyRep.List (List.map (fn x1251 => cvtDEFN x1251
                                   ) ls1252)), ("instanceDefns", PrettyRep.List (List.map (fn x1256 => 
                                                                                                 cvtDEFN x1256
                                                                                          ) ls1257)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1261 => cvtSTMT x1261
                                                     ) ls1262)), ("ctorDefn", 
          
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1266))
       ))]
   and cvtINTERFACE_DEFN {ident=x1296, ns=opt1298, nonnullable=b1302, params=ls1304, 
          extends=ls1309, instanceDefns=ls1314} = PrettyRep.Rec [("ident", 
          cvtIDENT x1296), ("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1297))
       )), ("nonnullable", PrettyRep.Bool b1302), ("params", PrettyRep.List (List.map (fn x1303 => 
                                                                                             cvtIDENT x1303
                                                                                      ) ls1304)), 
          ("extends", PrettyRep.List (List.map (fn x1308 => cvtTYPE_EXPR x1308
                                               ) ls1309)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1313 => cvtDEFN x1313
                                   ) ls1314))]
   and cvtTYPE_DEFN {ident=x1331, ns=opt1333, init=x1337} = PrettyRep.Rec [("ident", 
          cvtIDENT x1331), ("ns", 
       (case opt1333 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1332 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1332))
       )), ("init", cvtTYPE_EXPR x1337)]
   and cvtCLASS_BLOCK {ns=opt1346, ident=x1350, name=opt1352, block=x1356} = 
          PrettyRep.Rec [("ns", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1345))
       )), ("ident", cvtIDENT x1350), ("name", 
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1351))
       )), ("block", cvtBLOCK x1356)]
   and cvtFOR_ENUM_HEAD {isEach=b1366, bindings=(ls1368, ls1373), expr=x1378} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1366), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1367 => 
                                                                                                                         cvtBINDING x1367
                                                                                                                  ) ls1368), 
          PrettyRep.List (List.map (fn x1372 => cvtINIT_STEP x1372
                                   ) ls1373)]), ("expr", cvtEXPR x1378)]
   and cvtFOR_ENUM_STMT {isEach=b1386, defn=opt1417, obj=x1421, rib=opt1429, 
          next=x1433, labels=ls1435, body=x1439} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1386), ("defn", 
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1387, ns=opt1389, static=b1393, prototype=b1394, bindings=(ls1396, 
            ls1401)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1387), ("ns", 
         (case opt1389 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1388 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1388))
         )), ("static", PrettyRep.Bool b1393), ("prototype", PrettyRep.Bool b1394), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1395 => 
                                                                          cvtBINDING x1395
                                                                   ) ls1396), 
            PrettyRep.List (List.map (fn x1400 => cvtINIT_STEP x1400
                                     ) ls1401)])]))
       )), ("obj", cvtEXPR x1421), ("rib", 
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1425 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1422, 
                                                                                      x1423) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1422, 
                                                                                      cvtFIXTURE x1423]
                                                                               ) ls1425)))
       )), ("next", cvtSTMT x1433), ("labels", PrettyRep.List (List.map (fn x1434 => 
                                                                               cvtIDENT x1434
                                                                        ) ls1435)), 
          ("body", cvtSTMT x1439)]
   and cvtFOR_STMT {rib=opt1462, defn=opt1496, init=ls1501, cond=x1505, update=x1506, 
          labels=ls1508, body=x1512} = PrettyRep.Rec [("rib", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1458 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1455, 
                                                                                      x1456) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1455, 
                                                                                      cvtFIXTURE x1456]
                                                                               ) ls1458)))
       )), ("defn", 
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1466, ns=opt1468, static=b1472, prototype=b1473, bindings=(ls1475, 
            ls1480)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1466), ("ns", 
         (case opt1468 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1467))
         )), ("static", PrettyRep.Bool b1472), ("prototype", PrettyRep.Bool b1473), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1474 => 
                                                                          cvtBINDING x1474
                                                                   ) ls1475), 
            PrettyRep.List (List.map (fn x1479 => cvtINIT_STEP x1479
                                     ) ls1480)])]))
       )), ("init", PrettyRep.List (List.map (fn x1500 => cvtSTMT x1500
                                             ) ls1501)), ("cond", cvtEXPR x1505), 
          ("update", cvtEXPR x1506), ("labels", PrettyRep.List (List.map (fn x1507 => 
                                                                                cvtIDENT x1507
                                                                         ) ls1508)), 
          ("body", cvtSTMT x1512)]
   and cvtWHILE_STMT {cond=x1528, rib=opt1536, body=x1540, labels=ls1542} = 
          PrettyRep.Rec [("cond", cvtEXPR x1528), ("rib", 
       (case opt1536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1532 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1529, 
                                                                                      x1530) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1529, 
                                                                                      cvtFIXTURE x1530]
                                                                               ) ls1532)))
       )), ("body", cvtSTMT x1540), ("labels", PrettyRep.List (List.map (fn x1541 => 
                                                                               cvtIDENT x1541
                                                                        ) ls1542))]
   and cvtDIRECTIVES {pragmas=ls1556, defns=ls1561, head=opt1566, body=ls1571, 
          loc=opt1576} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1555 => 
                                                                                    cvtPRAGMA x1555
                                                                             ) ls1556)), 
          ("defns", PrettyRep.List (List.map (fn x1560 => cvtDEFN x1560
                                             ) ls1561)), ("head", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1565))
       )), ("body", PrettyRep.List (List.map (fn x1570 => cvtSTMT x1570
                                             ) ls1571)), ("loc", 
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1575 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1575))
       ))]
   and cvtCASE {label=opt1592, inits=opt1603, body=x1607} = PrettyRep.Rec [("label", 
          
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1591))
       )), ("inits", 
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1599 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1596, 
                                                                                      x1597) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1596, 
                                                                                      cvtEXPR x1597]
                                                                               ) ls1599)))
       )), ("body", cvtBLOCK x1607)]
   and cvtCATCH_CLAUSE {bindings=(ls1616, ls1621), ty=x1626, rib=opt1634, inits=opt1645, 
          block=x1649} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1615 => 
                                                                                                      cvtBINDING x1615
                                                                                               ) ls1616), 
          PrettyRep.List (List.map (fn x1620 => cvtINIT_STEP x1620
                                   ) ls1621)]), ("ty", cvtTY x1626), ("rib", 
          
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1630 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1627, 
                                                                                      x1628) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1627, 
                                                                                      cvtFIXTURE x1628]
                                                                               ) ls1630)))
       )), ("inits", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1641 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1638, 
                                                                                      x1639) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1638, 
                                                                                      cvtEXPR x1639]
                                                                               ) ls1641)))
       )), ("block", cvtBLOCK x1649)]
   and cvtFUNC_NAME {kind=x1661, ident=x1662} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1661), 
          ("ident", cvtIDENT x1662)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1668, getter=opt1670, setter=opt1675} = 
          PrettyRep.Rec [("ty", cvtTY x1668), ("getter", 
       (case opt1670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1669 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1669))
       )), ("setter", 
       (case opt1675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1674 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1674))
       ))]
   and cvtFRAGMENT (Package{name=ls1687, fragments=ls1692}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1686 => 
                                                                        cvtIDENT x1686
                                                                 ) ls1687)), 
          ("fragments", PrettyRep.List (List.map (fn x1691 => cvtFRAGMENT x1691
                                                 ) ls1692))]))
     | cvtFRAGMENT (Anon x1703) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1703))
end

