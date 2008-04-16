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
     | cvtPRAGMA (UseDecimalContext x135) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x135))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls141, name=x145}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x140 => 
                                                                           cvtIDENT x140
                                                                    ) ls141)), 
          ("name", cvtIDENT x145)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x159, ribId=opt161}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x159), ("ribId", 
       (case opt161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x160 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x160))
       ))]))
   and cvtCLS (Cls{name=x172, typeParams=ls174, nonnullable=b178, dynamic=b179, 
          extends=opt181, implements=ls186, classRib=x190, instanceRib=x191, 
          instanceInits=x192, constructor=opt194, classType=x198, instanceType=x199}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x172), 
          ("typeParams", PrettyRep.List (List.map (fn x173 => cvtIDENT x173
                                                  ) ls174)), ("nonnullable", 
          PrettyRep.Bool b178), ("dynamic", PrettyRep.Bool b179), ("extends", 
          
       (case opt181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x180 => PrettyRep.Ctor ("SOME", SOME (cvtTY x180))
       )), ("implements", PrettyRep.List (List.map (fn x185 => cvtTY x185
                                                   ) ls186)), ("classRib", 
          cvtRIB x190), ("instanceRib", cvtRIB x191), ("instanceInits", cvtHEAD x192), 
          ("constructor", 
       (case opt194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x193 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x193))
       )), ("classType", cvtTY x198), ("instanceType", cvtTY x199)]))
   and cvtIFACE (Iface{name=x227, typeParams=ls229, nonnullable=b233, extends=ls235, 
          instanceRib=x239, instanceType=x240}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x227), ("typeParams", PrettyRep.List (List.map (fn x228 => 
                                                                                                      cvtIDENT x228
                                                                                               ) ls229)), 
          ("nonnullable", PrettyRep.Bool b233), ("extends", PrettyRep.List (List.map (fn x234 => 
                                                                                            cvtTY x234
                                                                                     ) ls235)), 
          ("instanceRib", cvtRIB x239), ("instanceType", cvtTY x240)]))
   and cvtCTOR (Ctor{settings=x256, superArgs=ls258, func=x262}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x256), ("superArgs", PrettyRep.List (List.map (fn x257 => 
                                                                                                         cvtEXPR x257
                                                                                                  ) ls258)), 
          ("func", cvtFUNC x262)]))
   and cvtFUNC (Func{name=x272, fsig=x273, native=b274, block=opt276, param=x280, 
          defaults=ls282, ty=x286, loc=opt288}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x272), ("fsig", cvtFUNC_SIG x273), ("native", PrettyRep.Bool b274), 
          ("block", 
       (case opt276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x275 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x275))
       )), ("param", cvtHEAD x280), ("defaults", PrettyRep.List (List.map (fn x281 => 
                                                                                 cvtEXPR x281
                                                                          ) ls282)), 
          ("ty", cvtTY x286), ("loc", 
       (case opt288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x287 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x287))
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
     | cvtFIXTURE (InheritedFixture{baseName=x1036, baseTypeArgs=ls1038}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1036), ("baseTypeArgs", PrettyRep.List (List.map (fn x1037 => 
                                                                           cvtTY x1037
                                                                    ) ls1038))]))
   and cvtHEAD (Head(x1049, x1050)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1049, 
          cvtINITS x1050]))
   and cvtBINDINGS (ls1055, ls1060) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1054 => 
                                                                                       cvtBINDING x1054
                                                                                ) ls1055), 
          PrettyRep.List (List.map (fn x1059 => cvtINIT_STEP x1059
                                   ) ls1060)]
   and cvtRIB ls1068 = PrettyRep.List (List.map (fn (x1065, x1066) => PrettyRep.Tuple [cvtFIXTURE_NAME x1065, 
                                                       cvtFIXTURE x1066]
                                                ) ls1068)
   and cvtRIBS ls1079 = PrettyRep.List (List.map (fn ls1075 => PrettyRep.List (List.map (fn (x1072, 
                                                                                               x1073) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1072, 
                                                                                               cvtFIXTURE x1073]
                                                                                        ) ls1075)
                                                 ) ls1079)
   and cvtINITS ls1086 = PrettyRep.List (List.map (fn (x1083, x1084) => PrettyRep.Tuple [cvtFIXTURE_NAME x1083, 
                                                         cvtEXPR x1084]
                                                  ) ls1086)
   and cvtINSTANCE_TYPE {name=x1090, typeParams=ls1092, typeArgs=ls1097, nonnullable=b1101, 
          superTypes=ls1103, ty=x1107, dynamic=b1108} = PrettyRep.Rec [("name", 
          cvtNAME x1090), ("typeParams", PrettyRep.List (List.map (fn x1091 => 
                                                                         cvtIDENT x1091
                                                                  ) ls1092)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1096 => cvtTYPE_EXPR x1096
                                                ) ls1097)), ("nonnullable", 
          PrettyRep.Bool b1101), ("superTypes", PrettyRep.List (List.map (fn x1102 => 
                                                                                cvtTYPE_EXPR x1102
                                                                         ) ls1103)), 
          ("ty", cvtTYPE_EXPR x1107), ("dynamic", PrettyRep.Bool b1108)]
   and cvtFIELD {kind=x1124, name=x1125, init=x1126} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1124), ("name", cvtIDENT_EXPR x1125), ("init", cvtEXPR x1126)]
   and cvtFIELD_TYPE {name=x1134, ty=x1135} = PrettyRep.Rec [("name", cvtIDENT x1134), 
          ("ty", cvtTYPE_EXPR x1135)]
   and cvtFUNC_TYPE {params=ls1142, result=x1146, thisType=opt1148, hasRest=b1152, 
          minArgs=n1153} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1141 => 
                                                                                     cvtTYPE_EXPR x1141
                                                                              ) ls1142)), 
          ("result", cvtTYPE_EXPR x1146), ("thisType", 
       (case opt1148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1147 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1147))
       )), ("hasRest", PrettyRep.Bool b1152), ("minArgs", PrettyRep.Int n1153)]
   and cvtFUNC_DEFN {kind=x1165, ns=opt1167, final=b1171, override=b1172, prototype=b1173, 
          static=b1174, func=x1175} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1165), 
          ("ns", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1166))
       )), ("final", PrettyRep.Bool b1171), ("override", PrettyRep.Bool b1172), 
          ("prototype", PrettyRep.Bool b1173), ("static", PrettyRep.Bool b1174), 
          ("func", cvtFUNC x1175)]
   and cvtCTOR_DEFN x1191 = cvtCTOR x1191
   and cvtVAR_DEFN {kind=x1192, ns=opt1194, static=b1198, prototype=b1199, 
          bindings=(ls1201, ls1206)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1192), 
          ("ns", 
       (case opt1194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1193 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1193))
       )), ("static", PrettyRep.Bool b1198), ("prototype", PrettyRep.Bool b1199), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1200 => 
                                                                        cvtBINDING x1200
                                                                 ) ls1201), 
          PrettyRep.List (List.map (fn x1205 => cvtINIT_STEP x1205
                                   ) ls1206)])]
   and cvtNAMESPACE_DEFN {ident=x1222, ns=opt1224, init=opt1229} = PrettyRep.Rec [("ident", 
          cvtIDENT x1222), ("ns", 
       (case opt1224 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1223 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1223))
       )), ("init", 
       (case opt1229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1228 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1228))
       ))]
   and cvtCLASS_DEFN {ns=opt1241, ident=x1245, nonnullable=b1246, dynamic=b1247, 
          final=b1248, params=ls1250, extends=opt1255, implements=ls1260, classDefns=ls1265, 
          instanceDefns=ls1270, instanceStmts=ls1275, ctorDefn=opt1280} = PrettyRep.Rec [("ns", 
          
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1240))
       )), ("ident", cvtIDENT x1245), ("nonnullable", PrettyRep.Bool b1246), 
          ("dynamic", PrettyRep.Bool b1247), ("final", PrettyRep.Bool b1248), 
          ("params", PrettyRep.List (List.map (fn x1249 => cvtIDENT x1249
                                              ) ls1250)), ("extends", 
       (case opt1255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1254 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1254))
       )), ("implements", PrettyRep.List (List.map (fn x1259 => cvtTYPE_EXPR x1259
                                                   ) ls1260)), ("classDefns", 
          PrettyRep.List (List.map (fn x1264 => cvtDEFN x1264
                                   ) ls1265)), ("instanceDefns", PrettyRep.List (List.map (fn x1269 => 
                                                                                                 cvtDEFN x1269
                                                                                          ) ls1270)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1274 => cvtSTMT x1274
                                                     ) ls1275)), ("ctorDefn", 
          
       (case opt1280 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1279 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1279))
       ))]
   and cvtINTERFACE_DEFN {ident=x1309, ns=opt1311, nonnullable=b1315, params=ls1317, 
          extends=ls1322, instanceDefns=ls1327} = PrettyRep.Rec [("ident", 
          cvtIDENT x1309), ("ns", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1310))
       )), ("nonnullable", PrettyRep.Bool b1315), ("params", PrettyRep.List (List.map (fn x1316 => 
                                                                                             cvtIDENT x1316
                                                                                      ) ls1317)), 
          ("extends", PrettyRep.List (List.map (fn x1321 => cvtTYPE_EXPR x1321
                                               ) ls1322)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1326 => cvtDEFN x1326
                                   ) ls1327))]
   and cvtTYPE_DEFN {ident=x1344, ns=opt1346, init=x1350} = PrettyRep.Rec [("ident", 
          cvtIDENT x1344), ("ns", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1345))
       )), ("init", cvtTYPE_EXPR x1350)]
   and cvtCLASS_BLOCK {ns=opt1359, ident=x1363, name=opt1365, block=x1369} = 
          PrettyRep.Rec [("ns", 
       (case opt1359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1358))
       )), ("ident", cvtIDENT x1363), ("name", 
       (case opt1365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1364))
       )), ("block", cvtBLOCK x1369)]
   and cvtFOR_ENUM_HEAD {isEach=b1379, bindings=(ls1381, ls1386), expr=x1391} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1379), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1380 => 
                                                                                                                         cvtBINDING x1380
                                                                                                                  ) ls1381), 
          PrettyRep.List (List.map (fn x1385 => cvtINIT_STEP x1385
                                   ) ls1386)]), ("expr", cvtEXPR x1391)]
   and cvtFOR_ENUM_STMT {isEach=b1399, defn=opt1430, obj=x1434, rib=opt1442, 
          next=x1446, labels=ls1448, body=x1452} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1399), ("defn", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1400, ns=opt1402, static=b1406, prototype=b1407, bindings=(ls1409, 
            ls1414)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1400), ("ns", 
         (case opt1402 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1401 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1401))
         )), ("static", PrettyRep.Bool b1406), ("prototype", PrettyRep.Bool b1407), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1408 => 
                                                                          cvtBINDING x1408
                                                                   ) ls1409), 
            PrettyRep.List (List.map (fn x1413 => cvtINIT_STEP x1413
                                     ) ls1414)])]))
       )), ("obj", cvtEXPR x1434), ("rib", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1438 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1435, 
                                                                                      x1436) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1435, 
                                                                                      cvtFIXTURE x1436]
                                                                               ) ls1438)))
       )), ("next", cvtSTMT x1446), ("labels", PrettyRep.List (List.map (fn x1447 => 
                                                                               cvtIDENT x1447
                                                                        ) ls1448)), 
          ("body", cvtSTMT x1452)]
   and cvtFOR_STMT {rib=opt1475, defn=opt1509, init=ls1514, cond=x1518, update=x1519, 
          labels=ls1521, body=x1525} = PrettyRep.Rec [("rib", 
       (case opt1475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1471 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1468, 
                                                                                      x1469) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1468, 
                                                                                      cvtFIXTURE x1469]
                                                                               ) ls1471)))
       )), ("defn", 
       (case opt1509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1479, ns=opt1481, static=b1485, prototype=b1486, bindings=(ls1488, 
            ls1493)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1479), ("ns", 
         (case opt1481 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1480 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1480))
         )), ("static", PrettyRep.Bool b1485), ("prototype", PrettyRep.Bool b1486), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1487 => 
                                                                          cvtBINDING x1487
                                                                   ) ls1488), 
            PrettyRep.List (List.map (fn x1492 => cvtINIT_STEP x1492
                                     ) ls1493)])]))
       )), ("init", PrettyRep.List (List.map (fn x1513 => cvtSTMT x1513
                                             ) ls1514)), ("cond", cvtEXPR x1518), 
          ("update", cvtEXPR x1519), ("labels", PrettyRep.List (List.map (fn x1520 => 
                                                                                cvtIDENT x1520
                                                                         ) ls1521)), 
          ("body", cvtSTMT x1525)]
   and cvtWHILE_STMT {cond=x1541, rib=opt1549, body=x1553, labels=ls1555} = 
          PrettyRep.Rec [("cond", cvtEXPR x1541), ("rib", 
       (case opt1549 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1545 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1542, 
                                                                                      x1543) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1542, 
                                                                                      cvtFIXTURE x1543]
                                                                               ) ls1545)))
       )), ("body", cvtSTMT x1553), ("labels", PrettyRep.List (List.map (fn x1554 => 
                                                                               cvtIDENT x1554
                                                                        ) ls1555))]
   and cvtDIRECTIVES {pragmas=ls1569, defns=ls1574, head=opt1579, body=ls1584, 
          loc=opt1589} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1568 => 
                                                                                    cvtPRAGMA x1568
                                                                             ) ls1569)), 
          ("defns", PrettyRep.List (List.map (fn x1573 => cvtDEFN x1573
                                             ) ls1574)), ("head", 
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1578 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1578))
       )), ("body", PrettyRep.List (List.map (fn x1583 => cvtSTMT x1583
                                             ) ls1584)), ("loc", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1588))
       ))]
   and cvtCASE {label=opt1605, inits=opt1616, body=x1620} = PrettyRep.Rec [("label", 
          
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1604 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1604))
       )), ("inits", 
       (case opt1616 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1612 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1609, 
                                                                                      x1610) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1609, 
                                                                                      cvtEXPR x1610]
                                                                               ) ls1612)))
       )), ("body", cvtBLOCK x1620)]
   and cvtCATCH_CLAUSE {bindings=(ls1629, ls1634), ty=x1639, rib=opt1647, inits=opt1658, 
          block=x1662} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1628 => 
                                                                                                      cvtBINDING x1628
                                                                                               ) ls1629), 
          PrettyRep.List (List.map (fn x1633 => cvtINIT_STEP x1633
                                   ) ls1634)]), ("ty", cvtTY x1639), ("rib", 
          
       (case opt1647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1643 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1640, 
                                                                                      x1641) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1640, 
                                                                                      cvtFIXTURE x1641]
                                                                               ) ls1643)))
       )), ("inits", 
       (case opt1658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1654 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1651, 
                                                                                      x1652) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1651, 
                                                                                      cvtEXPR x1652]
                                                                               ) ls1654)))
       )), ("block", cvtBLOCK x1662)]
   and cvtFUNC_NAME {kind=x1674, ident=x1675} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1674), 
          ("ident", cvtIDENT x1675)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1681, getter=opt1683, setter=opt1688} = 
          PrettyRep.Rec [("ty", cvtTY x1681), ("getter", 
       (case opt1683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1682 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1682))
       )), ("setter", 
       (case opt1688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1687 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1687))
       ))]
   and cvtFRAGMENT (Package{name=ls1700, fragments=ls1705}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1699 => 
                                                                        cvtIDENT x1699
                                                                 ) ls1700)), 
          ("fragments", PrettyRep.List (List.map (fn x1704 => cvtFRAGMENT x1704
                                                 ) ls1705))]))
     | cvtFRAGMENT (Anon x1716) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1716))
end

