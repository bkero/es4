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
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x29) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x29))
     | cvtNAMESPACE (Protected x32) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x32))
     | cvtNAMESPACE (Public x35) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x35))
     | cvtNAMESPACE (Internal x38) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x38))
     | cvtNAMESPACE (UserNamespace s41) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s41))
     | cvtNAMESPACE (AnonUserNamespace n44) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n44))
     | cvtNAMESPACE (LimitedNamespace(x47, x48)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x47, cvtNAMESPACE x48]))
   and cvtNAME {ns=x52, id=x53} = PrettyRep.Rec [("ns", cvtNAMESPACE x52), 
          ("id", cvtIDENT x53)]
   and cvtMULTINAME {nss=ls64, id=x68} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls60 => 
                                                                                                PrettyRep.List (List.map (fn x59 => 
                                                                                                                                cvtNAMESPACE x59
                                                                                                                         ) ls60)
                                                                                         ) ls64)), 
          ("id", cvtIDENT x68)]
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
   and cvtPRAGMA (UseNamespace x137) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x137))
     | cvtPRAGMA (UseDefaultNamespace x140) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x140))
     | cvtPRAGMA (UseDecimalContext x143) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x143))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls149, name=x153}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x148 => 
                                                                           cvtIDENT x148
                                                                    ) ls149)), 
          ("name", cvtIDENT x153)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x167, ribId=opt169}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x167), ("ribId", 
       (case opt169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x168 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x168))
       ))]))
   and cvtCLS (Cls{name=x180, typeParams=ls182, nonnullable=b186, dynamic=b187, 
          extends=opt189, implements=ls194, classRib=x198, instanceRib=x199, 
          instanceInits=x200, constructor=opt202, classType=x206, instanceType=x207}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x180), 
          ("typeParams", PrettyRep.List (List.map (fn x181 => cvtIDENT x181
                                                  ) ls182)), ("nonnullable", 
          PrettyRep.Bool b186), ("dynamic", PrettyRep.Bool b187), ("extends", 
          
       (case opt189 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x188 => PrettyRep.Ctor ("SOME", SOME (cvtTY x188))
       )), ("implements", PrettyRep.List (List.map (fn x193 => cvtTY x193
                                                   ) ls194)), ("classRib", 
          cvtRIB x198), ("instanceRib", cvtRIB x199), ("instanceInits", cvtHEAD x200), 
          ("constructor", 
       (case opt202 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x201 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x201))
       )), ("classType", cvtTY x206), ("instanceType", cvtTY x207)]))
   and cvtIFACE (Iface{name=x235, typeParams=ls237, nonnullable=b241, extends=ls243, 
          instanceRib=x247, instanceType=x248}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x235), ("typeParams", PrettyRep.List (List.map (fn x236 => 
                                                                                                      cvtIDENT x236
                                                                                               ) ls237)), 
          ("nonnullable", PrettyRep.Bool b241), ("extends", PrettyRep.List (List.map (fn x242 => 
                                                                                            cvtTY x242
                                                                                     ) ls243)), 
          ("instanceRib", cvtRIB x247), ("instanceType", cvtTY x248)]))
   and cvtCTOR (Ctor{settings=x264, superArgs=ls266, func=x270}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x264), ("superArgs", PrettyRep.List (List.map (fn x265 => 
                                                                                                         cvtEXPR x265
                                                                                                  ) ls266)), 
          ("func", cvtFUNC x270)]))
   and cvtFUNC (Func{name=x280, fsig=x281, native=b282, block=opt284, param=x288, 
          defaults=ls290, ty=x294, loc=opt296}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x280), ("fsig", cvtFUNC_SIG x281), ("native", PrettyRep.Bool b282), 
          ("block", 
       (case opt284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x283 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x283))
       )), ("param", cvtHEAD x288), ("defaults", PrettyRep.List (List.map (fn x289 => 
                                                                                 cvtEXPR x289
                                                                          ) ls290)), 
          ("ty", cvtTY x294), ("loc", 
       (case opt296 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x295 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x295))
       ))]))
   and cvtDEFN (ClassDefn x319) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x319))
     | cvtDEFN (VariableDefn x322) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x322))
     | cvtDEFN (FunctionDefn x325) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x325))
     | cvtDEFN (ConstructorDefn x328) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x328))
     | cvtDEFN (InterfaceDefn x331) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x331))
     | cvtDEFN (NamespaceDefn x334) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x334))
     | cvtDEFN (TypeDefn x337) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x337))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls341, params=x345, paramTypes=ls347, 
          defaults=ls352, ctorInits=opt363, returnType=x367, thisType=opt369, 
          hasRest=b373}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x340 => cvtIDENT x340
                                   ) ls341)), ("params", cvtBINDINGS x345), 
          ("paramTypes", PrettyRep.List (List.map (fn x346 => cvtTYPE_EXPR x346
                                                  ) ls347)), ("defaults", PrettyRep.List (List.map (fn x351 => 
                                                                                                          cvtEXPR x351
                                                                                                   ) ls352)), 
          ("ctorInits", 
       (case opt363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x356, ls358) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x356, 
            PrettyRep.List (List.map (fn x357 => cvtEXPR x357
                                     ) ls358)]))
       )), ("returnType", cvtTYPE_EXPR x367), ("thisType", 
       (case opt369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x368 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x368))
       )), ("hasRest", PrettyRep.Bool b373)]))
   and cvtBINDING (Binding{ident=x393, ty=x394}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x393), ("ty", cvtTYPE_EXPR x394)]))
   and cvtBINDING_IDENT (TempIdent n402) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n402))
     | cvtBINDING_IDENT (ParamIdent n405) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n405))
     | cvtBINDING_IDENT (PropIdent x408) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x408))
   and cvtINIT_STEP (InitStep(x411, x412)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x411, 
          cvtEXPR x412]))
     | cvtINIT_STEP (AssignStep(x416, x417)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x416, cvtEXPR x417]))
   and cvtTYPE_EXPR (SpecialType x421) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x421))
     | cvtTYPE_EXPR (UnionType ls425) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x424 => 
                                                                                                           cvtTYPE_EXPR x424
                                                                                                    ) ls425)))
     | cvtTYPE_EXPR (ArrayType ls432) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x431 => 
                                                                                                           cvtTYPE_EXPR x431
                                                                                                    ) ls432)))
     | cvtTYPE_EXPR (TypeName x438) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x438))
     | cvtTYPE_EXPR (ElementTypeRef(x441, n442)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x441, PrettyRep.Int n442]))
     | cvtTYPE_EXPR (FieldTypeRef(x446, x447)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x446, cvtIDENT x447]))
     | cvtTYPE_EXPR (FunctionType x451) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x451))
     | cvtTYPE_EXPR (ObjectType ls455) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x454 => 
                                                                                                             cvtFIELD_TYPE x454
                                                                                                      ) ls455)))
     | cvtTYPE_EXPR (LikeType x461) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x461))
     | cvtTYPE_EXPR (WrapType x464) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x464))
     | cvtTYPE_EXPR (AppType{base=x467, args=ls469}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x467), ("args", PrettyRep.List (List.map (fn x468 => 
                                                                                                     cvtTYPE_EXPR x468
                                                                                              ) ls469))]))
     | cvtTYPE_EXPR (LamType{params=ls481, body=x485}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x480 => 
                                                                          cvtIDENT x480
                                                                   ) ls481)), 
          ("body", cvtTYPE_EXPR x485)]))
     | cvtTYPE_EXPR (NullableType{expr=x493, nullable=b494}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x493), ("nullable", PrettyRep.Bool b494)]))
     | cvtTYPE_EXPR (InstanceType x502) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x502))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x506) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x506))
     | cvtSTMT (InitStmt{kind=x509, ns=opt511, prototype=b515, static=b516, 
          temps=x517, inits=ls519}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x509), ("ns", 
       (case opt511 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x510 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x510))
       )), ("prototype", PrettyRep.Bool b515), ("static", PrettyRep.Bool b516), 
          ("temps", cvtBINDINGS x517), ("inits", PrettyRep.List (List.map (fn x518 => 
                                                                                 cvtINIT_STEP x518
                                                                          ) ls519))]))
     | cvtSTMT (ClassBlock x538) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x538))
     | cvtSTMT (ForInStmt x541) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x541))
     | cvtSTMT (ThrowStmt x544) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x544))
     | cvtSTMT (ReturnStmt x547) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x547))
     | cvtSTMT (BreakStmt opt551) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x550 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x550))
       ))
     | cvtSTMT (ContinueStmt opt558) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x557 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x557))
       ))
     | cvtSTMT (BlockStmt x564) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x564))
     | cvtSTMT (LabeledStmt(x567, x568)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x567, 
          cvtSTMT x568]))
     | cvtSTMT (LetStmt x572) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x572))
     | cvtSTMT (WhileStmt x575) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x575))
     | cvtSTMT (DoWhileStmt x578) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x578))
     | cvtSTMT (ForStmt x581) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x581))
     | cvtSTMT (IfStmt{cnd=x584, thn=x585, els=x586}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x584), ("thn", cvtSTMT x585), 
          ("els", cvtSTMT x586)]))
     | cvtSTMT (WithStmt{obj=x596, ty=x597, body=x598}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x596), ("ty", cvtTY x597), ("body", 
          cvtSTMT x598)]))
     | cvtSTMT (TryStmt{block=x608, catches=ls610, finally=opt615}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x608), ("catches", PrettyRep.List (List.map (fn x609 => 
                                                                                                     cvtCATCH_CLAUSE x609
                                                                                              ) ls610)), 
          ("finally", 
       (case opt615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x614 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x614))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x628, labels=ls630, cases=ls635}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x628), ("labels", PrettyRep.List (List.map (fn x629 => 
                                                                                                  cvtIDENT x629
                                                                                           ) ls630)), 
          ("cases", PrettyRep.List (List.map (fn x634 => cvtCASE x634
                                             ) ls635))]))
     | cvtSTMT (SwitchTypeStmt{cond=x648, ty=x649, cases=ls651}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x648), ("ty", cvtTY x649), 
          ("cases", PrettyRep.List (List.map (fn x650 => cvtCATCH_CLAUSE x650
                                             ) ls651))]))
     | cvtSTMT (DXNStmt{expr=x664}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x664)]))
   and cvtEXPR (TernaryExpr(x670, x671, x672)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x670, cvtEXPR x671, cvtEXPR x672]))
     | cvtEXPR (BinaryExpr(x676, x677, x678)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x676, cvtEXPR x677, cvtEXPR x678]))
     | cvtEXPR (BinaryTypeExpr(x682, x683, x684)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x682, cvtEXPR x683, cvtTY x684]))
     | cvtEXPR (UnaryExpr(x688, x689)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x688, 
          cvtEXPR x689]))
     | cvtEXPR (TypeExpr x693) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x693))
     | cvtEXPR (ThisExpr opt697) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt697 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x696 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x696))
       ))
     | cvtEXPR (YieldExpr opt704) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt704 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x703 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x703))
       ))
     | cvtEXPR (SuperExpr opt711) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x710 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x710))
       ))
     | cvtEXPR (LiteralExpr x717) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x717))
     | cvtEXPR (CallExpr{func=x720, actuals=ls722}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x720), ("actuals", PrettyRep.List (List.map (fn x721 => 
                                                                                                   cvtEXPR x721
                                                                                            ) ls722))]))
     | cvtEXPR (ApplyTypeExpr{expr=x733, actuals=ls735}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x733), ("actuals", PrettyRep.List (List.map (fn x734 => 
                                                                                                   cvtTY x734
                                                                                            ) ls735))]))
     | cvtEXPR (LetExpr{defs=x746, body=x747, head=opt749}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x746), ("body", cvtEXPR x747), 
          ("head", 
       (case opt749 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x748 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x748))
       ))]))
     | cvtEXPR (NewExpr{obj=x762, actuals=ls764}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x762), ("actuals", PrettyRep.List (List.map (fn x763 => 
                                                                                                  cvtEXPR x763
                                                                                           ) ls764))]))
     | cvtEXPR (ObjectRef{base=x775, ident=x776, loc=opt778}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x775), ("ident", cvtIDENT_EXPR x776), 
          ("loc", 
       (case opt778 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x777 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x777))
       ))]))
     | cvtEXPR (LexicalRef{ident=x791, loc=opt793}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x791), ("loc", 
       (case opt793 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x792 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x792))
       ))]))
     | cvtEXPR (SetExpr(x804, x805, x806)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x804, 
          cvtEXPR x805, cvtEXPR x806]))
     | cvtEXPR (ListExpr ls811) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x810 => 
                                                                                                    cvtEXPR x810
                                                                                             ) ls811)))
     | cvtEXPR (InitExpr(x817, x818, x819)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x817, 
          cvtHEAD x818, cvtINITS x819]))
     | cvtEXPR (GetTemp n823) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n823))
     | cvtEXPR (GetParam n826) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n826))
     | cvtEXPR (Comprehension(x829, ls831, opt836)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x829, PrettyRep.List (List.map (fn x830 => 
                                                                               cvtFOR_ENUM_HEAD x830
                                                                        ) ls831), 
          
       (case opt836 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x835 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x835))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n848) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n848))
     | cvtFIXTURE_NAME (PropName x851) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x851))
   and cvtIDENT_EXPR (Identifier{ident=x854, openNamespaces=ls860}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x854), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls856 => PrettyRep.List (List.map (fn x855 => 
                                                                                cvtNAMESPACE x855
                                                                         ) ls856)
                                   ) ls860))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x871, expr=x872}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x871), ("expr", cvtEXPR x872)]))
     | cvtIDENT_EXPR (AttributeIdentifier x880) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x880))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x883, openNamespaces=ls889}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x883), ("openNamespaces", PrettyRep.List (List.map (fn ls885 => 
                                                                            PrettyRep.List (List.map (fn x884 => 
                                                                                                            cvtNAMESPACE x884
                                                                                                     ) ls885)
                                                                     ) ls889))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x900, ident=s901}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x900), ("ident", PrettyRep.UniStr s901)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls910, x914)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x909 => cvtIDENT x909
                                                          ) ls910), cvtIDENT_EXPR x914]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r921) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r921))
     | cvtLITERAL (LiteralDecimal d924) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d924))
     | cvtLITERAL (LiteralBoolean b927) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b927))
     | cvtLITERAL (LiteralString s930) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s930))
     | cvtLITERAL (LiteralArray{exprs=x933, ty=opt935}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x933), ("ty", 
       (case opt935 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x934 => PrettyRep.Ctor ("SOME", SOME (cvtTY x934))
       ))]))
     | cvtLITERAL (LiteralXML ls947) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x946 => 
                                                                                                           cvtEXPR x946
                                                                                                    ) ls947)))
     | cvtLITERAL (LiteralNamespace x953) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x953))
     | cvtLITERAL (LiteralObject{expr=ls957, ty=opt962}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x956 => 
                                                                        cvtFIELD x956
                                                                 ) ls957)), 
          ("ty", 
       (case opt962 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x961 => PrettyRep.Ctor ("SOME", SOME (cvtTY x961))
       ))]))
     | cvtLITERAL (LiteralFunction x973) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x973))
     | cvtLITERAL (LiteralRegExp{str=s976}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s976)]))
   and cvtBLOCK (Block x982) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x982))
   and cvtFIXTURE (NamespaceFixture x985) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x985))
     | cvtFIXTURE (ClassFixture x988) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x988))
     | cvtFIXTURE (InterfaceFixture x991) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x991))
     | cvtFIXTURE (TypeVarFixture x994) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x994))
     | cvtFIXTURE (TypeFixture x997) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x997))
     | cvtFIXTURE (MethodFixture{func=x1000, ty=x1001, readOnly=b1002, override=b1003, 
          final=b1004}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1000), ("ty", cvtTY x1001), ("readOnly", PrettyRep.Bool b1002), 
          ("override", PrettyRep.Bool b1003), ("final", PrettyRep.Bool b1004)]))
     | cvtFIXTURE (ValFixture{ty=x1018, readOnly=b1019}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1018), ("readOnly", PrettyRep.Bool b1019)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1027, getter=opt1029, setter=opt1034}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1027), ("getter", 
       (case opt1029 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1028 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1028))
       )), ("setter", 
       (case opt1034 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1033 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1033))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1047, baseTypeArgs=ls1049}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1047), ("baseTypeArgs", PrettyRep.List (List.map (fn x1048 => 
                                                                           cvtTY x1048
                                                                    ) ls1049))]))
   and cvtHEAD (Head(x1060, x1061)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1060, 
          cvtINITS x1061]))
   and cvtBINDINGS (ls1066, ls1071) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1065 => 
                                                                                       cvtBINDING x1065
                                                                                ) ls1066), 
          PrettyRep.List (List.map (fn x1070 => cvtINIT_STEP x1070
                                   ) ls1071)]
   and cvtRIB ls1079 = PrettyRep.List (List.map (fn (x1076, x1077) => PrettyRep.Tuple [cvtFIXTURE_NAME x1076, 
                                                       cvtFIXTURE x1077]
                                                ) ls1079)
   and cvtRIBS ls1090 = PrettyRep.List (List.map (fn ls1086 => PrettyRep.List (List.map (fn (x1083, 
                                                                                               x1084) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1083, 
                                                                                               cvtFIXTURE x1084]
                                                                                        ) ls1086)
                                                 ) ls1090)
   and cvtINITS ls1097 = PrettyRep.List (List.map (fn (x1094, x1095) => PrettyRep.Tuple [cvtFIXTURE_NAME x1094, 
                                                         cvtEXPR x1095]
                                                  ) ls1097)
   and cvtINSTANCE_TYPE {name=x1101, typeParams=ls1103, typeArgs=ls1108, nonnullable=b1112, 
          superTypes=ls1114, ty=x1118, dynamic=b1119} = PrettyRep.Rec [("name", 
          cvtNAME x1101), ("typeParams", PrettyRep.List (List.map (fn x1102 => 
                                                                         cvtIDENT x1102
                                                                  ) ls1103)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1107 => cvtTYPE_EXPR x1107
                                                ) ls1108)), ("nonnullable", 
          PrettyRep.Bool b1112), ("superTypes", PrettyRep.List (List.map (fn x1113 => 
                                                                                cvtTYPE_EXPR x1113
                                                                         ) ls1114)), 
          ("ty", cvtTYPE_EXPR x1118), ("dynamic", PrettyRep.Bool b1119)]
   and cvtFIELD {kind=x1135, name=x1136, init=x1137} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1135), ("name", cvtIDENT_EXPR x1136), ("init", cvtEXPR x1137)]
   and cvtFIELD_TYPE {name=x1145, ty=x1146} = PrettyRep.Rec [("name", cvtIDENT x1145), 
          ("ty", cvtTYPE_EXPR x1146)]
   and cvtFUNC_TYPE {params=ls1153, result=x1157, thisType=opt1159, hasRest=b1163, 
          minArgs=n1164} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1152 => 
                                                                                     cvtTYPE_EXPR x1152
                                                                              ) ls1153)), 
          ("result", cvtTYPE_EXPR x1157), ("thisType", 
       (case opt1159 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1158 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1158))
       )), ("hasRest", PrettyRep.Bool b1163), ("minArgs", PrettyRep.Int n1164)]
   and cvtFUNC_DEFN {kind=x1176, ns=opt1178, final=b1182, override=b1183, prototype=b1184, 
          static=b1185, func=x1186} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1176), 
          ("ns", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1177))
       )), ("final", PrettyRep.Bool b1182), ("override", PrettyRep.Bool b1183), 
          ("prototype", PrettyRep.Bool b1184), ("static", PrettyRep.Bool b1185), 
          ("func", cvtFUNC x1186)]
   and cvtCTOR_DEFN x1202 = cvtCTOR x1202
   and cvtVAR_DEFN {kind=x1203, ns=opt1205, static=b1209, prototype=b1210, 
          bindings=(ls1212, ls1217)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1203), 
          ("ns", 
       (case opt1205 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1204 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1204))
       )), ("static", PrettyRep.Bool b1209), ("prototype", PrettyRep.Bool b1210), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1211 => 
                                                                        cvtBINDING x1211
                                                                 ) ls1212), 
          PrettyRep.List (List.map (fn x1216 => cvtINIT_STEP x1216
                                   ) ls1217)])]
   and cvtNAMESPACE_DEFN {ident=x1233, ns=opt1235, init=opt1240} = PrettyRep.Rec [("ident", 
          cvtIDENT x1233), ("ns", 
       (case opt1235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1234 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1234))
       )), ("init", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1239))
       ))]
   and cvtCLASS_DEFN {ns=opt1252, ident=x1256, nonnullable=b1257, dynamic=b1258, 
          final=b1259, params=ls1261, extends=opt1266, implements=ls1271, classDefns=ls1276, 
          instanceDefns=ls1281, instanceStmts=ls1286, ctorDefn=opt1291} = PrettyRep.Rec [("ns", 
          
       (case opt1252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1251 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1251))
       )), ("ident", cvtIDENT x1256), ("nonnullable", PrettyRep.Bool b1257), 
          ("dynamic", PrettyRep.Bool b1258), ("final", PrettyRep.Bool b1259), 
          ("params", PrettyRep.List (List.map (fn x1260 => cvtIDENT x1260
                                              ) ls1261)), ("extends", 
       (case opt1266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1265 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1265))
       )), ("implements", PrettyRep.List (List.map (fn x1270 => cvtTYPE_EXPR x1270
                                                   ) ls1271)), ("classDefns", 
          PrettyRep.List (List.map (fn x1275 => cvtDEFN x1275
                                   ) ls1276)), ("instanceDefns", PrettyRep.List (List.map (fn x1280 => 
                                                                                                 cvtDEFN x1280
                                                                                          ) ls1281)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1285 => cvtSTMT x1285
                                                     ) ls1286)), ("ctorDefn", 
          
       (case opt1291 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1290 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1290))
       ))]
   and cvtINTERFACE_DEFN {ident=x1320, ns=opt1322, nonnullable=b1326, params=ls1328, 
          extends=ls1333, instanceDefns=ls1338} = PrettyRep.Rec [("ident", 
          cvtIDENT x1320), ("ns", 
       (case opt1322 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1321 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1321))
       )), ("nonnullable", PrettyRep.Bool b1326), ("params", PrettyRep.List (List.map (fn x1327 => 
                                                                                             cvtIDENT x1327
                                                                                      ) ls1328)), 
          ("extends", PrettyRep.List (List.map (fn x1332 => cvtTYPE_EXPR x1332
                                               ) ls1333)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1337 => cvtDEFN x1337
                                   ) ls1338))]
   and cvtTYPE_DEFN {ident=x1355, ns=opt1357, init=x1361} = PrettyRep.Rec [("ident", 
          cvtIDENT x1355), ("ns", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1356))
       )), ("init", cvtTYPE_EXPR x1361)]
   and cvtCLASS_BLOCK {ns=opt1370, ident=x1374, name=opt1376, block=x1380} = 
          PrettyRep.Rec [("ns", 
       (case opt1370 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1369 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1369))
       )), ("ident", cvtIDENT x1374), ("name", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1375))
       )), ("block", cvtBLOCK x1380)]
   and cvtFOR_ENUM_HEAD {isEach=b1390, bindings=(ls1392, ls1397), expr=x1402} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1390), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1391 => 
                                                                                                                         cvtBINDING x1391
                                                                                                                  ) ls1392), 
          PrettyRep.List (List.map (fn x1396 => cvtINIT_STEP x1396
                                   ) ls1397)]), ("expr", cvtEXPR x1402)]
   and cvtFOR_ENUM_STMT {isEach=b1410, defn=opt1441, obj=x1445, rib=opt1453, 
          next=x1457, labels=ls1459, body=x1463} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1410), ("defn", 
       (case opt1441 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1411, ns=opt1413, static=b1417, prototype=b1418, bindings=(ls1420, 
            ls1425)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1411), ("ns", 
         (case opt1413 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1412 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1412))
         )), ("static", PrettyRep.Bool b1417), ("prototype", PrettyRep.Bool b1418), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1419 => 
                                                                          cvtBINDING x1419
                                                                   ) ls1420), 
            PrettyRep.List (List.map (fn x1424 => cvtINIT_STEP x1424
                                     ) ls1425)])]))
       )), ("obj", cvtEXPR x1445), ("rib", 
       (case opt1453 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1449 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1446, 
                                                                                      x1447) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1446, 
                                                                                      cvtFIXTURE x1447]
                                                                               ) ls1449)))
       )), ("next", cvtSTMT x1457), ("labels", PrettyRep.List (List.map (fn x1458 => 
                                                                               cvtIDENT x1458
                                                                        ) ls1459)), 
          ("body", cvtSTMT x1463)]
   and cvtFOR_STMT {rib=opt1486, defn=opt1520, init=ls1525, cond=x1529, update=x1530, 
          labels=ls1532, body=x1536} = PrettyRep.Rec [("rib", 
       (case opt1486 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1482 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1479, 
                                                                                      x1480) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1479, 
                                                                                      cvtFIXTURE x1480]
                                                                               ) ls1482)))
       )), ("defn", 
       (case opt1520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1490, ns=opt1492, static=b1496, prototype=b1497, bindings=(ls1499, 
            ls1504)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1490), ("ns", 
         (case opt1492 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1491 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1491))
         )), ("static", PrettyRep.Bool b1496), ("prototype", PrettyRep.Bool b1497), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1498 => 
                                                                          cvtBINDING x1498
                                                                   ) ls1499), 
            PrettyRep.List (List.map (fn x1503 => cvtINIT_STEP x1503
                                     ) ls1504)])]))
       )), ("init", PrettyRep.List (List.map (fn x1524 => cvtSTMT x1524
                                             ) ls1525)), ("cond", cvtEXPR x1529), 
          ("update", cvtEXPR x1530), ("labels", PrettyRep.List (List.map (fn x1531 => 
                                                                                cvtIDENT x1531
                                                                         ) ls1532)), 
          ("body", cvtSTMT x1536)]
   and cvtWHILE_STMT {cond=x1552, rib=opt1560, body=x1564, labels=ls1566} = 
          PrettyRep.Rec [("cond", cvtEXPR x1552), ("rib", 
       (case opt1560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1556 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1553, 
                                                                                      x1554) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1553, 
                                                                                      cvtFIXTURE x1554]
                                                                               ) ls1556)))
       )), ("body", cvtSTMT x1564), ("labels", PrettyRep.List (List.map (fn x1565 => 
                                                                               cvtIDENT x1565
                                                                        ) ls1566))]
   and cvtDIRECTIVES {pragmas=ls1580, defns=ls1585, head=opt1590, body=ls1595, 
          loc=opt1600} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1579 => 
                                                                                    cvtPRAGMA x1579
                                                                             ) ls1580)), 
          ("defns", PrettyRep.List (List.map (fn x1584 => cvtDEFN x1584
                                             ) ls1585)), ("head", 
       (case opt1590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1589 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1589))
       )), ("body", PrettyRep.List (List.map (fn x1594 => cvtSTMT x1594
                                             ) ls1595)), ("loc", 
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1599 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1599))
       ))]
   and cvtCASE {label=opt1616, inits=opt1627, body=x1631} = PrettyRep.Rec [("label", 
          
       (case opt1616 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1615 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1615))
       )), ("inits", 
       (case opt1627 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1623 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1620, 
                                                                                      x1621) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1620, 
                                                                                      cvtEXPR x1621]
                                                                               ) ls1623)))
       )), ("body", cvtBLOCK x1631)]
   and cvtCATCH_CLAUSE {bindings=(ls1640, ls1645), ty=x1650, rib=opt1658, inits=opt1669, 
          block=x1673} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1639 => 
                                                                                                      cvtBINDING x1639
                                                                                               ) ls1640), 
          PrettyRep.List (List.map (fn x1644 => cvtINIT_STEP x1644
                                   ) ls1645)]), ("ty", cvtTY x1650), ("rib", 
          
       (case opt1658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1654 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1651, 
                                                                                      x1652) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1651, 
                                                                                      cvtFIXTURE x1652]
                                                                               ) ls1654)))
       )), ("inits", 
       (case opt1669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1665 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1662, 
                                                                                      x1663) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1662, 
                                                                                      cvtEXPR x1663]
                                                                               ) ls1665)))
       )), ("block", cvtBLOCK x1673)]
   and cvtFUNC_NAME {kind=x1685, ident=x1686} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1685), 
          ("ident", cvtIDENT x1686)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1692, getter=opt1694, setter=opt1699} = 
          PrettyRep.Rec [("ty", cvtTY x1692), ("getter", 
       (case opt1694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1693 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1693))
       )), ("setter", 
       (case opt1699 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1698 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1698))
       ))]
   and cvtFRAGMENT (Unit{name=opt1711, fragments=ls1716}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1711 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1710 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1710))
       )), ("fragments", PrettyRep.List (List.map (fn x1715 => cvtFRAGMENT x1715
                                                  ) ls1716))]))
     | cvtFRAGMENT (Package{name=ls1728, fragments=ls1733}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1727 => 
                                                                        cvtIDENT x1727
                                                                 ) ls1728)), 
          ("fragments", PrettyRep.List (List.map (fn x1732 => cvtFRAGMENT x1732
                                                 ) ls1733))]))
     | cvtFRAGMENT (Anon x1744) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1744))
end

