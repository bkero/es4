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
     | cvtLITERAL (LiteralInt i927) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i927))
     | cvtLITERAL (LiteralUInt u930) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u930))
     | cvtLITERAL (LiteralBoolean b933) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b933))
     | cvtLITERAL (LiteralString s936) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s936))
     | cvtLITERAL (LiteralArray{exprs=x939, ty=opt941}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x939), ("ty", 
       (case opt941 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x940 => PrettyRep.Ctor ("SOME", SOME (cvtTY x940))
       ))]))
     | cvtLITERAL (LiteralXML ls953) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x952 => 
                                                                                                           cvtEXPR x952
                                                                                                    ) ls953)))
     | cvtLITERAL (LiteralNamespace x959) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x959))
     | cvtLITERAL (LiteralObject{expr=ls963, ty=opt968}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x962 => 
                                                                        cvtFIELD x962
                                                                 ) ls963)), 
          ("ty", 
       (case opt968 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x967 => PrettyRep.Ctor ("SOME", SOME (cvtTY x967))
       ))]))
     | cvtLITERAL (LiteralFunction x979) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x979))
     | cvtLITERAL (LiteralRegExp{str=s982}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s982)]))
   and cvtBLOCK (Block x988) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x988))
   and cvtFIXTURE (NamespaceFixture x991) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x991))
     | cvtFIXTURE (ClassFixture x994) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x994))
     | cvtFIXTURE (InterfaceFixture x997) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x997))
     | cvtFIXTURE (TypeVarFixture x1000) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x1000))
     | cvtFIXTURE (TypeFixture x1003) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1003))
     | cvtFIXTURE (MethodFixture{func=x1006, ty=x1007, readOnly=b1008, override=b1009, 
          final=b1010}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1006), ("ty", cvtTY x1007), ("readOnly", PrettyRep.Bool b1008), 
          ("override", PrettyRep.Bool b1009), ("final", PrettyRep.Bool b1010)]))
     | cvtFIXTURE (ValFixture{ty=x1024, readOnly=b1025}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1024), ("readOnly", PrettyRep.Bool b1025)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1033, getter=opt1035, setter=opt1040}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1033), ("getter", 
       (case opt1035 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1034 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1034))
       )), ("setter", 
       (case opt1040 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1039 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1039))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1053, baseTypeArgs=ls1055}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1053), ("baseTypeArgs", PrettyRep.List (List.map (fn x1054 => 
                                                                           cvtTY x1054
                                                                    ) ls1055))]))
   and cvtHEAD (Head(x1066, x1067)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1066, 
          cvtINITS x1067]))
   and cvtBINDINGS (ls1072, ls1077) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1071 => 
                                                                                       cvtBINDING x1071
                                                                                ) ls1072), 
          PrettyRep.List (List.map (fn x1076 => cvtINIT_STEP x1076
                                   ) ls1077)]
   and cvtRIB ls1085 = PrettyRep.List (List.map (fn (x1082, x1083) => PrettyRep.Tuple [cvtFIXTURE_NAME x1082, 
                                                       cvtFIXTURE x1083]
                                                ) ls1085)
   and cvtRIBS ls1096 = PrettyRep.List (List.map (fn ls1092 => PrettyRep.List (List.map (fn (x1089, 
                                                                                               x1090) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1089, 
                                                                                               cvtFIXTURE x1090]
                                                                                        ) ls1092)
                                                 ) ls1096)
   and cvtINITS ls1103 = PrettyRep.List (List.map (fn (x1100, x1101) => PrettyRep.Tuple [cvtFIXTURE_NAME x1100, 
                                                         cvtEXPR x1101]
                                                  ) ls1103)
   and cvtINSTANCE_TYPE {name=x1107, typeParams=ls1109, typeArgs=ls1114, nonnullable=b1118, 
          superTypes=ls1120, ty=x1124, dynamic=b1125} = PrettyRep.Rec [("name", 
          cvtNAME x1107), ("typeParams", PrettyRep.List (List.map (fn x1108 => 
                                                                         cvtIDENT x1108
                                                                  ) ls1109)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1113 => cvtTYPE_EXPR x1113
                                                ) ls1114)), ("nonnullable", 
          PrettyRep.Bool b1118), ("superTypes", PrettyRep.List (List.map (fn x1119 => 
                                                                                cvtTYPE_EXPR x1119
                                                                         ) ls1120)), 
          ("ty", cvtTYPE_EXPR x1124), ("dynamic", PrettyRep.Bool b1125)]
   and cvtFIELD {kind=x1141, name=x1142, init=x1143} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1141), ("name", cvtIDENT_EXPR x1142), ("init", cvtEXPR x1143)]
   and cvtFIELD_TYPE {name=x1151, ty=x1152} = PrettyRep.Rec [("name", cvtIDENT x1151), 
          ("ty", cvtTYPE_EXPR x1152)]
   and cvtFUNC_TYPE {params=ls1159, result=x1163, thisType=opt1165, hasRest=b1169, 
          minArgs=n1170} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1158 => 
                                                                                     cvtTYPE_EXPR x1158
                                                                              ) ls1159)), 
          ("result", cvtTYPE_EXPR x1163), ("thisType", 
       (case opt1165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1164 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1164))
       )), ("hasRest", PrettyRep.Bool b1169), ("minArgs", PrettyRep.Int n1170)]
   and cvtFUNC_DEFN {kind=x1182, ns=opt1184, final=b1188, override=b1189, prototype=b1190, 
          static=b1191, func=x1192} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1182), 
          ("ns", 
       (case opt1184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1183 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1183))
       )), ("final", PrettyRep.Bool b1188), ("override", PrettyRep.Bool b1189), 
          ("prototype", PrettyRep.Bool b1190), ("static", PrettyRep.Bool b1191), 
          ("func", cvtFUNC x1192)]
   and cvtCTOR_DEFN x1208 = cvtCTOR x1208
   and cvtVAR_DEFN {kind=x1209, ns=opt1211, static=b1215, prototype=b1216, 
          bindings=(ls1218, ls1223)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1209), 
          ("ns", 
       (case opt1211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1210 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1210))
       )), ("static", PrettyRep.Bool b1215), ("prototype", PrettyRep.Bool b1216), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1217 => 
                                                                        cvtBINDING x1217
                                                                 ) ls1218), 
          PrettyRep.List (List.map (fn x1222 => cvtINIT_STEP x1222
                                   ) ls1223)])]
   and cvtNAMESPACE_DEFN {ident=x1239, ns=opt1241, init=opt1246} = PrettyRep.Rec [("ident", 
          cvtIDENT x1239), ("ns", 
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1240))
       )), ("init", 
       (case opt1246 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1245 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1245))
       ))]
   and cvtCLASS_DEFN {ns=opt1258, ident=x1262, nonnullable=b1263, dynamic=b1264, 
          final=b1265, params=ls1267, extends=opt1272, implements=ls1277, classDefns=ls1282, 
          instanceDefns=ls1287, instanceStmts=ls1292, ctorDefn=opt1297} = PrettyRep.Rec [("ns", 
          
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1257))
       )), ("ident", cvtIDENT x1262), ("nonnullable", PrettyRep.Bool b1263), 
          ("dynamic", PrettyRep.Bool b1264), ("final", PrettyRep.Bool b1265), 
          ("params", PrettyRep.List (List.map (fn x1266 => cvtIDENT x1266
                                              ) ls1267)), ("extends", 
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1271))
       )), ("implements", PrettyRep.List (List.map (fn x1276 => cvtTYPE_EXPR x1276
                                                   ) ls1277)), ("classDefns", 
          PrettyRep.List (List.map (fn x1281 => cvtDEFN x1281
                                   ) ls1282)), ("instanceDefns", PrettyRep.List (List.map (fn x1286 => 
                                                                                                 cvtDEFN x1286
                                                                                          ) ls1287)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1291 => cvtSTMT x1291
                                                     ) ls1292)), ("ctorDefn", 
          
       (case opt1297 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1296 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1296))
       ))]
   and cvtINTERFACE_DEFN {ident=x1326, ns=opt1328, nonnullable=b1332, params=ls1334, 
          extends=ls1339, instanceDefns=ls1344} = PrettyRep.Rec [("ident", 
          cvtIDENT x1326), ("ns", 
       (case opt1328 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1327 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1327))
       )), ("nonnullable", PrettyRep.Bool b1332), ("params", PrettyRep.List (List.map (fn x1333 => 
                                                                                             cvtIDENT x1333
                                                                                      ) ls1334)), 
          ("extends", PrettyRep.List (List.map (fn x1338 => cvtTYPE_EXPR x1338
                                               ) ls1339)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1343 => cvtDEFN x1343
                                   ) ls1344))]
   and cvtTYPE_DEFN {ident=x1361, ns=opt1363, init=x1367} = PrettyRep.Rec [("ident", 
          cvtIDENT x1361), ("ns", 
       (case opt1363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1362))
       )), ("init", cvtTYPE_EXPR x1367)]
   and cvtCLASS_BLOCK {ns=opt1376, ident=x1380, name=opt1382, block=x1386} = 
          PrettyRep.Rec [("ns", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1375))
       )), ("ident", cvtIDENT x1380), ("name", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1381 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1381))
       )), ("block", cvtBLOCK x1386)]
   and cvtFOR_ENUM_HEAD {isEach=b1396, bindings=(ls1398, ls1403), expr=x1408} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1396), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1397 => 
                                                                                                                         cvtBINDING x1397
                                                                                                                  ) ls1398), 
          PrettyRep.List (List.map (fn x1402 => cvtINIT_STEP x1402
                                   ) ls1403)]), ("expr", cvtEXPR x1408)]
   and cvtFOR_ENUM_STMT {isEach=b1416, defn=opt1447, obj=x1451, rib=opt1459, 
          next=x1463, labels=ls1465, body=x1469} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1416), ("defn", 
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1417, ns=opt1419, static=b1423, prototype=b1424, bindings=(ls1426, 
            ls1431)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1417), ("ns", 
         (case opt1419 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1418 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1418))
         )), ("static", PrettyRep.Bool b1423), ("prototype", PrettyRep.Bool b1424), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1425 => 
                                                                          cvtBINDING x1425
                                                                   ) ls1426), 
            PrettyRep.List (List.map (fn x1430 => cvtINIT_STEP x1430
                                     ) ls1431)])]))
       )), ("obj", cvtEXPR x1451), ("rib", 
       (case opt1459 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1455 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1452, 
                                                                                      x1453) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1452, 
                                                                                      cvtFIXTURE x1453]
                                                                               ) ls1455)))
       )), ("next", cvtSTMT x1463), ("labels", PrettyRep.List (List.map (fn x1464 => 
                                                                               cvtIDENT x1464
                                                                        ) ls1465)), 
          ("body", cvtSTMT x1469)]
   and cvtFOR_STMT {rib=opt1492, defn=opt1526, init=ls1531, cond=x1535, update=x1536, 
          labels=ls1538, body=x1542} = PrettyRep.Rec [("rib", 
       (case opt1492 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1488 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1485, 
                                                                                      x1486) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1485, 
                                                                                      cvtFIXTURE x1486]
                                                                               ) ls1488)))
       )), ("defn", 
       (case opt1526 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1496, ns=opt1498, static=b1502, prototype=b1503, bindings=(ls1505, 
            ls1510)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1496), ("ns", 
         (case opt1498 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1497 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1497))
         )), ("static", PrettyRep.Bool b1502), ("prototype", PrettyRep.Bool b1503), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1504 => 
                                                                          cvtBINDING x1504
                                                                   ) ls1505), 
            PrettyRep.List (List.map (fn x1509 => cvtINIT_STEP x1509
                                     ) ls1510)])]))
       )), ("init", PrettyRep.List (List.map (fn x1530 => cvtSTMT x1530
                                             ) ls1531)), ("cond", cvtEXPR x1535), 
          ("update", cvtEXPR x1536), ("labels", PrettyRep.List (List.map (fn x1537 => 
                                                                                cvtIDENT x1537
                                                                         ) ls1538)), 
          ("body", cvtSTMT x1542)]
   and cvtWHILE_STMT {cond=x1558, rib=opt1566, body=x1570, labels=ls1572} = 
          PrettyRep.Rec [("cond", cvtEXPR x1558), ("rib", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1562 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1559, 
                                                                                      x1560) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1559, 
                                                                                      cvtFIXTURE x1560]
                                                                               ) ls1562)))
       )), ("body", cvtSTMT x1570), ("labels", PrettyRep.List (List.map (fn x1571 => 
                                                                               cvtIDENT x1571
                                                                        ) ls1572))]
   and cvtDIRECTIVES {pragmas=ls1586, defns=ls1591, head=opt1596, body=ls1601, 
          loc=opt1606} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1585 => 
                                                                                    cvtPRAGMA x1585
                                                                             ) ls1586)), 
          ("defns", PrettyRep.List (List.map (fn x1590 => cvtDEFN x1590
                                             ) ls1591)), ("head", 
       (case opt1596 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1595 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1595))
       )), ("body", PrettyRep.List (List.map (fn x1600 => cvtSTMT x1600
                                             ) ls1601)), ("loc", 
       (case opt1606 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1605 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1605))
       ))]
   and cvtCASE {label=opt1622, inits=opt1633, body=x1637} = PrettyRep.Rec [("label", 
          
       (case opt1622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1621 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1621))
       )), ("inits", 
       (case opt1633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1629 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1626, 
                                                                                      x1627) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1626, 
                                                                                      cvtEXPR x1627]
                                                                               ) ls1629)))
       )), ("body", cvtBLOCK x1637)]
   and cvtCATCH_CLAUSE {bindings=(ls1646, ls1651), ty=x1656, rib=opt1664, inits=opt1675, 
          block=x1679} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1645 => 
                                                                                                      cvtBINDING x1645
                                                                                               ) ls1646), 
          PrettyRep.List (List.map (fn x1650 => cvtINIT_STEP x1650
                                   ) ls1651)]), ("ty", cvtTY x1656), ("rib", 
          
       (case opt1664 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1660 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1657, 
                                                                                      x1658) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1657, 
                                                                                      cvtFIXTURE x1658]
                                                                               ) ls1660)))
       )), ("inits", 
       (case opt1675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1671 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1668, 
                                                                                      x1669) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1668, 
                                                                                      cvtEXPR x1669]
                                                                               ) ls1671)))
       )), ("block", cvtBLOCK x1679)]
   and cvtFUNC_NAME {kind=x1691, ident=x1692} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1691), 
          ("ident", cvtIDENT x1692)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1698, getter=opt1700, setter=opt1705} = 
          PrettyRep.Rec [("ty", cvtTY x1698), ("getter", 
       (case opt1700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1699 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1699))
       )), ("setter", 
       (case opt1705 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1704 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1704))
       ))]
   and cvtFRAGMENT (Unit{name=opt1717, fragments=ls1722}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1717 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1716 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1716))
       )), ("fragments", PrettyRep.List (List.map (fn x1721 => cvtFRAGMENT x1721
                                                  ) ls1722))]))
     | cvtFRAGMENT (Package{name=ls1734, fragments=ls1739}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1733 => 
                                                                        cvtIDENT x1733
                                                                 ) ls1734)), 
          ("fragments", PrettyRep.List (List.map (fn x1738 => cvtFRAGMENT x1738
                                                 ) ls1739))]))
     | cvtFRAGMENT (Anon x1750) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1750))
end

