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
     | cvtPRAGMA (UseDecimalContext x142) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x142))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls148, name=x152}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x147 => 
                                                                           cvtIDENT x147
                                                                    ) ls148)), 
          ("name", cvtIDENT x152)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x166, ribId=opt168}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x166), ("ribId", 
       (case opt168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x167 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x167))
       ))]))
   and cvtCLS (Cls{name=x179, typeParams=ls181, nonnullable=b185, dynamic=b186, 
          extends=opt188, implements=ls193, classRib=x197, instanceRib=x198, 
          instanceInits=x199, constructor=opt201, classType=x205, instanceType=x206}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x179), 
          ("typeParams", PrettyRep.List (List.map (fn x180 => cvtIDENT x180
                                                  ) ls181)), ("nonnullable", 
          PrettyRep.Bool b185), ("dynamic", PrettyRep.Bool b186), ("extends", 
          
       (case opt188 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x187 => PrettyRep.Ctor ("SOME", SOME (cvtTY x187))
       )), ("implements", PrettyRep.List (List.map (fn x192 => cvtTY x192
                                                   ) ls193)), ("classRib", 
          cvtRIB x197), ("instanceRib", cvtRIB x198), ("instanceInits", cvtHEAD x199), 
          ("constructor", 
       (case opt201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x200 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x200))
       )), ("classType", cvtTY x205), ("instanceType", cvtTY x206)]))
   and cvtIFACE (Iface{name=x234, typeParams=ls236, nonnullable=b240, extends=ls242, 
          instanceRib=x246, instanceType=x247}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x234), ("typeParams", PrettyRep.List (List.map (fn x235 => 
                                                                                                      cvtIDENT x235
                                                                                               ) ls236)), 
          ("nonnullable", PrettyRep.Bool b240), ("extends", PrettyRep.List (List.map (fn x241 => 
                                                                                            cvtTY x241
                                                                                     ) ls242)), 
          ("instanceRib", cvtRIB x246), ("instanceType", cvtTY x247)]))
   and cvtCTOR (Ctor{settings=x263, superArgs=ls265, func=x269}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x263), ("superArgs", PrettyRep.List (List.map (fn x264 => 
                                                                                                         cvtEXPR x264
                                                                                                  ) ls265)), 
          ("func", cvtFUNC x269)]))
   and cvtFUNC (Func{name=x279, fsig=x280, native=b281, block=opt283, param=x287, 
          defaults=ls289, ty=x293, loc=opt295}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x279), ("fsig", cvtFUNC_SIG x280), ("native", PrettyRep.Bool b281), 
          ("block", 
       (case opt283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x282 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x282))
       )), ("param", cvtHEAD x287), ("defaults", PrettyRep.List (List.map (fn x288 => 
                                                                                 cvtEXPR x288
                                                                          ) ls289)), 
          ("ty", cvtTY x293), ("loc", 
       (case opt295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x294 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x294))
       ))]))
   and cvtDEFN (ClassDefn x318) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x318))
     | cvtDEFN (VariableDefn x321) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x321))
     | cvtDEFN (FunctionDefn x324) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x324))
     | cvtDEFN (ConstructorDefn x327) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x327))
     | cvtDEFN (InterfaceDefn x330) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x330))
     | cvtDEFN (NamespaceDefn x333) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x333))
     | cvtDEFN (TypeDefn x336) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x336))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls340, params=x344, paramTypes=ls346, 
          defaults=ls351, ctorInits=opt362, returnType=x366, thisType=opt368, 
          hasRest=b372}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x339 => cvtIDENT x339
                                   ) ls340)), ("params", cvtBINDINGS x344), 
          ("paramTypes", PrettyRep.List (List.map (fn x345 => cvtTYPE_EXPR x345
                                                  ) ls346)), ("defaults", PrettyRep.List (List.map (fn x350 => 
                                                                                                          cvtEXPR x350
                                                                                                   ) ls351)), 
          ("ctorInits", 
       (case opt362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x355, ls357) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x355, 
            PrettyRep.List (List.map (fn x356 => cvtEXPR x356
                                     ) ls357)]))
       )), ("returnType", cvtTYPE_EXPR x366), ("thisType", 
       (case opt368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x367 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x367))
       )), ("hasRest", PrettyRep.Bool b372)]))
   and cvtBINDING (Binding{ident=x392, ty=x393}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x392), ("ty", cvtTYPE_EXPR x393)]))
   and cvtBINDING_IDENT (TempIdent n401) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n401))
     | cvtBINDING_IDENT (ParamIdent n404) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n404))
     | cvtBINDING_IDENT (PropIdent x407) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x407))
   and cvtINIT_STEP (InitStep(x410, x411)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x410, 
          cvtEXPR x411]))
     | cvtINIT_STEP (AssignStep(x415, x416)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x415, cvtEXPR x416]))
   and cvtTYPE_EXPR (SpecialType x420) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x420))
     | cvtTYPE_EXPR (UnionType ls424) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                           cvtTYPE_EXPR x423
                                                                                                    ) ls424)))
     | cvtTYPE_EXPR (ArrayType ls431) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x430 => 
                                                                                                           cvtTYPE_EXPR x430
                                                                                                    ) ls431)))
     | cvtTYPE_EXPR (TypeName x437) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x437))
     | cvtTYPE_EXPR (ElementTypeRef(x440, n441)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x440, PrettyRep.Int n441]))
     | cvtTYPE_EXPR (FieldTypeRef(x445, x446)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x445, cvtIDENT x446]))
     | cvtTYPE_EXPR (FunctionType x450) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x450))
     | cvtTYPE_EXPR (ObjectType ls454) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x453 => 
                                                                                                             cvtFIELD_TYPE x453
                                                                                                      ) ls454)))
     | cvtTYPE_EXPR (LikeType x460) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x460))
     | cvtTYPE_EXPR (WrapType x463) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x463))
     | cvtTYPE_EXPR (AppType{base=x466, args=ls468}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x466), ("args", PrettyRep.List (List.map (fn x467 => 
                                                                                                     cvtTYPE_EXPR x467
                                                                                              ) ls468))]))
     | cvtTYPE_EXPR (LamType{params=ls480, body=x484}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x479 => 
                                                                          cvtIDENT x479
                                                                   ) ls480)), 
          ("body", cvtTYPE_EXPR x484)]))
     | cvtTYPE_EXPR (NullableType{expr=x492, nullable=b493}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x492), ("nullable", PrettyRep.Bool b493)]))
     | cvtTYPE_EXPR (InstanceType x501) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x501))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x505) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x505))
     | cvtSTMT (InitStmt{kind=x508, ns=opt510, prototype=b514, static=b515, 
          temps=x516, inits=ls518}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x508), ("ns", 
       (case opt510 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x509 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x509))
       )), ("prototype", PrettyRep.Bool b514), ("static", PrettyRep.Bool b515), 
          ("temps", cvtBINDINGS x516), ("inits", PrettyRep.List (List.map (fn x517 => 
                                                                                 cvtINIT_STEP x517
                                                                          ) ls518))]))
     | cvtSTMT (ClassBlock x537) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x537))
     | cvtSTMT (ForInStmt x540) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x540))
     | cvtSTMT (ThrowStmt x543) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x543))
     | cvtSTMT (ReturnStmt x546) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x546))
     | cvtSTMT (BreakStmt opt550) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x549 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x549))
       ))
     | cvtSTMT (ContinueStmt opt557) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x556 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x556))
       ))
     | cvtSTMT (BlockStmt x563) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x563))
     | cvtSTMT (LabeledStmt(x566, x567)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x566, 
          cvtSTMT x567]))
     | cvtSTMT (LetStmt x571) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x571))
     | cvtSTMT (WhileStmt x574) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x574))
     | cvtSTMT (DoWhileStmt x577) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x577))
     | cvtSTMT (ForStmt x580) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x580))
     | cvtSTMT (IfStmt{cnd=x583, thn=x584, els=x585}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x583), ("thn", cvtSTMT x584), 
          ("els", cvtSTMT x585)]))
     | cvtSTMT (WithStmt{obj=x595, ty=x596, body=x597}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x595), ("ty", cvtTY x596), ("body", 
          cvtSTMT x597)]))
     | cvtSTMT (TryStmt{block=x607, catches=ls609, finally=opt614}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x607), ("catches", PrettyRep.List (List.map (fn x608 => 
                                                                                                     cvtCATCH_CLAUSE x608
                                                                                              ) ls609)), 
          ("finally", 
       (case opt614 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x613 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x613))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x627, labels=ls629, cases=ls634}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x627), ("labels", PrettyRep.List (List.map (fn x628 => 
                                                                                                  cvtIDENT x628
                                                                                           ) ls629)), 
          ("cases", PrettyRep.List (List.map (fn x633 => cvtCASE x633
                                             ) ls634))]))
     | cvtSTMT (SwitchTypeStmt{cond=x647, ty=x648, cases=ls650}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x647), ("ty", cvtTY x648), 
          ("cases", PrettyRep.List (List.map (fn x649 => cvtCATCH_CLAUSE x649
                                             ) ls650))]))
     | cvtSTMT (DXNStmt{expr=x663}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x663)]))
   and cvtEXPR (TernaryExpr(x669, x670, x671)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x669, cvtEXPR x670, cvtEXPR x671]))
     | cvtEXPR (BinaryExpr(x675, x676, x677)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x675, cvtEXPR x676, cvtEXPR x677]))
     | cvtEXPR (BinaryTypeExpr(x681, x682, x683)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x681, cvtEXPR x682, cvtTY x683]))
     | cvtEXPR (UnaryExpr(x687, x688)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x687, 
          cvtEXPR x688]))
     | cvtEXPR (TypeExpr x692) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x692))
     | cvtEXPR (ThisExpr opt696) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt696 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x695 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x695))
       ))
     | cvtEXPR (YieldExpr opt703) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt703 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x702 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x702))
       ))
     | cvtEXPR (SuperExpr opt710) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x709 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x709))
       ))
     | cvtEXPR (LiteralExpr x716) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x716))
     | cvtEXPR (CallExpr{func=x719, actuals=ls721}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x719), ("actuals", PrettyRep.List (List.map (fn x720 => 
                                                                                                   cvtEXPR x720
                                                                                            ) ls721))]))
     | cvtEXPR (ApplyTypeExpr{expr=x732, actuals=ls734}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x732), ("actuals", PrettyRep.List (List.map (fn x733 => 
                                                                                                   cvtTY x733
                                                                                            ) ls734))]))
     | cvtEXPR (LetExpr{defs=x745, body=x746, head=opt748}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x745), ("body", cvtEXPR x746), 
          ("head", 
       (case opt748 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x747 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x747))
       ))]))
     | cvtEXPR (NewExpr{obj=x761, actuals=ls763}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x761), ("actuals", PrettyRep.List (List.map (fn x762 => 
                                                                                                  cvtEXPR x762
                                                                                           ) ls763))]))
     | cvtEXPR (ObjectRef{base=x774, ident=x775, loc=opt777}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x774), ("ident", cvtIDENT_EXPR x775), 
          ("loc", 
       (case opt777 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x776 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x776))
       ))]))
     | cvtEXPR (LexicalRef{ident=x790, loc=opt792}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x790), ("loc", 
       (case opt792 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x791 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x791))
       ))]))
     | cvtEXPR (SetExpr(x803, x804, x805)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x803, 
          cvtEXPR x804, cvtEXPR x805]))
     | cvtEXPR (ListExpr ls810) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x809 => 
                                                                                                    cvtEXPR x809
                                                                                             ) ls810)))
     | cvtEXPR (InitExpr(x816, x817, x818)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x816, 
          cvtHEAD x817, cvtINITS x818]))
     | cvtEXPR (GetTemp n822) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n822))
     | cvtEXPR (GetParam n825) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n825))
     | cvtEXPR (Comprehension(x828, ls830, opt835)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x828, PrettyRep.List (List.map (fn x829 => 
                                                                               cvtFOR_ENUM_HEAD x829
                                                                        ) ls830), 
          
       (case opt835 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x834 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x834))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n847) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n847))
     | cvtFIXTURE_NAME (PropName x850) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x850))
   and cvtIDENT_EXPR (Identifier{ident=x853, openNamespaces=ls859}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x853), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls855 => PrettyRep.List (List.map (fn x854 => 
                                                                                cvtNAMESPACE x854
                                                                         ) ls855)
                                   ) ls859))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x870, expr=x871}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x870), ("expr", cvtEXPR x871)]))
     | cvtIDENT_EXPR (AttributeIdentifier x879) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x879))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x882, openNamespaces=ls888}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x882), ("openNamespaces", PrettyRep.List (List.map (fn ls884 => 
                                                                            PrettyRep.List (List.map (fn x883 => 
                                                                                                            cvtNAMESPACE x883
                                                                                                     ) ls884)
                                                                     ) ls888))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x899, ident=s900}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x899), ("ident", PrettyRep.UniStr s900)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls909, x913)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x908 => cvtIDENT x908
                                                          ) ls909), cvtIDENT_EXPR x913]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r920) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r920))
     | cvtLITERAL (LiteralDecimal d923) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d923))
     | cvtLITERAL (LiteralInt i926) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i926))
     | cvtLITERAL (LiteralUInt u929) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u929))
     | cvtLITERAL (LiteralBoolean b932) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b932))
     | cvtLITERAL (LiteralString s935) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s935))
     | cvtLITERAL (LiteralArray{exprs=x938, ty=opt940}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x938), ("ty", 
       (case opt940 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x939 => PrettyRep.Ctor ("SOME", SOME (cvtTY x939))
       ))]))
     | cvtLITERAL (LiteralXML ls952) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x951 => 
                                                                                                           cvtEXPR x951
                                                                                                    ) ls952)))
     | cvtLITERAL (LiteralNamespace x958) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x958))
     | cvtLITERAL (LiteralObject{expr=ls962, ty=opt967}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x961 => 
                                                                        cvtFIELD x961
                                                                 ) ls962)), 
          ("ty", 
       (case opt967 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x966 => PrettyRep.Ctor ("SOME", SOME (cvtTY x966))
       ))]))
     | cvtLITERAL (LiteralFunction x978) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x978))
     | cvtLITERAL (LiteralRegExp{str=s981}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s981)]))
   and cvtBLOCK (Block x987) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x987))
   and cvtFIXTURE (NamespaceFixture x990) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x990))
     | cvtFIXTURE (ClassFixture x993) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x993))
     | cvtFIXTURE (InterfaceFixture x996) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x996))
     | cvtFIXTURE (TypeVarFixture x999) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x999))
     | cvtFIXTURE (TypeFixture x1002) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1002))
     | cvtFIXTURE (MethodFixture{func=x1005, ty=x1006, readOnly=b1007, override=b1008, 
          final=b1009}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1005), ("ty", cvtTY x1006), ("readOnly", PrettyRep.Bool b1007), 
          ("override", PrettyRep.Bool b1008), ("final", PrettyRep.Bool b1009)]))
     | cvtFIXTURE (ValFixture{ty=x1023, readOnly=b1024}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1023), ("readOnly", PrettyRep.Bool b1024)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1032, getter=opt1034, setter=opt1039}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1032), ("getter", 
       (case opt1034 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1033 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1033))
       )), ("setter", 
       (case opt1039 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1038 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1038))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1052, baseTypeArgs=ls1054}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1052), ("baseTypeArgs", PrettyRep.List (List.map (fn x1053 => 
                                                                           cvtTY x1053
                                                                    ) ls1054))]))
   and cvtHEAD (Head(x1065, x1066)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1065, 
          cvtINITS x1066]))
   and cvtBINDINGS (ls1071, ls1076) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1070 => 
                                                                                       cvtBINDING x1070
                                                                                ) ls1071), 
          PrettyRep.List (List.map (fn x1075 => cvtINIT_STEP x1075
                                   ) ls1076)]
   and cvtRIB ls1084 = PrettyRep.List (List.map (fn (x1081, x1082) => PrettyRep.Tuple [cvtFIXTURE_NAME x1081, 
                                                       cvtFIXTURE x1082]
                                                ) ls1084)
   and cvtRIBS ls1095 = PrettyRep.List (List.map (fn ls1091 => PrettyRep.List (List.map (fn (x1088, 
                                                                                               x1089) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1088, 
                                                                                               cvtFIXTURE x1089]
                                                                                        ) ls1091)
                                                 ) ls1095)
   and cvtINITS ls1102 = PrettyRep.List (List.map (fn (x1099, x1100) => PrettyRep.Tuple [cvtFIXTURE_NAME x1099, 
                                                         cvtEXPR x1100]
                                                  ) ls1102)
   and cvtINSTANCE_TYPE {name=x1106, typeParams=ls1108, typeArgs=ls1113, nonnullable=b1117, 
          superTypes=ls1119, ty=x1123, dynamic=b1124} = PrettyRep.Rec [("name", 
          cvtNAME x1106), ("typeParams", PrettyRep.List (List.map (fn x1107 => 
                                                                         cvtIDENT x1107
                                                                  ) ls1108)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1112 => cvtTYPE_EXPR x1112
                                                ) ls1113)), ("nonnullable", 
          PrettyRep.Bool b1117), ("superTypes", PrettyRep.List (List.map (fn x1118 => 
                                                                                cvtTYPE_EXPR x1118
                                                                         ) ls1119)), 
          ("ty", cvtTYPE_EXPR x1123), ("dynamic", PrettyRep.Bool b1124)]
   and cvtFIELD {kind=x1140, name=x1141, init=x1142} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1140), ("name", cvtIDENT_EXPR x1141), ("init", cvtEXPR x1142)]
   and cvtFIELD_TYPE {name=x1150, ty=x1151} = PrettyRep.Rec [("name", cvtIDENT x1150), 
          ("ty", cvtTYPE_EXPR x1151)]
   and cvtFUNC_TYPE {params=ls1158, result=x1162, thisType=opt1164, hasRest=b1168, 
          minArgs=n1169} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1157 => 
                                                                                     cvtTYPE_EXPR x1157
                                                                              ) ls1158)), 
          ("result", cvtTYPE_EXPR x1162), ("thisType", 
       (case opt1164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1163 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1163))
       )), ("hasRest", PrettyRep.Bool b1168), ("minArgs", PrettyRep.Int n1169)]
   and cvtFUNC_DEFN {kind=x1181, ns=opt1183, final=b1187, override=b1188, prototype=b1189, 
          static=b1190, func=x1191} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1181), 
          ("ns", 
       (case opt1183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1182 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1182))
       )), ("final", PrettyRep.Bool b1187), ("override", PrettyRep.Bool b1188), 
          ("prototype", PrettyRep.Bool b1189), ("static", PrettyRep.Bool b1190), 
          ("func", cvtFUNC x1191)]
   and cvtCTOR_DEFN x1207 = cvtCTOR x1207
   and cvtVAR_DEFN {kind=x1208, ns=opt1210, static=b1214, prototype=b1215, 
          bindings=(ls1217, ls1222)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1208), 
          ("ns", 
       (case opt1210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1209 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1209))
       )), ("static", PrettyRep.Bool b1214), ("prototype", PrettyRep.Bool b1215), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1216 => 
                                                                        cvtBINDING x1216
                                                                 ) ls1217), 
          PrettyRep.List (List.map (fn x1221 => cvtINIT_STEP x1221
                                   ) ls1222)])]
   and cvtNAMESPACE_DEFN {ident=x1238, ns=opt1240, init=opt1245} = PrettyRep.Rec [("ident", 
          cvtIDENT x1238), ("ns", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1239))
       )), ("init", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1244))
       ))]
   and cvtCLASS_DEFN {ns=opt1257, ident=x1261, nonnullable=b1262, dynamic=b1263, 
          final=b1264, params=ls1266, extends=opt1271, implements=ls1276, classDefns=ls1281, 
          instanceDefns=ls1286, instanceStmts=ls1291, ctorDefn=opt1296} = PrettyRep.Rec [("ns", 
          
       (case opt1257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1256 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1256))
       )), ("ident", cvtIDENT x1261), ("nonnullable", PrettyRep.Bool b1262), 
          ("dynamic", PrettyRep.Bool b1263), ("final", PrettyRep.Bool b1264), 
          ("params", PrettyRep.List (List.map (fn x1265 => cvtIDENT x1265
                                              ) ls1266)), ("extends", 
       (case opt1271 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1270 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1270))
       )), ("implements", PrettyRep.List (List.map (fn x1275 => cvtTYPE_EXPR x1275
                                                   ) ls1276)), ("classDefns", 
          PrettyRep.List (List.map (fn x1280 => cvtDEFN x1280
                                   ) ls1281)), ("instanceDefns", PrettyRep.List (List.map (fn x1285 => 
                                                                                                 cvtDEFN x1285
                                                                                          ) ls1286)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1290 => cvtSTMT x1290
                                                     ) ls1291)), ("ctorDefn", 
          
       (case opt1296 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1295 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1295))
       ))]
   and cvtINTERFACE_DEFN {ident=x1325, ns=opt1327, nonnullable=b1331, params=ls1333, 
          extends=ls1338, instanceDefns=ls1343} = PrettyRep.Rec [("ident", 
          cvtIDENT x1325), ("ns", 
       (case opt1327 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1326 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1326))
       )), ("nonnullable", PrettyRep.Bool b1331), ("params", PrettyRep.List (List.map (fn x1332 => 
                                                                                             cvtIDENT x1332
                                                                                      ) ls1333)), 
          ("extends", PrettyRep.List (List.map (fn x1337 => cvtTYPE_EXPR x1337
                                               ) ls1338)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1342 => cvtDEFN x1342
                                   ) ls1343))]
   and cvtTYPE_DEFN {ident=x1360, ns=opt1362, init=x1366} = PrettyRep.Rec [("ident", 
          cvtIDENT x1360), ("ns", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1361))
       )), ("init", cvtTYPE_EXPR x1366)]
   and cvtCLASS_BLOCK {ns=opt1375, ident=x1379, name=opt1381, block=x1385} = 
          PrettyRep.Rec [("ns", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1374 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1374))
       )), ("ident", cvtIDENT x1379), ("name", 
       (case opt1381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1380 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1380))
       )), ("block", cvtBLOCK x1385)]
   and cvtFOR_ENUM_HEAD {isEach=b1395, bindings=(ls1397, ls1402), expr=x1407} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1395), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1396 => 
                                                                                                                         cvtBINDING x1396
                                                                                                                  ) ls1397), 
          PrettyRep.List (List.map (fn x1401 => cvtINIT_STEP x1401
                                   ) ls1402)]), ("expr", cvtEXPR x1407)]
   and cvtFOR_ENUM_STMT {isEach=b1415, defn=opt1446, obj=x1450, rib=opt1458, 
          next=x1462, labels=ls1464, body=x1468} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1415), ("defn", 
       (case opt1446 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1416, ns=opt1418, static=b1422, prototype=b1423, bindings=(ls1425, 
            ls1430)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1416), ("ns", 
         (case opt1418 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1417 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1417))
         )), ("static", PrettyRep.Bool b1422), ("prototype", PrettyRep.Bool b1423), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1424 => 
                                                                          cvtBINDING x1424
                                                                   ) ls1425), 
            PrettyRep.List (List.map (fn x1429 => cvtINIT_STEP x1429
                                     ) ls1430)])]))
       )), ("obj", cvtEXPR x1450), ("rib", 
       (case opt1458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1454 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1451, 
                                                                                      x1452) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1451, 
                                                                                      cvtFIXTURE x1452]
                                                                               ) ls1454)))
       )), ("next", cvtSTMT x1462), ("labels", PrettyRep.List (List.map (fn x1463 => 
                                                                               cvtIDENT x1463
                                                                        ) ls1464)), 
          ("body", cvtSTMT x1468)]
   and cvtFOR_STMT {rib=opt1491, defn=opt1525, init=ls1530, cond=x1534, update=x1535, 
          labels=ls1537, body=x1541} = PrettyRep.Rec [("rib", 
       (case opt1491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1487 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1484, 
                                                                                      x1485) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1484, 
                                                                                      cvtFIXTURE x1485]
                                                                               ) ls1487)))
       )), ("defn", 
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1495, ns=opt1497, static=b1501, prototype=b1502, bindings=(ls1504, 
            ls1509)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1495), ("ns", 
         (case opt1497 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1496 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1496))
         )), ("static", PrettyRep.Bool b1501), ("prototype", PrettyRep.Bool b1502), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1503 => 
                                                                          cvtBINDING x1503
                                                                   ) ls1504), 
            PrettyRep.List (List.map (fn x1508 => cvtINIT_STEP x1508
                                     ) ls1509)])]))
       )), ("init", PrettyRep.List (List.map (fn x1529 => cvtSTMT x1529
                                             ) ls1530)), ("cond", cvtEXPR x1534), 
          ("update", cvtEXPR x1535), ("labels", PrettyRep.List (List.map (fn x1536 => 
                                                                                cvtIDENT x1536
                                                                         ) ls1537)), 
          ("body", cvtSTMT x1541)]
   and cvtWHILE_STMT {cond=x1557, rib=opt1565, body=x1569, labels=ls1571} = 
          PrettyRep.Rec [("cond", cvtEXPR x1557), ("rib", 
       (case opt1565 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1561 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1558, 
                                                                                      x1559) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1558, 
                                                                                      cvtFIXTURE x1559]
                                                                               ) ls1561)))
       )), ("body", cvtSTMT x1569), ("labels", PrettyRep.List (List.map (fn x1570 => 
                                                                               cvtIDENT x1570
                                                                        ) ls1571))]
   and cvtDIRECTIVES {pragmas=ls1585, defns=ls1590, head=opt1595, body=ls1600, 
          loc=opt1605} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1584 => 
                                                                                    cvtPRAGMA x1584
                                                                             ) ls1585)), 
          ("defns", PrettyRep.List (List.map (fn x1589 => cvtDEFN x1589
                                             ) ls1590)), ("head", 
       (case opt1595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1594 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1594))
       )), ("body", PrettyRep.List (List.map (fn x1599 => cvtSTMT x1599
                                             ) ls1600)), ("loc", 
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1604 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1604))
       ))]
   and cvtCASE {label=opt1621, inits=opt1632, body=x1636} = PrettyRep.Rec [("label", 
          
       (case opt1621 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1620 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1620))
       )), ("inits", 
       (case opt1632 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1628 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1625, 
                                                                                      x1626) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1625, 
                                                                                      cvtEXPR x1626]
                                                                               ) ls1628)))
       )), ("body", cvtBLOCK x1636)]
   and cvtCATCH_CLAUSE {bindings=(ls1645, ls1650), ty=x1655, rib=opt1663, inits=opt1674, 
          block=x1678} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1644 => 
                                                                                                      cvtBINDING x1644
                                                                                               ) ls1645), 
          PrettyRep.List (List.map (fn x1649 => cvtINIT_STEP x1649
                                   ) ls1650)]), ("ty", cvtTY x1655), ("rib", 
          
       (case opt1663 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1659 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1656, 
                                                                                      x1657) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1656, 
                                                                                      cvtFIXTURE x1657]
                                                                               ) ls1659)))
       )), ("inits", 
       (case opt1674 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1670 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1667, 
                                                                                      x1668) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1667, 
                                                                                      cvtEXPR x1668]
                                                                               ) ls1670)))
       )), ("block", cvtBLOCK x1678)]
   and cvtFUNC_NAME {kind=x1690, ident=x1691} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1690), 
          ("ident", cvtIDENT x1691)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1697, getter=opt1699, setter=opt1704} = 
          PrettyRep.Rec [("ty", cvtTY x1697), ("getter", 
       (case opt1699 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1698 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1698))
       )), ("setter", 
       (case opt1704 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1703 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1703))
       ))]
   and cvtFRAGMENT (Unit{name=opt1716, fragments=ls1721}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1716 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1715 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1715))
       )), ("fragments", PrettyRep.List (List.map (fn x1720 => cvtFRAGMENT x1720
                                                  ) ls1721))]))
     | cvtFRAGMENT (Package{name=ls1733, fragments=ls1738}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1732 => 
                                                                        cvtIDENT x1732
                                                                 ) ls1733)), 
          ("fragments", PrettyRep.List (List.map (fn x1737 => cvtFRAGMENT x1737
                                                 ) ls1738))]))
     | cvtFRAGMENT (Anon x1749) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1749))
end

