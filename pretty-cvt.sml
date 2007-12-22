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
     | cvtEXPR (ExpectedTypeExpr(x687, x688)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x687, cvtEXPR x688]))
     | cvtEXPR (UnaryExpr(x692, x693)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x692, 
          cvtEXPR x693]))
     | cvtEXPR (TypeExpr x697) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x697))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt702) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt702 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x701 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x701))
       ))
     | cvtEXPR (SuperExpr opt709) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x708 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x708))
       ))
     | cvtEXPR (LiteralExpr x715) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x715))
     | cvtEXPR (CallExpr{func=x718, actuals=ls720}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x718), ("actuals", PrettyRep.List (List.map (fn x719 => 
                                                                                                   cvtEXPR x719
                                                                                            ) ls720))]))
     | cvtEXPR (ApplyTypeExpr{expr=x731, actuals=ls733}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x731), ("actuals", PrettyRep.List (List.map (fn x732 => 
                                                                                                   cvtTY x732
                                                                                            ) ls733))]))
     | cvtEXPR (LetExpr{defs=x744, body=x745, head=opt747}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x744), ("body", cvtEXPR x745), 
          ("head", 
       (case opt747 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x746 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x746))
       ))]))
     | cvtEXPR (NewExpr{obj=x760, actuals=ls762}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x760), ("actuals", PrettyRep.List (List.map (fn x761 => 
                                                                                                  cvtEXPR x761
                                                                                           ) ls762))]))
     | cvtEXPR (ObjectRef{base=x773, ident=x774, loc=opt776}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x773), ("ident", cvtIDENT_EXPR x774), 
          ("loc", 
       (case opt776 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x775 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x775))
       ))]))
     | cvtEXPR (LexicalRef{ident=x789, loc=opt791}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x789), ("loc", 
       (case opt791 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x790 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x790))
       ))]))
     | cvtEXPR (SetExpr(x802, x803, x804)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x802, 
          cvtEXPR x803, cvtEXPR x804]))
     | cvtEXPR (ListExpr ls809) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x808 => 
                                                                                                    cvtEXPR x808
                                                                                             ) ls809)))
     | cvtEXPR (InitExpr(x815, x816, x817)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x815, 
          cvtHEAD x816, cvtINITS x817]))
     | cvtEXPR (GetTemp n821) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n821))
     | cvtEXPR (GetParam n824) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n824))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n830) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n830))
     | cvtFIXTURE_NAME (PropName x833) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x833))
   and cvtIDENT_EXPR (Identifier{ident=x836, openNamespaces=ls842}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x836), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls838 => PrettyRep.List (List.map (fn x837 => 
                                                                                cvtNAMESPACE x837
                                                                         ) ls838)
                                   ) ls842))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x853, expr=x854}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x853), ("expr", cvtEXPR x854)]))
     | cvtIDENT_EXPR (AttributeIdentifier x862) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x862))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x865, openNamespaces=ls871}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x865), ("openNamespaces", PrettyRep.List (List.map (fn ls867 => 
                                                                            PrettyRep.List (List.map (fn x866 => 
                                                                                                            cvtNAMESPACE x866
                                                                                                     ) ls867)
                                                                     ) ls871))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x882, ident=s883}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x882), ("ident", PrettyRep.UniStr s883)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls892, x896)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x891 => cvtIDENT x891
                                                          ) ls892), cvtIDENT_EXPR x896]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r903) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r903))
     | cvtLITERAL (LiteralDecimal d906) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d906))
     | cvtLITERAL (LiteralInt i909) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i909))
     | cvtLITERAL (LiteralUInt u912) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u912))
     | cvtLITERAL (LiteralBoolean b915) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b915))
     | cvtLITERAL (LiteralString s918) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s918))
     | cvtLITERAL (LiteralArray{exprs=ls922, ty=opt927}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x921 => 
                                                                         cvtEXPR x921
                                                                  ) ls922)), 
          ("ty", 
       (case opt927 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x926 => PrettyRep.Ctor ("SOME", SOME (cvtTY x926))
       ))]))
     | cvtLITERAL (LiteralXML ls939) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x938 => 
                                                                                                           cvtEXPR x938
                                                                                                    ) ls939)))
     | cvtLITERAL (LiteralNamespace x945) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x945))
     | cvtLITERAL (LiteralObject{expr=ls949, ty=opt954}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x948 => 
                                                                        cvtFIELD x948
                                                                 ) ls949)), 
          ("ty", 
       (case opt954 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x953 => PrettyRep.Ctor ("SOME", SOME (cvtTY x953))
       ))]))
     | cvtLITERAL (LiteralFunction x965) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x965))
     | cvtLITERAL (LiteralRegExp{str=s968}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s968)]))
   and cvtBLOCK (Block x974) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x974))
   and cvtFIXTURE (NamespaceFixture x977) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x977))
     | cvtFIXTURE (ClassFixture x980) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x980))
     | cvtFIXTURE (InterfaceFixture x983) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x983))
     | cvtFIXTURE (TypeVarFixture x986) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x986))
     | cvtFIXTURE (TypeFixture x989) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x989))
     | cvtFIXTURE (MethodFixture{func=x992, ty=x993, readOnly=b994, override=b995, 
          final=b996}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x992), ("ty", cvtTY x993), ("readOnly", PrettyRep.Bool b994), 
          ("override", PrettyRep.Bool b995), ("final", PrettyRep.Bool b996)]))
     | cvtFIXTURE (ValFixture{ty=x1010, readOnly=b1011}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1010), ("readOnly", PrettyRep.Bool b1011)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1019, getter=opt1021, setter=opt1026}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1019), ("getter", 
       (case opt1021 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1020 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1020))
       )), ("setter", 
       (case opt1026 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1025 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1025))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1039, baseTypeArgs=ls1041}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1039), ("baseTypeArgs", PrettyRep.List (List.map (fn x1040 => 
                                                                           cvtTY x1040
                                                                    ) ls1041))]))
   and cvtHEAD (Head(x1052, x1053)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1052, 
          cvtINITS x1053]))
   and cvtBINDINGS (ls1058, ls1063) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1057 => 
                                                                                       cvtBINDING x1057
                                                                                ) ls1058), 
          PrettyRep.List (List.map (fn x1062 => cvtINIT_STEP x1062
                                   ) ls1063)]
   and cvtRIB ls1071 = PrettyRep.List (List.map (fn (x1068, x1069) => PrettyRep.Tuple [cvtFIXTURE_NAME x1068, 
                                                       cvtFIXTURE x1069]
                                                ) ls1071)
   and cvtRIBS ls1082 = PrettyRep.List (List.map (fn ls1078 => PrettyRep.List (List.map (fn (x1075, 
                                                                                               x1076) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1075, 
                                                                                               cvtFIXTURE x1076]
                                                                                        ) ls1078)
                                                 ) ls1082)
   and cvtINITS ls1089 = PrettyRep.List (List.map (fn (x1086, x1087) => PrettyRep.Tuple [cvtFIXTURE_NAME x1086, 
                                                         cvtEXPR x1087]
                                                  ) ls1089)
   and cvtINSTANCE_TYPE {name=x1093, typeParams=ls1095, typeArgs=ls1100, nonnullable=b1104, 
          superTypes=ls1106, ty=x1110, dynamic=b1111} = PrettyRep.Rec [("name", 
          cvtNAME x1093), ("typeParams", PrettyRep.List (List.map (fn x1094 => 
                                                                         cvtIDENT x1094
                                                                  ) ls1095)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1099 => cvtTYPE_EXPR x1099
                                                ) ls1100)), ("nonnullable", 
          PrettyRep.Bool b1104), ("superTypes", PrettyRep.List (List.map (fn x1105 => 
                                                                                cvtTYPE_EXPR x1105
                                                                         ) ls1106)), 
          ("ty", cvtTYPE_EXPR x1110), ("dynamic", PrettyRep.Bool b1111)]
   and cvtFIELD {kind=x1127, name=x1128, init=x1129} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1127), ("name", cvtIDENT_EXPR x1128), ("init", cvtEXPR x1129)]
   and cvtFIELD_TYPE {name=x1137, ty=x1138} = PrettyRep.Rec [("name", cvtIDENT x1137), 
          ("ty", cvtTYPE_EXPR x1138)]
   and cvtFUNC_TYPE {params=ls1145, result=x1149, thisType=opt1151, hasRest=b1155, 
          minArgs=n1156} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1144 => 
                                                                                     cvtTYPE_EXPR x1144
                                                                              ) ls1145)), 
          ("result", cvtTYPE_EXPR x1149), ("thisType", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1150))
       )), ("hasRest", PrettyRep.Bool b1155), ("minArgs", PrettyRep.Int n1156)]
   and cvtFUNC_DEFN {kind=x1168, ns=opt1170, final=b1174, override=b1175, prototype=b1176, 
          static=b1177, func=x1178} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1168), 
          ("ns", 
       (case opt1170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1169 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1169))
       )), ("final", PrettyRep.Bool b1174), ("override", PrettyRep.Bool b1175), 
          ("prototype", PrettyRep.Bool b1176), ("static", PrettyRep.Bool b1177), 
          ("func", cvtFUNC x1178)]
   and cvtCTOR_DEFN x1194 = cvtCTOR x1194
   and cvtVAR_DEFN {kind=x1195, ns=opt1197, static=b1201, prototype=b1202, 
          bindings=(ls1204, ls1209)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1195), 
          ("ns", 
       (case opt1197 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1196 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1196))
       )), ("static", PrettyRep.Bool b1201), ("prototype", PrettyRep.Bool b1202), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1203 => 
                                                                        cvtBINDING x1203
                                                                 ) ls1204), 
          PrettyRep.List (List.map (fn x1208 => cvtINIT_STEP x1208
                                   ) ls1209)])]
   and cvtNAMESPACE_DEFN {ident=x1225, ns=opt1227, init=opt1232} = PrettyRep.Rec [("ident", 
          cvtIDENT x1225), ("ns", 
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1226))
       )), ("init", 
       (case opt1232 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1231 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1231))
       ))]
   and cvtCLASS_DEFN {ns=opt1244, ident=x1248, nonnullable=b1249, dynamic=b1250, 
          final=b1251, params=ls1253, extends=opt1258, implements=ls1263, classDefns=ls1268, 
          instanceDefns=ls1273, instanceStmts=ls1278, ctorDefn=opt1283} = PrettyRep.Rec [("ns", 
          
       (case opt1244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1243 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1243))
       )), ("ident", cvtIDENT x1248), ("nonnullable", PrettyRep.Bool b1249), 
          ("dynamic", PrettyRep.Bool b1250), ("final", PrettyRep.Bool b1251), 
          ("params", PrettyRep.List (List.map (fn x1252 => cvtIDENT x1252
                                              ) ls1253)), ("extends", 
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1257))
       )), ("implements", PrettyRep.List (List.map (fn x1262 => cvtTYPE_EXPR x1262
                                                   ) ls1263)), ("classDefns", 
          PrettyRep.List (List.map (fn x1267 => cvtDEFN x1267
                                   ) ls1268)), ("instanceDefns", PrettyRep.List (List.map (fn x1272 => 
                                                                                                 cvtDEFN x1272
                                                                                          ) ls1273)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1277 => cvtSTMT x1277
                                                     ) ls1278)), ("ctorDefn", 
          
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1282))
       ))]
   and cvtINTERFACE_DEFN {ident=x1312, ns=opt1314, nonnullable=b1318, params=ls1320, 
          extends=ls1325, instanceDefns=ls1330} = PrettyRep.Rec [("ident", 
          cvtIDENT x1312), ("ns", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1313))
       )), ("nonnullable", PrettyRep.Bool b1318), ("params", PrettyRep.List (List.map (fn x1319 => 
                                                                                             cvtIDENT x1319
                                                                                      ) ls1320)), 
          ("extends", PrettyRep.List (List.map (fn x1324 => cvtTYPE_EXPR x1324
                                               ) ls1325)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1329 => cvtDEFN x1329
                                   ) ls1330))]
   and cvtTYPE_DEFN {ident=x1347, ns=opt1349, init=x1353} = PrettyRep.Rec [("ident", 
          cvtIDENT x1347), ("ns", 
       (case opt1349 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1348 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1348))
       )), ("init", cvtTYPE_EXPR x1353)]
   and cvtCLASS_BLOCK {ns=opt1362, ident=x1366, name=opt1368, block=x1372} = 
          PrettyRep.Rec [("ns", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1361))
       )), ("ident", cvtIDENT x1366), ("name", 
       (case opt1368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1367 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1367))
       )), ("block", cvtBLOCK x1372)]
   and cvtFOR_ENUM_STMT {isEach=b1382, defn=opt1413, obj=x1417, rib=opt1425, 
          next=x1429, labels=ls1431, body=x1435} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1382), ("defn", 
       (case opt1413 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1383, ns=opt1385, static=b1389, prototype=b1390, bindings=(ls1392, 
            ls1397)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1383), ("ns", 
         (case opt1385 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1384 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1384))
         )), ("static", PrettyRep.Bool b1389), ("prototype", PrettyRep.Bool b1390), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1391 => 
                                                                          cvtBINDING x1391
                                                                   ) ls1392), 
            PrettyRep.List (List.map (fn x1396 => cvtINIT_STEP x1396
                                     ) ls1397)])]))
       )), ("obj", cvtEXPR x1417), ("rib", 
       (case opt1425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1421 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1418, 
                                                                                      x1419) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1418, 
                                                                                      cvtFIXTURE x1419]
                                                                               ) ls1421)))
       )), ("next", cvtSTMT x1429), ("labels", PrettyRep.List (List.map (fn x1430 => 
                                                                               cvtIDENT x1430
                                                                        ) ls1431)), 
          ("body", cvtSTMT x1435)]
   and cvtFOR_STMT {rib=opt1458, defn=opt1492, init=ls1497, cond=x1501, update=x1502, 
          labels=ls1504, body=x1508} = PrettyRep.Rec [("rib", 
       (case opt1458 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1454 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1451, 
                                                                                      x1452) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1451, 
                                                                                      cvtFIXTURE x1452]
                                                                               ) ls1454)))
       )), ("defn", 
       (case opt1492 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1462, ns=opt1464, static=b1468, prototype=b1469, bindings=(ls1471, 
            ls1476)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1462), ("ns", 
         (case opt1464 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1463 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1463))
         )), ("static", PrettyRep.Bool b1468), ("prototype", PrettyRep.Bool b1469), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1470 => 
                                                                          cvtBINDING x1470
                                                                   ) ls1471), 
            PrettyRep.List (List.map (fn x1475 => cvtINIT_STEP x1475
                                     ) ls1476)])]))
       )), ("init", PrettyRep.List (List.map (fn x1496 => cvtSTMT x1496
                                             ) ls1497)), ("cond", cvtEXPR x1501), 
          ("update", cvtEXPR x1502), ("labels", PrettyRep.List (List.map (fn x1503 => 
                                                                                cvtIDENT x1503
                                                                         ) ls1504)), 
          ("body", cvtSTMT x1508)]
   and cvtWHILE_STMT {cond=x1524, rib=opt1532, body=x1536, labels=ls1538} = 
          PrettyRep.Rec [("cond", cvtEXPR x1524), ("rib", 
       (case opt1532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1528 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1525, 
                                                                                      x1526) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1525, 
                                                                                      cvtFIXTURE x1526]
                                                                               ) ls1528)))
       )), ("body", cvtSTMT x1536), ("labels", PrettyRep.List (List.map (fn x1537 => 
                                                                               cvtIDENT x1537
                                                                        ) ls1538))]
   and cvtDIRECTIVES {pragmas=ls1552, defns=ls1557, head=opt1562, body=ls1567, 
          loc=opt1572} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1551 => 
                                                                                    cvtPRAGMA x1551
                                                                             ) ls1552)), 
          ("defns", PrettyRep.List (List.map (fn x1556 => cvtDEFN x1556
                                             ) ls1557)), ("head", 
       (case opt1562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1561 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1561))
       )), ("body", PrettyRep.List (List.map (fn x1566 => cvtSTMT x1566
                                             ) ls1567)), ("loc", 
       (case opt1572 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1571 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1571))
       ))]
   and cvtCASE {label=opt1588, inits=opt1599, body=x1603} = PrettyRep.Rec [("label", 
          
       (case opt1588 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1587 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1587))
       )), ("inits", 
       (case opt1599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1595 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1592, 
                                                                                      x1593) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1592, 
                                                                                      cvtEXPR x1593]
                                                                               ) ls1595)))
       )), ("body", cvtBLOCK x1603)]
   and cvtCATCH_CLAUSE {bindings=(ls1612, ls1617), ty=x1622, rib=opt1630, inits=opt1641, 
          block=x1645} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1611 => 
                                                                                                      cvtBINDING x1611
                                                                                               ) ls1612), 
          PrettyRep.List (List.map (fn x1616 => cvtINIT_STEP x1616
                                   ) ls1617)]), ("ty", cvtTY x1622), ("rib", 
          
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1626 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1623, 
                                                                                      x1624) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1623, 
                                                                                      cvtFIXTURE x1624]
                                                                               ) ls1626)))
       )), ("inits", 
       (case opt1641 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1637 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1634, 
                                                                                      x1635) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1634, 
                                                                                      cvtEXPR x1635]
                                                                               ) ls1637)))
       )), ("block", cvtBLOCK x1645)]
   and cvtFUNC_NAME {kind=x1657, ident=x1658} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1657), 
          ("ident", cvtIDENT x1658)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1664, getter=opt1666, setter=opt1671} = 
          PrettyRep.Rec [("ty", cvtTY x1664), ("getter", 
       (case opt1666 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1665 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1665))
       )), ("setter", 
       (case opt1671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1670 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1670))
       ))]
   and cvtFRAGMENT (Unit{name=opt1683, fragments=ls1688}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1682 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1682))
       )), ("fragments", PrettyRep.List (List.map (fn x1687 => cvtFRAGMENT x1687
                                                  ) ls1688))]))
     | cvtFRAGMENT (Package{name=ls1700, fragments=ls1705}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1699 => 
                                                                        cvtIDENT x1699
                                                                 ) ls1700)), 
          ("fragments", PrettyRep.List (List.map (fn x1704 => cvtFRAGMENT x1704
                                                 ) ls1705))]))
     | cvtFRAGMENT (Anon x1716) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1716))
end

