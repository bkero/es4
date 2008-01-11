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
     | cvtEXPR (ThisExpr opt701) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt701 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x700 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x700))
       ))
     | cvtEXPR (YieldExpr opt708) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt708 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x707 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x707))
       ))
     | cvtEXPR (SuperExpr opt715) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt715 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x714 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x714))
       ))
     | cvtEXPR (LiteralExpr x721) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x721))
     | cvtEXPR (CallExpr{func=x724, actuals=ls726}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x724), ("actuals", PrettyRep.List (List.map (fn x725 => 
                                                                                                   cvtEXPR x725
                                                                                            ) ls726))]))
     | cvtEXPR (ApplyTypeExpr{expr=x737, actuals=ls739}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x737), ("actuals", PrettyRep.List (List.map (fn x738 => 
                                                                                                   cvtTY x738
                                                                                            ) ls739))]))
     | cvtEXPR (LetExpr{defs=x750, body=x751, head=opt753}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x750), ("body", cvtEXPR x751), 
          ("head", 
       (case opt753 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x752 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x752))
       ))]))
     | cvtEXPR (NewExpr{obj=x766, actuals=ls768}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x766), ("actuals", PrettyRep.List (List.map (fn x767 => 
                                                                                                  cvtEXPR x767
                                                                                           ) ls768))]))
     | cvtEXPR (ObjectRef{base=x779, ident=x780, loc=opt782}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x779), ("ident", cvtIDENT_EXPR x780), 
          ("loc", 
       (case opt782 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x781 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x781))
       ))]))
     | cvtEXPR (LexicalRef{ident=x795, loc=opt797}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x795), ("loc", 
       (case opt797 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x796 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x796))
       ))]))
     | cvtEXPR (SetExpr(x808, x809, x810)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x808, 
          cvtEXPR x809, cvtEXPR x810]))
     | cvtEXPR (ListExpr ls815) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x814 => 
                                                                                                    cvtEXPR x814
                                                                                             ) ls815)))
     | cvtEXPR (InitExpr(x821, x822, x823)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x821, 
          cvtHEAD x822, cvtINITS x823]))
     | cvtEXPR (GetTemp n827) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n827))
     | cvtEXPR (GetParam n830) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n830))
     | cvtEXPR (Comprehension(x833, ls835, opt840)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x833, PrettyRep.List (List.map (fn x834 => 
                                                                               cvtFOR_ENUM_HEAD x834
                                                                        ) ls835), 
          
       (case opt840 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x839 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x839))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n852) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n852))
     | cvtFIXTURE_NAME (PropName x855) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x855))
   and cvtIDENT_EXPR (Identifier{ident=x858, openNamespaces=ls864}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x858), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls860 => PrettyRep.List (List.map (fn x859 => 
                                                                                cvtNAMESPACE x859
                                                                         ) ls860)
                                   ) ls864))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x875, expr=x876}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x875), ("expr", cvtEXPR x876)]))
     | cvtIDENT_EXPR (AttributeIdentifier x884) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x884))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x887, openNamespaces=ls893}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x887), ("openNamespaces", PrettyRep.List (List.map (fn ls889 => 
                                                                            PrettyRep.List (List.map (fn x888 => 
                                                                                                            cvtNAMESPACE x888
                                                                                                     ) ls889)
                                                                     ) ls893))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x904, ident=s905}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x904), ("ident", PrettyRep.UniStr s905)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls914, x918)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x913 => cvtIDENT x913
                                                          ) ls914), cvtIDENT_EXPR x918]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r925) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r925))
     | cvtLITERAL (LiteralDecimal d928) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d928))
     | cvtLITERAL (LiteralInt i931) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i931))
     | cvtLITERAL (LiteralUInt u934) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u934))
     | cvtLITERAL (LiteralBoolean b937) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b937))
     | cvtLITERAL (LiteralString s940) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s940))
     | cvtLITERAL (LiteralArray{exprs=x943, ty=opt945}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x943), ("ty", 
       (case opt945 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x944 => PrettyRep.Ctor ("SOME", SOME (cvtTY x944))
       ))]))
     | cvtLITERAL (LiteralXML ls957) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x956 => 
                                                                                                           cvtEXPR x956
                                                                                                    ) ls957)))
     | cvtLITERAL (LiteralNamespace x963) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x963))
     | cvtLITERAL (LiteralObject{expr=ls967, ty=opt972}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x966 => 
                                                                        cvtFIELD x966
                                                                 ) ls967)), 
          ("ty", 
       (case opt972 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x971 => PrettyRep.Ctor ("SOME", SOME (cvtTY x971))
       ))]))
     | cvtLITERAL (LiteralFunction x983) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x983))
     | cvtLITERAL (LiteralRegExp{str=s986}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s986)]))
   and cvtBLOCK (Block x992) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x992))
   and cvtFIXTURE (NamespaceFixture x995) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x995))
     | cvtFIXTURE (ClassFixture x998) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x998))
     | cvtFIXTURE (InterfaceFixture x1001) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1001))
     | cvtFIXTURE (TypeVarFixture x1004) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x1004))
     | cvtFIXTURE (TypeFixture x1007) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1007))
     | cvtFIXTURE (MethodFixture{func=x1010, ty=x1011, readOnly=b1012, override=b1013, 
          final=b1014}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1010), ("ty", cvtTY x1011), ("readOnly", PrettyRep.Bool b1012), 
          ("override", PrettyRep.Bool b1013), ("final", PrettyRep.Bool b1014)]))
     | cvtFIXTURE (ValFixture{ty=x1028, readOnly=b1029}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1028), ("readOnly", PrettyRep.Bool b1029)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1037, getter=opt1039, setter=opt1044}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1037), ("getter", 
       (case opt1039 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1038 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1038))
       )), ("setter", 
       (case opt1044 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1043 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1043))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1057, baseTypeArgs=ls1059}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1057), ("baseTypeArgs", PrettyRep.List (List.map (fn x1058 => 
                                                                           cvtTY x1058
                                                                    ) ls1059))]))
   and cvtHEAD (Head(x1070, x1071)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1070, 
          cvtINITS x1071]))
   and cvtBINDINGS (ls1076, ls1081) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1075 => 
                                                                                       cvtBINDING x1075
                                                                                ) ls1076), 
          PrettyRep.List (List.map (fn x1080 => cvtINIT_STEP x1080
                                   ) ls1081)]
   and cvtRIB ls1089 = PrettyRep.List (List.map (fn (x1086, x1087) => PrettyRep.Tuple [cvtFIXTURE_NAME x1086, 
                                                       cvtFIXTURE x1087]
                                                ) ls1089)
   and cvtRIBS ls1100 = PrettyRep.List (List.map (fn ls1096 => PrettyRep.List (List.map (fn (x1093, 
                                                                                               x1094) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1093, 
                                                                                               cvtFIXTURE x1094]
                                                                                        ) ls1096)
                                                 ) ls1100)
   and cvtINITS ls1107 = PrettyRep.List (List.map (fn (x1104, x1105) => PrettyRep.Tuple [cvtFIXTURE_NAME x1104, 
                                                         cvtEXPR x1105]
                                                  ) ls1107)
   and cvtINSTANCE_TYPE {name=x1111, typeParams=ls1113, typeArgs=ls1118, nonnullable=b1122, 
          superTypes=ls1124, ty=x1128, dynamic=b1129} = PrettyRep.Rec [("name", 
          cvtNAME x1111), ("typeParams", PrettyRep.List (List.map (fn x1112 => 
                                                                         cvtIDENT x1112
                                                                  ) ls1113)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1117 => cvtTYPE_EXPR x1117
                                                ) ls1118)), ("nonnullable", 
          PrettyRep.Bool b1122), ("superTypes", PrettyRep.List (List.map (fn x1123 => 
                                                                                cvtTYPE_EXPR x1123
                                                                         ) ls1124)), 
          ("ty", cvtTYPE_EXPR x1128), ("dynamic", PrettyRep.Bool b1129)]
   and cvtFIELD {kind=x1145, name=x1146, init=x1147} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1145), ("name", cvtIDENT_EXPR x1146), ("init", cvtEXPR x1147)]
   and cvtFIELD_TYPE {name=x1155, ty=x1156} = PrettyRep.Rec [("name", cvtIDENT x1155), 
          ("ty", cvtTYPE_EXPR x1156)]
   and cvtFUNC_TYPE {params=ls1163, result=x1167, thisType=opt1169, hasRest=b1173, 
          minArgs=n1174} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1162 => 
                                                                                     cvtTYPE_EXPR x1162
                                                                              ) ls1163)), 
          ("result", cvtTYPE_EXPR x1167), ("thisType", 
       (case opt1169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1168 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1168))
       )), ("hasRest", PrettyRep.Bool b1173), ("minArgs", PrettyRep.Int n1174)]
   and cvtFUNC_DEFN {kind=x1186, ns=opt1188, final=b1192, override=b1193, prototype=b1194, 
          static=b1195, func=x1196} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1186), 
          ("ns", 
       (case opt1188 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1187 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1187))
       )), ("final", PrettyRep.Bool b1192), ("override", PrettyRep.Bool b1193), 
          ("prototype", PrettyRep.Bool b1194), ("static", PrettyRep.Bool b1195), 
          ("func", cvtFUNC x1196)]
   and cvtCTOR_DEFN x1212 = cvtCTOR x1212
   and cvtVAR_DEFN {kind=x1213, ns=opt1215, static=b1219, prototype=b1220, 
          bindings=(ls1222, ls1227)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1213), 
          ("ns", 
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1214 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1214))
       )), ("static", PrettyRep.Bool b1219), ("prototype", PrettyRep.Bool b1220), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1221 => 
                                                                        cvtBINDING x1221
                                                                 ) ls1222), 
          PrettyRep.List (List.map (fn x1226 => cvtINIT_STEP x1226
                                   ) ls1227)])]
   and cvtNAMESPACE_DEFN {ident=x1243, ns=opt1245, init=opt1250} = PrettyRep.Rec [("ident", 
          cvtIDENT x1243), ("ns", 
       (case opt1245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1244 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1244))
       )), ("init", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1249))
       ))]
   and cvtCLASS_DEFN {ns=opt1262, ident=x1266, nonnullable=b1267, dynamic=b1268, 
          final=b1269, params=ls1271, extends=opt1276, implements=ls1281, classDefns=ls1286, 
          instanceDefns=ls1291, instanceStmts=ls1296, ctorDefn=opt1301} = PrettyRep.Rec [("ns", 
          
       (case opt1262 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1261 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1261))
       )), ("ident", cvtIDENT x1266), ("nonnullable", PrettyRep.Bool b1267), 
          ("dynamic", PrettyRep.Bool b1268), ("final", PrettyRep.Bool b1269), 
          ("params", PrettyRep.List (List.map (fn x1270 => cvtIDENT x1270
                                              ) ls1271)), ("extends", 
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1275))
       )), ("implements", PrettyRep.List (List.map (fn x1280 => cvtTYPE_EXPR x1280
                                                   ) ls1281)), ("classDefns", 
          PrettyRep.List (List.map (fn x1285 => cvtDEFN x1285
                                   ) ls1286)), ("instanceDefns", PrettyRep.List (List.map (fn x1290 => 
                                                                                                 cvtDEFN x1290
                                                                                          ) ls1291)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1295 => cvtSTMT x1295
                                                     ) ls1296)), ("ctorDefn", 
          
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1300))
       ))]
   and cvtINTERFACE_DEFN {ident=x1330, ns=opt1332, nonnullable=b1336, params=ls1338, 
          extends=ls1343, instanceDefns=ls1348} = PrettyRep.Rec [("ident", 
          cvtIDENT x1330), ("ns", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1331))
       )), ("nonnullable", PrettyRep.Bool b1336), ("params", PrettyRep.List (List.map (fn x1337 => 
                                                                                             cvtIDENT x1337
                                                                                      ) ls1338)), 
          ("extends", PrettyRep.List (List.map (fn x1342 => cvtTYPE_EXPR x1342
                                               ) ls1343)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1347 => cvtDEFN x1347
                                   ) ls1348))]
   and cvtTYPE_DEFN {ident=x1365, ns=opt1367, init=x1371} = PrettyRep.Rec [("ident", 
          cvtIDENT x1365), ("ns", 
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1366))
       )), ("init", cvtTYPE_EXPR x1371)]
   and cvtCLASS_BLOCK {ns=opt1380, ident=x1384, name=opt1386, block=x1390} = 
          PrettyRep.Rec [("ns", 
       (case opt1380 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1379 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1379))
       )), ("ident", cvtIDENT x1384), ("name", 
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1385))
       )), ("block", cvtBLOCK x1390)]
   and cvtFOR_ENUM_HEAD {isEach=b1400, bindings=(ls1402, ls1407), expr=x1412} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1400), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1401 => 
                                                                                                                         cvtBINDING x1401
                                                                                                                  ) ls1402), 
          PrettyRep.List (List.map (fn x1406 => cvtINIT_STEP x1406
                                   ) ls1407)]), ("expr", cvtEXPR x1412)]
   and cvtFOR_ENUM_STMT {isEach=b1420, defn=opt1451, obj=x1455, rib=opt1463, 
          next=x1467, labels=ls1469, body=x1473} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1420), ("defn", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1421, ns=opt1423, static=b1427, prototype=b1428, bindings=(ls1430, 
            ls1435)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1421), ("ns", 
         (case opt1423 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1422 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1422))
         )), ("static", PrettyRep.Bool b1427), ("prototype", PrettyRep.Bool b1428), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1429 => 
                                                                          cvtBINDING x1429
                                                                   ) ls1430), 
            PrettyRep.List (List.map (fn x1434 => cvtINIT_STEP x1434
                                     ) ls1435)])]))
       )), ("obj", cvtEXPR x1455), ("rib", 
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1459 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1456, 
                                                                                      x1457) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1456, 
                                                                                      cvtFIXTURE x1457]
                                                                               ) ls1459)))
       )), ("next", cvtSTMT x1467), ("labels", PrettyRep.List (List.map (fn x1468 => 
                                                                               cvtIDENT x1468
                                                                        ) ls1469)), 
          ("body", cvtSTMT x1473)]
   and cvtFOR_STMT {rib=opt1496, defn=opt1530, init=ls1535, cond=x1539, update=x1540, 
          labels=ls1542, body=x1546} = PrettyRep.Rec [("rib", 
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1492 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1489, 
                                                                                      x1490) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1489, 
                                                                                      cvtFIXTURE x1490]
                                                                               ) ls1492)))
       )), ("defn", 
       (case opt1530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1500, ns=opt1502, static=b1506, prototype=b1507, bindings=(ls1509, 
            ls1514)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1500), ("ns", 
         (case opt1502 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1501 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1501))
         )), ("static", PrettyRep.Bool b1506), ("prototype", PrettyRep.Bool b1507), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1508 => 
                                                                          cvtBINDING x1508
                                                                   ) ls1509), 
            PrettyRep.List (List.map (fn x1513 => cvtINIT_STEP x1513
                                     ) ls1514)])]))
       )), ("init", PrettyRep.List (List.map (fn x1534 => cvtSTMT x1534
                                             ) ls1535)), ("cond", cvtEXPR x1539), 
          ("update", cvtEXPR x1540), ("labels", PrettyRep.List (List.map (fn x1541 => 
                                                                                cvtIDENT x1541
                                                                         ) ls1542)), 
          ("body", cvtSTMT x1546)]
   and cvtWHILE_STMT {cond=x1562, rib=opt1570, body=x1574, labels=ls1576} = 
          PrettyRep.Rec [("cond", cvtEXPR x1562), ("rib", 
       (case opt1570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1566 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1563, 
                                                                                      x1564) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1563, 
                                                                                      cvtFIXTURE x1564]
                                                                               ) ls1566)))
       )), ("body", cvtSTMT x1574), ("labels", PrettyRep.List (List.map (fn x1575 => 
                                                                               cvtIDENT x1575
                                                                        ) ls1576))]
   and cvtDIRECTIVES {pragmas=ls1590, defns=ls1595, head=opt1600, body=ls1605, 
          loc=opt1610} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1589 => 
                                                                                    cvtPRAGMA x1589
                                                                             ) ls1590)), 
          ("defns", PrettyRep.List (List.map (fn x1594 => cvtDEFN x1594
                                             ) ls1595)), ("head", 
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1599 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1599))
       )), ("body", PrettyRep.List (List.map (fn x1604 => cvtSTMT x1604
                                             ) ls1605)), ("loc", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1609 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1609))
       ))]
   and cvtCASE {label=opt1626, inits=opt1637, body=x1641} = PrettyRep.Rec [("label", 
          
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1625 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1625))
       )), ("inits", 
       (case opt1637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1633 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1630, 
                                                                                      x1631) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1630, 
                                                                                      cvtEXPR x1631]
                                                                               ) ls1633)))
       )), ("body", cvtBLOCK x1641)]
   and cvtCATCH_CLAUSE {bindings=(ls1650, ls1655), ty=x1660, rib=opt1668, inits=opt1679, 
          block=x1683} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1649 => 
                                                                                                      cvtBINDING x1649
                                                                                               ) ls1650), 
          PrettyRep.List (List.map (fn x1654 => cvtINIT_STEP x1654
                                   ) ls1655)]), ("ty", cvtTY x1660), ("rib", 
          
       (case opt1668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1664 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1661, 
                                                                                      x1662) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1661, 
                                                                                      cvtFIXTURE x1662]
                                                                               ) ls1664)))
       )), ("inits", 
       (case opt1679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1675 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1672, 
                                                                                      x1673) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1672, 
                                                                                      cvtEXPR x1673]
                                                                               ) ls1675)))
       )), ("block", cvtBLOCK x1683)]
   and cvtFUNC_NAME {kind=x1695, ident=x1696} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1695), 
          ("ident", cvtIDENT x1696)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1702, getter=opt1704, setter=opt1709} = 
          PrettyRep.Rec [("ty", cvtTY x1702), ("getter", 
       (case opt1704 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1703 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1703))
       )), ("setter", 
       (case opt1709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1708 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1708))
       ))]
   and cvtFRAGMENT (Unit{name=opt1721, fragments=ls1726}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1721 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1720 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1720))
       )), ("fragments", PrettyRep.List (List.map (fn x1725 => cvtFRAGMENT x1725
                                                  ) ls1726))]))
     | cvtFRAGMENT (Package{name=ls1738, fragments=ls1743}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1737 => 
                                                                        cvtIDENT x1737
                                                                 ) ls1738)), 
          ("fragments", PrettyRep.List (List.map (fn x1742 => cvtFRAGMENT x1742
                                                 ) ls1743))]))
     | cvtFRAGMENT (Anon x1754) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1754))
end

