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
     | cvtLITERAL (LiteralBoolean b926) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b926))
     | cvtLITERAL (LiteralString s929) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s929))
     | cvtLITERAL (LiteralArray{exprs=x932, ty=opt934}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x932), ("ty", 
       (case opt934 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x933 => PrettyRep.Ctor ("SOME", SOME (cvtTY x933))
       ))]))
     | cvtLITERAL (LiteralXML ls946) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x945 => 
                                                                                                           cvtEXPR x945
                                                                                                    ) ls946)))
     | cvtLITERAL (LiteralNamespace x952) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x952))
     | cvtLITERAL (LiteralObject{expr=ls956, ty=opt961}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x955 => 
                                                                        cvtFIELD x955
                                                                 ) ls956)), 
          ("ty", 
       (case opt961 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x960 => PrettyRep.Ctor ("SOME", SOME (cvtTY x960))
       ))]))
     | cvtLITERAL (LiteralFunction x972) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x972))
     | cvtLITERAL (LiteralRegExp{str=s975}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s975)]))
   and cvtBLOCK (Block x981) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x981))
   and cvtFIXTURE (NamespaceFixture x984) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x984))
     | cvtFIXTURE (ClassFixture x987) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x987))
     | cvtFIXTURE (InterfaceFixture x990) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x990))
     | cvtFIXTURE (TypeVarFixture x993) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x993))
     | cvtFIXTURE (TypeFixture x996) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x996))
     | cvtFIXTURE (MethodFixture{func=x999, ty=x1000, readOnly=b1001, override=b1002, 
          final=b1003}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x999), ("ty", cvtTY x1000), ("readOnly", PrettyRep.Bool b1001), 
          ("override", PrettyRep.Bool b1002), ("final", PrettyRep.Bool b1003)]))
     | cvtFIXTURE (ValFixture{ty=x1017, readOnly=b1018}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1017), ("readOnly", PrettyRep.Bool b1018)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1026, getter=opt1028, setter=opt1033}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1026), ("getter", 
       (case opt1028 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1027 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1027))
       )), ("setter", 
       (case opt1033 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1032 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1032))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1046, baseTypeArgs=ls1048}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1046), ("baseTypeArgs", PrettyRep.List (List.map (fn x1047 => 
                                                                           cvtTY x1047
                                                                    ) ls1048))]))
   and cvtHEAD (Head(x1059, x1060)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1059, 
          cvtINITS x1060]))
   and cvtBINDINGS (ls1065, ls1070) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1064 => 
                                                                                       cvtBINDING x1064
                                                                                ) ls1065), 
          PrettyRep.List (List.map (fn x1069 => cvtINIT_STEP x1069
                                   ) ls1070)]
   and cvtRIB ls1078 = PrettyRep.List (List.map (fn (x1075, x1076) => PrettyRep.Tuple [cvtFIXTURE_NAME x1075, 
                                                       cvtFIXTURE x1076]
                                                ) ls1078)
   and cvtRIBS ls1089 = PrettyRep.List (List.map (fn ls1085 => PrettyRep.List (List.map (fn (x1082, 
                                                                                               x1083) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1082, 
                                                                                               cvtFIXTURE x1083]
                                                                                        ) ls1085)
                                                 ) ls1089)
   and cvtINITS ls1096 = PrettyRep.List (List.map (fn (x1093, x1094) => PrettyRep.Tuple [cvtFIXTURE_NAME x1093, 
                                                         cvtEXPR x1094]
                                                  ) ls1096)
   and cvtINSTANCE_TYPE {name=x1100, typeParams=ls1102, typeArgs=ls1107, nonnullable=b1111, 
          superTypes=ls1113, ty=x1117, dynamic=b1118} = PrettyRep.Rec [("name", 
          cvtNAME x1100), ("typeParams", PrettyRep.List (List.map (fn x1101 => 
                                                                         cvtIDENT x1101
                                                                  ) ls1102)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1106 => cvtTYPE_EXPR x1106
                                                ) ls1107)), ("nonnullable", 
          PrettyRep.Bool b1111), ("superTypes", PrettyRep.List (List.map (fn x1112 => 
                                                                                cvtTYPE_EXPR x1112
                                                                         ) ls1113)), 
          ("ty", cvtTYPE_EXPR x1117), ("dynamic", PrettyRep.Bool b1118)]
   and cvtFIELD {kind=x1134, name=x1135, init=x1136} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1134), ("name", cvtIDENT_EXPR x1135), ("init", cvtEXPR x1136)]
   and cvtFIELD_TYPE {name=x1144, ty=x1145} = PrettyRep.Rec [("name", cvtIDENT x1144), 
          ("ty", cvtTYPE_EXPR x1145)]
   and cvtFUNC_TYPE {params=ls1152, result=x1156, thisType=opt1158, hasRest=b1162, 
          minArgs=n1163} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1151 => 
                                                                                     cvtTYPE_EXPR x1151
                                                                              ) ls1152)), 
          ("result", cvtTYPE_EXPR x1156), ("thisType", 
       (case opt1158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1157 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1157))
       )), ("hasRest", PrettyRep.Bool b1162), ("minArgs", PrettyRep.Int n1163)]
   and cvtFUNC_DEFN {kind=x1175, ns=opt1177, final=b1181, override=b1182, prototype=b1183, 
          static=b1184, func=x1185} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1175), 
          ("ns", 
       (case opt1177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1176 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1176))
       )), ("final", PrettyRep.Bool b1181), ("override", PrettyRep.Bool b1182), 
          ("prototype", PrettyRep.Bool b1183), ("static", PrettyRep.Bool b1184), 
          ("func", cvtFUNC x1185)]
   and cvtCTOR_DEFN x1201 = cvtCTOR x1201
   and cvtVAR_DEFN {kind=x1202, ns=opt1204, static=b1208, prototype=b1209, 
          bindings=(ls1211, ls1216)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1202), 
          ("ns", 
       (case opt1204 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1203 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1203))
       )), ("static", PrettyRep.Bool b1208), ("prototype", PrettyRep.Bool b1209), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1210 => 
                                                                        cvtBINDING x1210
                                                                 ) ls1211), 
          PrettyRep.List (List.map (fn x1215 => cvtINIT_STEP x1215
                                   ) ls1216)])]
   and cvtNAMESPACE_DEFN {ident=x1232, ns=opt1234, init=opt1239} = PrettyRep.Rec [("ident", 
          cvtIDENT x1232), ("ns", 
       (case opt1234 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1233 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1233))
       )), ("init", 
       (case opt1239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1238 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1238))
       ))]
   and cvtCLASS_DEFN {ns=opt1251, ident=x1255, nonnullable=b1256, dynamic=b1257, 
          final=b1258, params=ls1260, extends=opt1265, implements=ls1270, classDefns=ls1275, 
          instanceDefns=ls1280, instanceStmts=ls1285, ctorDefn=opt1290} = PrettyRep.Rec [("ns", 
          
       (case opt1251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1250 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1250))
       )), ("ident", cvtIDENT x1255), ("nonnullable", PrettyRep.Bool b1256), 
          ("dynamic", PrettyRep.Bool b1257), ("final", PrettyRep.Bool b1258), 
          ("params", PrettyRep.List (List.map (fn x1259 => cvtIDENT x1259
                                              ) ls1260)), ("extends", 
       (case opt1265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1264 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1264))
       )), ("implements", PrettyRep.List (List.map (fn x1269 => cvtTYPE_EXPR x1269
                                                   ) ls1270)), ("classDefns", 
          PrettyRep.List (List.map (fn x1274 => cvtDEFN x1274
                                   ) ls1275)), ("instanceDefns", PrettyRep.List (List.map (fn x1279 => 
                                                                                                 cvtDEFN x1279
                                                                                          ) ls1280)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1284 => cvtSTMT x1284
                                                     ) ls1285)), ("ctorDefn", 
          
       (case opt1290 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1289 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1289))
       ))]
   and cvtINTERFACE_DEFN {ident=x1319, ns=opt1321, nonnullable=b1325, params=ls1327, 
          extends=ls1332, instanceDefns=ls1337} = PrettyRep.Rec [("ident", 
          cvtIDENT x1319), ("ns", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1320))
       )), ("nonnullable", PrettyRep.Bool b1325), ("params", PrettyRep.List (List.map (fn x1326 => 
                                                                                             cvtIDENT x1326
                                                                                      ) ls1327)), 
          ("extends", PrettyRep.List (List.map (fn x1331 => cvtTYPE_EXPR x1331
                                               ) ls1332)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1336 => cvtDEFN x1336
                                   ) ls1337))]
   and cvtTYPE_DEFN {ident=x1354, ns=opt1356, init=x1360} = PrettyRep.Rec [("ident", 
          cvtIDENT x1354), ("ns", 
       (case opt1356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1355 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1355))
       )), ("init", cvtTYPE_EXPR x1360)]
   and cvtCLASS_BLOCK {ns=opt1369, ident=x1373, name=opt1375, block=x1379} = 
          PrettyRep.Rec [("ns", 
       (case opt1369 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1368 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1368))
       )), ("ident", cvtIDENT x1373), ("name", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1374 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1374))
       )), ("block", cvtBLOCK x1379)]
   and cvtFOR_ENUM_HEAD {isEach=b1389, bindings=(ls1391, ls1396), expr=x1401} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1389), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1390 => 
                                                                                                                         cvtBINDING x1390
                                                                                                                  ) ls1391), 
          PrettyRep.List (List.map (fn x1395 => cvtINIT_STEP x1395
                                   ) ls1396)]), ("expr", cvtEXPR x1401)]
   and cvtFOR_ENUM_STMT {isEach=b1409, defn=opt1440, obj=x1444, rib=opt1452, 
          next=x1456, labels=ls1458, body=x1462} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1409), ("defn", 
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1410, ns=opt1412, static=b1416, prototype=b1417, bindings=(ls1419, 
            ls1424)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1410), ("ns", 
         (case opt1412 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1411 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1411))
         )), ("static", PrettyRep.Bool b1416), ("prototype", PrettyRep.Bool b1417), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1418 => 
                                                                          cvtBINDING x1418
                                                                   ) ls1419), 
            PrettyRep.List (List.map (fn x1423 => cvtINIT_STEP x1423
                                     ) ls1424)])]))
       )), ("obj", cvtEXPR x1444), ("rib", 
       (case opt1452 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1448 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1445, 
                                                                                      x1446) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1445, 
                                                                                      cvtFIXTURE x1446]
                                                                               ) ls1448)))
       )), ("next", cvtSTMT x1456), ("labels", PrettyRep.List (List.map (fn x1457 => 
                                                                               cvtIDENT x1457
                                                                        ) ls1458)), 
          ("body", cvtSTMT x1462)]
   and cvtFOR_STMT {rib=opt1485, defn=opt1519, init=ls1524, cond=x1528, update=x1529, 
          labels=ls1531, body=x1535} = PrettyRep.Rec [("rib", 
       (case opt1485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1481 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1478, 
                                                                                      x1479) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1478, 
                                                                                      cvtFIXTURE x1479]
                                                                               ) ls1481)))
       )), ("defn", 
       (case opt1519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1489, ns=opt1491, static=b1495, prototype=b1496, bindings=(ls1498, 
            ls1503)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1489), ("ns", 
         (case opt1491 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1490 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1490))
         )), ("static", PrettyRep.Bool b1495), ("prototype", PrettyRep.Bool b1496), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1497 => 
                                                                          cvtBINDING x1497
                                                                   ) ls1498), 
            PrettyRep.List (List.map (fn x1502 => cvtINIT_STEP x1502
                                     ) ls1503)])]))
       )), ("init", PrettyRep.List (List.map (fn x1523 => cvtSTMT x1523
                                             ) ls1524)), ("cond", cvtEXPR x1528), 
          ("update", cvtEXPR x1529), ("labels", PrettyRep.List (List.map (fn x1530 => 
                                                                                cvtIDENT x1530
                                                                         ) ls1531)), 
          ("body", cvtSTMT x1535)]
   and cvtWHILE_STMT {cond=x1551, rib=opt1559, body=x1563, labels=ls1565} = 
          PrettyRep.Rec [("cond", cvtEXPR x1551), ("rib", 
       (case opt1559 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1555 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1552, 
                                                                                      x1553) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1552, 
                                                                                      cvtFIXTURE x1553]
                                                                               ) ls1555)))
       )), ("body", cvtSTMT x1563), ("labels", PrettyRep.List (List.map (fn x1564 => 
                                                                               cvtIDENT x1564
                                                                        ) ls1565))]
   and cvtDIRECTIVES {pragmas=ls1579, defns=ls1584, head=opt1589, body=ls1594, 
          loc=opt1599} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1578 => 
                                                                                    cvtPRAGMA x1578
                                                                             ) ls1579)), 
          ("defns", PrettyRep.List (List.map (fn x1583 => cvtDEFN x1583
                                             ) ls1584)), ("head", 
       (case opt1589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1588 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1588))
       )), ("body", PrettyRep.List (List.map (fn x1593 => cvtSTMT x1593
                                             ) ls1594)), ("loc", 
       (case opt1599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1598 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1598))
       ))]
   and cvtCASE {label=opt1615, inits=opt1626, body=x1630} = PrettyRep.Rec [("label", 
          
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1614 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1614))
       )), ("inits", 
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1622 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1619, 
                                                                                      x1620) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1619, 
                                                                                      cvtEXPR x1620]
                                                                               ) ls1622)))
       )), ("body", cvtBLOCK x1630)]
   and cvtCATCH_CLAUSE {bindings=(ls1639, ls1644), ty=x1649, rib=opt1657, inits=opt1668, 
          block=x1672} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1638 => 
                                                                                                      cvtBINDING x1638
                                                                                               ) ls1639), 
          PrettyRep.List (List.map (fn x1643 => cvtINIT_STEP x1643
                                   ) ls1644)]), ("ty", cvtTY x1649), ("rib", 
          
       (case opt1657 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1653 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1650, 
                                                                                      x1651) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1650, 
                                                                                      cvtFIXTURE x1651]
                                                                               ) ls1653)))
       )), ("inits", 
       (case opt1668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1664 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1661, 
                                                                                      x1662) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1661, 
                                                                                      cvtEXPR x1662]
                                                                               ) ls1664)))
       )), ("block", cvtBLOCK x1672)]
   and cvtFUNC_NAME {kind=x1684, ident=x1685} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1684), 
          ("ident", cvtIDENT x1685)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1691, getter=opt1693, setter=opt1698} = 
          PrettyRep.Rec [("ty", cvtTY x1691), ("getter", 
       (case opt1693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1692 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1692))
       )), ("setter", 
       (case opt1698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1697 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1697))
       ))]
   and cvtFRAGMENT (Unit{name=opt1710, fragments=ls1715}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1710 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1709 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1709))
       )), ("fragments", PrettyRep.List (List.map (fn x1714 => cvtFRAGMENT x1714
                                                  ) ls1715))]))
     | cvtFRAGMENT (Package{name=ls1727, fragments=ls1732}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1726 => 
                                                                        cvtIDENT x1726
                                                                 ) ls1727)), 
          ("fragments", PrettyRep.List (List.map (fn x1731 => cvtFRAGMENT x1731
                                                 ) ls1732))]))
     | cvtFRAGMENT (Anon x1743) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1743))
end

