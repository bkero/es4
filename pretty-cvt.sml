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
   and cvtPRAGMA (UseNamespace x135) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x135))
     | cvtPRAGMA (UseDefaultNamespace x138) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x138))
     | cvtPRAGMA (UseDecimalContext x141) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x141))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls147, name=x151, alias=opt153}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x146 => 
                                                                           cvtIDENT x146
                                                                    ) ls147)), 
          ("name", cvtIDENT x151), ("alias", 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x152))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x172, ribId=opt174}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x172), ("ribId", 
       (case opt174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x173 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x173))
       ))]))
   and cvtCLS (Cls{name=x185, typeParams=ls187, nonnullable=b191, dynamic=b192, 
          extends=opt194, implements=ls199, classRib=x203, instanceRib=x204, 
          instanceInits=x205, constructor=opt207, classType=x211, instanceType=x212}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x185), 
          ("typeParams", PrettyRep.List (List.map (fn x186 => cvtIDENT x186
                                                  ) ls187)), ("nonnullable", 
          PrettyRep.Bool b191), ("dynamic", PrettyRep.Bool b192), ("extends", 
          
       (case opt194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x193 => PrettyRep.Ctor ("SOME", SOME (cvtTY x193))
       )), ("implements", PrettyRep.List (List.map (fn x198 => cvtTY x198
                                                   ) ls199)), ("classRib", 
          cvtRIB x203), ("instanceRib", cvtRIB x204), ("instanceInits", cvtHEAD x205), 
          ("constructor", 
       (case opt207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x206 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x206))
       )), ("classType", cvtTY x211), ("instanceType", cvtTY x212)]))
   and cvtIFACE (Iface{name=x240, typeParams=ls242, nonnullable=b246, extends=ls248, 
          instanceRib=x252, instanceType=x253}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x240), ("typeParams", PrettyRep.List (List.map (fn x241 => 
                                                                                                      cvtIDENT x241
                                                                                               ) ls242)), 
          ("nonnullable", PrettyRep.Bool b246), ("extends", PrettyRep.List (List.map (fn x247 => 
                                                                                            cvtTY x247
                                                                                     ) ls248)), 
          ("instanceRib", cvtRIB x252), ("instanceType", cvtTY x253)]))
   and cvtCTOR (Ctor{settings=x269, superArgs=ls271, func=x275}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x269), ("superArgs", PrettyRep.List (List.map (fn x270 => 
                                                                                                         cvtEXPR x270
                                                                                                  ) ls271)), 
          ("func", cvtFUNC x275)]))
   and cvtFUNC (Func{name=x285, fsig=x286, native=b287, block=opt289, param=x293, 
          defaults=ls295, ty=x299, loc=opt301}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x285), ("fsig", cvtFUNC_SIG x286), ("native", PrettyRep.Bool b287), 
          ("block", 
       (case opt289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x288 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x288))
       )), ("param", cvtHEAD x293), ("defaults", PrettyRep.List (List.map (fn x294 => 
                                                                                 cvtEXPR x294
                                                                          ) ls295)), 
          ("ty", cvtTY x299), ("loc", 
       (case opt301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x300 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x300))
       ))]))
   and cvtDEFN (ClassDefn x324) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x324))
     | cvtDEFN (VariableDefn x327) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x327))
     | cvtDEFN (FunctionDefn x330) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x330))
     | cvtDEFN (ConstructorDefn x333) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x333))
     | cvtDEFN (InterfaceDefn x336) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x336))
     | cvtDEFN (NamespaceDefn x339) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x339))
     | cvtDEFN (TypeDefn x342) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x342))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls346, params=x350, paramTypes=ls352, 
          defaults=ls357, ctorInits=opt368, returnType=x372, thisType=opt374, 
          hasRest=b378}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x345 => cvtIDENT x345
                                   ) ls346)), ("params", cvtBINDINGS x350), 
          ("paramTypes", PrettyRep.List (List.map (fn x351 => cvtTYPE_EXPR x351
                                                  ) ls352)), ("defaults", PrettyRep.List (List.map (fn x356 => 
                                                                                                          cvtEXPR x356
                                                                                                   ) ls357)), 
          ("ctorInits", 
       (case opt368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x361, ls363) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x361, 
            PrettyRep.List (List.map (fn x362 => cvtEXPR x362
                                     ) ls363)]))
       )), ("returnType", cvtTYPE_EXPR x372), ("thisType", 
       (case opt374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x373 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x373))
       )), ("hasRest", PrettyRep.Bool b378)]))
   and cvtBINDING (Binding{ident=x398, ty=x399}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x398), ("ty", cvtTYPE_EXPR x399)]))
   and cvtBINDING_IDENT (TempIdent n407) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n407))
     | cvtBINDING_IDENT (ParamIdent n410) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n410))
     | cvtBINDING_IDENT (PropIdent x413) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x413))
   and cvtINIT_STEP (InitStep(x416, x417)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x416, 
          cvtEXPR x417]))
     | cvtINIT_STEP (AssignStep(x421, x422)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x421, cvtEXPR x422]))
   and cvtTYPE_EXPR (SpecialType x426) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x426))
     | cvtTYPE_EXPR (UnionType ls430) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x429 => 
                                                                                                           cvtTYPE_EXPR x429
                                                                                                    ) ls430)))
     | cvtTYPE_EXPR (ArrayType ls437) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x436 => 
                                                                                                           cvtTYPE_EXPR x436
                                                                                                    ) ls437)))
     | cvtTYPE_EXPR (TypeName x443) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x443))
     | cvtTYPE_EXPR (ElementTypeRef(x446, n447)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x446, PrettyRep.Int n447]))
     | cvtTYPE_EXPR (FieldTypeRef(x451, x452)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x451, cvtIDENT x452]))
     | cvtTYPE_EXPR (FunctionType x456) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x456))
     | cvtTYPE_EXPR (ObjectType ls460) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x459 => 
                                                                                                             cvtFIELD_TYPE x459
                                                                                                      ) ls460)))
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
     | cvtEXPR (SliceExpr(x821, x822, x823)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x821, cvtEXPR x822, cvtEXPR x823]))
     | cvtEXPR (GetTemp n827) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n827))
     | cvtEXPR (GetParam n830) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n830))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n836) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n836))
     | cvtFIXTURE_NAME (PropName x839) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x839))
   and cvtIDENT_EXPR (Identifier{ident=x842, openNamespaces=ls848}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x842), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls844 => PrettyRep.List (List.map (fn x843 => 
                                                                                cvtNAMESPACE x843
                                                                         ) ls844)
                                   ) ls848))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x859, expr=x860}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x859), ("expr", cvtEXPR x860)]))
     | cvtIDENT_EXPR (AttributeIdentifier x868) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x868))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x871, openNamespaces=ls877}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x871), ("openNamespaces", PrettyRep.List (List.map (fn ls873 => 
                                                                            PrettyRep.List (List.map (fn x872 => 
                                                                                                            cvtNAMESPACE x872
                                                                                                     ) ls873)
                                                                     ) ls877))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x888, ident=s889}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x888), ("ident", PrettyRep.UniStr s889)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls898, x902)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x897 => cvtIDENT x897
                                                          ) ls898), cvtIDENT_EXPR x902]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r909) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r909))
     | cvtLITERAL (LiteralDecimal d912) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d912))
     | cvtLITERAL (LiteralInt i915) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i915))
     | cvtLITERAL (LiteralUInt u918) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u918))
     | cvtLITERAL (LiteralBoolean b921) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b921))
     | cvtLITERAL (LiteralString s924) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s924))
     | cvtLITERAL (LiteralArray{exprs=ls928, ty=opt933}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x927 => 
                                                                         cvtEXPR x927
                                                                  ) ls928)), 
          ("ty", 
       (case opt933 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x932 => PrettyRep.Ctor ("SOME", SOME (cvtTY x932))
       ))]))
     | cvtLITERAL (LiteralXML ls945) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x944 => 
                                                                                                           cvtEXPR x944
                                                                                                    ) ls945)))
     | cvtLITERAL (LiteralNamespace x951) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x951))
     | cvtLITERAL (LiteralObject{expr=ls955, ty=opt960}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x954 => 
                                                                        cvtFIELD x954
                                                                 ) ls955)), 
          ("ty", 
       (case opt960 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x959 => PrettyRep.Ctor ("SOME", SOME (cvtTY x959))
       ))]))
     | cvtLITERAL (LiteralFunction x971) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x971))
     | cvtLITERAL (LiteralRegExp{str=s974}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s974)]))
   and cvtBLOCK (Block x980) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x980))
   and cvtFIXTURE (NamespaceFixture x983) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x983))
     | cvtFIXTURE (ClassFixture x986) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x986))
     | cvtFIXTURE (InterfaceFixture x989) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x989))
     | cvtFIXTURE (TypeVarFixture x992) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x992))
     | cvtFIXTURE (TypeFixture x995) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x995))
     | cvtFIXTURE (MethodFixture{func=x998, ty=x999, readOnly=b1000, override=b1001, 
          final=b1002}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x998), ("ty", cvtTY x999), ("readOnly", PrettyRep.Bool b1000), 
          ("override", PrettyRep.Bool b1001), ("final", PrettyRep.Bool b1002)]))
     | cvtFIXTURE (ValFixture{ty=x1016, readOnly=b1017}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1016), ("readOnly", PrettyRep.Bool b1017)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1025, getter=opt1027, setter=opt1032}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1025), ("getter", 
       (case opt1027 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1026 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1026))
       )), ("setter", 
       (case opt1032 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1031 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1031))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1045, baseTypeArgs=ls1047}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1045), ("baseTypeArgs", PrettyRep.List (List.map (fn x1046 => 
                                                                           cvtTY x1046
                                                                    ) ls1047))]))
   and cvtHEAD (Head(x1058, x1059)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1058, 
          cvtINITS x1059]))
   and cvtBINDINGS (ls1064, ls1069) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1063 => 
                                                                                       cvtBINDING x1063
                                                                                ) ls1064), 
          PrettyRep.List (List.map (fn x1068 => cvtINIT_STEP x1068
                                   ) ls1069)]
   and cvtRIB ls1077 = PrettyRep.List (List.map (fn (x1074, x1075) => PrettyRep.Tuple [cvtFIXTURE_NAME x1074, 
                                                       cvtFIXTURE x1075]
                                                ) ls1077)
   and cvtRIBS ls1088 = PrettyRep.List (List.map (fn ls1084 => PrettyRep.List (List.map (fn (x1081, 
                                                                                               x1082) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1081, 
                                                                                               cvtFIXTURE x1082]
                                                                                        ) ls1084)
                                                 ) ls1088)
   and cvtINITS ls1095 = PrettyRep.List (List.map (fn (x1092, x1093) => PrettyRep.Tuple [cvtFIXTURE_NAME x1092, 
                                                         cvtEXPR x1093]
                                                  ) ls1095)
   and cvtINSTANCE_TYPE {name=x1099, typeParams=ls1101, typeArgs=ls1106, nonnullable=b1110, 
          superTypes=ls1112, ty=x1116, dynamic=b1117} = PrettyRep.Rec [("name", 
          cvtNAME x1099), ("typeParams", PrettyRep.List (List.map (fn x1100 => 
                                                                         cvtIDENT x1100
                                                                  ) ls1101)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1105 => cvtTYPE_EXPR x1105
                                                ) ls1106)), ("nonnullable", 
          PrettyRep.Bool b1110), ("superTypes", PrettyRep.List (List.map (fn x1111 => 
                                                                                cvtTYPE_EXPR x1111
                                                                         ) ls1112)), 
          ("ty", cvtTYPE_EXPR x1116), ("dynamic", PrettyRep.Bool b1117)]
   and cvtFIELD {kind=x1133, name=x1134, init=x1135} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1133), ("name", cvtIDENT_EXPR x1134), ("init", cvtEXPR x1135)]
   and cvtFIELD_TYPE {name=x1143, ty=x1144} = PrettyRep.Rec [("name", cvtIDENT x1143), 
          ("ty", cvtTYPE_EXPR x1144)]
   and cvtFUNC_TYPE {params=ls1151, result=x1155, thisType=opt1157, hasRest=b1161, 
          minArgs=n1162} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1150 => 
                                                                                     cvtTYPE_EXPR x1150
                                                                              ) ls1151)), 
          ("result", cvtTYPE_EXPR x1155), ("thisType", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1156))
       )), ("hasRest", PrettyRep.Bool b1161), ("minArgs", PrettyRep.Int n1162)]
   and cvtFUNC_DEFN {kind=x1174, ns=opt1176, final=b1180, override=b1181, prototype=b1182, 
          static=b1183, func=x1184} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1174), 
          ("ns", 
       (case opt1176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1175 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1175))
       )), ("final", PrettyRep.Bool b1180), ("override", PrettyRep.Bool b1181), 
          ("prototype", PrettyRep.Bool b1182), ("static", PrettyRep.Bool b1183), 
          ("func", cvtFUNC x1184)]
   and cvtCTOR_DEFN x1200 = cvtCTOR x1200
   and cvtVAR_DEFN {kind=x1201, ns=opt1203, static=b1207, prototype=b1208, 
          bindings=(ls1210, ls1215)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1201), 
          ("ns", 
       (case opt1203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1202 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1202))
       )), ("static", PrettyRep.Bool b1207), ("prototype", PrettyRep.Bool b1208), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1209 => 
                                                                        cvtBINDING x1209
                                                                 ) ls1210), 
          PrettyRep.List (List.map (fn x1214 => cvtINIT_STEP x1214
                                   ) ls1215)])]
   and cvtNAMESPACE_DEFN {ident=x1231, ns=opt1233, init=opt1238} = PrettyRep.Rec [("ident", 
          cvtIDENT x1231), ("ns", 
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1232))
       )), ("init", 
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1237))
       ))]
   and cvtCLASS_DEFN {ns=opt1250, ident=x1254, nonnullable=b1255, dynamic=b1256, 
          final=b1257, params=ls1259, extends=opt1264, implements=ls1269, classDefns=ls1274, 
          instanceDefns=ls1279, instanceStmts=ls1284, ctorDefn=opt1289} = PrettyRep.Rec [("ns", 
          
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1249))
       )), ("ident", cvtIDENT x1254), ("nonnullable", PrettyRep.Bool b1255), 
          ("dynamic", PrettyRep.Bool b1256), ("final", PrettyRep.Bool b1257), 
          ("params", PrettyRep.List (List.map (fn x1258 => cvtIDENT x1258
                                              ) ls1259)), ("extends", 
       (case opt1264 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1263 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1263))
       )), ("implements", PrettyRep.List (List.map (fn x1268 => cvtTYPE_EXPR x1268
                                                   ) ls1269)), ("classDefns", 
          PrettyRep.List (List.map (fn x1273 => cvtDEFN x1273
                                   ) ls1274)), ("instanceDefns", PrettyRep.List (List.map (fn x1278 => 
                                                                                                 cvtDEFN x1278
                                                                                          ) ls1279)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1283 => cvtSTMT x1283
                                                     ) ls1284)), ("ctorDefn", 
          
       (case opt1289 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1288 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1288))
       ))]
   and cvtINTERFACE_DEFN {ident=x1318, ns=opt1320, nonnullable=b1324, params=ls1326, 
          extends=ls1331, instanceDefns=ls1336} = PrettyRep.Rec [("ident", 
          cvtIDENT x1318), ("ns", 
       (case opt1320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1319 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1319))
       )), ("nonnullable", PrettyRep.Bool b1324), ("params", PrettyRep.List (List.map (fn x1325 => 
                                                                                             cvtIDENT x1325
                                                                                      ) ls1326)), 
          ("extends", PrettyRep.List (List.map (fn x1330 => cvtTYPE_EXPR x1330
                                               ) ls1331)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1335 => cvtDEFN x1335
                                   ) ls1336))]
   and cvtTYPE_DEFN {ident=x1353, ns=opt1355, init=x1359} = PrettyRep.Rec [("ident", 
          cvtIDENT x1353), ("ns", 
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1354))
       )), ("init", cvtTYPE_EXPR x1359)]
   and cvtCLASS_BLOCK {ns=opt1368, ident=x1372, name=opt1374, block=x1378} = 
          PrettyRep.Rec [("ns", 
       (case opt1368 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1367 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1367))
       )), ("ident", cvtIDENT x1372), ("name", 
       (case opt1374 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1373 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1373))
       )), ("block", cvtBLOCK x1378)]
   and cvtFOR_ENUM_STMT {isEach=b1388, defn=opt1419, obj=x1423, rib=opt1431, 
          next=x1435, labels=ls1437, body=x1441} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1388), ("defn", 
       (case opt1419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1389, ns=opt1391, static=b1395, prototype=b1396, bindings=(ls1398, 
            ls1403)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1389), ("ns", 
         (case opt1391 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1390))
         )), ("static", PrettyRep.Bool b1395), ("prototype", PrettyRep.Bool b1396), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1397 => 
                                                                          cvtBINDING x1397
                                                                   ) ls1398), 
            PrettyRep.List (List.map (fn x1402 => cvtINIT_STEP x1402
                                     ) ls1403)])]))
       )), ("obj", cvtEXPR x1423), ("rib", 
       (case opt1431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1427 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1424, 
                                                                                      x1425) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1424, 
                                                                                      cvtFIXTURE x1425]
                                                                               ) ls1427)))
       )), ("next", cvtSTMT x1435), ("labels", PrettyRep.List (List.map (fn x1436 => 
                                                                               cvtIDENT x1436
                                                                        ) ls1437)), 
          ("body", cvtSTMT x1441)]
   and cvtFOR_STMT {rib=opt1464, defn=opt1498, init=ls1503, cond=x1507, update=x1508, 
          labels=ls1510, body=x1514} = PrettyRep.Rec [("rib", 
       (case opt1464 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1460 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1457, 
                                                                                      x1458) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1457, 
                                                                                      cvtFIXTURE x1458]
                                                                               ) ls1460)))
       )), ("defn", 
       (case opt1498 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1468, ns=opt1470, static=b1474, prototype=b1475, bindings=(ls1477, 
            ls1482)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1468), ("ns", 
         (case opt1470 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1469))
         )), ("static", PrettyRep.Bool b1474), ("prototype", PrettyRep.Bool b1475), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1476 => 
                                                                          cvtBINDING x1476
                                                                   ) ls1477), 
            PrettyRep.List (List.map (fn x1481 => cvtINIT_STEP x1481
                                     ) ls1482)])]))
       )), ("init", PrettyRep.List (List.map (fn x1502 => cvtSTMT x1502
                                             ) ls1503)), ("cond", cvtEXPR x1507), 
          ("update", cvtEXPR x1508), ("labels", PrettyRep.List (List.map (fn x1509 => 
                                                                                cvtIDENT x1509
                                                                         ) ls1510)), 
          ("body", cvtSTMT x1514)]
   and cvtWHILE_STMT {cond=x1530, rib=opt1538, body=x1542, labels=ls1544} = 
          PrettyRep.Rec [("cond", cvtEXPR x1530), ("rib", 
       (case opt1538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1534 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1531, 
                                                                                      x1532) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1531, 
                                                                                      cvtFIXTURE x1532]
                                                                               ) ls1534)))
       )), ("body", cvtSTMT x1542), ("labels", PrettyRep.List (List.map (fn x1543 => 
                                                                               cvtIDENT x1543
                                                                        ) ls1544))]
   and cvtDIRECTIVES {pragmas=ls1558, defns=ls1563, head=opt1568, body=ls1573, 
          loc=opt1578} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1557 => 
                                                                                    cvtPRAGMA x1557
                                                                             ) ls1558)), 
          ("defns", PrettyRep.List (List.map (fn x1562 => cvtDEFN x1562
                                             ) ls1563)), ("head", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1567))
       )), ("body", PrettyRep.List (List.map (fn x1572 => cvtSTMT x1572
                                             ) ls1573)), ("loc", 
       (case opt1578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1577 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1577))
       ))]
   and cvtCASE {label=opt1594, inits=opt1605, body=x1609} = PrettyRep.Rec [("label", 
          
       (case opt1594 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1593 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1593))
       )), ("inits", 
       (case opt1605 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1601 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1598, 
                                                                                      x1599) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1598, 
                                                                                      cvtEXPR x1599]
                                                                               ) ls1601)))
       )), ("body", cvtBLOCK x1609)]
   and cvtCATCH_CLAUSE {bindings=(ls1618, ls1623), ty=x1628, rib=opt1636, inits=opt1647, 
          block=x1651} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1617 => 
                                                                                                      cvtBINDING x1617
                                                                                               ) ls1618), 
          PrettyRep.List (List.map (fn x1622 => cvtINIT_STEP x1622
                                   ) ls1623)]), ("ty", cvtTY x1628), ("rib", 
          
       (case opt1636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1632 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1629, 
                                                                                      x1630) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1629, 
                                                                                      cvtFIXTURE x1630]
                                                                               ) ls1632)))
       )), ("inits", 
       (case opt1647 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1643 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1640, 
                                                                                      x1641) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1640, 
                                                                                      cvtEXPR x1641]
                                                                               ) ls1643)))
       )), ("block", cvtBLOCK x1651)]
   and cvtFUNC_NAME {kind=x1663, ident=x1664} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1663), 
          ("ident", cvtIDENT x1664)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1670, getter=opt1672, setter=opt1677} = 
          PrettyRep.Rec [("ty", cvtTY x1670), ("getter", 
       (case opt1672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1671 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1671))
       )), ("setter", 
       (case opt1677 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1676 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1676))
       ))]
   and cvtFRAGMENT (Unit{name=opt1689, fragments=ls1694}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1689 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1688 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1688))
       )), ("fragments", PrettyRep.List (List.map (fn x1693 => cvtFRAGMENT x1693
                                                  ) ls1694))]))
     | cvtFRAGMENT (Package{name=ls1706, fragments=ls1711}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1705 => 
                                                                        cvtIDENT x1705
                                                                 ) ls1706)), 
          ("fragments", PrettyRep.List (List.map (fn x1710 => cvtFRAGMENT x1710
                                                 ) ls1711))]))
     | cvtFRAGMENT (Anon x1722) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1722))
end

