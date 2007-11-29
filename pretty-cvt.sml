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
     | cvtSTMT (ClassBlock{ns=opt538, ident=x542, name=opt544, block=x548}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x537 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x537))
       )), ("ident", cvtIDENT x542), ("name", 
       (case opt544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x543 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x543))
       )), ("block", cvtBLOCK x548)]))
     | cvtSTMT (ForInStmt x560) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x560))
     | cvtSTMT (ThrowStmt x563) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x563))
     | cvtSTMT (ReturnStmt x566) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x566))
     | cvtSTMT (BreakStmt opt570) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x569 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x569))
       ))
     | cvtSTMT (ContinueStmt opt577) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x576 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x576))
       ))
     | cvtSTMT (BlockStmt x583) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x583))
     | cvtSTMT (LabeledStmt(x586, x587)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x586, 
          cvtSTMT x587]))
     | cvtSTMT (LetStmt x591) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x591))
     | cvtSTMT (WhileStmt x594) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x594))
     | cvtSTMT (DoWhileStmt x597) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x597))
     | cvtSTMT (ForStmt x600) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x600))
     | cvtSTMT (IfStmt{cnd=x603, thn=x604, els=x605}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x603), ("thn", cvtSTMT x604), 
          ("els", cvtSTMT x605)]))
     | cvtSTMT (WithStmt{obj=x615, ty=x616, body=x617}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x615), ("ty", cvtTY x616), ("body", 
          cvtSTMT x617)]))
     | cvtSTMT (TryStmt{block=x627, catches=ls629, finally=opt634}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x627), ("catches", PrettyRep.List (List.map (fn x628 => 
                                                                                                     cvtCATCH_CLAUSE x628
                                                                                              ) ls629)), 
          ("finally", 
       (case opt634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x633 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x633))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x647, labels=ls649, cases=ls654}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x647), ("labels", PrettyRep.List (List.map (fn x648 => 
                                                                                                  cvtIDENT x648
                                                                                           ) ls649)), 
          ("cases", PrettyRep.List (List.map (fn x653 => cvtCASE x653
                                             ) ls654))]))
     | cvtSTMT (SwitchTypeStmt{cond=x667, ty=x668, cases=ls670}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x667), ("ty", cvtTY x668), 
          ("cases", PrettyRep.List (List.map (fn x669 => cvtCATCH_CLAUSE x669
                                             ) ls670))]))
     | cvtSTMT (DXNStmt{expr=x683}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x683)]))
   and cvtEXPR (TernaryExpr(x689, x690, x691)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x689, cvtEXPR x690, cvtEXPR x691]))
     | cvtEXPR (BinaryExpr(x695, x696, x697)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x695, cvtEXPR x696, cvtEXPR x697]))
     | cvtEXPR (BinaryTypeExpr(x701, x702, x703)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x701, cvtEXPR x702, cvtTY x703]))
     | cvtEXPR (ExpectedTypeExpr(x707, x708)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x707, cvtEXPR x708]))
     | cvtEXPR (UnaryExpr(x712, x713)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x712, 
          cvtEXPR x713]))
     | cvtEXPR (TypeExpr x717) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x717))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt722) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt722 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x721 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x721))
       ))
     | cvtEXPR (SuperExpr opt729) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt729 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x728 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x728))
       ))
     | cvtEXPR (LiteralExpr x735) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x735))
     | cvtEXPR (CallExpr{func=x738, actuals=ls740}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x738), ("actuals", PrettyRep.List (List.map (fn x739 => 
                                                                                                   cvtEXPR x739
                                                                                            ) ls740))]))
     | cvtEXPR (ApplyTypeExpr{expr=x751, actuals=ls753}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x751), ("actuals", PrettyRep.List (List.map (fn x752 => 
                                                                                                   cvtTY x752
                                                                                            ) ls753))]))
     | cvtEXPR (LetExpr{defs=x764, body=x765, head=opt767}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x764), ("body", cvtEXPR x765), 
          ("head", 
       (case opt767 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x766 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x766))
       ))]))
     | cvtEXPR (NewExpr{obj=x780, actuals=ls782}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x780), ("actuals", PrettyRep.List (List.map (fn x781 => 
                                                                                                  cvtEXPR x781
                                                                                           ) ls782))]))
     | cvtEXPR (ObjectRef{base=x793, ident=x794, loc=opt796}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x793), ("ident", cvtIDENT_EXPR x794), 
          ("loc", 
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x795))
       ))]))
     | cvtEXPR (LexicalRef{ident=x809, loc=opt811}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x809), ("loc", 
       (case opt811 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x810 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x810))
       ))]))
     | cvtEXPR (SetExpr(x822, x823, x824)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x822, 
          cvtEXPR x823, cvtEXPR x824]))
     | cvtEXPR (ListExpr ls829) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x828 => 
                                                                                                    cvtEXPR x828
                                                                                             ) ls829)))
     | cvtEXPR (InitExpr(x835, x836, x837)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x835, 
          cvtHEAD x836, cvtINITS x837]))
     | cvtEXPR (SliceExpr(x841, x842, x843)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x841, cvtEXPR x842, cvtEXPR x843]))
     | cvtEXPR (GetTemp n847) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n847))
     | cvtEXPR (GetParam n850) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n850))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n856) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n856))
     | cvtFIXTURE_NAME (PropName x859) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x859))
   and cvtIDENT_EXPR (Identifier{ident=x862, openNamespaces=ls868}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x862), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls864 => PrettyRep.List (List.map (fn x863 => 
                                                                                cvtNAMESPACE x863
                                                                         ) ls864)
                                   ) ls868))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x879, expr=x880}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x879), ("expr", cvtEXPR x880)]))
     | cvtIDENT_EXPR (AttributeIdentifier x888) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x888))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x891, openNamespaces=ls897}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x891), ("openNamespaces", PrettyRep.List (List.map (fn ls893 => 
                                                                            PrettyRep.List (List.map (fn x892 => 
                                                                                                            cvtNAMESPACE x892
                                                                                                     ) ls893)
                                                                     ) ls897))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x908, ident=s909}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x908), ("ident", PrettyRep.UniStr s909)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls918, x922)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x917 => cvtIDENT x917
                                                          ) ls918), cvtIDENT_EXPR x922]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r929) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r929))
     | cvtLITERAL (LiteralDecimal d932) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d932))
     | cvtLITERAL (LiteralInt i935) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i935))
     | cvtLITERAL (LiteralUInt u938) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u938))
     | cvtLITERAL (LiteralBoolean b941) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b941))
     | cvtLITERAL (LiteralString s944) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s944))
     | cvtLITERAL (LiteralArray{exprs=ls948, ty=opt953}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x947 => 
                                                                         cvtEXPR x947
                                                                  ) ls948)), 
          ("ty", 
       (case opt953 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x952 => PrettyRep.Ctor ("SOME", SOME (cvtTY x952))
       ))]))
     | cvtLITERAL (LiteralXML ls965) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x964 => 
                                                                                                           cvtEXPR x964
                                                                                                    ) ls965)))
     | cvtLITERAL (LiteralNamespace x971) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x971))
     | cvtLITERAL (LiteralObject{expr=ls975, ty=opt980}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x974 => 
                                                                        cvtFIELD x974
                                                                 ) ls975)), 
          ("ty", 
       (case opt980 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x979 => PrettyRep.Ctor ("SOME", SOME (cvtTY x979))
       ))]))
     | cvtLITERAL (LiteralFunction x991) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x991))
     | cvtLITERAL (LiteralRegExp{str=s994}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s994)]))
   and cvtBLOCK (Block x1000) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1000))
   and cvtFIXTURE (NamespaceFixture x1003) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1003))
     | cvtFIXTURE (ClassFixture x1006) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1006))
     | cvtFIXTURE (InterfaceFixture x1009) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1009))
     | cvtFIXTURE (TypeVarFixture x1012) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x1012))
     | cvtFIXTURE (TypeFixture x1015) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1015))
     | cvtFIXTURE (MethodFixture{func=x1018, ty=x1019, readOnly=b1020, override=b1021, 
          final=b1022}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1018), ("ty", cvtTY x1019), ("readOnly", PrettyRep.Bool b1020), 
          ("override", PrettyRep.Bool b1021), ("final", PrettyRep.Bool b1022)]))
     | cvtFIXTURE (ValFixture{ty=x1036, readOnly=b1037}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1036), ("readOnly", PrettyRep.Bool b1037)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1045, getter=opt1047, setter=opt1052}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1045), ("getter", 
       (case opt1047 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1046 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1046))
       )), ("setter", 
       (case opt1052 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1051 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1051))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1065, baseTypeArgs=ls1067}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1065), ("baseTypeArgs", PrettyRep.List (List.map (fn x1066 => 
                                                                           cvtTY x1066
                                                                    ) ls1067))]))
   and cvtHEAD (Head(x1078, x1079)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1078, 
          cvtINITS x1079]))
   and cvtBINDINGS (ls1084, ls1089) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1083 => 
                                                                                       cvtBINDING x1083
                                                                                ) ls1084), 
          PrettyRep.List (List.map (fn x1088 => cvtINIT_STEP x1088
                                   ) ls1089)]
   and cvtRIB ls1097 = PrettyRep.List (List.map (fn (x1094, x1095) => PrettyRep.Tuple [cvtFIXTURE_NAME x1094, 
                                                       cvtFIXTURE x1095]
                                                ) ls1097)
   and cvtRIBS ls1108 = PrettyRep.List (List.map (fn ls1104 => PrettyRep.List (List.map (fn (x1101, 
                                                                                               x1102) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1101, 
                                                                                               cvtFIXTURE x1102]
                                                                                        ) ls1104)
                                                 ) ls1108)
   and cvtINITS ls1115 = PrettyRep.List (List.map (fn (x1112, x1113) => PrettyRep.Tuple [cvtFIXTURE_NAME x1112, 
                                                         cvtEXPR x1113]
                                                  ) ls1115)
   and cvtINSTANCE_TYPE {name=x1119, typeParams=ls1121, typeArgs=ls1126, nonnullable=b1130, 
          superTypes=ls1132, ty=x1136, dynamic=b1137} = PrettyRep.Rec [("name", 
          cvtNAME x1119), ("typeParams", PrettyRep.List (List.map (fn x1120 => 
                                                                         cvtIDENT x1120
                                                                  ) ls1121)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1125 => cvtTYPE_EXPR x1125
                                                ) ls1126)), ("nonnullable", 
          PrettyRep.Bool b1130), ("superTypes", PrettyRep.List (List.map (fn x1131 => 
                                                                                cvtTYPE_EXPR x1131
                                                                         ) ls1132)), 
          ("ty", cvtTYPE_EXPR x1136), ("dynamic", PrettyRep.Bool b1137)]
   and cvtFIELD {kind=x1153, name=x1154, init=x1155} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1153), ("name", cvtIDENT_EXPR x1154), ("init", cvtEXPR x1155)]
   and cvtFIELD_TYPE {name=x1163, ty=x1164} = PrettyRep.Rec [("name", cvtIDENT x1163), 
          ("ty", cvtTYPE_EXPR x1164)]
   and cvtFUNC_TYPE {params=ls1171, result=x1175, thisType=opt1177, hasRest=b1181, 
          minArgs=n1182} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1170 => 
                                                                                     cvtTYPE_EXPR x1170
                                                                              ) ls1171)), 
          ("result", cvtTYPE_EXPR x1175), ("thisType", 
       (case opt1177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1176 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1176))
       )), ("hasRest", PrettyRep.Bool b1181), ("minArgs", PrettyRep.Int n1182)]
   and cvtFUNC_DEFN {kind=x1194, ns=opt1196, final=b1200, override=b1201, prototype=b1202, 
          static=b1203, func=x1204} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1194), 
          ("ns", 
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1195))
       )), ("final", PrettyRep.Bool b1200), ("override", PrettyRep.Bool b1201), 
          ("prototype", PrettyRep.Bool b1202), ("static", PrettyRep.Bool b1203), 
          ("func", cvtFUNC x1204)]
   and cvtCTOR_DEFN x1220 = cvtCTOR x1220
   and cvtVAR_DEFN {kind=x1221, ns=opt1223, static=b1227, prototype=b1228, 
          bindings=(ls1230, ls1235)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1221), 
          ("ns", 
       (case opt1223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1222 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1222))
       )), ("static", PrettyRep.Bool b1227), ("prototype", PrettyRep.Bool b1228), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1229 => 
                                                                        cvtBINDING x1229
                                                                 ) ls1230), 
          PrettyRep.List (List.map (fn x1234 => cvtINIT_STEP x1234
                                   ) ls1235)])]
   and cvtNAMESPACE_DEFN {ident=x1251, ns=opt1253, init=opt1258} = PrettyRep.Rec [("ident", 
          cvtIDENT x1251), ("ns", 
       (case opt1253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1252 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1252))
       )), ("init", 
       (case opt1258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1257 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1257))
       ))]
   and cvtCLASS_DEFN {ns=opt1270, ident=x1274, nonnullable=b1275, dynamic=b1276, 
          final=b1277, params=ls1279, extends=opt1284, implements=ls1289, classDefns=ls1294, 
          instanceDefns=ls1299, instanceStmts=ls1304, ctorDefn=opt1309} = PrettyRep.Rec [("ns", 
          
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1269))
       )), ("ident", cvtIDENT x1274), ("nonnullable", PrettyRep.Bool b1275), 
          ("dynamic", PrettyRep.Bool b1276), ("final", PrettyRep.Bool b1277), 
          ("params", PrettyRep.List (List.map (fn x1278 => cvtIDENT x1278
                                              ) ls1279)), ("extends", 
       (case opt1284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1283 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1283))
       )), ("implements", PrettyRep.List (List.map (fn x1288 => cvtTYPE_EXPR x1288
                                                   ) ls1289)), ("classDefns", 
          PrettyRep.List (List.map (fn x1293 => cvtDEFN x1293
                                   ) ls1294)), ("instanceDefns", PrettyRep.List (List.map (fn x1298 => 
                                                                                                 cvtDEFN x1298
                                                                                          ) ls1299)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1303 => cvtSTMT x1303
                                                     ) ls1304)), ("ctorDefn", 
          
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1308))
       ))]
   and cvtINTERFACE_DEFN {ident=x1338, ns=opt1340, nonnullable=b1344, params=ls1346, 
          extends=ls1351, instanceDefns=ls1356} = PrettyRep.Rec [("ident", 
          cvtIDENT x1338), ("ns", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1339))
       )), ("nonnullable", PrettyRep.Bool b1344), ("params", PrettyRep.List (List.map (fn x1345 => 
                                                                                             cvtIDENT x1345
                                                                                      ) ls1346)), 
          ("extends", PrettyRep.List (List.map (fn x1350 => cvtTYPE_EXPR x1350
                                               ) ls1351)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1355 => cvtDEFN x1355
                                   ) ls1356))]
   and cvtTYPE_DEFN {ident=x1373, ns=opt1375, init=x1379} = PrettyRep.Rec [("ident", 
          cvtIDENT x1373), ("ns", 
       (case opt1375 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1374 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1374))
       )), ("init", cvtTYPE_EXPR x1379)]
   and cvtFOR_ENUM_STMT {isEach=b1387, defn=opt1418, obj=x1422, rib=opt1430, 
          next=x1434, labels=ls1436, body=x1440} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1387), ("defn", 
       (case opt1418 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1388, ns=opt1390, static=b1394, prototype=b1395, bindings=(ls1397, 
            ls1402)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1388), ("ns", 
         (case opt1390 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1389 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1389))
         )), ("static", PrettyRep.Bool b1394), ("prototype", PrettyRep.Bool b1395), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1396 => 
                                                                          cvtBINDING x1396
                                                                   ) ls1397), 
            PrettyRep.List (List.map (fn x1401 => cvtINIT_STEP x1401
                                     ) ls1402)])]))
       )), ("obj", cvtEXPR x1422), ("rib", 
       (case opt1430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1426 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1423, 
                                                                                      x1424) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1423, 
                                                                                      cvtFIXTURE x1424]
                                                                               ) ls1426)))
       )), ("next", cvtSTMT x1434), ("labels", PrettyRep.List (List.map (fn x1435 => 
                                                                               cvtIDENT x1435
                                                                        ) ls1436)), 
          ("body", cvtSTMT x1440)]
   and cvtFOR_STMT {rib=opt1463, defn=opt1497, init=ls1502, cond=x1506, update=x1507, 
          labels=ls1509, body=x1513} = PrettyRep.Rec [("rib", 
       (case opt1463 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1459 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1456, 
                                                                                      x1457) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1456, 
                                                                                      cvtFIXTURE x1457]
                                                                               ) ls1459)))
       )), ("defn", 
       (case opt1497 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1467, ns=opt1469, static=b1473, prototype=b1474, bindings=(ls1476, 
            ls1481)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1467), ("ns", 
         (case opt1469 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1468 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1468))
         )), ("static", PrettyRep.Bool b1473), ("prototype", PrettyRep.Bool b1474), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1475 => 
                                                                          cvtBINDING x1475
                                                                   ) ls1476), 
            PrettyRep.List (List.map (fn x1480 => cvtINIT_STEP x1480
                                     ) ls1481)])]))
       )), ("init", PrettyRep.List (List.map (fn x1501 => cvtSTMT x1501
                                             ) ls1502)), ("cond", cvtEXPR x1506), 
          ("update", cvtEXPR x1507), ("labels", PrettyRep.List (List.map (fn x1508 => 
                                                                                cvtIDENT x1508
                                                                         ) ls1509)), 
          ("body", cvtSTMT x1513)]
   and cvtWHILE_STMT {cond=x1529, rib=opt1537, body=x1541, labels=ls1543} = 
          PrettyRep.Rec [("cond", cvtEXPR x1529), ("rib", 
       (case opt1537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1533 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1530, 
                                                                                      x1531) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1530, 
                                                                                      cvtFIXTURE x1531]
                                                                               ) ls1533)))
       )), ("body", cvtSTMT x1541), ("labels", PrettyRep.List (List.map (fn x1542 => 
                                                                               cvtIDENT x1542
                                                                        ) ls1543))]
   and cvtDIRECTIVES {pragmas=ls1557, defns=ls1562, head=opt1567, body=ls1572, 
          loc=opt1577} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1556 => 
                                                                                    cvtPRAGMA x1556
                                                                             ) ls1557)), 
          ("defns", PrettyRep.List (List.map (fn x1561 => cvtDEFN x1561
                                             ) ls1562)), ("head", 
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1566 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1566))
       )), ("body", PrettyRep.List (List.map (fn x1571 => cvtSTMT x1571
                                             ) ls1572)), ("loc", 
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1576))
       ))]
   and cvtCASE {label=opt1593, inits=opt1604, body=x1608} = PrettyRep.Rec [("label", 
          
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1592 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1592))
       )), ("inits", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1600 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1597, 
                                                                                      x1598) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1597, 
                                                                                      cvtEXPR x1598]
                                                                               ) ls1600)))
       )), ("body", cvtBLOCK x1608)]
   and cvtCATCH_CLAUSE {bindings=(ls1617, ls1622), ty=x1627, rib=opt1635, inits=opt1646, 
          block=x1650} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1616 => 
                                                                                                      cvtBINDING x1616
                                                                                               ) ls1617), 
          PrettyRep.List (List.map (fn x1621 => cvtINIT_STEP x1621
                                   ) ls1622)]), ("ty", cvtTY x1627), ("rib", 
          
       (case opt1635 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1631 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1628, 
                                                                                      x1629) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1628, 
                                                                                      cvtFIXTURE x1629]
                                                                               ) ls1631)))
       )), ("inits", 
       (case opt1646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1642 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1639, 
                                                                                      x1640) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1639, 
                                                                                      cvtEXPR x1640]
                                                                               ) ls1642)))
       )), ("block", cvtBLOCK x1650)]
   and cvtFUNC_NAME {kind=x1662, ident=x1663} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1662), 
          ("ident", cvtIDENT x1663)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1669, getter=opt1671, setter=opt1676} = 
          PrettyRep.Rec [("ty", cvtTY x1669), ("getter", 
       (case opt1671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1670 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1670))
       )), ("setter", 
       (case opt1676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1675 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1675))
       ))]
   and cvtFRAGMENT (Unit{name=opt1688, fragments=ls1693}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1687 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1687))
       )), ("fragments", PrettyRep.List (List.map (fn x1692 => cvtFRAGMENT x1692
                                                  ) ls1693))]))
     | cvtFRAGMENT (Package{name=ls1705, fragments=ls1710}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1704 => 
                                                                        cvtIDENT x1704
                                                                 ) ls1705)), 
          ("fragments", PrettyRep.List (List.map (fn x1709 => cvtFRAGMENT x1709
                                                 ) ls1710))]))
     | cvtFRAGMENT (Anon x1721) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1721))
end

