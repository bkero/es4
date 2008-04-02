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
   and cvtPRAGMA (UseNamespace x131) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x131))
     | cvtPRAGMA (UseDefaultNamespace x134) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x134))
     | cvtPRAGMA (UseDecimalContext x137) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x137))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls143, name=x147}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x142 => 
                                                                           cvtIDENT x142
                                                                    ) ls143)), 
          ("name", cvtIDENT x147)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x161, ribId=opt163}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x161), ("ribId", 
       (case opt163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x162 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x162))
       ))]))
   and cvtCLS (Cls{name=x174, typeParams=ls176, nonnullable=b180, dynamic=b181, 
          extends=opt183, implements=ls188, classRib=x192, instanceRib=x193, 
          instanceInits=x194, constructor=opt196, classType=x200, instanceType=x201}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x174), 
          ("typeParams", PrettyRep.List (List.map (fn x175 => cvtIDENT x175
                                                  ) ls176)), ("nonnullable", 
          PrettyRep.Bool b180), ("dynamic", PrettyRep.Bool b181), ("extends", 
          
       (case opt183 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtTY x182))
       )), ("implements", PrettyRep.List (List.map (fn x187 => cvtTY x187
                                                   ) ls188)), ("classRib", 
          cvtRIB x192), ("instanceRib", cvtRIB x193), ("instanceInits", cvtHEAD x194), 
          ("constructor", 
       (case opt196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x195 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x195))
       )), ("classType", cvtTY x200), ("instanceType", cvtTY x201)]))
   and cvtIFACE (Iface{name=x229, typeParams=ls231, nonnullable=b235, extends=ls237, 
          instanceRib=x241, instanceType=x242}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x229), ("typeParams", PrettyRep.List (List.map (fn x230 => 
                                                                                                      cvtIDENT x230
                                                                                               ) ls231)), 
          ("nonnullable", PrettyRep.Bool b235), ("extends", PrettyRep.List (List.map (fn x236 => 
                                                                                            cvtTY x236
                                                                                     ) ls237)), 
          ("instanceRib", cvtRIB x241), ("instanceType", cvtTY x242)]))
   and cvtCTOR (Ctor{settings=x258, superArgs=ls260, func=x264}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x258), ("superArgs", PrettyRep.List (List.map (fn x259 => 
                                                                                                         cvtEXPR x259
                                                                                                  ) ls260)), 
          ("func", cvtFUNC x264)]))
   and cvtFUNC (Func{name=x274, fsig=x275, native=b276, block=opt278, param=x282, 
          defaults=ls284, ty=x288, loc=opt290}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x274), ("fsig", cvtFUNC_SIG x275), ("native", PrettyRep.Bool b276), 
          ("block", 
       (case opt278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x277 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x277))
       )), ("param", cvtHEAD x282), ("defaults", PrettyRep.List (List.map (fn x283 => 
                                                                                 cvtEXPR x283
                                                                          ) ls284)), 
          ("ty", cvtTY x288), ("loc", 
       (case opt290 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x289 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x289))
       ))]))
   and cvtDEFN (ClassDefn x313) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x313))
     | cvtDEFN (VariableDefn x316) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x316))
     | cvtDEFN (FunctionDefn x319) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x319))
     | cvtDEFN (ConstructorDefn x322) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x322))
     | cvtDEFN (InterfaceDefn x325) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x325))
     | cvtDEFN (NamespaceDefn x328) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x328))
     | cvtDEFN (TypeDefn x331) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x331))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls335, params=x339, paramTypes=ls341, 
          defaults=ls346, ctorInits=opt357, returnType=x361, thisType=opt363, 
          hasRest=b367}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x334 => cvtIDENT x334
                                   ) ls335)), ("params", cvtBINDINGS x339), 
          ("paramTypes", PrettyRep.List (List.map (fn x340 => cvtTYPE_EXPR x340
                                                  ) ls341)), ("defaults", PrettyRep.List (List.map (fn x345 => 
                                                                                                          cvtEXPR x345
                                                                                                   ) ls346)), 
          ("ctorInits", 
       (case opt357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x350, ls352) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x350, 
            PrettyRep.List (List.map (fn x351 => cvtEXPR x351
                                     ) ls352)]))
       )), ("returnType", cvtTYPE_EXPR x361), ("thisType", 
       (case opt363 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x362 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x362))
       )), ("hasRest", PrettyRep.Bool b367)]))
   and cvtBINDING (Binding{ident=x387, ty=x388}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x387), ("ty", cvtTYPE_EXPR x388)]))
   and cvtBINDING_IDENT (TempIdent n396) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n396))
     | cvtBINDING_IDENT (ParamIdent n399) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n399))
     | cvtBINDING_IDENT (PropIdent x402) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x402))
   and cvtINIT_STEP (InitStep(x405, x406)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x405, 
          cvtEXPR x406]))
     | cvtINIT_STEP (AssignStep(x410, x411)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x410, cvtEXPR x411]))
   and cvtTYPE_EXPR (SpecialType x415) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x415))
     | cvtTYPE_EXPR (UnionType ls419) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x418 => 
                                                                                                           cvtTYPE_EXPR x418
                                                                                                    ) ls419)))
     | cvtTYPE_EXPR (ArrayType ls426) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x425 => 
                                                                                                           cvtTYPE_EXPR x425
                                                                                                    ) ls426)))
     | cvtTYPE_EXPR (TypeName x432) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x432))
     | cvtTYPE_EXPR (ElementTypeRef(x435, n436)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x435, PrettyRep.Int n436]))
     | cvtTYPE_EXPR (FieldTypeRef(x440, x441)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x440, cvtIDENT x441]))
     | cvtTYPE_EXPR (FunctionType x445) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x445))
     | cvtTYPE_EXPR (ObjectType ls449) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x448 => 
                                                                                                             cvtFIELD_TYPE x448
                                                                                                      ) ls449)))
     | cvtTYPE_EXPR (LikeType x455) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x455))
     | cvtTYPE_EXPR (WrapType x458) = PrettyRep.Ctor ("WrapType", SOME (cvtTYPE_EXPR x458))
     | cvtTYPE_EXPR (AppType{base=x461, args=ls463}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x461), ("args", PrettyRep.List (List.map (fn x462 => 
                                                                                                     cvtTYPE_EXPR x462
                                                                                              ) ls463))]))
     | cvtTYPE_EXPR (LamType{params=ls475, body=x479}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x474 => 
                                                                          cvtIDENT x474
                                                                   ) ls475)), 
          ("body", cvtTYPE_EXPR x479)]))
     | cvtTYPE_EXPR (NullableType{expr=x487, nullable=b488}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x487), ("nullable", PrettyRep.Bool b488)]))
     | cvtTYPE_EXPR (InstanceType x496) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x496))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x500) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x500))
     | cvtSTMT (InitStmt{kind=x503, ns=opt505, prototype=b509, static=b510, 
          temps=x511, inits=ls513}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x503), ("ns", 
       (case opt505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x504 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x504))
       )), ("prototype", PrettyRep.Bool b509), ("static", PrettyRep.Bool b510), 
          ("temps", cvtBINDINGS x511), ("inits", PrettyRep.List (List.map (fn x512 => 
                                                                                 cvtINIT_STEP x512
                                                                          ) ls513))]))
     | cvtSTMT (ClassBlock x532) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x532))
     | cvtSTMT (ForInStmt x535) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x535))
     | cvtSTMT (ThrowStmt x538) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x538))
     | cvtSTMT (ReturnStmt x541) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x541))
     | cvtSTMT (BreakStmt opt545) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x544 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x544))
       ))
     | cvtSTMT (ContinueStmt opt552) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt552 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x551 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x551))
       ))
     | cvtSTMT (BlockStmt x558) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x558))
     | cvtSTMT (LabeledStmt(x561, x562)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x561, 
          cvtSTMT x562]))
     | cvtSTMT (LetStmt x566) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x566))
     | cvtSTMT (WhileStmt x569) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x569))
     | cvtSTMT (DoWhileStmt x572) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x572))
     | cvtSTMT (ForStmt x575) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x575))
     | cvtSTMT (IfStmt{cnd=x578, thn=x579, els=x580}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x578), ("thn", cvtSTMT x579), 
          ("els", cvtSTMT x580)]))
     | cvtSTMT (WithStmt{obj=x590, ty=x591, body=x592}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x590), ("ty", cvtTY x591), ("body", 
          cvtSTMT x592)]))
     | cvtSTMT (TryStmt{block=x602, catches=ls604, finally=opt609}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x602), ("catches", PrettyRep.List (List.map (fn x603 => 
                                                                                                     cvtCATCH_CLAUSE x603
                                                                                              ) ls604)), 
          ("finally", 
       (case opt609 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x608 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x608))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x622, labels=ls624, cases=ls629}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x622), ("labels", PrettyRep.List (List.map (fn x623 => 
                                                                                                  cvtIDENT x623
                                                                                           ) ls624)), 
          ("cases", PrettyRep.List (List.map (fn x628 => cvtCASE x628
                                             ) ls629))]))
     | cvtSTMT (SwitchTypeStmt{cond=x642, ty=x643, cases=ls645}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x642), ("ty", cvtTY x643), 
          ("cases", PrettyRep.List (List.map (fn x644 => cvtCATCH_CLAUSE x644
                                             ) ls645))]))
     | cvtSTMT (DXNStmt{expr=x658}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x658)]))
   and cvtEXPR (TernaryExpr(x664, x665, x666)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x664, cvtEXPR x665, cvtEXPR x666]))
     | cvtEXPR (BinaryExpr(x670, x671, x672)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x670, cvtEXPR x671, cvtEXPR x672]))
     | cvtEXPR (BinaryTypeExpr(x676, x677, x678)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x676, cvtEXPR x677, cvtTY x678]))
     | cvtEXPR (UnaryExpr(x682, x683)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x682, 
          cvtEXPR x683]))
     | cvtEXPR (TypeExpr x687) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x687))
     | cvtEXPR (ThisExpr opt691) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt691 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x690 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x690))
       ))
     | cvtEXPR (YieldExpr opt698) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x697 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x697))
       ))
     | cvtEXPR (SuperExpr opt705) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt705 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x704 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x704))
       ))
     | cvtEXPR (LiteralExpr x711) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x711))
     | cvtEXPR (CallExpr{func=x714, actuals=ls716}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x714), ("actuals", PrettyRep.List (List.map (fn x715 => 
                                                                                                   cvtEXPR x715
                                                                                            ) ls716))]))
     | cvtEXPR (ApplyTypeExpr{expr=x727, actuals=ls729}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x727), ("actuals", PrettyRep.List (List.map (fn x728 => 
                                                                                                   cvtTY x728
                                                                                            ) ls729))]))
     | cvtEXPR (LetExpr{defs=x740, body=x741, head=opt743}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x740), ("body", cvtEXPR x741), 
          ("head", 
       (case opt743 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x742 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x742))
       ))]))
     | cvtEXPR (NewExpr{obj=x756, actuals=ls758}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x756), ("actuals", PrettyRep.List (List.map (fn x757 => 
                                                                                                  cvtEXPR x757
                                                                                           ) ls758))]))
     | cvtEXPR (ObjectRef{base=x769, ident=x770, loc=opt772}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x769), ("ident", cvtIDENT_EXPR x770), 
          ("loc", 
       (case opt772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x771 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x771))
       ))]))
     | cvtEXPR (LexicalRef{ident=x785, loc=opt787}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x785), ("loc", 
       (case opt787 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x786 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x786))
       ))]))
     | cvtEXPR (SetExpr(x798, x799, x800)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x798, 
          cvtEXPR x799, cvtEXPR x800]))
     | cvtEXPR (ListExpr ls805) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x804 => 
                                                                                                    cvtEXPR x804
                                                                                             ) ls805)))
     | cvtEXPR (InitExpr(x811, x812, x813)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x811, 
          cvtHEAD x812, cvtINITS x813]))
     | cvtEXPR (GetTemp n817) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n817))
     | cvtEXPR (GetParam n820) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n820))
     | cvtEXPR (Comprehension(x823, ls825, opt830)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x823, PrettyRep.List (List.map (fn x824 => 
                                                                               cvtFOR_ENUM_HEAD x824
                                                                        ) ls825), 
          
       (case opt830 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x829 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x829))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n842) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n842))
     | cvtFIXTURE_NAME (PropName x845) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x845))
   and cvtIDENT_EXPR (Identifier{ident=x848, openNamespaces=ls854}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x848), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls850 => PrettyRep.List (List.map (fn x849 => 
                                                                                cvtNAMESPACE x849
                                                                         ) ls850)
                                   ) ls854))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x865, expr=x866}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x865), ("expr", cvtEXPR x866)]))
     | cvtIDENT_EXPR (AttributeIdentifier x874) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x874))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x877, openNamespaces=ls883}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x877), ("openNamespaces", PrettyRep.List (List.map (fn ls879 => 
                                                                            PrettyRep.List (List.map (fn x878 => 
                                                                                                            cvtNAMESPACE x878
                                                                                                     ) ls879)
                                                                     ) ls883))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x894, ident=s895}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x894), ("ident", PrettyRep.UniStr s895)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls904, x908)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x903 => cvtIDENT x903
                                                          ) ls904), cvtIDENT_EXPR x908]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r915) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r915))
     | cvtLITERAL (LiteralDecimal d918) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d918))
     | cvtLITERAL (LiteralInt i921) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i921))
     | cvtLITERAL (LiteralUInt u924) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u924))
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
   and cvtFRAGMENT (Package{name=ls1711, fragments=ls1716}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1710 => 
                                                                        cvtIDENT x1710
                                                                 ) ls1711)), 
          ("fragments", PrettyRep.List (List.map (fn x1715 => cvtFRAGMENT x1715
                                                 ) ls1716))]))
     | cvtFRAGMENT (Anon x1727) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1727))
end

