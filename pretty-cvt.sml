structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtNONCE n20 = PrettyRep.Int n20
   and cvtNAMESPACE (OpaqueNamespace x21) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtNONCE x21))
     | cvtNAMESPACE (TransparentNamespace s24) = PrettyRep.Ctor ("TransparentNamespace", 
          SOME (PrettyRep.UniStr s24))
   and cvtNAME {ns=x27, id=x28} = PrettyRep.Rec [("ns", cvtNAMESPACE x27), 
          ("id", cvtIDENT x28)]
   and cvtMULTINAME {nss=ls39, id=x43} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls35 => 
                                                                                                PrettyRep.List (List.map (fn x34 => 
                                                                                                                                cvtNAMESPACE x34
                                                                                                                         ) ls35)
                                                                                         ) ls39)), 
          ("id", cvtIDENT x43)]
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
     | cvtUNOP (Spread) = PrettyRep.Ctor ("Spread", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x110) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x110))
     | cvtPRAGMA (UseDefaultNamespace x113) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x113))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x124, privateNS=x125, protectedNS=x126, parentProtectedNSs=ls128, 
          typeParams=ls133, nonnullable=b137, dynamic=b138, extends=opt140, 
          implements=ls145, classRib=x149, instanceRib=x150, instanceInits=x151, 
          constructor=opt153, classType=x157, instanceType=x158}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x124), ("privateNS", cvtNAMESPACE x125), 
          ("protectedNS", cvtNAMESPACE x126), ("parentProtectedNSs", PrettyRep.List (List.map (fn x127 => 
                                                                                                     cvtNAMESPACE x127
                                                                                              ) ls128)), 
          ("typeParams", PrettyRep.List (List.map (fn x132 => cvtIDENT x132
                                                  ) ls133)), ("nonnullable", 
          PrettyRep.Bool b137), ("dynamic", PrettyRep.Bool b138), ("extends", 
          
       (case opt140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x139 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x139))
       )), ("implements", PrettyRep.List (List.map (fn x144 => cvtTYPE_EXPR x144
                                                   ) ls145)), ("classRib", 
          cvtRIB x149), ("instanceRib", cvtRIB x150), ("instanceInits", cvtHEAD x151), 
          ("constructor", 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x152))
       )), ("classType", cvtTYPE_EXPR x157), ("instanceType", cvtTYPE_EXPR x158)]))
   and cvtIFACE (Iface{name=x192, typeParams=ls194, nonnullable=b198, extends=ls200, 
          instanceRib=x204, instanceType=x205}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x192), ("typeParams", PrettyRep.List (List.map (fn x193 => 
                                                                                                      cvtIDENT x193
                                                                                               ) ls194)), 
          ("nonnullable", PrettyRep.Bool b198), ("extends", PrettyRep.List (List.map (fn x199 => 
                                                                                            cvtTYPE_EXPR x199
                                                                                     ) ls200)), 
          ("instanceRib", cvtRIB x204), ("instanceType", cvtTYPE_EXPR x205)]))
   and cvtCTOR (Ctor{settings=x221, superArgs=ls223, func=x227}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x221), ("superArgs", PrettyRep.List (List.map (fn x222 => 
                                                                                                         cvtEXPR x222
                                                                                                  ) ls223)), 
          ("func", cvtFUNC x227)]))
   and cvtFUNC (Func{name=x237, fsig=x238, native=b239, generator=b240, block=opt242, 
          param=x246, defaults=ls248, ty=x252, loc=opt254}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x237), ("fsig", cvtFUNC_SIG x238), 
          ("native", PrettyRep.Bool b239), ("generator", PrettyRep.Bool b240), 
          ("block", 
       (case opt242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x241 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x241))
       )), ("param", cvtHEAD x246), ("defaults", PrettyRep.List (List.map (fn x247 => 
                                                                                 cvtEXPR x247
                                                                          ) ls248)), 
          ("ty", cvtTYPE_EXPR x252), ("loc", 
       (case opt254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x253 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x253))
       ))]))
   and cvtDEFN (ClassDefn x279) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x279))
     | cvtDEFN (VariableDefn x282) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x282))
     | cvtDEFN (FunctionDefn x285) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x285))
     | cvtDEFN (ConstructorDefn x288) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x288))
     | cvtDEFN (InterfaceDefn x291) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x291))
     | cvtDEFN (NamespaceDefn x294) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x294))
     | cvtDEFN (TypeDefn x297) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x297))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls301, params=x305, paramTypes=ls307, 
          defaults=ls312, ctorInits=opt323, returnType=x327, thisType=opt329, 
          hasRest=b333}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x300 => cvtIDENT x300
                                   ) ls301)), ("params", cvtBINDINGS x305), 
          ("paramTypes", PrettyRep.List (List.map (fn x306 => cvtTYPE_EXPR x306
                                                  ) ls307)), ("defaults", PrettyRep.List (List.map (fn x311 => 
                                                                                                          cvtEXPR x311
                                                                                                   ) ls312)), 
          ("ctorInits", 
       (case opt323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x316, ls318) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x316, 
            PrettyRep.List (List.map (fn x317 => cvtEXPR x317
                                     ) ls318)]))
       )), ("returnType", cvtTYPE_EXPR x327), ("thisType", 
       (case opt329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x328 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x328))
       )), ("hasRest", PrettyRep.Bool b333)]))
   and cvtBINDING (Binding{ident=x353, ty=x354}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x353), ("ty", cvtTYPE_EXPR x354)]))
   and cvtBINDING_IDENT (TempIdent n362) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n362))
     | cvtBINDING_IDENT (ParamIdent n365) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n365))
     | cvtBINDING_IDENT (PropIdent x368) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x368))
   and cvtINIT_STEP (InitStep(x371, x372)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x371, 
          cvtEXPR x372]))
     | cvtINIT_STEP (AssignStep(x376, x377)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x376, cvtEXPR x377]))
   and cvtTYPE_EXPR (SpecialType x381) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x381))
     | cvtTYPE_EXPR (UnionType ls385) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x384 => 
                                                                                                           cvtTYPE_EXPR x384
                                                                                                    ) ls385)))
     | cvtTYPE_EXPR (ArrayType ls392) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x391 => 
                                                                                                           cvtTYPE_EXPR x391
                                                                                                    ) ls392)))
     | cvtTYPE_EXPR (TypeName(x398, opt400)) = PrettyRep.Ctor ("TypeName", 
          SOME (PrettyRep.Tuple [cvtIDENT_EXPR x398, 
       (case opt400 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x399 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x399))
       )]))
     | cvtTYPE_EXPR (ElementTypeRef(x407, n408)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x407, PrettyRep.Int n408]))
     | cvtTYPE_EXPR (FieldTypeRef(x412, x413)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x412, cvtIDENT x413]))
     | cvtTYPE_EXPR (FunctionType x417) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x417))
     | cvtTYPE_EXPR (ObjectType ls421) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x420 => 
                                                                                                             cvtFIELD_TYPE x420
                                                                                                      ) ls421)))
     | cvtTYPE_EXPR (AppType{base=x427, args=ls429}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x427), ("args", PrettyRep.List (List.map (fn x428 => 
                                                                                                     cvtTYPE_EXPR x428
                                                                                              ) ls429))]))
     | cvtTYPE_EXPR (LamType{params=ls441, body=x445}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x440 => 
                                                                          cvtIDENT x440
                                                                   ) ls441)), 
          ("body", cvtTYPE_EXPR x445)]))
     | cvtTYPE_EXPR (NullableType{expr=x453, nullable=b454}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x453), ("nullable", PrettyRep.Bool b454)]))
     | cvtTYPE_EXPR (InstanceType x462) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x462))
     | cvtTYPE_EXPR (TypeVarFixtureRef x465) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x465))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x469) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x469))
     | cvtSTMT (InitStmt{kind=x472, ns=opt474, prototype=b478, static=b479, 
          temps=x480, inits=ls482}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x472), ("ns", 
       (case opt474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x473 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x473))
       )), ("prototype", PrettyRep.Bool b478), ("static", PrettyRep.Bool b479), 
          ("temps", cvtBINDINGS x480), ("inits", PrettyRep.List (List.map (fn x481 => 
                                                                                 cvtINIT_STEP x481
                                                                          ) ls482))]))
     | cvtSTMT (ClassBlock x501) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x501))
     | cvtSTMT (ForInStmt x504) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x504))
     | cvtSTMT (ThrowStmt x507) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x507))
     | cvtSTMT (ReturnStmt x510) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x510))
     | cvtSTMT (BreakStmt opt514) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x513 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x513))
       ))
     | cvtSTMT (ContinueStmt opt521) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x520 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x520))
       ))
     | cvtSTMT (BlockStmt x527) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x527))
     | cvtSTMT (LabeledStmt(x530, x531)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x530, 
          cvtSTMT x531]))
     | cvtSTMT (LetStmt x535) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x535))
     | cvtSTMT (WhileStmt x538) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x538))
     | cvtSTMT (DoWhileStmt x541) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x541))
     | cvtSTMT (ForStmt x544) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x544))
     | cvtSTMT (IfStmt{cnd=x547, thn=x548, els=x549}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x547), ("thn", cvtSTMT x548), 
          ("els", cvtSTMT x549)]))
     | cvtSTMT (WithStmt{obj=x559, ty=x560, body=x561}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x559), ("ty", cvtTYPE_EXPR x560), 
          ("body", cvtSTMT x561)]))
     | cvtSTMT (TryStmt{block=x571, catches=ls573, finally=opt578}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x571), ("catches", PrettyRep.List (List.map (fn x572 => 
                                                                                                     cvtCATCH_CLAUSE x572
                                                                                              ) ls573)), 
          ("finally", 
       (case opt578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x577 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x577))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x591, labels=ls593, cases=ls598}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x591), ("labels", PrettyRep.List (List.map (fn x592 => 
                                                                                                  cvtIDENT x592
                                                                                           ) ls593)), 
          ("cases", PrettyRep.List (List.map (fn x597 => cvtCASE x597
                                             ) ls598))]))
     | cvtSTMT (SwitchTypeStmt{cond=x611, ty=x612, cases=ls614}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x611), ("ty", cvtTYPE_EXPR x612), 
          ("cases", PrettyRep.List (List.map (fn x613 => cvtCATCH_CLAUSE x613
                                             ) ls614))]))
     | cvtSTMT (DXNStmt{expr=x627}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x627)]))
   and cvtEXPR (TernaryExpr(x633, x634, x635)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x633, cvtEXPR x634, cvtEXPR x635]))
     | cvtEXPR (BinaryExpr(x639, x640, x641)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x639, cvtEXPR x640, cvtEXPR x641]))
     | cvtEXPR (BinaryTypeExpr(x645, x646, x647)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x645, cvtEXPR x646, cvtTYPE_EXPR x647]))
     | cvtEXPR (UnaryExpr(x651, x652)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x651, 
          cvtEXPR x652]))
     | cvtEXPR (TypeExpr x656) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x656))
     | cvtEXPR (ThisExpr opt660) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt660 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x659 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x659))
       ))
     | cvtEXPR (YieldExpr opt667) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x666 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x666))
       ))
     | cvtEXPR (SuperExpr opt674) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt674 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x673 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x673))
       ))
     | cvtEXPR (CallExpr{func=x680, actuals=ls682}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x680), ("actuals", PrettyRep.List (List.map (fn x681 => 
                                                                                                   cvtEXPR x681
                                                                                            ) ls682))]))
     | cvtEXPR (ApplyTypeExpr{expr=x693, actuals=ls695}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x693), ("actuals", PrettyRep.List (List.map (fn x694 => 
                                                                                                   cvtTYPE_EXPR x694
                                                                                            ) ls695))]))
     | cvtEXPR (LetExpr{defs=x706, body=x707, head=opt709}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x706), ("body", cvtEXPR x707), 
          ("head", 
       (case opt709 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x708 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x708))
       ))]))
     | cvtEXPR (NewExpr{obj=x722, actuals=ls724}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x722), ("actuals", PrettyRep.List (List.map (fn x723 => 
                                                                                                  cvtEXPR x723
                                                                                           ) ls724))]))
     | cvtEXPR (ObjectRef{base=x735, ident=x736, loc=opt738}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x735), ("ident", cvtIDENT_EXPR x736), 
          ("loc", 
       (case opt738 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x737 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x737))
       ))]))
     | cvtEXPR (LexicalRef{ident=x751, loc=opt753}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x751), ("loc", 
       (case opt753 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x752 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x752))
       ))]))
     | cvtEXPR (SetExpr(x764, x765, x766)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x764, 
          cvtEXPR x765, cvtEXPR x766]))
     | cvtEXPR (ListExpr ls771) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x770 => 
                                                                                                    cvtEXPR x770
                                                                                             ) ls771)))
     | cvtEXPR (InitExpr(x777, x778, x779)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x777, 
          cvtHEAD x778, cvtINITS x779]))
     | cvtEXPR (GetTemp n783) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n783))
     | cvtEXPR (GetParam n786) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n786))
     | cvtEXPR (Comprehension(x789, ls791, opt796)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x789, PrettyRep.List (List.map (fn x790 => 
                                                                               cvtFOR_ENUM_HEAD x790
                                                                        ) ls791), 
          
       (case opt796 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x795 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x795))
       )]))
     | cvtEXPR (LiteralExpr x803) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x803))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n811) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n811))
     | cvtFIXTURE_NAME (PropName x814) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x814))
   and cvtIDENT_EXPR (Identifier{ident=x817, openNamespaces=ls823}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x817), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls819 => PrettyRep.List (List.map (fn x818 => 
                                                                                cvtNAMESPACE x818
                                                                         ) ls819)
                                   ) ls823))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x834, expr=x835}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x834), ("expr", cvtEXPR x835)]))
     | cvtIDENT_EXPR (AttributeIdentifier x843) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x843))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x846, openNamespaces=ls852}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x846), ("openNamespaces", PrettyRep.List (List.map (fn ls848 => 
                                                                            PrettyRep.List (List.map (fn x847 => 
                                                                                                            cvtNAMESPACE x847
                                                                                                     ) ls848)
                                                                     ) ls852))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x863, ident=s864}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x863), ("ident", PrettyRep.UniStr s864)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r874) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r874))
     | cvtLITERAL (LiteralDecimal d877) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d877))
     | cvtLITERAL (LiteralBoolean b880) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b880))
     | cvtLITERAL (LiteralString s883) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s883))
     | cvtLITERAL (LiteralArray{exprs=x886, ty=opt888}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x886), ("ty", 
       (case opt888 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x887 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x887))
       ))]))
     | cvtLITERAL (LiteralXML ls900) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x899 => 
                                                                                                           cvtEXPR x899
                                                                                                    ) ls900)))
     | cvtLITERAL (LiteralNamespace x906) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x906))
     | cvtLITERAL (LiteralObject{expr=ls910, ty=opt915}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x909 => 
                                                                        cvtFIELD x909
                                                                 ) ls910)), 
          ("ty", 
       (case opt915 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x914 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x914))
       ))]))
     | cvtLITERAL (LiteralFunction x926) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x926))
     | cvtLITERAL (LiteralRegExp{str=s929}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s929)]))
   and cvtBLOCK (Block x935) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x935))
   and cvtFIXTURE (NamespaceFixture x938) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x938))
     | cvtFIXTURE (ClassFixture x941) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x941))
     | cvtFIXTURE (InterfaceFixture x944) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x944))
     | cvtFIXTURE (TypeVarFixture x947) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x947))
     | cvtFIXTURE (TypeFixture x950) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x950))
     | cvtFIXTURE (MethodFixture{func=x953, ty=x954, readOnly=b955, override=b956, 
          final=b957}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x953), ("ty", cvtTYPE_EXPR x954), ("readOnly", PrettyRep.Bool b955), 
          ("override", PrettyRep.Bool b956), ("final", PrettyRep.Bool b957)]))
     | cvtFIXTURE (ValFixture{ty=x971, readOnly=b972}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x971), ("readOnly", PrettyRep.Bool b972)]))
     | cvtFIXTURE (VirtualValFixture{ty=x980, getter=opt982, setter=opt987}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x980), ("getter", 
       (case opt982 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x981 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x981))
       )), ("setter", 
       (case opt987 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x986 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x986))
       ))]))
   and cvtHEAD (Head(x1000, x1001)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1000, 
          cvtINITS x1001]))
   and cvtBINDINGS (ls1006, ls1011) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1005 => 
                                                                                       cvtBINDING x1005
                                                                                ) ls1006), 
          PrettyRep.List (List.map (fn x1010 => cvtINIT_STEP x1010
                                   ) ls1011)]
   and cvtRIB ls1019 = PrettyRep.List (List.map (fn (x1016, x1017) => PrettyRep.Tuple [cvtFIXTURE_NAME x1016, 
                                                       cvtFIXTURE x1017]
                                                ) ls1019)
   and cvtRIBS ls1030 = PrettyRep.List (List.map (fn ls1026 => PrettyRep.List (List.map (fn (x1023, 
                                                                                               x1024) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1023, 
                                                                                               cvtFIXTURE x1024]
                                                                                        ) ls1026)
                                                 ) ls1030)
   and cvtINITS ls1037 = PrettyRep.List (List.map (fn (x1034, x1035) => PrettyRep.Tuple [cvtFIXTURE_NAME x1034, 
                                                         cvtEXPR x1035]
                                                  ) ls1037)
   and cvtINSTANCE_TYPE {name=x1041, typeParams=ls1043, typeArgs=ls1048, nonnullable=b1052, 
          superTypes=ls1054, ty=x1058, dynamic=b1059} = PrettyRep.Rec [("name", 
          cvtNAME x1041), ("typeParams", PrettyRep.List (List.map (fn x1042 => 
                                                                         cvtIDENT x1042
                                                                  ) ls1043)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1047 => cvtTYPE_EXPR x1047
                                                ) ls1048)), ("nonnullable", 
          PrettyRep.Bool b1052), ("superTypes", PrettyRep.List (List.map (fn x1053 => 
                                                                                cvtTYPE_EXPR x1053
                                                                         ) ls1054)), 
          ("ty", cvtTYPE_EXPR x1058), ("dynamic", PrettyRep.Bool b1059)]
   and cvtFIELD {kind=x1075, name=x1076, init=x1077} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1075), ("name", cvtIDENT_EXPR x1076), ("init", cvtEXPR x1077)]
   and cvtFIELD_TYPE {name=x1085, ty=x1086} = PrettyRep.Rec [("name", cvtIDENT x1085), 
          ("ty", cvtTYPE_EXPR x1086)]
   and cvtFUNC_TYPE {params=ls1093, result=x1097, thisType=opt1099, hasRest=b1103, 
          minArgs=n1104} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1092 => 
                                                                                     cvtTYPE_EXPR x1092
                                                                              ) ls1093)), 
          ("result", cvtTYPE_EXPR x1097), ("thisType", 
       (case opt1099 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1098 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1098))
       )), ("hasRest", PrettyRep.Bool b1103), ("minArgs", PrettyRep.Int n1104)]
   and cvtFUNC_DEFN {kind=x1116, ns=opt1118, final=b1122, override=b1123, prototype=b1124, 
          static=b1125, func=x1126} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1116), 
          ("ns", 
       (case opt1118 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1117 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1117))
       )), ("final", PrettyRep.Bool b1122), ("override", PrettyRep.Bool b1123), 
          ("prototype", PrettyRep.Bool b1124), ("static", PrettyRep.Bool b1125), 
          ("func", cvtFUNC x1126)]
   and cvtCTOR_DEFN x1142 = cvtCTOR x1142
   and cvtVAR_DEFN {kind=x1143, ns=opt1145, static=b1149, prototype=b1150, 
          bindings=(ls1152, ls1157)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1143), 
          ("ns", 
       (case opt1145 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1144 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1144))
       )), ("static", PrettyRep.Bool b1149), ("prototype", PrettyRep.Bool b1150), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1151 => 
                                                                        cvtBINDING x1151
                                                                 ) ls1152), 
          PrettyRep.List (List.map (fn x1156 => cvtINIT_STEP x1156
                                   ) ls1157)])]
   and cvtNAMESPACE_DEFN {ident=x1173, ns=opt1175, init=opt1180} = PrettyRep.Rec [("ident", 
          cvtIDENT x1173), ("ns", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1174))
       )), ("init", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1179))
       ))]
   and cvtCLASS_DEFN {ns=opt1192, privateNS=x1196, protectedNS=x1197, ident=x1198, 
          nonnullable=b1199, dynamic=b1200, final=b1201, params=ls1203, extends=opt1208, 
          implements=ls1213, classDefns=ls1218, instanceDefns=ls1223, instanceStmts=ls1228, 
          ctorDefn=opt1233} = PrettyRep.Rec [("ns", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1191))
       )), ("privateNS", cvtNAMESPACE x1196), ("protectedNS", cvtNAMESPACE x1197), 
          ("ident", cvtIDENT x1198), ("nonnullable", PrettyRep.Bool b1199), 
          ("dynamic", PrettyRep.Bool b1200), ("final", PrettyRep.Bool b1201), 
          ("params", PrettyRep.List (List.map (fn x1202 => cvtIDENT x1202
                                              ) ls1203)), ("extends", 
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1207))
       )), ("implements", PrettyRep.List (List.map (fn x1212 => cvtTYPE_EXPR x1212
                                                   ) ls1213)), ("classDefns", 
          PrettyRep.List (List.map (fn x1217 => cvtDEFN x1217
                                   ) ls1218)), ("instanceDefns", PrettyRep.List (List.map (fn x1222 => 
                                                                                                 cvtDEFN x1222
                                                                                          ) ls1223)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1227 => cvtSTMT x1227
                                                     ) ls1228)), ("ctorDefn", 
          
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1232))
       ))]
   and cvtINTERFACE_DEFN {ident=x1266, ns=opt1268, nonnullable=b1272, params=ls1274, 
          extends=ls1279, instanceDefns=ls1284} = PrettyRep.Rec [("ident", 
          cvtIDENT x1266), ("ns", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1267))
       )), ("nonnullable", PrettyRep.Bool b1272), ("params", PrettyRep.List (List.map (fn x1273 => 
                                                                                             cvtIDENT x1273
                                                                                      ) ls1274)), 
          ("extends", PrettyRep.List (List.map (fn x1278 => cvtTYPE_EXPR x1278
                                               ) ls1279)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1283 => cvtDEFN x1283
                                   ) ls1284))]
   and cvtTYPE_DEFN {ident=x1301, ns=opt1303, init=x1307} = PrettyRep.Rec [("ident", 
          cvtIDENT x1301), ("ns", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1302))
       )), ("init", cvtTYPE_EXPR x1307)]
   and cvtCLASS_BLOCK {ns=opt1316, protectedNS=x1320, privateNS=x1321, ident=x1322, 
          name=opt1324, block=x1328} = PrettyRep.Rec [("ns", 
       (case opt1316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1315 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1315))
       )), ("protectedNS", cvtNAMESPACE x1320), ("privateNS", cvtNAMESPACE x1321), 
          ("ident", cvtIDENT x1322), ("name", 
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1323))
       )), ("block", cvtBLOCK x1328)]
   and cvtFOR_ENUM_HEAD {isEach=b1342, bindings=(ls1344, ls1349), expr=x1354} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1342), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1343 => 
                                                                                                                         cvtBINDING x1343
                                                                                                                  ) ls1344), 
          PrettyRep.List (List.map (fn x1348 => cvtINIT_STEP x1348
                                   ) ls1349)]), ("expr", cvtEXPR x1354)]
   and cvtFOR_ENUM_STMT {isEach=b1362, defn=opt1393, obj=x1397, rib=opt1405, 
          next=x1409, labels=ls1411, body=x1415} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1362), ("defn", 
       (case opt1393 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1363, ns=opt1365, static=b1369, prototype=b1370, bindings=(ls1372, 
            ls1377)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1363), ("ns", 
         (case opt1365 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1364))
         )), ("static", PrettyRep.Bool b1369), ("prototype", PrettyRep.Bool b1370), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1371 => 
                                                                          cvtBINDING x1371
                                                                   ) ls1372), 
            PrettyRep.List (List.map (fn x1376 => cvtINIT_STEP x1376
                                     ) ls1377)])]))
       )), ("obj", cvtEXPR x1397), ("rib", 
       (case opt1405 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1401 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1398, 
                                                                                      x1399) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1398, 
                                                                                      cvtFIXTURE x1399]
                                                                               ) ls1401)))
       )), ("next", cvtSTMT x1409), ("labels", PrettyRep.List (List.map (fn x1410 => 
                                                                               cvtIDENT x1410
                                                                        ) ls1411)), 
          ("body", cvtSTMT x1415)]
   and cvtFOR_STMT {rib=opt1438, defn=opt1472, init=ls1477, cond=x1481, update=x1482, 
          labels=ls1484, body=x1488} = PrettyRep.Rec [("rib", 
       (case opt1438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1434 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1431, 
                                                                                      x1432) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1431, 
                                                                                      cvtFIXTURE x1432]
                                                                               ) ls1434)))
       )), ("defn", 
       (case opt1472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1442, ns=opt1444, static=b1448, prototype=b1449, bindings=(ls1451, 
            ls1456)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1442), ("ns", 
         (case opt1444 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1443 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1443))
         )), ("static", PrettyRep.Bool b1448), ("prototype", PrettyRep.Bool b1449), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1450 => 
                                                                          cvtBINDING x1450
                                                                   ) ls1451), 
            PrettyRep.List (List.map (fn x1455 => cvtINIT_STEP x1455
                                     ) ls1456)])]))
       )), ("init", PrettyRep.List (List.map (fn x1476 => cvtSTMT x1476
                                             ) ls1477)), ("cond", cvtEXPR x1481), 
          ("update", cvtEXPR x1482), ("labels", PrettyRep.List (List.map (fn x1483 => 
                                                                                cvtIDENT x1483
                                                                         ) ls1484)), 
          ("body", cvtSTMT x1488)]
   and cvtWHILE_STMT {cond=x1504, rib=opt1512, body=x1516, labels=ls1518} = 
          PrettyRep.Rec [("cond", cvtEXPR x1504), ("rib", 
       (case opt1512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1508 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1505, 
                                                                                      x1506) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1505, 
                                                                                      cvtFIXTURE x1506]
                                                                               ) ls1508)))
       )), ("body", cvtSTMT x1516), ("labels", PrettyRep.List (List.map (fn x1517 => 
                                                                               cvtIDENT x1517
                                                                        ) ls1518))]
   and cvtDIRECTIVES {pragmas=ls1532, defns=ls1537, head=opt1542, body=ls1547, 
          loc=opt1552} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1531 => 
                                                                                    cvtPRAGMA x1531
                                                                             ) ls1532)), 
          ("defns", PrettyRep.List (List.map (fn x1536 => cvtDEFN x1536
                                             ) ls1537)), ("head", 
       (case opt1542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1541 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1541))
       )), ("body", PrettyRep.List (List.map (fn x1546 => cvtSTMT x1546
                                             ) ls1547)), ("loc", 
       (case opt1552 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1551 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1551))
       ))]
   and cvtCASE {label=opt1568, inits=opt1579, body=x1583} = PrettyRep.Rec [("label", 
          
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1567))
       )), ("inits", 
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1575 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1572, 
                                                                                      x1573) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1572, 
                                                                                      cvtEXPR x1573]
                                                                               ) ls1575)))
       )), ("body", cvtBLOCK x1583)]
   and cvtCATCH_CLAUSE {bindings=(ls1592, ls1597), ty=x1602, rib=opt1610, inits=opt1621, 
          block=x1625} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1591 => 
                                                                                                      cvtBINDING x1591
                                                                                               ) ls1592), 
          PrettyRep.List (List.map (fn x1596 => cvtINIT_STEP x1596
                                   ) ls1597)]), ("ty", cvtTYPE_EXPR x1602), 
          ("rib", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1606 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1603, 
                                                                                      x1604) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1603, 
                                                                                      cvtFIXTURE x1604]
                                                                               ) ls1606)))
       )), ("inits", 
       (case opt1621 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1617 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1614, 
                                                                                      x1615) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1614, 
                                                                                      cvtEXPR x1615]
                                                                               ) ls1617)))
       )), ("block", cvtBLOCK x1625)]
   and cvtFUNC_NAME {kind=x1637, ident=x1638} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1637), 
          ("ident", cvtIDENT x1638)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1644, getter=opt1646, setter=opt1651} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1644), ("getter", 
       (case opt1646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1645 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1645))
       )), ("setter", 
       (case opt1651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1650 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1650))
       ))]
   and cvtFRAGMENT (Anon x1662) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1662))
end

