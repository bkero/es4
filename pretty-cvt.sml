structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENTIFIER s19 = PrettyRep.UniStr s19
   and cvtNONCE n20 = PrettyRep.Int n20
   and cvtOPAQUE_NAMESPACE_IDENTIFIER x21 = cvtNONCE x21
   and cvtNAMESPACE (TransparentNamespace s22) = PrettyRep.Ctor ("TransparentNamespace", 
          SOME (PrettyRep.UniStr s22))
     | cvtNAMESPACE (OpaqueNamespace x25) = PrettyRep.Ctor ("OpaqueNamespace", 
          SOME (cvtOPAQUE_NAMESPACE_IDENTIFIER x25))
   and cvtNAME {ns=x28, id=x29} = PrettyRep.Rec [("ns", cvtNAMESPACE x28), 
          ("id", cvtIDENTIFIER x29)]
   and cvtNAMESPACE_SET ls36 = PrettyRep.List (List.map (fn x35 => cvtNAMESPACE x35
                                                        ) ls36)
   and cvtOPEN_NAMESPACES ls41 = PrettyRep.List (List.map (fn x40 => cvtNAMESPACE_SET x40
                                                          ) ls41)
   and cvtNAME_SET ls46 = PrettyRep.List (List.map (fn x45 => cvtNAME x45
                                                   ) ls46)
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
   and cvtPRAGMA (UseNamespace x107) = PrettyRep.Ctor ("UseNamespace", SOME (cvtNAMESPACE_EXPRESSION x107))
     | cvtPRAGMA (UseDefaultNamespace x110) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtNAMESPACE_EXPRESSION x110))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtNAME_EXPRESSION (QualifiedName{namespace=x115, identifier=x116}) = 
          PrettyRep.Ctor ("QualifiedName", SOME (PrettyRep.Rec [("namespace", 
          cvtNAMESPACE_EXPRESSION x115), ("identifier", cvtIDENTIFIER x116)]))
     | cvtNAME_EXPRESSION (UnqualifiedName{identifier=x124, openNamespaces=x125, 
          globalNames=x126}) = PrettyRep.Ctor ("UnqualifiedName", SOME (PrettyRep.Rec [("identifier", 
          cvtIDENTIFIER x124), ("openNamespaces", cvtOPEN_NAMESPACES x125), 
          ("globalNames", cvtNAME_SET x126)]))
   and cvtNAMESPACE_EXPRESSION (Namespace x136) = PrettyRep.Ctor ("Namespace", 
          SOME (cvtNAMESPACE x136))
     | cvtNAMESPACE_EXPRESSION (NamespaceName x139) = PrettyRep.Ctor ("NamespaceName", 
          SOME (cvtNAME_EXPRESSION x139))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x148, privateNS=x149, protectedNS=x150, parentProtectedNSs=ls152, 
          typeParams=ls157, nonnullable=b161, dynamic=b162, extends=opt164, 
          implements=ls169, classRib=x173, instanceRib=x174, instanceInits=x175, 
          constructor=opt177, classType=x181, instanceType=x182}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x148), ("privateNS", cvtNAMESPACE x149), 
          ("protectedNS", cvtNAMESPACE x150), ("parentProtectedNSs", PrettyRep.List (List.map (fn x151 => 
                                                                                                     cvtNAMESPACE x151
                                                                                              ) ls152)), 
          ("typeParams", PrettyRep.List (List.map (fn x156 => cvtIDENTIFIER x156
                                                  ) ls157)), ("nonnullable", 
          PrettyRep.Bool b161), ("dynamic", PrettyRep.Bool b162), ("extends", 
          
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x163))
       )), ("implements", PrettyRep.List (List.map (fn x168 => cvtTYPE x168
                                                   ) ls169)), ("classRib", 
          cvtRIB x173), ("instanceRib", cvtRIB x174), ("instanceInits", cvtHEAD x175), 
          ("constructor", 
       (case opt177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x176 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x176))
       )), ("classType", cvtTYPE x181), ("instanceType", cvtTYPE x182)]))
   and cvtIFACE (Iface{name=x216, typeParams=ls218, nonnullable=b222, extends=ls224, 
          instanceRib=x228, instanceType=x229}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x216), ("typeParams", PrettyRep.List (List.map (fn x217 => 
                                                                                                      cvtIDENTIFIER x217
                                                                                               ) ls218)), 
          ("nonnullable", PrettyRep.Bool b222), ("extends", PrettyRep.List (List.map (fn x223 => 
                                                                                            cvtTYPE x223
                                                                                     ) ls224)), 
          ("instanceRib", cvtRIB x228), ("instanceType", cvtTYPE x229)]))
   and cvtCTOR (Ctor{settings=x245, superArgs=ls247, func=x251}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x245), ("superArgs", PrettyRep.List (List.map (fn x246 => 
                                                                                                         cvtEXPRESSION x246
                                                                                                  ) ls247)), 
          ("func", cvtFUNC x251)]))
   and cvtFUNC (Func{name=x261, fsig=x262, native=b263, generator=b264, block=opt266, 
          param=x270, defaults=ls272, ty=x276, loc=opt278}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x261), ("fsig", cvtFUNC_SIG x262), 
          ("native", PrettyRep.Bool b263), ("generator", PrettyRep.Bool b264), 
          ("block", 
       (case opt266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x265 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x265))
       )), ("param", cvtHEAD x270), ("defaults", PrettyRep.List (List.map (fn x271 => 
                                                                                 cvtEXPRESSION x271
                                                                          ) ls272)), 
          ("ty", cvtTYPE x276), ("loc", 
       (case opt278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x277 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x277))
       ))]))
   and cvtDEFN (ClassDefn x303) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x303))
     | cvtDEFN (VariableDefn x306) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x306))
     | cvtDEFN (FunctionDefn x309) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x309))
     | cvtDEFN (ConstructorDefn x312) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x312))
     | cvtDEFN (InterfaceDefn x315) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x315))
     | cvtDEFN (NamespaceDefn x318) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x318))
     | cvtDEFN (TypeDefn x321) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x321))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls325, params=x329, paramTypes=ls331, 
          defaults=ls336, ctorInits=opt347, returnType=x351, thisType=opt353, 
          hasRest=b357}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x324 => cvtIDENTIFIER x324
                                   ) ls325)), ("params", cvtBINDINGS x329), 
          ("paramTypes", PrettyRep.List (List.map (fn x330 => cvtTYPE x330
                                                  ) ls331)), ("defaults", PrettyRep.List (List.map (fn x335 => 
                                                                                                          cvtEXPRESSION x335
                                                                                                   ) ls336)), 
          ("ctorInits", 
       (case opt347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x340, ls342) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x340, 
            PrettyRep.List (List.map (fn x341 => cvtEXPRESSION x341
                                     ) ls342)]))
       )), ("returnType", cvtTYPE x351), ("thisType", 
       (case opt353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x352 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x352))
       )), ("hasRest", PrettyRep.Bool b357)]))
   and cvtBINDING (Binding{ident=x377, ty=x378}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x377), ("ty", 
          cvtTYPE x378)]))
   and cvtBINDING_IDENTIFIER (TempIdent n386) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n386))
     | cvtBINDING_IDENTIFIER (ParamIdent n389) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n389))
     | cvtBINDING_IDENTIFIER (PropIdent x392) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x392))
   and cvtINIT_STEP (InitStep(x395, x396)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x395, 
          cvtEXPRESSION x396]))
     | cvtINIT_STEP (AssignStep(x400, x401)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x400, cvtEXPRESSION x401]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
     | cvtTYPE (ObjectType ls410) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x409 => 
                                                                                                        cvtFIELD_TYPE x409
                                                                                                 ) ls410)))
     | cvtTYPE (UnionType ls417) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x416 => 
                                                                                                      cvtTYPE x416
                                                                                               ) ls417)))
     | cvtTYPE (ArrayType ls424) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                      cvtTYPE x423
                                                                                               ) ls424)))
     | cvtTYPE (FunctionType x430) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x430))
     | cvtTYPE (TypeName(x433, opt435)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x433, 
          
       (case opt435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x434 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x434))
       )]))
     | cvtTYPE (ElementTypeRef(x442, n443)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x442, PrettyRep.Int n443]))
     | cvtTYPE (FieldTypeRef(x447, x448)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x447, cvtNAME_EXPRESSION x448]))
     | cvtTYPE (AppType{base=x452, args=ls454}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x452), ("args", PrettyRep.List (List.map (fn x453 => 
                                                                                                cvtTYPE x453
                                                                                         ) ls454))]))
     | cvtTYPE (NonNullType x465) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x465))
     | cvtTYPE (InstanceType x468) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x468))
     | cvtTYPE (TypeVarFixtureRef x471) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x471))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x475) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x475))
     | cvtSTATEMENT (InitStmt{kind=x478, ns=opt480, prototype=b484, static=b485, 
          temps=x486, inits=ls488}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x478), ("ns", 
       (case opt480 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x479 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x479))
       )), ("prototype", PrettyRep.Bool b484), ("static", PrettyRep.Bool b485), 
          ("temps", cvtBINDINGS x486), ("inits", PrettyRep.List (List.map (fn x487 => 
                                                                                 cvtINIT_STEP x487
                                                                          ) ls488))]))
     | cvtSTATEMENT (ClassBlock x507) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x507))
     | cvtSTATEMENT (ForInStmt x510) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x510))
     | cvtSTATEMENT (ThrowStmt x513) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x513))
     | cvtSTATEMENT (ReturnStmt x516) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x516))
     | cvtSTATEMENT (BreakStmt opt520) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt520 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x519 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x519))
       ))
     | cvtSTATEMENT (ContinueStmt opt527) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt527 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x526 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x526))
       ))
     | cvtSTATEMENT (BlockStmt x533) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x533))
     | cvtSTATEMENT (LabeledStmt(x536, x537)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x536, cvtSTATEMENT x537]))
     | cvtSTATEMENT (LetStmt x541) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x541))
     | cvtSTATEMENT (WhileStmt x544) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x544))
     | cvtSTATEMENT (DoWhileStmt x547) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x547))
     | cvtSTATEMENT (ForStmt x550) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x550))
     | cvtSTATEMENT (IfStmt{cnd=x553, thn=x554, els=x555}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x553), ("thn", cvtSTATEMENT x554), 
          ("els", cvtSTATEMENT x555)]))
     | cvtSTATEMENT (WithStmt{obj=x565, ty=x566, body=x567}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x565), ("ty", cvtTYPE x566), 
          ("body", cvtSTATEMENT x567)]))
     | cvtSTATEMENT (TryStmt{block=x577, catches=ls579, finally=opt584}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x577), 
          ("catches", PrettyRep.List (List.map (fn x578 => cvtCATCH_CLAUSE x578
                                               ) ls579)), ("finally", 
       (case opt584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x583 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x583))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x597, labels=ls599, cases=ls604}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x597), ("labels", PrettyRep.List (List.map (fn x598 => 
                                                                                                        cvtIDENTIFIER x598
                                                                                                 ) ls599)), 
          ("cases", PrettyRep.List (List.map (fn x603 => cvtCASE x603
                                             ) ls604))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x617, ty=x618, cases=ls620}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x617), ("ty", cvtTYPE x618), 
          ("cases", PrettyRep.List (List.map (fn x619 => cvtCATCH_CLAUSE x619
                                             ) ls620))]))
     | cvtSTATEMENT (DXNStmt{expr=x633}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x633)]))
   and cvtEXPRESSION (TernaryExpr(x639, x640, x641)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x639, cvtEXPRESSION x640, cvtEXPRESSION x641]))
     | cvtEXPRESSION (BinaryExpr(x645, x646, x647)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x645, cvtEXPRESSION x646, cvtEXPRESSION x647]))
     | cvtEXPRESSION (BinaryTypeExpr(x651, x652, x653)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x651, cvtEXPRESSION x652, cvtTYPE x653]))
     | cvtEXPRESSION (UnaryExpr(x657, x658)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x657, cvtEXPRESSION x658]))
     | cvtEXPRESSION (TypeExpr x662) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x662))
     | cvtEXPRESSION (ThisExpr opt666) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt666 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x665 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x665))
       ))
     | cvtEXPRESSION (YieldExpr opt673) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt673 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x672 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x672))
       ))
     | cvtEXPRESSION (SuperExpr opt680) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt680 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x679 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x679))
       ))
     | cvtEXPRESSION (CallExpr{func=x686, actuals=ls688}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x686), ("actuals", PrettyRep.List (List.map (fn x687 => 
                                                                                                         cvtEXPRESSION x687
                                                                                                  ) ls688))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x699, actuals=ls701}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x699), ("actuals", PrettyRep.List (List.map (fn x700 => 
                                                                                                         cvtTYPE x700
                                                                                                  ) ls701))]))
     | cvtEXPRESSION (LetExpr{defs=x712, body=x713, head=opt715}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x712), ("body", cvtEXPRESSION x713), 
          ("head", 
       (case opt715 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x714 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x714))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x728, actuals=ls730}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x728), ("actuals", PrettyRep.List (List.map (fn x729 => 
                                                                                                        cvtEXPRESSION x729
                                                                                                 ) ls730))]))
     | cvtEXPRESSION (SetExpr(x741, x742, x743)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x741, cvtEXPRESSION x742, cvtEXPRESSION x743]))
     | cvtEXPRESSION (ListExpr ls748) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x747 => 
                                                                                                          cvtEXPRESSION x747
                                                                                                   ) ls748)))
     | cvtEXPRESSION (InitExpr(x754, x755, x756)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x754, cvtHEAD x755, cvtINITS x756]))
     | cvtEXPRESSION (GetTemp n760) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n760))
     | cvtEXPRESSION (GetParam n763) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n763))
     | cvtEXPRESSION (Comprehension(x766, ls768, opt773)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x766, PrettyRep.List (List.map (fn x767 => 
                                                                                     cvtFOR_ENUM_HEAD x767
                                                                              ) ls768), 
          
       (case opt773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x772 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x772))
       )]))
     | cvtEXPRESSION (LiteralExpr x780) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x780))
     | cvtEXPRESSION (LexicalReference{name=x783, loc=opt785}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x783), ("loc", 
       (case opt785 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x784 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x784))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x796, name=x797, loc=opt799}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x796), ("name", cvtNAME_EXPRESSION x797), ("loc", 
       (case opt799 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x798 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x798))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x812, index=x813, loc=opt815}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x812), ("index", cvtEXPRESSION x813), ("loc", 
       (case opt815 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x814 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x814))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n833) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n833))
     | cvtFIXTURE_NAME (PropName x836) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x836))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r841) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r841))
     | cvtLITERAL (LiteralDecimal d844) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d844))
     | cvtLITERAL (LiteralBoolean b847) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b847))
     | cvtLITERAL (LiteralString s850) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s850))
     | cvtLITERAL (LiteralArray{exprs=x853, ty=opt855}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x853), ("ty", 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x854))
       ))]))
     | cvtLITERAL (LiteralXML ls867) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x866 => 
                                                                                                           cvtEXPRESSION x866
                                                                                                    ) ls867)))
     | cvtLITERAL (LiteralNamespace x873) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x873))
     | cvtLITERAL (LiteralObject{expr=ls877, ty=opt882}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x876 => 
                                                                        cvtFIELD x876
                                                                 ) ls877)), 
          ("ty", 
       (case opt882 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x881 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x881))
       ))]))
     | cvtLITERAL (LiteralFunction x893) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x893))
     | cvtLITERAL (LiteralRegExp{str=s896}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s896)]))
   and cvtBLOCK (Block x902) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x902))
   and cvtFIXTURE (NamespaceFixture x905) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x905))
     | cvtFIXTURE (ClassFixture x908) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x908))
     | cvtFIXTURE (InterfaceFixture x911) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x911))
     | cvtFIXTURE (TypeVarFixture x914) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x914))
     | cvtFIXTURE (TypeFixture(ls918, x922)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x917 => cvtIDENTIFIER x917
                                                          ) ls918), cvtTYPE x922]))
     | cvtFIXTURE (MethodFixture{func=x926, ty=x927, readOnly=b928, override=b929, 
          final=b930}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x926), ("ty", cvtTYPE x927), ("readOnly", PrettyRep.Bool b928), 
          ("override", PrettyRep.Bool b929), ("final", PrettyRep.Bool b930)]))
     | cvtFIXTURE (ValFixture{ty=x944, readOnly=b945}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x944), ("readOnly", PrettyRep.Bool b945)]))
     | cvtFIXTURE (VirtualValFixture{ty=x953, getter=opt955, setter=opt960}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x953), ("getter", 
       (case opt955 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x954 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x954))
       )), ("setter", 
       (case opt960 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x959 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x959))
       ))]))
   and cvtHEAD (Head(x973, x974)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x973, 
          cvtINITS x974]))
   and cvtBINDINGS (ls979, ls984) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x978 => 
                                                                                     cvtBINDING x978
                                                                              ) ls979), 
          PrettyRep.List (List.map (fn x983 => cvtINIT_STEP x983
                                   ) ls984)]
   and cvtRIB ls992 = PrettyRep.List (List.map (fn (x989, x990) => PrettyRep.Tuple [cvtFIXTURE_NAME x989, 
                                                      cvtFIXTURE x990]
                                               ) ls992)
   and cvtRIBS ls1003 = PrettyRep.List (List.map (fn ls999 => PrettyRep.List (List.map (fn (x996, 
                                                                                              x997) => 
                                                                                              PrettyRep.Tuple [cvtFIXTURE_NAME x996, 
                                                                                              cvtFIXTURE x997]
                                                                                       ) ls999)
                                                 ) ls1003)
   and cvtINITS ls1010 = PrettyRep.List (List.map (fn (x1007, x1008) => PrettyRep.Tuple [cvtFIXTURE_NAME x1007, 
                                                         cvtEXPRESSION x1008]
                                                  ) ls1010)
   and cvtINSTANCE_TYPE {name=x1014, typeParams=ls1016, typeArgs=ls1021, nonnullable=b1025, 
          superTypes=ls1027, ty=x1031, dynamic=b1032} = PrettyRep.Rec [("name", 
          cvtNAME x1014), ("typeParams", PrettyRep.List (List.map (fn x1015 => 
                                                                         cvtIDENTIFIER x1015
                                                                  ) ls1016)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1020 => cvtTYPE x1020
                                                ) ls1021)), ("nonnullable", 
          PrettyRep.Bool b1025), ("superTypes", PrettyRep.List (List.map (fn x1026 => 
                                                                                cvtTYPE x1026
                                                                         ) ls1027)), 
          ("ty", cvtTYPE x1031), ("dynamic", PrettyRep.Bool b1032)]
   and cvtFIELD {kind=x1048, name=x1049, init=x1050} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1048), ("name", cvtNAME_EXPRESSION x1049), ("init", 
          cvtEXPRESSION x1050)]
   and cvtFIELD_TYPE {name=x1058, ty=x1059} = PrettyRep.Rec [("name", cvtNAME_EXPRESSION x1058), 
          ("ty", cvtTYPE x1059)]
   and cvtFUNC_TYPE {typeParams=ls1066, thisType=x1070, params=ls1072, hasRest=b1076, 
          minArgs=n1077, result=x1078} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1065 => 
                                                                                                       cvtIDENTIFIER x1065
                                                                                                ) ls1066)), 
          ("thisType", cvtTYPE x1070), ("params", PrettyRep.List (List.map (fn x1071 => 
                                                                                  cvtTYPE x1071
                                                                           ) ls1072)), 
          ("hasRest", PrettyRep.Bool b1076), ("minArgs", PrettyRep.Int n1077), 
          ("result", cvtTYPE x1078)]
   and cvtFUNC_DEFN {kind=x1092, ns=opt1094, final=b1098, override=b1099, prototype=b1100, 
          static=b1101, func=x1102} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1092), 
          ("ns", 
       (case opt1094 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1093 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1093))
       )), ("final", PrettyRep.Bool b1098), ("override", PrettyRep.Bool b1099), 
          ("prototype", PrettyRep.Bool b1100), ("static", PrettyRep.Bool b1101), 
          ("func", cvtFUNC x1102)]
   and cvtCTOR_DEFN x1118 = cvtCTOR x1118
   and cvtVAR_DEFN {kind=x1119, ns=opt1121, static=b1125, prototype=b1126, 
          bindings=(ls1128, ls1133)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1119), 
          ("ns", 
       (case opt1121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1120 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1120))
       )), ("static", PrettyRep.Bool b1125), ("prototype", PrettyRep.Bool b1126), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1127 => 
                                                                        cvtBINDING x1127
                                                                 ) ls1128), 
          PrettyRep.List (List.map (fn x1132 => cvtINIT_STEP x1132
                                   ) ls1133)])]
   and cvtNAMESPACE_DEFN {ident=x1149, ns=opt1151, init=opt1156} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1149), ("ns", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1150))
       )), ("init", 
       (case opt1156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1155 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1155))
       ))]
   and cvtCLASS_DEFN {ns=opt1168, privateNS=x1172, protectedNS=x1173, ident=x1174, 
          nonnullable=b1175, dynamic=b1176, final=b1177, params=ls1179, extends=opt1184, 
          implements=ls1189, classDefns=ls1194, instanceDefns=ls1199, instanceStmts=ls1204, 
          ctorDefn=opt1209} = PrettyRep.Rec [("ns", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1167))
       )), ("privateNS", cvtNAMESPACE x1172), ("protectedNS", cvtNAMESPACE x1173), 
          ("ident", cvtIDENTIFIER x1174), ("nonnullable", PrettyRep.Bool b1175), 
          ("dynamic", PrettyRep.Bool b1176), ("final", PrettyRep.Bool b1177), 
          ("params", PrettyRep.List (List.map (fn x1178 => cvtIDENTIFIER x1178
                                              ) ls1179)), ("extends", 
       (case opt1184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1183 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1183))
       )), ("implements", PrettyRep.List (List.map (fn x1188 => cvtTYPE x1188
                                                   ) ls1189)), ("classDefns", 
          PrettyRep.List (List.map (fn x1193 => cvtDEFN x1193
                                   ) ls1194)), ("instanceDefns", PrettyRep.List (List.map (fn x1198 => 
                                                                                                 cvtDEFN x1198
                                                                                          ) ls1199)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1203 => cvtSTATEMENT x1203
                                                     ) ls1204)), ("ctorDefn", 
          
       (case opt1209 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1208 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1208))
       ))]
   and cvtINTERFACE_DEFN {ident=x1242, ns=opt1244, nonnullable=b1248, params=ls1250, 
          extends=ls1255, instanceDefns=ls1260} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1242), ("ns", 
       (case opt1244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1243 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1243))
       )), ("nonnullable", PrettyRep.Bool b1248), ("params", PrettyRep.List (List.map (fn x1249 => 
                                                                                             cvtIDENTIFIER x1249
                                                                                      ) ls1250)), 
          ("extends", PrettyRep.List (List.map (fn x1254 => cvtTYPE x1254
                                               ) ls1255)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1259 => cvtDEFN x1259
                                   ) ls1260))]
   and cvtTYPE_DEFN {ident=x1277, ns=opt1279, typeParams=ls1284, init=x1288} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1277), ("ns", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1278))
       )), ("typeParams", PrettyRep.List (List.map (fn x1283 => cvtIDENTIFIER x1283
                                                   ) ls1284)), ("init", cvtTYPE x1288)]
   and cvtCLASS_BLOCK {ns=opt1299, protectedNS=x1303, privateNS=x1304, ident=x1305, 
          name=opt1307, block=x1311} = PrettyRep.Rec [("ns", 
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1298))
       )), ("protectedNS", cvtNAMESPACE x1303), ("privateNS", cvtNAMESPACE x1304), 
          ("ident", cvtIDENTIFIER x1305), ("name", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1306))
       )), ("block", cvtBLOCK x1311)]
   and cvtFOR_ENUM_HEAD {isEach=b1325, bindings=(ls1327, ls1332), expr=x1337} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1325), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1326 => 
                                                                                                                         cvtBINDING x1326
                                                                                                                  ) ls1327), 
          PrettyRep.List (List.map (fn x1331 => cvtINIT_STEP x1331
                                   ) ls1332)]), ("expr", cvtEXPRESSION x1337)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1345, defn=opt1376, obj=x1380, rib=opt1388, 
          next=x1392, labels=ls1394, body=x1398} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1345), ("defn", 
       (case opt1376 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1346, ns=opt1348, static=b1352, prototype=b1353, bindings=(ls1355, 
            ls1360)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1346), ("ns", 
         (case opt1348 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1347 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1347))
         )), ("static", PrettyRep.Bool b1352), ("prototype", PrettyRep.Bool b1353), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1354 => 
                                                                          cvtBINDING x1354
                                                                   ) ls1355), 
            PrettyRep.List (List.map (fn x1359 => cvtINIT_STEP x1359
                                     ) ls1360)])]))
       )), ("obj", cvtEXPRESSION x1380), ("rib", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1384 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1381, 
                                                                                      x1382) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1381, 
                                                                                      cvtFIXTURE x1382]
                                                                               ) ls1384)))
       )), ("next", cvtSTATEMENT x1392), ("labels", PrettyRep.List (List.map (fn x1393 => 
                                                                                    cvtIDENTIFIER x1393
                                                                             ) ls1394)), 
          ("body", cvtSTATEMENT x1398)]
   and cvtFOR_STATEMENT {rib=opt1421, defn=opt1455, init=ls1460, cond=x1464, 
          update=x1465, labels=ls1467, body=x1471} = PrettyRep.Rec [("rib", 
          
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1417 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1414, 
                                                                                      x1415) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1414, 
                                                                                      cvtFIXTURE x1415]
                                                                               ) ls1417)))
       )), ("defn", 
       (case opt1455 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1425, ns=opt1427, static=b1431, prototype=b1432, bindings=(ls1434, 
            ls1439)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1425), ("ns", 
         (case opt1427 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1426))
         )), ("static", PrettyRep.Bool b1431), ("prototype", PrettyRep.Bool b1432), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1433 => 
                                                                          cvtBINDING x1433
                                                                   ) ls1434), 
            PrettyRep.List (List.map (fn x1438 => cvtINIT_STEP x1438
                                     ) ls1439)])]))
       )), ("init", PrettyRep.List (List.map (fn x1459 => cvtSTATEMENT x1459
                                             ) ls1460)), ("cond", cvtEXPRESSION x1464), 
          ("update", cvtEXPRESSION x1465), ("labels", PrettyRep.List (List.map (fn x1466 => 
                                                                                      cvtIDENTIFIER x1466
                                                                               ) ls1467)), 
          ("body", cvtSTATEMENT x1471)]
   and cvtWHILE_STATEMENT {cond=x1487, rib=opt1495, body=x1499, labels=ls1501} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1487), ("rib", 
       (case opt1495 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1491 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1488, 
                                                                                      x1489) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1488, 
                                                                                      cvtFIXTURE x1489]
                                                                               ) ls1491)))
       )), ("body", cvtSTATEMENT x1499), ("labels", PrettyRep.List (List.map (fn x1500 => 
                                                                                    cvtIDENTIFIER x1500
                                                                             ) ls1501))]
   and cvtDIRECTIVES {pragmas=ls1515, defns=ls1520, head=opt1525, body=ls1530, 
          loc=opt1535} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1514 => 
                                                                                    cvtPRAGMA x1514
                                                                             ) ls1515)), 
          ("defns", PrettyRep.List (List.map (fn x1519 => cvtDEFN x1519
                                             ) ls1520)), ("head", 
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1524 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1524))
       )), ("body", PrettyRep.List (List.map (fn x1529 => cvtSTATEMENT x1529
                                             ) ls1530)), ("loc", 
       (case opt1535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1534 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1534))
       ))]
   and cvtCASE {label=opt1551, inits=opt1562, body=x1566} = PrettyRep.Rec [("label", 
          
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1550))
       )), ("inits", 
       (case opt1562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1558 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1555, 
                                                                                      x1556) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1555, 
                                                                                      cvtEXPRESSION x1556]
                                                                               ) ls1558)))
       )), ("body", cvtBLOCK x1566)]
   and cvtCATCH_CLAUSE {bindings=(ls1575, ls1580), ty=x1585, rib=opt1593, inits=opt1604, 
          block=x1608} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1574 => 
                                                                                                      cvtBINDING x1574
                                                                                               ) ls1575), 
          PrettyRep.List (List.map (fn x1579 => cvtINIT_STEP x1579
                                   ) ls1580)]), ("ty", cvtTYPE x1585), ("rib", 
          
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1589 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1586, 
                                                                                      x1587) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1586, 
                                                                                      cvtFIXTURE x1587]
                                                                               ) ls1589)))
       )), ("inits", 
       (case opt1604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1600 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1597, 
                                                                                      x1598) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1597, 
                                                                                      cvtEXPRESSION x1598]
                                                                               ) ls1600)))
       )), ("block", cvtBLOCK x1608)]
   and cvtFUNC_NAME {kind=x1620, ident=x1621} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1620), 
          ("ident", cvtIDENTIFIER x1621)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1627, getter=opt1629, setter=opt1634} = 
          PrettyRep.Rec [("ty", cvtTYPE x1627), ("getter", 
       (case opt1629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1628 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1628))
       )), ("setter", 
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1633 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1633))
       ))]
   and cvtFRAGMENT (Anon x1645) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1645))
end

