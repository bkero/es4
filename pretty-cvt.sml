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
     | cvtTYPE (NullableType{expr=x465, nullable=b466}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE x465), ("nullable", PrettyRep.Bool b466)]))
     | cvtTYPE (InstanceType x474) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x474))
     | cvtTYPE (TypeVarFixtureRef x477) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x477))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x481) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x481))
     | cvtSTATEMENT (InitStmt{kind=x484, ns=opt486, prototype=b490, static=b491, 
          temps=x492, inits=ls494}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x484), ("ns", 
       (case opt486 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x485 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x485))
       )), ("prototype", PrettyRep.Bool b490), ("static", PrettyRep.Bool b491), 
          ("temps", cvtBINDINGS x492), ("inits", PrettyRep.List (List.map (fn x493 => 
                                                                                 cvtINIT_STEP x493
                                                                          ) ls494))]))
     | cvtSTATEMENT (ClassBlock x513) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x513))
     | cvtSTATEMENT (ForInStmt x516) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x516))
     | cvtSTATEMENT (ThrowStmt x519) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x519))
     | cvtSTATEMENT (ReturnStmt x522) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x522))
     | cvtSTATEMENT (BreakStmt opt526) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt526 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x525 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x525))
       ))
     | cvtSTATEMENT (ContinueStmt opt533) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x532 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x532))
       ))
     | cvtSTATEMENT (BlockStmt x539) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x539))
     | cvtSTATEMENT (LabeledStmt(x542, x543)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x542, cvtSTATEMENT x543]))
     | cvtSTATEMENT (LetStmt x547) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x547))
     | cvtSTATEMENT (WhileStmt x550) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x550))
     | cvtSTATEMENT (DoWhileStmt x553) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x553))
     | cvtSTATEMENT (ForStmt x556) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x556))
     | cvtSTATEMENT (IfStmt{cnd=x559, thn=x560, els=x561}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x559), ("thn", cvtSTATEMENT x560), 
          ("els", cvtSTATEMENT x561)]))
     | cvtSTATEMENT (WithStmt{obj=x571, ty=x572, body=x573}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x571), ("ty", cvtTYPE x572), 
          ("body", cvtSTATEMENT x573)]))
     | cvtSTATEMENT (TryStmt{block=x583, catches=ls585, finally=opt590}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x583), 
          ("catches", PrettyRep.List (List.map (fn x584 => cvtCATCH_CLAUSE x584
                                               ) ls585)), ("finally", 
       (case opt590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x589 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x589))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x603, labels=ls605, cases=ls610}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x603), ("labels", PrettyRep.List (List.map (fn x604 => 
                                                                                                        cvtIDENTIFIER x604
                                                                                                 ) ls605)), 
          ("cases", PrettyRep.List (List.map (fn x609 => cvtCASE x609
                                             ) ls610))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x623, ty=x624, cases=ls626}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x623), ("ty", cvtTYPE x624), 
          ("cases", PrettyRep.List (List.map (fn x625 => cvtCATCH_CLAUSE x625
                                             ) ls626))]))
     | cvtSTATEMENT (DXNStmt{expr=x639}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x639)]))
   and cvtEXPRESSION (TernaryExpr(x645, x646, x647)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x645, cvtEXPRESSION x646, cvtEXPRESSION x647]))
     | cvtEXPRESSION (BinaryExpr(x651, x652, x653)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x651, cvtEXPRESSION x652, cvtEXPRESSION x653]))
     | cvtEXPRESSION (BinaryTypeExpr(x657, x658, x659)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x657, cvtEXPRESSION x658, cvtTYPE x659]))
     | cvtEXPRESSION (UnaryExpr(x663, x664)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x663, cvtEXPRESSION x664]))
     | cvtEXPRESSION (TypeExpr x668) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x668))
     | cvtEXPRESSION (ThisExpr opt672) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x671))
       ))
     | cvtEXPRESSION (YieldExpr opt679) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x678 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x678))
       ))
     | cvtEXPRESSION (SuperExpr opt686) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt686 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x685 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x685))
       ))
     | cvtEXPRESSION (CallExpr{func=x692, actuals=ls694}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x692), ("actuals", PrettyRep.List (List.map (fn x693 => 
                                                                                                         cvtEXPRESSION x693
                                                                                                  ) ls694))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x705, actuals=ls707}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x705), ("actuals", PrettyRep.List (List.map (fn x706 => 
                                                                                                         cvtTYPE x706
                                                                                                  ) ls707))]))
     | cvtEXPRESSION (LetExpr{defs=x718, body=x719, head=opt721}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x718), ("body", cvtEXPRESSION x719), 
          ("head", 
       (case opt721 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x720 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x720))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x734, actuals=ls736}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x734), ("actuals", PrettyRep.List (List.map (fn x735 => 
                                                                                                        cvtEXPRESSION x735
                                                                                                 ) ls736))]))
     | cvtEXPRESSION (SetExpr(x747, x748, x749)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x747, cvtEXPRESSION x748, cvtEXPRESSION x749]))
     | cvtEXPRESSION (ListExpr ls754) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x753 => 
                                                                                                          cvtEXPRESSION x753
                                                                                                   ) ls754)))
     | cvtEXPRESSION (InitExpr(x760, x761, x762)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x760, cvtHEAD x761, cvtINITS x762]))
     | cvtEXPRESSION (GetTemp n766) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n766))
     | cvtEXPRESSION (GetParam n769) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n769))
     | cvtEXPRESSION (Comprehension(x772, ls774, opt779)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x772, PrettyRep.List (List.map (fn x773 => 
                                                                                     cvtFOR_ENUM_HEAD x773
                                                                              ) ls774), 
          
       (case opt779 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x778 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x778))
       )]))
     | cvtEXPRESSION (LiteralExpr x786) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x786))
     | cvtEXPRESSION (LexicalReference{name=x789, loc=opt791}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x789), ("loc", 
       (case opt791 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x790 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x790))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x802, name=x803, loc=opt805}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x802), ("name", cvtNAME_EXPRESSION x803), ("loc", 
       (case opt805 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x804 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x804))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x818, index=x819, loc=opt821}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x818), ("index", cvtEXPRESSION x819), ("loc", 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x820))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n839) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n839))
     | cvtFIXTURE_NAME (PropName x842) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x842))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r847) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r847))
     | cvtLITERAL (LiteralDecimal d850) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d850))
     | cvtLITERAL (LiteralBoolean b853) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b853))
     | cvtLITERAL (LiteralString s856) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s856))
     | cvtLITERAL (LiteralArray{exprs=x859, ty=opt861}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x859), ("ty", 
       (case opt861 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x860 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x860))
       ))]))
     | cvtLITERAL (LiteralXML ls873) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x872 => 
                                                                                                           cvtEXPRESSION x872
                                                                                                    ) ls873)))
     | cvtLITERAL (LiteralNamespace x879) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x879))
     | cvtLITERAL (LiteralObject{expr=ls883, ty=opt888}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x882 => 
                                                                        cvtFIELD x882
                                                                 ) ls883)), 
          ("ty", 
       (case opt888 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x887 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x887))
       ))]))
     | cvtLITERAL (LiteralFunction x899) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x899))
     | cvtLITERAL (LiteralRegExp{str=s902}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s902)]))
   and cvtBLOCK (Block x908) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x908))
   and cvtFIXTURE (NamespaceFixture x911) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x911))
     | cvtFIXTURE (ClassFixture x914) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x914))
     | cvtFIXTURE (InterfaceFixture x917) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x917))
     | cvtFIXTURE (TypeVarFixture x920) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x920))
     | cvtFIXTURE (TypeFixture(ls924, x928)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x923 => cvtIDENTIFIER x923
                                                          ) ls924), cvtTYPE x928]))
     | cvtFIXTURE (MethodFixture{func=x932, ty=x933, readOnly=b934, override=b935, 
          final=b936}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x932), ("ty", cvtTYPE x933), ("readOnly", PrettyRep.Bool b934), 
          ("override", PrettyRep.Bool b935), ("final", PrettyRep.Bool b936)]))
     | cvtFIXTURE (ValFixture{ty=x950, readOnly=b951}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x950), ("readOnly", PrettyRep.Bool b951)]))
     | cvtFIXTURE (VirtualValFixture{ty=x959, getter=opt961, setter=opt966}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x959), ("getter", 
       (case opt961 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x960 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x960))
       )), ("setter", 
       (case opt966 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x965 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x965))
       ))]))
   and cvtHEAD (Head(x979, x980)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x979, 
          cvtINITS x980]))
   and cvtBINDINGS (ls985, ls990) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x984 => 
                                                                                     cvtBINDING x984
                                                                              ) ls985), 
          PrettyRep.List (List.map (fn x989 => cvtINIT_STEP x989
                                   ) ls990)]
   and cvtRIB ls998 = PrettyRep.List (List.map (fn (x995, x996) => PrettyRep.Tuple [cvtFIXTURE_NAME x995, 
                                                      cvtFIXTURE x996]
                                               ) ls998)
   and cvtRIBS ls1009 = PrettyRep.List (List.map (fn ls1005 => PrettyRep.List (List.map (fn (x1002, 
                                                                                               x1003) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1002, 
                                                                                               cvtFIXTURE x1003]
                                                                                        ) ls1005)
                                                 ) ls1009)
   and cvtINITS ls1016 = PrettyRep.List (List.map (fn (x1013, x1014) => PrettyRep.Tuple [cvtFIXTURE_NAME x1013, 
                                                         cvtEXPRESSION x1014]
                                                  ) ls1016)
   and cvtINSTANCE_TYPE {name=x1020, typeParams=ls1022, typeArgs=ls1027, nonnullable=b1031, 
          superTypes=ls1033, ty=x1037, dynamic=b1038} = PrettyRep.Rec [("name", 
          cvtNAME x1020), ("typeParams", PrettyRep.List (List.map (fn x1021 => 
                                                                         cvtIDENTIFIER x1021
                                                                  ) ls1022)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1026 => cvtTYPE x1026
                                                ) ls1027)), ("nonnullable", 
          PrettyRep.Bool b1031), ("superTypes", PrettyRep.List (List.map (fn x1032 => 
                                                                                cvtTYPE x1032
                                                                         ) ls1033)), 
          ("ty", cvtTYPE x1037), ("dynamic", PrettyRep.Bool b1038)]
   and cvtFIELD {kind=x1054, name=x1055, init=x1056} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1054), ("name", cvtNAME_EXPRESSION x1055), ("init", 
          cvtEXPRESSION x1056)]
   and cvtFIELD_TYPE {name=x1064, ty=x1065} = PrettyRep.Rec [("name", cvtNAME_EXPRESSION x1064), 
          ("ty", cvtTYPE x1065)]
   and cvtFUNC_TYPE {typeParams=ls1072, thisType=x1076, params=ls1078, hasRest=b1082, 
          minArgs=n1083, result=x1084} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1071 => 
                                                                                                       cvtIDENTIFIER x1071
                                                                                                ) ls1072)), 
          ("thisType", cvtTYPE x1076), ("params", PrettyRep.List (List.map (fn x1077 => 
                                                                                  cvtTYPE x1077
                                                                           ) ls1078)), 
          ("hasRest", PrettyRep.Bool b1082), ("minArgs", PrettyRep.Int n1083), 
          ("result", cvtTYPE x1084)]
   and cvtFUNC_DEFN {kind=x1098, ns=opt1100, final=b1104, override=b1105, prototype=b1106, 
          static=b1107, func=x1108} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1098), 
          ("ns", 
       (case opt1100 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1099 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1099))
       )), ("final", PrettyRep.Bool b1104), ("override", PrettyRep.Bool b1105), 
          ("prototype", PrettyRep.Bool b1106), ("static", PrettyRep.Bool b1107), 
          ("func", cvtFUNC x1108)]
   and cvtCTOR_DEFN x1124 = cvtCTOR x1124
   and cvtVAR_DEFN {kind=x1125, ns=opt1127, static=b1131, prototype=b1132, 
          bindings=(ls1134, ls1139)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1125), 
          ("ns", 
       (case opt1127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1126 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1126))
       )), ("static", PrettyRep.Bool b1131), ("prototype", PrettyRep.Bool b1132), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1133 => 
                                                                        cvtBINDING x1133
                                                                 ) ls1134), 
          PrettyRep.List (List.map (fn x1138 => cvtINIT_STEP x1138
                                   ) ls1139)])]
   and cvtNAMESPACE_DEFN {ident=x1155, ns=opt1157, init=opt1162} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1155), ("ns", 
       (case opt1157 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1156 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1156))
       )), ("init", 
       (case opt1162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1161 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1161))
       ))]
   and cvtCLASS_DEFN {ns=opt1174, privateNS=x1178, protectedNS=x1179, ident=x1180, 
          nonnullable=b1181, dynamic=b1182, final=b1183, params=ls1185, extends=opt1190, 
          implements=ls1195, classDefns=ls1200, instanceDefns=ls1205, instanceStmts=ls1210, 
          ctorDefn=opt1215} = PrettyRep.Rec [("ns", 
       (case opt1174 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1173 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1173))
       )), ("privateNS", cvtNAMESPACE x1178), ("protectedNS", cvtNAMESPACE x1179), 
          ("ident", cvtIDENTIFIER x1180), ("nonnullable", PrettyRep.Bool b1181), 
          ("dynamic", PrettyRep.Bool b1182), ("final", PrettyRep.Bool b1183), 
          ("params", PrettyRep.List (List.map (fn x1184 => cvtIDENTIFIER x1184
                                              ) ls1185)), ("extends", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1189 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1189))
       )), ("implements", PrettyRep.List (List.map (fn x1194 => cvtTYPE x1194
                                                   ) ls1195)), ("classDefns", 
          PrettyRep.List (List.map (fn x1199 => cvtDEFN x1199
                                   ) ls1200)), ("instanceDefns", PrettyRep.List (List.map (fn x1204 => 
                                                                                                 cvtDEFN x1204
                                                                                          ) ls1205)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1209 => cvtSTATEMENT x1209
                                                     ) ls1210)), ("ctorDefn", 
          
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1214 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1214))
       ))]
   and cvtINTERFACE_DEFN {ident=x1248, ns=opt1250, nonnullable=b1254, params=ls1256, 
          extends=ls1261, instanceDefns=ls1266} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1248), ("ns", 
       (case opt1250 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1249 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1249))
       )), ("nonnullable", PrettyRep.Bool b1254), ("params", PrettyRep.List (List.map (fn x1255 => 
                                                                                             cvtIDENTIFIER x1255
                                                                                      ) ls1256)), 
          ("extends", PrettyRep.List (List.map (fn x1260 => cvtTYPE x1260
                                               ) ls1261)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1265 => cvtDEFN x1265
                                   ) ls1266))]
   and cvtTYPE_DEFN {ident=x1283, ns=opt1285, typeParams=ls1290, init=x1294} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1283), ("ns", 
       (case opt1285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1284 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1284))
       )), ("typeParams", PrettyRep.List (List.map (fn x1289 => cvtIDENTIFIER x1289
                                                   ) ls1290)), ("init", cvtTYPE x1294)]
   and cvtCLASS_BLOCK {ns=opt1305, protectedNS=x1309, privateNS=x1310, ident=x1311, 
          name=opt1313, block=x1317} = PrettyRep.Rec [("ns", 
       (case opt1305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1304 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1304))
       )), ("protectedNS", cvtNAMESPACE x1309), ("privateNS", cvtNAMESPACE x1310), 
          ("ident", cvtIDENTIFIER x1311), ("name", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1312))
       )), ("block", cvtBLOCK x1317)]
   and cvtFOR_ENUM_HEAD {isEach=b1331, bindings=(ls1333, ls1338), expr=x1343} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1331), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1332 => 
                                                                                                                         cvtBINDING x1332
                                                                                                                  ) ls1333), 
          PrettyRep.List (List.map (fn x1337 => cvtINIT_STEP x1337
                                   ) ls1338)]), ("expr", cvtEXPRESSION x1343)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1351, defn=opt1382, obj=x1386, rib=opt1394, 
          next=x1398, labels=ls1400, body=x1404} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1351), ("defn", 
       (case opt1382 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1352, ns=opt1354, static=b1358, prototype=b1359, bindings=(ls1361, 
            ls1366)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1352), ("ns", 
         (case opt1354 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1353 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1353))
         )), ("static", PrettyRep.Bool b1358), ("prototype", PrettyRep.Bool b1359), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1360 => 
                                                                          cvtBINDING x1360
                                                                   ) ls1361), 
            PrettyRep.List (List.map (fn x1365 => cvtINIT_STEP x1365
                                     ) ls1366)])]))
       )), ("obj", cvtEXPRESSION x1386), ("rib", 
       (case opt1394 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1390 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1387, 
                                                                                      x1388) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1387, 
                                                                                      cvtFIXTURE x1388]
                                                                               ) ls1390)))
       )), ("next", cvtSTATEMENT x1398), ("labels", PrettyRep.List (List.map (fn x1399 => 
                                                                                    cvtIDENTIFIER x1399
                                                                             ) ls1400)), 
          ("body", cvtSTATEMENT x1404)]
   and cvtFOR_STATEMENT {rib=opt1427, defn=opt1461, init=ls1466, cond=x1470, 
          update=x1471, labels=ls1473, body=x1477} = PrettyRep.Rec [("rib", 
          
       (case opt1427 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1423 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1420, 
                                                                                      x1421) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1420, 
                                                                                      cvtFIXTURE x1421]
                                                                               ) ls1423)))
       )), ("defn", 
       (case opt1461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1431, ns=opt1433, static=b1437, prototype=b1438, bindings=(ls1440, 
            ls1445)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1431), ("ns", 
         (case opt1433 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1432))
         )), ("static", PrettyRep.Bool b1437), ("prototype", PrettyRep.Bool b1438), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1439 => 
                                                                          cvtBINDING x1439
                                                                   ) ls1440), 
            PrettyRep.List (List.map (fn x1444 => cvtINIT_STEP x1444
                                     ) ls1445)])]))
       )), ("init", PrettyRep.List (List.map (fn x1465 => cvtSTATEMENT x1465
                                             ) ls1466)), ("cond", cvtEXPRESSION x1470), 
          ("update", cvtEXPRESSION x1471), ("labels", PrettyRep.List (List.map (fn x1472 => 
                                                                                      cvtIDENTIFIER x1472
                                                                               ) ls1473)), 
          ("body", cvtSTATEMENT x1477)]
   and cvtWHILE_STATEMENT {cond=x1493, rib=opt1501, body=x1505, labels=ls1507} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1493), ("rib", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1497 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1494, 
                                                                                      x1495) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1494, 
                                                                                      cvtFIXTURE x1495]
                                                                               ) ls1497)))
       )), ("body", cvtSTATEMENT x1505), ("labels", PrettyRep.List (List.map (fn x1506 => 
                                                                                    cvtIDENTIFIER x1506
                                                                             ) ls1507))]
   and cvtDIRECTIVES {pragmas=ls1521, defns=ls1526, head=opt1531, body=ls1536, 
          loc=opt1541} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1520 => 
                                                                                    cvtPRAGMA x1520
                                                                             ) ls1521)), 
          ("defns", PrettyRep.List (List.map (fn x1525 => cvtDEFN x1525
                                             ) ls1526)), ("head", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1530))
       )), ("body", PrettyRep.List (List.map (fn x1535 => cvtSTATEMENT x1535
                                             ) ls1536)), ("loc", 
       (case opt1541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1540 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1540))
       ))]
   and cvtCASE {label=opt1557, inits=opt1568, body=x1572} = PrettyRep.Rec [("label", 
          
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1556))
       )), ("inits", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1564 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1561, 
                                                                                      x1562) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1561, 
                                                                                      cvtEXPRESSION x1562]
                                                                               ) ls1564)))
       )), ("body", cvtBLOCK x1572)]
   and cvtCATCH_CLAUSE {bindings=(ls1581, ls1586), ty=x1591, rib=opt1599, inits=opt1610, 
          block=x1614} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1580 => 
                                                                                                      cvtBINDING x1580
                                                                                               ) ls1581), 
          PrettyRep.List (List.map (fn x1585 => cvtINIT_STEP x1585
                                   ) ls1586)]), ("ty", cvtTYPE x1591), ("rib", 
          
       (case opt1599 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1595 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1592, 
                                                                                      x1593) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1592, 
                                                                                      cvtFIXTURE x1593]
                                                                               ) ls1595)))
       )), ("inits", 
       (case opt1610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1606 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1603, 
                                                                                      x1604) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1603, 
                                                                                      cvtEXPRESSION x1604]
                                                                               ) ls1606)))
       )), ("block", cvtBLOCK x1614)]
   and cvtFUNC_NAME {kind=x1626, ident=x1627} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1626), 
          ("ident", cvtIDENTIFIER x1627)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1633, getter=opt1635, setter=opt1640} = 
          PrettyRep.Rec [("ty", cvtTYPE x1633), ("getter", 
       (case opt1635 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1634 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1634))
       )), ("setter", 
       (case opt1640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1639 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1639))
       ))]
   and cvtFRAGMENT (Anon x1651) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1651))
end

