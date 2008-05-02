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
          defaults=ls336, ctorInits=opt347, returnType=opt352, thisType=opt357, 
          hasRest=b361}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
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
       )), ("returnType", 
       (case opt352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x351 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x351))
       )), ("thisType", 
       (case opt357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x356 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x356))
       )), ("hasRest", PrettyRep.Bool b361)]))
   and cvtBINDING (Binding{ident=x381, ty=x382}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x381), ("ty", 
          cvtTYPE x382)]))
   and cvtBINDING_IDENTIFIER (TempIdent n390) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n390))
     | cvtBINDING_IDENTIFIER (ParamIdent n393) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n393))
     | cvtBINDING_IDENTIFIER (PropIdent x396) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x396))
   and cvtINIT_STEP (InitStep(x399, x400)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x399, 
          cvtEXPRESSION x400]))
     | cvtINIT_STEP (AssignStep(x404, x405)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x404, cvtEXPRESSION x405]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (RecordType ls413) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn x412 => 
                                                                                                        cvtFIELD_TYPE x412
                                                                                                 ) ls413)))
     | cvtTYPE (UnionType ls420) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x419 => 
                                                                                                      cvtTYPE x419
                                                                                               ) ls420)))
     | cvtTYPE (ArrayType ls427) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x426 => 
                                                                                                      cvtTYPE x426
                                                                                               ) ls427)))
     | cvtTYPE (FunctionType x433) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x433))
     | cvtTYPE (TypeName(x436, opt438)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x436, 
          
       (case opt438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x437 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x437))
       )]))
     | cvtTYPE (ElementTypeRef(x445, n446)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x445, PrettyRep.Int n446]))
     | cvtTYPE (FieldTypeRef(x450, x451)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x450, cvtNAME_EXPRESSION x451]))
     | cvtTYPE (AppType{base=x455, args=ls457}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x455), ("args", PrettyRep.List (List.map (fn x456 => 
                                                                                                cvtTYPE x456
                                                                                         ) ls457))]))
     | cvtTYPE (NonNullType x468) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x468))
     | cvtTYPE (InstanceType x471) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x471))
     | cvtTYPE (TypeVarFixtureRef x474) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x474))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x478) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x478))
     | cvtSTATEMENT (InitStmt{kind=x481, ns=opt483, prototype=b487, static=b488, 
          temps=x489, inits=ls491}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x481), ("ns", 
       (case opt483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x482 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x482))
       )), ("prototype", PrettyRep.Bool b487), ("static", PrettyRep.Bool b488), 
          ("temps", cvtBINDINGS x489), ("inits", PrettyRep.List (List.map (fn x490 => 
                                                                                 cvtINIT_STEP x490
                                                                          ) ls491))]))
     | cvtSTATEMENT (ClassBlock x510) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x510))
     | cvtSTATEMENT (ForInStmt x513) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x513))
     | cvtSTATEMENT (ThrowStmt x516) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x516))
     | cvtSTATEMENT (ReturnStmt x519) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x519))
     | cvtSTATEMENT (BreakStmt opt523) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x522 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x522))
       ))
     | cvtSTATEMENT (ContinueStmt opt530) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt530 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x529 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x529))
       ))
     | cvtSTATEMENT (BlockStmt x536) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x536))
     | cvtSTATEMENT (LabeledStmt(x539, x540)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x539, cvtSTATEMENT x540]))
     | cvtSTATEMENT (LetStmt x544) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x544))
     | cvtSTATEMENT (WhileStmt x547) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x547))
     | cvtSTATEMENT (DoWhileStmt x550) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x550))
     | cvtSTATEMENT (ForStmt x553) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x553))
     | cvtSTATEMENT (IfStmt{cnd=x556, thn=x557, els=x558}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x556), ("thn", cvtSTATEMENT x557), 
          ("els", cvtSTATEMENT x558)]))
     | cvtSTATEMENT (WithStmt{obj=x568, ty=x569, body=x570}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x568), ("ty", cvtTYPE x569), 
          ("body", cvtSTATEMENT x570)]))
     | cvtSTATEMENT (TryStmt{block=x580, catches=ls582, finally=opt587}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x580), 
          ("catches", PrettyRep.List (List.map (fn x581 => cvtCATCH_CLAUSE x581
                                               ) ls582)), ("finally", 
       (case opt587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x586 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x586))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x600, labels=ls602, cases=ls607}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x600), ("labels", PrettyRep.List (List.map (fn x601 => 
                                                                                                        cvtIDENTIFIER x601
                                                                                                 ) ls602)), 
          ("cases", PrettyRep.List (List.map (fn x606 => cvtCASE x606
                                             ) ls607))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x620, ty=x621, cases=ls623}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x620), ("ty", cvtTYPE x621), 
          ("cases", PrettyRep.List (List.map (fn x622 => cvtCATCH_CLAUSE x622
                                             ) ls623))]))
     | cvtSTATEMENT (DXNStmt{expr=x636}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x636)]))
   and cvtEXPRESSION (TernaryExpr(x642, x643, x644)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x642, cvtEXPRESSION x643, cvtEXPRESSION x644]))
     | cvtEXPRESSION (BinaryExpr(x648, x649, x650)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x648, cvtEXPRESSION x649, cvtEXPRESSION x650]))
     | cvtEXPRESSION (BinaryTypeExpr(x654, x655, x656)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x654, cvtEXPRESSION x655, cvtTYPE x656]))
     | cvtEXPRESSION (UnaryExpr(x660, x661)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x660, cvtEXPRESSION x661]))
     | cvtEXPRESSION (TypeExpr x665) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x665))
     | cvtEXPRESSION (ThisExpr opt669) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x668))
       ))
     | cvtEXPRESSION (YieldExpr opt676) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x675 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x675))
       ))
     | cvtEXPRESSION (SuperExpr opt683) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt683 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x682 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x682))
       ))
     | cvtEXPRESSION (CallExpr{func=x689, actuals=ls691}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x689), ("actuals", PrettyRep.List (List.map (fn x690 => 
                                                                                                         cvtEXPRESSION x690
                                                                                                  ) ls691))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x702, actuals=ls704}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x702), ("actuals", PrettyRep.List (List.map (fn x703 => 
                                                                                                         cvtTYPE x703
                                                                                                  ) ls704))]))
     | cvtEXPRESSION (LetExpr{defs=x715, body=x716, head=opt718}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x715), ("body", cvtEXPRESSION x716), 
          ("head", 
       (case opt718 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x717 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x717))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x731, actuals=ls733}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x731), ("actuals", PrettyRep.List (List.map (fn x732 => 
                                                                                                        cvtEXPRESSION x732
                                                                                                 ) ls733))]))
     | cvtEXPRESSION (SetExpr(x744, x745, x746)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x744, cvtEXPRESSION x745, cvtEXPRESSION x746]))
     | cvtEXPRESSION (ListExpr ls751) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x750 => 
                                                                                                          cvtEXPRESSION x750
                                                                                                   ) ls751)))
     | cvtEXPRESSION (InitExpr(x757, x758, x759)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x757, cvtHEAD x758, cvtINITS x759]))
     | cvtEXPRESSION (GetTemp n763) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n763))
     | cvtEXPRESSION (GetParam n766) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n766))
     | cvtEXPRESSION (Comprehension(x769, ls771, opt776)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x769, PrettyRep.List (List.map (fn x770 => 
                                                                                     cvtFOR_ENUM_HEAD x770
                                                                              ) ls771), 
          
       (case opt776 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x775 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x775))
       )]))
     | cvtEXPRESSION (LiteralExpr x783) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x783))
     | cvtEXPRESSION (LexicalReference{name=x786, loc=opt788}) = PrettyRep.Ctor ("LexicalReference", 
          SOME (PrettyRep.Rec [("name", cvtNAME_EXPRESSION x786), ("loc", 
       (case opt788 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x787 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x787))
       ))]))
     | cvtEXPRESSION (ObjectNameReference{object=x799, name=x800, loc=opt802}) = 
          PrettyRep.Ctor ("ObjectNameReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x799), ("name", cvtNAME_EXPRESSION x800), ("loc", 
       (case opt802 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x801 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x801))
       ))]))
     | cvtEXPRESSION (ObjectIndexReference{object=x815, index=x816, loc=opt818}) = 
          PrettyRep.Ctor ("ObjectIndexReference", SOME (PrettyRep.Rec [("object", 
          cvtEXPRESSION x815), ("index", cvtEXPRESSION x816), ("loc", 
       (case opt818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x817 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x817))
       ))]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n836) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n836))
     | cvtFIXTURE_NAME (PropName x839) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x839))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r844) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r844))
     | cvtLITERAL (LiteralDecimal d847) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d847))
     | cvtLITERAL (LiteralBoolean b850) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b850))
     | cvtLITERAL (LiteralString s853) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s853))
     | cvtLITERAL (LiteralArray{exprs=x856, ty=opt858}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x856), ("ty", 
       (case opt858 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x857 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x857))
       ))]))
     | cvtLITERAL (LiteralXML ls870) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x869 => 
                                                                                                           cvtEXPRESSION x869
                                                                                                    ) ls870)))
     | cvtLITERAL (LiteralNamespace x876) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x876))
     | cvtLITERAL (LiteralObject{expr=ls880, ty=opt885}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x879 => 
                                                                        cvtFIELD x879
                                                                 ) ls880)), 
          ("ty", 
       (case opt885 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x884 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x884))
       ))]))
     | cvtLITERAL (LiteralFunction x896) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x896))
     | cvtLITERAL (LiteralRegExp{str=s899}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s899)]))
   and cvtBLOCK (Block x905) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x905))
   and cvtFIXTURE (NamespaceFixture x908) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x908))
     | cvtFIXTURE (ClassFixture x911) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x911))
     | cvtFIXTURE (InterfaceFixture x914) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x914))
     | cvtFIXTURE (TypeVarFixture x917) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x917))
     | cvtFIXTURE (TypeFixture(ls921, x925)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x920 => cvtIDENTIFIER x920
                                                          ) ls921), cvtTYPE x925]))
     | cvtFIXTURE (MethodFixture{func=x929, ty=x930, readOnly=b931, override=b932, 
          final=b933}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x929), ("ty", cvtTYPE x930), ("readOnly", PrettyRep.Bool b931), 
          ("override", PrettyRep.Bool b932), ("final", PrettyRep.Bool b933)]))
     | cvtFIXTURE (ValFixture{ty=x947, readOnly=b948}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x947), ("readOnly", PrettyRep.Bool b948)]))
     | cvtFIXTURE (VirtualValFixture{ty=x956, getter=opt958, setter=opt963}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x956), ("getter", 
       (case opt958 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x957 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x957))
       )), ("setter", 
       (case opt963 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x962 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x962))
       ))]))
   and cvtHEAD (Head(x976, x977)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x976, 
          cvtINITS x977]))
   and cvtBINDINGS (ls982, ls987) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x981 => 
                                                                                     cvtBINDING x981
                                                                              ) ls982), 
          PrettyRep.List (List.map (fn x986 => cvtINIT_STEP x986
                                   ) ls987)]
   and cvtRIB ls995 = PrettyRep.List (List.map (fn (x992, x993) => PrettyRep.Tuple [cvtFIXTURE_NAME x992, 
                                                      cvtFIXTURE x993]
                                               ) ls995)
   and cvtRIBS ls1006 = PrettyRep.List (List.map (fn ls1002 => PrettyRep.List (List.map (fn (x999, 
                                                                                               x1000) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x999, 
                                                                                               cvtFIXTURE x1000]
                                                                                        ) ls1002)
                                                 ) ls1006)
   and cvtINITS ls1013 = PrettyRep.List (List.map (fn (x1010, x1011) => PrettyRep.Tuple [cvtFIXTURE_NAME x1010, 
                                                         cvtEXPRESSION x1011]
                                                  ) ls1013)
   and cvtINSTANCE_TYPE {name=x1017, typeParams=ls1019, typeArgs=ls1024, nonnullable=b1028, 
          superTypes=ls1030, ty=x1034, dynamic=b1035} = PrettyRep.Rec [("name", 
          cvtNAME x1017), ("typeParams", PrettyRep.List (List.map (fn x1018 => 
                                                                         cvtIDENTIFIER x1018
                                                                  ) ls1019)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1023 => cvtTYPE x1023
                                                ) ls1024)), ("nonnullable", 
          PrettyRep.Bool b1028), ("superTypes", PrettyRep.List (List.map (fn x1029 => 
                                                                                cvtTYPE x1029
                                                                         ) ls1030)), 
          ("ty", cvtTYPE x1034), ("dynamic", PrettyRep.Bool b1035)]
   and cvtFIELD {kind=x1051, name=x1052, init=x1053} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1051), ("name", cvtNAME_EXPRESSION x1052), ("init", 
          cvtEXPRESSION x1053)]
   and cvtFIELD_TYPE {name=x1061, ty=x1062} = PrettyRep.Rec [("name", cvtNAME_EXPRESSION x1061), 
          ("ty", cvtTYPE x1062)]
   and cvtFUNC_TYPE {typeParams=ls1069, thisType=x1073, params=ls1075, hasRest=b1079, 
          minArgs=n1080, result=opt1082} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1068 => 
                                                                                                         cvtIDENTIFIER x1068
                                                                                                  ) ls1069)), 
          ("thisType", cvtTYPE x1073), ("params", PrettyRep.List (List.map (fn x1074 => 
                                                                                  cvtTYPE x1074
                                                                           ) ls1075)), 
          ("hasRest", PrettyRep.Bool b1079), ("minArgs", PrettyRep.Int n1080), 
          ("result", 
       (case opt1082 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1081 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1081))
       ))]
   and cvtFUNC_DEFN {kind=x1099, ns=opt1101, final=b1105, override=b1106, prototype=b1107, 
          static=b1108, func=x1109} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1099), 
          ("ns", 
       (case opt1101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1100 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1100))
       )), ("final", PrettyRep.Bool b1105), ("override", PrettyRep.Bool b1106), 
          ("prototype", PrettyRep.Bool b1107), ("static", PrettyRep.Bool b1108), 
          ("func", cvtFUNC x1109)]
   and cvtCTOR_DEFN x1125 = cvtCTOR x1125
   and cvtVAR_DEFN {kind=x1126, ns=opt1128, static=b1132, prototype=b1133, 
          bindings=(ls1135, ls1140)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1126), 
          ("ns", 
       (case opt1128 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1127 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1127))
       )), ("static", PrettyRep.Bool b1132), ("prototype", PrettyRep.Bool b1133), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1134 => 
                                                                        cvtBINDING x1134
                                                                 ) ls1135), 
          PrettyRep.List (List.map (fn x1139 => cvtINIT_STEP x1139
                                   ) ls1140)])]
   and cvtNAMESPACE_DEFN {ident=x1156, ns=opt1158, init=opt1163} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1156), ("ns", 
       (case opt1158 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1157 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1157))
       )), ("init", 
       (case opt1163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1162 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1162))
       ))]
   and cvtCLASS_DEFN {ns=opt1175, privateNS=x1179, protectedNS=x1180, ident=x1181, 
          nonnullable=b1182, dynamic=b1183, final=b1184, params=ls1186, extends=opt1191, 
          implements=ls1196, classDefns=ls1201, instanceDefns=ls1206, instanceStmts=ls1211, 
          ctorDefn=opt1216} = PrettyRep.Rec [("ns", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1174))
       )), ("privateNS", cvtNAMESPACE x1179), ("protectedNS", cvtNAMESPACE x1180), 
          ("ident", cvtIDENTIFIER x1181), ("nonnullable", PrettyRep.Bool b1182), 
          ("dynamic", PrettyRep.Bool b1183), ("final", PrettyRep.Bool b1184), 
          ("params", PrettyRep.List (List.map (fn x1185 => cvtIDENTIFIER x1185
                                              ) ls1186)), ("extends", 
       (case opt1191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1190 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1190))
       )), ("implements", PrettyRep.List (List.map (fn x1195 => cvtTYPE x1195
                                                   ) ls1196)), ("classDefns", 
          PrettyRep.List (List.map (fn x1200 => cvtDEFN x1200
                                   ) ls1201)), ("instanceDefns", PrettyRep.List (List.map (fn x1205 => 
                                                                                                 cvtDEFN x1205
                                                                                          ) ls1206)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1210 => cvtSTATEMENT x1210
                                                     ) ls1211)), ("ctorDefn", 
          
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1215))
       ))]
   and cvtINTERFACE_DEFN {ident=x1249, ns=opt1251, nonnullable=b1255, params=ls1257, 
          extends=ls1262, instanceDefns=ls1267} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1249), ("ns", 
       (case opt1251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1250 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1250))
       )), ("nonnullable", PrettyRep.Bool b1255), ("params", PrettyRep.List (List.map (fn x1256 => 
                                                                                             cvtIDENTIFIER x1256
                                                                                      ) ls1257)), 
          ("extends", PrettyRep.List (List.map (fn x1261 => cvtTYPE x1261
                                               ) ls1262)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1266 => cvtDEFN x1266
                                   ) ls1267))]
   and cvtTYPE_DEFN {ident=x1284, ns=opt1286, typeParams=ls1291, init=x1295} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1284), ("ns", 
       (case opt1286 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1285 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1285))
       )), ("typeParams", PrettyRep.List (List.map (fn x1290 => cvtIDENTIFIER x1290
                                                   ) ls1291)), ("init", cvtTYPE x1295)]
   and cvtCLASS_BLOCK {ns=opt1306, protectedNS=x1310, privateNS=x1311, ident=x1312, 
          name=opt1314, block=x1318} = PrettyRep.Rec [("ns", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1305))
       )), ("protectedNS", cvtNAMESPACE x1310), ("privateNS", cvtNAMESPACE x1311), 
          ("ident", cvtIDENTIFIER x1312), ("name", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1313))
       )), ("block", cvtBLOCK x1318)]
   and cvtFOR_ENUM_HEAD {isEach=b1332, bindings=(ls1334, ls1339), expr=x1344} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1332), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1333 => 
                                                                                                                         cvtBINDING x1333
                                                                                                                  ) ls1334), 
          PrettyRep.List (List.map (fn x1338 => cvtINIT_STEP x1338
                                   ) ls1339)]), ("expr", cvtEXPRESSION x1344)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1352, defn=opt1383, obj=x1387, rib=opt1395, 
          next=x1399, labels=ls1401, body=x1405} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1352), ("defn", 
       (case opt1383 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1353, ns=opt1355, static=b1359, prototype=b1360, bindings=(ls1362, 
            ls1367)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1353), ("ns", 
         (case opt1355 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1354))
         )), ("static", PrettyRep.Bool b1359), ("prototype", PrettyRep.Bool b1360), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1361 => 
                                                                          cvtBINDING x1361
                                                                   ) ls1362), 
            PrettyRep.List (List.map (fn x1366 => cvtINIT_STEP x1366
                                     ) ls1367)])]))
       )), ("obj", cvtEXPRESSION x1387), ("rib", 
       (case opt1395 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1391 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1388, 
                                                                                      x1389) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1388, 
                                                                                      cvtFIXTURE x1389]
                                                                               ) ls1391)))
       )), ("next", cvtSTATEMENT x1399), ("labels", PrettyRep.List (List.map (fn x1400 => 
                                                                                    cvtIDENTIFIER x1400
                                                                             ) ls1401)), 
          ("body", cvtSTATEMENT x1405)]
   and cvtFOR_STATEMENT {rib=opt1428, defn=opt1462, init=ls1467, cond=x1471, 
          update=x1472, labels=ls1474, body=x1478} = PrettyRep.Rec [("rib", 
          
       (case opt1428 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1424 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1421, 
                                                                                      x1422) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1421, 
                                                                                      cvtFIXTURE x1422]
                                                                               ) ls1424)))
       )), ("defn", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1432, ns=opt1434, static=b1438, prototype=b1439, bindings=(ls1441, 
            ls1446)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1432), ("ns", 
         (case opt1434 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1433 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1433))
         )), ("static", PrettyRep.Bool b1438), ("prototype", PrettyRep.Bool b1439), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1440 => 
                                                                          cvtBINDING x1440
                                                                   ) ls1441), 
            PrettyRep.List (List.map (fn x1445 => cvtINIT_STEP x1445
                                     ) ls1446)])]))
       )), ("init", PrettyRep.List (List.map (fn x1466 => cvtSTATEMENT x1466
                                             ) ls1467)), ("cond", cvtEXPRESSION x1471), 
          ("update", cvtEXPRESSION x1472), ("labels", PrettyRep.List (List.map (fn x1473 => 
                                                                                      cvtIDENTIFIER x1473
                                                                               ) ls1474)), 
          ("body", cvtSTATEMENT x1478)]
   and cvtWHILE_STATEMENT {cond=x1494, rib=opt1502, body=x1506, labels=ls1508} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1494), ("rib", 
       (case opt1502 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1498 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1495, 
                                                                                      x1496) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1495, 
                                                                                      cvtFIXTURE x1496]
                                                                               ) ls1498)))
       )), ("body", cvtSTATEMENT x1506), ("labels", PrettyRep.List (List.map (fn x1507 => 
                                                                                    cvtIDENTIFIER x1507
                                                                             ) ls1508))]
   and cvtDIRECTIVES {pragmas=ls1522, defns=ls1527, head=opt1532, body=ls1537, 
          loc=opt1542} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1521 => 
                                                                                    cvtPRAGMA x1521
                                                                             ) ls1522)), 
          ("defns", PrettyRep.List (List.map (fn x1526 => cvtDEFN x1526
                                             ) ls1527)), ("head", 
       (case opt1532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1531 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1531))
       )), ("body", PrettyRep.List (List.map (fn x1536 => cvtSTATEMENT x1536
                                             ) ls1537)), ("loc", 
       (case opt1542 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1541 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1541))
       ))]
   and cvtCASE {label=opt1558, inits=opt1569, body=x1573} = PrettyRep.Rec [("label", 
          
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1557 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1557))
       )), ("inits", 
       (case opt1569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1565 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1562, 
                                                                                      x1563) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1562, 
                                                                                      cvtEXPRESSION x1563]
                                                                               ) ls1565)))
       )), ("body", cvtBLOCK x1573)]
   and cvtCATCH_CLAUSE {bindings=(ls1582, ls1587), ty=x1592, rib=opt1600, inits=opt1611, 
          block=x1615} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1581 => 
                                                                                                      cvtBINDING x1581
                                                                                               ) ls1582), 
          PrettyRep.List (List.map (fn x1586 => cvtINIT_STEP x1586
                                   ) ls1587)]), ("ty", cvtTYPE x1592), ("rib", 
          
       (case opt1600 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1596 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1593, 
                                                                                      x1594) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1593, 
                                                                                      cvtFIXTURE x1594]
                                                                               ) ls1596)))
       )), ("inits", 
       (case opt1611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1607 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1604, 
                                                                                      x1605) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1604, 
                                                                                      cvtEXPRESSION x1605]
                                                                               ) ls1607)))
       )), ("block", cvtBLOCK x1615)]
   and cvtFUNC_NAME {kind=x1627, ident=x1628} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1627), 
          ("ident", cvtIDENTIFIER x1628)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1634, getter=opt1636, setter=opt1641} = 
          PrettyRep.Rec [("ty", cvtTYPE x1634), ("getter", 
       (case opt1636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1635 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1635))
       )), ("setter", 
       (case opt1641 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1640 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1640))
       ))]
   and cvtFRAGMENT (Anon x1652) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1652))
end

