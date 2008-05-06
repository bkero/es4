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
     | cvtBINTYPEOP (Like) = PrettyRep.Ctor ("Like", NONE)
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
   and cvtPRAGMA (UseNamespace x108) = PrettyRep.Ctor ("UseNamespace", SOME (cvtNAMESPACE_EXPRESSION x108))
     | cvtPRAGMA (UseDefaultNamespace x111) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtNAMESPACE_EXPRESSION x111))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtNAME_EXPRESSION (QualifiedName{namespace=x116, identifier=x117}) = 
          PrettyRep.Ctor ("QualifiedName", SOME (PrettyRep.Rec [("namespace", 
          cvtNAMESPACE_EXPRESSION x116), ("identifier", cvtIDENTIFIER x117)]))
     | cvtNAME_EXPRESSION (UnqualifiedName{identifier=x125, openNamespaces=x126, 
          globalNames=x127}) = PrettyRep.Ctor ("UnqualifiedName", SOME (PrettyRep.Rec [("identifier", 
          cvtIDENTIFIER x125), ("openNamespaces", cvtOPEN_NAMESPACES x126), 
          ("globalNames", cvtNAME_SET x127)]))
     | cvtNAME_EXPRESSION (ResolvedName x137) = PrettyRep.Ctor ("ResolvedName", 
          SOME (cvtNAME x137))
   and cvtNAMESPACE_EXPRESSION (Namespace x140) = PrettyRep.Ctor ("Namespace", 
          SOME (cvtNAMESPACE x140))
     | cvtNAMESPACE_EXPRESSION (NamespaceName x143) = PrettyRep.Ctor ("NamespaceName", 
          SOME (cvtNAME_EXPRESSION x143))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x152, privateNS=x153, protectedNS=x154, parentProtectedNSs=ls156, 
          typeParams=ls161, nonnullable=b165, dynamic=b166, extends=opt168, 
          implements=ls173, classRib=x177, instanceRib=x178, instanceInits=x179, 
          constructor=opt181, classType=x185, instanceType=x186}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x152), ("privateNS", cvtNAMESPACE x153), 
          ("protectedNS", cvtNAMESPACE x154), ("parentProtectedNSs", PrettyRep.List (List.map (fn x155 => 
                                                                                                     cvtNAMESPACE x155
                                                                                              ) ls156)), 
          ("typeParams", PrettyRep.List (List.map (fn x160 => cvtIDENTIFIER x160
                                                  ) ls161)), ("nonnullable", 
          PrettyRep.Bool b165), ("dynamic", PrettyRep.Bool b166), ("extends", 
          
       (case opt168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x167 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x167))
       )), ("implements", PrettyRep.List (List.map (fn x172 => cvtTYPE x172
                                                   ) ls173)), ("classRib", 
          cvtRIB x177), ("instanceRib", cvtRIB x178), ("instanceInits", cvtHEAD x179), 
          ("constructor", 
       (case opt181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x180 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x180))
       )), ("classType", cvtTYPE x185), ("instanceType", cvtTYPE x186)]))
   and cvtIFACE (Iface{name=x220, typeParams=ls222, nonnullable=b226, extends=ls228, 
          instanceRib=x232, instanceType=x233}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x220), ("typeParams", PrettyRep.List (List.map (fn x221 => 
                                                                                                      cvtIDENTIFIER x221
                                                                                               ) ls222)), 
          ("nonnullable", PrettyRep.Bool b226), ("extends", PrettyRep.List (List.map (fn x227 => 
                                                                                            cvtTYPE x227
                                                                                     ) ls228)), 
          ("instanceRib", cvtRIB x232), ("instanceType", cvtTYPE x233)]))
   and cvtCTOR (Ctor{settings=x249, superArgs=ls251, func=x255}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x249), ("superArgs", PrettyRep.List (List.map (fn x250 => 
                                                                                                         cvtEXPRESSION x250
                                                                                                  ) ls251)), 
          ("func", cvtFUNC x255)]))
   and cvtFUNC (Func{name=x265, fsig=x266, native=b267, generator=b268, block=opt270, 
          param=x274, defaults=ls276, ty=x280, loc=opt282}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x265), ("fsig", cvtFUNC_SIG x266), 
          ("native", PrettyRep.Bool b267), ("generator", PrettyRep.Bool b268), 
          ("block", 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x269))
       )), ("param", cvtHEAD x274), ("defaults", PrettyRep.List (List.map (fn x275 => 
                                                                                 cvtEXPRESSION x275
                                                                          ) ls276)), 
          ("ty", cvtTYPE x280), ("loc", 
       (case opt282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x281 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x281))
       ))]))
   and cvtDEFN (ClassDefn x307) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x307))
     | cvtDEFN (VariableDefn x310) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x310))
     | cvtDEFN (FunctionDefn x313) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x313))
     | cvtDEFN (ConstructorDefn x316) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x316))
     | cvtDEFN (InterfaceDefn x319) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x319))
     | cvtDEFN (NamespaceDefn x322) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x322))
     | cvtDEFN (TypeDefn x325) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x325))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls329, params=x333, paramTypes=ls335, 
          defaults=ls340, ctorInits=opt351, returnType=opt356, thisType=opt361, 
          hasRest=b365}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x328 => cvtIDENTIFIER x328
                                   ) ls329)), ("params", cvtBINDINGS x333), 
          ("paramTypes", PrettyRep.List (List.map (fn x334 => cvtTYPE x334
                                                  ) ls335)), ("defaults", PrettyRep.List (List.map (fn x339 => 
                                                                                                          cvtEXPRESSION x339
                                                                                                   ) ls340)), 
          ("ctorInits", 
       (case opt351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x344, ls346) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x344, 
            PrettyRep.List (List.map (fn x345 => cvtEXPRESSION x345
                                     ) ls346)]))
       )), ("returnType", 
       (case opt356 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x355 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x355))
       )), ("thisType", 
       (case opt361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x360 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x360))
       )), ("hasRest", PrettyRep.Bool b365)]))
   and cvtBINDING (Binding{ident=x385, ty=x386}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x385), ("ty", 
          cvtTYPE x386)]))
   and cvtBINDING_IDENTIFIER (TempIdent n394) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n394))
     | cvtBINDING_IDENTIFIER (ParamIdent n397) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n397))
     | cvtBINDING_IDENTIFIER (PropIdent x400) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x400))
   and cvtINIT_STEP (InitStep(x403, x404)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x403, 
          cvtEXPRESSION x404]))
     | cvtINIT_STEP (AssignStep(x408, x409)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x408, cvtEXPRESSION x409]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (RecordType ls417) = PrettyRep.Ctor ("RecordType", SOME (PrettyRep.List (List.map (fn x416 => 
                                                                                                        cvtFIELD_TYPE x416
                                                                                                 ) ls417)))
     | cvtTYPE (ArrayType ls424) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x423 => 
                                                                                                      cvtTYPE x423
                                                                                               ) ls424)))
     | cvtTYPE (UnionType ls431) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x430 => 
                                                                                                      cvtTYPE x430
                                                                                               ) ls431)))
     | cvtTYPE (FunctionType x437) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x437))
     | cvtTYPE (NonNullType x440) = PrettyRep.Ctor ("NonNullType", SOME (cvtTYPE x440))
     | cvtTYPE (AppType(x443, ls445)) = PrettyRep.Ctor ("AppType", SOME (PrettyRep.Tuple [cvtTYPE x443, 
          PrettyRep.List (List.map (fn x444 => cvtTYPE x444
                                   ) ls445)]))
     | cvtTYPE (TypeName(x452, opt454)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtNAME_EXPRESSION x452, 
          
       (case opt454 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x453 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x453))
       )]))
     | cvtTYPE (TypeNameReferenceType(x461, x462)) = PrettyRep.Ctor ("TypeNameReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x461, cvtNAME_EXPRESSION x462]))
     | cvtTYPE (TypeIndexReferenceType(x466, n467)) = PrettyRep.Ctor ("TypeIndexReferenceType", 
          SOME (PrettyRep.Tuple [cvtTYPE x466, PrettyRep.Int n467]))
     | cvtTYPE (InstanceType x471) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x471))
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
          minArgs=n1077, result=opt1079} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1065 => 
                                                                                                         cvtIDENTIFIER x1065
                                                                                                  ) ls1066)), 
          ("thisType", cvtTYPE x1070), ("params", PrettyRep.List (List.map (fn x1071 => 
                                                                                  cvtTYPE x1071
                                                                           ) ls1072)), 
          ("hasRest", PrettyRep.Bool b1076), ("minArgs", PrettyRep.Int n1077), 
          ("result", 
       (case opt1079 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1078 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1078))
       ))]
   and cvtFUNC_DEFN {kind=x1096, ns=opt1098, final=b1102, override=b1103, prototype=b1104, 
          static=b1105, func=x1106} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1096), 
          ("ns", 
       (case opt1098 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1097 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1097))
       )), ("final", PrettyRep.Bool b1102), ("override", PrettyRep.Bool b1103), 
          ("prototype", PrettyRep.Bool b1104), ("static", PrettyRep.Bool b1105), 
          ("func", cvtFUNC x1106)]
   and cvtCTOR_DEFN x1122 = cvtCTOR x1122
   and cvtVAR_DEFN {kind=x1123, ns=opt1125, static=b1129, prototype=b1130, 
          bindings=(ls1132, ls1137)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1123), 
          ("ns", 
       (case opt1125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1124 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1124))
       )), ("static", PrettyRep.Bool b1129), ("prototype", PrettyRep.Bool b1130), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1131 => 
                                                                        cvtBINDING x1131
                                                                 ) ls1132), 
          PrettyRep.List (List.map (fn x1136 => cvtINIT_STEP x1136
                                   ) ls1137)])]
   and cvtNAMESPACE_DEFN {ident=x1153, ns=opt1155, init=opt1160} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1153), ("ns", 
       (case opt1155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1154 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1154))
       )), ("init", 
       (case opt1160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1159 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1159))
       ))]
   and cvtCLASS_DEFN {ns=opt1172, privateNS=x1176, protectedNS=x1177, ident=x1178, 
          nonnullable=b1179, dynamic=b1180, final=b1181, params=ls1183, extends=opt1188, 
          implements=ls1193, classDefns=ls1198, instanceDefns=ls1203, instanceStmts=ls1208, 
          ctorDefn=opt1213} = PrettyRep.Rec [("ns", 
       (case opt1172 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1171 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1171))
       )), ("privateNS", cvtNAMESPACE x1176), ("protectedNS", cvtNAMESPACE x1177), 
          ("ident", cvtIDENTIFIER x1178), ("nonnullable", PrettyRep.Bool b1179), 
          ("dynamic", PrettyRep.Bool b1180), ("final", PrettyRep.Bool b1181), 
          ("params", PrettyRep.List (List.map (fn x1182 => cvtIDENTIFIER x1182
                                              ) ls1183)), ("extends", 
       (case opt1188 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1187 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1187))
       )), ("implements", PrettyRep.List (List.map (fn x1192 => cvtTYPE x1192
                                                   ) ls1193)), ("classDefns", 
          PrettyRep.List (List.map (fn x1197 => cvtDEFN x1197
                                   ) ls1198)), ("instanceDefns", PrettyRep.List (List.map (fn x1202 => 
                                                                                                 cvtDEFN x1202
                                                                                          ) ls1203)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1207 => cvtSTATEMENT x1207
                                                     ) ls1208)), ("ctorDefn", 
          
       (case opt1213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1212 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1212))
       ))]
   and cvtINTERFACE_DEFN {ident=x1246, ns=opt1248, nonnullable=b1252, params=ls1254, 
          extends=ls1259, instanceDefns=ls1264} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1246), ("ns", 
       (case opt1248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1247 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1247))
       )), ("nonnullable", PrettyRep.Bool b1252), ("params", PrettyRep.List (List.map (fn x1253 => 
                                                                                             cvtIDENTIFIER x1253
                                                                                      ) ls1254)), 
          ("extends", PrettyRep.List (List.map (fn x1258 => cvtTYPE x1258
                                               ) ls1259)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1263 => cvtDEFN x1263
                                   ) ls1264))]
   and cvtTYPE_DEFN {ident=x1281, ns=opt1283, typeParams=ls1288, init=x1292} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1281), ("ns", 
       (case opt1283 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1282 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1282))
       )), ("typeParams", PrettyRep.List (List.map (fn x1287 => cvtIDENTIFIER x1287
                                                   ) ls1288)), ("init", cvtTYPE x1292)]
   and cvtCLASS_BLOCK {ns=opt1303, protectedNS=x1307, privateNS=x1308, ident=x1309, 
          name=opt1311, block=x1315} = PrettyRep.Rec [("ns", 
       (case opt1303 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1302 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1302))
       )), ("protectedNS", cvtNAMESPACE x1307), ("privateNS", cvtNAMESPACE x1308), 
          ("ident", cvtIDENTIFIER x1309), ("name", 
       (case opt1311 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1310 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1310))
       )), ("block", cvtBLOCK x1315)]
   and cvtFOR_ENUM_HEAD {isEach=b1329, bindings=(ls1331, ls1336), expr=x1341} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1329), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1330 => 
                                                                                                                         cvtBINDING x1330
                                                                                                                  ) ls1331), 
          PrettyRep.List (List.map (fn x1335 => cvtINIT_STEP x1335
                                   ) ls1336)]), ("expr", cvtEXPRESSION x1341)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1349, defn=opt1380, obj=x1384, rib=opt1392, 
          next=x1396, labels=ls1398, body=x1402} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1349), ("defn", 
       (case opt1380 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1350, ns=opt1352, static=b1356, prototype=b1357, bindings=(ls1359, 
            ls1364)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1350), ("ns", 
         (case opt1352 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1351))
         )), ("static", PrettyRep.Bool b1356), ("prototype", PrettyRep.Bool b1357), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1358 => 
                                                                          cvtBINDING x1358
                                                                   ) ls1359), 
            PrettyRep.List (List.map (fn x1363 => cvtINIT_STEP x1363
                                     ) ls1364)])]))
       )), ("obj", cvtEXPRESSION x1384), ("rib", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1388 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1385, 
                                                                                      x1386) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1385, 
                                                                                      cvtFIXTURE x1386]
                                                                               ) ls1388)))
       )), ("next", cvtSTATEMENT x1396), ("labels", PrettyRep.List (List.map (fn x1397 => 
                                                                                    cvtIDENTIFIER x1397
                                                                             ) ls1398)), 
          ("body", cvtSTATEMENT x1402)]
   and cvtFOR_STATEMENT {rib=opt1425, defn=opt1459, init=ls1464, cond=x1468, 
          update=x1469, labels=ls1471, body=x1475} = PrettyRep.Rec [("rib", 
          
       (case opt1425 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1421 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1418, 
                                                                                      x1419) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1418, 
                                                                                      cvtFIXTURE x1419]
                                                                               ) ls1421)))
       )), ("defn", 
       (case opt1459 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1429, ns=opt1431, static=b1435, prototype=b1436, bindings=(ls1438, 
            ls1443)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1429), ("ns", 
         (case opt1431 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1430 => PrettyRep.Ctor ("SOME", SOME (cvtNAMESPACE_EXPRESSION x1430))
         )), ("static", PrettyRep.Bool b1435), ("prototype", PrettyRep.Bool b1436), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1437 => 
                                                                          cvtBINDING x1437
                                                                   ) ls1438), 
            PrettyRep.List (List.map (fn x1442 => cvtINIT_STEP x1442
                                     ) ls1443)])]))
       )), ("init", PrettyRep.List (List.map (fn x1463 => cvtSTATEMENT x1463
                                             ) ls1464)), ("cond", cvtEXPRESSION x1468), 
          ("update", cvtEXPRESSION x1469), ("labels", PrettyRep.List (List.map (fn x1470 => 
                                                                                      cvtIDENTIFIER x1470
                                                                               ) ls1471)), 
          ("body", cvtSTATEMENT x1475)]
   and cvtWHILE_STATEMENT {cond=x1491, rib=opt1499, body=x1503, labels=ls1505} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1491), ("rib", 
       (case opt1499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1495 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1492, 
                                                                                      x1493) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1492, 
                                                                                      cvtFIXTURE x1493]
                                                                               ) ls1495)))
       )), ("body", cvtSTATEMENT x1503), ("labels", PrettyRep.List (List.map (fn x1504 => 
                                                                                    cvtIDENTIFIER x1504
                                                                             ) ls1505))]
   and cvtDIRECTIVES {pragmas=ls1519, defns=ls1524, head=opt1529, body=ls1534, 
          loc=opt1539} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1518 => 
                                                                                    cvtPRAGMA x1518
                                                                             ) ls1519)), 
          ("defns", PrettyRep.List (List.map (fn x1523 => cvtDEFN x1523
                                             ) ls1524)), ("head", 
       (case opt1529 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1528 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1528))
       )), ("body", PrettyRep.List (List.map (fn x1533 => cvtSTATEMENT x1533
                                             ) ls1534)), ("loc", 
       (case opt1539 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1538 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1538))
       ))]
   and cvtCASE {label=opt1555, inits=opt1566, body=x1570} = PrettyRep.Rec [("label", 
          
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1554))
       )), ("inits", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1562 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1559, 
                                                                                      x1560) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1559, 
                                                                                      cvtEXPRESSION x1560]
                                                                               ) ls1562)))
       )), ("body", cvtBLOCK x1570)]
   and cvtCATCH_CLAUSE {bindings=(ls1579, ls1584), ty=x1589, rib=opt1597, inits=opt1608, 
          block=x1612} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1578 => 
                                                                                                      cvtBINDING x1578
                                                                                               ) ls1579), 
          PrettyRep.List (List.map (fn x1583 => cvtINIT_STEP x1583
                                   ) ls1584)]), ("ty", cvtTYPE x1589), ("rib", 
          
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1593 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1590, 
                                                                                      x1591) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1590, 
                                                                                      cvtFIXTURE x1591]
                                                                               ) ls1593)))
       )), ("inits", 
       (case opt1608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1604 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1601, 
                                                                                      x1602) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1601, 
                                                                                      cvtEXPRESSION x1602]
                                                                               ) ls1604)))
       )), ("block", cvtBLOCK x1612)]
   and cvtFUNC_NAME {kind=x1624, ident=x1625} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1624), 
          ("ident", cvtIDENTIFIER x1625)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1631, getter=opt1633, setter=opt1638} = 
          PrettyRep.Rec [("ty", cvtTYPE x1631), ("getter", 
       (case opt1633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1632 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1632))
       )), ("setter", 
       (case opt1638 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1637 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1637))
       ))]
   and cvtFRAGMENT (Anon x1649) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1649))
end

