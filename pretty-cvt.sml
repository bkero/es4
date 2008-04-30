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
   and cvtMULTINAME {nss=ls40, id=x44} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls36 => 
                                                                                                PrettyRep.List (List.map (fn x35 => 
                                                                                                                                cvtNAMESPACE x35
                                                                                                                         ) ls36)
                                                                                         ) ls40)), 
          ("id", cvtIDENTIFIER x44)]
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
   and cvtPRAGMA (UseNamespace x107) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPRESSION x107))
     | cvtPRAGMA (UseDefaultNamespace x110) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPRESSION x110))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x121, privateNS=x122, protectedNS=x123, parentProtectedNSs=ls125, 
          typeParams=ls130, nonnullable=b134, dynamic=b135, extends=opt137, 
          implements=ls142, classRib=x146, instanceRib=x147, instanceInits=x148, 
          constructor=opt150, classType=x154, instanceType=x155}) = PrettyRep.Ctor ("Cls", 
          SOME (PrettyRep.Rec [("name", cvtNAME x121), ("privateNS", cvtNAMESPACE x122), 
          ("protectedNS", cvtNAMESPACE x123), ("parentProtectedNSs", PrettyRep.List (List.map (fn x124 => 
                                                                                                     cvtNAMESPACE x124
                                                                                              ) ls125)), 
          ("typeParams", PrettyRep.List (List.map (fn x129 => cvtIDENTIFIER x129
                                                  ) ls130)), ("nonnullable", 
          PrettyRep.Bool b134), ("dynamic", PrettyRep.Bool b135), ("extends", 
          
       (case opt137 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x136 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x136))
       )), ("implements", PrettyRep.List (List.map (fn x141 => cvtTYPE x141
                                                   ) ls142)), ("classRib", 
          cvtRIB x146), ("instanceRib", cvtRIB x147), ("instanceInits", cvtHEAD x148), 
          ("constructor", 
       (case opt150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x149 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x149))
       )), ("classType", cvtTYPE x154), ("instanceType", cvtTYPE x155)]))
   and cvtIFACE (Iface{name=x189, typeParams=ls191, nonnullable=b195, extends=ls197, 
          instanceRib=x201, instanceType=x202}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x189), ("typeParams", PrettyRep.List (List.map (fn x190 => 
                                                                                                      cvtIDENTIFIER x190
                                                                                               ) ls191)), 
          ("nonnullable", PrettyRep.Bool b195), ("extends", PrettyRep.List (List.map (fn x196 => 
                                                                                            cvtTYPE x196
                                                                                     ) ls197)), 
          ("instanceRib", cvtRIB x201), ("instanceType", cvtTYPE x202)]))
   and cvtCTOR (Ctor{settings=x218, superArgs=ls220, func=x224}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x218), ("superArgs", PrettyRep.List (List.map (fn x219 => 
                                                                                                         cvtEXPRESSION x219
                                                                                                  ) ls220)), 
          ("func", cvtFUNC x224)]))
   and cvtFUNC (Func{name=x234, fsig=x235, native=b236, generator=b237, block=opt239, 
          param=x243, defaults=ls245, ty=x249, loc=opt251}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x234), ("fsig", cvtFUNC_SIG x235), 
          ("native", PrettyRep.Bool b236), ("generator", PrettyRep.Bool b237), 
          ("block", 
       (case opt239 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x238 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x238))
       )), ("param", cvtHEAD x243), ("defaults", PrettyRep.List (List.map (fn x244 => 
                                                                                 cvtEXPRESSION x244
                                                                          ) ls245)), 
          ("ty", cvtTYPE x249), ("loc", 
       (case opt251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x250 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x250))
       ))]))
   and cvtDEFN (ClassDefn x276) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x276))
     | cvtDEFN (VariableDefn x279) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x279))
     | cvtDEFN (FunctionDefn x282) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x282))
     | cvtDEFN (ConstructorDefn x285) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x285))
     | cvtDEFN (InterfaceDefn x288) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x288))
     | cvtDEFN (NamespaceDefn x291) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x291))
     | cvtDEFN (TypeDefn x294) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x294))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls298, params=x302, paramTypes=ls304, 
          defaults=ls309, ctorInits=opt320, returnType=x324, thisType=opt326, 
          hasRest=b330}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x297 => cvtIDENTIFIER x297
                                   ) ls298)), ("params", cvtBINDINGS x302), 
          ("paramTypes", PrettyRep.List (List.map (fn x303 => cvtTYPE x303
                                                  ) ls304)), ("defaults", PrettyRep.List (List.map (fn x308 => 
                                                                                                          cvtEXPRESSION x308
                                                                                                   ) ls309)), 
          ("ctorInits", 
       (case opt320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x313, ls315) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x313, 
            PrettyRep.List (List.map (fn x314 => cvtEXPRESSION x314
                                     ) ls315)]))
       )), ("returnType", cvtTYPE x324), ("thisType", 
       (case opt326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x325 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x325))
       )), ("hasRest", PrettyRep.Bool b330)]))
   and cvtBINDING (Binding{ident=x350, ty=x351}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENTIFIER x350), ("ty", 
          cvtTYPE x351)]))
   and cvtBINDING_IDENTIFIER (TempIdent n359) = PrettyRep.Ctor ("TempIdent", 
          SOME (PrettyRep.Int n359))
     | cvtBINDING_IDENTIFIER (ParamIdent n362) = PrettyRep.Ctor ("ParamIdent", 
          SOME (PrettyRep.Int n362))
     | cvtBINDING_IDENTIFIER (PropIdent x365) = PrettyRep.Ctor ("PropIdent", 
          SOME (cvtIDENTIFIER x365))
   and cvtINIT_STEP (InitStep(x368, x369)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENTIFIER x368, 
          cvtEXPRESSION x369]))
     | cvtINIT_STEP (AssignStep(x373, x374)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x373, cvtEXPRESSION x374]))
   and cvtTYPE (NullType) = PrettyRep.Ctor ("NullType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
     | cvtTYPE (UnionType ls383) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x382 => 
                                                                                                      cvtTYPE x382
                                                                                               ) ls383)))
     | cvtTYPE (ArrayType ls390) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x389 => 
                                                                                                      cvtTYPE x389
                                                                                               ) ls390)))
     | cvtTYPE (TypeName(x396, opt398)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtIDENTIFIER_EXPRESSION x396, 
          
       (case opt398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x397 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x397))
       )]))
     | cvtTYPE (ElementTypeRef(x405, n406)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x405, PrettyRep.Int n406]))
     | cvtTYPE (FieldTypeRef(x410, x411)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x410, cvtIDENTIFIER x411]))
     | cvtTYPE (FunctionType x415) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x415))
     | cvtTYPE (ObjectType ls419) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x418 => 
                                                                                                        cvtFIELD_TYPE x418
                                                                                                 ) ls419)))
     | cvtTYPE (AppType{base=x425, args=ls427}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x425), ("args", PrettyRep.List (List.map (fn x426 => 
                                                                                                cvtTYPE x426
                                                                                         ) ls427))]))
     | cvtTYPE (LamType{params=ls439, body=x443}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x438 => 
                                                                          cvtIDENTIFIER x438
                                                                   ) ls439)), 
          ("body", cvtTYPE x443)]))
     | cvtTYPE (NullableType{expr=x451, nullable=b452}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE x451), ("nullable", PrettyRep.Bool b452)]))
     | cvtTYPE (InstanceType x460) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x460))
     | cvtTYPE (TypeVarFixtureRef x463) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x463))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x467) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x467))
     | cvtSTATEMENT (InitStmt{kind=x470, ns=opt472, prototype=b476, static=b477, 
          temps=x478, inits=ls480}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x470), ("ns", 
       (case opt472 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x471 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x471))
       )), ("prototype", PrettyRep.Bool b476), ("static", PrettyRep.Bool b477), 
          ("temps", cvtBINDINGS x478), ("inits", PrettyRep.List (List.map (fn x479 => 
                                                                                 cvtINIT_STEP x479
                                                                          ) ls480))]))
     | cvtSTATEMENT (ClassBlock x499) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x499))
     | cvtSTATEMENT (ForInStmt x502) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x502))
     | cvtSTATEMENT (ThrowStmt x505) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x505))
     | cvtSTATEMENT (ReturnStmt x508) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x508))
     | cvtSTATEMENT (BreakStmt opt512) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt512 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x511 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x511))
       ))
     | cvtSTATEMENT (ContinueStmt opt519) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x518 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x518))
       ))
     | cvtSTATEMENT (BlockStmt x525) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x525))
     | cvtSTATEMENT (LabeledStmt(x528, x529)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x528, cvtSTATEMENT x529]))
     | cvtSTATEMENT (LetStmt x533) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x533))
     | cvtSTATEMENT (WhileStmt x536) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x536))
     | cvtSTATEMENT (DoWhileStmt x539) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x539))
     | cvtSTATEMENT (ForStmt x542) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x542))
     | cvtSTATEMENT (IfStmt{cnd=x545, thn=x546, els=x547}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x545), ("thn", cvtSTATEMENT x546), 
          ("els", cvtSTATEMENT x547)]))
     | cvtSTATEMENT (WithStmt{obj=x557, ty=x558, body=x559}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x557), ("ty", cvtTYPE x558), 
          ("body", cvtSTATEMENT x559)]))
     | cvtSTATEMENT (TryStmt{block=x569, catches=ls571, finally=opt576}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x569), 
          ("catches", PrettyRep.List (List.map (fn x570 => cvtCATCH_CLAUSE x570
                                               ) ls571)), ("finally", 
       (case opt576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x575 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x575))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x589, labels=ls591, cases=ls596}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x589), ("labels", PrettyRep.List (List.map (fn x590 => 
                                                                                                        cvtIDENTIFIER x590
                                                                                                 ) ls591)), 
          ("cases", PrettyRep.List (List.map (fn x595 => cvtCASE x595
                                             ) ls596))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x609, ty=x610, cases=ls612}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x609), ("ty", cvtTYPE x610), 
          ("cases", PrettyRep.List (List.map (fn x611 => cvtCATCH_CLAUSE x611
                                             ) ls612))]))
     | cvtSTATEMENT (DXNStmt{expr=x625}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x625)]))
   and cvtEXPRESSION (TernaryExpr(x631, x632, x633)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x631, cvtEXPRESSION x632, cvtEXPRESSION x633]))
     | cvtEXPRESSION (BinaryExpr(x637, x638, x639)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x637, cvtEXPRESSION x638, cvtEXPRESSION x639]))
     | cvtEXPRESSION (BinaryTypeExpr(x643, x644, x645)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x643, cvtEXPRESSION x644, cvtTYPE x645]))
     | cvtEXPRESSION (UnaryExpr(x649, x650)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x649, cvtEXPRESSION x650]))
     | cvtEXPRESSION (TypeExpr x654) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x654))
     | cvtEXPRESSION (ThisExpr opt658) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt658 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x657 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x657))
       ))
     | cvtEXPRESSION (YieldExpr opt665) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt665 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x664 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x664))
       ))
     | cvtEXPRESSION (SuperExpr opt672) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x671 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x671))
       ))
     | cvtEXPRESSION (CallExpr{func=x678, actuals=ls680}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x678), ("actuals", PrettyRep.List (List.map (fn x679 => 
                                                                                                         cvtEXPRESSION x679
                                                                                                  ) ls680))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x691, actuals=ls693}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x691), ("actuals", PrettyRep.List (List.map (fn x692 => 
                                                                                                         cvtTYPE x692
                                                                                                  ) ls693))]))
     | cvtEXPRESSION (LetExpr{defs=x704, body=x705, head=opt707}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x704), ("body", cvtEXPRESSION x705), 
          ("head", 
       (case opt707 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x706 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x706))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x720, actuals=ls722}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x720), ("actuals", PrettyRep.List (List.map (fn x721 => 
                                                                                                        cvtEXPRESSION x721
                                                                                                 ) ls722))]))
     | cvtEXPRESSION (ObjectRef{base=x733, ident=x734, loc=opt736}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPRESSION x733), ("ident", cvtIDENTIFIER_EXPRESSION x734), 
          ("loc", 
       (case opt736 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x735 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x735))
       ))]))
     | cvtEXPRESSION (LexicalRef{ident=x749, loc=opt751}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENTIFIER_EXPRESSION x749), ("loc", 
          
       (case opt751 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x750 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x750))
       ))]))
     | cvtEXPRESSION (SetExpr(x762, x763, x764)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x762, cvtEXPRESSION x763, cvtEXPRESSION x764]))
     | cvtEXPRESSION (ListExpr ls769) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x768 => 
                                                                                                          cvtEXPRESSION x768
                                                                                                   ) ls769)))
     | cvtEXPRESSION (InitExpr(x775, x776, x777)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x775, cvtHEAD x776, cvtINITS x777]))
     | cvtEXPRESSION (GetTemp n781) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n781))
     | cvtEXPRESSION (GetParam n784) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n784))
     | cvtEXPRESSION (Comprehension(x787, ls789, opt794)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x787, PrettyRep.List (List.map (fn x788 => 
                                                                                     cvtFOR_ENUM_HEAD x788
                                                                              ) ls789), 
          
       (case opt794 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x793 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x793))
       )]))
     | cvtEXPRESSION (LiteralExpr x801) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x801))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n809) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n809))
     | cvtFIXTURE_NAME (PropName x812) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x812))
   and cvtIDENTIFIER_EXPRESSION (Identifier{ident=x815, openNamespaces=ls821, 
          rootRib=opt826}) = PrettyRep.Ctor ("Identifier", SOME (PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x815), ("openNamespaces", PrettyRep.List (List.map (fn ls817 => 
                                                                                  PrettyRep.List (List.map (fn x816 => 
                                                                                                                  cvtNAMESPACE x816
                                                                                                           ) ls817)
                                                                           ) ls821)), 
          ("rootRib", 
       (case opt826 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x825 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x825))
       ))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedExpression{qual=x839, expr=x840}) = 
          PrettyRep.Ctor ("QualifiedExpression", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x839), ("expr", cvtEXPRESSION x840)]))
     | cvtIDENTIFIER_EXPRESSION (ExpressionIdentifier{expr=x848, openNamespaces=ls854}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x848), ("openNamespaces", PrettyRep.List (List.map (fn ls850 => 
                                                                                  PrettyRep.List (List.map (fn x849 => 
                                                                                                                  cvtNAMESPACE x849
                                                                                                           ) ls850)
                                                                           ) ls854))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedIdentifier{qual=x865, ident=s866}) = 
          PrettyRep.Ctor ("QualifiedIdentifier", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x865), ("ident", PrettyRep.UniStr s866)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r876) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r876))
     | cvtLITERAL (LiteralDecimal d879) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d879))
     | cvtLITERAL (LiteralBoolean b882) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b882))
     | cvtLITERAL (LiteralString s885) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s885))
     | cvtLITERAL (LiteralArray{exprs=x888, ty=opt890}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x888), ("ty", 
       (case opt890 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x889 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x889))
       ))]))
     | cvtLITERAL (LiteralXML ls902) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x901 => 
                                                                                                           cvtEXPRESSION x901
                                                                                                    ) ls902)))
     | cvtLITERAL (LiteralNamespace x908) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x908))
     | cvtLITERAL (LiteralObject{expr=ls912, ty=opt917}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x911 => 
                                                                        cvtFIELD x911
                                                                 ) ls912)), 
          ("ty", 
       (case opt917 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x916 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x916))
       ))]))
     | cvtLITERAL (LiteralFunction x928) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x928))
     | cvtLITERAL (LiteralRegExp{str=s931}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s931)]))
   and cvtBLOCK (Block x937) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x937))
   and cvtFIXTURE (NamespaceFixture x940) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x940))
     | cvtFIXTURE (ClassFixture x943) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x943))
     | cvtFIXTURE (InterfaceFixture x946) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x946))
     | cvtFIXTURE (TypeVarFixture x949) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x949))
     | cvtFIXTURE (TypeFixture x952) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE x952))
     | cvtFIXTURE (MethodFixture{func=x955, ty=x956, readOnly=b957, override=b958, 
          final=b959}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x955), ("ty", cvtTYPE x956), ("readOnly", PrettyRep.Bool b957), 
          ("override", PrettyRep.Bool b958), ("final", PrettyRep.Bool b959)]))
     | cvtFIXTURE (ValFixture{ty=x973, readOnly=b974}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x973), ("readOnly", PrettyRep.Bool b974)]))
     | cvtFIXTURE (VirtualValFixture{ty=x982, getter=opt984, setter=opt989}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x982), ("getter", 
       (case opt984 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x983 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x983))
       )), ("setter", 
       (case opt989 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x988 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x988))
       ))]))
   and cvtHEAD (Head(x1002, x1003)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1002, 
          cvtINITS x1003]))
   and cvtBINDINGS (ls1008, ls1013) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1007 => 
                                                                                       cvtBINDING x1007
                                                                                ) ls1008), 
          PrettyRep.List (List.map (fn x1012 => cvtINIT_STEP x1012
                                   ) ls1013)]
   and cvtRIB ls1021 = PrettyRep.List (List.map (fn (x1018, x1019) => PrettyRep.Tuple [cvtFIXTURE_NAME x1018, 
                                                       cvtFIXTURE x1019]
                                                ) ls1021)
   and cvtRIBS ls1032 = PrettyRep.List (List.map (fn ls1028 => PrettyRep.List (List.map (fn (x1025, 
                                                                                               x1026) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1025, 
                                                                                               cvtFIXTURE x1026]
                                                                                        ) ls1028)
                                                 ) ls1032)
   and cvtINITS ls1039 = PrettyRep.List (List.map (fn (x1036, x1037) => PrettyRep.Tuple [cvtFIXTURE_NAME x1036, 
                                                         cvtEXPRESSION x1037]
                                                  ) ls1039)
   and cvtINSTANCE_TYPE {name=x1043, typeParams=ls1045, typeArgs=ls1050, nonnullable=b1054, 
          superTypes=ls1056, ty=x1060, dynamic=b1061} = PrettyRep.Rec [("name", 
          cvtNAME x1043), ("typeParams", PrettyRep.List (List.map (fn x1044 => 
                                                                         cvtIDENTIFIER x1044
                                                                  ) ls1045)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1049 => cvtTYPE x1049
                                                ) ls1050)), ("nonnullable", 
          PrettyRep.Bool b1054), ("superTypes", PrettyRep.List (List.map (fn x1055 => 
                                                                                cvtTYPE x1055
                                                                         ) ls1056)), 
          ("ty", cvtTYPE x1060), ("dynamic", PrettyRep.Bool b1061)]
   and cvtFIELD {kind=x1077, name=x1078, init=x1079} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1077), ("name", cvtIDENTIFIER_EXPRESSION x1078), 
          ("init", cvtEXPRESSION x1079)]
   and cvtFIELD_TYPE {name=x1087, ty=x1088} = PrettyRep.Rec [("name", cvtIDENTIFIER x1087), 
          ("ty", cvtTYPE x1088)]
   and cvtFUNC_TYPE {params=ls1095, result=x1099, thisType=x1100, hasRest=b1101, 
          minArgs=n1102} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1094 => 
                                                                                     cvtTYPE x1094
                                                                              ) ls1095)), 
          ("result", cvtTYPE x1099), ("thisType", cvtTYPE x1100), ("hasRest", 
          PrettyRep.Bool b1101), ("minArgs", PrettyRep.Int n1102)]
   and cvtFUNC_DEFN {kind=x1114, ns=opt1116, final=b1120, override=b1121, prototype=b1122, 
          static=b1123, func=x1124} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1114), 
          ("ns", 
       (case opt1116 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1115 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1115))
       )), ("final", PrettyRep.Bool b1120), ("override", PrettyRep.Bool b1121), 
          ("prototype", PrettyRep.Bool b1122), ("static", PrettyRep.Bool b1123), 
          ("func", cvtFUNC x1124)]
   and cvtCTOR_DEFN x1140 = cvtCTOR x1140
   and cvtVAR_DEFN {kind=x1141, ns=opt1143, static=b1147, prototype=b1148, 
          bindings=(ls1150, ls1155)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1141), 
          ("ns", 
       (case opt1143 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1142 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1142))
       )), ("static", PrettyRep.Bool b1147), ("prototype", PrettyRep.Bool b1148), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1149 => 
                                                                        cvtBINDING x1149
                                                                 ) ls1150), 
          PrettyRep.List (List.map (fn x1154 => cvtINIT_STEP x1154
                                   ) ls1155)])]
   and cvtNAMESPACE_DEFN {ident=x1171, ns=opt1173, init=opt1178} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1171), ("ns", 
       (case opt1173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1172 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1172))
       )), ("init", 
       (case opt1178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1177 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1177))
       ))]
   and cvtCLASS_DEFN {ns=opt1190, privateNS=x1194, protectedNS=x1195, ident=x1196, 
          nonnullable=b1197, dynamic=b1198, final=b1199, params=ls1201, extends=opt1206, 
          implements=ls1211, classDefns=ls1216, instanceDefns=ls1221, instanceStmts=ls1226, 
          ctorDefn=opt1231} = PrettyRep.Rec [("ns", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1189 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1189))
       )), ("privateNS", cvtNAMESPACE x1194), ("protectedNS", cvtNAMESPACE x1195), 
          ("ident", cvtIDENTIFIER x1196), ("nonnullable", PrettyRep.Bool b1197), 
          ("dynamic", PrettyRep.Bool b1198), ("final", PrettyRep.Bool b1199), 
          ("params", PrettyRep.List (List.map (fn x1200 => cvtIDENTIFIER x1200
                                              ) ls1201)), ("extends", 
       (case opt1206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1205 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1205))
       )), ("implements", PrettyRep.List (List.map (fn x1210 => cvtTYPE x1210
                                                   ) ls1211)), ("classDefns", 
          PrettyRep.List (List.map (fn x1215 => cvtDEFN x1215
                                   ) ls1216)), ("instanceDefns", PrettyRep.List (List.map (fn x1220 => 
                                                                                                 cvtDEFN x1220
                                                                                          ) ls1221)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1225 => cvtSTATEMENT x1225
                                                     ) ls1226)), ("ctorDefn", 
          
       (case opt1231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1230 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1230))
       ))]
   and cvtINTERFACE_DEFN {ident=x1264, ns=opt1266, nonnullable=b1270, params=ls1272, 
          extends=ls1277, instanceDefns=ls1282} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1264), ("ns", 
       (case opt1266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1265 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1265))
       )), ("nonnullable", PrettyRep.Bool b1270), ("params", PrettyRep.List (List.map (fn x1271 => 
                                                                                             cvtIDENTIFIER x1271
                                                                                      ) ls1272)), 
          ("extends", PrettyRep.List (List.map (fn x1276 => cvtTYPE x1276
                                               ) ls1277)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1281 => cvtDEFN x1281
                                   ) ls1282))]
   and cvtTYPE_DEFN {ident=x1299, ns=opt1301, init=x1305} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1299), ("ns", 
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1300))
       )), ("init", cvtTYPE x1305)]
   and cvtCLASS_BLOCK {ns=opt1314, protectedNS=x1318, privateNS=x1319, ident=x1320, 
          name=opt1322, block=x1326} = PrettyRep.Rec [("ns", 
       (case opt1314 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1313))
       )), ("protectedNS", cvtNAMESPACE x1318), ("privateNS", cvtNAMESPACE x1319), 
          ("ident", cvtIDENTIFIER x1320), ("name", 
       (case opt1322 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1321 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1321))
       )), ("block", cvtBLOCK x1326)]
   and cvtFOR_ENUM_HEAD {isEach=b1340, bindings=(ls1342, ls1347), expr=x1352} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1340), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1341 => 
                                                                                                                         cvtBINDING x1341
                                                                                                                  ) ls1342), 
          PrettyRep.List (List.map (fn x1346 => cvtINIT_STEP x1346
                                   ) ls1347)]), ("expr", cvtEXPRESSION x1352)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1360, defn=opt1391, obj=x1395, rib=opt1403, 
          next=x1407, labels=ls1409, body=x1413} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1360), ("defn", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1361, ns=opt1363, static=b1367, prototype=b1368, bindings=(ls1370, 
            ls1375)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1361), ("ns", 
         (case opt1363 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1362 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1362))
         )), ("static", PrettyRep.Bool b1367), ("prototype", PrettyRep.Bool b1368), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1369 => 
                                                                          cvtBINDING x1369
                                                                   ) ls1370), 
            PrettyRep.List (List.map (fn x1374 => cvtINIT_STEP x1374
                                     ) ls1375)])]))
       )), ("obj", cvtEXPRESSION x1395), ("rib", 
       (case opt1403 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1399 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1396, 
                                                                                      x1397) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1396, 
                                                                                      cvtFIXTURE x1397]
                                                                               ) ls1399)))
       )), ("next", cvtSTATEMENT x1407), ("labels", PrettyRep.List (List.map (fn x1408 => 
                                                                                    cvtIDENTIFIER x1408
                                                                             ) ls1409)), 
          ("body", cvtSTATEMENT x1413)]
   and cvtFOR_STATEMENT {rib=opt1436, defn=opt1470, init=ls1475, cond=x1479, 
          update=x1480, labels=ls1482, body=x1486} = PrettyRep.Rec [("rib", 
          
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1432 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1429, 
                                                                                      x1430) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1429, 
                                                                                      cvtFIXTURE x1430]
                                                                               ) ls1432)))
       )), ("defn", 
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1440, ns=opt1442, static=b1446, prototype=b1447, bindings=(ls1449, 
            ls1454)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1440), ("ns", 
         (case opt1442 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1441 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1441))
         )), ("static", PrettyRep.Bool b1446), ("prototype", PrettyRep.Bool b1447), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1448 => 
                                                                          cvtBINDING x1448
                                                                   ) ls1449), 
            PrettyRep.List (List.map (fn x1453 => cvtINIT_STEP x1453
                                     ) ls1454)])]))
       )), ("init", PrettyRep.List (List.map (fn x1474 => cvtSTATEMENT x1474
                                             ) ls1475)), ("cond", cvtEXPRESSION x1479), 
          ("update", cvtEXPRESSION x1480), ("labels", PrettyRep.List (List.map (fn x1481 => 
                                                                                      cvtIDENTIFIER x1481
                                                                               ) ls1482)), 
          ("body", cvtSTATEMENT x1486)]
   and cvtWHILE_STATEMENT {cond=x1502, rib=opt1510, body=x1514, labels=ls1516} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1502), ("rib", 
       (case opt1510 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1506 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1503, 
                                                                                      x1504) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1503, 
                                                                                      cvtFIXTURE x1504]
                                                                               ) ls1506)))
       )), ("body", cvtSTATEMENT x1514), ("labels", PrettyRep.List (List.map (fn x1515 => 
                                                                                    cvtIDENTIFIER x1515
                                                                             ) ls1516))]
   and cvtDIRECTIVES {pragmas=ls1530, defns=ls1535, head=opt1540, body=ls1545, 
          loc=opt1550} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1529 => 
                                                                                    cvtPRAGMA x1529
                                                                             ) ls1530)), 
          ("defns", PrettyRep.List (List.map (fn x1534 => cvtDEFN x1534
                                             ) ls1535)), ("head", 
       (case opt1540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1539 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1539))
       )), ("body", PrettyRep.List (List.map (fn x1544 => cvtSTATEMENT x1544
                                             ) ls1545)), ("loc", 
       (case opt1550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1549))
       ))]
   and cvtCASE {label=opt1566, inits=opt1577, body=x1581} = PrettyRep.Rec [("label", 
          
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1565))
       )), ("inits", 
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1573 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1570, 
                                                                                      x1571) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1570, 
                                                                                      cvtEXPRESSION x1571]
                                                                               ) ls1573)))
       )), ("body", cvtBLOCK x1581)]
   and cvtCATCH_CLAUSE {bindings=(ls1590, ls1595), ty=x1600, rib=opt1608, inits=opt1619, 
          block=x1623} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1589 => 
                                                                                                      cvtBINDING x1589
                                                                                               ) ls1590), 
          PrettyRep.List (List.map (fn x1594 => cvtINIT_STEP x1594
                                   ) ls1595)]), ("ty", cvtTYPE x1600), ("rib", 
          
       (case opt1608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1604 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1601, 
                                                                                      x1602) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1601, 
                                                                                      cvtFIXTURE x1602]
                                                                               ) ls1604)))
       )), ("inits", 
       (case opt1619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1615 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1612, 
                                                                                      x1613) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1612, 
                                                                                      cvtEXPRESSION x1613]
                                                                               ) ls1615)))
       )), ("block", cvtBLOCK x1623)]
   and cvtFUNC_NAME {kind=x1635, ident=x1636} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1635), 
          ("ident", cvtIDENTIFIER x1636)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1642, getter=opt1644, setter=opt1649} = 
          PrettyRep.Rec [("ty", cvtTYPE x1642), ("getter", 
       (case opt1644 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1643 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1643))
       )), ("setter", 
       (case opt1649 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1648 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1648))
       ))]
   and cvtFRAGMENT (Anon x1660) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1660))
end

