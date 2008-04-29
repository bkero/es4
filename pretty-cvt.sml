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
   and cvtFUNC_TYPE {params=ls1095, result=x1099, thisType=opt1101, hasRest=b1105, 
          minArgs=n1106} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1094 => 
                                                                                     cvtTYPE x1094
                                                                              ) ls1095)), 
          ("result", cvtTYPE x1099), ("thisType", 
       (case opt1101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1100 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1100))
       )), ("hasRest", PrettyRep.Bool b1105), ("minArgs", PrettyRep.Int n1106)]
   and cvtFUNC_DEFN {kind=x1118, ns=opt1120, final=b1124, override=b1125, prototype=b1126, 
          static=b1127, func=x1128} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1118), 
          ("ns", 
       (case opt1120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1119 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1119))
       )), ("final", PrettyRep.Bool b1124), ("override", PrettyRep.Bool b1125), 
          ("prototype", PrettyRep.Bool b1126), ("static", PrettyRep.Bool b1127), 
          ("func", cvtFUNC x1128)]
   and cvtCTOR_DEFN x1144 = cvtCTOR x1144
   and cvtVAR_DEFN {kind=x1145, ns=opt1147, static=b1151, prototype=b1152, 
          bindings=(ls1154, ls1159)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1145), 
          ("ns", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1146))
       )), ("static", PrettyRep.Bool b1151), ("prototype", PrettyRep.Bool b1152), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1153 => 
                                                                        cvtBINDING x1153
                                                                 ) ls1154), 
          PrettyRep.List (List.map (fn x1158 => cvtINIT_STEP x1158
                                   ) ls1159)])]
   and cvtNAMESPACE_DEFN {ident=x1175, ns=opt1177, init=opt1182} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1175), ("ns", 
       (case opt1177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1176 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1176))
       )), ("init", 
       (case opt1182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1181 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1181))
       ))]
   and cvtCLASS_DEFN {ns=opt1194, privateNS=x1198, protectedNS=x1199, ident=x1200, 
          nonnullable=b1201, dynamic=b1202, final=b1203, params=ls1205, extends=opt1210, 
          implements=ls1215, classDefns=ls1220, instanceDefns=ls1225, instanceStmts=ls1230, 
          ctorDefn=opt1235} = PrettyRep.Rec [("ns", 
       (case opt1194 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1193 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1193))
       )), ("privateNS", cvtNAMESPACE x1198), ("protectedNS", cvtNAMESPACE x1199), 
          ("ident", cvtIDENTIFIER x1200), ("nonnullable", PrettyRep.Bool b1201), 
          ("dynamic", PrettyRep.Bool b1202), ("final", PrettyRep.Bool b1203), 
          ("params", PrettyRep.List (List.map (fn x1204 => cvtIDENTIFIER x1204
                                              ) ls1205)), ("extends", 
       (case opt1210 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1209 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x1209))
       )), ("implements", PrettyRep.List (List.map (fn x1214 => cvtTYPE x1214
                                                   ) ls1215)), ("classDefns", 
          PrettyRep.List (List.map (fn x1219 => cvtDEFN x1219
                                   ) ls1220)), ("instanceDefns", PrettyRep.List (List.map (fn x1224 => 
                                                                                                 cvtDEFN x1224
                                                                                          ) ls1225)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1229 => cvtSTATEMENT x1229
                                                     ) ls1230)), ("ctorDefn", 
          
       (case opt1235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1234 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1234))
       ))]
   and cvtINTERFACE_DEFN {ident=x1268, ns=opt1270, nonnullable=b1274, params=ls1276, 
          extends=ls1281, instanceDefns=ls1286} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1268), ("ns", 
       (case opt1270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1269))
       )), ("nonnullable", PrettyRep.Bool b1274), ("params", PrettyRep.List (List.map (fn x1275 => 
                                                                                             cvtIDENTIFIER x1275
                                                                                      ) ls1276)), 
          ("extends", PrettyRep.List (List.map (fn x1280 => cvtTYPE x1280
                                               ) ls1281)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1285 => cvtDEFN x1285
                                   ) ls1286))]
   and cvtTYPE_DEFN {ident=x1303, ns=opt1305, init=x1309} = PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x1303), ("ns", 
       (case opt1305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1304 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1304))
       )), ("init", cvtTYPE x1309)]
   and cvtCLASS_BLOCK {ns=opt1318, protectedNS=x1322, privateNS=x1323, ident=x1324, 
          name=opt1326, block=x1330} = PrettyRep.Rec [("ns", 
       (case opt1318 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1317 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1317))
       )), ("protectedNS", cvtNAMESPACE x1322), ("privateNS", cvtNAMESPACE x1323), 
          ("ident", cvtIDENTIFIER x1324), ("name", 
       (case opt1326 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1325 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1325))
       )), ("block", cvtBLOCK x1330)]
   and cvtFOR_ENUM_HEAD {isEach=b1344, bindings=(ls1346, ls1351), expr=x1356} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1344), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1345 => 
                                                                                                                         cvtBINDING x1345
                                                                                                                  ) ls1346), 
          PrettyRep.List (List.map (fn x1350 => cvtINIT_STEP x1350
                                   ) ls1351)]), ("expr", cvtEXPRESSION x1356)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1364, defn=opt1395, obj=x1399, rib=opt1407, 
          next=x1411, labels=ls1413, body=x1417} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1364), ("defn", 
       (case opt1395 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1365, ns=opt1367, static=b1371, prototype=b1372, bindings=(ls1374, 
            ls1379)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1365), ("ns", 
         (case opt1367 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1366))
         )), ("static", PrettyRep.Bool b1371), ("prototype", PrettyRep.Bool b1372), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1373 => 
                                                                          cvtBINDING x1373
                                                                   ) ls1374), 
            PrettyRep.List (List.map (fn x1378 => cvtINIT_STEP x1378
                                     ) ls1379)])]))
       )), ("obj", cvtEXPRESSION x1399), ("rib", 
       (case opt1407 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1403 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1400, 
                                                                                      x1401) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1400, 
                                                                                      cvtFIXTURE x1401]
                                                                               ) ls1403)))
       )), ("next", cvtSTATEMENT x1411), ("labels", PrettyRep.List (List.map (fn x1412 => 
                                                                                    cvtIDENTIFIER x1412
                                                                             ) ls1413)), 
          ("body", cvtSTATEMENT x1417)]
   and cvtFOR_STATEMENT {rib=opt1440, defn=opt1474, init=ls1479, cond=x1483, 
          update=x1484, labels=ls1486, body=x1490} = PrettyRep.Rec [("rib", 
          
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1436 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1433, 
                                                                                      x1434) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1433, 
                                                                                      cvtFIXTURE x1434]
                                                                               ) ls1436)))
       )), ("defn", 
       (case opt1474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1444, ns=opt1446, static=b1450, prototype=b1451, bindings=(ls1453, 
            ls1458)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1444), ("ns", 
         (case opt1446 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1445 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1445))
         )), ("static", PrettyRep.Bool b1450), ("prototype", PrettyRep.Bool b1451), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1452 => 
                                                                          cvtBINDING x1452
                                                                   ) ls1453), 
            PrettyRep.List (List.map (fn x1457 => cvtINIT_STEP x1457
                                     ) ls1458)])]))
       )), ("init", PrettyRep.List (List.map (fn x1478 => cvtSTATEMENT x1478
                                             ) ls1479)), ("cond", cvtEXPRESSION x1483), 
          ("update", cvtEXPRESSION x1484), ("labels", PrettyRep.List (List.map (fn x1485 => 
                                                                                      cvtIDENTIFIER x1485
                                                                               ) ls1486)), 
          ("body", cvtSTATEMENT x1490)]
   and cvtWHILE_STATEMENT {cond=x1506, rib=opt1514, body=x1518, labels=ls1520} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1506), ("rib", 
       (case opt1514 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1510 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1507, 
                                                                                      x1508) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1507, 
                                                                                      cvtFIXTURE x1508]
                                                                               ) ls1510)))
       )), ("body", cvtSTATEMENT x1518), ("labels", PrettyRep.List (List.map (fn x1519 => 
                                                                                    cvtIDENTIFIER x1519
                                                                             ) ls1520))]
   and cvtDIRECTIVES {pragmas=ls1534, defns=ls1539, head=opt1544, body=ls1549, 
          loc=opt1554} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1533 => 
                                                                                    cvtPRAGMA x1533
                                                                             ) ls1534)), 
          ("defns", PrettyRep.List (List.map (fn x1538 => cvtDEFN x1538
                                             ) ls1539)), ("head", 
       (case opt1544 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1543 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1543))
       )), ("body", PrettyRep.List (List.map (fn x1548 => cvtSTATEMENT x1548
                                             ) ls1549)), ("loc", 
       (case opt1554 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1553 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1553))
       ))]
   and cvtCASE {label=opt1570, inits=opt1581, body=x1585} = PrettyRep.Rec [("label", 
          
       (case opt1570 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1569 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1569))
       )), ("inits", 
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1577 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1574, 
                                                                                      x1575) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1574, 
                                                                                      cvtEXPRESSION x1575]
                                                                               ) ls1577)))
       )), ("body", cvtBLOCK x1585)]
   and cvtCATCH_CLAUSE {bindings=(ls1594, ls1599), ty=x1604, rib=opt1612, inits=opt1623, 
          block=x1627} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1593 => 
                                                                                                      cvtBINDING x1593
                                                                                               ) ls1594), 
          PrettyRep.List (List.map (fn x1598 => cvtINIT_STEP x1598
                                   ) ls1599)]), ("ty", cvtTYPE x1604), ("rib", 
          
       (case opt1612 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1608 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1605, 
                                                                                      x1606) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1605, 
                                                                                      cvtFIXTURE x1606]
                                                                               ) ls1608)))
       )), ("inits", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1619 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1616, 
                                                                                      x1617) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1616, 
                                                                                      cvtEXPRESSION x1617]
                                                                               ) ls1619)))
       )), ("block", cvtBLOCK x1627)]
   and cvtFUNC_NAME {kind=x1639, ident=x1640} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1639), 
          ("ident", cvtIDENTIFIER x1640)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1646, getter=opt1648, setter=opt1653} = 
          PrettyRep.Rec [("ty", cvtTYPE x1646), ("getter", 
       (case opt1648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1647 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1647))
       )), ("setter", 
       (case opt1653 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1652 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1652))
       ))]
   and cvtFRAGMENT (Anon x1664) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1664))
end

