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
     | cvtTYPE (UndefinedType) = PrettyRep.Ctor ("UndefinedType", NONE)
     | cvtTYPE (AnyType) = PrettyRep.Ctor ("AnyType", NONE)
     | cvtTYPE (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
     | cvtTYPE (ObjectType ls383) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x382 => 
                                                                                                        cvtFIELD_TYPE x382
                                                                                                 ) ls383)))
     | cvtTYPE (UnionType ls390) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x389 => 
                                                                                                      cvtTYPE x389
                                                                                               ) ls390)))
     | cvtTYPE (ArrayType ls397) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x396 => 
                                                                                                      cvtTYPE x396
                                                                                               ) ls397)))
     | cvtTYPE (FunctionType x403) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x403))
     | cvtTYPE (TypeName(x406, opt408)) = PrettyRep.Ctor ("TypeName", SOME (PrettyRep.Tuple [cvtIDENTIFIER_EXPRESSION x406, 
          
       (case opt408 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x407 => PrettyRep.Ctor ("SOME", SOME (cvtNONCE x407))
       )]))
     | cvtTYPE (ElementTypeRef(x415, n416)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x415, PrettyRep.Int n416]))
     | cvtTYPE (FieldTypeRef(x420, x421)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE x420, cvtIDENTIFIER x421]))
     | cvtTYPE (AppType{base=x425, args=ls427}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE x425), ("args", PrettyRep.List (List.map (fn x426 => 
                                                                                                cvtTYPE x426
                                                                                         ) ls427))]))
     | cvtTYPE (NullableType{expr=x438, nullable=b439}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE x438), ("nullable", PrettyRep.Bool b439)]))
     | cvtTYPE (InstanceType x447) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x447))
     | cvtTYPE (TypeVarFixtureRef x450) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtNONCE x450))
   and cvtSTATEMENT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTATEMENT (ExprStmt x454) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPRESSION x454))
     | cvtSTATEMENT (InitStmt{kind=x457, ns=opt459, prototype=b463, static=b464, 
          temps=x465, inits=ls467}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x457), ("ns", 
       (case opt459 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x458 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x458))
       )), ("prototype", PrettyRep.Bool b463), ("static", PrettyRep.Bool b464), 
          ("temps", cvtBINDINGS x465), ("inits", PrettyRep.List (List.map (fn x466 => 
                                                                                 cvtINIT_STEP x466
                                                                          ) ls467))]))
     | cvtSTATEMENT (ClassBlock x486) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x486))
     | cvtSTATEMENT (ForInStmt x489) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STATEMENT x489))
     | cvtSTATEMENT (ThrowStmt x492) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPRESSION x492))
     | cvtSTATEMENT (ReturnStmt x495) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPRESSION x495))
     | cvtSTATEMENT (BreakStmt opt499) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x498 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x498))
       ))
     | cvtSTATEMENT (ContinueStmt opt506) = PrettyRep.Ctor ("ContinueStmt", 
          SOME 
       (case opt506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x505 => PrettyRep.Ctor ("SOME", SOME (cvtIDENTIFIER x505))
       ))
     | cvtSTATEMENT (BlockStmt x512) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x512))
     | cvtSTATEMENT (LabeledStmt(x515, x516)) = PrettyRep.Ctor ("LabeledStmt", 
          SOME (PrettyRep.Tuple [cvtIDENTIFIER x515, cvtSTATEMENT x516]))
     | cvtSTATEMENT (LetStmt x520) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x520))
     | cvtSTATEMENT (WhileStmt x523) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STATEMENT x523))
     | cvtSTATEMENT (DoWhileStmt x526) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STATEMENT x526))
     | cvtSTATEMENT (ForStmt x529) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STATEMENT x529))
     | cvtSTATEMENT (IfStmt{cnd=x532, thn=x533, els=x534}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPRESSION x532), ("thn", cvtSTATEMENT x533), 
          ("els", cvtSTATEMENT x534)]))
     | cvtSTATEMENT (WithStmt{obj=x544, ty=x545, body=x546}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x544), ("ty", cvtTYPE x545), 
          ("body", cvtSTATEMENT x546)]))
     | cvtSTATEMENT (TryStmt{block=x556, catches=ls558, finally=opt563}) = 
          PrettyRep.Ctor ("TryStmt", SOME (PrettyRep.Rec [("block", cvtBLOCK x556), 
          ("catches", PrettyRep.List (List.map (fn x557 => cvtCATCH_CLAUSE x557
                                               ) ls558)), ("finally", 
       (case opt563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x562 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x562))
       ))]))
     | cvtSTATEMENT (SwitchStmt{cond=x576, labels=ls578, cases=ls583}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x576), ("labels", PrettyRep.List (List.map (fn x577 => 
                                                                                                        cvtIDENTIFIER x577
                                                                                                 ) ls578)), 
          ("cases", PrettyRep.List (List.map (fn x582 => cvtCASE x582
                                             ) ls583))]))
     | cvtSTATEMENT (SwitchTypeStmt{cond=x596, ty=x597, cases=ls599}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPRESSION x596), ("ty", cvtTYPE x597), 
          ("cases", PrettyRep.List (List.map (fn x598 => cvtCATCH_CLAUSE x598
                                             ) ls599))]))
     | cvtSTATEMENT (DXNStmt{expr=x612}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x612)]))
   and cvtEXPRESSION (TernaryExpr(x618, x619, x620)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x618, cvtEXPRESSION x619, cvtEXPRESSION x620]))
     | cvtEXPRESSION (BinaryExpr(x624, x625, x626)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x624, cvtEXPRESSION x625, cvtEXPRESSION x626]))
     | cvtEXPRESSION (BinaryTypeExpr(x630, x631, x632)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x630, cvtEXPRESSION x631, cvtTYPE x632]))
     | cvtEXPRESSION (UnaryExpr(x636, x637)) = PrettyRep.Ctor ("UnaryExpr", 
          SOME (PrettyRep.Tuple [cvtUNOP x636, cvtEXPRESSION x637]))
     | cvtEXPRESSION (TypeExpr x641) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE x641))
     | cvtEXPRESSION (ThisExpr opt645) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x644 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x644))
       ))
     | cvtEXPRESSION (YieldExpr opt652) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt652 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x651 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x651))
       ))
     | cvtEXPRESSION (SuperExpr opt659) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x658 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x658))
       ))
     | cvtEXPRESSION (CallExpr{func=x665, actuals=ls667}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPRESSION x665), ("actuals", PrettyRep.List (List.map (fn x666 => 
                                                                                                         cvtEXPRESSION x666
                                                                                                  ) ls667))]))
     | cvtEXPRESSION (ApplyTypeExpr{expr=x678, actuals=ls680}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPRESSION x678), ("actuals", PrettyRep.List (List.map (fn x679 => 
                                                                                                         cvtTYPE x679
                                                                                                  ) ls680))]))
     | cvtEXPRESSION (LetExpr{defs=x691, body=x692, head=opt694}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x691), ("body", cvtEXPRESSION x692), 
          ("head", 
       (case opt694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x693 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x693))
       ))]))
     | cvtEXPRESSION (NewExpr{obj=x707, actuals=ls709}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPRESSION x707), ("actuals", PrettyRep.List (List.map (fn x708 => 
                                                                                                        cvtEXPRESSION x708
                                                                                                 ) ls709))]))
     | cvtEXPRESSION (ObjectRef{base=x720, ident=x721, loc=opt723}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPRESSION x720), ("ident", cvtIDENTIFIER_EXPRESSION x721), 
          ("loc", 
       (case opt723 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x722 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x722))
       ))]))
     | cvtEXPRESSION (LexicalRef{ident=x736, loc=opt738}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENTIFIER_EXPRESSION x736), ("loc", 
          
       (case opt738 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x737 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x737))
       ))]))
     | cvtEXPRESSION (SetExpr(x749, x750, x751)) = PrettyRep.Ctor ("SetExpr", 
          SOME (PrettyRep.Tuple [cvtASSIGNOP x749, cvtEXPRESSION x750, cvtEXPRESSION x751]))
     | cvtEXPRESSION (ListExpr ls756) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x755 => 
                                                                                                          cvtEXPRESSION x755
                                                                                                   ) ls756)))
     | cvtEXPRESSION (InitExpr(x762, x763, x764)) = PrettyRep.Ctor ("InitExpr", 
          SOME (PrettyRep.Tuple [cvtINIT_TARGET x762, cvtHEAD x763, cvtINITS x764]))
     | cvtEXPRESSION (GetTemp n768) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n768))
     | cvtEXPRESSION (GetParam n771) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n771))
     | cvtEXPRESSION (Comprehension(x774, ls776, opt781)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPRESSION x774, PrettyRep.List (List.map (fn x775 => 
                                                                                     cvtFOR_ENUM_HEAD x775
                                                                              ) ls776), 
          
       (case opt781 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x780 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x780))
       )]))
     | cvtEXPRESSION (LiteralExpr x788) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x788))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n796) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n796))
     | cvtFIXTURE_NAME (PropName x799) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x799))
   and cvtIDENTIFIER_EXPRESSION (Identifier{ident=x802, openNamespaces=ls808, 
          rootRib=opt813}) = PrettyRep.Ctor ("Identifier", SOME (PrettyRep.Rec [("ident", 
          cvtIDENTIFIER x802), ("openNamespaces", PrettyRep.List (List.map (fn ls804 => 
                                                                                  PrettyRep.List (List.map (fn x803 => 
                                                                                                                  cvtNAMESPACE x803
                                                                                                           ) ls804)
                                                                           ) ls808)), 
          ("rootRib", 
       (case opt813 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x812 => PrettyRep.Ctor ("SOME", SOME (cvtRIB x812))
       ))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedExpression{qual=x826, expr=x827}) = 
          PrettyRep.Ctor ("QualifiedExpression", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x826), ("expr", cvtEXPRESSION x827)]))
     | cvtIDENTIFIER_EXPRESSION (ExpressionIdentifier{expr=x835, openNamespaces=ls841}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPRESSION x835), ("openNamespaces", PrettyRep.List (List.map (fn ls837 => 
                                                                                  PrettyRep.List (List.map (fn x836 => 
                                                                                                                  cvtNAMESPACE x836
                                                                                                           ) ls837)
                                                                           ) ls841))]))
     | cvtIDENTIFIER_EXPRESSION (QualifiedIdentifier{qual=x852, ident=s853}) = 
          PrettyRep.Ctor ("QualifiedIdentifier", SOME (PrettyRep.Rec [("qual", 
          cvtEXPRESSION x852), ("ident", PrettyRep.UniStr s853)]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r863) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r863))
     | cvtLITERAL (LiteralDecimal d866) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d866))
     | cvtLITERAL (LiteralBoolean b869) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b869))
     | cvtLITERAL (LiteralString s872) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s872))
     | cvtLITERAL (LiteralArray{exprs=x875, ty=opt877}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPRESSION x875), ("ty", 
       (case opt877 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x876 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x876))
       ))]))
     | cvtLITERAL (LiteralXML ls889) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x888 => 
                                                                                                           cvtEXPRESSION x888
                                                                                                    ) ls889)))
     | cvtLITERAL (LiteralNamespace x895) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x895))
     | cvtLITERAL (LiteralObject{expr=ls899, ty=opt904}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x898 => 
                                                                        cvtFIELD x898
                                                                 ) ls899)), 
          ("ty", 
       (case opt904 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x903 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE x903))
       ))]))
     | cvtLITERAL (LiteralFunction x915) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x915))
     | cvtLITERAL (LiteralRegExp{str=s918}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s918)]))
   and cvtBLOCK (Block x924) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x924))
   and cvtFIXTURE (NamespaceFixture x927) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x927))
     | cvtFIXTURE (ClassFixture x930) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x930))
     | cvtFIXTURE (InterfaceFixture x933) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x933))
     | cvtFIXTURE (TypeVarFixture x936) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtNONCE x936))
     | cvtFIXTURE (TypeFixture(ls940, x944)) = PrettyRep.Ctor ("TypeFixture", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x939 => cvtIDENTIFIER x939
                                                          ) ls940), cvtTYPE x944]))
     | cvtFIXTURE (MethodFixture{func=x948, ty=x949, readOnly=b950, override=b951, 
          final=b952}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x948), ("ty", cvtTYPE x949), ("readOnly", PrettyRep.Bool b950), 
          ("override", PrettyRep.Bool b951), ("final", PrettyRep.Bool b952)]))
     | cvtFIXTURE (ValFixture{ty=x966, readOnly=b967}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE x966), ("readOnly", PrettyRep.Bool b967)]))
     | cvtFIXTURE (VirtualValFixture{ty=x975, getter=opt977, setter=opt982}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE x975), ("getter", 
       (case opt977 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x976 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x976))
       )), ("setter", 
       (case opt982 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x981 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x981))
       ))]))
   and cvtHEAD (Head(x995, x996)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x995, 
          cvtINITS x996]))
   and cvtBINDINGS (ls1001, ls1006) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1000 => 
                                                                                       cvtBINDING x1000
                                                                                ) ls1001), 
          PrettyRep.List (List.map (fn x1005 => cvtINIT_STEP x1005
                                   ) ls1006)]
   and cvtRIB ls1014 = PrettyRep.List (List.map (fn (x1011, x1012) => PrettyRep.Tuple [cvtFIXTURE_NAME x1011, 
                                                       cvtFIXTURE x1012]
                                                ) ls1014)
   and cvtRIBS ls1025 = PrettyRep.List (List.map (fn ls1021 => PrettyRep.List (List.map (fn (x1018, 
                                                                                               x1019) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1018, 
                                                                                               cvtFIXTURE x1019]
                                                                                        ) ls1021)
                                                 ) ls1025)
   and cvtINITS ls1032 = PrettyRep.List (List.map (fn (x1029, x1030) => PrettyRep.Tuple [cvtFIXTURE_NAME x1029, 
                                                         cvtEXPRESSION x1030]
                                                  ) ls1032)
   and cvtINSTANCE_TYPE {name=x1036, typeParams=ls1038, typeArgs=ls1043, nonnullable=b1047, 
          superTypes=ls1049, ty=x1053, dynamic=b1054} = PrettyRep.Rec [("name", 
          cvtNAME x1036), ("typeParams", PrettyRep.List (List.map (fn x1037 => 
                                                                         cvtIDENTIFIER x1037
                                                                  ) ls1038)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1042 => cvtTYPE x1042
                                                ) ls1043)), ("nonnullable", 
          PrettyRep.Bool b1047), ("superTypes", PrettyRep.List (List.map (fn x1048 => 
                                                                                cvtTYPE x1048
                                                                         ) ls1049)), 
          ("ty", cvtTYPE x1053), ("dynamic", PrettyRep.Bool b1054)]
   and cvtFIELD {kind=x1070, name=x1071, init=x1072} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1070), ("name", cvtIDENTIFIER_EXPRESSION x1071), 
          ("init", cvtEXPRESSION x1072)]
   and cvtFIELD_TYPE {name=x1080, ty=x1081} = PrettyRep.Rec [("name", cvtIDENTIFIER x1080), 
          ("ty", cvtTYPE x1081)]
   and cvtFUNC_TYPE {typeParams=ls1088, thisType=x1092, params=ls1094, minArgs=n1098, 
          hasRest=b1099, result=x1100} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1087 => 
                                                                                                       cvtIDENTIFIER x1087
                                                                                                ) ls1088)), 
          ("thisType", cvtTYPE x1092), ("params", PrettyRep.List (List.map (fn x1093 => 
                                                                                  cvtTYPE x1093
                                                                           ) ls1094)), 
          ("minArgs", PrettyRep.Int n1098), ("hasRest", PrettyRep.Bool b1099), 
          ("result", cvtTYPE x1100)]
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
   and cvtTYPE_DEFN {ident=x1299, ns=opt1301, typeParams=ls1306, init=x1310} = 
          PrettyRep.Rec [("ident", cvtIDENTIFIER x1299), ("ns", 
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1300))
       )), ("typeParams", PrettyRep.List (List.map (fn x1305 => cvtIDENTIFIER x1305
                                                   ) ls1306)), ("init", cvtTYPE x1310)]
   and cvtCLASS_BLOCK {ns=opt1321, protectedNS=x1325, privateNS=x1326, ident=x1327, 
          name=opt1329, block=x1333} = PrettyRep.Rec [("ns", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1320))
       )), ("protectedNS", cvtNAMESPACE x1325), ("privateNS", cvtNAMESPACE x1326), 
          ("ident", cvtIDENTIFIER x1327), ("name", 
       (case opt1329 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1328 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1328))
       )), ("block", cvtBLOCK x1333)]
   and cvtFOR_ENUM_HEAD {isEach=b1347, bindings=(ls1349, ls1354), expr=x1359} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1347), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1348 => 
                                                                                                                         cvtBINDING x1348
                                                                                                                  ) ls1349), 
          PrettyRep.List (List.map (fn x1353 => cvtINIT_STEP x1353
                                   ) ls1354)]), ("expr", cvtEXPRESSION x1359)]
   and cvtFOR_ENUM_STATEMENT {isEach=b1367, defn=opt1398, obj=x1402, rib=opt1410, 
          next=x1414, labels=ls1416, body=x1420} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1367), ("defn", 
       (case opt1398 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1368, ns=opt1370, static=b1374, prototype=b1375, bindings=(ls1377, 
            ls1382)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1368), ("ns", 
         (case opt1370 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1369 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1369))
         )), ("static", PrettyRep.Bool b1374), ("prototype", PrettyRep.Bool b1375), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1376 => 
                                                                          cvtBINDING x1376
                                                                   ) ls1377), 
            PrettyRep.List (List.map (fn x1381 => cvtINIT_STEP x1381
                                     ) ls1382)])]))
       )), ("obj", cvtEXPRESSION x1402), ("rib", 
       (case opt1410 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1406 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1403, 
                                                                                      x1404) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1403, 
                                                                                      cvtFIXTURE x1404]
                                                                               ) ls1406)))
       )), ("next", cvtSTATEMENT x1414), ("labels", PrettyRep.List (List.map (fn x1415 => 
                                                                                    cvtIDENTIFIER x1415
                                                                             ) ls1416)), 
          ("body", cvtSTATEMENT x1420)]
   and cvtFOR_STATEMENT {rib=opt1443, defn=opt1477, init=ls1482, cond=x1486, 
          update=x1487, labels=ls1489, body=x1493} = PrettyRep.Rec [("rib", 
          
       (case opt1443 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1439 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1436, 
                                                                                      x1437) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1436, 
                                                                                      cvtFIXTURE x1437]
                                                                               ) ls1439)))
       )), ("defn", 
       (case opt1477 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1447, ns=opt1449, static=b1453, prototype=b1454, bindings=(ls1456, 
            ls1461)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1447), ("ns", 
         (case opt1449 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1448 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1448))
         )), ("static", PrettyRep.Bool b1453), ("prototype", PrettyRep.Bool b1454), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1455 => 
                                                                          cvtBINDING x1455
                                                                   ) ls1456), 
            PrettyRep.List (List.map (fn x1460 => cvtINIT_STEP x1460
                                     ) ls1461)])]))
       )), ("init", PrettyRep.List (List.map (fn x1481 => cvtSTATEMENT x1481
                                             ) ls1482)), ("cond", cvtEXPRESSION x1486), 
          ("update", cvtEXPRESSION x1487), ("labels", PrettyRep.List (List.map (fn x1488 => 
                                                                                      cvtIDENTIFIER x1488
                                                                               ) ls1489)), 
          ("body", cvtSTATEMENT x1493)]
   and cvtWHILE_STATEMENT {cond=x1509, rib=opt1517, body=x1521, labels=ls1523} = 
          PrettyRep.Rec [("cond", cvtEXPRESSION x1509), ("rib", 
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1513 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1510, 
                                                                                      x1511) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1510, 
                                                                                      cvtFIXTURE x1511]
                                                                               ) ls1513)))
       )), ("body", cvtSTATEMENT x1521), ("labels", PrettyRep.List (List.map (fn x1522 => 
                                                                                    cvtIDENTIFIER x1522
                                                                             ) ls1523))]
   and cvtDIRECTIVES {pragmas=ls1537, defns=ls1542, head=opt1547, body=ls1552, 
          loc=opt1557} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1536 => 
                                                                                    cvtPRAGMA x1536
                                                                             ) ls1537)), 
          ("defns", PrettyRep.List (List.map (fn x1541 => cvtDEFN x1541
                                             ) ls1542)), ("head", 
       (case opt1547 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1546 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1546))
       )), ("body", PrettyRep.List (List.map (fn x1551 => cvtSTATEMENT x1551
                                             ) ls1552)), ("loc", 
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1556))
       ))]
   and cvtCASE {label=opt1573, inits=opt1584, body=x1588} = PrettyRep.Rec [("label", 
          
       (case opt1573 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1572 => PrettyRep.Ctor ("SOME", SOME (cvtEXPRESSION x1572))
       )), ("inits", 
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1580 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1577, 
                                                                                      x1578) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1577, 
                                                                                      cvtEXPRESSION x1578]
                                                                               ) ls1580)))
       )), ("body", cvtBLOCK x1588)]
   and cvtCATCH_CLAUSE {bindings=(ls1597, ls1602), ty=x1607, rib=opt1615, inits=opt1626, 
          block=x1630} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1596 => 
                                                                                                      cvtBINDING x1596
                                                                                               ) ls1597), 
          PrettyRep.List (List.map (fn x1601 => cvtINIT_STEP x1601
                                   ) ls1602)]), ("ty", cvtTYPE x1607), ("rib", 
          
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1611 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1608, 
                                                                                      x1609) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1608, 
                                                                                      cvtFIXTURE x1609]
                                                                               ) ls1611)))
       )), ("inits", 
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1622 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1619, 
                                                                                      x1620) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1619, 
                                                                                      cvtEXPRESSION x1620]
                                                                               ) ls1622)))
       )), ("block", cvtBLOCK x1630)]
   and cvtFUNC_NAME {kind=x1642, ident=x1643} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1642), 
          ("ident", cvtIDENTIFIER x1643)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1649, getter=opt1651, setter=opt1656} = 
          PrettyRep.Rec [("ty", cvtTYPE x1649), ("getter", 
       (case opt1651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1650 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1650))
       )), ("setter", 
       (case opt1656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1655 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1655))
       ))]
   and cvtFRAGMENT (Anon x1667) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1667))
end

