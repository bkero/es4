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
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x28) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Protected x31) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x31))
     | cvtNAMESPACE (Public x34) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x34))
     | cvtNAMESPACE (Internal x37) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x37))
     | cvtNAMESPACE (UserNamespace s40) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s40))
     | cvtNAMESPACE (AnonUserNamespace n43) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n43))
     | cvtNAMESPACE (LimitedNamespace(x46, x47)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x46, cvtNAMESPACE x47]))
   and cvtNAME {ns=x51, id=x52} = PrettyRep.Rec [("ns", cvtNAMESPACE x51), 
          ("id", cvtIDENT x52)]
   and cvtMULTINAME {nss=ls63, id=x67} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls59 => 
                                                                                                PrettyRep.List (List.map (fn x58 => 
                                                                                                                                cvtNAMESPACE x58
                                                                                                                         ) ls59)
                                                                                         ) ls63)), 
          ("id", cvtIDENT x67)]
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
   and cvtPRAGMA (UseNamespace x134) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x134))
     | cvtPRAGMA (UseDefaultNamespace x137) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x137))
     | cvtPRAGMA (UseDecimalContext x140) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x140))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls146, name=x150, alias=opt152}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x145 => 
                                                                           cvtIDENT x145
                                                                    ) ls146)), 
          ("name", cvtIDENT x150), ("alias", 
       (case opt152 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x151 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x151))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x171, ribId=opt173}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x171), ("ribId", 
       (case opt173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x172 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x172))
       ))]))
   and cvtCLS (Cls{name=x184, typeParams=ls186, nonnullable=b190, dynamic=b191, 
          extends=opt193, implements=ls198, classRib=x202, instanceRib=x203, 
          instanceInits=x204, constructor=opt206, classType=x210, instanceType=x211}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x184), 
          ("typeParams", PrettyRep.List (List.map (fn x185 => cvtIDENT x185
                                                  ) ls186)), ("nonnullable", 
          PrettyRep.Bool b190), ("dynamic", PrettyRep.Bool b191), ("extends", 
          
       (case opt193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x192 => PrettyRep.Ctor ("SOME", SOME (cvtTY x192))
       )), ("implements", PrettyRep.List (List.map (fn x197 => cvtTY x197
                                                   ) ls198)), ("classRib", 
          cvtRIB x202), ("instanceRib", cvtRIB x203), ("instanceInits", cvtHEAD x204), 
          ("constructor", 
       (case opt206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x205 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x205))
       )), ("classType", cvtTY x210), ("instanceType", cvtTY x211)]))
   and cvtIFACE (Iface{name=x239, typeParams=ls241, nonnullable=b245, extends=ls247, 
          instanceRib=x251, instanceType=x252}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x239), ("typeParams", PrettyRep.List (List.map (fn x240 => 
                                                                                                      cvtIDENT x240
                                                                                               ) ls241)), 
          ("nonnullable", PrettyRep.Bool b245), ("extends", PrettyRep.List (List.map (fn x246 => 
                                                                                            cvtTY x246
                                                                                     ) ls247)), 
          ("instanceRib", cvtRIB x251), ("instanceType", cvtTY x252)]))
   and cvtCTOR (Ctor{settings=x268, superArgs=ls270, func=x274}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x268), ("superArgs", PrettyRep.List (List.map (fn x269 => 
                                                                                                         cvtEXPR x269
                                                                                                  ) ls270)), 
          ("func", cvtFUNC x274)]))
   and cvtFUNC (Func{name=x284, fsig=x285, native=b286, block=opt288, param=x292, 
          defaults=ls294, ty=x298, loc=opt300}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x284), ("fsig", cvtFUNC_SIG x285), ("native", PrettyRep.Bool b286), 
          ("block", 
       (case opt288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x287 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x287))
       )), ("param", cvtHEAD x292), ("defaults", PrettyRep.List (List.map (fn x293 => 
                                                                                 cvtEXPR x293
                                                                          ) ls294)), 
          ("ty", cvtTY x298), ("loc", 
       (case opt300 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x299 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x299))
       ))]))
   and cvtDEFN (ClassDefn x323) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x323))
     | cvtDEFN (VariableDefn x326) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x326))
     | cvtDEFN (FunctionDefn x329) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x329))
     | cvtDEFN (ConstructorDefn x332) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x332))
     | cvtDEFN (InterfaceDefn x335) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x335))
     | cvtDEFN (NamespaceDefn x338) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x338))
     | cvtDEFN (TypeDefn x341) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x341))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls345, params=x349, paramTypes=ls351, 
          defaults=ls356, ctorInits=opt367, returnType=x371, thisType=opt373, 
          hasRest=b377}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x344 => cvtIDENT x344
                                   ) ls345)), ("params", cvtBINDINGS x349), 
          ("paramTypes", PrettyRep.List (List.map (fn x350 => cvtTYPE_EXPR x350
                                                  ) ls351)), ("defaults", PrettyRep.List (List.map (fn x355 => 
                                                                                                          cvtEXPR x355
                                                                                                   ) ls356)), 
          ("ctorInits", 
       (case opt367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x360, ls362) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x360, 
            PrettyRep.List (List.map (fn x361 => cvtEXPR x361
                                     ) ls362)]))
       )), ("returnType", cvtTYPE_EXPR x371), ("thisType", 
       (case opt373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x372 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x372))
       )), ("hasRest", PrettyRep.Bool b377)]))
   and cvtBINDING (Binding{ident=x397, ty=x398}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x397), ("ty", cvtTYPE_EXPR x398)]))
   and cvtBINDING_IDENT (TempIdent n406) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n406))
     | cvtBINDING_IDENT (ParamIdent n409) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n409))
     | cvtBINDING_IDENT (PropIdent x412) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x412))
   and cvtINIT_STEP (InitStep(x415, x416)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x415, 
          cvtEXPR x416]))
     | cvtINIT_STEP (AssignStep(x420, x421)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x420, cvtEXPR x421]))
   and cvtTYPE_EXPR (SpecialType x425) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x425))
     | cvtTYPE_EXPR (UnionType ls429) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x428 => 
                                                                                                           cvtTYPE_EXPR x428
                                                                                                    ) ls429)))
     | cvtTYPE_EXPR (ArrayType ls436) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x435 => 
                                                                                                           cvtTYPE_EXPR x435
                                                                                                    ) ls436)))
     | cvtTYPE_EXPR (TypeName x442) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x442))
     | cvtTYPE_EXPR (ElementTypeRef(x445, n446)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x445, PrettyRep.Int n446]))
     | cvtTYPE_EXPR (FieldTypeRef(x450, x451)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x450, cvtIDENT x451]))
     | cvtTYPE_EXPR (FunctionType x455) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x455))
     | cvtTYPE_EXPR (ObjectType ls459) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x458 => 
                                                                                                             cvtFIELD_TYPE x458
                                                                                                      ) ls459)))
     | cvtTYPE_EXPR (AppType{base=x465, args=ls467}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x465), ("args", PrettyRep.List (List.map (fn x466 => 
                                                                                                     cvtTYPE_EXPR x466
                                                                                              ) ls467))]))
     | cvtTYPE_EXPR (LamType{params=ls479, body=x483}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x478 => 
                                                                          cvtIDENT x478
                                                                   ) ls479)), 
          ("body", cvtTYPE_EXPR x483)]))
     | cvtTYPE_EXPR (NullableType{expr=x491, nullable=b492}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x491), ("nullable", PrettyRep.Bool b492)]))
     | cvtTYPE_EXPR (InstanceType x500) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x500))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x504) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x504))
     | cvtSTMT (InitStmt{kind=x507, ns=opt509, prototype=b513, static=b514, 
          temps=x515, inits=ls517}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x507), ("ns", 
       (case opt509 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x508 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x508))
       )), ("prototype", PrettyRep.Bool b513), ("static", PrettyRep.Bool b514), 
          ("temps", cvtBINDINGS x515), ("inits", PrettyRep.List (List.map (fn x516 => 
                                                                                 cvtINIT_STEP x516
                                                                          ) ls517))]))
     | cvtSTMT (ClassBlock{ns=opt537, ident=x541, name=opt543, block=x547}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt537 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x536 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x536))
       )), ("ident", cvtIDENT x541), ("name", 
       (case opt543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x542 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x542))
       )), ("block", cvtBLOCK x547)]))
     | cvtSTMT (ForInStmt x559) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x559))
     | cvtSTMT (ThrowStmt x562) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x562))
     | cvtSTMT (ReturnStmt x565) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x565))
     | cvtSTMT (BreakStmt opt569) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x568 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x568))
       ))
     | cvtSTMT (ContinueStmt opt576) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x575 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x575))
       ))
     | cvtSTMT (BlockStmt x582) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x582))
     | cvtSTMT (LabeledStmt(x585, x586)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x585, 
          cvtSTMT x586]))
     | cvtSTMT (LetStmt x590) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x590))
     | cvtSTMT (WhileStmt x593) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x593))
     | cvtSTMT (DoWhileStmt x596) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x596))
     | cvtSTMT (ForStmt x599) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x599))
     | cvtSTMT (IfStmt{cnd=x602, thn=x603, els=x604}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x602), ("thn", cvtSTMT x603), 
          ("els", cvtSTMT x604)]))
     | cvtSTMT (WithStmt{obj=x614, ty=x615, body=x616}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x614), ("ty", cvtTY x615), ("body", 
          cvtSTMT x616)]))
     | cvtSTMT (TryStmt{block=x626, catches=ls628, finally=opt633}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x626), ("catches", PrettyRep.List (List.map (fn x627 => 
                                                                                                     cvtCATCH_CLAUSE x627
                                                                                              ) ls628)), 
          ("finally", 
       (case opt633 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x632 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x632))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x646, labels=ls648, cases=ls653}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x646), ("labels", PrettyRep.List (List.map (fn x647 => 
                                                                                                  cvtIDENT x647
                                                                                           ) ls648)), 
          ("cases", PrettyRep.List (List.map (fn x652 => cvtCASE x652
                                             ) ls653))]))
     | cvtSTMT (SwitchTypeStmt{cond=x666, ty=x667, cases=ls669}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x666), ("ty", cvtTY x667), 
          ("cases", PrettyRep.List (List.map (fn x668 => cvtCATCH_CLAUSE x668
                                             ) ls669))]))
     | cvtSTMT (DXNStmt{expr=x682}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x682)]))
   and cvtEXPR (TernaryExpr(x688, x689, x690)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x688, cvtEXPR x689, cvtEXPR x690]))
     | cvtEXPR (BinaryExpr(x694, x695, x696)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x694, cvtEXPR x695, cvtEXPR x696]))
     | cvtEXPR (BinaryTypeExpr(x700, x701, x702)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x700, cvtEXPR x701, cvtTY x702]))
     | cvtEXPR (ExpectedTypeExpr(x706, x707)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x706, cvtEXPR x707]))
     | cvtEXPR (UnaryExpr(x711, x712)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x711, 
          cvtEXPR x712]))
     | cvtEXPR (TypeExpr x716) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x716))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt721) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt721 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x720 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x720))
       ))
     | cvtEXPR (SuperExpr opt728) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt728 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x727 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x727))
       ))
     | cvtEXPR (LiteralExpr x734) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x734))
     | cvtEXPR (CallExpr{func=x737, actuals=ls739}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x737), ("actuals", PrettyRep.List (List.map (fn x738 => 
                                                                                                   cvtEXPR x738
                                                                                            ) ls739))]))
     | cvtEXPR (ApplyTypeExpr{expr=x750, actuals=ls752}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x750), ("actuals", PrettyRep.List (List.map (fn x751 => 
                                                                                                   cvtTY x751
                                                                                            ) ls752))]))
     | cvtEXPR (LetExpr{defs=x763, body=x764, head=opt766}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x763), ("body", cvtEXPR x764), 
          ("head", 
       (case opt766 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x765 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x765))
       ))]))
     | cvtEXPR (NewExpr{obj=x779, actuals=ls781}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x779), ("actuals", PrettyRep.List (List.map (fn x780 => 
                                                                                                  cvtEXPR x780
                                                                                           ) ls781))]))
     | cvtEXPR (ObjectRef{base=x792, ident=x793, loc=opt795}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x792), ("ident", cvtIDENT_EXPR x793), 
          ("loc", 
       (case opt795 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x794 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x794))
       ))]))
     | cvtEXPR (LexicalRef{ident=x808, loc=opt810}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x808), ("loc", 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x809))
       ))]))
     | cvtEXPR (SetExpr(x821, x822, x823)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x821, 
          cvtEXPR x822, cvtEXPR x823]))
     | cvtEXPR (ListExpr ls828) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x827 => 
                                                                                                    cvtEXPR x827
                                                                                             ) ls828)))
     | cvtEXPR (InitExpr(x834, x835, x836)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x834, 
          cvtHEAD x835, cvtINITS x836]))
     | cvtEXPR (SliceExpr(x840, x841, x842)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x840, cvtEXPR x841, cvtEXPR x842]))
     | cvtEXPR (GetTemp n846) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n846))
     | cvtEXPR (GetParam n849) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n849))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n855) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n855))
     | cvtFIXTURE_NAME (PropName x858) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x858))
   and cvtIDENT_EXPR (Identifier{ident=x861, openNamespaces=ls867}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x861), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls863 => PrettyRep.List (List.map (fn x862 => 
                                                                                cvtNAMESPACE x862
                                                                         ) ls863)
                                   ) ls867))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x878, expr=x879}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x878), ("expr", cvtEXPR x879)]))
     | cvtIDENT_EXPR (AttributeIdentifier x887) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x887))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x890, openNamespaces=ls896}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x890), ("openNamespaces", PrettyRep.List (List.map (fn ls892 => 
                                                                            PrettyRep.List (List.map (fn x891 => 
                                                                                                            cvtNAMESPACE x891
                                                                                                     ) ls892)
                                                                     ) ls896))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x907, ident=s908}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x907), ("ident", PrettyRep.UniStr s908)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls917, x921)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x916 => cvtIDENT x916
                                                          ) ls917), cvtIDENT_EXPR x921]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r928) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r928))
     | cvtLITERAL (LiteralDecimal d931) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d931))
     | cvtLITERAL (LiteralInt i934) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i934))
     | cvtLITERAL (LiteralUInt u937) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u937))
     | cvtLITERAL (LiteralBoolean b940) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b940))
     | cvtLITERAL (LiteralString s943) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s943))
     | cvtLITERAL (LiteralArray{exprs=ls947, ty=opt952}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x946 => 
                                                                         cvtEXPR x946
                                                                  ) ls947)), 
          ("ty", 
       (case opt952 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x951 => PrettyRep.Ctor ("SOME", SOME (cvtTY x951))
       ))]))
     | cvtLITERAL (LiteralXML ls964) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x963 => 
                                                                                                           cvtEXPR x963
                                                                                                    ) ls964)))
     | cvtLITERAL (LiteralNamespace x970) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x970))
     | cvtLITERAL (LiteralObject{expr=ls974, ty=opt979}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x973 => 
                                                                        cvtFIELD x973
                                                                 ) ls974)), 
          ("ty", 
       (case opt979 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x978 => PrettyRep.Ctor ("SOME", SOME (cvtTY x978))
       ))]))
     | cvtLITERAL (LiteralFunction x990) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x990))
     | cvtLITERAL (LiteralRegExp{str=s993}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s993)]))
   and cvtBLOCK (Block x999) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x999))
   and cvtFIXTURE (NamespaceFixture x1002) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1002))
     | cvtFIXTURE (ClassFixture x1005) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1005))
     | cvtFIXTURE (InterfaceFixture x1008) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x1008))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1012) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1012))
     | cvtFIXTURE (MethodFixture{func=x1015, ty=x1016, readOnly=b1017, override=b1018, 
          final=b1019}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1015), ("ty", cvtTY x1016), ("readOnly", PrettyRep.Bool b1017), 
          ("override", PrettyRep.Bool b1018), ("final", PrettyRep.Bool b1019)]))
     | cvtFIXTURE (ValFixture{ty=x1033, readOnly=b1034}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1033), ("readOnly", PrettyRep.Bool b1034)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1042, getter=opt1044, setter=opt1049}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1042), ("getter", 
       (case opt1044 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1043 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1043))
       )), ("setter", 
       (case opt1049 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1048 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1048))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1062, baseTypeArgs=ls1064}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1062), ("baseTypeArgs", PrettyRep.List (List.map (fn x1063 => 
                                                                           cvtTY x1063
                                                                    ) ls1064))]))
   and cvtHEAD (Head(x1075, x1076)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1075, 
          cvtINITS x1076]))
   and cvtBINDINGS (ls1081, ls1086) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1080 => 
                                                                                       cvtBINDING x1080
                                                                                ) ls1081), 
          PrettyRep.List (List.map (fn x1085 => cvtINIT_STEP x1085
                                   ) ls1086)]
   and cvtRIB ls1094 = PrettyRep.List (List.map (fn (x1091, x1092) => PrettyRep.Tuple [cvtFIXTURE_NAME x1091, 
                                                       cvtFIXTURE x1092]
                                                ) ls1094)
   and cvtRIBS ls1099 = PrettyRep.List (List.map (fn x1098 => cvtRIB x1098
                                                 ) ls1099)
   and cvtINITS ls1106 = PrettyRep.List (List.map (fn (x1103, x1104) => PrettyRep.Tuple [cvtFIXTURE_NAME x1103, 
                                                         cvtEXPR x1104]
                                                  ) ls1106)
   and cvtINSTANCE_TYPE {name=x1110, typeArgs=ls1112, nonnullable=b1116, superTypes=ls1118, 
          ty=x1122, dynamic=b1123} = PrettyRep.Rec [("name", cvtNAME x1110), 
          ("typeArgs", PrettyRep.List (List.map (fn x1111 => cvtTYPE_EXPR x1111
                                                ) ls1112)), ("nonnullable", 
          PrettyRep.Bool b1116), ("superTypes", PrettyRep.List (List.map (fn x1117 => 
                                                                                cvtTYPE_EXPR x1117
                                                                         ) ls1118)), 
          ("ty", cvtTYPE_EXPR x1122), ("dynamic", PrettyRep.Bool b1123)]
   and cvtFIELD {kind=x1137, name=x1138, init=x1139} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1137), ("name", cvtIDENT_EXPR x1138), ("init", cvtEXPR x1139)]
   and cvtFIELD_TYPE {name=x1147, ty=x1148} = PrettyRep.Rec [("name", cvtIDENT x1147), 
          ("ty", cvtTYPE_EXPR x1148)]
   and cvtFUNC_TYPE {params=ls1155, result=x1159, thisType=opt1161, hasRest=b1165, 
          minArgs=n1166} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1154 => 
                                                                                     cvtTYPE_EXPR x1154
                                                                              ) ls1155)), 
          ("result", cvtTYPE_EXPR x1159), ("thisType", 
       (case opt1161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1160 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1160))
       )), ("hasRest", PrettyRep.Bool b1165), ("minArgs", PrettyRep.Int n1166)]
   and cvtFUNC_DEFN {kind=x1178, ns=opt1180, final=b1184, override=b1185, prototype=b1186, 
          static=b1187, func=x1188} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1178), 
          ("ns", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1179))
       )), ("final", PrettyRep.Bool b1184), ("override", PrettyRep.Bool b1185), 
          ("prototype", PrettyRep.Bool b1186), ("static", PrettyRep.Bool b1187), 
          ("func", cvtFUNC x1188)]
   and cvtCTOR_DEFN x1204 = cvtCTOR x1204
   and cvtVAR_DEFN {kind=x1205, ns=opt1207, static=b1211, prototype=b1212, 
          bindings=(ls1214, ls1219)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1205), 
          ("ns", 
       (case opt1207 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1206 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1206))
       )), ("static", PrettyRep.Bool b1211), ("prototype", PrettyRep.Bool b1212), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1213 => 
                                                                        cvtBINDING x1213
                                                                 ) ls1214), 
          PrettyRep.List (List.map (fn x1218 => cvtINIT_STEP x1218
                                   ) ls1219)])]
   and cvtNAMESPACE_DEFN {ident=x1235, ns=opt1237, init=opt1242} = PrettyRep.Rec [("ident", 
          cvtIDENT x1235), ("ns", 
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1236))
       )), ("init", 
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1241))
       ))]
   and cvtCLASS_DEFN {ns=opt1254, ident=x1258, nonnullable=b1259, dynamic=b1260, 
          final=b1261, params=ls1263, extends=opt1268, implements=ls1273, classDefns=ls1278, 
          instanceDefns=ls1283, instanceStmts=ls1288, ctorDefn=opt1293} = PrettyRep.Rec [("ns", 
          
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       )), ("ident", cvtIDENT x1258), ("nonnullable", PrettyRep.Bool b1259), 
          ("dynamic", PrettyRep.Bool b1260), ("final", PrettyRep.Bool b1261), 
          ("params", PrettyRep.List (List.map (fn x1262 => cvtIDENT x1262
                                              ) ls1263)), ("extends", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1267 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1267))
       )), ("implements", PrettyRep.List (List.map (fn x1272 => cvtTYPE_EXPR x1272
                                                   ) ls1273)), ("classDefns", 
          PrettyRep.List (List.map (fn x1277 => cvtDEFN x1277
                                   ) ls1278)), ("instanceDefns", PrettyRep.List (List.map (fn x1282 => 
                                                                                                 cvtDEFN x1282
                                                                                          ) ls1283)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1287 => cvtSTMT x1287
                                                     ) ls1288)), ("ctorDefn", 
          
       (case opt1293 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1292 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1292))
       ))]
   and cvtINTERFACE_DEFN {ident=x1322, ns=opt1324, nonnullable=b1328, params=ls1330, 
          extends=ls1335, instanceDefns=ls1340} = PrettyRep.Rec [("ident", 
          cvtIDENT x1322), ("ns", 
       (case opt1324 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1323 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1323))
       )), ("nonnullable", PrettyRep.Bool b1328), ("params", PrettyRep.List (List.map (fn x1329 => 
                                                                                             cvtIDENT x1329
                                                                                      ) ls1330)), 
          ("extends", PrettyRep.List (List.map (fn x1334 => cvtTYPE_EXPR x1334
                                               ) ls1335)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1339 => cvtDEFN x1339
                                   ) ls1340))]
   and cvtTYPE_DEFN {ident=x1357, ns=opt1359, init=x1363} = PrettyRep.Rec [("ident", 
          cvtIDENT x1357), ("ns", 
       (case opt1359 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1358 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1358))
       )), ("init", cvtTYPE_EXPR x1363)]
   and cvtFOR_ENUM_STMT {isEach=b1371, defn=opt1402, obj=x1406, rib=opt1414, 
          next=x1418, labels=ls1420, body=x1424} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1371), ("defn", 
       (case opt1402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1372, ns=opt1374, static=b1378, prototype=b1379, bindings=(ls1381, 
            ls1386)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1372), ("ns", 
         (case opt1374 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1373 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1373))
         )), ("static", PrettyRep.Bool b1378), ("prototype", PrettyRep.Bool b1379), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1380 => 
                                                                          cvtBINDING x1380
                                                                   ) ls1381), 
            PrettyRep.List (List.map (fn x1385 => cvtINIT_STEP x1385
                                     ) ls1386)])]))
       )), ("obj", cvtEXPR x1406), ("rib", 
       (case opt1414 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1410 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1407, 
                                                                                      x1408) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1407, 
                                                                                      cvtFIXTURE x1408]
                                                                               ) ls1410)))
       )), ("next", cvtSTMT x1418), ("labels", PrettyRep.List (List.map (fn x1419 => 
                                                                               cvtIDENT x1419
                                                                        ) ls1420)), 
          ("body", cvtSTMT x1424)]
   and cvtFOR_STMT {rib=opt1447, defn=opt1481, init=ls1486, cond=x1490, update=x1491, 
          labels=ls1493, body=x1497} = PrettyRep.Rec [("rib", 
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1443 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1440, 
                                                                                      x1441) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1440, 
                                                                                      cvtFIXTURE x1441]
                                                                               ) ls1443)))
       )), ("defn", 
       (case opt1481 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1451, ns=opt1453, static=b1457, prototype=b1458, bindings=(ls1460, 
            ls1465)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1451), ("ns", 
         (case opt1453 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1452 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1452))
         )), ("static", PrettyRep.Bool b1457), ("prototype", PrettyRep.Bool b1458), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1459 => 
                                                                          cvtBINDING x1459
                                                                   ) ls1460), 
            PrettyRep.List (List.map (fn x1464 => cvtINIT_STEP x1464
                                     ) ls1465)])]))
       )), ("init", PrettyRep.List (List.map (fn x1485 => cvtSTMT x1485
                                             ) ls1486)), ("cond", cvtEXPR x1490), 
          ("update", cvtEXPR x1491), ("labels", PrettyRep.List (List.map (fn x1492 => 
                                                                                cvtIDENT x1492
                                                                         ) ls1493)), 
          ("body", cvtSTMT x1497)]
   and cvtWHILE_STMT {cond=x1513, rib=opt1521, body=x1525, labels=ls1527} = 
          PrettyRep.Rec [("cond", cvtEXPR x1513), ("rib", 
       (case opt1521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1517 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1514, 
                                                                                      x1515) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1514, 
                                                                                      cvtFIXTURE x1515]
                                                                               ) ls1517)))
       )), ("body", cvtSTMT x1525), ("labels", PrettyRep.List (List.map (fn x1526 => 
                                                                               cvtIDENT x1526
                                                                        ) ls1527))]
   and cvtDIRECTIVES {pragmas=ls1541, defns=ls1546, head=opt1551, body=ls1556, 
          loc=opt1561} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1540 => 
                                                                                    cvtPRAGMA x1540
                                                                             ) ls1541)), 
          ("defns", PrettyRep.List (List.map (fn x1545 => cvtDEFN x1545
                                             ) ls1546)), ("head", 
       (case opt1551 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1550 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1550))
       )), ("body", PrettyRep.List (List.map (fn x1555 => cvtSTMT x1555
                                             ) ls1556)), ("loc", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1560))
       ))]
   and cvtCASE {label=opt1577, inits=opt1588, body=x1592} = PrettyRep.Rec [("label", 
          
       (case opt1577 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1576 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1576))
       )), ("inits", 
       (case opt1588 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1584 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1581, 
                                                                                      x1582) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1581, 
                                                                                      cvtEXPR x1582]
                                                                               ) ls1584)))
       )), ("body", cvtBLOCK x1592)]
   and cvtCATCH_CLAUSE {bindings=(ls1601, ls1606), ty=x1611, rib=opt1619, inits=opt1630, 
          block=x1634} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1600 => 
                                                                                                      cvtBINDING x1600
                                                                                               ) ls1601), 
          PrettyRep.List (List.map (fn x1605 => cvtINIT_STEP x1605
                                   ) ls1606)]), ("ty", cvtTY x1611), ("rib", 
          
       (case opt1619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1615 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1612, 
                                                                                      x1613) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1612, 
                                                                                      cvtFIXTURE x1613]
                                                                               ) ls1615)))
       )), ("inits", 
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1626 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1623, 
                                                                                      x1624) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1623, 
                                                                                      cvtEXPR x1624]
                                                                               ) ls1626)))
       )), ("block", cvtBLOCK x1634)]
   and cvtFUNC_NAME {kind=x1646, ident=x1647} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1646), 
          ("ident", cvtIDENT x1647)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1653, getter=opt1655, setter=opt1660} = 
          PrettyRep.Rec [("ty", cvtTY x1653), ("getter", 
       (case opt1655 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1654 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1654))
       )), ("setter", 
       (case opt1660 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1659 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1659))
       ))]
   and cvtFRAGMENT (Unit{name=opt1672, fragments=ls1677}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1671 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1671))
       )), ("fragments", PrettyRep.List (List.map (fn x1676 => cvtFRAGMENT x1676
                                                  ) ls1677))]))
     | cvtFRAGMENT (Package{name=ls1689, fragments=ls1694}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1688 => 
                                                                        cvtIDENT x1688
                                                                 ) ls1689)), 
          ("fragments", PrettyRep.List (List.map (fn x1693 => cvtFRAGMENT x1693
                                                 ) ls1694))]))
     | cvtFRAGMENT (Anon x1705) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1705))
end

