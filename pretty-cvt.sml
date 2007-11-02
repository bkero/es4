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
   and cvtRIBS ls1105 = PrettyRep.List (List.map (fn ls1101 => PrettyRep.List (List.map (fn (x1098, 
                                                                                               x1099) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1098, 
                                                                                               cvtFIXTURE x1099]
                                                                                        ) ls1101)
                                                 ) ls1105)
   and cvtINITS ls1112 = PrettyRep.List (List.map (fn (x1109, x1110) => PrettyRep.Tuple [cvtFIXTURE_NAME x1109, 
                                                         cvtEXPR x1110]
                                                  ) ls1112)
   and cvtINSTANCE_TYPE {name=x1116, typeArgs=ls1118, nonnullable=b1122, superTypes=ls1124, 
          ty=x1128, dynamic=b1129} = PrettyRep.Rec [("name", cvtNAME x1116), 
          ("typeArgs", PrettyRep.List (List.map (fn x1117 => cvtTYPE_EXPR x1117
                                                ) ls1118)), ("nonnullable", 
          PrettyRep.Bool b1122), ("superTypes", PrettyRep.List (List.map (fn x1123 => 
                                                                                cvtTYPE_EXPR x1123
                                                                         ) ls1124)), 
          ("ty", cvtTYPE_EXPR x1128), ("dynamic", PrettyRep.Bool b1129)]
   and cvtFIELD {kind=x1143, name=x1144, init=x1145} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1143), ("name", cvtIDENT_EXPR x1144), ("init", cvtEXPR x1145)]
   and cvtFIELD_TYPE {name=x1153, ty=x1154} = PrettyRep.Rec [("name", cvtIDENT x1153), 
          ("ty", cvtTYPE_EXPR x1154)]
   and cvtFUNC_TYPE {params=ls1161, result=x1165, thisType=opt1167, hasRest=b1171, 
          minArgs=n1172} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1160 => 
                                                                                     cvtTYPE_EXPR x1160
                                                                              ) ls1161)), 
          ("result", cvtTYPE_EXPR x1165), ("thisType", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1166))
       )), ("hasRest", PrettyRep.Bool b1171), ("minArgs", PrettyRep.Int n1172)]
   and cvtFUNC_DEFN {kind=x1184, ns=opt1186, final=b1190, override=b1191, prototype=b1192, 
          static=b1193, func=x1194} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1184), 
          ("ns", 
       (case opt1186 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1185 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1185))
       )), ("final", PrettyRep.Bool b1190), ("override", PrettyRep.Bool b1191), 
          ("prototype", PrettyRep.Bool b1192), ("static", PrettyRep.Bool b1193), 
          ("func", cvtFUNC x1194)]
   and cvtCTOR_DEFN x1210 = cvtCTOR x1210
   and cvtVAR_DEFN {kind=x1211, ns=opt1213, static=b1217, prototype=b1218, 
          bindings=(ls1220, ls1225)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1211), 
          ("ns", 
       (case opt1213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1212 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1212))
       )), ("static", PrettyRep.Bool b1217), ("prototype", PrettyRep.Bool b1218), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1219 => 
                                                                        cvtBINDING x1219
                                                                 ) ls1220), 
          PrettyRep.List (List.map (fn x1224 => cvtINIT_STEP x1224
                                   ) ls1225)])]
   and cvtNAMESPACE_DEFN {ident=x1241, ns=opt1243, init=opt1248} = PrettyRep.Rec [("ident", 
          cvtIDENT x1241), ("ns", 
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       )), ("init", 
       (case opt1248 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1247 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1247))
       ))]
   and cvtCLASS_DEFN {ns=opt1260, ident=x1264, nonnullable=b1265, dynamic=b1266, 
          final=b1267, params=ls1269, extends=opt1274, implements=ls1279, classDefns=ls1284, 
          instanceDefns=ls1289, instanceStmts=ls1294, ctorDefn=opt1299} = PrettyRep.Rec [("ns", 
          
       (case opt1260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1259 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1259))
       )), ("ident", cvtIDENT x1264), ("nonnullable", PrettyRep.Bool b1265), 
          ("dynamic", PrettyRep.Bool b1266), ("final", PrettyRep.Bool b1267), 
          ("params", PrettyRep.List (List.map (fn x1268 => cvtIDENT x1268
                                              ) ls1269)), ("extends", 
       (case opt1274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1273 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1273))
       )), ("implements", PrettyRep.List (List.map (fn x1278 => cvtTYPE_EXPR x1278
                                                   ) ls1279)), ("classDefns", 
          PrettyRep.List (List.map (fn x1283 => cvtDEFN x1283
                                   ) ls1284)), ("instanceDefns", PrettyRep.List (List.map (fn x1288 => 
                                                                                                 cvtDEFN x1288
                                                                                          ) ls1289)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1293 => cvtSTMT x1293
                                                     ) ls1294)), ("ctorDefn", 
          
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1298))
       ))]
   and cvtINTERFACE_DEFN {ident=x1328, ns=opt1330, nonnullable=b1334, params=ls1336, 
          extends=ls1341, instanceDefns=ls1346} = PrettyRep.Rec [("ident", 
          cvtIDENT x1328), ("ns", 
       (case opt1330 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1329 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1329))
       )), ("nonnullable", PrettyRep.Bool b1334), ("params", PrettyRep.List (List.map (fn x1335 => 
                                                                                             cvtIDENT x1335
                                                                                      ) ls1336)), 
          ("extends", PrettyRep.List (List.map (fn x1340 => cvtTYPE_EXPR x1340
                                               ) ls1341)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1345 => cvtDEFN x1345
                                   ) ls1346))]
   and cvtTYPE_DEFN {ident=x1363, ns=opt1365, init=x1369} = PrettyRep.Rec [("ident", 
          cvtIDENT x1363), ("ns", 
       (case opt1365 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1364))
       )), ("init", cvtTYPE_EXPR x1369)]
   and cvtFOR_ENUM_STMT {isEach=b1377, defn=opt1408, obj=x1412, rib=opt1420, 
          next=x1424, labels=ls1426, body=x1430} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1377), ("defn", 
       (case opt1408 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1378, ns=opt1380, static=b1384, prototype=b1385, bindings=(ls1387, 
            ls1392)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1378), ("ns", 
         (case opt1380 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1379 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1379))
         )), ("static", PrettyRep.Bool b1384), ("prototype", PrettyRep.Bool b1385), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1386 => 
                                                                          cvtBINDING x1386
                                                                   ) ls1387), 
            PrettyRep.List (List.map (fn x1391 => cvtINIT_STEP x1391
                                     ) ls1392)])]))
       )), ("obj", cvtEXPR x1412), ("rib", 
       (case opt1420 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1416 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1413, 
                                                                                      x1414) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1413, 
                                                                                      cvtFIXTURE x1414]
                                                                               ) ls1416)))
       )), ("next", cvtSTMT x1424), ("labels", PrettyRep.List (List.map (fn x1425 => 
                                                                               cvtIDENT x1425
                                                                        ) ls1426)), 
          ("body", cvtSTMT x1430)]
   and cvtFOR_STMT {rib=opt1453, defn=opt1487, init=ls1492, cond=x1496, update=x1497, 
          labels=ls1499, body=x1503} = PrettyRep.Rec [("rib", 
       (case opt1453 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1449 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1446, 
                                                                                      x1447) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1446, 
                                                                                      cvtFIXTURE x1447]
                                                                               ) ls1449)))
       )), ("defn", 
       (case opt1487 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1457, ns=opt1459, static=b1463, prototype=b1464, bindings=(ls1466, 
            ls1471)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1457), ("ns", 
         (case opt1459 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1458 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1458))
         )), ("static", PrettyRep.Bool b1463), ("prototype", PrettyRep.Bool b1464), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1465 => 
                                                                          cvtBINDING x1465
                                                                   ) ls1466), 
            PrettyRep.List (List.map (fn x1470 => cvtINIT_STEP x1470
                                     ) ls1471)])]))
       )), ("init", PrettyRep.List (List.map (fn x1491 => cvtSTMT x1491
                                             ) ls1492)), ("cond", cvtEXPR x1496), 
          ("update", cvtEXPR x1497), ("labels", PrettyRep.List (List.map (fn x1498 => 
                                                                                cvtIDENT x1498
                                                                         ) ls1499)), 
          ("body", cvtSTMT x1503)]
   and cvtWHILE_STMT {cond=x1519, rib=opt1527, body=x1531, labels=ls1533} = 
          PrettyRep.Rec [("cond", cvtEXPR x1519), ("rib", 
       (case opt1527 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1523 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1520, 
                                                                                      x1521) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1520, 
                                                                                      cvtFIXTURE x1521]
                                                                               ) ls1523)))
       )), ("body", cvtSTMT x1531), ("labels", PrettyRep.List (List.map (fn x1532 => 
                                                                               cvtIDENT x1532
                                                                        ) ls1533))]
   and cvtDIRECTIVES {pragmas=ls1547, defns=ls1552, head=opt1557, body=ls1562, 
          loc=opt1567} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1546 => 
                                                                                    cvtPRAGMA x1546
                                                                             ) ls1547)), 
          ("defns", PrettyRep.List (List.map (fn x1551 => cvtDEFN x1551
                                             ) ls1552)), ("head", 
       (case opt1557 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1556 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1556))
       )), ("body", PrettyRep.List (List.map (fn x1561 => cvtSTMT x1561
                                             ) ls1562)), ("loc", 
       (case opt1567 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1566 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1566))
       ))]
   and cvtCASE {label=opt1583, inits=opt1594, body=x1598} = PrettyRep.Rec [("label", 
          
       (case opt1583 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1582 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1582))
       )), ("inits", 
       (case opt1594 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1590 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1587, 
                                                                                      x1588) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1587, 
                                                                                      cvtEXPR x1588]
                                                                               ) ls1590)))
       )), ("body", cvtBLOCK x1598)]
   and cvtCATCH_CLAUSE {bindings=(ls1607, ls1612), ty=x1617, rib=opt1625, inits=opt1636, 
          block=x1640} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1606 => 
                                                                                                      cvtBINDING x1606
                                                                                               ) ls1607), 
          PrettyRep.List (List.map (fn x1611 => cvtINIT_STEP x1611
                                   ) ls1612)]), ("ty", cvtTY x1617), ("rib", 
          
       (case opt1625 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1621 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1618, 
                                                                                      x1619) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1618, 
                                                                                      cvtFIXTURE x1619]
                                                                               ) ls1621)))
       )), ("inits", 
       (case opt1636 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1632 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1629, 
                                                                                      x1630) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1629, 
                                                                                      cvtEXPR x1630]
                                                                               ) ls1632)))
       )), ("block", cvtBLOCK x1640)]
   and cvtFUNC_NAME {kind=x1652, ident=x1653} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1652), 
          ("ident", cvtIDENT x1653)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1659, getter=opt1661, setter=opt1666} = 
          PrettyRep.Rec [("ty", cvtTY x1659), ("getter", 
       (case opt1661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1660 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1660))
       )), ("setter", 
       (case opt1666 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1665 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1665))
       ))]
   and cvtFRAGMENT (Unit{name=opt1678, fragments=ls1683}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1678 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1677 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1677))
       )), ("fragments", PrettyRep.List (List.map (fn x1682 => cvtFRAGMENT x1682
                                                  ) ls1683))]))
     | cvtFRAGMENT (Package{name=ls1695, fragments=ls1700}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1694 => 
                                                                        cvtIDENT x1694
                                                                 ) ls1695)), 
          ("fragments", PrettyRep.List (List.map (fn x1699 => cvtFRAGMENT x1699
                                                 ) ls1700))]))
     | cvtFRAGMENT (Anon x1711) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1711))
end

