structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtAST_NONCE n20 = PrettyRep.Int n20
   and cvtTYPEVAR_NONCE n21 = PrettyRep.Int n21
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Private x23) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x23))
     | cvtNAMESPACE (Protected x26) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x26))
     | cvtNAMESPACE (Public x29) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x29))
     | cvtNAMESPACE (Internal x32) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x32))
     | cvtNAMESPACE (UserNamespace s35) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s35))
     | cvtNAMESPACE (AnonUserNamespace n38) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n38))
     | cvtNAMESPACE (LimitedNamespace(x41, x42)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x41, cvtNAMESPACE x42]))
   and cvtNAME {ns=x46, id=x47} = PrettyRep.Rec [("ns", cvtNAMESPACE x46), 
          ("id", cvtIDENT x47)]
   and cvtMULTINAME {nss=ls58, id=x62} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls54 => 
                                                                                                PrettyRep.List (List.map (fn x53 => 
                                                                                                                                cvtNAMESPACE x53
                                                                                                                         ) ls54)
                                                                                         ) ls58)), 
          ("id", cvtIDENT x62)]
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
   and cvtPRAGMA (UseNamespace x129) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x129))
     | cvtPRAGMA (UseDefaultNamespace x132) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x132))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls138, name=x142}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x137 => 
                                                                           cvtIDENT x137
                                                                    ) ls138)), 
          ("name", cvtIDENT x142)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x156, typeParams=ls158, nonnullable=b162, dynamic=b163, 
          extends=opt165, implements=ls170, classRib=x174, instanceRib=x175, 
          instanceInits=x176, constructor=opt178, classType=x182, instanceType=x183}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x156), 
          ("typeParams", PrettyRep.List (List.map (fn x157 => cvtIDENT x157
                                                  ) ls158)), ("nonnullable", 
          PrettyRep.Bool b162), ("dynamic", PrettyRep.Bool b163), ("extends", 
          
       (case opt165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x164 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x164))
       )), ("implements", PrettyRep.List (List.map (fn x169 => cvtTYPE_EXPR x169
                                                   ) ls170)), ("classRib", 
          cvtRIB x174), ("instanceRib", cvtRIB x175), ("instanceInits", cvtHEAD x176), 
          ("constructor", 
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x177))
       )), ("classType", cvtTYPE_EXPR x182), ("instanceType", cvtTYPE_EXPR x183)]))
   and cvtIFACE (Iface{name=x211, typeParams=ls213, nonnullable=b217, extends=ls219, 
          instanceRib=x223, instanceType=x224}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x211), ("typeParams", PrettyRep.List (List.map (fn x212 => 
                                                                                                      cvtIDENT x212
                                                                                               ) ls213)), 
          ("nonnullable", PrettyRep.Bool b217), ("extends", PrettyRep.List (List.map (fn x218 => 
                                                                                            cvtTYPE_EXPR x218
                                                                                     ) ls219)), 
          ("instanceRib", cvtRIB x223), ("instanceType", cvtTYPE_EXPR x224)]))
   and cvtCTOR (Ctor{settings=x240, superArgs=ls242, func=x246}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x240), ("superArgs", PrettyRep.List (List.map (fn x241 => 
                                                                                                         cvtEXPR x241
                                                                                                  ) ls242)), 
          ("func", cvtFUNC x246)]))
   and cvtFUNC (Func{name=x256, fsig=x257, native=b258, block=opt260, param=x264, 
          defaults=ls266, ty=x270, loc=opt272}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x256), ("fsig", cvtFUNC_SIG x257), ("native", PrettyRep.Bool b258), 
          ("block", 
       (case opt260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x259 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x259))
       )), ("param", cvtHEAD x264), ("defaults", PrettyRep.List (List.map (fn x265 => 
                                                                                 cvtEXPR x265
                                                                          ) ls266)), 
          ("ty", cvtTYPE_EXPR x270), ("loc", 
       (case opt272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x271 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x271))
       ))]))
   and cvtDEFN (ClassDefn x295) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x295))
     | cvtDEFN (VariableDefn x298) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x298))
     | cvtDEFN (FunctionDefn x301) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x301))
     | cvtDEFN (ConstructorDefn x304) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x304))
     | cvtDEFN (InterfaceDefn x307) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x307))
     | cvtDEFN (NamespaceDefn x310) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x310))
     | cvtDEFN (TypeDefn x313) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x313))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls317, params=x321, paramTypes=ls323, 
          defaults=ls328, ctorInits=opt339, returnType=x343, thisType=opt345, 
          hasRest=b349}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x316 => cvtIDENT x316
                                   ) ls317)), ("params", cvtBINDINGS x321), 
          ("paramTypes", PrettyRep.List (List.map (fn x322 => cvtTYPE_EXPR x322
                                                  ) ls323)), ("defaults", PrettyRep.List (List.map (fn x327 => 
                                                                                                          cvtEXPR x327
                                                                                                   ) ls328)), 
          ("ctorInits", 
       (case opt339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x332, ls334) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x332, 
            PrettyRep.List (List.map (fn x333 => cvtEXPR x333
                                     ) ls334)]))
       )), ("returnType", cvtTYPE_EXPR x343), ("thisType", 
       (case opt345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x344 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x344))
       )), ("hasRest", PrettyRep.Bool b349)]))
   and cvtBINDING (Binding{ident=x369, ty=x370}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x369), ("ty", cvtTYPE_EXPR x370)]))
   and cvtBINDING_IDENT (TempIdent n378) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n378))
     | cvtBINDING_IDENT (ParamIdent n381) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n381))
     | cvtBINDING_IDENT (PropIdent x384) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x384))
   and cvtINIT_STEP (InitStep(x387, x388)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x387, 
          cvtEXPR x388]))
     | cvtINIT_STEP (AssignStep(x392, x393)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x392, cvtEXPR x393]))
   and cvtTYPE_EXPR (SpecialType x397) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x397))
     | cvtTYPE_EXPR (UnionType ls401) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x400 => 
                                                                                                           cvtTYPE_EXPR x400
                                                                                                    ) ls401)))
     | cvtTYPE_EXPR (ArrayType ls408) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x407 => 
                                                                                                           cvtTYPE_EXPR x407
                                                                                                    ) ls408)))
     | cvtTYPE_EXPR (TypeName(x414, opt416)) = PrettyRep.Ctor ("TypeName", 
          SOME (PrettyRep.Tuple [cvtIDENT_EXPR x414, 
       (case opt416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x415 => PrettyRep.Ctor ("SOME", SOME (cvtAST_NONCE x415))
       )]))
     | cvtTYPE_EXPR (ElementTypeRef(x423, n424)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x423, PrettyRep.Int n424]))
     | cvtTYPE_EXPR (FieldTypeRef(x428, x429)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x428, cvtIDENT x429]))
     | cvtTYPE_EXPR (FunctionType x433) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x433))
     | cvtTYPE_EXPR (ObjectType ls437) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x436 => 
                                                                                                             cvtFIELD_TYPE x436
                                                                                                      ) ls437)))
     | cvtTYPE_EXPR (LikeType x443) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x443))
     | cvtTYPE_EXPR (AppType{base=x446, args=ls448}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x446), ("args", PrettyRep.List (List.map (fn x447 => 
                                                                                                     cvtTYPE_EXPR x447
                                                                                              ) ls448))]))
     | cvtTYPE_EXPR (LamType{params=ls460, body=x464}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x459 => 
                                                                          cvtIDENT x459
                                                                   ) ls460)), 
          ("body", cvtTYPE_EXPR x464)]))
     | cvtTYPE_EXPR (NullableType{expr=x472, nullable=b473}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x472), ("nullable", PrettyRep.Bool b473)]))
     | cvtTYPE_EXPR (InstanceType x481) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x481))
     | cvtTYPE_EXPR (TypeVarFixtureRef x484) = PrettyRep.Ctor ("TypeVarFixtureRef", 
          SOME (cvtTYPEVAR_NONCE x484))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x488) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x488))
     | cvtSTMT (InitStmt{kind=x491, ns=opt493, prototype=b497, static=b498, 
          temps=x499, inits=ls501}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x491), ("ns", 
       (case opt493 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x492 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x492))
       )), ("prototype", PrettyRep.Bool b497), ("static", PrettyRep.Bool b498), 
          ("temps", cvtBINDINGS x499), ("inits", PrettyRep.List (List.map (fn x500 => 
                                                                                 cvtINIT_STEP x500
                                                                          ) ls501))]))
     | cvtSTMT (ClassBlock x520) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x520))
     | cvtSTMT (ForInStmt x523) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x523))
     | cvtSTMT (ThrowStmt x526) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x526))
     | cvtSTMT (ReturnStmt x529) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x529))
     | cvtSTMT (BreakStmt opt533) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt533 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x532 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x532))
       ))
     | cvtSTMT (ContinueStmt opt540) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt540 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x539 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x539))
       ))
     | cvtSTMT (BlockStmt x546) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x546))
     | cvtSTMT (LabeledStmt(x549, x550)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x549, 
          cvtSTMT x550]))
     | cvtSTMT (LetStmt x554) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x554))
     | cvtSTMT (WhileStmt x557) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x557))
     | cvtSTMT (DoWhileStmt x560) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x560))
     | cvtSTMT (ForStmt x563) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x563))
     | cvtSTMT (IfStmt{cnd=x566, thn=x567, els=x568}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x566), ("thn", cvtSTMT x567), 
          ("els", cvtSTMT x568)]))
     | cvtSTMT (WithStmt{obj=x578, ty=x579, body=x580}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x578), ("ty", cvtTYPE_EXPR x579), 
          ("body", cvtSTMT x580)]))
     | cvtSTMT (TryStmt{block=x590, catches=ls592, finally=opt597}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x590), ("catches", PrettyRep.List (List.map (fn x591 => 
                                                                                                     cvtCATCH_CLAUSE x591
                                                                                              ) ls592)), 
          ("finally", 
       (case opt597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x596 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x596))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x610, labels=ls612, cases=ls617}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x610), ("labels", PrettyRep.List (List.map (fn x611 => 
                                                                                                  cvtIDENT x611
                                                                                           ) ls612)), 
          ("cases", PrettyRep.List (List.map (fn x616 => cvtCASE x616
                                             ) ls617))]))
     | cvtSTMT (SwitchTypeStmt{cond=x630, ty=x631, cases=ls633}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x630), ("ty", cvtTYPE_EXPR x631), 
          ("cases", PrettyRep.List (List.map (fn x632 => cvtCATCH_CLAUSE x632
                                             ) ls633))]))
     | cvtSTMT (DXNStmt{expr=x646}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x646)]))
   and cvtEXPR (TernaryExpr(x652, x653, x654)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x652, cvtEXPR x653, cvtEXPR x654]))
     | cvtEXPR (BinaryExpr(x658, x659, x660)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x658, cvtEXPR x659, cvtEXPR x660]))
     | cvtEXPR (BinaryTypeExpr(x664, x665, x666)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x664, cvtEXPR x665, cvtTYPE_EXPR x666]))
     | cvtEXPR (UnaryExpr(x670, x671)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x670, 
          cvtEXPR x671]))
     | cvtEXPR (TypeExpr x675) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x675))
     | cvtEXPR (ThisExpr opt679) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x678 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x678))
       ))
     | cvtEXPR (YieldExpr opt686) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt686 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x685 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x685))
       ))
     | cvtEXPR (SuperExpr opt693) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt693 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x692 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x692))
       ))
     | cvtEXPR (CallExpr{func=x699, actuals=ls701}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x699), ("actuals", PrettyRep.List (List.map (fn x700 => 
                                                                                                   cvtEXPR x700
                                                                                            ) ls701))]))
     | cvtEXPR (ApplyTypeExpr{expr=x712, actuals=ls714}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x712), ("actuals", PrettyRep.List (List.map (fn x713 => 
                                                                                                   cvtTYPE_EXPR x713
                                                                                            ) ls714))]))
     | cvtEXPR (LetExpr{defs=x725, body=x726, head=opt728}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x725), ("body", cvtEXPR x726), 
          ("head", 
       (case opt728 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x727 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x727))
       ))]))
     | cvtEXPR (NewExpr{obj=x741, actuals=ls743}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x741), ("actuals", PrettyRep.List (List.map (fn x742 => 
                                                                                                  cvtEXPR x742
                                                                                           ) ls743))]))
     | cvtEXPR (ObjectRef{base=x754, ident=x755, loc=opt757}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x754), ("ident", cvtIDENT_EXPR x755), 
          ("loc", 
       (case opt757 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x756 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x756))
       ))]))
     | cvtEXPR (LexicalRef{ident=x770, loc=opt772}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x770), ("loc", 
       (case opt772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x771 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x771))
       ))]))
     | cvtEXPR (SetExpr(x783, x784, x785)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x783, 
          cvtEXPR x784, cvtEXPR x785]))
     | cvtEXPR (ListExpr ls790) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x789 => 
                                                                                                    cvtEXPR x789
                                                                                             ) ls790)))
     | cvtEXPR (InitExpr(x796, x797, x798)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x796, 
          cvtHEAD x797, cvtINITS x798]))
     | cvtEXPR (GetTemp n802) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n802))
     | cvtEXPR (GetParam n805) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n805))
     | cvtEXPR (Comprehension(x808, ls810, opt815)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x808, PrettyRep.List (List.map (fn x809 => 
                                                                               cvtFOR_ENUM_HEAD x809
                                                                        ) ls810), 
          
       (case opt815 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x814 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x814))
       )]))
     | cvtEXPR (LiteralExpr x822) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x822))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n830) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n830))
     | cvtFIXTURE_NAME (PropName x833) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x833))
   and cvtIDENT_EXPR (Identifier{ident=x836, openNamespaces=ls842}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x836), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls838 => PrettyRep.List (List.map (fn x837 => 
                                                                                cvtNAMESPACE x837
                                                                         ) ls838)
                                   ) ls842))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x853, expr=x854}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x853), ("expr", cvtEXPR x854)]))
     | cvtIDENT_EXPR (AttributeIdentifier x862) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x862))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x865, openNamespaces=ls871}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x865), ("openNamespaces", PrettyRep.List (List.map (fn ls867 => 
                                                                            PrettyRep.List (List.map (fn x866 => 
                                                                                                            cvtNAMESPACE x866
                                                                                                     ) ls867)
                                                                     ) ls871))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x882, ident=s883}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x882), ("ident", PrettyRep.UniStr s883)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls892, x896)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x891 => cvtIDENT x891
                                                          ) ls892), cvtIDENT_EXPR x896]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r903) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r903))
     | cvtLITERAL (LiteralDecimal d906) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d906))
     | cvtLITERAL (LiteralBoolean b909) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b909))
     | cvtLITERAL (LiteralString s912) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s912))
     | cvtLITERAL (LiteralArray{exprs=x915, ty=opt917}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x915), ("ty", 
       (case opt917 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x916 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x916))
       ))]))
     | cvtLITERAL (LiteralXML ls929) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x928 => 
                                                                                                           cvtEXPR x928
                                                                                                    ) ls929)))
     | cvtLITERAL (LiteralNamespace x935) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x935))
     | cvtLITERAL (LiteralObject{expr=ls939, ty=opt944}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x938 => 
                                                                        cvtFIELD x938
                                                                 ) ls939)), 
          ("ty", 
       (case opt944 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x943 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x943))
       ))]))
     | cvtLITERAL (LiteralFunction x955) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x955))
     | cvtLITERAL (LiteralRegExp{str=s958}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s958)]))
   and cvtBLOCK (Block x964) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x964))
   and cvtFIXTURE (NamespaceFixture x967) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x967))
     | cvtFIXTURE (ClassFixture x970) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x970))
     | cvtFIXTURE (InterfaceFixture x973) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x973))
     | cvtFIXTURE (TypeVarFixture x976) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x976))
     | cvtFIXTURE (TypeFixture x979) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x979))
     | cvtFIXTURE (MethodFixture{func=x982, ty=x983, readOnly=b984, override=b985, 
          final=b986}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x982), ("ty", cvtTYPE_EXPR x983), ("readOnly", PrettyRep.Bool b984), 
          ("override", PrettyRep.Bool b985), ("final", PrettyRep.Bool b986)]))
     | cvtFIXTURE (ValFixture{ty=x1000, readOnly=b1001}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1000), ("readOnly", PrettyRep.Bool b1001)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1009, getter=opt1011, setter=opt1016}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1009), ("getter", 
       (case opt1011 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1010 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1010))
       )), ("setter", 
       (case opt1016 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1015 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1015))
       ))]))
   and cvtHEAD (Head(x1029, x1030)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1029, 
          cvtINITS x1030]))
   and cvtBINDINGS (ls1035, ls1040) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1034 => 
                                                                                       cvtBINDING x1034
                                                                                ) ls1035), 
          PrettyRep.List (List.map (fn x1039 => cvtINIT_STEP x1039
                                   ) ls1040)]
   and cvtRIB ls1048 = PrettyRep.List (List.map (fn (x1045, x1046) => PrettyRep.Tuple [cvtFIXTURE_NAME x1045, 
                                                       cvtFIXTURE x1046]
                                                ) ls1048)
   and cvtRIBS ls1053 = PrettyRep.List (List.map (fn x1052 => cvtRIB x1052
                                                 ) ls1053)
   and cvtINITS ls1060 = PrettyRep.List (List.map (fn (x1057, x1058) => PrettyRep.Tuple [cvtFIXTURE_NAME x1057, 
                                                         cvtEXPR x1058]
                                                  ) ls1060)
   and cvtINSTANCE_TYPE {name=x1064, typeParams=ls1066, typeArgs=ls1071, nonnullable=b1075, 
          superTypes=ls1077, ty=x1081, dynamic=b1082} = PrettyRep.Rec [("name", 
          cvtNAME x1064), ("typeParams", PrettyRep.List (List.map (fn x1065 => 
                                                                         cvtIDENT x1065
                                                                  ) ls1066)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1070 => cvtTYPE_EXPR x1070
                                                ) ls1071)), ("nonnullable", 
          PrettyRep.Bool b1075), ("superTypes", PrettyRep.List (List.map (fn x1076 => 
                                                                                cvtTYPE_EXPR x1076
                                                                         ) ls1077)), 
          ("ty", cvtTYPE_EXPR x1081), ("dynamic", PrettyRep.Bool b1082)]
   and cvtFIELD {kind=x1098, name=x1099, init=x1100} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1098), ("name", cvtIDENT_EXPR x1099), ("init", cvtEXPR x1100)]
   and cvtFIELD_TYPE {name=x1108, ty=x1109} = PrettyRep.Rec [("name", cvtIDENT x1108), 
          ("ty", cvtTYPE_EXPR x1109)]
   and cvtFUNC_TYPE {params=ls1116, result=x1120, thisType=opt1122, hasRest=b1126, 
          minArgs=n1127} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1115 => 
                                                                                     cvtTYPE_EXPR x1115
                                                                              ) ls1116)), 
          ("result", cvtTYPE_EXPR x1120), ("thisType", 
       (case opt1122 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1121 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1121))
       )), ("hasRest", PrettyRep.Bool b1126), ("minArgs", PrettyRep.Int n1127)]
   and cvtFUNC_DEFN {kind=x1139, ns=opt1141, final=b1145, override=b1146, prototype=b1147, 
          static=b1148, func=x1149} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1139), 
          ("ns", 
       (case opt1141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1140 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1140))
       )), ("final", PrettyRep.Bool b1145), ("override", PrettyRep.Bool b1146), 
          ("prototype", PrettyRep.Bool b1147), ("static", PrettyRep.Bool b1148), 
          ("func", cvtFUNC x1149)]
   and cvtCTOR_DEFN x1165 = cvtCTOR x1165
   and cvtVAR_DEFN {kind=x1166, ns=opt1168, static=b1172, prototype=b1173, 
          bindings=(ls1175, ls1180)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1166), 
          ("ns", 
       (case opt1168 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1167 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1167))
       )), ("static", PrettyRep.Bool b1172), ("prototype", PrettyRep.Bool b1173), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1174 => 
                                                                        cvtBINDING x1174
                                                                 ) ls1175), 
          PrettyRep.List (List.map (fn x1179 => cvtINIT_STEP x1179
                                   ) ls1180)])]
   and cvtNAMESPACE_DEFN {ident=x1196, ns=opt1198, init=opt1203} = PrettyRep.Rec [("ident", 
          cvtIDENT x1196), ("ns", 
       (case opt1198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1197 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1197))
       )), ("init", 
       (case opt1203 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1202 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1202))
       ))]
   and cvtCLASS_DEFN {ns=opt1215, ident=x1219, nonnullable=b1220, dynamic=b1221, 
          final=b1222, params=ls1224, extends=opt1229, implements=ls1234, classDefns=ls1239, 
          instanceDefns=ls1244, instanceStmts=ls1249, ctorDefn=opt1254} = PrettyRep.Rec [("ns", 
          
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1214 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1214))
       )), ("ident", cvtIDENT x1219), ("nonnullable", PrettyRep.Bool b1220), 
          ("dynamic", PrettyRep.Bool b1221), ("final", PrettyRep.Bool b1222), 
          ("params", PrettyRep.List (List.map (fn x1223 => cvtIDENT x1223
                                              ) ls1224)), ("extends", 
       (case opt1229 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1228 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1228))
       )), ("implements", PrettyRep.List (List.map (fn x1233 => cvtTYPE_EXPR x1233
                                                   ) ls1234)), ("classDefns", 
          PrettyRep.List (List.map (fn x1238 => cvtDEFN x1238
                                   ) ls1239)), ("instanceDefns", PrettyRep.List (List.map (fn x1243 => 
                                                                                                 cvtDEFN x1243
                                                                                          ) ls1244)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1248 => cvtSTMT x1248
                                                     ) ls1249)), ("ctorDefn", 
          
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1253))
       ))]
   and cvtINTERFACE_DEFN {ident=x1283, ns=opt1285, nonnullable=b1289, params=ls1291, 
          extends=ls1296, instanceDefns=ls1301} = PrettyRep.Rec [("ident", 
          cvtIDENT x1283), ("ns", 
       (case opt1285 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1284 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1284))
       )), ("nonnullable", PrettyRep.Bool b1289), ("params", PrettyRep.List (List.map (fn x1290 => 
                                                                                             cvtIDENT x1290
                                                                                      ) ls1291)), 
          ("extends", PrettyRep.List (List.map (fn x1295 => cvtTYPE_EXPR x1295
                                               ) ls1296)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1300 => cvtDEFN x1300
                                   ) ls1301))]
   and cvtTYPE_DEFN {ident=x1318, ns=opt1320, init=x1324} = PrettyRep.Rec [("ident", 
          cvtIDENT x1318), ("ns", 
       (case opt1320 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1319 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1319))
       )), ("init", cvtTYPE_EXPR x1324)]
   and cvtCLASS_BLOCK {ns=opt1333, ident=x1337, name=opt1339, block=x1343} = 
          PrettyRep.Rec [("ns", 
       (case opt1333 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1332 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1332))
       )), ("ident", cvtIDENT x1337), ("name", 
       (case opt1339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1338 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1338))
       )), ("block", cvtBLOCK x1343)]
   and cvtFOR_ENUM_HEAD {isEach=b1353, bindings=(ls1355, ls1360), expr=x1365} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1353), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1354 => 
                                                                                                                         cvtBINDING x1354
                                                                                                                  ) ls1355), 
          PrettyRep.List (List.map (fn x1359 => cvtINIT_STEP x1359
                                   ) ls1360)]), ("expr", cvtEXPR x1365)]
   and cvtFOR_ENUM_STMT {isEach=b1373, defn=opt1404, obj=x1408, rib=opt1416, 
          next=x1420, labels=ls1422, body=x1426} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1373), ("defn", 
       (case opt1404 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1374, ns=opt1376, static=b1380, prototype=b1381, bindings=(ls1383, 
            ls1388)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1374), ("ns", 
         (case opt1376 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1375 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1375))
         )), ("static", PrettyRep.Bool b1380), ("prototype", PrettyRep.Bool b1381), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1382 => 
                                                                          cvtBINDING x1382
                                                                   ) ls1383), 
            PrettyRep.List (List.map (fn x1387 => cvtINIT_STEP x1387
                                     ) ls1388)])]))
       )), ("obj", cvtEXPR x1408), ("rib", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1412 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1409, 
                                                                                      x1410) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1409, 
                                                                                      cvtFIXTURE x1410]
                                                                               ) ls1412)))
       )), ("next", cvtSTMT x1420), ("labels", PrettyRep.List (List.map (fn x1421 => 
                                                                               cvtIDENT x1421
                                                                        ) ls1422)), 
          ("body", cvtSTMT x1426)]
   and cvtFOR_STMT {rib=opt1449, defn=opt1483, init=ls1488, cond=x1492, update=x1493, 
          labels=ls1495, body=x1499} = PrettyRep.Rec [("rib", 
       (case opt1449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1445 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1442, 
                                                                                      x1443) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1442, 
                                                                                      cvtFIXTURE x1443]
                                                                               ) ls1445)))
       )), ("defn", 
       (case opt1483 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1453, ns=opt1455, static=b1459, prototype=b1460, bindings=(ls1462, 
            ls1467)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1453), ("ns", 
         (case opt1455 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1454 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1454))
         )), ("static", PrettyRep.Bool b1459), ("prototype", PrettyRep.Bool b1460), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1461 => 
                                                                          cvtBINDING x1461
                                                                   ) ls1462), 
            PrettyRep.List (List.map (fn x1466 => cvtINIT_STEP x1466
                                     ) ls1467)])]))
       )), ("init", PrettyRep.List (List.map (fn x1487 => cvtSTMT x1487
                                             ) ls1488)), ("cond", cvtEXPR x1492), 
          ("update", cvtEXPR x1493), ("labels", PrettyRep.List (List.map (fn x1494 => 
                                                                                cvtIDENT x1494
                                                                         ) ls1495)), 
          ("body", cvtSTMT x1499)]
   and cvtWHILE_STMT {cond=x1515, rib=opt1523, body=x1527, labels=ls1529} = 
          PrettyRep.Rec [("cond", cvtEXPR x1515), ("rib", 
       (case opt1523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1519 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1516, 
                                                                                      x1517) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1516, 
                                                                                      cvtFIXTURE x1517]
                                                                               ) ls1519)))
       )), ("body", cvtSTMT x1527), ("labels", PrettyRep.List (List.map (fn x1528 => 
                                                                               cvtIDENT x1528
                                                                        ) ls1529))]
   and cvtDIRECTIVES {pragmas=ls1543, defns=ls1548, head=opt1553, body=ls1558, 
          loc=opt1563} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1542 => 
                                                                                    cvtPRAGMA x1542
                                                                             ) ls1543)), 
          ("defns", PrettyRep.List (List.map (fn x1547 => cvtDEFN x1547
                                             ) ls1548)), ("head", 
       (case opt1553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1552 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1552))
       )), ("body", PrettyRep.List (List.map (fn x1557 => cvtSTMT x1557
                                             ) ls1558)), ("loc", 
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1562 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1562))
       ))]
   and cvtCASE {label=opt1579, inits=opt1590, body=x1594} = PrettyRep.Rec [("label", 
          
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1578 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1578))
       )), ("inits", 
       (case opt1590 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1586 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1583, 
                                                                                      x1584) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1583, 
                                                                                      cvtEXPR x1584]
                                                                               ) ls1586)))
       )), ("body", cvtBLOCK x1594)]
   and cvtCATCH_CLAUSE {bindings=(ls1603, ls1608), ty=x1613, rib=opt1621, inits=opt1632, 
          block=x1636} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1602 => 
                                                                                                      cvtBINDING x1602
                                                                                               ) ls1603), 
          PrettyRep.List (List.map (fn x1607 => cvtINIT_STEP x1607
                                   ) ls1608)]), ("ty", cvtTYPE_EXPR x1613), 
          ("rib", 
       (case opt1621 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1617 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1614, 
                                                                                      x1615) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1614, 
                                                                                      cvtFIXTURE x1615]
                                                                               ) ls1617)))
       )), ("inits", 
       (case opt1632 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1628 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1625, 
                                                                                      x1626) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1625, 
                                                                                      cvtEXPR x1626]
                                                                               ) ls1628)))
       )), ("block", cvtBLOCK x1636)]
   and cvtFUNC_NAME {kind=x1648, ident=x1649} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1648), 
          ("ident", cvtIDENT x1649)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1655, getter=opt1657, setter=opt1662} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1655), ("getter", 
       (case opt1657 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1656 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1656))
       )), ("setter", 
       (case opt1662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1661 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1661))
       ))]
   and cvtFRAGMENT (Package{name=ls1674, fragments=ls1679}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1673 => 
                                                                        cvtIDENT x1673
                                                                 ) ls1674)), 
          ("fragments", PrettyRep.List (List.map (fn x1678 => cvtFRAGMENT x1678
                                                 ) ls1679))]))
     | cvtFRAGMENT (Anon x1690) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1690))
end

