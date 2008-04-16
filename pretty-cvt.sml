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
     | cvtUNOP (Splat) = PrettyRep.Ctor ("Splat", NONE)
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
     | cvtEXPR (LiteralExpr x699) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x699))
     | cvtEXPR (CallExpr{func=x702, actuals=ls704}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x702), ("actuals", PrettyRep.List (List.map (fn x703 => 
                                                                                                   cvtEXPR x703
                                                                                            ) ls704))]))
     | cvtEXPR (ApplyTypeExpr{expr=x715, actuals=ls717}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x715), ("actuals", PrettyRep.List (List.map (fn x716 => 
                                                                                                   cvtTYPE_EXPR x716
                                                                                            ) ls717))]))
     | cvtEXPR (LetExpr{defs=x728, body=x729, head=opt731}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x728), ("body", cvtEXPR x729), 
          ("head", 
       (case opt731 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x730 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x730))
       ))]))
     | cvtEXPR (NewExpr{obj=x744, actuals=ls746}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x744), ("actuals", PrettyRep.List (List.map (fn x745 => 
                                                                                                  cvtEXPR x745
                                                                                           ) ls746))]))
     | cvtEXPR (ObjectRef{base=x757, ident=x758, loc=opt760}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x757), ("ident", cvtIDENT_EXPR x758), 
          ("loc", 
       (case opt760 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x759 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x759))
       ))]))
     | cvtEXPR (LexicalRef{ident=x773, loc=opt775}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x773), ("loc", 
       (case opt775 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x774 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x774))
       ))]))
     | cvtEXPR (SetExpr(x786, x787, x788)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x786, 
          cvtEXPR x787, cvtEXPR x788]))
     | cvtEXPR (ListExpr ls793) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x792 => 
                                                                                                    cvtEXPR x792
                                                                                             ) ls793)))
     | cvtEXPR (InitExpr(x799, x800, x801)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x799, 
          cvtHEAD x800, cvtINITS x801]))
     | cvtEXPR (GetTemp n805) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n805))
     | cvtEXPR (GetParam n808) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n808))
     | cvtEXPR (Comprehension(x811, ls813, opt818)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x811, PrettyRep.List (List.map (fn x812 => 
                                                                               cvtFOR_ENUM_HEAD x812
                                                                        ) ls813), 
          
       (case opt818 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x817 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x817))
       )]))
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
     | cvtFIXTURE (InheritedFixture{baseName=x1029, baseTypeArgs=ls1031}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1029), ("baseTypeArgs", PrettyRep.List (List.map (fn x1030 => 
                                                                           cvtTYPE_EXPR x1030
                                                                    ) ls1031))]))
   and cvtHEAD (Head(x1042, x1043)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1042, 
          cvtINITS x1043]))
   and cvtBINDINGS (ls1048, ls1053) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1047 => 
                                                                                       cvtBINDING x1047
                                                                                ) ls1048), 
          PrettyRep.List (List.map (fn x1052 => cvtINIT_STEP x1052
                                   ) ls1053)]
   and cvtRIB ls1061 = PrettyRep.List (List.map (fn (x1058, x1059) => PrettyRep.Tuple [cvtFIXTURE_NAME x1058, 
                                                       cvtFIXTURE x1059]
                                                ) ls1061)
   and cvtRIBS ls1066 = PrettyRep.List (List.map (fn x1065 => cvtRIB x1065
                                                 ) ls1066)
   and cvtINITS ls1073 = PrettyRep.List (List.map (fn (x1070, x1071) => PrettyRep.Tuple [cvtFIXTURE_NAME x1070, 
                                                         cvtEXPR x1071]
                                                  ) ls1073)
   and cvtINSTANCE_TYPE {name=x1077, typeParams=ls1079, typeArgs=ls1084, nonnullable=b1088, 
          superTypes=ls1090, ty=x1094, dynamic=b1095} = PrettyRep.Rec [("name", 
          cvtNAME x1077), ("typeParams", PrettyRep.List (List.map (fn x1078 => 
                                                                         cvtIDENT x1078
                                                                  ) ls1079)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1083 => cvtTYPE_EXPR x1083
                                                ) ls1084)), ("nonnullable", 
          PrettyRep.Bool b1088), ("superTypes", PrettyRep.List (List.map (fn x1089 => 
                                                                                cvtTYPE_EXPR x1089
                                                                         ) ls1090)), 
          ("ty", cvtTYPE_EXPR x1094), ("dynamic", PrettyRep.Bool b1095)]
   and cvtFIELD {kind=x1111, name=x1112, init=x1113} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1111), ("name", cvtIDENT_EXPR x1112), ("init", cvtEXPR x1113)]
   and cvtFIELD_TYPE {name=x1121, ty=x1122} = PrettyRep.Rec [("name", cvtIDENT x1121), 
          ("ty", cvtTYPE_EXPR x1122)]
   and cvtFUNC_TYPE {params=ls1129, result=x1133, thisType=opt1135, hasRest=b1139, 
          minArgs=n1140} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1128 => 
                                                                                     cvtTYPE_EXPR x1128
                                                                              ) ls1129)), 
          ("result", cvtTYPE_EXPR x1133), ("thisType", 
       (case opt1135 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1134 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1134))
       )), ("hasRest", PrettyRep.Bool b1139), ("minArgs", PrettyRep.Int n1140)]
   and cvtFUNC_DEFN {kind=x1152, ns=opt1154, final=b1158, override=b1159, prototype=b1160, 
          static=b1161, func=x1162} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1152), 
          ("ns", 
       (case opt1154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1153 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1153))
       )), ("final", PrettyRep.Bool b1158), ("override", PrettyRep.Bool b1159), 
          ("prototype", PrettyRep.Bool b1160), ("static", PrettyRep.Bool b1161), 
          ("func", cvtFUNC x1162)]
   and cvtCTOR_DEFN x1178 = cvtCTOR x1178
   and cvtVAR_DEFN {kind=x1179, ns=opt1181, static=b1185, prototype=b1186, 
          bindings=(ls1188, ls1193)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1179), 
          ("ns", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1180))
       )), ("static", PrettyRep.Bool b1185), ("prototype", PrettyRep.Bool b1186), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1187 => 
                                                                        cvtBINDING x1187
                                                                 ) ls1188), 
          PrettyRep.List (List.map (fn x1192 => cvtINIT_STEP x1192
                                   ) ls1193)])]
   and cvtNAMESPACE_DEFN {ident=x1209, ns=opt1211, init=opt1216} = PrettyRep.Rec [("ident", 
          cvtIDENT x1209), ("ns", 
       (case opt1211 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1210 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1210))
       )), ("init", 
       (case opt1216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1215 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1215))
       ))]
   and cvtCLASS_DEFN {ns=opt1228, ident=x1232, nonnullable=b1233, dynamic=b1234, 
          final=b1235, params=ls1237, extends=opt1242, implements=ls1247, classDefns=ls1252, 
          instanceDefns=ls1257, instanceStmts=ls1262, ctorDefn=opt1267} = PrettyRep.Rec [("ns", 
          
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1227))
       )), ("ident", cvtIDENT x1232), ("nonnullable", PrettyRep.Bool b1233), 
          ("dynamic", PrettyRep.Bool b1234), ("final", PrettyRep.Bool b1235), 
          ("params", PrettyRep.List (List.map (fn x1236 => cvtIDENT x1236
                                              ) ls1237)), ("extends", 
       (case opt1242 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1241 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1241))
       )), ("implements", PrettyRep.List (List.map (fn x1246 => cvtTYPE_EXPR x1246
                                                   ) ls1247)), ("classDefns", 
          PrettyRep.List (List.map (fn x1251 => cvtDEFN x1251
                                   ) ls1252)), ("instanceDefns", PrettyRep.List (List.map (fn x1256 => 
                                                                                                 cvtDEFN x1256
                                                                                          ) ls1257)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1261 => cvtSTMT x1261
                                                     ) ls1262)), ("ctorDefn", 
          
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1266))
       ))]
   and cvtINTERFACE_DEFN {ident=x1296, ns=opt1298, nonnullable=b1302, params=ls1304, 
          extends=ls1309, instanceDefns=ls1314} = PrettyRep.Rec [("ident", 
          cvtIDENT x1296), ("ns", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1297))
       )), ("nonnullable", PrettyRep.Bool b1302), ("params", PrettyRep.List (List.map (fn x1303 => 
                                                                                             cvtIDENT x1303
                                                                                      ) ls1304)), 
          ("extends", PrettyRep.List (List.map (fn x1308 => cvtTYPE_EXPR x1308
                                               ) ls1309)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1313 => cvtDEFN x1313
                                   ) ls1314))]
   and cvtTYPE_DEFN {ident=x1331, ns=opt1333, init=x1337} = PrettyRep.Rec [("ident", 
          cvtIDENT x1331), ("ns", 
       (case opt1333 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1332 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1332))
       )), ("init", cvtTYPE_EXPR x1337)]
   and cvtCLASS_BLOCK {ns=opt1346, ident=x1350, name=opt1352, block=x1356} = 
          PrettyRep.Rec [("ns", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1345))
       )), ("ident", cvtIDENT x1350), ("name", 
       (case opt1352 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1351 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1351))
       )), ("block", cvtBLOCK x1356)]
   and cvtFOR_ENUM_HEAD {isEach=b1366, bindings=(ls1368, ls1373), expr=x1378} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1366), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1367 => 
                                                                                                                         cvtBINDING x1367
                                                                                                                  ) ls1368), 
          PrettyRep.List (List.map (fn x1372 => cvtINIT_STEP x1372
                                   ) ls1373)]), ("expr", cvtEXPR x1378)]
   and cvtFOR_ENUM_STMT {isEach=b1386, defn=opt1417, obj=x1421, rib=opt1429, 
          next=x1433, labels=ls1435, body=x1439} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1386), ("defn", 
       (case opt1417 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1387, ns=opt1389, static=b1393, prototype=b1394, bindings=(ls1396, 
            ls1401)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1387), ("ns", 
         (case opt1389 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1388 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1388))
         )), ("static", PrettyRep.Bool b1393), ("prototype", PrettyRep.Bool b1394), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1395 => 
                                                                          cvtBINDING x1395
                                                                   ) ls1396), 
            PrettyRep.List (List.map (fn x1400 => cvtINIT_STEP x1400
                                     ) ls1401)])]))
       )), ("obj", cvtEXPR x1421), ("rib", 
       (case opt1429 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1425 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1422, 
                                                                                      x1423) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1422, 
                                                                                      cvtFIXTURE x1423]
                                                                               ) ls1425)))
       )), ("next", cvtSTMT x1433), ("labels", PrettyRep.List (List.map (fn x1434 => 
                                                                               cvtIDENT x1434
                                                                        ) ls1435)), 
          ("body", cvtSTMT x1439)]
   and cvtFOR_STMT {rib=opt1462, defn=opt1496, init=ls1501, cond=x1505, update=x1506, 
          labels=ls1508, body=x1512} = PrettyRep.Rec [("rib", 
       (case opt1462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1458 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1455, 
                                                                                      x1456) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1455, 
                                                                                      cvtFIXTURE x1456]
                                                                               ) ls1458)))
       )), ("defn", 
       (case opt1496 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1466, ns=opt1468, static=b1472, prototype=b1473, bindings=(ls1475, 
            ls1480)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1466), ("ns", 
         (case opt1468 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1467 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1467))
         )), ("static", PrettyRep.Bool b1472), ("prototype", PrettyRep.Bool b1473), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1474 => 
                                                                          cvtBINDING x1474
                                                                   ) ls1475), 
            PrettyRep.List (List.map (fn x1479 => cvtINIT_STEP x1479
                                     ) ls1480)])]))
       )), ("init", PrettyRep.List (List.map (fn x1500 => cvtSTMT x1500
                                             ) ls1501)), ("cond", cvtEXPR x1505), 
          ("update", cvtEXPR x1506), ("labels", PrettyRep.List (List.map (fn x1507 => 
                                                                                cvtIDENT x1507
                                                                         ) ls1508)), 
          ("body", cvtSTMT x1512)]
   and cvtWHILE_STMT {cond=x1528, rib=opt1536, body=x1540, labels=ls1542} = 
          PrettyRep.Rec [("cond", cvtEXPR x1528), ("rib", 
       (case opt1536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1532 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1529, 
                                                                                      x1530) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1529, 
                                                                                      cvtFIXTURE x1530]
                                                                               ) ls1532)))
       )), ("body", cvtSTMT x1540), ("labels", PrettyRep.List (List.map (fn x1541 => 
                                                                               cvtIDENT x1541
                                                                        ) ls1542))]
   and cvtDIRECTIVES {pragmas=ls1556, defns=ls1561, head=opt1566, body=ls1571, 
          loc=opt1576} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1555 => 
                                                                                    cvtPRAGMA x1555
                                                                             ) ls1556)), 
          ("defns", PrettyRep.List (List.map (fn x1560 => cvtDEFN x1560
                                             ) ls1561)), ("head", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1565))
       )), ("body", PrettyRep.List (List.map (fn x1570 => cvtSTMT x1570
                                             ) ls1571)), ("loc", 
       (case opt1576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1575 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1575))
       ))]
   and cvtCASE {label=opt1592, inits=opt1603, body=x1607} = PrettyRep.Rec [("label", 
          
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1591))
       )), ("inits", 
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1599 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1596, 
                                                                                      x1597) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1596, 
                                                                                      cvtEXPR x1597]
                                                                               ) ls1599)))
       )), ("body", cvtBLOCK x1607)]
   and cvtCATCH_CLAUSE {bindings=(ls1616, ls1621), ty=x1626, rib=opt1634, inits=opt1645, 
          block=x1649} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1615 => 
                                                                                                      cvtBINDING x1615
                                                                                               ) ls1616), 
          PrettyRep.List (List.map (fn x1620 => cvtINIT_STEP x1620
                                   ) ls1621)]), ("ty", cvtTYPE_EXPR x1626), 
          ("rib", 
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1630 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1627, 
                                                                                      x1628) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1627, 
                                                                                      cvtFIXTURE x1628]
                                                                               ) ls1630)))
       )), ("inits", 
       (case opt1645 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1641 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1638, 
                                                                                      x1639) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1638, 
                                                                                      cvtEXPR x1639]
                                                                               ) ls1641)))
       )), ("block", cvtBLOCK x1649)]
   and cvtFUNC_NAME {kind=x1661, ident=x1662} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1661), 
          ("ident", cvtIDENT x1662)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1668, getter=opt1670, setter=opt1675} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1668), ("getter", 
       (case opt1670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1669 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1669))
       )), ("setter", 
       (case opt1675 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1674 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1674))
       ))]
   and cvtFRAGMENT (Package{name=ls1687, fragments=ls1692}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1686 => 
                                                                        cvtIDENT x1686
                                                                 ) ls1687)), 
          ("fragments", PrettyRep.List (List.map (fn x1691 => cvtFRAGMENT x1691
                                                 ) ls1692))]))
     | cvtFRAGMENT (Anon x1703) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1703))
end

