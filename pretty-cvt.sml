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
   and cvtTYPEVAR_NONCE n26 = PrettyRep.Int n26
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x29) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x29))
     | cvtNAMESPACE (Protected x32) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x32))
     | cvtNAMESPACE (Public x35) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x35))
     | cvtNAMESPACE (Internal x38) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x38))
     | cvtNAMESPACE (UserNamespace s41) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s41))
     | cvtNAMESPACE (AnonUserNamespace n44) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n44))
     | cvtNAMESPACE (LimitedNamespace(x47, x48)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x47, cvtNAMESPACE x48]))
   and cvtNAME {ns=x52, id=x53} = PrettyRep.Rec [("ns", cvtNAMESPACE x52), 
          ("id", cvtIDENT x53)]
   and cvtMULTINAME {nss=ls64, id=x68} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls60 => 
                                                                                                PrettyRep.List (List.map (fn x59 => 
                                                                                                                                cvtNAMESPACE x59
                                                                                                                         ) ls60)
                                                                                         ) ls64)), 
          ("id", cvtIDENT x68)]
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
   and cvtPRAGMA (UseNamespace x135) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x135))
     | cvtPRAGMA (UseDefaultNamespace x138) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x138))
     | cvtPRAGMA (UseDecimalContext x141) = PrettyRep.Ctor ("UseDecimalContext", 
          SOME (cvtEXPR x141))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls147, name=x151}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x146 => 
                                                                           cvtIDENT x146
                                                                    ) ls147)), 
          ("name", cvtIDENT x151)]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtTY (Ty{expr=x165, ribId=opt167}) = PrettyRep.Ctor ("Ty", SOME (PrettyRep.Rec [("expr", 
          cvtTYPE_EXPR x165), ("ribId", 
       (case opt167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtRIB_ID x166))
       ))]))
   and cvtCLS (Cls{name=x178, typeParams=ls180, nonnullable=b184, dynamic=b185, 
          extends=opt187, implements=ls192, classRib=x196, instanceRib=x197, 
          instanceInits=x198, constructor=opt200, classType=x204, instanceType=x205}) = 
          PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", cvtNAME x178), 
          ("typeParams", PrettyRep.List (List.map (fn x179 => cvtIDENT x179
                                                  ) ls180)), ("nonnullable", 
          PrettyRep.Bool b184), ("dynamic", PrettyRep.Bool b185), ("extends", 
          
       (case opt187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x186 => PrettyRep.Ctor ("SOME", SOME (cvtTY x186))
       )), ("implements", PrettyRep.List (List.map (fn x191 => cvtTY x191
                                                   ) ls192)), ("classRib", 
          cvtRIB x196), ("instanceRib", cvtRIB x197), ("instanceInits", cvtHEAD x198), 
          ("constructor", 
       (case opt200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x199 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x199))
       )), ("classType", cvtTY x204), ("instanceType", cvtTY x205)]))
   and cvtIFACE (Iface{name=x233, typeParams=ls235, nonnullable=b239, extends=ls241, 
          instanceRib=x245, instanceType=x246}) = PrettyRep.Ctor ("Iface", 
          SOME (PrettyRep.Rec [("name", cvtNAME x233), ("typeParams", PrettyRep.List (List.map (fn x234 => 
                                                                                                      cvtIDENT x234
                                                                                               ) ls235)), 
          ("nonnullable", PrettyRep.Bool b239), ("extends", PrettyRep.List (List.map (fn x240 => 
                                                                                            cvtTY x240
                                                                                     ) ls241)), 
          ("instanceRib", cvtRIB x245), ("instanceType", cvtTY x246)]))
   and cvtCTOR (Ctor{settings=x262, superArgs=ls264, func=x268}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x262), ("superArgs", PrettyRep.List (List.map (fn x263 => 
                                                                                                         cvtEXPR x263
                                                                                                  ) ls264)), 
          ("func", cvtFUNC x268)]))
   and cvtFUNC (Func{name=x278, fsig=x279, native=b280, block=opt282, param=x286, 
          defaults=ls288, ty=x292, loc=opt294}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x278), ("fsig", cvtFUNC_SIG x279), ("native", PrettyRep.Bool b280), 
          ("block", 
       (case opt282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x281 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x281))
       )), ("param", cvtHEAD x286), ("defaults", PrettyRep.List (List.map (fn x287 => 
                                                                                 cvtEXPR x287
                                                                          ) ls288)), 
          ("ty", cvtTY x292), ("loc", 
       (case opt294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x293 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x293))
       ))]))
   and cvtDEFN (ClassDefn x317) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x317))
     | cvtDEFN (VariableDefn x320) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x320))
     | cvtDEFN (FunctionDefn x323) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x323))
     | cvtDEFN (ConstructorDefn x326) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x326))
     | cvtDEFN (InterfaceDefn x329) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x329))
     | cvtDEFN (NamespaceDefn x332) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x332))
     | cvtDEFN (TypeDefn x335) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x335))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls339, params=x343, paramTypes=ls345, 
          defaults=ls350, ctorInits=opt361, returnType=x365, thisType=opt367, 
          hasRest=b371}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x338 => cvtIDENT x338
                                   ) ls339)), ("params", cvtBINDINGS x343), 
          ("paramTypes", PrettyRep.List (List.map (fn x344 => cvtTYPE_EXPR x344
                                                  ) ls345)), ("defaults", PrettyRep.List (List.map (fn x349 => 
                                                                                                          cvtEXPR x349
                                                                                                   ) ls350)), 
          ("ctorInits", 
       (case opt361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x354, ls356) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x354, 
            PrettyRep.List (List.map (fn x355 => cvtEXPR x355
                                     ) ls356)]))
       )), ("returnType", cvtTYPE_EXPR x365), ("thisType", 
       (case opt367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x366 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x366))
       )), ("hasRest", PrettyRep.Bool b371)]))
   and cvtBINDING (Binding{ident=x391, ty=x392}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x391), ("ty", cvtTYPE_EXPR x392)]))
   and cvtBINDING_IDENT (TempIdent n400) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n400))
     | cvtBINDING_IDENT (ParamIdent n403) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n403))
     | cvtBINDING_IDENT (PropIdent x406) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x406))
   and cvtINIT_STEP (InitStep(x409, x410)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x409, 
          cvtEXPR x410]))
     | cvtINIT_STEP (AssignStep(x414, x415)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x414, cvtEXPR x415]))
   and cvtTYPE_EXPR (SpecialType x419) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x419))
     | cvtTYPE_EXPR (UnionType ls423) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x422 => 
                                                                                                           cvtTYPE_EXPR x422
                                                                                                    ) ls423)))
     | cvtTYPE_EXPR (ArrayType ls430) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x429 => 
                                                                                                           cvtTYPE_EXPR x429
                                                                                                    ) ls430)))
     | cvtTYPE_EXPR (TypeName x436) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x436))
     | cvtTYPE_EXPR (ElementTypeRef(x439, n440)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x439, PrettyRep.Int n440]))
     | cvtTYPE_EXPR (FieldTypeRef(x444, x445)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x444, cvtIDENT x445]))
     | cvtTYPE_EXPR (FunctionType x449) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x449))
     | cvtTYPE_EXPR (ObjectType ls453) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x452 => 
                                                                                                             cvtFIELD_TYPE x452
                                                                                                      ) ls453)))
     | cvtTYPE_EXPR (LikeType x459) = PrettyRep.Ctor ("LikeType", SOME (cvtTYPE_EXPR x459))
     | cvtTYPE_EXPR (AppType{base=x462, args=ls464}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x462), ("args", PrettyRep.List (List.map (fn x463 => 
                                                                                                     cvtTYPE_EXPR x463
                                                                                              ) ls464))]))
     | cvtTYPE_EXPR (LamType{params=ls476, body=x480}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x475 => 
                                                                          cvtIDENT x475
                                                                   ) ls476)), 
          ("body", cvtTYPE_EXPR x480)]))
     | cvtTYPE_EXPR (NullableType{expr=x488, nullable=b489}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x488), ("nullable", PrettyRep.Bool b489)]))
     | cvtTYPE_EXPR (InstanceType x497) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x497))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x501) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x501))
     | cvtSTMT (InitStmt{kind=x504, ns=opt506, prototype=b510, static=b511, 
          temps=x512, inits=ls514}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x504), ("ns", 
       (case opt506 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x505 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x505))
       )), ("prototype", PrettyRep.Bool b510), ("static", PrettyRep.Bool b511), 
          ("temps", cvtBINDINGS x512), ("inits", PrettyRep.List (List.map (fn x513 => 
                                                                                 cvtINIT_STEP x513
                                                                          ) ls514))]))
     | cvtSTMT (ClassBlock x533) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x533))
     | cvtSTMT (ForInStmt x536) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x536))
     | cvtSTMT (ThrowStmt x539) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x539))
     | cvtSTMT (ReturnStmt x542) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x542))
     | cvtSTMT (BreakStmt opt546) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt546 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x545 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x545))
       ))
     | cvtSTMT (ContinueStmt opt553) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt553 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x552 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x552))
       ))
     | cvtSTMT (BlockStmt x559) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x559))
     | cvtSTMT (LabeledStmt(x562, x563)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x562, 
          cvtSTMT x563]))
     | cvtSTMT (LetStmt x567) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x567))
     | cvtSTMT (WhileStmt x570) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x570))
     | cvtSTMT (DoWhileStmt x573) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x573))
     | cvtSTMT (ForStmt x576) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x576))
     | cvtSTMT (IfStmt{cnd=x579, thn=x580, els=x581}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x579), ("thn", cvtSTMT x580), 
          ("els", cvtSTMT x581)]))
     | cvtSTMT (WithStmt{obj=x591, ty=x592, body=x593}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x591), ("ty", cvtTY x592), ("body", 
          cvtSTMT x593)]))
     | cvtSTMT (TryStmt{block=x603, catches=ls605, finally=opt610}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x603), ("catches", PrettyRep.List (List.map (fn x604 => 
                                                                                                     cvtCATCH_CLAUSE x604
                                                                                              ) ls605)), 
          ("finally", 
       (case opt610 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x609 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x609))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x623, labels=ls625, cases=ls630}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x623), ("labels", PrettyRep.List (List.map (fn x624 => 
                                                                                                  cvtIDENT x624
                                                                                           ) ls625)), 
          ("cases", PrettyRep.List (List.map (fn x629 => cvtCASE x629
                                             ) ls630))]))
     | cvtSTMT (SwitchTypeStmt{cond=x643, ty=x644, cases=ls646}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x643), ("ty", cvtTY x644), 
          ("cases", PrettyRep.List (List.map (fn x645 => cvtCATCH_CLAUSE x645
                                             ) ls646))]))
     | cvtSTMT (DXNStmt{expr=x659}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x659)]))
   and cvtEXPR (TernaryExpr(x665, x666, x667)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x665, cvtEXPR x666, cvtEXPR x667]))
     | cvtEXPR (BinaryExpr(x671, x672, x673)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x671, cvtEXPR x672, cvtEXPR x673]))
     | cvtEXPR (BinaryTypeExpr(x677, x678, x679)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x677, cvtEXPR x678, cvtTY x679]))
     | cvtEXPR (ExpectedTypeExpr(x683, x684)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x683, cvtEXPR x684]))
     | cvtEXPR (UnaryExpr(x688, x689)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x688, 
          cvtEXPR x689]))
     | cvtEXPR (TypeExpr x693) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x693))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt698) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x697 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x697))
       ))
     | cvtEXPR (SuperExpr opt705) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt705 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x704 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x704))
       ))
     | cvtEXPR (LiteralExpr x711) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x711))
     | cvtEXPR (CallExpr{func=x714, actuals=ls716}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x714), ("actuals", PrettyRep.List (List.map (fn x715 => 
                                                                                                   cvtEXPR x715
                                                                                            ) ls716))]))
     | cvtEXPR (ApplyTypeExpr{expr=x727, actuals=ls729}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x727), ("actuals", PrettyRep.List (List.map (fn x728 => 
                                                                                                   cvtTY x728
                                                                                            ) ls729))]))
     | cvtEXPR (LetExpr{defs=x740, body=x741, head=opt743}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x740), ("body", cvtEXPR x741), 
          ("head", 
       (case opt743 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x742 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x742))
       ))]))
     | cvtEXPR (NewExpr{obj=x756, actuals=ls758}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x756), ("actuals", PrettyRep.List (List.map (fn x757 => 
                                                                                                  cvtEXPR x757
                                                                                           ) ls758))]))
     | cvtEXPR (ObjectRef{base=x769, ident=x770, loc=opt772}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x769), ("ident", cvtIDENT_EXPR x770), 
          ("loc", 
       (case opt772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x771 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x771))
       ))]))
     | cvtEXPR (LexicalRef{ident=x785, loc=opt787}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x785), ("loc", 
       (case opt787 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x786 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x786))
       ))]))
     | cvtEXPR (SetExpr(x798, x799, x800)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x798, 
          cvtEXPR x799, cvtEXPR x800]))
     | cvtEXPR (ListExpr ls805) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x804 => 
                                                                                                    cvtEXPR x804
                                                                                             ) ls805)))
     | cvtEXPR (InitExpr(x811, x812, x813)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x811, 
          cvtHEAD x812, cvtINITS x813]))
     | cvtEXPR (GetTemp n817) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n817))
     | cvtEXPR (GetParam n820) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n820))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n826) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n826))
     | cvtFIXTURE_NAME (PropName x829) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x829))
   and cvtIDENT_EXPR (Identifier{ident=x832, openNamespaces=ls838}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x832), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls834 => PrettyRep.List (List.map (fn x833 => 
                                                                                cvtNAMESPACE x833
                                                                         ) ls834)
                                   ) ls838))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x849, expr=x850}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x849), ("expr", cvtEXPR x850)]))
     | cvtIDENT_EXPR (AttributeIdentifier x858) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x858))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x861, openNamespaces=ls867}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x861), ("openNamespaces", PrettyRep.List (List.map (fn ls863 => 
                                                                            PrettyRep.List (List.map (fn x862 => 
                                                                                                            cvtNAMESPACE x862
                                                                                                     ) ls863)
                                                                     ) ls867))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x878, ident=s879}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x878), ("ident", PrettyRep.UniStr s879)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls888, x892)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x887 => cvtIDENT x887
                                                          ) ls888), cvtIDENT_EXPR x892]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r899) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r899))
     | cvtLITERAL (LiteralDecimal d902) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d902))
     | cvtLITERAL (LiteralInt i905) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i905))
     | cvtLITERAL (LiteralUInt u908) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u908))
     | cvtLITERAL (LiteralBoolean b911) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b911))
     | cvtLITERAL (LiteralString s914) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s914))
     | cvtLITERAL (LiteralArray{exprs=ls918, ty=opt923}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x917 => 
                                                                         cvtEXPR x917
                                                                  ) ls918)), 
          ("ty", 
       (case opt923 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x922 => PrettyRep.Ctor ("SOME", SOME (cvtTY x922))
       ))]))
     | cvtLITERAL (LiteralXML ls935) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x934 => 
                                                                                                           cvtEXPR x934
                                                                                                    ) ls935)))
     | cvtLITERAL (LiteralNamespace x941) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x941))
     | cvtLITERAL (LiteralObject{expr=ls945, ty=opt950}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x944 => 
                                                                        cvtFIELD x944
                                                                 ) ls945)), 
          ("ty", 
       (case opt950 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x949 => PrettyRep.Ctor ("SOME", SOME (cvtTY x949))
       ))]))
     | cvtLITERAL (LiteralFunction x961) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x961))
     | cvtLITERAL (LiteralRegExp{str=s964}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s964)]))
   and cvtBLOCK (Block x970) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x970))
   and cvtFIXTURE (NamespaceFixture x973) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x973))
     | cvtFIXTURE (ClassFixture x976) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x976))
     | cvtFIXTURE (InterfaceFixture x979) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x979))
     | cvtFIXTURE (TypeVarFixture x982) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x982))
     | cvtFIXTURE (TypeFixture x985) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x985))
     | cvtFIXTURE (MethodFixture{func=x988, ty=x989, readOnly=b990, override=b991, 
          final=b992}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x988), ("ty", cvtTY x989), ("readOnly", PrettyRep.Bool b990), 
          ("override", PrettyRep.Bool b991), ("final", PrettyRep.Bool b992)]))
     | cvtFIXTURE (ValFixture{ty=x1006, readOnly=b1007}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1006), ("readOnly", PrettyRep.Bool b1007)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1015, getter=opt1017, setter=opt1022}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1015), ("getter", 
       (case opt1017 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1016 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1016))
       )), ("setter", 
       (case opt1022 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1021 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1021))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1035, baseTypeArgs=ls1037}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1035), ("baseTypeArgs", PrettyRep.List (List.map (fn x1036 => 
                                                                           cvtTY x1036
                                                                    ) ls1037))]))
   and cvtHEAD (Head(x1048, x1049)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1048, 
          cvtINITS x1049]))
   and cvtBINDINGS (ls1054, ls1059) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1053 => 
                                                                                       cvtBINDING x1053
                                                                                ) ls1054), 
          PrettyRep.List (List.map (fn x1058 => cvtINIT_STEP x1058
                                   ) ls1059)]
   and cvtRIB ls1067 = PrettyRep.List (List.map (fn (x1064, x1065) => PrettyRep.Tuple [cvtFIXTURE_NAME x1064, 
                                                       cvtFIXTURE x1065]
                                                ) ls1067)
   and cvtRIBS ls1078 = PrettyRep.List (List.map (fn ls1074 => PrettyRep.List (List.map (fn (x1071, 
                                                                                               x1072) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1071, 
                                                                                               cvtFIXTURE x1072]
                                                                                        ) ls1074)
                                                 ) ls1078)
   and cvtINITS ls1085 = PrettyRep.List (List.map (fn (x1082, x1083) => PrettyRep.Tuple [cvtFIXTURE_NAME x1082, 
                                                         cvtEXPR x1083]
                                                  ) ls1085)
   and cvtINSTANCE_TYPE {name=x1089, typeParams=ls1091, typeArgs=ls1096, nonnullable=b1100, 
          superTypes=ls1102, ty=x1106, dynamic=b1107} = PrettyRep.Rec [("name", 
          cvtNAME x1089), ("typeParams", PrettyRep.List (List.map (fn x1090 => 
                                                                         cvtIDENT x1090
                                                                  ) ls1091)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1095 => cvtTYPE_EXPR x1095
                                                ) ls1096)), ("nonnullable", 
          PrettyRep.Bool b1100), ("superTypes", PrettyRep.List (List.map (fn x1101 => 
                                                                                cvtTYPE_EXPR x1101
                                                                         ) ls1102)), 
          ("ty", cvtTYPE_EXPR x1106), ("dynamic", PrettyRep.Bool b1107)]
   and cvtFIELD {kind=x1123, name=x1124, init=x1125} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1123), ("name", cvtIDENT_EXPR x1124), ("init", cvtEXPR x1125)]
   and cvtFIELD_TYPE {name=x1133, ty=x1134} = PrettyRep.Rec [("name", cvtIDENT x1133), 
          ("ty", cvtTYPE_EXPR x1134)]
   and cvtFUNC_TYPE {params=ls1141, result=x1145, thisType=opt1147, hasRest=b1151, 
          minArgs=n1152} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1140 => 
                                                                                     cvtTYPE_EXPR x1140
                                                                              ) ls1141)), 
          ("result", cvtTYPE_EXPR x1145), ("thisType", 
       (case opt1147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1146 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1146))
       )), ("hasRest", PrettyRep.Bool b1151), ("minArgs", PrettyRep.Int n1152)]
   and cvtFUNC_DEFN {kind=x1164, ns=opt1166, final=b1170, override=b1171, prototype=b1172, 
          static=b1173, func=x1174} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1164), 
          ("ns", 
       (case opt1166 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1165 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1165))
       )), ("final", PrettyRep.Bool b1170), ("override", PrettyRep.Bool b1171), 
          ("prototype", PrettyRep.Bool b1172), ("static", PrettyRep.Bool b1173), 
          ("func", cvtFUNC x1174)]
   and cvtCTOR_DEFN x1190 = cvtCTOR x1190
   and cvtVAR_DEFN {kind=x1191, ns=opt1193, static=b1197, prototype=b1198, 
          bindings=(ls1200, ls1205)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1191), 
          ("ns", 
       (case opt1193 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1192 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1192))
       )), ("static", PrettyRep.Bool b1197), ("prototype", PrettyRep.Bool b1198), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1199 => 
                                                                        cvtBINDING x1199
                                                                 ) ls1200), 
          PrettyRep.List (List.map (fn x1204 => cvtINIT_STEP x1204
                                   ) ls1205)])]
   and cvtNAMESPACE_DEFN {ident=x1221, ns=opt1223, init=opt1228} = PrettyRep.Rec [("ident", 
          cvtIDENT x1221), ("ns", 
       (case opt1223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1222 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1222))
       )), ("init", 
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1227))
       ))]
   and cvtCLASS_DEFN {ns=opt1240, ident=x1244, nonnullable=b1245, dynamic=b1246, 
          final=b1247, params=ls1249, extends=opt1254, implements=ls1259, classDefns=ls1264, 
          instanceDefns=ls1269, instanceStmts=ls1274, ctorDefn=opt1279} = PrettyRep.Rec [("ns", 
          
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1239))
       )), ("ident", cvtIDENT x1244), ("nonnullable", PrettyRep.Bool b1245), 
          ("dynamic", PrettyRep.Bool b1246), ("final", PrettyRep.Bool b1247), 
          ("params", PrettyRep.List (List.map (fn x1248 => cvtIDENT x1248
                                              ) ls1249)), ("extends", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1253))
       )), ("implements", PrettyRep.List (List.map (fn x1258 => cvtTYPE_EXPR x1258
                                                   ) ls1259)), ("classDefns", 
          PrettyRep.List (List.map (fn x1263 => cvtDEFN x1263
                                   ) ls1264)), ("instanceDefns", PrettyRep.List (List.map (fn x1268 => 
                                                                                                 cvtDEFN x1268
                                                                                          ) ls1269)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1273 => cvtSTMT x1273
                                                     ) ls1274)), ("ctorDefn", 
          
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1278))
       ))]
   and cvtINTERFACE_DEFN {ident=x1308, ns=opt1310, nonnullable=b1314, params=ls1316, 
          extends=ls1321, instanceDefns=ls1326} = PrettyRep.Rec [("ident", 
          cvtIDENT x1308), ("ns", 
       (case opt1310 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1309 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1309))
       )), ("nonnullable", PrettyRep.Bool b1314), ("params", PrettyRep.List (List.map (fn x1315 => 
                                                                                             cvtIDENT x1315
                                                                                      ) ls1316)), 
          ("extends", PrettyRep.List (List.map (fn x1320 => cvtTYPE_EXPR x1320
                                               ) ls1321)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1325 => cvtDEFN x1325
                                   ) ls1326))]
   and cvtTYPE_DEFN {ident=x1343, ns=opt1345, init=x1349} = PrettyRep.Rec [("ident", 
          cvtIDENT x1343), ("ns", 
       (case opt1345 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1344 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1344))
       )), ("init", cvtTYPE_EXPR x1349)]
   and cvtCLASS_BLOCK {ns=opt1358, ident=x1362, name=opt1364, block=x1368} = 
          PrettyRep.Rec [("ns", 
       (case opt1358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1357))
       )), ("ident", cvtIDENT x1362), ("name", 
       (case opt1364 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1363 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1363))
       )), ("block", cvtBLOCK x1368)]
   and cvtFOR_ENUM_STMT {isEach=b1378, defn=opt1409, obj=x1413, rib=opt1421, 
          next=x1425, labels=ls1427, body=x1431} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1378), ("defn", 
       (case opt1409 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1379, ns=opt1381, static=b1385, prototype=b1386, bindings=(ls1388, 
            ls1393)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1379), ("ns", 
         (case opt1381 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1380 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1380))
         )), ("static", PrettyRep.Bool b1385), ("prototype", PrettyRep.Bool b1386), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1387 => 
                                                                          cvtBINDING x1387
                                                                   ) ls1388), 
            PrettyRep.List (List.map (fn x1392 => cvtINIT_STEP x1392
                                     ) ls1393)])]))
       )), ("obj", cvtEXPR x1413), ("rib", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1417 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1414, 
                                                                                      x1415) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1414, 
                                                                                      cvtFIXTURE x1415]
                                                                               ) ls1417)))
       )), ("next", cvtSTMT x1425), ("labels", PrettyRep.List (List.map (fn x1426 => 
                                                                               cvtIDENT x1426
                                                                        ) ls1427)), 
          ("body", cvtSTMT x1431)]
   and cvtFOR_STMT {rib=opt1454, defn=opt1488, init=ls1493, cond=x1497, update=x1498, 
          labels=ls1500, body=x1504} = PrettyRep.Rec [("rib", 
       (case opt1454 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1450 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1447, 
                                                                                      x1448) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1447, 
                                                                                      cvtFIXTURE x1448]
                                                                               ) ls1450)))
       )), ("defn", 
       (case opt1488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1458, ns=opt1460, static=b1464, prototype=b1465, bindings=(ls1467, 
            ls1472)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1458), ("ns", 
         (case opt1460 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1459))
         )), ("static", PrettyRep.Bool b1464), ("prototype", PrettyRep.Bool b1465), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1466 => 
                                                                          cvtBINDING x1466
                                                                   ) ls1467), 
            PrettyRep.List (List.map (fn x1471 => cvtINIT_STEP x1471
                                     ) ls1472)])]))
       )), ("init", PrettyRep.List (List.map (fn x1492 => cvtSTMT x1492
                                             ) ls1493)), ("cond", cvtEXPR x1497), 
          ("update", cvtEXPR x1498), ("labels", PrettyRep.List (List.map (fn x1499 => 
                                                                                cvtIDENT x1499
                                                                         ) ls1500)), 
          ("body", cvtSTMT x1504)]
   and cvtWHILE_STMT {cond=x1520, rib=opt1528, body=x1532, labels=ls1534} = 
          PrettyRep.Rec [("cond", cvtEXPR x1520), ("rib", 
       (case opt1528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1524 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1521, 
                                                                                      x1522) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1521, 
                                                                                      cvtFIXTURE x1522]
                                                                               ) ls1524)))
       )), ("body", cvtSTMT x1532), ("labels", PrettyRep.List (List.map (fn x1533 => 
                                                                               cvtIDENT x1533
                                                                        ) ls1534))]
   and cvtDIRECTIVES {pragmas=ls1548, defns=ls1553, head=opt1558, body=ls1563, 
          loc=opt1568} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1547 => 
                                                                                    cvtPRAGMA x1547
                                                                             ) ls1548)), 
          ("defns", PrettyRep.List (List.map (fn x1552 => cvtDEFN x1552
                                             ) ls1553)), ("head", 
       (case opt1558 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1557 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1557))
       )), ("body", PrettyRep.List (List.map (fn x1562 => cvtSTMT x1562
                                             ) ls1563)), ("loc", 
       (case opt1568 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1567))
       ))]
   and cvtCASE {label=opt1584, inits=opt1595, body=x1599} = PrettyRep.Rec [("label", 
          
       (case opt1584 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1583 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1583))
       )), ("inits", 
       (case opt1595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1591 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1588, 
                                                                                      x1589) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1588, 
                                                                                      cvtEXPR x1589]
                                                                               ) ls1591)))
       )), ("body", cvtBLOCK x1599)]
   and cvtCATCH_CLAUSE {bindings=(ls1608, ls1613), ty=x1618, rib=opt1626, inits=opt1637, 
          block=x1641} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1607 => 
                                                                                                      cvtBINDING x1607
                                                                                               ) ls1608), 
          PrettyRep.List (List.map (fn x1612 => cvtINIT_STEP x1612
                                   ) ls1613)]), ("ty", cvtTY x1618), ("rib", 
          
       (case opt1626 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1622 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1619, 
                                                                                      x1620) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1619, 
                                                                                      cvtFIXTURE x1620]
                                                                               ) ls1622)))
       )), ("inits", 
       (case opt1637 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1633 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1630, 
                                                                                      x1631) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1630, 
                                                                                      cvtEXPR x1631]
                                                                               ) ls1633)))
       )), ("block", cvtBLOCK x1641)]
   and cvtFUNC_NAME {kind=x1653, ident=x1654} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1653), 
          ("ident", cvtIDENT x1654)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1660, getter=opt1662, setter=opt1667} = 
          PrettyRep.Rec [("ty", cvtTY x1660), ("getter", 
       (case opt1662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1661 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1661))
       )), ("setter", 
       (case opt1667 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1666 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1666))
       ))]
   and cvtFRAGMENT (Unit{name=opt1679, fragments=ls1684}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1679 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1678 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1678))
       )), ("fragments", PrettyRep.List (List.map (fn x1683 => cvtFRAGMENT x1683
                                                  ) ls1684))]))
     | cvtFRAGMENT (Package{name=ls1696, fragments=ls1701}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1695 => 
                                                                        cvtIDENT x1695
                                                                 ) ls1696)), 
          ("fragments", PrettyRep.List (List.map (fn x1700 => cvtFRAGMENT x1700
                                                 ) ls1701))]))
     | cvtFRAGMENT (Anon x1712) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1712))
end

