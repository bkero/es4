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
     | cvtUNOP (Splat) = PrettyRep.Ctor ("Splat", NONE)
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
     | cvtEXPR (UnaryExpr(x683, x684)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x683, 
          cvtEXPR x684]))
     | cvtEXPR (TypeExpr x688) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x688))
     | cvtEXPR (ThisExpr opt692) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt692 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x691 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x691))
       ))
     | cvtEXPR (YieldExpr opt699) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt699 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x698 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x698))
       ))
     | cvtEXPR (SuperExpr opt706) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt706 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x705 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x705))
       ))
     | cvtEXPR (LiteralExpr x712) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x712))
     | cvtEXPR (CallExpr{func=x715, actuals=ls717}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x715), ("actuals", PrettyRep.List (List.map (fn x716 => 
                                                                                                   cvtEXPR x716
                                                                                            ) ls717))]))
     | cvtEXPR (ApplyTypeExpr{expr=x728, actuals=ls730}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x728), ("actuals", PrettyRep.List (List.map (fn x729 => 
                                                                                                   cvtTY x729
                                                                                            ) ls730))]))
     | cvtEXPR (LetExpr{defs=x741, body=x742, head=opt744}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x741), ("body", cvtEXPR x742), 
          ("head", 
       (case opt744 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x743 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x743))
       ))]))
     | cvtEXPR (NewExpr{obj=x757, actuals=ls759}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x757), ("actuals", PrettyRep.List (List.map (fn x758 => 
                                                                                                  cvtEXPR x758
                                                                                           ) ls759))]))
     | cvtEXPR (ObjectRef{base=x770, ident=x771, loc=opt773}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x770), ("ident", cvtIDENT_EXPR x771), 
          ("loc", 
       (case opt773 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x772 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x772))
       ))]))
     | cvtEXPR (LexicalRef{ident=x786, loc=opt788}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x786), ("loc", 
       (case opt788 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x787 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x787))
       ))]))
     | cvtEXPR (SetExpr(x799, x800, x801)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x799, 
          cvtEXPR x800, cvtEXPR x801]))
     | cvtEXPR (ListExpr ls806) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x805 => 
                                                                                                    cvtEXPR x805
                                                                                             ) ls806)))
     | cvtEXPR (InitExpr(x812, x813, x814)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x812, 
          cvtHEAD x813, cvtINITS x814]))
     | cvtEXPR (GetTemp n818) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n818))
     | cvtEXPR (GetParam n821) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n821))
     | cvtEXPR (Comprehension(x824, ls826, opt831)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x824, PrettyRep.List (List.map (fn x825 => 
                                                                               cvtFOR_ENUM_HEAD x825
                                                                        ) ls826), 
          
       (case opt831 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x830 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x830))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n843) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n843))
     | cvtFIXTURE_NAME (PropName x846) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x846))
   and cvtIDENT_EXPR (Identifier{ident=x849, openNamespaces=ls855}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x849), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls851 => PrettyRep.List (List.map (fn x850 => 
                                                                                cvtNAMESPACE x850
                                                                         ) ls851)
                                   ) ls855))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x866, expr=x867}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x866), ("expr", cvtEXPR x867)]))
     | cvtIDENT_EXPR (AttributeIdentifier x875) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x875))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x878, openNamespaces=ls884}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x878), ("openNamespaces", PrettyRep.List (List.map (fn ls880 => 
                                                                            PrettyRep.List (List.map (fn x879 => 
                                                                                                            cvtNAMESPACE x879
                                                                                                     ) ls880)
                                                                     ) ls884))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x895, ident=s896}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x895), ("ident", PrettyRep.UniStr s896)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls905, x909)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x904 => cvtIDENT x904
                                                          ) ls905), cvtIDENT_EXPR x909]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r916) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r916))
     | cvtLITERAL (LiteralDecimal d919) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d919))
     | cvtLITERAL (LiteralInt i922) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i922))
     | cvtLITERAL (LiteralUInt u925) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u925))
     | cvtLITERAL (LiteralBoolean b928) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b928))
     | cvtLITERAL (LiteralString s931) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s931))
     | cvtLITERAL (LiteralArray{exprs=x934, ty=opt936}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x934), ("ty", 
       (case opt936 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x935 => PrettyRep.Ctor ("SOME", SOME (cvtTY x935))
       ))]))
     | cvtLITERAL (LiteralXML ls948) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x947 => 
                                                                                                           cvtEXPR x947
                                                                                                    ) ls948)))
     | cvtLITERAL (LiteralNamespace x954) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x954))
     | cvtLITERAL (LiteralObject{expr=ls958, ty=opt963}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x957 => 
                                                                        cvtFIELD x957
                                                                 ) ls958)), 
          ("ty", 
       (case opt963 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x962 => PrettyRep.Ctor ("SOME", SOME (cvtTY x962))
       ))]))
     | cvtLITERAL (LiteralFunction x974) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x974))
     | cvtLITERAL (LiteralRegExp{str=s977}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s977)]))
   and cvtBLOCK (Block x983) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x983))
   and cvtFIXTURE (NamespaceFixture x986) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x986))
     | cvtFIXTURE (ClassFixture x989) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x989))
     | cvtFIXTURE (InterfaceFixture x992) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x992))
     | cvtFIXTURE (TypeVarFixture x995) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x995))
     | cvtFIXTURE (TypeFixture x998) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x998))
     | cvtFIXTURE (MethodFixture{func=x1001, ty=x1002, readOnly=b1003, override=b1004, 
          final=b1005}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1001), ("ty", cvtTY x1002), ("readOnly", PrettyRep.Bool b1003), 
          ("override", PrettyRep.Bool b1004), ("final", PrettyRep.Bool b1005)]))
     | cvtFIXTURE (ValFixture{ty=x1019, readOnly=b1020}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1019), ("readOnly", PrettyRep.Bool b1020)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1028, getter=opt1030, setter=opt1035}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1028), ("getter", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1029))
       )), ("setter", 
       (case opt1035 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1034 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1034))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1048, baseTypeArgs=ls1050}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1048), ("baseTypeArgs", PrettyRep.List (List.map (fn x1049 => 
                                                                           cvtTY x1049
                                                                    ) ls1050))]))
   and cvtHEAD (Head(x1061, x1062)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1061, 
          cvtINITS x1062]))
   and cvtBINDINGS (ls1067, ls1072) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1066 => 
                                                                                       cvtBINDING x1066
                                                                                ) ls1067), 
          PrettyRep.List (List.map (fn x1071 => cvtINIT_STEP x1071
                                   ) ls1072)]
   and cvtRIB ls1080 = PrettyRep.List (List.map (fn (x1077, x1078) => PrettyRep.Tuple [cvtFIXTURE_NAME x1077, 
                                                       cvtFIXTURE x1078]
                                                ) ls1080)
   and cvtRIBS ls1091 = PrettyRep.List (List.map (fn ls1087 => PrettyRep.List (List.map (fn (x1084, 
                                                                                               x1085) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1084, 
                                                                                               cvtFIXTURE x1085]
                                                                                        ) ls1087)
                                                 ) ls1091)
   and cvtINITS ls1098 = PrettyRep.List (List.map (fn (x1095, x1096) => PrettyRep.Tuple [cvtFIXTURE_NAME x1095, 
                                                         cvtEXPR x1096]
                                                  ) ls1098)
   and cvtINSTANCE_TYPE {name=x1102, typeParams=ls1104, typeArgs=ls1109, nonnullable=b1113, 
          superTypes=ls1115, ty=x1119, dynamic=b1120} = PrettyRep.Rec [("name", 
          cvtNAME x1102), ("typeParams", PrettyRep.List (List.map (fn x1103 => 
                                                                         cvtIDENT x1103
                                                                  ) ls1104)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1108 => cvtTYPE_EXPR x1108
                                                ) ls1109)), ("nonnullable", 
          PrettyRep.Bool b1113), ("superTypes", PrettyRep.List (List.map (fn x1114 => 
                                                                                cvtTYPE_EXPR x1114
                                                                         ) ls1115)), 
          ("ty", cvtTYPE_EXPR x1119), ("dynamic", PrettyRep.Bool b1120)]
   and cvtFIELD {kind=x1136, name=x1137, init=x1138} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1136), ("name", cvtIDENT_EXPR x1137), ("init", cvtEXPR x1138)]
   and cvtFIELD_TYPE {name=x1146, ty=x1147} = PrettyRep.Rec [("name", cvtIDENT x1146), 
          ("ty", cvtTYPE_EXPR x1147)]
   and cvtFUNC_TYPE {params=ls1154, result=x1158, thisType=opt1160, hasRest=b1164, 
          minArgs=n1165} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1153 => 
                                                                                     cvtTYPE_EXPR x1153
                                                                              ) ls1154)), 
          ("result", cvtTYPE_EXPR x1158), ("thisType", 
       (case opt1160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1159 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1159))
       )), ("hasRest", PrettyRep.Bool b1164), ("minArgs", PrettyRep.Int n1165)]
   and cvtFUNC_DEFN {kind=x1177, ns=opt1179, final=b1183, override=b1184, prototype=b1185, 
          static=b1186, func=x1187} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1177), 
          ("ns", 
       (case opt1179 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1178 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1178))
       )), ("final", PrettyRep.Bool b1183), ("override", PrettyRep.Bool b1184), 
          ("prototype", PrettyRep.Bool b1185), ("static", PrettyRep.Bool b1186), 
          ("func", cvtFUNC x1187)]
   and cvtCTOR_DEFN x1203 = cvtCTOR x1203
   and cvtVAR_DEFN {kind=x1204, ns=opt1206, static=b1210, prototype=b1211, 
          bindings=(ls1213, ls1218)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1204), 
          ("ns", 
       (case opt1206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1205 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1205))
       )), ("static", PrettyRep.Bool b1210), ("prototype", PrettyRep.Bool b1211), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1212 => 
                                                                        cvtBINDING x1212
                                                                 ) ls1213), 
          PrettyRep.List (List.map (fn x1217 => cvtINIT_STEP x1217
                                   ) ls1218)])]
   and cvtNAMESPACE_DEFN {ident=x1234, ns=opt1236, init=opt1241} = PrettyRep.Rec [("ident", 
          cvtIDENT x1234), ("ns", 
       (case opt1236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1235 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1235))
       )), ("init", 
       (case opt1241 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1240))
       ))]
   and cvtCLASS_DEFN {ns=opt1253, ident=x1257, nonnullable=b1258, dynamic=b1259, 
          final=b1260, params=ls1262, extends=opt1267, implements=ls1272, classDefns=ls1277, 
          instanceDefns=ls1282, instanceStmts=ls1287, ctorDefn=opt1292} = PrettyRep.Rec [("ns", 
          
       (case opt1253 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1252 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1252))
       )), ("ident", cvtIDENT x1257), ("nonnullable", PrettyRep.Bool b1258), 
          ("dynamic", PrettyRep.Bool b1259), ("final", PrettyRep.Bool b1260), 
          ("params", PrettyRep.List (List.map (fn x1261 => cvtIDENT x1261
                                              ) ls1262)), ("extends", 
       (case opt1267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1266 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1266))
       )), ("implements", PrettyRep.List (List.map (fn x1271 => cvtTYPE_EXPR x1271
                                                   ) ls1272)), ("classDefns", 
          PrettyRep.List (List.map (fn x1276 => cvtDEFN x1276
                                   ) ls1277)), ("instanceDefns", PrettyRep.List (List.map (fn x1281 => 
                                                                                                 cvtDEFN x1281
                                                                                          ) ls1282)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1286 => cvtSTMT x1286
                                                     ) ls1287)), ("ctorDefn", 
          
       (case opt1292 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1291 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1291))
       ))]
   and cvtINTERFACE_DEFN {ident=x1321, ns=opt1323, nonnullable=b1327, params=ls1329, 
          extends=ls1334, instanceDefns=ls1339} = PrettyRep.Rec [("ident", 
          cvtIDENT x1321), ("ns", 
       (case opt1323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1322 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1322))
       )), ("nonnullable", PrettyRep.Bool b1327), ("params", PrettyRep.List (List.map (fn x1328 => 
                                                                                             cvtIDENT x1328
                                                                                      ) ls1329)), 
          ("extends", PrettyRep.List (List.map (fn x1333 => cvtTYPE_EXPR x1333
                                               ) ls1334)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1338 => cvtDEFN x1338
                                   ) ls1339))]
   and cvtTYPE_DEFN {ident=x1356, ns=opt1358, init=x1362} = PrettyRep.Rec [("ident", 
          cvtIDENT x1356), ("ns", 
       (case opt1358 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1357 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1357))
       )), ("init", cvtTYPE_EXPR x1362)]
   and cvtCLASS_BLOCK {ns=opt1371, ident=x1375, name=opt1377, block=x1381} = 
          PrettyRep.Rec [("ns", 
       (case opt1371 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1370 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1370))
       )), ("ident", cvtIDENT x1375), ("name", 
       (case opt1377 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1376))
       )), ("block", cvtBLOCK x1381)]
   and cvtFOR_ENUM_HEAD {isEach=b1391, bindings=(ls1393, ls1398), expr=x1403} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1391), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1392 => 
                                                                                                                         cvtBINDING x1392
                                                                                                                  ) ls1393), 
          PrettyRep.List (List.map (fn x1397 => cvtINIT_STEP x1397
                                   ) ls1398)]), ("expr", cvtEXPR x1403)]
   and cvtFOR_ENUM_STMT {isEach=b1411, defn=opt1442, obj=x1446, rib=opt1454, 
          next=x1458, labels=ls1460, body=x1464} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1411), ("defn", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1412, ns=opt1414, static=b1418, prototype=b1419, bindings=(ls1421, 
            ls1426)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1412), ("ns", 
         (case opt1414 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1413 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1413))
         )), ("static", PrettyRep.Bool b1418), ("prototype", PrettyRep.Bool b1419), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1420 => 
                                                                          cvtBINDING x1420
                                                                   ) ls1421), 
            PrettyRep.List (List.map (fn x1425 => cvtINIT_STEP x1425
                                     ) ls1426)])]))
       )), ("obj", cvtEXPR x1446), ("rib", 
       (case opt1454 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1450 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1447, 
                                                                                      x1448) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1447, 
                                                                                      cvtFIXTURE x1448]
                                                                               ) ls1450)))
       )), ("next", cvtSTMT x1458), ("labels", PrettyRep.List (List.map (fn x1459 => 
                                                                               cvtIDENT x1459
                                                                        ) ls1460)), 
          ("body", cvtSTMT x1464)]
   and cvtFOR_STMT {rib=opt1487, defn=opt1521, init=ls1526, cond=x1530, update=x1531, 
          labels=ls1533, body=x1537} = PrettyRep.Rec [("rib", 
       (case opt1487 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1483 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1480, 
                                                                                      x1481) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1480, 
                                                                                      cvtFIXTURE x1481]
                                                                               ) ls1483)))
       )), ("defn", 
       (case opt1521 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1491, ns=opt1493, static=b1497, prototype=b1498, bindings=(ls1500, 
            ls1505)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1491), ("ns", 
         (case opt1493 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1492 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1492))
         )), ("static", PrettyRep.Bool b1497), ("prototype", PrettyRep.Bool b1498), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1499 => 
                                                                          cvtBINDING x1499
                                                                   ) ls1500), 
            PrettyRep.List (List.map (fn x1504 => cvtINIT_STEP x1504
                                     ) ls1505)])]))
       )), ("init", PrettyRep.List (List.map (fn x1525 => cvtSTMT x1525
                                             ) ls1526)), ("cond", cvtEXPR x1530), 
          ("update", cvtEXPR x1531), ("labels", PrettyRep.List (List.map (fn x1532 => 
                                                                                cvtIDENT x1532
                                                                         ) ls1533)), 
          ("body", cvtSTMT x1537)]
   and cvtWHILE_STMT {cond=x1553, rib=opt1561, body=x1565, labels=ls1567} = 
          PrettyRep.Rec [("cond", cvtEXPR x1553), ("rib", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1557 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1554, 
                                                                                      x1555) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1554, 
                                                                                      cvtFIXTURE x1555]
                                                                               ) ls1557)))
       )), ("body", cvtSTMT x1565), ("labels", PrettyRep.List (List.map (fn x1566 => 
                                                                               cvtIDENT x1566
                                                                        ) ls1567))]
   and cvtDIRECTIVES {pragmas=ls1581, defns=ls1586, head=opt1591, body=ls1596, 
          loc=opt1601} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1580 => 
                                                                                    cvtPRAGMA x1580
                                                                             ) ls1581)), 
          ("defns", PrettyRep.List (List.map (fn x1585 => cvtDEFN x1585
                                             ) ls1586)), ("head", 
       (case opt1591 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1590 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1590))
       )), ("body", PrettyRep.List (List.map (fn x1595 => cvtSTMT x1595
                                             ) ls1596)), ("loc", 
       (case opt1601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1600 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1600))
       ))]
   and cvtCASE {label=opt1617, inits=opt1628, body=x1632} = PrettyRep.Rec [("label", 
          
       (case opt1617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1616 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1616))
       )), ("inits", 
       (case opt1628 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1624 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1621, 
                                                                                      x1622) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1621, 
                                                                                      cvtEXPR x1622]
                                                                               ) ls1624)))
       )), ("body", cvtBLOCK x1632)]
   and cvtCATCH_CLAUSE {bindings=(ls1641, ls1646), ty=x1651, rib=opt1659, inits=opt1670, 
          block=x1674} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1640 => 
                                                                                                      cvtBINDING x1640
                                                                                               ) ls1641), 
          PrettyRep.List (List.map (fn x1645 => cvtINIT_STEP x1645
                                   ) ls1646)]), ("ty", cvtTY x1651), ("rib", 
          
       (case opt1659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1655 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1652, 
                                                                                      x1653) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1652, 
                                                                                      cvtFIXTURE x1653]
                                                                               ) ls1655)))
       )), ("inits", 
       (case opt1670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1666 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1663, 
                                                                                      x1664) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1663, 
                                                                                      cvtEXPR x1664]
                                                                               ) ls1666)))
       )), ("block", cvtBLOCK x1674)]
   and cvtFUNC_NAME {kind=x1686, ident=x1687} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1686), 
          ("ident", cvtIDENT x1687)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1693, getter=opt1695, setter=opt1700} = 
          PrettyRep.Rec [("ty", cvtTY x1693), ("getter", 
       (case opt1695 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1694 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1694))
       )), ("setter", 
       (case opt1700 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1699 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1699))
       ))]
   and cvtFRAGMENT (Unit{name=opt1712, fragments=ls1717}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1712 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1711 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1711))
       )), ("fragments", PrettyRep.List (List.map (fn x1716 => cvtFRAGMENT x1716
                                                  ) ls1717))]))
     | cvtFRAGMENT (Package{name=ls1729, fragments=ls1734}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1728 => 
                                                                        cvtIDENT x1728
                                                                 ) ls1729)), 
          ("fragments", PrettyRep.List (List.map (fn x1733 => cvtFRAGMENT x1733
                                                 ) ls1734))]))
     | cvtFRAGMENT (Anon x1745) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1745))
end

