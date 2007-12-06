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
     | cvtTYPE_EXPR (AppType{base=x459, args=ls461}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x459), ("args", PrettyRep.List (List.map (fn x460 => 
                                                                                                     cvtTYPE_EXPR x460
                                                                                              ) ls461))]))
     | cvtTYPE_EXPR (LamType{params=ls473, body=x477}) = PrettyRep.Ctor ("LamType", 
          SOME (PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x472 => 
                                                                          cvtIDENT x472
                                                                   ) ls473)), 
          ("body", cvtTYPE_EXPR x477)]))
     | cvtTYPE_EXPR (NullableType{expr=x485, nullable=b486}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x485), ("nullable", PrettyRep.Bool b486)]))
     | cvtTYPE_EXPR (InstanceType x494) = PrettyRep.Ctor ("InstanceType", SOME (cvtINSTANCE_TYPE x494))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x498) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x498))
     | cvtSTMT (InitStmt{kind=x501, ns=opt503, prototype=b507, static=b508, 
          temps=x509, inits=ls511}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x501), ("ns", 
       (case opt503 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x502 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x502))
       )), ("prototype", PrettyRep.Bool b507), ("static", PrettyRep.Bool b508), 
          ("temps", cvtBINDINGS x509), ("inits", PrettyRep.List (List.map (fn x510 => 
                                                                                 cvtINIT_STEP x510
                                                                          ) ls511))]))
     | cvtSTMT (ClassBlock x530) = PrettyRep.Ctor ("ClassBlock", SOME (cvtCLASS_BLOCK x530))
     | cvtSTMT (ForInStmt x533) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x533))
     | cvtSTMT (ThrowStmt x536) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x536))
     | cvtSTMT (ReturnStmt x539) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x539))
     | cvtSTMT (BreakStmt opt543) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt543 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x542 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x542))
       ))
     | cvtSTMT (ContinueStmt opt550) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt550 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x549 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x549))
       ))
     | cvtSTMT (BlockStmt x556) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x556))
     | cvtSTMT (LabeledStmt(x559, x560)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x559, 
          cvtSTMT x560]))
     | cvtSTMT (LetStmt x564) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x564))
     | cvtSTMT (WhileStmt x567) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x567))
     | cvtSTMT (DoWhileStmt x570) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x570))
     | cvtSTMT (ForStmt x573) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x573))
     | cvtSTMT (IfStmt{cnd=x576, thn=x577, els=x578}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x576), ("thn", cvtSTMT x577), 
          ("els", cvtSTMT x578)]))
     | cvtSTMT (WithStmt{obj=x588, ty=x589, body=x590}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x588), ("ty", cvtTY x589), ("body", 
          cvtSTMT x590)]))
     | cvtSTMT (TryStmt{block=x600, catches=ls602, finally=opt607}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x600), ("catches", PrettyRep.List (List.map (fn x601 => 
                                                                                                     cvtCATCH_CLAUSE x601
                                                                                              ) ls602)), 
          ("finally", 
       (case opt607 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x606 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x606))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x620, labels=ls622, cases=ls627}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x620), ("labels", PrettyRep.List (List.map (fn x621 => 
                                                                                                  cvtIDENT x621
                                                                                           ) ls622)), 
          ("cases", PrettyRep.List (List.map (fn x626 => cvtCASE x626
                                             ) ls627))]))
     | cvtSTMT (SwitchTypeStmt{cond=x640, ty=x641, cases=ls643}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x640), ("ty", cvtTY x641), 
          ("cases", PrettyRep.List (List.map (fn x642 => cvtCATCH_CLAUSE x642
                                             ) ls643))]))
     | cvtSTMT (DXNStmt{expr=x656}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x656)]))
   and cvtEXPR (TernaryExpr(x662, x663, x664)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x662, cvtEXPR x663, cvtEXPR x664]))
     | cvtEXPR (BinaryExpr(x668, x669, x670)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x668, cvtEXPR x669, cvtEXPR x670]))
     | cvtEXPR (BinaryTypeExpr(x674, x675, x676)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x674, cvtEXPR x675, cvtTY x676]))
     | cvtEXPR (ExpectedTypeExpr(x680, x681)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x680, cvtEXPR x681]))
     | cvtEXPR (UnaryExpr(x685, x686)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x685, 
          cvtEXPR x686]))
     | cvtEXPR (TypeExpr x690) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTY x690))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt695) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt695 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x694 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x694))
       ))
     | cvtEXPR (SuperExpr opt702) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt702 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x701 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x701))
       ))
     | cvtEXPR (LiteralExpr x708) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x708))
     | cvtEXPR (CallExpr{func=x711, actuals=ls713}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x711), ("actuals", PrettyRep.List (List.map (fn x712 => 
                                                                                                   cvtEXPR x712
                                                                                            ) ls713))]))
     | cvtEXPR (ApplyTypeExpr{expr=x724, actuals=ls726}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x724), ("actuals", PrettyRep.List (List.map (fn x725 => 
                                                                                                   cvtTY x725
                                                                                            ) ls726))]))
     | cvtEXPR (LetExpr{defs=x737, body=x738, head=opt740}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x737), ("body", cvtEXPR x738), 
          ("head", 
       (case opt740 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x739 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x739))
       ))]))
     | cvtEXPR (NewExpr{obj=x753, actuals=ls755}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x753), ("actuals", PrettyRep.List (List.map (fn x754 => 
                                                                                                  cvtEXPR x754
                                                                                           ) ls755))]))
     | cvtEXPR (ObjectRef{base=x766, ident=x767, loc=opt769}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x766), ("ident", cvtIDENT_EXPR x767), 
          ("loc", 
       (case opt769 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x768 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x768))
       ))]))
     | cvtEXPR (LexicalRef{ident=x782, loc=opt784}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x782), ("loc", 
       (case opt784 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x783 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x783))
       ))]))
     | cvtEXPR (SetExpr(x795, x796, x797)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x795, 
          cvtEXPR x796, cvtEXPR x797]))
     | cvtEXPR (ListExpr ls802) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x801 => 
                                                                                                    cvtEXPR x801
                                                                                             ) ls802)))
     | cvtEXPR (InitExpr(x808, x809, x810)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x808, 
          cvtHEAD x809, cvtINITS x810]))
     | cvtEXPR (SliceExpr(x814, x815, x816)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x814, cvtEXPR x815, cvtEXPR x816]))
     | cvtEXPR (GetTemp n820) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n820))
     | cvtEXPR (GetParam n823) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n823))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n829) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n829))
     | cvtFIXTURE_NAME (PropName x832) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x832))
   and cvtIDENT_EXPR (Identifier{ident=x835, openNamespaces=ls841}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x835), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls837 => PrettyRep.List (List.map (fn x836 => 
                                                                                cvtNAMESPACE x836
                                                                         ) ls837)
                                   ) ls841))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x852, expr=x853}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x852), ("expr", cvtEXPR x853)]))
     | cvtIDENT_EXPR (AttributeIdentifier x861) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x861))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x864, openNamespaces=ls870}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x864), ("openNamespaces", PrettyRep.List (List.map (fn ls866 => 
                                                                            PrettyRep.List (List.map (fn x865 => 
                                                                                                            cvtNAMESPACE x865
                                                                                                     ) ls866)
                                                                     ) ls870))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x881, ident=s882}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x881), ("ident", PrettyRep.UniStr s882)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls891, x895)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x890 => cvtIDENT x890
                                                          ) ls891), cvtIDENT_EXPR x895]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r902) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r902))
     | cvtLITERAL (LiteralDecimal d905) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d905))
     | cvtLITERAL (LiteralInt i908) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i908))
     | cvtLITERAL (LiteralUInt u911) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u911))
     | cvtLITERAL (LiteralBoolean b914) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b914))
     | cvtLITERAL (LiteralString s917) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s917))
     | cvtLITERAL (LiteralArray{exprs=ls921, ty=opt926}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x920 => 
                                                                         cvtEXPR x920
                                                                  ) ls921)), 
          ("ty", 
       (case opt926 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x925 => PrettyRep.Ctor ("SOME", SOME (cvtTY x925))
       ))]))
     | cvtLITERAL (LiteralXML ls938) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x937 => 
                                                                                                           cvtEXPR x937
                                                                                                    ) ls938)))
     | cvtLITERAL (LiteralNamespace x944) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x944))
     | cvtLITERAL (LiteralObject{expr=ls948, ty=opt953}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x947 => 
                                                                        cvtFIELD x947
                                                                 ) ls948)), 
          ("ty", 
       (case opt953 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x952 => PrettyRep.Ctor ("SOME", SOME (cvtTY x952))
       ))]))
     | cvtLITERAL (LiteralFunction x964) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x964))
     | cvtLITERAL (LiteralRegExp{str=s967}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s967)]))
   and cvtBLOCK (Block x973) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x973))
   and cvtFIXTURE (NamespaceFixture x976) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x976))
     | cvtFIXTURE (ClassFixture x979) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x979))
     | cvtFIXTURE (InterfaceFixture x982) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x982))
     | cvtFIXTURE (TypeVarFixture x985) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x985))
     | cvtFIXTURE (TypeFixture x988) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x988))
     | cvtFIXTURE (MethodFixture{func=x991, ty=x992, readOnly=b993, override=b994, 
          final=b995}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x991), ("ty", cvtTY x992), ("readOnly", PrettyRep.Bool b993), 
          ("override", PrettyRep.Bool b994), ("final", PrettyRep.Bool b995)]))
     | cvtFIXTURE (ValFixture{ty=x1009, readOnly=b1010}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1009), ("readOnly", PrettyRep.Bool b1010)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1018, getter=opt1020, setter=opt1025}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1018), ("getter", 
       (case opt1020 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1019 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1019))
       )), ("setter", 
       (case opt1025 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1024 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1024))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1038, baseTypeArgs=ls1040}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1038), ("baseTypeArgs", PrettyRep.List (List.map (fn x1039 => 
                                                                           cvtTY x1039
                                                                    ) ls1040))]))
   and cvtHEAD (Head(x1051, x1052)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1051, 
          cvtINITS x1052]))
   and cvtBINDINGS (ls1057, ls1062) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1056 => 
                                                                                       cvtBINDING x1056
                                                                                ) ls1057), 
          PrettyRep.List (List.map (fn x1061 => cvtINIT_STEP x1061
                                   ) ls1062)]
   and cvtRIB ls1070 = PrettyRep.List (List.map (fn (x1067, x1068) => PrettyRep.Tuple [cvtFIXTURE_NAME x1067, 
                                                       cvtFIXTURE x1068]
                                                ) ls1070)
   and cvtRIBS ls1081 = PrettyRep.List (List.map (fn ls1077 => PrettyRep.List (List.map (fn (x1074, 
                                                                                               x1075) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1074, 
                                                                                               cvtFIXTURE x1075]
                                                                                        ) ls1077)
                                                 ) ls1081)
   and cvtINITS ls1088 = PrettyRep.List (List.map (fn (x1085, x1086) => PrettyRep.Tuple [cvtFIXTURE_NAME x1085, 
                                                         cvtEXPR x1086]
                                                  ) ls1088)
   and cvtINSTANCE_TYPE {name=x1092, typeParams=ls1094, typeArgs=ls1099, nonnullable=b1103, 
          superTypes=ls1105, ty=x1109, dynamic=b1110} = PrettyRep.Rec [("name", 
          cvtNAME x1092), ("typeParams", PrettyRep.List (List.map (fn x1093 => 
                                                                         cvtIDENT x1093
                                                                  ) ls1094)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1098 => cvtTYPE_EXPR x1098
                                                ) ls1099)), ("nonnullable", 
          PrettyRep.Bool b1103), ("superTypes", PrettyRep.List (List.map (fn x1104 => 
                                                                                cvtTYPE_EXPR x1104
                                                                         ) ls1105)), 
          ("ty", cvtTYPE_EXPR x1109), ("dynamic", PrettyRep.Bool b1110)]
   and cvtFIELD {kind=x1126, name=x1127, init=x1128} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1126), ("name", cvtIDENT_EXPR x1127), ("init", cvtEXPR x1128)]
   and cvtFIELD_TYPE {name=x1136, ty=x1137} = PrettyRep.Rec [("name", cvtIDENT x1136), 
          ("ty", cvtTYPE_EXPR x1137)]
   and cvtFUNC_TYPE {params=ls1144, result=x1148, thisType=opt1150, hasRest=b1154, 
          minArgs=n1155} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1143 => 
                                                                                     cvtTYPE_EXPR x1143
                                                                              ) ls1144)), 
          ("result", cvtTYPE_EXPR x1148), ("thisType", 
       (case opt1150 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1149 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1149))
       )), ("hasRest", PrettyRep.Bool b1154), ("minArgs", PrettyRep.Int n1155)]
   and cvtFUNC_DEFN {kind=x1167, ns=opt1169, final=b1173, override=b1174, prototype=b1175, 
          static=b1176, func=x1177} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1167), 
          ("ns", 
       (case opt1169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1168 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1168))
       )), ("final", PrettyRep.Bool b1173), ("override", PrettyRep.Bool b1174), 
          ("prototype", PrettyRep.Bool b1175), ("static", PrettyRep.Bool b1176), 
          ("func", cvtFUNC x1177)]
   and cvtCTOR_DEFN x1193 = cvtCTOR x1193
   and cvtVAR_DEFN {kind=x1194, ns=opt1196, static=b1200, prototype=b1201, 
          bindings=(ls1203, ls1208)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1194), 
          ("ns", 
       (case opt1196 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1195 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1195))
       )), ("static", PrettyRep.Bool b1200), ("prototype", PrettyRep.Bool b1201), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1202 => 
                                                                        cvtBINDING x1202
                                                                 ) ls1203), 
          PrettyRep.List (List.map (fn x1207 => cvtINIT_STEP x1207
                                   ) ls1208)])]
   and cvtNAMESPACE_DEFN {ident=x1224, ns=opt1226, init=opt1231} = PrettyRep.Rec [("ident", 
          cvtIDENT x1224), ("ns", 
       (case opt1226 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1225 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1225))
       )), ("init", 
       (case opt1231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1230 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1230))
       ))]
   and cvtCLASS_DEFN {ns=opt1243, ident=x1247, nonnullable=b1248, dynamic=b1249, 
          final=b1250, params=ls1252, extends=opt1257, implements=ls1262, classDefns=ls1267, 
          instanceDefns=ls1272, instanceStmts=ls1277, ctorDefn=opt1282} = PrettyRep.Rec [("ns", 
          
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       )), ("ident", cvtIDENT x1247), ("nonnullable", PrettyRep.Bool b1248), 
          ("dynamic", PrettyRep.Bool b1249), ("final", PrettyRep.Bool b1250), 
          ("params", PrettyRep.List (List.map (fn x1251 => cvtIDENT x1251
                                              ) ls1252)), ("extends", 
       (case opt1257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1256 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1256))
       )), ("implements", PrettyRep.List (List.map (fn x1261 => cvtTYPE_EXPR x1261
                                                   ) ls1262)), ("classDefns", 
          PrettyRep.List (List.map (fn x1266 => cvtDEFN x1266
                                   ) ls1267)), ("instanceDefns", PrettyRep.List (List.map (fn x1271 => 
                                                                                                 cvtDEFN x1271
                                                                                          ) ls1272)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1276 => cvtSTMT x1276
                                                     ) ls1277)), ("ctorDefn", 
          
       (case opt1282 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1281 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1281))
       ))]
   and cvtINTERFACE_DEFN {ident=x1311, ns=opt1313, nonnullable=b1317, params=ls1319, 
          extends=ls1324, instanceDefns=ls1329} = PrettyRep.Rec [("ident", 
          cvtIDENT x1311), ("ns", 
       (case opt1313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1312 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1312))
       )), ("nonnullable", PrettyRep.Bool b1317), ("params", PrettyRep.List (List.map (fn x1318 => 
                                                                                             cvtIDENT x1318
                                                                                      ) ls1319)), 
          ("extends", PrettyRep.List (List.map (fn x1323 => cvtTYPE_EXPR x1323
                                               ) ls1324)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1328 => cvtDEFN x1328
                                   ) ls1329))]
   and cvtTYPE_DEFN {ident=x1346, ns=opt1348, init=x1352} = PrettyRep.Rec [("ident", 
          cvtIDENT x1346), ("ns", 
       (case opt1348 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1347 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1347))
       )), ("init", cvtTYPE_EXPR x1352)]
   and cvtCLASS_BLOCK {ns=opt1361, ident=x1365, name=opt1367, block=x1371} = 
          PrettyRep.Rec [("ns", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1360))
       )), ("ident", cvtIDENT x1365), ("name", 
       (case opt1367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1366 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1366))
       )), ("block", cvtBLOCK x1371)]
   and cvtFOR_ENUM_STMT {isEach=b1381, defn=opt1412, obj=x1416, rib=opt1424, 
          next=x1428, labels=ls1430, body=x1434} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1381), ("defn", 
       (case opt1412 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1382, ns=opt1384, static=b1388, prototype=b1389, bindings=(ls1391, 
            ls1396)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1382), ("ns", 
         (case opt1384 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1383 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1383))
         )), ("static", PrettyRep.Bool b1388), ("prototype", PrettyRep.Bool b1389), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1390 => 
                                                                          cvtBINDING x1390
                                                                   ) ls1391), 
            PrettyRep.List (List.map (fn x1395 => cvtINIT_STEP x1395
                                     ) ls1396)])]))
       )), ("obj", cvtEXPR x1416), ("rib", 
       (case opt1424 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1420 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1417, 
                                                                                      x1418) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1417, 
                                                                                      cvtFIXTURE x1418]
                                                                               ) ls1420)))
       )), ("next", cvtSTMT x1428), ("labels", PrettyRep.List (List.map (fn x1429 => 
                                                                               cvtIDENT x1429
                                                                        ) ls1430)), 
          ("body", cvtSTMT x1434)]
   and cvtFOR_STMT {rib=opt1457, defn=opt1491, init=ls1496, cond=x1500, update=x1501, 
          labels=ls1503, body=x1507} = PrettyRep.Rec [("rib", 
       (case opt1457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1453 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1450, 
                                                                                      x1451) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1450, 
                                                                                      cvtFIXTURE x1451]
                                                                               ) ls1453)))
       )), ("defn", 
       (case opt1491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1461, ns=opt1463, static=b1467, prototype=b1468, bindings=(ls1470, 
            ls1475)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1461), ("ns", 
         (case opt1463 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1462 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1462))
         )), ("static", PrettyRep.Bool b1467), ("prototype", PrettyRep.Bool b1468), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1469 => 
                                                                          cvtBINDING x1469
                                                                   ) ls1470), 
            PrettyRep.List (List.map (fn x1474 => cvtINIT_STEP x1474
                                     ) ls1475)])]))
       )), ("init", PrettyRep.List (List.map (fn x1495 => cvtSTMT x1495
                                             ) ls1496)), ("cond", cvtEXPR x1500), 
          ("update", cvtEXPR x1501), ("labels", PrettyRep.List (List.map (fn x1502 => 
                                                                                cvtIDENT x1502
                                                                         ) ls1503)), 
          ("body", cvtSTMT x1507)]
   and cvtWHILE_STMT {cond=x1523, rib=opt1531, body=x1535, labels=ls1537} = 
          PrettyRep.Rec [("cond", cvtEXPR x1523), ("rib", 
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1527 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1524, 
                                                                                      x1525) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1524, 
                                                                                      cvtFIXTURE x1525]
                                                                               ) ls1527)))
       )), ("body", cvtSTMT x1535), ("labels", PrettyRep.List (List.map (fn x1536 => 
                                                                               cvtIDENT x1536
                                                                        ) ls1537))]
   and cvtDIRECTIVES {pragmas=ls1551, defns=ls1556, head=opt1561, body=ls1566, 
          loc=opt1571} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1550 => 
                                                                                    cvtPRAGMA x1550
                                                                             ) ls1551)), 
          ("defns", PrettyRep.List (List.map (fn x1555 => cvtDEFN x1555
                                             ) ls1556)), ("head", 
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1560))
       )), ("body", PrettyRep.List (List.map (fn x1565 => cvtSTMT x1565
                                             ) ls1566)), ("loc", 
       (case opt1571 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1570 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1570))
       ))]
   and cvtCASE {label=opt1587, inits=opt1598, body=x1602} = PrettyRep.Rec [("label", 
          
       (case opt1587 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1586))
       )), ("inits", 
       (case opt1598 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1594 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1591, 
                                                                                      x1592) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1591, 
                                                                                      cvtEXPR x1592]
                                                                               ) ls1594)))
       )), ("body", cvtBLOCK x1602)]
   and cvtCATCH_CLAUSE {bindings=(ls1611, ls1616), ty=x1621, rib=opt1629, inits=opt1640, 
          block=x1644} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1610 => 
                                                                                                      cvtBINDING x1610
                                                                                               ) ls1611), 
          PrettyRep.List (List.map (fn x1615 => cvtINIT_STEP x1615
                                   ) ls1616)]), ("ty", cvtTY x1621), ("rib", 
          
       (case opt1629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1625 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1622, 
                                                                                      x1623) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1622, 
                                                                                      cvtFIXTURE x1623]
                                                                               ) ls1625)))
       )), ("inits", 
       (case opt1640 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1636 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1633, 
                                                                                      x1634) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1633, 
                                                                                      cvtEXPR x1634]
                                                                               ) ls1636)))
       )), ("block", cvtBLOCK x1644)]
   and cvtFUNC_NAME {kind=x1656, ident=x1657} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1656), 
          ("ident", cvtIDENT x1657)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1663, getter=opt1665, setter=opt1670} = 
          PrettyRep.Rec [("ty", cvtTY x1663), ("getter", 
       (case opt1665 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1664 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1664))
       )), ("setter", 
       (case opt1670 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1669 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1669))
       ))]
   and cvtFRAGMENT (Unit{name=opt1682, fragments=ls1687}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1682 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1681 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1681))
       )), ("fragments", PrettyRep.List (List.map (fn x1686 => cvtFRAGMENT x1686
                                                  ) ls1687))]))
     | cvtFRAGMENT (Package{name=ls1699, fragments=ls1704}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1698 => 
                                                                        cvtIDENT x1698
                                                                 ) ls1699)), 
          ("fragments", PrettyRep.List (List.map (fn x1703 => cvtFRAGMENT x1703
                                                 ) ls1704))]))
     | cvtFRAGMENT (Anon x1715) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1715))
end

