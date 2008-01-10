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
     | cvtEXPR (ThisExpr opt694) = PrettyRep.Ctor ("ThisExpr", SOME 
       (case opt694 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x693 => PrettyRep.Ctor ("SOME", SOME (cvtTHIS_KIND x693))
       ))
     | cvtEXPR (YieldExpr opt701) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt701 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x700 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x700))
       ))
     | cvtEXPR (SuperExpr opt708) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt708 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x707 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x707))
       ))
     | cvtEXPR (LiteralExpr x714) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x714))
     | cvtEXPR (CallExpr{func=x717, actuals=ls719}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x717), ("actuals", PrettyRep.List (List.map (fn x718 => 
                                                                                                   cvtEXPR x718
                                                                                            ) ls719))]))
     | cvtEXPR (ApplyTypeExpr{expr=x730, actuals=ls732}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x730), ("actuals", PrettyRep.List (List.map (fn x731 => 
                                                                                                   cvtTY x731
                                                                                            ) ls732))]))
     | cvtEXPR (LetExpr{defs=x743, body=x744, head=opt746}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x743), ("body", cvtEXPR x744), 
          ("head", 
       (case opt746 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x745 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x745))
       ))]))
     | cvtEXPR (NewExpr{obj=x759, actuals=ls761}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x759), ("actuals", PrettyRep.List (List.map (fn x760 => 
                                                                                                  cvtEXPR x760
                                                                                           ) ls761))]))
     | cvtEXPR (ObjectRef{base=x772, ident=x773, loc=opt775}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x772), ("ident", cvtIDENT_EXPR x773), 
          ("loc", 
       (case opt775 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x774 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x774))
       ))]))
     | cvtEXPR (LexicalRef{ident=x788, loc=opt790}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x788), ("loc", 
       (case opt790 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x789 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x789))
       ))]))
     | cvtEXPR (SetExpr(x801, x802, x803)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x801, 
          cvtEXPR x802, cvtEXPR x803]))
     | cvtEXPR (ListExpr ls808) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x807 => 
                                                                                                    cvtEXPR x807
                                                                                             ) ls808)))
     | cvtEXPR (InitExpr(x814, x815, x816)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x814, 
          cvtHEAD x815, cvtINITS x816]))
     | cvtEXPR (GetTemp n820) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n820))
     | cvtEXPR (GetParam n823) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n823))
     | cvtEXPR (Comprehension(x826, ls828, opt833)) = PrettyRep.Ctor ("Comprehension", 
          SOME (PrettyRep.Tuple [cvtEXPR x826, PrettyRep.List (List.map (fn x827 => 
                                                                               cvtFOR_ENUM_HEAD x827
                                                                        ) ls828), 
          
       (case opt833 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x832 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x832))
       )]))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtTHIS_KIND (FunctionThis) = PrettyRep.Ctor ("FunctionThis", NONE)
     | cvtTHIS_KIND (GeneratorThis) = PrettyRep.Ctor ("GeneratorThis", NONE)
   and cvtFIXTURE_NAME (TempName n845) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n845))
     | cvtFIXTURE_NAME (PropName x848) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x848))
   and cvtIDENT_EXPR (Identifier{ident=x851, openNamespaces=ls857}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x851), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls853 => PrettyRep.List (List.map (fn x852 => 
                                                                                cvtNAMESPACE x852
                                                                         ) ls853)
                                   ) ls857))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x868, expr=x869}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x868), ("expr", cvtEXPR x869)]))
     | cvtIDENT_EXPR (AttributeIdentifier x877) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x877))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x880, openNamespaces=ls886}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x880), ("openNamespaces", PrettyRep.List (List.map (fn ls882 => 
                                                                            PrettyRep.List (List.map (fn x881 => 
                                                                                                            cvtNAMESPACE x881
                                                                                                     ) ls882)
                                                                     ) ls886))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x897, ident=s898}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x897), ("ident", PrettyRep.UniStr s898)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls907, x911)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x906 => cvtIDENT x906
                                                          ) ls907), cvtIDENT_EXPR x911]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r918) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r918))
     | cvtLITERAL (LiteralDecimal d921) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d921))
     | cvtLITERAL (LiteralInt i924) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i924))
     | cvtLITERAL (LiteralUInt u927) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u927))
     | cvtLITERAL (LiteralBoolean b930) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b930))
     | cvtLITERAL (LiteralString s933) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s933))
     | cvtLITERAL (LiteralArray{exprs=x936, ty=opt938}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", cvtEXPR x936), ("ty", 
       (case opt938 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x937 => PrettyRep.Ctor ("SOME", SOME (cvtTY x937))
       ))]))
     | cvtLITERAL (LiteralXML ls950) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x949 => 
                                                                                                           cvtEXPR x949
                                                                                                    ) ls950)))
     | cvtLITERAL (LiteralNamespace x956) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x956))
     | cvtLITERAL (LiteralObject{expr=ls960, ty=opt965}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x959 => 
                                                                        cvtFIELD x959
                                                                 ) ls960)), 
          ("ty", 
       (case opt965 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x964 => PrettyRep.Ctor ("SOME", SOME (cvtTY x964))
       ))]))
     | cvtLITERAL (LiteralFunction x976) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x976))
     | cvtLITERAL (LiteralRegExp{str=s979}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s979)]))
   and cvtBLOCK (Block x985) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x985))
   and cvtFIXTURE (NamespaceFixture x988) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x988))
     | cvtFIXTURE (ClassFixture x991) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x991))
     | cvtFIXTURE (InterfaceFixture x994) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x994))
     | cvtFIXTURE (TypeVarFixture x997) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x997))
     | cvtFIXTURE (TypeFixture x1000) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x1000))
     | cvtFIXTURE (MethodFixture{func=x1003, ty=x1004, readOnly=b1005, override=b1006, 
          final=b1007}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1003), ("ty", cvtTY x1004), ("readOnly", PrettyRep.Bool b1005), 
          ("override", PrettyRep.Bool b1006), ("final", PrettyRep.Bool b1007)]))
     | cvtFIXTURE (ValFixture{ty=x1021, readOnly=b1022}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1021), ("readOnly", PrettyRep.Bool b1022)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1030, getter=opt1032, setter=opt1037}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1030), ("getter", 
       (case opt1032 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1031 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1031))
       )), ("setter", 
       (case opt1037 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1036 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1036))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1050, baseTypeArgs=ls1052}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1050), ("baseTypeArgs", PrettyRep.List (List.map (fn x1051 => 
                                                                           cvtTY x1051
                                                                    ) ls1052))]))
   and cvtHEAD (Head(x1063, x1064)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1063, 
          cvtINITS x1064]))
   and cvtBINDINGS (ls1069, ls1074) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1068 => 
                                                                                       cvtBINDING x1068
                                                                                ) ls1069), 
          PrettyRep.List (List.map (fn x1073 => cvtINIT_STEP x1073
                                   ) ls1074)]
   and cvtRIB ls1082 = PrettyRep.List (List.map (fn (x1079, x1080) => PrettyRep.Tuple [cvtFIXTURE_NAME x1079, 
                                                       cvtFIXTURE x1080]
                                                ) ls1082)
   and cvtRIBS ls1093 = PrettyRep.List (List.map (fn ls1089 => PrettyRep.List (List.map (fn (x1086, 
                                                                                               x1087) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1086, 
                                                                                               cvtFIXTURE x1087]
                                                                                        ) ls1089)
                                                 ) ls1093)
   and cvtINITS ls1100 = PrettyRep.List (List.map (fn (x1097, x1098) => PrettyRep.Tuple [cvtFIXTURE_NAME x1097, 
                                                         cvtEXPR x1098]
                                                  ) ls1100)
   and cvtINSTANCE_TYPE {name=x1104, typeParams=ls1106, typeArgs=ls1111, nonnullable=b1115, 
          superTypes=ls1117, ty=x1121, dynamic=b1122} = PrettyRep.Rec [("name", 
          cvtNAME x1104), ("typeParams", PrettyRep.List (List.map (fn x1105 => 
                                                                         cvtIDENT x1105
                                                                  ) ls1106)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1110 => cvtTYPE_EXPR x1110
                                                ) ls1111)), ("nonnullable", 
          PrettyRep.Bool b1115), ("superTypes", PrettyRep.List (List.map (fn x1116 => 
                                                                                cvtTYPE_EXPR x1116
                                                                         ) ls1117)), 
          ("ty", cvtTYPE_EXPR x1121), ("dynamic", PrettyRep.Bool b1122)]
   and cvtFIELD {kind=x1138, name=x1139, init=x1140} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1138), ("name", cvtIDENT_EXPR x1139), ("init", cvtEXPR x1140)]
   and cvtFIELD_TYPE {name=x1148, ty=x1149} = PrettyRep.Rec [("name", cvtIDENT x1148), 
          ("ty", cvtTYPE_EXPR x1149)]
   and cvtFUNC_TYPE {params=ls1156, result=x1160, thisType=opt1162, hasRest=b1166, 
          minArgs=n1167} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1155 => 
                                                                                     cvtTYPE_EXPR x1155
                                                                              ) ls1156)), 
          ("result", cvtTYPE_EXPR x1160), ("thisType", 
       (case opt1162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1161 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1161))
       )), ("hasRest", PrettyRep.Bool b1166), ("minArgs", PrettyRep.Int n1167)]
   and cvtFUNC_DEFN {kind=x1179, ns=opt1181, final=b1185, override=b1186, prototype=b1187, 
          static=b1188, func=x1189} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1179), 
          ("ns", 
       (case opt1181 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1180 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1180))
       )), ("final", PrettyRep.Bool b1185), ("override", PrettyRep.Bool b1186), 
          ("prototype", PrettyRep.Bool b1187), ("static", PrettyRep.Bool b1188), 
          ("func", cvtFUNC x1189)]
   and cvtCTOR_DEFN x1205 = cvtCTOR x1205
   and cvtVAR_DEFN {kind=x1206, ns=opt1208, static=b1212, prototype=b1213, 
          bindings=(ls1215, ls1220)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1206), 
          ("ns", 
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1207))
       )), ("static", PrettyRep.Bool b1212), ("prototype", PrettyRep.Bool b1213), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1214 => 
                                                                        cvtBINDING x1214
                                                                 ) ls1215), 
          PrettyRep.List (List.map (fn x1219 => cvtINIT_STEP x1219
                                   ) ls1220)])]
   and cvtNAMESPACE_DEFN {ident=x1236, ns=opt1238, init=opt1243} = PrettyRep.Rec [("ident", 
          cvtIDENT x1236), ("ns", 
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1237))
       )), ("init", 
       (case opt1243 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       ))]
   and cvtCLASS_DEFN {ns=opt1255, ident=x1259, nonnullable=b1260, dynamic=b1261, 
          final=b1262, params=ls1264, extends=opt1269, implements=ls1274, classDefns=ls1279, 
          instanceDefns=ls1284, instanceStmts=ls1289, ctorDefn=opt1294} = PrettyRep.Rec [("ns", 
          
       (case opt1255 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1254 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1254))
       )), ("ident", cvtIDENT x1259), ("nonnullable", PrettyRep.Bool b1260), 
          ("dynamic", PrettyRep.Bool b1261), ("final", PrettyRep.Bool b1262), 
          ("params", PrettyRep.List (List.map (fn x1263 => cvtIDENT x1263
                                              ) ls1264)), ("extends", 
       (case opt1269 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1268 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1268))
       )), ("implements", PrettyRep.List (List.map (fn x1273 => cvtTYPE_EXPR x1273
                                                   ) ls1274)), ("classDefns", 
          PrettyRep.List (List.map (fn x1278 => cvtDEFN x1278
                                   ) ls1279)), ("instanceDefns", PrettyRep.List (List.map (fn x1283 => 
                                                                                                 cvtDEFN x1283
                                                                                          ) ls1284)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1288 => cvtSTMT x1288
                                                     ) ls1289)), ("ctorDefn", 
          
       (case opt1294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1293 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1293))
       ))]
   and cvtINTERFACE_DEFN {ident=x1323, ns=opt1325, nonnullable=b1329, params=ls1331, 
          extends=ls1336, instanceDefns=ls1341} = PrettyRep.Rec [("ident", 
          cvtIDENT x1323), ("ns", 
       (case opt1325 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1324 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1324))
       )), ("nonnullable", PrettyRep.Bool b1329), ("params", PrettyRep.List (List.map (fn x1330 => 
                                                                                             cvtIDENT x1330
                                                                                      ) ls1331)), 
          ("extends", PrettyRep.List (List.map (fn x1335 => cvtTYPE_EXPR x1335
                                               ) ls1336)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1340 => cvtDEFN x1340
                                   ) ls1341))]
   and cvtTYPE_DEFN {ident=x1358, ns=opt1360, init=x1364} = PrettyRep.Rec [("ident", 
          cvtIDENT x1358), ("ns", 
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1359))
       )), ("init", cvtTYPE_EXPR x1364)]
   and cvtCLASS_BLOCK {ns=opt1373, ident=x1377, name=opt1379, block=x1383} = 
          PrettyRep.Rec [("ns", 
       (case opt1373 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1372 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1372))
       )), ("ident", cvtIDENT x1377), ("name", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1378 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1378))
       )), ("block", cvtBLOCK x1383)]
   and cvtFOR_ENUM_HEAD {isEach=b1393, bindings=(ls1395, ls1400), expr=x1405} = 
          PrettyRep.Rec [("isEach", PrettyRep.Bool b1393), ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1394 => 
                                                                                                                         cvtBINDING x1394
                                                                                                                  ) ls1395), 
          PrettyRep.List (List.map (fn x1399 => cvtINIT_STEP x1399
                                   ) ls1400)]), ("expr", cvtEXPR x1405)]
   and cvtFOR_ENUM_STMT {isEach=b1413, defn=opt1444, obj=x1448, rib=opt1456, 
          next=x1460, labels=ls1462, body=x1466} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1413), ("defn", 
       (case opt1444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1414, ns=opt1416, static=b1420, prototype=b1421, bindings=(ls1423, 
            ls1428)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1414), ("ns", 
         (case opt1416 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1415))
         )), ("static", PrettyRep.Bool b1420), ("prototype", PrettyRep.Bool b1421), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1422 => 
                                                                          cvtBINDING x1422
                                                                   ) ls1423), 
            PrettyRep.List (List.map (fn x1427 => cvtINIT_STEP x1427
                                     ) ls1428)])]))
       )), ("obj", cvtEXPR x1448), ("rib", 
       (case opt1456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1452 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1449, 
                                                                                      x1450) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1449, 
                                                                                      cvtFIXTURE x1450]
                                                                               ) ls1452)))
       )), ("next", cvtSTMT x1460), ("labels", PrettyRep.List (List.map (fn x1461 => 
                                                                               cvtIDENT x1461
                                                                        ) ls1462)), 
          ("body", cvtSTMT x1466)]
   and cvtFOR_STMT {rib=opt1489, defn=opt1523, init=ls1528, cond=x1532, update=x1533, 
          labels=ls1535, body=x1539} = PrettyRep.Rec [("rib", 
       (case opt1489 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1485 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1482, 
                                                                                      x1483) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1482, 
                                                                                      cvtFIXTURE x1483]
                                                                               ) ls1485)))
       )), ("defn", 
       (case opt1523 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1493, ns=opt1495, static=b1499, prototype=b1500, bindings=(ls1502, 
            ls1507)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1493), ("ns", 
         (case opt1495 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1494 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1494))
         )), ("static", PrettyRep.Bool b1499), ("prototype", PrettyRep.Bool b1500), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1501 => 
                                                                          cvtBINDING x1501
                                                                   ) ls1502), 
            PrettyRep.List (List.map (fn x1506 => cvtINIT_STEP x1506
                                     ) ls1507)])]))
       )), ("init", PrettyRep.List (List.map (fn x1527 => cvtSTMT x1527
                                             ) ls1528)), ("cond", cvtEXPR x1532), 
          ("update", cvtEXPR x1533), ("labels", PrettyRep.List (List.map (fn x1534 => 
                                                                                cvtIDENT x1534
                                                                         ) ls1535)), 
          ("body", cvtSTMT x1539)]
   and cvtWHILE_STMT {cond=x1555, rib=opt1563, body=x1567, labels=ls1569} = 
          PrettyRep.Rec [("cond", cvtEXPR x1555), ("rib", 
       (case opt1563 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1559 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1556, 
                                                                                      x1557) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1556, 
                                                                                      cvtFIXTURE x1557]
                                                                               ) ls1559)))
       )), ("body", cvtSTMT x1567), ("labels", PrettyRep.List (List.map (fn x1568 => 
                                                                               cvtIDENT x1568
                                                                        ) ls1569))]
   and cvtDIRECTIVES {pragmas=ls1583, defns=ls1588, head=opt1593, body=ls1598, 
          loc=opt1603} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1582 => 
                                                                                    cvtPRAGMA x1582
                                                                             ) ls1583)), 
          ("defns", PrettyRep.List (List.map (fn x1587 => cvtDEFN x1587
                                             ) ls1588)), ("head", 
       (case opt1593 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1592 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1592))
       )), ("body", PrettyRep.List (List.map (fn x1597 => cvtSTMT x1597
                                             ) ls1598)), ("loc", 
       (case opt1603 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1602 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1602))
       ))]
   and cvtCASE {label=opt1619, inits=opt1630, body=x1634} = PrettyRep.Rec [("label", 
          
       (case opt1619 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1618 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1618))
       )), ("inits", 
       (case opt1630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1626 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1623, 
                                                                                      x1624) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1623, 
                                                                                      cvtEXPR x1624]
                                                                               ) ls1626)))
       )), ("body", cvtBLOCK x1634)]
   and cvtCATCH_CLAUSE {bindings=(ls1643, ls1648), ty=x1653, rib=opt1661, inits=opt1672, 
          block=x1676} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1642 => 
                                                                                                      cvtBINDING x1642
                                                                                               ) ls1643), 
          PrettyRep.List (List.map (fn x1647 => cvtINIT_STEP x1647
                                   ) ls1648)]), ("ty", cvtTY x1653), ("rib", 
          
       (case opt1661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1657 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1654, 
                                                                                      x1655) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1654, 
                                                                                      cvtFIXTURE x1655]
                                                                               ) ls1657)))
       )), ("inits", 
       (case opt1672 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1668 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1665, 
                                                                                      x1666) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1665, 
                                                                                      cvtEXPR x1666]
                                                                               ) ls1668)))
       )), ("block", cvtBLOCK x1676)]
   and cvtFUNC_NAME {kind=x1688, ident=x1689} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1688), 
          ("ident", cvtIDENT x1689)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1695, getter=opt1697, setter=opt1702} = 
          PrettyRep.Rec [("ty", cvtTY x1695), ("getter", 
       (case opt1697 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1696 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1696))
       )), ("setter", 
       (case opt1702 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1701 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1701))
       ))]
   and cvtFRAGMENT (Unit{name=opt1714, fragments=ls1719}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1714 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1713 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1713))
       )), ("fragments", PrettyRep.List (List.map (fn x1718 => cvtFRAGMENT x1718
                                                  ) ls1719))]))
     | cvtFRAGMENT (Package{name=ls1731, fragments=ls1736}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1730 => 
                                                                        cvtIDENT x1730
                                                                 ) ls1731)), 
          ("fragments", PrettyRep.List (List.map (fn x1735 => cvtFRAGMENT x1735
                                                 ) ls1736))]))
     | cvtFRAGMENT (Anon x1747) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1747))
end

