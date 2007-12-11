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
     | cvtEXPR (GetTemp n814) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n814))
     | cvtEXPR (GetParam n817) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n817))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n823) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n823))
     | cvtFIXTURE_NAME (PropName x826) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x826))
   and cvtIDENT_EXPR (Identifier{ident=x829, openNamespaces=ls835}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x829), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls831 => PrettyRep.List (List.map (fn x830 => 
                                                                                cvtNAMESPACE x830
                                                                         ) ls831)
                                   ) ls835))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x846, expr=x847}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x846), ("expr", cvtEXPR x847)]))
     | cvtIDENT_EXPR (AttributeIdentifier x855) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x855))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x858, openNamespaces=ls864}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x858), ("openNamespaces", PrettyRep.List (List.map (fn ls860 => 
                                                                            PrettyRep.List (List.map (fn x859 => 
                                                                                                            cvtNAMESPACE x859
                                                                                                     ) ls860)
                                                                     ) ls864))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x875, ident=s876}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x875), ("ident", PrettyRep.UniStr s876)]))
     | cvtIDENT_EXPR (UnresolvedPath(ls885, x889)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x884 => cvtIDENT x884
                                                          ) ls885), cvtIDENT_EXPR x889]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralDouble r896) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r896))
     | cvtLITERAL (LiteralDecimal d899) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d899))
     | cvtLITERAL (LiteralInt i902) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i902))
     | cvtLITERAL (LiteralUInt u905) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u905))
     | cvtLITERAL (LiteralBoolean b908) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b908))
     | cvtLITERAL (LiteralString s911) = PrettyRep.Ctor ("LiteralString", SOME (PrettyRep.UniStr s911))
     | cvtLITERAL (LiteralArray{exprs=ls915, ty=opt920}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x914 => 
                                                                         cvtEXPR x914
                                                                  ) ls915)), 
          ("ty", 
       (case opt920 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x919 => PrettyRep.Ctor ("SOME", SOME (cvtTY x919))
       ))]))
     | cvtLITERAL (LiteralXML ls932) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x931 => 
                                                                                                           cvtEXPR x931
                                                                                                    ) ls932)))
     | cvtLITERAL (LiteralNamespace x938) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x938))
     | cvtLITERAL (LiteralObject{expr=ls942, ty=opt947}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x941 => 
                                                                        cvtFIELD x941
                                                                 ) ls942)), 
          ("ty", 
       (case opt947 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x946 => PrettyRep.Ctor ("SOME", SOME (cvtTY x946))
       ))]))
     | cvtLITERAL (LiteralFunction x958) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x958))
     | cvtLITERAL (LiteralRegExp{str=s961}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s961)]))
   and cvtBLOCK (Block x967) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x967))
   and cvtFIXTURE (NamespaceFixture x970) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x970))
     | cvtFIXTURE (ClassFixture x973) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x973))
     | cvtFIXTURE (InterfaceFixture x976) = PrettyRep.Ctor ("InterfaceFixture", 
          SOME (cvtIFACE x976))
     | cvtFIXTURE (TypeVarFixture x979) = PrettyRep.Ctor ("TypeVarFixture", 
          SOME (cvtTYPEVAR_NONCE x979))
     | cvtFIXTURE (TypeFixture x982) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTY x982))
     | cvtFIXTURE (MethodFixture{func=x985, ty=x986, readOnly=b987, override=b988, 
          final=b989}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x985), ("ty", cvtTY x986), ("readOnly", PrettyRep.Bool b987), 
          ("override", PrettyRep.Bool b988), ("final", PrettyRep.Bool b989)]))
     | cvtFIXTURE (ValFixture{ty=x1003, readOnly=b1004}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTY x1003), ("readOnly", PrettyRep.Bool b1004)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1012, getter=opt1014, setter=opt1019}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTY x1012), ("getter", 
       (case opt1014 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1013 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1013))
       )), ("setter", 
       (case opt1019 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1018 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1018))
       ))]))
     | cvtFIXTURE (InheritedFixture{baseName=x1032, baseTypeArgs=ls1034}) = 
          PrettyRep.Ctor ("InheritedFixture", SOME (PrettyRep.Rec [("baseName", 
          cvtNAME x1032), ("baseTypeArgs", PrettyRep.List (List.map (fn x1033 => 
                                                                           cvtTY x1033
                                                                    ) ls1034))]))
   and cvtHEAD (Head(x1045, x1046)) = PrettyRep.Ctor ("Head", SOME (PrettyRep.Tuple [cvtRIB x1045, 
          cvtINITS x1046]))
   and cvtBINDINGS (ls1051, ls1056) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1050 => 
                                                                                       cvtBINDING x1050
                                                                                ) ls1051), 
          PrettyRep.List (List.map (fn x1055 => cvtINIT_STEP x1055
                                   ) ls1056)]
   and cvtRIB ls1064 = PrettyRep.List (List.map (fn (x1061, x1062) => PrettyRep.Tuple [cvtFIXTURE_NAME x1061, 
                                                       cvtFIXTURE x1062]
                                                ) ls1064)
   and cvtRIBS ls1075 = PrettyRep.List (List.map (fn ls1071 => PrettyRep.List (List.map (fn (x1068, 
                                                                                               x1069) => 
                                                                                               PrettyRep.Tuple [cvtFIXTURE_NAME x1068, 
                                                                                               cvtFIXTURE x1069]
                                                                                        ) ls1071)
                                                 ) ls1075)
   and cvtINITS ls1082 = PrettyRep.List (List.map (fn (x1079, x1080) => PrettyRep.Tuple [cvtFIXTURE_NAME x1079, 
                                                         cvtEXPR x1080]
                                                  ) ls1082)
   and cvtINSTANCE_TYPE {name=x1086, typeParams=ls1088, typeArgs=ls1093, nonnullable=b1097, 
          superTypes=ls1099, ty=x1103, dynamic=b1104} = PrettyRep.Rec [("name", 
          cvtNAME x1086), ("typeParams", PrettyRep.List (List.map (fn x1087 => 
                                                                         cvtIDENT x1087
                                                                  ) ls1088)), 
          ("typeArgs", PrettyRep.List (List.map (fn x1092 => cvtTYPE_EXPR x1092
                                                ) ls1093)), ("nonnullable", 
          PrettyRep.Bool b1097), ("superTypes", PrettyRep.List (List.map (fn x1098 => 
                                                                                cvtTYPE_EXPR x1098
                                                                         ) ls1099)), 
          ("ty", cvtTYPE_EXPR x1103), ("dynamic", PrettyRep.Bool b1104)]
   and cvtFIELD {kind=x1120, name=x1121, init=x1122} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1120), ("name", cvtIDENT_EXPR x1121), ("init", cvtEXPR x1122)]
   and cvtFIELD_TYPE {name=x1130, ty=x1131} = PrettyRep.Rec [("name", cvtIDENT x1130), 
          ("ty", cvtTYPE_EXPR x1131)]
   and cvtFUNC_TYPE {params=ls1138, result=x1142, thisType=opt1144, hasRest=b1148, 
          minArgs=n1149} = PrettyRep.Rec [("params", PrettyRep.List (List.map (fn x1137 => 
                                                                                     cvtTYPE_EXPR x1137
                                                                              ) ls1138)), 
          ("result", cvtTYPE_EXPR x1142), ("thisType", 
       (case opt1144 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1143 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1143))
       )), ("hasRest", PrettyRep.Bool b1148), ("minArgs", PrettyRep.Int n1149)]
   and cvtFUNC_DEFN {kind=x1161, ns=opt1163, final=b1167, override=b1168, prototype=b1169, 
          static=b1170, func=x1171} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1161), 
          ("ns", 
       (case opt1163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1162 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1162))
       )), ("final", PrettyRep.Bool b1167), ("override", PrettyRep.Bool b1168), 
          ("prototype", PrettyRep.Bool b1169), ("static", PrettyRep.Bool b1170), 
          ("func", cvtFUNC x1171)]
   and cvtCTOR_DEFN x1187 = cvtCTOR x1187
   and cvtVAR_DEFN {kind=x1188, ns=opt1190, static=b1194, prototype=b1195, 
          bindings=(ls1197, ls1202)} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1188), 
          ("ns", 
       (case opt1190 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1189 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1189))
       )), ("static", PrettyRep.Bool b1194), ("prototype", PrettyRep.Bool b1195), 
          ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1196 => 
                                                                        cvtBINDING x1196
                                                                 ) ls1197), 
          PrettyRep.List (List.map (fn x1201 => cvtINIT_STEP x1201
                                   ) ls1202)])]
   and cvtNAMESPACE_DEFN {ident=x1218, ns=opt1220, init=opt1225} = PrettyRep.Rec [("ident", 
          cvtIDENT x1218), ("ns", 
       (case opt1220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1219 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1219))
       )), ("init", 
       (case opt1225 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1224 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1224))
       ))]
   and cvtCLASS_DEFN {ns=opt1237, ident=x1241, nonnullable=b1242, dynamic=b1243, 
          final=b1244, params=ls1246, extends=opt1251, implements=ls1256, classDefns=ls1261, 
          instanceDefns=ls1266, instanceStmts=ls1271, ctorDefn=opt1276} = PrettyRep.Rec [("ns", 
          
       (case opt1237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1236 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1236))
       )), ("ident", cvtIDENT x1241), ("nonnullable", PrettyRep.Bool b1242), 
          ("dynamic", PrettyRep.Bool b1243), ("final", PrettyRep.Bool b1244), 
          ("params", PrettyRep.List (List.map (fn x1245 => cvtIDENT x1245
                                              ) ls1246)), ("extends", 
       (case opt1251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1250 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1250))
       )), ("implements", PrettyRep.List (List.map (fn x1255 => cvtTYPE_EXPR x1255
                                                   ) ls1256)), ("classDefns", 
          PrettyRep.List (List.map (fn x1260 => cvtDEFN x1260
                                   ) ls1261)), ("instanceDefns", PrettyRep.List (List.map (fn x1265 => 
                                                                                                 cvtDEFN x1265
                                                                                          ) ls1266)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1270 => cvtSTMT x1270
                                                     ) ls1271)), ("ctorDefn", 
          
       (case opt1276 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1275 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1275))
       ))]
   and cvtINTERFACE_DEFN {ident=x1305, ns=opt1307, nonnullable=b1311, params=ls1313, 
          extends=ls1318, instanceDefns=ls1323} = PrettyRep.Rec [("ident", 
          cvtIDENT x1305), ("ns", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1306))
       )), ("nonnullable", PrettyRep.Bool b1311), ("params", PrettyRep.List (List.map (fn x1312 => 
                                                                                             cvtIDENT x1312
                                                                                      ) ls1313)), 
          ("extends", PrettyRep.List (List.map (fn x1317 => cvtTYPE_EXPR x1317
                                               ) ls1318)), ("instanceDefns", 
          PrettyRep.List (List.map (fn x1322 => cvtDEFN x1322
                                   ) ls1323))]
   and cvtTYPE_DEFN {ident=x1340, ns=opt1342, init=x1346} = PrettyRep.Rec [("ident", 
          cvtIDENT x1340), ("ns", 
       (case opt1342 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1341 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1341))
       )), ("init", cvtTYPE_EXPR x1346)]
   and cvtCLASS_BLOCK {ns=opt1355, ident=x1359, name=opt1361, block=x1365} = 
          PrettyRep.Rec [("ns", 
       (case opt1355 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1354 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1354))
       )), ("ident", cvtIDENT x1359), ("name", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x1360))
       )), ("block", cvtBLOCK x1365)]
   and cvtFOR_ENUM_STMT {isEach=b1375, defn=opt1406, obj=x1410, rib=opt1418, 
          next=x1422, labels=ls1424, body=x1428} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1375), ("defn", 
       (case opt1406 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1376, ns=opt1378, static=b1382, prototype=b1383, bindings=(ls1385, 
            ls1390)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1376), ("ns", 
         (case opt1378 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1377 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1377))
         )), ("static", PrettyRep.Bool b1382), ("prototype", PrettyRep.Bool b1383), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1384 => 
                                                                          cvtBINDING x1384
                                                                   ) ls1385), 
            PrettyRep.List (List.map (fn x1389 => cvtINIT_STEP x1389
                                     ) ls1390)])]))
       )), ("obj", cvtEXPR x1410), ("rib", 
       (case opt1418 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1414 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1411, 
                                                                                      x1412) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1411, 
                                                                                      cvtFIXTURE x1412]
                                                                               ) ls1414)))
       )), ("next", cvtSTMT x1422), ("labels", PrettyRep.List (List.map (fn x1423 => 
                                                                               cvtIDENT x1423
                                                                        ) ls1424)), 
          ("body", cvtSTMT x1428)]
   and cvtFOR_STMT {rib=opt1451, defn=opt1485, init=ls1490, cond=x1494, update=x1495, 
          labels=ls1497, body=x1501} = PrettyRep.Rec [("rib", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1447 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1444, 
                                                                                      x1445) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1444, 
                                                                                      cvtFIXTURE x1445]
                                                                               ) ls1447)))
       )), ("defn", 
       (case opt1485 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME{kind=x1455, ns=opt1457, static=b1461, prototype=b1462, bindings=(ls1464, 
            ls1469)} => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Rec [("kind", 
            cvtVAR_DEFN_TAG x1455), ("ns", 
         (case opt1457 of
           NONE => PrettyRep.Ctor ("NONE", NONE)
         | SOME x1456 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1456))
         )), ("static", PrettyRep.Bool b1461), ("prototype", PrettyRep.Bool b1462), 
            ("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1463 => 
                                                                          cvtBINDING x1463
                                                                   ) ls1464), 
            PrettyRep.List (List.map (fn x1468 => cvtINIT_STEP x1468
                                     ) ls1469)])]))
       )), ("init", PrettyRep.List (List.map (fn x1489 => cvtSTMT x1489
                                             ) ls1490)), ("cond", cvtEXPR x1494), 
          ("update", cvtEXPR x1495), ("labels", PrettyRep.List (List.map (fn x1496 => 
                                                                                cvtIDENT x1496
                                                                         ) ls1497)), 
          ("body", cvtSTMT x1501)]
   and cvtWHILE_STMT {cond=x1517, rib=opt1525, body=x1529, labels=ls1531} = 
          PrettyRep.Rec [("cond", cvtEXPR x1517), ("rib", 
       (case opt1525 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1521 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1518, 
                                                                                      x1519) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1518, 
                                                                                      cvtFIXTURE x1519]
                                                                               ) ls1521)))
       )), ("body", cvtSTMT x1529), ("labels", PrettyRep.List (List.map (fn x1530 => 
                                                                               cvtIDENT x1530
                                                                        ) ls1531))]
   and cvtDIRECTIVES {pragmas=ls1545, defns=ls1550, head=opt1555, body=ls1560, 
          loc=opt1565} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1544 => 
                                                                                    cvtPRAGMA x1544
                                                                             ) ls1545)), 
          ("defns", PrettyRep.List (List.map (fn x1549 => cvtDEFN x1549
                                             ) ls1550)), ("head", 
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1554))
       )), ("body", PrettyRep.List (List.map (fn x1559 => cvtSTMT x1559
                                             ) ls1560)), ("loc", 
       (case opt1565 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1564 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1564))
       ))]
   and cvtCASE {label=opt1581, inits=opt1592, body=x1596} = PrettyRep.Rec [("label", 
          
       (case opt1581 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1580))
       )), ("inits", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1588 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1585, 
                                                                                      x1586) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1585, 
                                                                                      cvtEXPR x1586]
                                                                               ) ls1588)))
       )), ("body", cvtBLOCK x1596)]
   and cvtCATCH_CLAUSE {bindings=(ls1605, ls1610), ty=x1615, rib=opt1623, inits=opt1634, 
          block=x1638} = PrettyRep.Rec [("bindings", PrettyRep.Tuple [PrettyRep.List (List.map (fn x1604 => 
                                                                                                      cvtBINDING x1604
                                                                                               ) ls1605), 
          PrettyRep.List (List.map (fn x1609 => cvtINIT_STEP x1609
                                   ) ls1610)]), ("ty", cvtTY x1615), ("rib", 
          
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1619 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1616, 
                                                                                      x1617) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1616, 
                                                                                      cvtFIXTURE x1617]
                                                                               ) ls1619)))
       )), ("inits", 
       (case opt1634 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1630 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn (x1627, 
                                                                                      x1628) => 
                                                                                      PrettyRep.Tuple [cvtFIXTURE_NAME x1627, 
                                                                                      cvtEXPR x1628]
                                                                               ) ls1630)))
       )), ("block", cvtBLOCK x1638)]
   and cvtFUNC_NAME {kind=x1650, ident=x1651} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1650), 
          ("ident", cvtIDENT x1651)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1657, getter=opt1659, setter=opt1664} = 
          PrettyRep.Rec [("ty", cvtTY x1657), ("getter", 
       (case opt1659 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1658 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1658))
       )), ("setter", 
       (case opt1664 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1663 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1663))
       ))]
   and cvtFRAGMENT (Unit{name=opt1676, fragments=ls1681}) = PrettyRep.Ctor ("Unit", 
          SOME (PrettyRep.Rec [("name", 
       (case opt1676 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1675 => PrettyRep.Ctor ("SOME", SOME (cvtUNIT_NAME x1675))
       )), ("fragments", PrettyRep.List (List.map (fn x1680 => cvtFRAGMENT x1680
                                                  ) ls1681))]))
     | cvtFRAGMENT (Package{name=ls1693, fragments=ls1698}) = PrettyRep.Ctor ("Package", 
          SOME (PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1692 => 
                                                                        cvtIDENT x1692
                                                                 ) ls1693)), 
          ("fragments", PrettyRep.List (List.map (fn x1697 => cvtFRAGMENT x1697
                                                 ) ls1698))]))
     | cvtFRAGMENT (Anon x1709) = PrettyRep.Ctor ("Anon", SOME (cvtBLOCK x1709))
end

