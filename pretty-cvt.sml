structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x4) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x4))
     | cvtNAMESPACE (Protected x7) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x7))
     | cvtNAMESPACE (Public x10) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x10))
     | cvtNAMESPACE (Internal x13) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x13))
     | cvtNAMESPACE (UserNamespace x16) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x16))
   and cvtNAME {ns=x19, id=x20} = PrettyRep.Rec [("ns", cvtNAMESPACE x19), 
          ("id", cvtIDENT x20)]
   and cvtMULTINAME {nss=ls31, id=x35} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls27 => 
                                                                                                PrettyRep.List (List.map (fn x26 => 
                                                                                                                                cvtNAMESPACE x26
                                                                                                                         ) ls27)
                                                                                         ) ls31)), 
          ("id", cvtIDENT x35)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x46, roundingMode=r47, precision=n48} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x46), ("roundingMode", 
          PrettyRep.DecRm r47), ("precision", PrettyRep.Int n48)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt61) = PrettyRep.Ctor ("Plus", SOME 
       (case opt61 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x60 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x60))
       ))
     | cvtBINOP (Minus opt68) = PrettyRep.Ctor ("Minus", SOME 
       (case opt68 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x67 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x67))
       ))
     | cvtBINOP (Times opt75) = PrettyRep.Ctor ("Times", SOME 
       (case opt75 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x74 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x74))
       ))
     | cvtBINOP (Divide opt82) = PrettyRep.Ctor ("Divide", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Remainder opt89) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
       ))
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
     | cvtBINOP (Equals opt106) = PrettyRep.Ctor ("Equals", SOME 
       (case opt106 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x105 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x105))
       ))
     | cvtBINOP (NotEquals opt113) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt113 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x112 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x112))
       ))
     | cvtBINOP (StrictEquals opt120) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt120 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x119 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x119))
       ))
     | cvtBINOP (StrictNotEquals opt127) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (Less opt134) = PrettyRep.Ctor ("Less", SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (LessOrEqual opt141) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (Greater opt148) = PrettyRep.Ctor ("Greater", SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (GreaterOrEqual opt155) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt164) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt164 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x163 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x163))
       ))
     | cvtASSIGNOP (AssignMinus opt171) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x170 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x170))
       ))
     | cvtASSIGNOP (AssignTimes opt178) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt178 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x177 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x177))
       ))
     | cvtASSIGNOP (AssignDivide opt185) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x184 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x184))
       ))
     | cvtASSIGNOP (AssignRemainder opt192) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x191 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x191))
       ))
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
   and cvtPRAGMA (UseNamespace x226) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x226))
     | cvtPRAGMA (UseDefaultNamespace x229) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x229))
     | cvtPRAGMA (UseNumber x232) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x232))
     | cvtPRAGMA (UseRounding r235) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r235))
     | cvtPRAGMA (UsePrecision n238) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n238))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x243, name=x244, alias=opt246}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x243), ("name", cvtIDENT x244), 
          ("alias", 
       (case opt246 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x245 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x245))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtCLS (Cls{name=x266, extends=opt268, implements=ls273, classFixtures=x277, 
          instanceFixtures=x278, instanceInits=x279, constructor=opt281, classType=x285, 
          instanceType=x286}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x266), ("extends", 
       (case opt268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x267 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x267))
       )), ("implements", PrettyRep.List (List.map (fn x272 => cvtNAME x272
                                                   ) ls273)), ("classFixtures", 
          cvtFIXTURES x277), ("instanceFixtures", cvtFIXTURES x278), ("instanceInits", 
          cvtHEAD x279), ("constructor", 
       (case opt281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x280 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x280))
       )), ("classType", cvtTYPE_EXPR x285), ("instanceType", cvtTYPE_EXPR x286)]))
   and cvtCTOR (Ctor{settings=x308, superArgs=ls310, func=x314}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x308), ("superArgs", PrettyRep.List (List.map (fn x309 => 
                                                                                                         cvtEXPR x309
                                                                                                  ) ls310)), 
          ("func", cvtFUNC x314)]))
   and cvtFUNC (Func{name=x324, fsig=x325, isNative=b326, block=x327, param=x328, 
          defaults=ls330, ty=x334}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x324), ("fsig", cvtFUNC_SIG x325), ("isNative", PrettyRep.Bool b326), 
          ("block", cvtBLOCK x327), ("param", cvtHEAD x328), ("defaults", PrettyRep.List (List.map (fn x329 => 
                                                                                                          cvtEXPR x329
                                                                                                   ) ls330)), 
          ("ty", cvtFUNC_TYPE x334)]))
   and cvtDEFN (ClassDefn x352) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x352))
     | cvtDEFN (VariableDefn x355) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x355))
     | cvtDEFN (FunctionDefn x358) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x358))
     | cvtDEFN (ConstructorDefn x361) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x361))
     | cvtDEFN (InterfaceDefn x364) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x364))
     | cvtDEFN (NamespaceDefn x367) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x367))
     | cvtDEFN (TypeDefn x370) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x370))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls374, params=x378, defaults=ls380, 
          ctorInits=opt391, returnType=x395, thisType=opt397, hasRest=b401}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x373 => cvtIDENT x373
                                   ) ls374)), ("params", cvtBINDINGS x378), 
          ("defaults", PrettyRep.List (List.map (fn x379 => cvtEXPR x379
                                                ) ls380)), ("ctorInits", 
       (case opt391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x384, ls386) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x384, 
            PrettyRep.List (List.map (fn x385 => cvtEXPR x385
                                     ) ls386)]))
       )), ("returnType", cvtTYPE_EXPR x395), ("thisType", 
       (case opt397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x396 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x396))
       )), ("hasRest", PrettyRep.Bool b401)]))
   and cvtBINDING (Binding{ident=x419, ty=opt421}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x419), ("ty", 
       (case opt421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x420 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x420))
       ))]))
   and cvtBINDING_IDENT (TempIdent n432) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n432))
     | cvtBINDING_IDENT (PropIdent x435) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x435))
   and cvtINIT_STEP (InitStep(x438, x439)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x438, 
          cvtEXPR x439]))
     | cvtINIT_STEP (AssignStep(x443, x444)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x443, cvtEXPR x444]))
   and cvtTYPE_EXPR (SpecialType x448) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x448))
     | cvtTYPE_EXPR (UnionType ls452) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x451 => 
                                                                                                           cvtTYPE_EXPR x451
                                                                                                    ) ls452)))
     | cvtTYPE_EXPR (ArrayType ls459) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x458 => 
                                                                                                           cvtTYPE_EXPR x458
                                                                                                    ) ls459)))
     | cvtTYPE_EXPR (TypeName x465) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x465))
     | cvtTYPE_EXPR (ElementTypeRef(x468, n469)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x468, PrettyRep.Int n469]))
     | cvtTYPE_EXPR (FieldTypeRef(x473, x474)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x473, cvtIDENT x474]))
     | cvtTYPE_EXPR (FunctionType x478) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x478))
     | cvtTYPE_EXPR (ObjectType ls482) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x481 => 
                                                                                                             cvtFIELD_TYPE x481
                                                                                                      ) ls482)))
     | cvtTYPE_EXPR (AppType{base=x488, args=ls490}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x488), ("args", PrettyRep.List (List.map (fn x489 => 
                                                                                                     cvtTYPE_EXPR x489
                                                                                              ) ls490))]))
     | cvtTYPE_EXPR (NullableType{expr=x501, nullable=b502}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x501), ("nullable", PrettyRep.Bool b502)]))
     | cvtTYPE_EXPR (InstanceType{name=x510, typeParams=ls512, ty=x516}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x510), 
          ("typeParams", PrettyRep.List (List.map (fn x511 => cvtIDENT x511
                                                  ) ls512)), ("ty", cvtTYPE_EXPR x516)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x527) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x527))
     | cvtSTMT (InitStmt{kind=x530, ns=opt532, prototype=b536, static=b537, 
          temps=x538, inits=ls540}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x530), ("ns", 
       (case opt532 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x531 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x531))
       )), ("prototype", PrettyRep.Bool b536), ("static", PrettyRep.Bool b537), 
          ("temps", cvtBINDINGS x538), ("inits", PrettyRep.List (List.map (fn x539 => 
                                                                                 cvtINIT_STEP x539
                                                                          ) ls540))]))
     | cvtSTMT (ClassBlock{ns=opt560, ident=x564, name=opt566, block=x570}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt560 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x559 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x559))
       )), ("ident", cvtIDENT x564), ("name", 
       (case opt566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x565 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x565))
       )), ("block", cvtBLOCK x570)]))
     | cvtSTMT (PackageBlock{name=x582, block=x583}) = PrettyRep.Ctor ("PackageBlock", 
          SOME (PrettyRep.Rec [("name", cvtIDENT x582), ("block", cvtBLOCK x583)]))
     | cvtSTMT (ForEachStmt x591) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x591))
     | cvtSTMT (ForInStmt x594) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x594))
     | cvtSTMT (ThrowStmt x597) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x597))
     | cvtSTMT (ReturnStmt x600) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x600))
     | cvtSTMT (BreakStmt opt604) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x603 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x603))
       ))
     | cvtSTMT (ContinueStmt opt611) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x610 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x610))
       ))
     | cvtSTMT (BlockStmt x617) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x617))
     | cvtSTMT (LabeledStmt(x620, x621)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x620, 
          cvtSTMT x621]))
     | cvtSTMT (LetStmt x625) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x625))
     | cvtSTMT (WhileStmt x628) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x628))
     | cvtSTMT (DoWhileStmt x631) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x631))
     | cvtSTMT (ForStmt x634) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x634))
     | cvtSTMT (IfStmt{cnd=x637, thn=x638, els=x639}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x637), ("thn", cvtSTMT x638), 
          ("els", cvtSTMT x639)]))
     | cvtSTMT (WithStmt{obj=x649, ty=x650, body=x651}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x649), ("ty", cvtTYPE_EXPR x650), 
          ("body", cvtSTMT x651)]))
     | cvtSTMT (TryStmt{block=x661, catches=ls683, finally=opt688}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x661), ("catches", PrettyRep.List (List.map (fn {bindings=x662, 
                                                                                                     ty=opt664, 
                                                                                                     fixtures=opt669, 
                                                                                                     block=x673} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x662), 
                                                                                                     ("ty", 
                                                                                                     
                                                                                                  (case opt664 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x663 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtTYPE_EXPR x663))
                                                                                                  )), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt669 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x668 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x668))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x673)]
                                                                                              ) ls683)), 
          ("finally", 
       (case opt688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x687 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x687))
       ))]))
     | cvtSTMT (SwitchStmt{cond=x701, cases=ls703}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x701), ("cases", PrettyRep.List (List.map (fn x702 => 
                                                                                                 cvtCASE x702
                                                                                          ) ls703))]))
     | cvtSTMT (SwitchTypeStmt{cond=x714, ty=x715, cases=ls717}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x714), ("ty", cvtTYPE_EXPR x715), 
          ("cases", PrettyRep.List (List.map (fn x716 => cvtTYPE_CASE x716
                                             ) ls717))]))
     | cvtSTMT (Dxns{expr=x730}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x730)]))
   and cvtEXPR (TrinaryExpr(x736, x737, x738, x739)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x736, cvtEXPR x737, cvtEXPR x738, 
          cvtEXPR x739]))
     | cvtEXPR (BinaryExpr(x743, x744, x745)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x743, cvtEXPR x744, cvtEXPR x745]))
     | cvtEXPR (BinaryTypeExpr(x749, x750, x751)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x749, cvtEXPR x750, cvtTYPE_EXPR x751]))
     | cvtEXPR (UnaryExpr(x755, x756)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x755, 
          cvtEXPR x756]))
     | cvtEXPR (TypeExpr x760) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x760))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt765) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt765 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x764 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x764))
       ))
     | cvtEXPR (SuperExpr opt772) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt772 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x771 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x771))
       ))
     | cvtEXPR (LiteralExpr x778) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x778))
     | cvtEXPR (CallExpr{func=x781, actuals=ls783}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x781), ("actuals", PrettyRep.List (List.map (fn x782 => 
                                                                                                   cvtEXPR x782
                                                                                            ) ls783))]))
     | cvtEXPR (ApplyTypeExpr{expr=x794, actuals=ls796}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x794), ("actuals", PrettyRep.List (List.map (fn x795 => 
                                                                                                   cvtTYPE_EXPR x795
                                                                                            ) ls796))]))
     | cvtEXPR (LetExpr{defs=x807, body=x808, head=opt810}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x807), ("body", cvtEXPR x808), 
          ("head", 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x809))
       ))]))
     | cvtEXPR (NewExpr{obj=x823, actuals=ls825}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x823), ("actuals", PrettyRep.List (List.map (fn x824 => 
                                                                                                  cvtEXPR x824
                                                                                           ) ls825))]))
     | cvtEXPR (ObjectRef{base=x836, ident=x837}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x836), ("ident", cvtIDENT_EXPR x837)]))
     | cvtEXPR (LexicalRef{ident=x845}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x845)]))
     | cvtEXPR (SetExpr(x851, x852, x853)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x851, 
          cvtEXPR x852, cvtEXPR x853]))
     | cvtEXPR (ListExpr ls858) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x857 => 
                                                                                                    cvtEXPR x857
                                                                                             ) ls858)))
     | cvtEXPR (InitExpr(x864, x865, x866)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x864, 
          cvtHEAD x865, cvtINITS x866]))
     | cvtEXPR (SliceExpr(x870, x871, x872)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x870, cvtEXPR x871, cvtEXPR x872]))
     | cvtEXPR (GetTemp n876) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n876))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n882) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n882))
     | cvtFIXTURE_NAME (PropName x885) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x885))
   and cvtIDENT_EXPR (Identifier{ident=x888, openNamespaces=ls894}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x888), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls890 => PrettyRep.List (List.map (fn x889 => 
                                                                                cvtNAMESPACE x889
                                                                         ) ls890)
                                   ) ls894))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x905, expr=x906}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x905), ("expr", cvtEXPR x906)]))
     | cvtIDENT_EXPR (AttributeIdentifier x914) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x914))
     | cvtIDENT_EXPR (ExpressionIdentifier x917) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x917))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x920, ident=x921}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x920), ("ident", cvtUSTRING x921)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x929, typeParams=ls931}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x929), ("typeParams", 
          PrettyRep.List (List.map (fn x930 => cvtTYPE_EXPR x930
                                   ) ls931))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s944) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s944))
     | cvtLITERAL (LiteralContextualDecimalInteger s947) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s947))
     | cvtLITERAL (LiteralContextualHexInteger s950) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s950))
     | cvtLITERAL (LiteralDouble r953) = PrettyRep.Ctor ("LiteralDouble", SOME (PrettyRep.Real64 r953))
     | cvtLITERAL (LiteralDecimal d956) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d956))
     | cvtLITERAL (LiteralInt i959) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i959))
     | cvtLITERAL (LiteralUInt u962) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u962))
     | cvtLITERAL (LiteralBoolean b965) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b965))
     | cvtLITERAL (LiteralString x968) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x968))
     | cvtLITERAL (LiteralArray{exprs=ls972, ty=opt977}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x971 => 
                                                                         cvtEXPR x971
                                                                  ) ls972)), 
          ("ty", 
       (case opt977 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x976 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x976))
       ))]))
     | cvtLITERAL (LiteralXML ls989) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x988 => 
                                                                                                           cvtEXPR x988
                                                                                                    ) ls989)))
     | cvtLITERAL (LiteralNamespace x995) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x995))
     | cvtLITERAL (LiteralObject{expr=ls999, ty=opt1004}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x998 => 
                                                                        cvtFIELD x998
                                                                 ) ls999)), 
          ("ty", 
       (case opt1004 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1003 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1003))
       ))]))
     | cvtLITERAL (LiteralFunction x1015) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1015))
     | cvtLITERAL (LiteralRegExp{str=x1018}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1018)]))
   and cvtBLOCK (Block x1024) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1024))
   and cvtFIXTURE (NamespaceFixture x1027) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1027))
     | cvtFIXTURE (ClassFixture x1030) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1030))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1034) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1034))
     | cvtFIXTURE (MethodFixture{func=x1037, ty=x1038, readOnly=b1039, override=b1040, 
          final=b1041}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1037), ("ty", cvtTYPE_EXPR x1038), ("readOnly", PrettyRep.Bool b1039), 
          ("override", PrettyRep.Bool b1040), ("final", PrettyRep.Bool b1041)]))
     | cvtFIXTURE (ValFixture{ty=x1055, readOnly=b1056}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1055), ("readOnly", PrettyRep.Bool b1056)]))
     | cvtFIXTURE (VirtualValFixture{ty=x1064, getter=opt1066, setter=opt1071}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x1064), ("getter", 
       (case opt1066 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1065 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1065))
       )), ("setter", 
       (case opt1071 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1070 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1070))
       ))]))
   and cvtBINDINGS (ls1085, ls1090) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1084 => 
                                                                                       cvtBINDING x1084
                                                                                ) ls1085), 
          PrettyRep.List (List.map (fn x1089 => cvtINIT_STEP x1089
                                   ) ls1090)]
   and cvtFIXTURES ls1098 = PrettyRep.List (List.map (fn (x1095, x1096) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1095, 
                                                            cvtFIXTURE x1096]
                                                     ) ls1098)
   and cvtINITS ls1105 = PrettyRep.List (List.map (fn (x1102, x1103) => PrettyRep.Tuple [cvtFIXTURE_NAME x1102, 
                                                         cvtEXPR x1103]
                                                  ) ls1105)
   and cvtHEAD (x1109, x1110) = PrettyRep.Tuple [cvtFIXTURES x1109, cvtINITS x1110]
   and cvtFIELD {kind=x1112, name=x1113, init=x1114} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1112), ("name", cvtIDENT_EXPR x1113), ("init", cvtEXPR x1114)]
   and cvtFIELD_TYPE {name=x1122, ty=x1123} = PrettyRep.Rec [("name", cvtIDENT x1122), 
          ("ty", cvtTYPE_EXPR x1123)]
   and cvtTYPED_IDENT {name=x1129, ty=opt1131} = PrettyRep.Rec [("name", cvtIDENT x1129), 
          ("ty", 
       (case opt1131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1130 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1130))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1141, params=ls1146, result=x1150, thisType=opt1152, 
          hasRest=b1156, minArgs=n1157} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1140 => 
                                                                                                        cvtIDENT x1140
                                                                                                 ) ls1141)), 
          ("params", PrettyRep.List (List.map (fn x1145 => cvtTYPE_EXPR x1145
                                              ) ls1146)), ("result", cvtTYPE_EXPR x1150), 
          ("thisType", 
       (case opt1152 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1151 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1151))
       )), ("hasRest", PrettyRep.Bool b1156), ("minArgs", PrettyRep.Int n1157)]
   and cvtFUNC_DEFN {kind=x1171, ns=opt1173, final=b1177, override=b1178, prototype=b1179, 
          static=b1180, func=x1181} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1171), 
          ("ns", 
       (case opt1173 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1172 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1172))
       )), ("final", PrettyRep.Bool b1177), ("override", PrettyRep.Bool b1178), 
          ("prototype", PrettyRep.Bool b1179), ("static", PrettyRep.Bool b1180), 
          ("func", cvtFUNC x1181)]
   and cvtCTOR_DEFN x1197 = cvtCTOR x1197
   and cvtVAR_DEFN {kind=x1198, ns=opt1200, static=b1204, prototype=b1205, 
          bindings=x1206} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1198), 
          ("ns", 
       (case opt1200 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1199 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1199))
       )), ("static", PrettyRep.Bool b1204), ("prototype", PrettyRep.Bool b1205), 
          ("bindings", cvtBINDINGS x1206)]
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
   and cvtCLASS_DEFN {ident=x1236, ns=opt1238, nonnullable=b1242, dynamic=b1243, 
          final=b1244, params=ls1246, extends=opt1251, implements=ls1256, classDefns=ls1261, 
          instanceDefns=ls1266, instanceStmts=ls1271, ctorDefn=opt1276} = PrettyRep.Rec [("ident", 
          cvtIDENT x1236), ("ns", 
       (case opt1238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1237 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1237))
       )), ("nonnullable", PrettyRep.Bool b1242), ("dynamic", PrettyRep.Bool b1243), 
          ("final", PrettyRep.Bool b1244), ("params", PrettyRep.List (List.map (fn x1245 => 
                                                                                      cvtIDENT x1245
                                                                               ) ls1246)), 
          ("extends", 
       (case opt1251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1250 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1250))
       )), ("implements", PrettyRep.List (List.map (fn x1255 => cvtIDENT_EXPR x1255
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
          extends=ls1318, block=x1322} = PrettyRep.Rec [("ident", cvtIDENT x1305), 
          ("ns", 
       (case opt1307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1306 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1306))
       )), ("nonnullable", PrettyRep.Bool b1311), ("params", PrettyRep.List (List.map (fn x1312 => 
                                                                                             cvtIDENT x1312
                                                                                      ) ls1313)), 
          ("extends", PrettyRep.List (List.map (fn x1317 => cvtIDENT_EXPR x1317
                                               ) ls1318)), ("block", cvtBLOCK x1322)]
   and cvtTYPE_DEFN {ident=x1336, ns=opt1338, init=x1342} = PrettyRep.Rec [("ident", 
          cvtIDENT x1336), ("ns", 
       (case opt1338 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1337 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1337))
       )), ("init", cvtTYPE_EXPR x1342)]
   and cvtFOR_ENUM_STMT {defn=opt1351, obj=x1355, fixtures=opt1357, inits=opt1362, 
          labels=ls1367, body=x1371} = PrettyRep.Rec [("defn", 
       (case opt1351 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1350 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1350))
       )), ("obj", cvtEXPR x1355), ("fixtures", 
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1356))
       )), ("inits", 
       (case opt1362 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1361 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1361))
       )), ("labels", PrettyRep.List (List.map (fn x1366 => cvtIDENT x1366
                                               ) ls1367)), ("body", cvtSTMT x1371)]
   and cvtFOR_STMT {fixtures=opt1386, defn=opt1391, init=x1395, cond=x1396, 
          update=x1397, labels=ls1399, body=x1403} = PrettyRep.Rec [("fixtures", 
          
       (case opt1386 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1385 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1385))
       )), ("defn", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1390))
       )), ("init", cvtSTMT x1395), ("cond", cvtEXPR x1396), ("update", cvtEXPR x1397), 
          ("labels", PrettyRep.List (List.map (fn x1398 => cvtIDENT x1398
                                              ) ls1399)), ("body", cvtSTMT x1403)]
   and cvtWHILE_STMT {cond=x1419, fixtures=opt1421, body=x1425, labels=ls1427} = 
          PrettyRep.Rec [("cond", cvtEXPR x1419), ("fixtures", 
       (case opt1421 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1420 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1420))
       )), ("body", cvtSTMT x1425), ("labels", PrettyRep.List (List.map (fn x1426 => 
                                                                               cvtIDENT x1426
                                                                        ) ls1427))]
   and cvtDIRECTIVES {pragmas=ls1441, defns=ls1446, head=opt1451, body=ls1456} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1440 => 
                                                                     cvtPRAGMA x1440
                                                              ) ls1441)), ("defns", 
          PrettyRep.List (List.map (fn x1445 => cvtDEFN x1445
                                   ) ls1446)), ("head", 
       (case opt1451 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1450 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1450))
       )), ("body", PrettyRep.List (List.map (fn x1455 => cvtSTMT x1455
                                             ) ls1456))]
   and cvtCASE {label=opt1470, inits=opt1475, body=x1479} = PrettyRep.Rec [("label", 
          
       (case opt1470 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1469 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1469))
       )), ("inits", 
       (case opt1475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1474 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1474))
       )), ("body", cvtBLOCK x1479)]
   and cvtTYPE_CASE {ty=opt1488, bindings=x1492, inits=opt1494, body=x1498} = 
          PrettyRep.Rec [("ty", 
       (case opt1488 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1487 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1487))
       )), ("bindings", cvtBINDINGS x1492), ("inits", 
       (case opt1494 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1493))
       )), ("body", cvtBLOCK x1498)]
   and cvtFUNC_NAME {kind=x1508, ident=x1509} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1508), 
          ("ident", cvtIDENT x1509)]
   and cvtPACKAGE {name=x1515, block=x1516} = PrettyRep.Rec [("name", cvtUSTRING x1515), 
          ("block", cvtBLOCK x1516)]
   and cvtPROGRAM {packages=ls1523, fixtures=opt1528, block=x1532} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1522 => cvtPACKAGE x1522
                                   ) ls1523)), ("fixtures", 
       (case opt1528 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1527 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1527))
       )), ("block", cvtBLOCK x1532)]
end

