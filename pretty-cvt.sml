structure PrettyCvt = struct
   open Ast
   fun cvtPOS n0 = PrettyRep.Int n0
   and cvtUSTRING s1 = PrettyRep.String s1
   and cvtIDENT x2 = cvtUSTRING x2
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x5) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x5))
     | cvtNAMESPACE (Protected x8) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x8))
     | cvtNAMESPACE (Public x11) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x11))
     | cvtNAMESPACE (Internal x14) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x14))
     | cvtNAMESPACE (UserNamespace x17) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtIDENT x17))
     | cvtNAMESPACE (Imported(x20, x21)) = PrettyRep.Ctor ("Imported", SOME (PrettyRep.Tuple [cvtIDENT x20, 
          cvtIDENT x21]))
   and cvtNAME {ns=x25, id=x26} = PrettyRep.Rec [("ns", cvtNAMESPACE x25), 
          ("id", cvtIDENT x26)]
   and cvtMULTINAME {nss=ls37, id=x41} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls33 => 
                                                                                                PrettyRep.List (List.map (fn x32 => 
                                                                                                                                cvtNAMESPACE x32
                                                                                                                         ) ls33)
                                                                                         ) ls37)), 
          ("id", cvtIDENT x41)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x52, roundingMode=r53, precision=n54} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x52), ("roundingMode", 
          PrettyRep.DecRm r53), ("precision", PrettyRep.Int n54)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt67) = PrettyRep.Ctor ("Plus", SOME 
       (case opt67 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x66 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x66))
       ))
     | cvtBINOP (Minus opt74) = PrettyRep.Ctor ("Minus", SOME 
       (case opt74 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x73 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x73))
       ))
     | cvtBINOP (Times opt81) = PrettyRep.Ctor ("Times", SOME 
       (case opt81 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x80 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x80))
       ))
     | cvtBINOP (Divide opt88) = PrettyRep.Ctor ("Divide", SOME 
       (case opt88 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x87 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x87))
       ))
     | cvtBINOP (Remainder opt95) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt95 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x94 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x94))
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
     | cvtBINOP (Equals opt112) = PrettyRep.Ctor ("Equals", SOME 
       (case opt112 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x111 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x111))
       ))
     | cvtBINOP (NotEquals opt119) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x118 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x118))
       ))
     | cvtBINOP (StrictEquals opt126) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x125 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x125))
       ))
     | cvtBINOP (StrictNotEquals opt133) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt133 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x132 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x132))
       ))
     | cvtBINOP (Less opt140) = PrettyRep.Ctor ("Less", SOME 
       (case opt140 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x139 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x139))
       ))
     | cvtBINOP (LessOrEqual opt147) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt147 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x146 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x146))
       ))
     | cvtBINOP (Greater opt154) = PrettyRep.Ctor ("Greater", SOME 
       (case opt154 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x153 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x153))
       ))
     | cvtBINOP (GreaterOrEqual opt161) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt161 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x160 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x160))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt170) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x169 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x169))
       ))
     | cvtASSIGNOP (AssignMinus opt177) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x176 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x176))
       ))
     | cvtASSIGNOP (AssignTimes opt184) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x183 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x183))
       ))
     | cvtASSIGNOP (AssignDivide opt191) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x190 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x190))
       ))
     | cvtASSIGNOP (AssignRemainder opt198) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt198 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x197 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x197))
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
     | cvtUNOP (PreIncrement opt216) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt216 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x215 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x215))
       ))
     | cvtUNOP (PreDecrement opt223) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt223 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x222 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x222))
       ))
     | cvtUNOP (PostIncrement opt230) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x229 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x229))
       ))
     | cvtUNOP (PostDecrement opt237) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt237 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x236 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x236))
       ))
     | cvtUNOP (UnaryPlus opt244) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt244 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x243 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x243))
       ))
     | cvtUNOP (UnaryMinus opt251) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt251 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x250 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x250))
       ))
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
   and cvtPRAGMA (UseNamespace x268) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x268))
     | cvtPRAGMA (UseDefaultNamespace x271) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x271))
     | cvtPRAGMA (UseNumber x274) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x274))
     | cvtPRAGMA (UseRounding r277) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r277))
     | cvtPRAGMA (UsePrecision n280) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n280))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls286, name=x290, alias=opt292}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x285 => 
                                                                           cvtIDENT x285
                                                                    ) ls286)), 
          ("name", cvtIDENT x290), ("alias", 
       (case opt292 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x291 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x291))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
   and cvtCLS (Cls{name=x308, extends=opt310, implements=ls315, classFixtures=x319, 
          instanceFixtures=x320, instanceInits=x321, constructor=opt323, classType=x327, 
          instanceType=x328}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x308), ("extends", 
       (case opt310 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x309 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x309))
       )), ("implements", PrettyRep.List (List.map (fn x314 => cvtNAME x314
                                                   ) ls315)), ("classFixtures", 
          cvtFIXTURES x319), ("instanceFixtures", cvtFIXTURES x320), ("instanceInits", 
          cvtHEAD x321), ("constructor", 
       (case opt323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x322 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x322))
       )), ("classType", cvtTYPE_EXPR x327), ("instanceType", cvtTYPE_EXPR x328)]))
   and cvtCTOR (Ctor{settings=x350, superArgs=ls352, func=x356}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x350), ("superArgs", PrettyRep.List (List.map (fn x351 => 
                                                                                                         cvtEXPR x351
                                                                                                  ) ls352)), 
          ("func", cvtFUNC x356)]))
   and cvtFUNC (Func{name=x366, fsig=x367, isNative=b368, block=x369, param=x370, 
          defaults=ls372, ty=x376}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x366), ("fsig", cvtFUNC_SIG x367), ("isNative", PrettyRep.Bool b368), 
          ("block", cvtBLOCK x369), ("param", cvtHEAD x370), ("defaults", PrettyRep.List (List.map (fn x371 => 
                                                                                                          cvtEXPR x371
                                                                                                   ) ls372)), 
          ("ty", cvtFUNC_TYPE x376)]))
   and cvtDEFN (ClassDefn x394) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x394))
     | cvtDEFN (VariableDefn x397) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x397))
     | cvtDEFN (FunctionDefn x400) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x400))
     | cvtDEFN (ConstructorDefn x403) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x403))
     | cvtDEFN (InterfaceDefn x406) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x406))
     | cvtDEFN (NamespaceDefn x409) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x409))
     | cvtDEFN (TypeDefn x412) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x412))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls416, params=x420, paramTypes=ls422, 
          defaults=ls427, ctorInits=opt438, returnType=x442, thisType=opt444, 
          hasRest=b448}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x415 => cvtIDENT x415
                                   ) ls416)), ("params", cvtBINDINGS x420), 
          ("paramTypes", PrettyRep.List (List.map (fn x421 => cvtTYPE_EXPR x421
                                                  ) ls422)), ("defaults", PrettyRep.List (List.map (fn x426 => 
                                                                                                          cvtEXPR x426
                                                                                                   ) ls427)), 
          ("ctorInits", 
       (case opt438 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x431, ls433) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x431, 
            PrettyRep.List (List.map (fn x432 => cvtEXPR x432
                                     ) ls433)]))
       )), ("returnType", cvtTYPE_EXPR x442), ("thisType", 
       (case opt444 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x443 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x443))
       )), ("hasRest", PrettyRep.Bool b448)]))
   and cvtBINDING (Binding{ident=x468, ty=x469}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x468), ("ty", cvtTYPE_EXPR x469)]))
   and cvtBINDING_IDENT (TempIdent n477) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n477))
     | cvtBINDING_IDENT (ParamIdent n480) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n480))
     | cvtBINDING_IDENT (PropIdent x483) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x483))
   and cvtINIT_STEP (InitStep(x486, x487)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x486, 
          cvtEXPR x487]))
     | cvtINIT_STEP (AssignStep(x491, x492)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x491, cvtEXPR x492]))
   and cvtTYPE_EXPR (SpecialType x496) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x496))
     | cvtTYPE_EXPR (UnionType ls500) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x499 => 
                                                                                                           cvtTYPE_EXPR x499
                                                                                                    ) ls500)))
     | cvtTYPE_EXPR (ArrayType ls507) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x506 => 
                                                                                                           cvtTYPE_EXPR x506
                                                                                                    ) ls507)))
     | cvtTYPE_EXPR (TypeName x513) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x513))
     | cvtTYPE_EXPR (ElementTypeRef(x516, n517)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x516, PrettyRep.Int n517]))
     | cvtTYPE_EXPR (FieldTypeRef(x521, x522)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x521, cvtIDENT x522]))
     | cvtTYPE_EXPR (FunctionType x526) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x526))
     | cvtTYPE_EXPR (ObjectType ls530) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x529 => 
                                                                                                             cvtFIELD_TYPE x529
                                                                                                      ) ls530)))
     | cvtTYPE_EXPR (AppType{base=x536, args=ls538}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x536), ("args", PrettyRep.List (List.map (fn x537 => 
                                                                                                     cvtTYPE_EXPR x537
                                                                                              ) ls538))]))
     | cvtTYPE_EXPR (NullableType{expr=x549, nullable=b550}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x549), ("nullable", PrettyRep.Bool b550)]))
     | cvtTYPE_EXPR (InstanceType{name=x558, typeParams=ls560, ty=x564, isDynamic=b565}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x558), 
          ("typeParams", PrettyRep.List (List.map (fn x559 => cvtIDENT x559
                                                  ) ls560)), ("ty", cvtTYPE_EXPR x564), 
          ("isDynamic", PrettyRep.Bool b565)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x578) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x578))
     | cvtSTMT (InitStmt{kind=x581, ns=opt583, prototype=b587, static=b588, 
          temps=x589, inits=ls591}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x581), ("ns", 
       (case opt583 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x582 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x582))
       )), ("prototype", PrettyRep.Bool b587), ("static", PrettyRep.Bool b588), 
          ("temps", cvtBINDINGS x589), ("inits", PrettyRep.List (List.map (fn x590 => 
                                                                                 cvtINIT_STEP x590
                                                                          ) ls591))]))
     | cvtSTMT (ClassBlock{ns=opt611, ident=x615, name=opt617, block=x621}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x610 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x610))
       )), ("ident", cvtIDENT x615), ("name", 
       (case opt617 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x616 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x616))
       )), ("block", cvtBLOCK x621)]))
     | cvtSTMT (ForEachStmt x633) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x633))
     | cvtSTMT (ForInStmt x636) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x636))
     | cvtSTMT (ThrowStmt x639) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x639))
     | cvtSTMT (ReturnStmt x642) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x642))
     | cvtSTMT (BreakStmt opt646) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt646 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x645 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x645))
       ))
     | cvtSTMT (ContinueStmt opt653) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt653 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x652 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x652))
       ))
     | cvtSTMT (BlockStmt x659) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x659))
     | cvtSTMT (LabeledStmt(x662, x663)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x662, 
          cvtSTMT x663]))
     | cvtSTMT (LetStmt x667) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x667))
     | cvtSTMT (WhileStmt x670) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x670))
     | cvtSTMT (DoWhileStmt x673) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x673))
     | cvtSTMT (ForStmt x676) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x676))
     | cvtSTMT (IfStmt{cnd=x679, thn=x680, els=x681}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x679), ("thn", cvtSTMT x680), 
          ("els", cvtSTMT x681)]))
     | cvtSTMT (WithStmt{obj=x691, ty=x692, body=x693}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x691), ("ty", cvtTYPE_EXPR x692), 
          ("body", cvtSTMT x693)]))
     | cvtSTMT (TryStmt{block=x703, catches=ls721, finally=opt726}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x703), ("catches", PrettyRep.List (List.map (fn {bindings=x704, 
                                                                                                     ty=x705, 
                                                                                                     fixtures=opt707, 
                                                                                                     block=x711} => 
                                                                                                     PrettyRep.Rec [("bindings", 
                                                                                                     cvtBINDINGS x704), 
                                                                                                     ("ty", 
                                                                                                     cvtTYPE_EXPR x705), 
                                                                                                     ("fixtures", 
                                                                                                     
                                                                                                  (case opt707 of
                                                                                                    NONE => 
                                                                                                       PrettyRep.Ctor ("NONE", 
                                                                                                       NONE)
                                                                                                  | SOME x706 => 
                                                                                                       PrettyRep.Ctor ("SOME", 
                                                                                                       SOME (cvtFIXTURES x706))
                                                                                                  )), 
                                                                                                     ("block", 
                                                                                                     cvtBLOCK x711)]
                                                                                              ) ls721)), 
          ("finally", 
       (case opt726 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x725 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x725))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt740, cond=x744, cases=ls746}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("mode", 
       (case opt740 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x739 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x739))
       )), ("cond", cvtEXPR x744), ("cases", PrettyRep.List (List.map (fn x745 => 
                                                                             cvtCASE x745
                                                                      ) ls746))]))
     | cvtSTMT (SwitchTypeStmt{cond=x759, ty=x760, cases=ls762}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x759), ("ty", cvtTYPE_EXPR x760), 
          ("cases", PrettyRep.List (List.map (fn x761 => cvtTYPE_CASE x761
                                             ) ls762))]))
     | cvtSTMT (Dxns{expr=x775}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x775)]))
   and cvtEXPR (TrinaryExpr(x781, x782, x783, x784)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x781, cvtEXPR x782, cvtEXPR x783, 
          cvtEXPR x784]))
     | cvtEXPR (BinaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryTypeExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x794, cvtEXPR x795, cvtTYPE_EXPR x796]))
     | cvtEXPR (UnaryExpr(x800, x801)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x800, 
          cvtEXPR x801]))
     | cvtEXPR (TypeExpr x805) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x805))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt810) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt810 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x809 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x809))
       ))
     | cvtEXPR (SuperExpr opt817) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt817 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x816 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x816))
       ))
     | cvtEXPR (LiteralExpr x823) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x823))
     | cvtEXPR (CallExpr{func=x826, actuals=ls828}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x826), ("actuals", PrettyRep.List (List.map (fn x827 => 
                                                                                                   cvtEXPR x827
                                                                                            ) ls828))]))
     | cvtEXPR (ApplyTypeExpr{expr=x839, actuals=ls841}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x839), ("actuals", PrettyRep.List (List.map (fn x840 => 
                                                                                                   cvtTYPE_EXPR x840
                                                                                            ) ls841))]))
     | cvtEXPR (LetExpr{defs=x852, body=x853, head=opt855}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x852), ("body", cvtEXPR x853), 
          ("head", 
       (case opt855 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x854 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x854))
       ))]))
     | cvtEXPR (NewExpr{obj=x868, actuals=ls870}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x868), ("actuals", PrettyRep.List (List.map (fn x869 => 
                                                                                                  cvtEXPR x869
                                                                                           ) ls870))]))
     | cvtEXPR (ObjectRef{base=x881, ident=x882}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x881), ("ident", cvtIDENT_EXPR x882)]))
     | cvtEXPR (LexicalRef{ident=x890}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x890)]))
     | cvtEXPR (SetExpr(x896, x897, x898)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x896, 
          cvtEXPR x897, cvtEXPR x898]))
     | cvtEXPR (ListExpr ls903) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x902 => 
                                                                                                    cvtEXPR x902
                                                                                             ) ls903)))
     | cvtEXPR (InitExpr(x909, x910, x911)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x909, 
          cvtHEAD x910, cvtINITS x911]))
     | cvtEXPR (SliceExpr(x915, x916, x917)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x915, cvtEXPR x916, cvtEXPR x917]))
     | cvtEXPR (GetTemp n921) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n921))
     | cvtEXPR (GetParam n924) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n924))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n930) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n930))
     | cvtFIXTURE_NAME (PropName x933) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x933))
   and cvtIDENT_EXPR (Identifier{ident=x936, openNamespaces=ls942}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x936), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls938 => PrettyRep.List (List.map (fn x937 => 
                                                                                cvtNAMESPACE x937
                                                                         ) ls938)
                                   ) ls942))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x953, expr=x954}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x953), ("expr", cvtEXPR x954)]))
     | cvtIDENT_EXPR (AttributeIdentifier x962) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x962))
     | cvtIDENT_EXPR (ExpressionIdentifier x965) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x965))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x968, ident=x969}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x968), ("ident", cvtUSTRING x969)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x977, typeArgs=ls979}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x977), ("typeArgs", 
          PrettyRep.List (List.map (fn x978 => cvtTYPE_EXPR x978
                                   ) ls979))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls991, x995)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x990 => cvtIDENT x990
                                                          ) ls991), cvtIDENT_EXPR x995]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1001) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1001))
     | cvtLITERAL (LiteralContextualDecimalInteger s1004) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1004))
     | cvtLITERAL (LiteralContextualHexInteger s1007) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1007))
     | cvtLITERAL (LiteralDouble r1010) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1010))
     | cvtLITERAL (LiteralDecimal d1013) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1013))
     | cvtLITERAL (LiteralInt i1016) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1016))
     | cvtLITERAL (LiteralUInt u1019) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1019))
     | cvtLITERAL (LiteralBoolean b1022) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1022))
     | cvtLITERAL (LiteralString x1025) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1025))
     | cvtLITERAL (LiteralArray{exprs=ls1029, ty=opt1034}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1028 => 
                                                                         cvtEXPR x1028
                                                                  ) ls1029)), 
          ("ty", 
       (case opt1034 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1033 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1033))
       ))]))
     | cvtLITERAL (LiteralXML ls1046) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1045 => 
                                                                                                            cvtEXPR x1045
                                                                                                     ) ls1046)))
     | cvtLITERAL (LiteralNamespace x1052) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1052))
     | cvtLITERAL (LiteralObject{expr=ls1056, ty=opt1061}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1055 => 
                                                                        cvtFIELD x1055
                                                                 ) ls1056)), 
          ("ty", 
       (case opt1061 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1060 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1060))
       ))]))
     | cvtLITERAL (LiteralFunction x1072) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1072))
     | cvtLITERAL (LiteralRegExp{str=x1075}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1075)]))
   and cvtBLOCK (Block x1081) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1081))
   and cvtFIXTURE (NamespaceFixture x1084) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1084))
     | cvtFIXTURE (ClassFixture x1087) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1087))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1091) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1091))
     | cvtFIXTURE (MethodFixture{func=x1094, ty=x1095, readOnly=b1096, override=b1097, 
          final=b1098}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1094), ("ty", cvtTYPE_EXPR x1095), ("readOnly", PrettyRep.Bool b1096), 
          ("override", PrettyRep.Bool b1097), ("final", PrettyRep.Bool b1098)]))
     | cvtFIXTURE (ValFixture{ty=x1112, readOnly=b1113}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1112), ("readOnly", PrettyRep.Bool b1113)]))
     | cvtFIXTURE (VirtualValFixture x1121) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1121))
   and cvtBINDINGS (ls1125, ls1130) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1124 => 
                                                                                       cvtBINDING x1124
                                                                                ) ls1125), 
          PrettyRep.List (List.map (fn x1129 => cvtINIT_STEP x1129
                                   ) ls1130)]
   and cvtFIXTURES ls1138 = PrettyRep.List (List.map (fn (x1135, x1136) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1135, 
                                                            cvtFIXTURE x1136]
                                                     ) ls1138)
   and cvtINITS ls1145 = PrettyRep.List (List.map (fn (x1142, x1143) => PrettyRep.Tuple [cvtFIXTURE_NAME x1142, 
                                                         cvtEXPR x1143]
                                                  ) ls1145)
   and cvtHEAD (x1149, x1150) = PrettyRep.Tuple [cvtFIXTURES x1149, cvtINITS x1150]
   and cvtFIELD {kind=x1152, name=x1153, init=x1154} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1152), ("name", cvtIDENT_EXPR x1153), ("init", cvtEXPR x1154)]
   and cvtFIELD_TYPE {name=x1162, ty=x1163} = PrettyRep.Rec [("name", cvtIDENT x1162), 
          ("ty", cvtTYPE_EXPR x1163)]
   and cvtTYPED_IDENT {name=x1169, ty=opt1171} = PrettyRep.Rec [("name", cvtIDENT x1169), 
          ("ty", 
       (case opt1171 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1170 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1170))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1181, params=ls1186, result=x1190, thisType=opt1192, 
          hasRest=b1196, minArgs=n1197} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1180 => 
                                                                                                        cvtIDENT x1180
                                                                                                 ) ls1181)), 
          ("params", PrettyRep.List (List.map (fn x1185 => cvtTYPE_EXPR x1185
                                              ) ls1186)), ("result", cvtTYPE_EXPR x1190), 
          ("thisType", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1191))
       )), ("hasRest", PrettyRep.Bool b1196), ("minArgs", PrettyRep.Int n1197)]
   and cvtFUNC_DEFN {kind=x1211, ns=opt1213, final=b1217, override=b1218, prototype=b1219, 
          static=b1220, func=x1221} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1211), 
          ("ns", 
       (case opt1213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1212 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1212))
       )), ("final", PrettyRep.Bool b1217), ("override", PrettyRep.Bool b1218), 
          ("prototype", PrettyRep.Bool b1219), ("static", PrettyRep.Bool b1220), 
          ("func", cvtFUNC x1221)]
   and cvtCTOR_DEFN x1237 = cvtCTOR x1237
   and cvtVAR_DEFN {kind=x1238, ns=opt1240, static=b1244, prototype=b1245, 
          bindings=x1246} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1238), 
          ("ns", 
       (case opt1240 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1239 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1239))
       )), ("static", PrettyRep.Bool b1244), ("prototype", PrettyRep.Bool b1245), 
          ("bindings", cvtBINDINGS x1246)]
   and cvtNAMESPACE_DEFN {ident=x1258, ns=opt1260, init=opt1265} = PrettyRep.Rec [("ident", 
          cvtIDENT x1258), ("ns", 
       (case opt1260 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1259 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1259))
       )), ("init", 
       (case opt1265 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1264 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1264))
       ))]
   and cvtCLASS_DEFN {ident=x1276, ns=opt1278, nonnullable=b1282, dynamic=b1283, 
          final=b1284, params=ls1286, extends=opt1291, implements=ls1296, classDefns=ls1301, 
          instanceDefns=ls1306, instanceStmts=ls1311, ctorDefn=opt1316} = PrettyRep.Rec [("ident", 
          cvtIDENT x1276), ("ns", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1277))
       )), ("nonnullable", PrettyRep.Bool b1282), ("dynamic", PrettyRep.Bool b1283), 
          ("final", PrettyRep.Bool b1284), ("params", PrettyRep.List (List.map (fn x1285 => 
                                                                                      cvtIDENT x1285
                                                                               ) ls1286)), 
          ("extends", 
       (case opt1291 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1290 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1290))
       )), ("implements", PrettyRep.List (List.map (fn x1295 => cvtIDENT_EXPR x1295
                                                   ) ls1296)), ("classDefns", 
          PrettyRep.List (List.map (fn x1300 => cvtDEFN x1300
                                   ) ls1301)), ("instanceDefns", PrettyRep.List (List.map (fn x1305 => 
                                                                                                 cvtDEFN x1305
                                                                                          ) ls1306)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1310 => cvtSTMT x1310
                                                     ) ls1311)), ("ctorDefn", 
          
       (case opt1316 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1315 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1315))
       ))]
   and cvtINTERFACE_DEFN {ident=x1345, ns=opt1347, nonnullable=b1351, params=ls1353, 
          extends=ls1358, block=x1362} = PrettyRep.Rec [("ident", cvtIDENT x1345), 
          ("ns", 
       (case opt1347 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1346 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1346))
       )), ("nonnullable", PrettyRep.Bool b1351), ("params", PrettyRep.List (List.map (fn x1352 => 
                                                                                             cvtIDENT x1352
                                                                                      ) ls1353)), 
          ("extends", PrettyRep.List (List.map (fn x1357 => cvtIDENT_EXPR x1357
                                               ) ls1358)), ("block", cvtBLOCK x1362)]
   and cvtTYPE_DEFN {ident=x1376, ns=opt1378, init=x1382} = PrettyRep.Rec [("ident", 
          cvtIDENT x1376), ("ns", 
       (case opt1378 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1377 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1377))
       )), ("init", cvtTYPE_EXPR x1382)]
   and cvtFOR_ENUM_STMT {defn=opt1391, obj=x1395, fixtures=opt1397, inits=opt1402, 
          labels=ls1407, body=x1411} = PrettyRep.Rec [("defn", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1390))
       )), ("obj", cvtEXPR x1395), ("fixtures", 
       (case opt1397 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1396 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1396))
       )), ("inits", 
       (case opt1402 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1401 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1401))
       )), ("labels", PrettyRep.List (List.map (fn x1406 => cvtIDENT x1406
                                               ) ls1407)), ("body", cvtSTMT x1411)]
   and cvtFOR_STMT {fixtures=opt1426, defn=opt1431, init=x1435, cond=x1436, 
          update=x1437, labels=ls1439, body=x1443} = PrettyRep.Rec [("fixtures", 
          
       (case opt1426 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1425 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1425))
       )), ("defn", 
       (case opt1431 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1430 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1430))
       )), ("init", cvtSTMT x1435), ("cond", cvtEXPR x1436), ("update", cvtEXPR x1437), 
          ("labels", PrettyRep.List (List.map (fn x1438 => cvtIDENT x1438
                                              ) ls1439)), ("body", cvtSTMT x1443)]
   and cvtWHILE_STMT {cond=x1459, fixtures=opt1461, body=x1465, labels=ls1467} = 
          PrettyRep.Rec [("cond", cvtEXPR x1459), ("fixtures", 
       (case opt1461 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1460 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1460))
       )), ("body", cvtSTMT x1465), ("labels", PrettyRep.List (List.map (fn x1466 => 
                                                                               cvtIDENT x1466
                                                                        ) ls1467))]
   and cvtDIRECTIVES {pragmas=ls1481, defns=ls1486, head=opt1491, body=ls1496, 
          pos=opt1501} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1480 => 
                                                                                    cvtPRAGMA x1480
                                                                             ) ls1481)), 
          ("defns", PrettyRep.List (List.map (fn x1485 => cvtDEFN x1485
                                             ) ls1486)), ("head", 
       (case opt1491 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1490 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1490))
       )), ("body", PrettyRep.List (List.map (fn x1495 => cvtSTMT x1495
                                             ) ls1496)), ("pos", 
       (case opt1501 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1500 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1500))
       ))]
   and cvtCASE {label=opt1517, inits=opt1522, body=x1526} = PrettyRep.Rec [("label", 
          
       (case opt1517 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1516 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1516))
       )), ("inits", 
       (case opt1522 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1521 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1521))
       )), ("body", cvtBLOCK x1526)]
   and cvtTYPE_CASE {ty=opt1535, bindings=x1539, inits=opt1541, body=x1545} = 
          PrettyRep.Rec [("ty", 
       (case opt1535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1534 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1534))
       )), ("bindings", cvtBINDINGS x1539), ("inits", 
       (case opt1541 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1540 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1540))
       )), ("body", cvtBLOCK x1545)]
   and cvtFUNC_NAME {kind=x1555, ident=x1556} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1555), 
          ("ident", cvtIDENT x1556)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1562, getter=opt1564, setter=opt1569} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1562), ("getter", 
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1563))
       )), ("setter", 
       (case opt1569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1568))
       ))]
   and cvtPACKAGE {name=ls1581, block=x1585} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1580 => 
                                                                                                       cvtIDENT x1580
                                                                                                ) ls1581)), 
          ("block", cvtBLOCK x1585)]
   and cvtPROGRAM {packages=ls1592, fixtures=opt1597, block=x1601} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1591 => cvtPACKAGE x1591
                                   ) ls1592)), ("fixtures", 
       (case opt1597 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1596 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1596))
       )), ("block", cvtBLOCK x1601)]
end

