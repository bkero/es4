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
     | cvtEXPR (ObjectRef{base=x881, ident=x882, pos=opt884}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x881), ("ident", cvtIDENT_EXPR x882), 
          ("pos", 
       (case opt884 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x883 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x883))
       ))]))
     | cvtEXPR (LexicalRef{ident=x897, pos=opt899}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x897), ("pos", 
       (case opt899 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x898 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x898))
       ))]))
     | cvtEXPR (SetExpr(x910, x911, x912)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x910, 
          cvtEXPR x911, cvtEXPR x912]))
     | cvtEXPR (ListExpr ls917) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x916 => 
                                                                                                    cvtEXPR x916
                                                                                             ) ls917)))
     | cvtEXPR (InitExpr(x923, x924, x925)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x923, 
          cvtHEAD x924, cvtINITS x925]))
     | cvtEXPR (SliceExpr(x929, x930, x931)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x929, cvtEXPR x930, cvtEXPR x931]))
     | cvtEXPR (GetTemp n935) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n935))
     | cvtEXPR (GetParam n938) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n938))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n944) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n944))
     | cvtFIXTURE_NAME (PropName x947) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x947))
   and cvtIDENT_EXPR (Identifier{ident=x950, openNamespaces=ls956}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x950), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls952 => PrettyRep.List (List.map (fn x951 => 
                                                                                cvtNAMESPACE x951
                                                                         ) ls952)
                                   ) ls956))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x967, expr=x968}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x967), ("expr", cvtEXPR x968)]))
     | cvtIDENT_EXPR (AttributeIdentifier x976) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x976))
     | cvtIDENT_EXPR (ExpressionIdentifier x979) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x979))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x982, ident=x983}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x982), ("ident", cvtUSTRING x983)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x991, typeArgs=ls993}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x991), ("typeArgs", 
          PrettyRep.List (List.map (fn x992 => cvtTYPE_EXPR x992
                                   ) ls993))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1005, x1009)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1004 => cvtIDENT x1004
                                                          ) ls1005), cvtIDENT_EXPR x1009]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1015) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1015))
     | cvtLITERAL (LiteralContextualDecimalInteger s1018) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1018))
     | cvtLITERAL (LiteralContextualHexInteger s1021) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1021))
     | cvtLITERAL (LiteralDouble r1024) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1024))
     | cvtLITERAL (LiteralDecimal d1027) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1027))
     | cvtLITERAL (LiteralInt i1030) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1030))
     | cvtLITERAL (LiteralUInt u1033) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1033))
     | cvtLITERAL (LiteralBoolean b1036) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1036))
     | cvtLITERAL (LiteralString x1039) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1039))
     | cvtLITERAL (LiteralArray{exprs=ls1043, ty=opt1048}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1042 => 
                                                                         cvtEXPR x1042
                                                                  ) ls1043)), 
          ("ty", 
       (case opt1048 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1047 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1047))
       ))]))
     | cvtLITERAL (LiteralXML ls1060) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1059 => 
                                                                                                            cvtEXPR x1059
                                                                                                     ) ls1060)))
     | cvtLITERAL (LiteralNamespace x1066) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1066))
     | cvtLITERAL (LiteralObject{expr=ls1070, ty=opt1075}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1069 => 
                                                                        cvtFIELD x1069
                                                                 ) ls1070)), 
          ("ty", 
       (case opt1075 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1074 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1074))
       ))]))
     | cvtLITERAL (LiteralFunction x1086) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1086))
     | cvtLITERAL (LiteralRegExp{str=x1089}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1089)]))
   and cvtBLOCK (Block x1095) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1095))
   and cvtFIXTURE (NamespaceFixture x1098) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1098))
     | cvtFIXTURE (ClassFixture x1101) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1101))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1105) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1105))
     | cvtFIXTURE (MethodFixture{func=x1108, ty=x1109, readOnly=b1110, override=b1111, 
          final=b1112}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1108), ("ty", cvtTYPE_EXPR x1109), ("readOnly", PrettyRep.Bool b1110), 
          ("override", PrettyRep.Bool b1111), ("final", PrettyRep.Bool b1112)]))
     | cvtFIXTURE (ValFixture{ty=x1126, readOnly=b1127}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1126), ("readOnly", PrettyRep.Bool b1127)]))
     | cvtFIXTURE (VirtualValFixture x1135) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1135))
   and cvtBINDINGS (ls1139, ls1144) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1138 => 
                                                                                       cvtBINDING x1138
                                                                                ) ls1139), 
          PrettyRep.List (List.map (fn x1143 => cvtINIT_STEP x1143
                                   ) ls1144)]
   and cvtFIXTURES ls1152 = PrettyRep.List (List.map (fn (x1149, x1150) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1149, 
                                                            cvtFIXTURE x1150]
                                                     ) ls1152)
   and cvtINITS ls1159 = PrettyRep.List (List.map (fn (x1156, x1157) => PrettyRep.Tuple [cvtFIXTURE_NAME x1156, 
                                                         cvtEXPR x1157]
                                                  ) ls1159)
   and cvtHEAD (x1163, x1164) = PrettyRep.Tuple [cvtFIXTURES x1163, cvtINITS x1164]
   and cvtFIELD {kind=x1166, name=x1167, init=x1168} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1166), ("name", cvtIDENT_EXPR x1167), ("init", cvtEXPR x1168)]
   and cvtFIELD_TYPE {name=x1176, ty=x1177} = PrettyRep.Rec [("name", cvtIDENT x1176), 
          ("ty", cvtTYPE_EXPR x1177)]
   and cvtTYPED_IDENT {name=x1183, ty=opt1185} = PrettyRep.Rec [("name", cvtIDENT x1183), 
          ("ty", 
       (case opt1185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1184 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1184))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1195, params=ls1200, result=x1204, thisType=opt1206, 
          hasRest=b1210, minArgs=n1211} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1194 => 
                                                                                                        cvtIDENT x1194
                                                                                                 ) ls1195)), 
          ("params", PrettyRep.List (List.map (fn x1199 => cvtTYPE_EXPR x1199
                                              ) ls1200)), ("result", cvtTYPE_EXPR x1204), 
          ("thisType", 
       (case opt1206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1205 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1205))
       )), ("hasRest", PrettyRep.Bool b1210), ("minArgs", PrettyRep.Int n1211)]
   and cvtFUNC_DEFN {kind=x1225, ns=opt1227, final=b1231, override=b1232, prototype=b1233, 
          static=b1234, func=x1235} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1225), 
          ("ns", 
       (case opt1227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1226 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1226))
       )), ("final", PrettyRep.Bool b1231), ("override", PrettyRep.Bool b1232), 
          ("prototype", PrettyRep.Bool b1233), ("static", PrettyRep.Bool b1234), 
          ("func", cvtFUNC x1235)]
   and cvtCTOR_DEFN x1251 = cvtCTOR x1251
   and cvtVAR_DEFN {kind=x1252, ns=opt1254, static=b1258, prototype=b1259, 
          bindings=x1260} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1252), 
          ("ns", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       )), ("static", PrettyRep.Bool b1258), ("prototype", PrettyRep.Bool b1259), 
          ("bindings", cvtBINDINGS x1260)]
   and cvtNAMESPACE_DEFN {ident=x1272, ns=opt1274, init=opt1279} = PrettyRep.Rec [("ident", 
          cvtIDENT x1272), ("ns", 
       (case opt1274 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1273 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1273))
       )), ("init", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1278))
       ))]
   and cvtCLASS_DEFN {ident=x1290, ns=opt1292, nonnullable=b1296, dynamic=b1297, 
          final=b1298, params=ls1300, extends=opt1305, implements=ls1310, classDefns=ls1315, 
          instanceDefns=ls1320, instanceStmts=ls1325, ctorDefn=opt1330} = PrettyRep.Rec [("ident", 
          cvtIDENT x1290), ("ns", 
       (case opt1292 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1291 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1291))
       )), ("nonnullable", PrettyRep.Bool b1296), ("dynamic", PrettyRep.Bool b1297), 
          ("final", PrettyRep.Bool b1298), ("params", PrettyRep.List (List.map (fn x1299 => 
                                                                                      cvtIDENT x1299
                                                                               ) ls1300)), 
          ("extends", 
       (case opt1305 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1304 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1304))
       )), ("implements", PrettyRep.List (List.map (fn x1309 => cvtIDENT_EXPR x1309
                                                   ) ls1310)), ("classDefns", 
          PrettyRep.List (List.map (fn x1314 => cvtDEFN x1314
                                   ) ls1315)), ("instanceDefns", PrettyRep.List (List.map (fn x1319 => 
                                                                                                 cvtDEFN x1319
                                                                                          ) ls1320)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1324 => cvtSTMT x1324
                                                     ) ls1325)), ("ctorDefn", 
          
       (case opt1330 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1329 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1329))
       ))]
   and cvtINTERFACE_DEFN {ident=x1359, ns=opt1361, nonnullable=b1365, params=ls1367, 
          extends=ls1372, block=x1376} = PrettyRep.Rec [("ident", cvtIDENT x1359), 
          ("ns", 
       (case opt1361 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1360 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1360))
       )), ("nonnullable", PrettyRep.Bool b1365), ("params", PrettyRep.List (List.map (fn x1366 => 
                                                                                             cvtIDENT x1366
                                                                                      ) ls1367)), 
          ("extends", PrettyRep.List (List.map (fn x1371 => cvtIDENT_EXPR x1371
                                               ) ls1372)), ("block", cvtBLOCK x1376)]
   and cvtTYPE_DEFN {ident=x1390, ns=opt1392, init=x1396} = PrettyRep.Rec [("ident", 
          cvtIDENT x1390), ("ns", 
       (case opt1392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1391 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1391))
       )), ("init", cvtTYPE_EXPR x1396)]
   and cvtFOR_ENUM_STMT {defn=opt1405, obj=x1409, fixtures=opt1411, inits=opt1416, 
          labels=ls1421, body=x1425} = PrettyRep.Rec [("defn", 
       (case opt1405 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1404 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1404))
       )), ("obj", cvtEXPR x1409), ("fixtures", 
       (case opt1411 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1410 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1410))
       )), ("inits", 
       (case opt1416 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1415 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1415))
       )), ("labels", PrettyRep.List (List.map (fn x1420 => cvtIDENT x1420
                                               ) ls1421)), ("body", cvtSTMT x1425)]
   and cvtFOR_STMT {fixtures=opt1440, defn=opt1445, init=x1449, cond=x1450, 
          update=x1451, labels=ls1453, body=x1457} = PrettyRep.Rec [("fixtures", 
          
       (case opt1440 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1439 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1439))
       )), ("defn", 
       (case opt1445 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1444 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1444))
       )), ("init", cvtSTMT x1449), ("cond", cvtEXPR x1450), ("update", cvtEXPR x1451), 
          ("labels", PrettyRep.List (List.map (fn x1452 => cvtIDENT x1452
                                              ) ls1453)), ("body", cvtSTMT x1457)]
   and cvtWHILE_STMT {cond=x1473, fixtures=opt1475, body=x1479, labels=ls1481} = 
          PrettyRep.Rec [("cond", cvtEXPR x1473), ("fixtures", 
       (case opt1475 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1474 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1474))
       )), ("body", cvtSTMT x1479), ("labels", PrettyRep.List (List.map (fn x1480 => 
                                                                               cvtIDENT x1480
                                                                        ) ls1481))]
   and cvtDIRECTIVES {pragmas=ls1495, defns=ls1500, head=opt1505, body=ls1510, 
          pos=opt1515} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1494 => 
                                                                                    cvtPRAGMA x1494
                                                                             ) ls1495)), 
          ("defns", PrettyRep.List (List.map (fn x1499 => cvtDEFN x1499
                                             ) ls1500)), ("head", 
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1504 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1504))
       )), ("body", PrettyRep.List (List.map (fn x1509 => cvtSTMT x1509
                                             ) ls1510)), ("pos", 
       (case opt1515 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1514 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1514))
       ))]
   and cvtCASE {label=opt1531, inits=opt1536, body=x1540} = PrettyRep.Rec [("label", 
          
       (case opt1531 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1530 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1530))
       )), ("inits", 
       (case opt1536 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1535 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1535))
       )), ("body", cvtBLOCK x1540)]
   and cvtTYPE_CASE {ty=opt1549, bindings=x1553, inits=opt1555, body=x1559} = 
          PrettyRep.Rec [("ty", 
       (case opt1549 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1548 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1548))
       )), ("bindings", cvtBINDINGS x1553), ("inits", 
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1554))
       )), ("body", cvtBLOCK x1559)]
   and cvtFUNC_NAME {kind=x1569, ident=x1570} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1569), 
          ("ident", cvtIDENT x1570)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1576, getter=opt1578, setter=opt1583} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1576), ("getter", 
       (case opt1578 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1577 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1577))
       )), ("setter", 
       (case opt1583 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1582 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1582))
       ))]
   and cvtPACKAGE {name=ls1595, block=x1599} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1594 => 
                                                                                                       cvtIDENT x1594
                                                                                                ) ls1595)), 
          ("block", cvtBLOCK x1599)]
   and cvtPROGRAM {packages=ls1606, fixtures=opt1611, block=x1615} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1605 => cvtPACKAGE x1605
                                   ) ls1606)), ("fixtures", 
       (case opt1611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1610 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1610))
       )), ("block", cvtBLOCK x1615)]
end

