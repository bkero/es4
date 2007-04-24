structure PrettyCvt = struct
   open Ast
   fun cvtPOS {file=s0, span=s1, sm=s2, post_newline=b3} = PrettyRep.Rec [("file", 
          PrettyRep.String s0), ("span", PrettyRep.StrmPosSpan s1), ("sm", 
          PrettyRep.StrmPosSM s2), ("post_newline", PrettyRep.Bool b3)]
   and cvtUSTRING s13 = PrettyRep.String s13
   and cvtIDENT x14 = cvtUSTRING x14
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x17) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x17))
     | cvtNAMESPACE (Protected x20) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x20))
     | cvtNAMESPACE (Public x23) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x23))
     | cvtNAMESPACE (Internal x26) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x26))
     | cvtNAMESPACE (UserNamespace x29) = PrettyRep.Ctor ("UserNamespace", 
          SOME (cvtUSTRING x29))
     | cvtNAMESPACE (AnonUserNamespace n32) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n32))
     | cvtNAMESPACE (LimitedNamespace(x35, x36)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x35, cvtNAMESPACE x36]))
   and cvtNAME {ns=x40, id=x41} = PrettyRep.Rec [("ns", cvtNAMESPACE x40), 
          ("id", cvtIDENT x41)]
   and cvtMULTINAME {nss=ls52, id=x56} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls48 => 
                                                                                                PrettyRep.List (List.map (fn x47 => 
                                                                                                                                cvtNAMESPACE x47
                                                                                                                         ) ls48)
                                                                                         ) ls52)), 
          ("id", cvtIDENT x56)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x67, roundingMode=r68, precision=n69} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x67), ("roundingMode", 
          PrettyRep.DecRm r68), ("precision", PrettyRep.Int n69)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt82) = PrettyRep.Ctor ("Plus", SOME 
       (case opt82 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x81 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x81))
       ))
     | cvtBINOP (Minus opt89) = PrettyRep.Ctor ("Minus", SOME 
       (case opt89 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x88 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x88))
       ))
     | cvtBINOP (Times opt96) = PrettyRep.Ctor ("Times", SOME 
       (case opt96 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x95 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x95))
       ))
     | cvtBINOP (Divide opt103) = PrettyRep.Ctor ("Divide", SOME 
       (case opt103 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x102 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x102))
       ))
     | cvtBINOP (Remainder opt110) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt110 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x109 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x109))
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
     | cvtBINOP (Equals opt127) = PrettyRep.Ctor ("Equals", SOME 
       (case opt127 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x126 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x126))
       ))
     | cvtBINOP (NotEquals opt134) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt134 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x133 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x133))
       ))
     | cvtBINOP (StrictEquals opt141) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x140 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x140))
       ))
     | cvtBINOP (StrictNotEquals opt148) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt148 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x147 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x147))
       ))
     | cvtBINOP (Less opt155) = PrettyRep.Ctor ("Less", SOME 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x154))
       ))
     | cvtBINOP (LessOrEqual opt162) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt162 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x161 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x161))
       ))
     | cvtBINOP (Greater opt169) = PrettyRep.Ctor ("Greater", SOME 
       (case opt169 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x168 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x168))
       ))
     | cvtBINOP (GreaterOrEqual opt176) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt176 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x175 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x175))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt185) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt185 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x184 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x184))
       ))
     | cvtASSIGNOP (AssignMinus opt192) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x191 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x191))
       ))
     | cvtASSIGNOP (AssignTimes opt199) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt199 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x198 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x198))
       ))
     | cvtASSIGNOP (AssignDivide opt206) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt206 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x205 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x205))
       ))
     | cvtASSIGNOP (AssignRemainder opt213) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt213 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x212 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x212))
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
     | cvtUNOP (PreIncrement opt231) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt231 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x230 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x230))
       ))
     | cvtUNOP (PreDecrement opt238) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt238 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x237 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x237))
       ))
     | cvtUNOP (PostIncrement opt245) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt245 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x244 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x244))
       ))
     | cvtUNOP (PostDecrement opt252) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt252 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x251 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x251))
       ))
     | cvtUNOP (UnaryPlus opt259) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt259 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x258 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x258))
       ))
     | cvtUNOP (UnaryMinus opt266) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt266 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x265 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x265))
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
   and cvtPRAGMA (UseNamespace x283) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x283))
     | cvtPRAGMA (UseDefaultNamespace x286) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x286))
     | cvtPRAGMA (UseNumber x289) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x289))
     | cvtPRAGMA (UseRounding r292) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r292))
     | cvtPRAGMA (UsePrecision n295) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n295))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=ls301, name=x305, alias=opt307}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x300 => 
                                                                           cvtIDENT x300
                                                                    ) ls301)), 
          ("name", cvtIDENT x305), ("alias", 
       (case opt307 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x306 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x306))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
   and cvtCLS (Cls{name=x326, extends=opt328, implements=ls333, classFixtures=x337, 
          instanceFixtures=x338, instanceInits=x339, constructor=opt341, classType=x345, 
          instanceType=x346}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x326), ("extends", 
       (case opt328 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x327 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x327))
       )), ("implements", PrettyRep.List (List.map (fn x332 => cvtNAME x332
                                                   ) ls333)), ("classFixtures", 
          cvtFIXTURES x337), ("instanceFixtures", cvtFIXTURES x338), ("instanceInits", 
          cvtHEAD x339), ("constructor", 
       (case opt341 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x340 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x340))
       )), ("classType", cvtTYPE_EXPR x345), ("instanceType", cvtTYPE_EXPR x346)]))
   and cvtCTOR (Ctor{settings=x368, superArgs=ls370, func=x374}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x368), ("superArgs", PrettyRep.List (List.map (fn x369 => 
                                                                                                         cvtEXPR x369
                                                                                                  ) ls370)), 
          ("func", cvtFUNC x374)]))
   and cvtFUNC (Func{name=x384, fsig=x385, isNative=b386, block=x387, param=x388, 
          defaults=ls390, ty=x394}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x384), ("fsig", cvtFUNC_SIG x385), ("isNative", PrettyRep.Bool b386), 
          ("block", cvtBLOCK x387), ("param", cvtHEAD x388), ("defaults", PrettyRep.List (List.map (fn x389 => 
                                                                                                          cvtEXPR x389
                                                                                                   ) ls390)), 
          ("ty", cvtFUNC_TYPE x394)]))
   and cvtDEFN (ClassDefn x412) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x412))
     | cvtDEFN (VariableDefn x415) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x415))
     | cvtDEFN (FunctionDefn x418) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x418))
     | cvtDEFN (ConstructorDefn x421) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x421))
     | cvtDEFN (InterfaceDefn x424) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x424))
     | cvtDEFN (NamespaceDefn x427) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x427))
     | cvtDEFN (TypeDefn x430) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x430))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls434, params=x438, paramTypes=ls440, 
          defaults=ls445, ctorInits=opt456, returnType=x460, thisType=opt462, 
          hasRest=b466}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x433 => cvtIDENT x433
                                   ) ls434)), ("params", cvtBINDINGS x438), 
          ("paramTypes", PrettyRep.List (List.map (fn x439 => cvtTYPE_EXPR x439
                                                  ) ls440)), ("defaults", PrettyRep.List (List.map (fn x444 => 
                                                                                                          cvtEXPR x444
                                                                                                   ) ls445)), 
          ("ctorInits", 
       (case opt456 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x449, ls451) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x449, 
            PrettyRep.List (List.map (fn x450 => cvtEXPR x450
                                     ) ls451)]))
       )), ("returnType", cvtTYPE_EXPR x460), ("thisType", 
       (case opt462 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x461 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x461))
       )), ("hasRest", PrettyRep.Bool b466)]))
   and cvtBINDING (Binding{ident=x486, ty=x487}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x486), ("ty", cvtTYPE_EXPR x487)]))
   and cvtBINDING_IDENT (TempIdent n495) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n495))
     | cvtBINDING_IDENT (ParamIdent n498) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n498))
     | cvtBINDING_IDENT (PropIdent x501) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x501))
   and cvtINIT_STEP (InitStep(x504, x505)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x504, 
          cvtEXPR x505]))
     | cvtINIT_STEP (AssignStep(x509, x510)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x509, cvtEXPR x510]))
   and cvtTYPE_EXPR (SpecialType x514) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x514))
     | cvtTYPE_EXPR (UnionType ls518) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x517 => 
                                                                                                           cvtTYPE_EXPR x517
                                                                                                    ) ls518)))
     | cvtTYPE_EXPR (ArrayType ls525) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x524 => 
                                                                                                           cvtTYPE_EXPR x524
                                                                                                    ) ls525)))
     | cvtTYPE_EXPR (TypeName x531) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x531))
     | cvtTYPE_EXPR (ElementTypeRef(x534, n535)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x534, PrettyRep.Int n535]))
     | cvtTYPE_EXPR (FieldTypeRef(x539, x540)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x539, cvtIDENT x540]))
     | cvtTYPE_EXPR (FunctionType x544) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x544))
     | cvtTYPE_EXPR (ObjectType ls548) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x547 => 
                                                                                                             cvtFIELD_TYPE x547
                                                                                                      ) ls548)))
     | cvtTYPE_EXPR (AppType{base=x554, args=ls556}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x554), ("args", PrettyRep.List (List.map (fn x555 => 
                                                                                                     cvtTYPE_EXPR x555
                                                                                              ) ls556))]))
     | cvtTYPE_EXPR (NullableType{expr=x567, nullable=b568}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x567), ("nullable", PrettyRep.Bool b568)]))
     | cvtTYPE_EXPR (InstanceType{name=x576, typeParams=ls578, ty=x582, isDynamic=b583}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x576), 
          ("typeParams", PrettyRep.List (List.map (fn x577 => cvtIDENT x577
                                                  ) ls578)), ("ty", cvtTYPE_EXPR x582), 
          ("isDynamic", PrettyRep.Bool b583)]))
     | cvtTYPE_EXPR (NominalType x595) = PrettyRep.Ctor ("NominalType", SOME (cvtNAME x595))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x599) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x599))
     | cvtSTMT (InitStmt{kind=x602, ns=opt604, prototype=b608, static=b609, 
          temps=x610, inits=ls612}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x602), ("ns", 
       (case opt604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x603 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x603))
       )), ("prototype", PrettyRep.Bool b608), ("static", PrettyRep.Bool b609), 
          ("temps", cvtBINDINGS x610), ("inits", PrettyRep.List (List.map (fn x611 => 
                                                                                 cvtINIT_STEP x611
                                                                          ) ls612))]))
     | cvtSTMT (ClassBlock{ns=opt632, ident=x636, name=opt638, block=x642}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt632 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x631 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x631))
       )), ("ident", cvtIDENT x636), ("name", 
       (case opt638 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x637 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x637))
       )), ("block", cvtBLOCK x642)]))
     | cvtSTMT (ForInStmt x654) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x654))
     | cvtSTMT (ThrowStmt x657) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x657))
     | cvtSTMT (ReturnStmt x660) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x660))
     | cvtSTMT (BreakStmt opt664) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt664 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x663 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x663))
       ))
     | cvtSTMT (ContinueStmt opt671) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt671 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x670 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x670))
       ))
     | cvtSTMT (BlockStmt x677) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x677))
     | cvtSTMT (LabeledStmt(x680, x681)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x680, 
          cvtSTMT x681]))
     | cvtSTMT (LetStmt x685) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x685))
     | cvtSTMT (WhileStmt x688) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x688))
     | cvtSTMT (DoWhileStmt x691) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x691))
     | cvtSTMT (ForStmt x694) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x694))
     | cvtSTMT (IfStmt{cnd=x697, thn=x698, els=x699}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x697), ("thn", cvtSTMT x698), 
          ("els", cvtSTMT x699)]))
     | cvtSTMT (WithStmt{obj=x709, ty=x710, body=x711}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x709), ("ty", cvtTYPE_EXPR x710), 
          ("body", cvtSTMT x711)]))
     | cvtSTMT (TryStmt{block=x721, catches=ls723, finally=opt728}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x721), ("catches", PrettyRep.List (List.map (fn x722 => 
                                                                                                     cvtCATCH_CLAUSE x722
                                                                                              ) ls723)), 
          ("finally", 
       (case opt728 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x727 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x727))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt742, cond=x746, labels=ls748, cases=ls753}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt742 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x741 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x741))
       )), ("cond", cvtEXPR x746), ("labels", PrettyRep.List (List.map (fn x747 => 
                                                                              cvtIDENT x747
                                                                       ) ls748)), 
          ("cases", PrettyRep.List (List.map (fn x752 => cvtCASE x752
                                             ) ls753))]))
     | cvtSTMT (SwitchTypeStmt{cond=x768, ty=x769, cases=ls771}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x768), ("ty", cvtTYPE_EXPR x769), 
          ("cases", PrettyRep.List (List.map (fn x770 => cvtTYPE_CASE x770
                                             ) ls771))]))
     | cvtSTMT (Dxns{expr=x784}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x784)]))
   and cvtEXPR (TernaryExpr(x790, x791, x792, x793)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x790, cvtEXPR x791, cvtEXPR x792, 
          cvtEXPR x793]))
     | cvtEXPR (BinaryExpr(x797, x798, x799)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x797, cvtEXPR x798, cvtEXPR x799]))
     | cvtEXPR (BinaryTypeExpr(x803, x804, x805)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x803, cvtEXPR x804, cvtTYPE_EXPR x805]))
     | cvtEXPR (ExpectedTypeExpr(x809, x810)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x809, cvtEXPR x810]))
     | cvtEXPR (UnaryExpr(x814, x815)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x814, 
          cvtEXPR x815]))
     | cvtEXPR (TypeExpr x819) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x819))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt824) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt824 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x823 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x823))
       ))
     | cvtEXPR (SuperExpr opt831) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt831 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x830 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x830))
       ))
     | cvtEXPR (LiteralExpr x837) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x837))
     | cvtEXPR (CallExpr{func=x840, actuals=ls842}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x840), ("actuals", PrettyRep.List (List.map (fn x841 => 
                                                                                                   cvtEXPR x841
                                                                                            ) ls842))]))
     | cvtEXPR (ApplyTypeExpr{expr=x853, actuals=ls855}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x853), ("actuals", PrettyRep.List (List.map (fn x854 => 
                                                                                                   cvtTYPE_EXPR x854
                                                                                            ) ls855))]))
     | cvtEXPR (LetExpr{defs=x866, body=x867, head=opt869}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x866), ("body", cvtEXPR x867), 
          ("head", 
       (case opt869 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x868 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x868))
       ))]))
     | cvtEXPR (NewExpr{obj=x882, actuals=ls884}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x882), ("actuals", PrettyRep.List (List.map (fn x883 => 
                                                                                                  cvtEXPR x883
                                                                                           ) ls884))]))
     | cvtEXPR (ObjectRef{base=x895, ident=x896, pos=opt898}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x895), ("ident", cvtIDENT_EXPR x896), 
          ("pos", 
       (case opt898 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x897 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x897))
       ))]))
     | cvtEXPR (LexicalRef{ident=x911, pos=opt913}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x911), ("pos", 
       (case opt913 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x912 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x912))
       ))]))
     | cvtEXPR (SetExpr(x924, x925, x926)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x924, 
          cvtEXPR x925, cvtEXPR x926]))
     | cvtEXPR (ListExpr ls931) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x930 => 
                                                                                                    cvtEXPR x930
                                                                                             ) ls931)))
     | cvtEXPR (InitExpr(x937, x938, x939)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x937, 
          cvtHEAD x938, cvtINITS x939]))
     | cvtEXPR (SliceExpr(x943, x944, x945)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x943, cvtEXPR x944, cvtEXPR x945]))
     | cvtEXPR (GetTemp n949) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n949))
     | cvtEXPR (GetParam n952) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n952))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n958) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n958))
     | cvtFIXTURE_NAME (PropName x961) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x961))
   and cvtIDENT_EXPR (Identifier{ident=x964, openNamespaces=ls970}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x964), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls966 => PrettyRep.List (List.map (fn x965 => 
                                                                                cvtNAMESPACE x965
                                                                         ) ls966)
                                   ) ls970))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x981, expr=x982}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x981), ("expr", cvtEXPR x982)]))
     | cvtIDENT_EXPR (AttributeIdentifier x990) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x990))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x993, openNamespaces=ls999}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x993), ("openNamespaces", PrettyRep.List (List.map (fn ls995 => 
                                                                            PrettyRep.List (List.map (fn x994 => 
                                                                                                            cvtNAMESPACE x994
                                                                                                     ) ls995)
                                                                     ) ls999))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1010, ident=x1011}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1010), ("ident", cvtUSTRING x1011)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1019, typeArgs=ls1021}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1019), ("typeArgs", 
          PrettyRep.List (List.map (fn x1020 => cvtTYPE_EXPR x1020
                                   ) ls1021))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1033, x1037)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1032 => cvtIDENT x1032
                                                          ) ls1033), cvtIDENT_EXPR x1037]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1044) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1044))
     | cvtLITERAL (LiteralContextualDecimalInteger s1047) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1047))
     | cvtLITERAL (LiteralContextualHexInteger s1050) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1050))
     | cvtLITERAL (LiteralDouble r1053) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1053))
     | cvtLITERAL (LiteralDecimal d1056) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1056))
     | cvtLITERAL (LiteralInt i1059) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1059))
     | cvtLITERAL (LiteralUInt u1062) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1062))
     | cvtLITERAL (LiteralBoolean b1065) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1065))
     | cvtLITERAL (LiteralString x1068) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1068))
     | cvtLITERAL (LiteralArray{exprs=ls1072, ty=opt1077}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1071 => 
                                                                         cvtEXPR x1071
                                                                  ) ls1072)), 
          ("ty", 
       (case opt1077 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1076 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1076))
       ))]))
     | cvtLITERAL (LiteralXML ls1089) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1088 => 
                                                                                                            cvtEXPR x1088
                                                                                                     ) ls1089)))
     | cvtLITERAL (LiteralNamespace x1095) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1095))
     | cvtLITERAL (LiteralObject{expr=ls1099, ty=opt1104}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1098 => 
                                                                        cvtFIELD x1098
                                                                 ) ls1099)), 
          ("ty", 
       (case opt1104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1103 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1103))
       ))]))
     | cvtLITERAL (LiteralFunction x1115) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1115))
     | cvtLITERAL (LiteralRegExp{str=x1118}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1118)]))
   and cvtBLOCK (Block x1124) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1124))
   and cvtFIXTURE (NamespaceFixture x1127) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1127))
     | cvtFIXTURE (ClassFixture x1130) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1130))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1135) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1135))
     | cvtFIXTURE (MethodFixture{func=x1138, ty=x1139, readOnly=b1140, override=b1141, 
          final=b1142}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1138), ("ty", cvtTYPE_EXPR x1139), ("readOnly", PrettyRep.Bool b1140), 
          ("override", PrettyRep.Bool b1141), ("final", PrettyRep.Bool b1142)]))
     | cvtFIXTURE (ValFixture{ty=x1156, readOnly=b1157}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1156), ("readOnly", PrettyRep.Bool b1157)]))
     | cvtFIXTURE (VirtualValFixture x1165) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1165))
   and cvtBINDINGS (ls1169, ls1174) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1168 => 
                                                                                       cvtBINDING x1168
                                                                                ) ls1169), 
          PrettyRep.List (List.map (fn x1173 => cvtINIT_STEP x1173
                                   ) ls1174)]
   and cvtFIXTURES ls1182 = PrettyRep.List (List.map (fn (x1179, x1180) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1179, 
                                                            cvtFIXTURE x1180]
                                                     ) ls1182)
   and cvtINITS ls1189 = PrettyRep.List (List.map (fn (x1186, x1187) => PrettyRep.Tuple [cvtFIXTURE_NAME x1186, 
                                                         cvtEXPR x1187]
                                                  ) ls1189)
   and cvtHEAD (x1193, x1194) = PrettyRep.Tuple [cvtFIXTURES x1193, cvtINITS x1194]
   and cvtFIELD {kind=x1196, name=x1197, init=x1198} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1196), ("name", cvtIDENT_EXPR x1197), ("init", cvtEXPR x1198)]
   and cvtFIELD_TYPE {name=x1206, ty=x1207} = PrettyRep.Rec [("name", cvtIDENT x1206), 
          ("ty", cvtTYPE_EXPR x1207)]
   and cvtTYPED_IDENT {name=x1213, ty=opt1215} = PrettyRep.Rec [("name", cvtIDENT x1213), 
          ("ty", 
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1214 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1214))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1225, params=ls1230, result=x1234, thisType=opt1236, 
          hasRest=b1240, minArgs=n1241} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1224 => 
                                                                                                        cvtIDENT x1224
                                                                                                 ) ls1225)), 
          ("params", PrettyRep.List (List.map (fn x1229 => cvtTYPE_EXPR x1229
                                              ) ls1230)), ("result", cvtTYPE_EXPR x1234), 
          ("thisType", 
       (case opt1236 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1235 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1235))
       )), ("hasRest", PrettyRep.Bool b1240), ("minArgs", PrettyRep.Int n1241)]
   and cvtFUNC_DEFN {kind=x1255, ns=opt1257, final=b1261, override=b1262, prototype=b1263, 
          static=b1264, func=x1265} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1255), 
          ("ns", 
       (case opt1257 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1256 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1256))
       )), ("final", PrettyRep.Bool b1261), ("override", PrettyRep.Bool b1262), 
          ("prototype", PrettyRep.Bool b1263), ("static", PrettyRep.Bool b1264), 
          ("func", cvtFUNC x1265)]
   and cvtCTOR_DEFN x1281 = cvtCTOR x1281
   and cvtVAR_DEFN {kind=x1282, ns=opt1284, static=b1288, prototype=b1289, 
          bindings=x1290} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1282), 
          ("ns", 
       (case opt1284 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1283 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1283))
       )), ("static", PrettyRep.Bool b1288), ("prototype", PrettyRep.Bool b1289), 
          ("bindings", cvtBINDINGS x1290)]
   and cvtNAMESPACE_DEFN {ident=x1302, ns=opt1304, init=opt1309} = PrettyRep.Rec [("ident", 
          cvtIDENT x1302), ("ns", 
       (case opt1304 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1303 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1303))
       )), ("init", 
       (case opt1309 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1308))
       ))]
   and cvtCLASS_DEFN {ident=x1320, ns=opt1322, nonnullable=b1326, dynamic=b1327, 
          final=b1328, params=ls1330, extends=opt1335, implements=ls1340, classDefns=ls1345, 
          instanceDefns=ls1350, instanceStmts=ls1355, ctorDefn=opt1360} = PrettyRep.Rec [("ident", 
          cvtIDENT x1320), ("ns", 
       (case opt1322 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1321 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1321))
       )), ("nonnullable", PrettyRep.Bool b1326), ("dynamic", PrettyRep.Bool b1327), 
          ("final", PrettyRep.Bool b1328), ("params", PrettyRep.List (List.map (fn x1329 => 
                                                                                      cvtIDENT x1329
                                                                               ) ls1330)), 
          ("extends", 
       (case opt1335 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1334 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1334))
       )), ("implements", PrettyRep.List (List.map (fn x1339 => cvtIDENT_EXPR x1339
                                                   ) ls1340)), ("classDefns", 
          PrettyRep.List (List.map (fn x1344 => cvtDEFN x1344
                                   ) ls1345)), ("instanceDefns", PrettyRep.List (List.map (fn x1349 => 
                                                                                                 cvtDEFN x1349
                                                                                          ) ls1350)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1354 => cvtSTMT x1354
                                                     ) ls1355)), ("ctorDefn", 
          
       (case opt1360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1359 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1359))
       ))]
   and cvtINTERFACE_DEFN {ident=x1389, ns=opt1391, nonnullable=b1395, params=ls1397, 
          extends=ls1402, block=x1406} = PrettyRep.Rec [("ident", cvtIDENT x1389), 
          ("ns", 
       (case opt1391 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1390 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1390))
       )), ("nonnullable", PrettyRep.Bool b1395), ("params", PrettyRep.List (List.map (fn x1396 => 
                                                                                             cvtIDENT x1396
                                                                                      ) ls1397)), 
          ("extends", PrettyRep.List (List.map (fn x1401 => cvtIDENT_EXPR x1401
                                               ) ls1402)), ("block", cvtBLOCK x1406)]
   and cvtTYPE_DEFN {ident=x1420, ns=opt1422, init=x1426} = PrettyRep.Rec [("ident", 
          cvtIDENT x1420), ("ns", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1421))
       )), ("init", cvtTYPE_EXPR x1426)]
   and cvtFOR_ENUM_STMT {isEach=b1434, defn=opt1436, obj=x1440, fixtures=opt1442, 
          next=x1446, labels=ls1448, body=x1452} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1434), ("defn", 
       (case opt1436 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1435 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1435))
       )), ("obj", cvtEXPR x1440), ("fixtures", 
       (case opt1442 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1441 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1441))
       )), ("next", cvtSTMT x1446), ("labels", PrettyRep.List (List.map (fn x1447 => 
                                                                               cvtIDENT x1447
                                                                        ) ls1448)), 
          ("body", cvtSTMT x1452)]
   and cvtFOR_STMT {fixtures=opt1469, defn=opt1474, init=ls1479, cond=x1483, 
          update=x1484, labels=ls1486, body=x1490} = PrettyRep.Rec [("fixtures", 
          
       (case opt1469 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1468 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1468))
       )), ("defn", 
       (case opt1474 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1473 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1473))
       )), ("init", PrettyRep.List (List.map (fn x1478 => cvtSTMT x1478
                                             ) ls1479)), ("cond", cvtEXPR x1483), 
          ("update", cvtEXPR x1484), ("labels", PrettyRep.List (List.map (fn x1485 => 
                                                                                cvtIDENT x1485
                                                                         ) ls1486)), 
          ("body", cvtSTMT x1490)]
   and cvtWHILE_STMT {cond=x1506, fixtures=opt1508, body=x1512, labels=ls1514} = 
          PrettyRep.Rec [("cond", cvtEXPR x1506), ("fixtures", 
       (case opt1508 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1507 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1507))
       )), ("body", cvtSTMT x1512), ("labels", PrettyRep.List (List.map (fn x1513 => 
                                                                               cvtIDENT x1513
                                                                        ) ls1514))]
   and cvtDIRECTIVES {pragmas=ls1528, defns=ls1533, head=opt1538, body=ls1543, 
          pos=opt1548} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1527 => 
                                                                                    cvtPRAGMA x1527
                                                                             ) ls1528)), 
          ("defns", PrettyRep.List (List.map (fn x1532 => cvtDEFN x1532
                                             ) ls1533)), ("head", 
       (case opt1538 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1537 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1537))
       )), ("body", PrettyRep.List (List.map (fn x1542 => cvtSTMT x1542
                                             ) ls1543)), ("pos", 
       (case opt1548 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1547 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1547))
       ))]
   and cvtCASE {label=opt1564, inits=opt1569, body=x1573} = PrettyRep.Rec [("label", 
          
       (case opt1564 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1563 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1563))
       )), ("inits", 
       (case opt1569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1568))
       )), ("body", cvtBLOCK x1573)]
   and cvtTYPE_CASE {ty=opt1582, body=x1586} = PrettyRep.Rec [("ty", 
       (case opt1582 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1581 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1581))
       )), ("body", cvtSTMT x1586)]
   and cvtCATCH_CLAUSE {bindings=x1592, ty=x1593, fixtures=opt1595, block=x1599} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1592), ("ty", cvtTYPE_EXPR x1593), 
          ("fixtures", 
       (case opt1595 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1594 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1594))
       )), ("block", cvtBLOCK x1599)]
   and cvtFUNC_NAME {kind=x1609, ident=x1610} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1609), 
          ("ident", cvtIDENT x1610)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1616, getter=opt1618, setter=opt1623} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1616), ("getter", 
       (case opt1618 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1617 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1617))
       )), ("setter", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1622 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1622))
       ))]
   and cvtPACKAGE {name=ls1635, block=x1639} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1634 => 
                                                                                                       cvtIDENT x1634
                                                                                                ) ls1635)), 
          ("block", cvtBLOCK x1639)]
   and cvtPROGRAM {packages=ls1646, fixtures=opt1651, block=x1655} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1645 => cvtPACKAGE x1645
                                   ) ls1646)), ("fixtures", 
       (case opt1651 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1650 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1650))
       )), ("block", cvtBLOCK x1655)]
end

