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
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt x596) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x596))
     | cvtSTMT (InitStmt{kind=x599, ns=opt601, prototype=b605, static=b606, 
          temps=x607, inits=ls609}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x599), ("ns", 
       (case opt601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x600 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x600))
       )), ("prototype", PrettyRep.Bool b605), ("static", PrettyRep.Bool b606), 
          ("temps", cvtBINDINGS x607), ("inits", PrettyRep.List (List.map (fn x608 => 
                                                                                 cvtINIT_STEP x608
                                                                          ) ls609))]))
     | cvtSTMT (ClassBlock{ns=opt629, ident=x633, name=opt635, block=x639}) = 
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
       (case opt629 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x628 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x628))
       )), ("ident", cvtIDENT x633), ("name", 
       (case opt635 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x634 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x634))
       )), ("block", cvtBLOCK x639)]))
     | cvtSTMT (ForInStmt x651) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x651))
     | cvtSTMT (ThrowStmt x654) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x654))
     | cvtSTMT (ReturnStmt x657) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x657))
     | cvtSTMT (BreakStmt opt661) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt661 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x660 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x660))
       ))
     | cvtSTMT (ContinueStmt opt668) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt668 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x667 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x667))
       ))
     | cvtSTMT (BlockStmt x674) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x674))
     | cvtSTMT (LabeledStmt(x677, x678)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x677, 
          cvtSTMT x678]))
     | cvtSTMT (LetStmt x682) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x682))
     | cvtSTMT (WhileStmt x685) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x685))
     | cvtSTMT (DoWhileStmt x688) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x688))
     | cvtSTMT (ForStmt x691) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x691))
     | cvtSTMT (IfStmt{cnd=x694, thn=x695, els=x696}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x694), ("thn", cvtSTMT x695), 
          ("els", cvtSTMT x696)]))
     | cvtSTMT (WithStmt{obj=x706, ty=x707, body=x708}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x706), ("ty", cvtTYPE_EXPR x707), 
          ("body", cvtSTMT x708)]))
     | cvtSTMT (TryStmt{block=x718, catches=ls720, finally=opt725}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x718), ("catches", PrettyRep.List (List.map (fn x719 => 
                                                                                                     cvtCATCH_CLAUSE x719
                                                                                              ) ls720)), 
          ("finally", 
       (case opt725 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x724 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x724))
       ))]))
     | cvtSTMT (SwitchStmt{mode=opt739, cond=x743, labels=ls745, cases=ls750}) = 
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
       (case opt739 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x738 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x738))
       )), ("cond", cvtEXPR x743), ("labels", PrettyRep.List (List.map (fn x744 => 
                                                                              cvtIDENT x744
                                                                       ) ls745)), 
          ("cases", PrettyRep.List (List.map (fn x749 => cvtCASE x749
                                             ) ls750))]))
     | cvtSTMT (SwitchTypeStmt{cond=x765, ty=x766, cases=ls768}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x765), ("ty", cvtTYPE_EXPR x766), 
          ("cases", PrettyRep.List (List.map (fn x767 => cvtTYPE_CASE x767
                                             ) ls768))]))
     | cvtSTMT (Dxns{expr=x781}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x781)]))
   and cvtEXPR (TernaryExpr(x787, x788, x789, x790)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x787, cvtEXPR x788, cvtEXPR x789, 
          cvtEXPR x790]))
     | cvtEXPR (BinaryExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x794, cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (BinaryTypeExpr(x800, x801, x802)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x800, cvtEXPR x801, cvtTYPE_EXPR x802]))
     | cvtEXPR (ExpectedTypeExpr(x806, x807)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x806, cvtEXPR x807]))
     | cvtEXPR (UnaryExpr(x811, x812)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x811, 
          cvtEXPR x812]))
     | cvtEXPR (TypeExpr x816) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x816))
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
     | cvtEXPR (YieldExpr opt821) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt821 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x820))
       ))
     | cvtEXPR (SuperExpr opt828) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt828 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x827 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x827))
       ))
     | cvtEXPR (LiteralExpr x834) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x834))
     | cvtEXPR (CallExpr{func=x837, actuals=ls839}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x837), ("actuals", PrettyRep.List (List.map (fn x838 => 
                                                                                                   cvtEXPR x838
                                                                                            ) ls839))]))
     | cvtEXPR (ApplyTypeExpr{expr=x850, actuals=ls852}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x850), ("actuals", PrettyRep.List (List.map (fn x851 => 
                                                                                                   cvtTYPE_EXPR x851
                                                                                            ) ls852))]))
     | cvtEXPR (LetExpr{defs=x863, body=x864, head=opt866}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x863), ("body", cvtEXPR x864), 
          ("head", 
       (case opt866 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x865 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x865))
       ))]))
     | cvtEXPR (NewExpr{obj=x879, actuals=ls881}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x879), ("actuals", PrettyRep.List (List.map (fn x880 => 
                                                                                                  cvtEXPR x880
                                                                                           ) ls881))]))
     | cvtEXPR (ObjectRef{base=x892, ident=x893, pos=opt895}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x892), ("ident", cvtIDENT_EXPR x893), 
          ("pos", 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x894))
       ))]))
     | cvtEXPR (LexicalRef{ident=x908, pos=opt910}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x908), ("pos", 
       (case opt910 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x909 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x909))
       ))]))
     | cvtEXPR (SetExpr(x921, x922, x923)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x921, 
          cvtEXPR x922, cvtEXPR x923]))
     | cvtEXPR (ListExpr ls928) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x927 => 
                                                                                                    cvtEXPR x927
                                                                                             ) ls928)))
     | cvtEXPR (InitExpr(x934, x935, x936)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x934, 
          cvtHEAD x935, cvtINITS x936]))
     | cvtEXPR (SliceExpr(x940, x941, x942)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x940, cvtEXPR x941, cvtEXPR x942]))
     | cvtEXPR (GetTemp n946) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n946))
     | cvtEXPR (GetParam n949) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n949))
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
   and cvtFIXTURE_NAME (TempName n955) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n955))
     | cvtFIXTURE_NAME (PropName x958) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x958))
   and cvtIDENT_EXPR (Identifier{ident=x961, openNamespaces=ls967}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x961), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls963 => PrettyRep.List (List.map (fn x962 => 
                                                                                cvtNAMESPACE x962
                                                                         ) ls963)
                                   ) ls967))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x978, expr=x979}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x978), ("expr", cvtEXPR x979)]))
     | cvtIDENT_EXPR (AttributeIdentifier x987) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x987))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x990, openNamespaces=ls996}) = 
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x990), ("openNamespaces", PrettyRep.List (List.map (fn ls992 => 
                                                                            PrettyRep.List (List.map (fn x991 => 
                                                                                                            cvtNAMESPACE x991
                                                                                                     ) ls992)
                                                                     ) ls996))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1007, ident=x1008}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1007), ("ident", cvtUSTRING x1008)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1016, typeArgs=ls1018}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1016), ("typeArgs", 
          PrettyRep.List (List.map (fn x1017 => cvtTYPE_EXPR x1017
                                   ) ls1018))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1030, x1034)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1029 => cvtIDENT x1029
                                                          ) ls1030), cvtIDENT_EXPR x1034]))
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralContextualDecimal s1041) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1041))
     | cvtLITERAL (LiteralContextualDecimalInteger s1044) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1044))
     | cvtLITERAL (LiteralContextualHexInteger s1047) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1047))
     | cvtLITERAL (LiteralDouble r1050) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1050))
     | cvtLITERAL (LiteralDecimal d1053) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1053))
     | cvtLITERAL (LiteralInt i1056) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1056))
     | cvtLITERAL (LiteralUInt u1059) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1059))
     | cvtLITERAL (LiteralBoolean b1062) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1062))
     | cvtLITERAL (LiteralString x1065) = PrettyRep.Ctor ("LiteralString", 
          SOME (cvtUSTRING x1065))
     | cvtLITERAL (LiteralArray{exprs=ls1069, ty=opt1074}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1068 => 
                                                                         cvtEXPR x1068
                                                                  ) ls1069)), 
          ("ty", 
       (case opt1074 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1073))
       ))]))
     | cvtLITERAL (LiteralXML ls1086) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1085 => 
                                                                                                            cvtEXPR x1085
                                                                                                     ) ls1086)))
     | cvtLITERAL (LiteralNamespace x1092) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1092))
     | cvtLITERAL (LiteralObject{expr=ls1096, ty=opt1101}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1095 => 
                                                                        cvtFIELD x1095
                                                                 ) ls1096)), 
          ("ty", 
       (case opt1101 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1100 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1100))
       ))]))
     | cvtLITERAL (LiteralFunction x1112) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1112))
     | cvtLITERAL (LiteralRegExp{str=x1115}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x1115)]))
   and cvtBLOCK (Block x1121) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1121))
   and cvtFIXTURE (NamespaceFixture x1124) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1124))
     | cvtFIXTURE (ClassFixture x1127) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1127))
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x1132) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1132))
     | cvtFIXTURE (MethodFixture{func=x1135, ty=x1136, readOnly=b1137, override=b1138, 
          final=b1139}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1135), ("ty", cvtTYPE_EXPR x1136), ("readOnly", PrettyRep.Bool b1137), 
          ("override", PrettyRep.Bool b1138), ("final", PrettyRep.Bool b1139)]))
     | cvtFIXTURE (ValFixture{ty=x1153, readOnly=b1154}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1153), ("readOnly", PrettyRep.Bool b1154)]))
     | cvtFIXTURE (VirtualValFixture x1162) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1162))
   and cvtBINDINGS (ls1166, ls1171) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1165 => 
                                                                                       cvtBINDING x1165
                                                                                ) ls1166), 
          PrettyRep.List (List.map (fn x1170 => cvtINIT_STEP x1170
                                   ) ls1171)]
   and cvtFIXTURES ls1179 = PrettyRep.List (List.map (fn (x1176, x1177) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1176, 
                                                            cvtFIXTURE x1177]
                                                     ) ls1179)
   and cvtINITS ls1186 = PrettyRep.List (List.map (fn (x1183, x1184) => PrettyRep.Tuple [cvtFIXTURE_NAME x1183, 
                                                         cvtEXPR x1184]
                                                  ) ls1186)
   and cvtHEAD (x1190, x1191) = PrettyRep.Tuple [cvtFIXTURES x1190, cvtINITS x1191]
   and cvtFIELD {kind=x1193, name=x1194, init=x1195} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1193), ("name", cvtIDENT_EXPR x1194), ("init", cvtEXPR x1195)]
   and cvtFIELD_TYPE {name=x1203, ty=x1204} = PrettyRep.Rec [("name", cvtIDENT x1203), 
          ("ty", cvtTYPE_EXPR x1204)]
   and cvtTYPED_IDENT {name=x1210, ty=opt1212} = PrettyRep.Rec [("name", cvtIDENT x1210), 
          ("ty", 
       (case opt1212 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1211 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1211))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1222, params=ls1227, result=x1231, thisType=opt1233, 
          hasRest=b1237, minArgs=n1238} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1221 => 
                                                                                                        cvtIDENT x1221
                                                                                                 ) ls1222)), 
          ("params", PrettyRep.List (List.map (fn x1226 => cvtTYPE_EXPR x1226
                                              ) ls1227)), ("result", cvtTYPE_EXPR x1231), 
          ("thisType", 
       (case opt1233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1232 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1232))
       )), ("hasRest", PrettyRep.Bool b1237), ("minArgs", PrettyRep.Int n1238)]
   and cvtFUNC_DEFN {kind=x1252, ns=opt1254, final=b1258, override=b1259, prototype=b1260, 
          static=b1261, func=x1262} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1252), 
          ("ns", 
       (case opt1254 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1253 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1253))
       )), ("final", PrettyRep.Bool b1258), ("override", PrettyRep.Bool b1259), 
          ("prototype", PrettyRep.Bool b1260), ("static", PrettyRep.Bool b1261), 
          ("func", cvtFUNC x1262)]
   and cvtCTOR_DEFN x1278 = cvtCTOR x1278
   and cvtVAR_DEFN {kind=x1279, ns=opt1281, static=b1285, prototype=b1286, 
          bindings=x1287} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1279), 
          ("ns", 
       (case opt1281 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1280 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1280))
       )), ("static", PrettyRep.Bool b1285), ("prototype", PrettyRep.Bool b1286), 
          ("bindings", cvtBINDINGS x1287)]
   and cvtNAMESPACE_DEFN {ident=x1299, ns=opt1301, init=opt1306} = PrettyRep.Rec [("ident", 
          cvtIDENT x1299), ("ns", 
       (case opt1301 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1300 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1300))
       )), ("init", 
       (case opt1306 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1305 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1305))
       ))]
   and cvtCLASS_DEFN {ident=x1317, ns=opt1319, nonnullable=b1323, dynamic=b1324, 
          final=b1325, params=ls1327, extends=opt1332, implements=ls1337, classDefns=ls1342, 
          instanceDefns=ls1347, instanceStmts=ls1352, ctorDefn=opt1357} = PrettyRep.Rec [("ident", 
          cvtIDENT x1317), ("ns", 
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1318))
       )), ("nonnullable", PrettyRep.Bool b1323), ("dynamic", PrettyRep.Bool b1324), 
          ("final", PrettyRep.Bool b1325), ("params", PrettyRep.List (List.map (fn x1326 => 
                                                                                      cvtIDENT x1326
                                                                               ) ls1327)), 
          ("extends", 
       (case opt1332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1331 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1331))
       )), ("implements", PrettyRep.List (List.map (fn x1336 => cvtIDENT_EXPR x1336
                                                   ) ls1337)), ("classDefns", 
          PrettyRep.List (List.map (fn x1341 => cvtDEFN x1341
                                   ) ls1342)), ("instanceDefns", PrettyRep.List (List.map (fn x1346 => 
                                                                                                 cvtDEFN x1346
                                                                                          ) ls1347)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1351 => cvtSTMT x1351
                                                     ) ls1352)), ("ctorDefn", 
          
       (case opt1357 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1356 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1356))
       ))]
   and cvtINTERFACE_DEFN {ident=x1386, ns=opt1388, nonnullable=b1392, params=ls1394, 
          extends=ls1399, block=x1403} = PrettyRep.Rec [("ident", cvtIDENT x1386), 
          ("ns", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1387 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1387))
       )), ("nonnullable", PrettyRep.Bool b1392), ("params", PrettyRep.List (List.map (fn x1393 => 
                                                                                             cvtIDENT x1393
                                                                                      ) ls1394)), 
          ("extends", PrettyRep.List (List.map (fn x1398 => cvtIDENT_EXPR x1398
                                               ) ls1399)), ("block", cvtBLOCK x1403)]
   and cvtTYPE_DEFN {ident=x1417, ns=opt1419, init=x1423} = PrettyRep.Rec [("ident", 
          cvtIDENT x1417), ("ns", 
       (case opt1419 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1418 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1418))
       )), ("init", cvtTYPE_EXPR x1423)]
   and cvtFOR_ENUM_STMT {isEach=b1431, defn=opt1433, obj=x1437, fixtures=opt1439, 
          next=x1443, labels=ls1445, body=x1449} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1431), ("defn", 
       (case opt1433 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1432 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1432))
       )), ("obj", cvtEXPR x1437), ("fixtures", 
       (case opt1439 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1438 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1438))
       )), ("next", cvtSTMT x1443), ("labels", PrettyRep.List (List.map (fn x1444 => 
                                                                               cvtIDENT x1444
                                                                        ) ls1445)), 
          ("body", cvtSTMT x1449)]
   and cvtFOR_STMT {fixtures=opt1466, defn=opt1471, init=ls1476, cond=x1480, 
          update=x1481, labels=ls1483, body=x1487} = PrettyRep.Rec [("fixtures", 
          
       (case opt1466 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1465 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1465))
       )), ("defn", 
       (case opt1471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1470 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1470))
       )), ("init", PrettyRep.List (List.map (fn x1475 => cvtSTMT x1475
                                             ) ls1476)), ("cond", cvtEXPR x1480), 
          ("update", cvtEXPR x1481), ("labels", PrettyRep.List (List.map (fn x1482 => 
                                                                                cvtIDENT x1482
                                                                         ) ls1483)), 
          ("body", cvtSTMT x1487)]
   and cvtWHILE_STMT {cond=x1503, fixtures=opt1505, body=x1509, labels=ls1511} = 
          PrettyRep.Rec [("cond", cvtEXPR x1503), ("fixtures", 
       (case opt1505 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1504 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1504))
       )), ("body", cvtSTMT x1509), ("labels", PrettyRep.List (List.map (fn x1510 => 
                                                                               cvtIDENT x1510
                                                                        ) ls1511))]
   and cvtDIRECTIVES {pragmas=ls1525, defns=ls1530, head=opt1535, body=ls1540, 
          pos=opt1545} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1524 => 
                                                                                    cvtPRAGMA x1524
                                                                             ) ls1525)), 
          ("defns", PrettyRep.List (List.map (fn x1529 => cvtDEFN x1529
                                             ) ls1530)), ("head", 
       (case opt1535 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1534 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1534))
       )), ("body", PrettyRep.List (List.map (fn x1539 => cvtSTMT x1539
                                             ) ls1540)), ("pos", 
       (case opt1545 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1544 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1544))
       ))]
   and cvtCASE {label=opt1561, inits=opt1566, body=x1570} = PrettyRep.Rec [("label", 
          
       (case opt1561 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1560 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1560))
       )), ("inits", 
       (case opt1566 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1565 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1565))
       )), ("body", cvtBLOCK x1570)]
   and cvtTYPE_CASE {ty=opt1579, body=x1583} = PrettyRep.Rec [("ty", 
       (case opt1579 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1578 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1578))
       )), ("body", cvtSTMT x1583)]
   and cvtCATCH_CLAUSE {bindings=x1589, ty=x1590, fixtures=opt1592, block=x1596} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1589), ("ty", cvtTYPE_EXPR x1590), 
          ("fixtures", 
       (case opt1592 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1591 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1591))
       )), ("block", cvtBLOCK x1596)]
   and cvtFUNC_NAME {kind=x1606, ident=x1607} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1606), 
          ("ident", cvtIDENT x1607)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1613, getter=opt1615, setter=opt1620} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1613), ("getter", 
       (case opt1615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1614 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1614))
       )), ("setter", 
       (case opt1620 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1619 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1619))
       ))]
   and cvtPACKAGE {name=ls1632, block=x1636} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1631 => 
                                                                                                       cvtIDENT x1631
                                                                                                ) ls1632)), 
          ("block", cvtBLOCK x1636)]
   and cvtPROGRAM {packages=ls1643, fixtures=opt1648, block=x1652} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1642 => cvtPACKAGE x1642
                                   ) ls1643)), ("fixtures", 
       (case opt1648 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1647 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1647))
       )), ("block", cvtBLOCK x1652)]
end

