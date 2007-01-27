structure PrettyCvt = struct
   open Ast
   fun cvtUSTRING s0 = PrettyRep.String s0
   and cvtIDENT x1 = cvtUSTRING x1
   and cvtNAMESPACE (Private) = PrettyRep.Ctor ("Private", NONE)
     | cvtNAMESPACE (Protected) = PrettyRep.Ctor ("Protected", NONE)
     | cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (Public x5) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x5))
     | cvtNAMESPACE (Internal x8) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x8))
     | cvtNAMESPACE (UserDefined x11) = PrettyRep.Ctor ("UserDefined", SOME (cvtIDENT x11))
   and cvtNAME {ns=x14, id=x15} = PrettyRep.Rec [("ns", cvtNAMESPACE x14), 
          ("id", cvtIDENT x15)]
   and cvtMULTINAME {nss=ls22, id=x26} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn x21 => 
                                                                                                cvtNAMESPACE x21
                                                                                         ) ls22)), 
          ("id", cvtIDENT x26)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtROUNDING_MODE (Ceiling) = PrettyRep.Ctor ("Ceiling", NONE)
     | cvtROUNDING_MODE (Floor) = PrettyRep.Ctor ("Floor", NONE)
     | cvtROUNDING_MODE (Up) = PrettyRep.Ctor ("Up", NONE)
     | cvtROUNDING_MODE (Down) = PrettyRep.Ctor ("Down", NONE)
     | cvtROUNDING_MODE (HalfUp) = PrettyRep.Ctor ("HalfUp", NONE)
     | cvtROUNDING_MODE (HalfDown) = PrettyRep.Ctor ("HalfDown", NONE)
     | cvtROUNDING_MODE (HalfEven) = PrettyRep.Ctor ("HalfEven", NONE)
   and cvtNUMERIC_MODE {numberType=x44, roundingMode=x45, precision=n46} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x44), ("roundingMode", 
          cvtROUNDING_MODE x45), ("precision", PrettyRep.Int n46)]
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
   and cvtBINOP (Plus opt59) = PrettyRep.Ctor ("Plus", SOME 
       (case opt59 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x58 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x58))
       ))
     | cvtBINOP (Minus opt66) = PrettyRep.Ctor ("Minus", SOME 
       (case opt66 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x65 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x65))
       ))
     | cvtBINOP (Times opt73) = PrettyRep.Ctor ("Times", SOME 
       (case opt73 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x72 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x72))
       ))
     | cvtBINOP (Divide opt80) = PrettyRep.Ctor ("Divide", SOME 
       (case opt80 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x79 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x79))
       ))
     | cvtBINOP (Remainder opt87) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt87 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
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
     | cvtBINOP (Equals opt104) = PrettyRep.Ctor ("Equals", SOME 
       (case opt104 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x103 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x103))
       ))
     | cvtBINOP (NotEquals opt111) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt111 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x110 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x110))
       ))
     | cvtBINOP (StrictEquals opt118) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt118 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x117 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x117))
       ))
     | cvtBINOP (StrictNotEquals opt125) = PrettyRep.Ctor ("StrictNotEquals", 
          SOME 
       (case opt125 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x124 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x124))
       ))
     | cvtBINOP (Less opt132) = PrettyRep.Ctor ("Less", SOME 
       (case opt132 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
       ))
     | cvtBINOP (LessOrEqual opt139) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt139 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
       ))
     | cvtBINOP (Greater opt146) = PrettyRep.Ctor ("Greater", SOME 
       (case opt146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
       ))
     | cvtBINOP (GreaterOrEqual opt153) = PrettyRep.Ctor ("GreaterOrEqual", 
          SOME 
       (case opt153 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
     | cvtASSIGNOP (AssignPlus opt163) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt163 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x162 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x162))
       ))
     | cvtASSIGNOP (AssignMinus opt170) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x169 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x169))
       ))
     | cvtASSIGNOP (AssignTimes opt177) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt177 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x176 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x176))
       ))
     | cvtASSIGNOP (AssignDivide opt184) = PrettyRep.Ctor ("AssignDivide", 
          SOME 
       (case opt184 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x183 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x183))
       ))
     | cvtASSIGNOP (AssignRemainder opt191) = PrettyRep.Ctor ("AssignRemainder", 
          SOME 
       (case opt191 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x190 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x190))
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
     | cvtUNOP (MakeNamespace) = PrettyRep.Ctor ("MakeNamespace", NONE)
     | cvtUNOP (Type) = PrettyRep.Ctor ("Type", NONE)
   and cvtNULOP (This) = PrettyRep.Ctor ("This", NONE)
     | cvtNULOP (Empty) = PrettyRep.Ctor ("Empty", NONE)
   and cvtVAR_DEFN_TAG (Const) = PrettyRep.Ctor ("Const", NONE)
     | cvtVAR_DEFN_TAG (Var) = PrettyRep.Ctor ("Var", NONE)
     | cvtVAR_DEFN_TAG (LetVar) = PrettyRep.Ctor ("LetVar", NONE)
     | cvtVAR_DEFN_TAG (LetConst) = PrettyRep.Ctor ("LetConst", NONE)
     | cvtVAR_DEFN_TAG (Rest) = PrettyRep.Ctor ("Rest", NONE)
   and cvtSPECIAL_TY (Any) = PrettyRep.Ctor ("Any", NONE)
     | cvtSPECIAL_TY (Null) = PrettyRep.Ctor ("Null", NONE)
     | cvtSPECIAL_TY (Undefined) = PrettyRep.Ctor ("Undefined", NONE)
     | cvtSPECIAL_TY (VoidType) = PrettyRep.Ctor ("VoidType", NONE)
   and cvtPRAGMA (UseNamespace x229) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x229))
     | cvtPRAGMA (UseDefaultNamespace x232) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x232))
     | cvtPRAGMA (UseNumber x235) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x235))
     | cvtPRAGMA (UseRounding x238) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x238))
     | cvtPRAGMA (UsePrecision x241) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x241))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x246, name=x247, alias=opt249}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x246), ("name", cvtIDENT x247), 
          ("alias", 
       (case opt249 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x248 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x248))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x269, fsig=x270, body=x271, fixtures=opt273}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x269), ("fsig", cvtFUNC_SIG x270), 
          ("body", cvtBLOCK x271), ("fixtures", 
       (case opt273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x272 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x272))
       ))]))
   and cvtDEFN (ClassDefn x288) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x288))
     | cvtDEFN (VariableDefn x291) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x291))
     | cvtDEFN (FunctionDefn x294) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x294))
     | cvtDEFN (InterfaceDefn x297) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x297))
     | cvtDEFN (NamespaceDefn x300) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x300))
     | cvtDEFN (TypeDefn x303) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x303))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls307, params=ls312, inits=opt317, 
          returnType=x321, thisType=opt323, hasBoundThis=b327, hasRest=b328}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x306 => cvtIDENT x306
                                   ) ls307)), ("params", PrettyRep.List (List.map (fn x311 => 
                                                                                         cvtVAR_BINDING x311
                                                                                  ) ls312)), 
          ("inits", 
       (case opt317 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x316 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x316))
       )), ("returnType", cvtTYPE_EXPR x321), ("thisType", 
       (case opt323 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x322 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x322))
       )), ("hasBoundThis", PrettyRep.Bool b327), ("hasRest", PrettyRep.Bool b328)]))
   and cvtVAR_BINDING (Binding{pattern=x346, ty=opt348, init=opt353}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x346), ("ty", 
       (case opt348 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x347 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x347))
       )), ("init", 
       (case opt353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x352 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x352))
       ))]))
   and cvtTYPE_EXPR (SpecialType x366) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x366))
     | cvtTYPE_EXPR (UnionType ls370) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x369 => 
                                                                                                           cvtTYPE_EXPR x369
                                                                                                    ) ls370)))
     | cvtTYPE_EXPR (ArrayType ls377) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x376 => 
                                                                                                           cvtTYPE_EXPR x376
                                                                                                    ) ls377)))
     | cvtTYPE_EXPR (NominalType{ident=x383}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x383)]))
     | cvtTYPE_EXPR (FunctionType x389) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x389))
     | cvtTYPE_EXPR (ObjectType ls393) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x392 => 
                                                                                                             cvtFIELD_TYPE x392
                                                                                                      ) ls393)))
     | cvtTYPE_EXPR (AppType{base=x399, args=ls401}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x399), ("args", PrettyRep.List (List.map (fn x400 => 
                                                                                                     cvtTYPE_EXPR x400
                                                                                              ) ls401))]))
     | cvtTYPE_EXPR (NullableType{expr=x412, nullable=b413}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x412), ("nullable", PrettyRep.Bool b413)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls423) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x422 => 
                                                                                                    cvtEXPR x422
                                                                                             ) ls423)))
     | cvtSTMT (ForEachStmt x429) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x429))
     | cvtSTMT (ForInStmt x432) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x432))
     | cvtSTMT (ThrowStmt ls436) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x435 => 
                                                                                                      cvtEXPR x435
                                                                                               ) ls436)))
     | cvtSTMT (ReturnStmt ls443) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x442 => 
                                                                                                        cvtEXPR x442
                                                                                                 ) ls443)))
     | cvtSTMT (BreakStmt opt450) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt450 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x449 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x449))
       ))
     | cvtSTMT (ContinueStmt opt457) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x456 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x456))
       ))
     | cvtSTMT (BlockStmt x463) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x463))
     | cvtSTMT (LabeledStmt(x466, x467)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x466, 
          cvtSTMT x467]))
     | cvtSTMT (LetStmt(ls472, x476)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x471 => 
                                                                                                                          cvtVAR_BINDING x471
                                                                                                                   ) ls472), 
          cvtSTMT x476]))
     | cvtSTMT (SuperStmt ls481) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x480 => 
                                                                                                      cvtEXPR x480
                                                                                               ) ls481)))
     | cvtSTMT (WhileStmt x487) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x487))
     | cvtSTMT (DoWhileStmt x490) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x490))
     | cvtSTMT (ForStmt{defns=ls494, fixtures=opt499, init=ls504, cond=ls509, 
          update=ls514, contLabel=opt519, body=x523}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x493 => 
                                                                         cvtVAR_BINDING x493
                                                                  ) ls494)), 
          ("fixtures", 
       (case opt499 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x498 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x498))
       )), ("init", PrettyRep.List (List.map (fn x503 => cvtEXPR x503
                                             ) ls504)), ("cond", PrettyRep.List (List.map (fn x508 => 
                                                                                                 cvtEXPR x508
                                                                                          ) ls509)), 
          ("update", PrettyRep.List (List.map (fn x513 => cvtEXPR x513
                                              ) ls514)), ("contLabel", 
       (case opt519 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x518 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x518))
       )), ("body", cvtSTMT x523)]))
     | cvtSTMT (IfStmt{cnd=x541, thn=x542, els=x543}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x541), ("thn", cvtSTMT x542), 
          ("els", cvtSTMT x543)]))
     | cvtSTMT (WithStmt{obj=ls554, ty=x558, body=x559}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x553 => 
                                                                       cvtEXPR x553
                                                                ) ls554)), 
          ("ty", cvtTYPE_EXPR x558), ("body", cvtSTMT x559)]))
     | cvtSTMT (TryStmt{body=x569, catches=ls584, finally=opt589}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x569), ("catches", PrettyRep.List (List.map (fn {bind=x570, 
                                                                                                    fixtures=opt572, 
                                                                                                    body=x576} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x570), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt572 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x571 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x571))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x576)]
                                                                                             ) ls584)), 
          ("finally", 
       (case opt589 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x588 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x588))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls603, cases=ls608}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x602 => 
                                                                        cvtEXPR x602
                                                                 ) ls603)), 
          ("cases", PrettyRep.List (List.map (fn x607 => cvtCASE x607
                                             ) ls608))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls620, ty=x624, cases=ls626}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x619 => 
                                                                        cvtEXPR x619
                                                                 ) ls620)), 
          ("ty", cvtTYPE_EXPR x624), ("cases", PrettyRep.List (List.map (fn x625 => 
                                                                               cvtTYPE_CASE x625
                                                                        ) ls626))]))
     | cvtSTMT (Dxns{expr=x639}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x639)]))
   and cvtEXPR (TrinaryExpr(x645, x646, x647, x648)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x645, cvtEXPR x646, cvtEXPR x647, 
          cvtEXPR x648]))
     | cvtEXPR (BinaryExpr(x652, x653, x654)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x652, cvtEXPR x653, cvtEXPR x654]))
     | cvtEXPR (BinaryTypeExpr(x658, x659, x660)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x658, cvtEXPR x659, cvtTYPE_EXPR x660]))
     | cvtEXPR (UnaryExpr(x664, x665)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x664, 
          cvtEXPR x665]))
     | cvtEXPR (TypeExpr x669) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x669))
     | cvtEXPR (NullaryExpr x672) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x672))
     | cvtEXPR (YieldExpr opt680) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt680 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls676 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x675 => 
                                                                                     cvtEXPR x675
                                                                              ) ls676)))
       ))
     | cvtEXPR (SuperExpr opt687) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt687 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x686 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x686))
       ))
     | cvtEXPR (LiteralExpr x693) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x693))
     | cvtEXPR (CallExpr{func=x696, actuals=ls698}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x696), ("actuals", PrettyRep.List (List.map (fn x697 => 
                                                                                                   cvtEXPR x697
                                                                                            ) ls698))]))
     | cvtEXPR (ApplyTypeExpr{expr=x709, actuals=ls711}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x709), ("actuals", PrettyRep.List (List.map (fn x710 => 
                                                                                                   cvtTYPE_EXPR x710
                                                                                            ) ls711))]))
     | cvtEXPR (LetExpr{defs=ls723, body=ls728, fixtures=opt733}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x722 => 
                                                                        cvtVAR_BINDING x722
                                                                 ) ls723)), 
          ("body", PrettyRep.List (List.map (fn x727 => cvtEXPR x727
                                            ) ls728)), ("fixtures", 
       (case opt733 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x732))
       ))]))
     | cvtEXPR (NewExpr{obj=x746, actuals=ls748}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x746), ("actuals", PrettyRep.List (List.map (fn x747 => 
                                                                                                  cvtEXPR x747
                                                                                           ) ls748))]))
     | cvtEXPR (FunExpr{ident=opt760, fsig=x764, body=x765, fixtures=opt767}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt760 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x759 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x759))
       )), ("fsig", cvtFUNC_SIG x764), ("body", cvtBLOCK x765), ("fixtures", 
          
       (case opt767 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x766 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x766))
       ))]))
     | cvtEXPR (ObjectRef{base=x782, ident=x783}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x782), ("ident", cvtIDENT_EXPR x783)]))
     | cvtEXPR (LexicalRef{ident=x791}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x791)]))
     | cvtEXPR (SetExpr(x797, x798, x799)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x797, 
          cvtPATTERN x798, cvtEXPR x799]))
     | cvtEXPR (ListExpr ls804) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x803 => 
                                                                                                    cvtEXPR x803
                                                                                             ) ls804)))
     | cvtEXPR (SliceExpr(ls811, ls816, ls821)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x810 => cvtEXPR x810
                                                          ) ls811), PrettyRep.List (List.map (fn x815 => 
                                                                                                    cvtEXPR x815
                                                                                             ) ls816), 
          PrettyRep.List (List.map (fn x820 => cvtEXPR x820
                                   ) ls821)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x828, ident=x829}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x828), ("ident", cvtUSTRING x829)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x837, expr=x838}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x837), ("expr", cvtEXPR x838)]))
     | cvtIDENT_EXPR (AttributeIdentifier x846) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x846))
     | cvtIDENT_EXPR (Identifier{ident=x849, openNamespaces=ls851}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x849), ("openNamespaces", 
          PrettyRep.List (List.map (fn x850 => cvtNAMESPACE x850
                                   ) ls851))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x862) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x862))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x865, typeParams=ls867}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x865), ("typeParams", 
          PrettyRep.List (List.map (fn x866 => cvtTYPE_EXPR x866
                                   ) ls867))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r880) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r880))
     | cvtLITERAL (LiteralBoolean b883) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b883))
     | cvtLITERAL (LiteralString x886) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x886))
     | cvtLITERAL (LiteralArray{exprs=ls890, ty=opt895}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x889 => 
                                                                         cvtEXPR x889
                                                                  ) ls890)), 
          ("ty", 
       (case opt895 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x894))
       ))]))
     | cvtLITERAL (LiteralXML ls907) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x906 => 
                                                                                                           cvtEXPR x906
                                                                                                    ) ls907)))
     | cvtLITERAL (LiteralNamespace x913) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x913))
     | cvtLITERAL (LiteralObject{expr=ls917, ty=opt922}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x916 => 
                                                                        cvtFIELD x916
                                                                 ) ls917)), 
          ("ty", 
       (case opt922 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x921 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x921))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x933}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x933)]))
   and cvtBLOCK (Block x939) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x939))
   and cvtPATTERN (ObjectPattern ls943) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x942 => cvtFIELD_PATTERN x942
                                         ) ls943)))
     | cvtPATTERN (ArrayPattern ls950) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x949 => 
                                                                                                               cvtPATTERN x949
                                                                                                        ) ls950)))
     | cvtPATTERN (SimplePattern x956) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x956))
     | cvtPATTERN (IdentifierPattern x959) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x959))
   and cvtFIXTURE (NamespaceFixture x962) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x962))
     | cvtFIXTURE (ClassFixture x965) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x965))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x969) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x969))
     | cvtFIXTURE (ValFixture{ty=x972, readOnly=b973, isOverride=b974}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x972), ("readOnly", PrettyRep.Bool b973), 
          ("isOverride", PrettyRep.Bool b974)]))
     | cvtFIXTURE (VirtualValFixture{ty=x984, getter=opt986, setter=opt991}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x984), ("getter", 
       (case opt986 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x985 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x985))
       )), ("setter", 
       (case opt991 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x990 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x990))
       ))]))
   and cvtFIELD {kind=x1004, name=x1005, init=x1006} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1004), ("name", cvtIDENT x1005), ("init", cvtEXPR x1006)]
   and cvtFIELD_PATTERN {name=x1014, ptrn=x1015} = PrettyRep.Rec [("name", 
          cvtIDENT x1014), ("ptrn", cvtPATTERN x1015)]
   and cvtFIELD_TYPE {name=x1021, ty=x1022} = PrettyRep.Rec [("name", cvtIDENT x1021), 
          ("ty", cvtTYPE_EXPR x1022)]
   and cvtTYPED_IDENT {name=x1028, ty=opt1030} = PrettyRep.Rec [("name", cvtIDENT x1028), 
          ("ty", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1029))
       ))]
   and cvtATTRIBUTES {ns=x1039, override=b1040, static=b1041, final=b1042, 
          dynamic=b1043, prototype=b1044, native=b1045, rest=b1046} = PrettyRep.Rec [("ns", 
          cvtEXPR x1039), ("override", PrettyRep.Bool b1040), ("static", PrettyRep.Bool b1041), 
          ("final", PrettyRep.Bool b1042), ("dynamic", PrettyRep.Bool b1043), 
          ("prototype", PrettyRep.Bool b1044), ("native", PrettyRep.Bool b1045), 
          ("rest", PrettyRep.Bool b1046)]
   and cvtFUNC_DEFN {kind=x1064, ns=x1065, final=b1066, native=b1067, override=b1068, 
          prototype=b1069, static=b1070, func=x1071} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1064), ("ns", cvtEXPR x1065), ("final", PrettyRep.Bool b1066), 
          ("native", PrettyRep.Bool b1067), ("override", PrettyRep.Bool b1068), 
          ("prototype", PrettyRep.Bool b1069), ("static", PrettyRep.Bool b1070), 
          ("func", cvtFUNC x1071)]
   and cvtVAR_DEFN {kind=x1089, ns=x1090, static=b1091, prototype=b1092, bindings=ls1094} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1089), ("ns", cvtEXPR x1090), 
          ("static", PrettyRep.Bool b1091), ("prototype", PrettyRep.Bool b1092), 
          ("bindings", PrettyRep.List (List.map (fn x1093 => cvtVAR_BINDING x1093
                                                ) ls1094))]
   and cvtFIXTURES ls1112 = PrettyRep.List (List.map (fn (x1109, x1110) => 
                                                            PrettyRep.Tuple [cvtNAME x1109, 
                                                            cvtFIXTURE x1110]
                                                     ) ls1112)
   and cvtNAMESPACE_DEFN {ident=x1116, ns=x1117, init=opt1119} = PrettyRep.Rec [("ident", 
          cvtIDENT x1116), ("ns", cvtEXPR x1117), ("init", 
       (case opt1119 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1118 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1118))
       ))]
   and cvtCLASS_DEFN {ident=x1130, ns=x1131, nonnullable=b1132, dynamic=b1133, 
          final=b1134, params=ls1136, extends=opt1141, implements=ls1146, classFixtures=opt1151, 
          instanceFixtures=opt1156, body=x1160, protoVars=ls1162, protoMethods=ls1167, 
          instanceVars=ls1172, instanceMethods=ls1177, vars=ls1182, methods=ls1187, 
          constructor=opt1192, initializer=ls1197} = PrettyRep.Rec [("ident", 
          cvtIDENT x1130), ("ns", cvtEXPR x1131), ("nonnullable", PrettyRep.Bool b1132), 
          ("dynamic", PrettyRep.Bool b1133), ("final", PrettyRep.Bool b1134), 
          ("params", PrettyRep.List (List.map (fn x1135 => cvtIDENT x1135
                                              ) ls1136)), ("extends", 
       (case opt1141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1140 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1140))
       )), ("implements", PrettyRep.List (List.map (fn x1145 => cvtIDENT_EXPR x1145
                                                   ) ls1146)), ("classFixtures", 
          
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1150))
       )), ("instanceFixtures", 
       (case opt1156 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1155 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1155))
       )), ("body", cvtBLOCK x1160), ("protoVars", PrettyRep.List (List.map (fn x1161 => 
                                                                                   cvtVAR_DEFN x1161
                                                                            ) ls1162)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1166 => cvtFUNC_DEFN x1166
                                                    ) ls1167)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1171 => cvtVAR_DEFN x1171
                                   ) ls1172)), ("instanceMethods", PrettyRep.List (List.map (fn x1176 => 
                                                                                                   cvtFUNC_DEFN x1176
                                                                                            ) ls1177)), 
          ("vars", PrettyRep.List (List.map (fn x1181 => cvtVAR_DEFN x1181
                                            ) ls1182)), ("methods", PrettyRep.List (List.map (fn x1186 => 
                                                                                                    cvtFUNC_DEFN x1186
                                                                                             ) ls1187)), 
          ("constructor", 
       (case opt1192 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1191 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1191))
       )), ("initializer", PrettyRep.List (List.map (fn x1196 => cvtSTMT x1196
                                                    ) ls1197))]
   and cvtINTERFACE_DEFN {ident=x1240, ns=x1241, nonnullable=b1242, params=ls1244, 
          extends=ls1249, body=x1253} = PrettyRep.Rec [("ident", cvtIDENT x1240), 
          ("ns", cvtEXPR x1241), ("nonnullable", PrettyRep.Bool b1242), ("params", 
          PrettyRep.List (List.map (fn x1243 => cvtIDENT x1243
                                   ) ls1244)), ("extends", PrettyRep.List (List.map (fn x1248 => 
                                                                                           cvtIDENT_EXPR x1248
                                                                                    ) ls1249)), 
          ("body", cvtBLOCK x1253)]
   and cvtTYPE_DEFN {ident=x1267, ns=x1268, init=x1269} = PrettyRep.Rec [("ident", 
          cvtIDENT x1267), ("ns", cvtEXPR x1268), ("init", cvtTYPE_EXPR x1269)]
   and cvtFOR_ENUM_STMT {ptrn=opt1278, obj=ls1283, defns=ls1288, fixtures=opt1293, 
          contLabel=opt1298, body=x1302} = PrettyRep.Rec [("ptrn", 
       (case opt1278 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1277 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1277))
       )), ("obj", PrettyRep.List (List.map (fn x1282 => cvtEXPR x1282
                                            ) ls1283)), ("defns", PrettyRep.List (List.map (fn x1287 => 
                                                                                                  cvtVAR_BINDING x1287
                                                                                           ) ls1288)), 
          ("fixtures", 
       (case opt1293 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1292 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1292))
       )), ("contLabel", 
       (case opt1298 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1297 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1297))
       )), ("body", cvtSTMT x1302)]
   and cvtWHILE_STMT {cond=x1316, body=x1317, contLabel=opt1319} = PrettyRep.Rec [("cond", 
          cvtEXPR x1316), ("body", cvtSTMT x1317), ("contLabel", 
       (case opt1319 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1318 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1318))
       ))]
   and cvtDIRECTIVES {pragmas=ls1331, defns=ls1336, stmts=ls1341, fixtures=opt1346} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1330 => 
                                                                     cvtPRAGMA x1330
                                                              ) ls1331)), ("defns", 
          PrettyRep.List (List.map (fn x1335 => cvtDEFN x1335
                                   ) ls1336)), ("stmts", PrettyRep.List (List.map (fn x1340 => 
                                                                                         cvtSTMT x1340
                                                                                  ) ls1341)), 
          ("fixtures", 
       (case opt1346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1345))
       ))]
   and cvtBINDINGS {b=ls1360, i=ls1365} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1359 => 
                                                                                               cvtVAR_BINDING x1359
                                                                                        ) ls1360)), 
          ("i", PrettyRep.List (List.map (fn x1364 => cvtEXPR x1364
                                         ) ls1365))]
   and cvtCASE {label=opt1379, body=x1383} = PrettyRep.Rec [("label", 
       (case opt1379 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1375 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1374 => 
                                                                                      cvtEXPR x1374
                                                                               ) ls1375)))
       )), ("body", cvtBLOCK x1383)]
   and cvtTYPE_CASE {ptrn=opt1390, body=x1394} = PrettyRep.Rec [("ptrn", 
       (case opt1390 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1389 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1389))
       )), ("body", cvtBLOCK x1394)]
   and cvtFUNC_NAME {kind=x1400, ident=x1401} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1400), 
          ("ident", cvtIDENT x1401)]
   and cvtPACKAGE {name=x1407, body=x1408} = PrettyRep.Rec [("name", cvtUSTRING x1407), 
          ("body", cvtBLOCK x1408)]
   and cvtPROGRAM {packages=ls1415, body=x1419} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1414 => cvtPACKAGE x1414
                                   ) ls1415)), ("body", cvtBLOCK x1419)]
end

