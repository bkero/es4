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
   and cvtTRIOP (Cond) = PrettyRep.Ctor ("Cond", NONE)
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
     | cvtBINOP (DefVar) = PrettyRep.Ctor ("DefVar", NONE)
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
   and cvtPRAGMA (UseNamespace x111) = PrettyRep.Ctor ("UseNamespace", SOME (cvtIDENT_EXPR x111))
     | cvtPRAGMA (UseDefaultNamespace x114) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtIDENT_EXPR x114))
     | cvtPRAGMA (UseNumber x117) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x117))
     | cvtPRAGMA (UseRounding x120) = PrettyRep.Ctor ("UseRounding", SOME (cvtROUNDING_MODE x120))
     | cvtPRAGMA (UsePrecision x123) = PrettyRep.Ctor ("UsePrecision", SOME (cvtLITERAL x123))
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
     | cvtPRAGMA (Import{package=x128, name=x129, alias=opt131}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", cvtIDENT x128), ("name", cvtIDENT x129), 
          ("alias", 
       (case opt131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x130 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x130))
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Construct) = PrettyRep.Ctor ("Construct", NONE)
     | cvtFUNC_NAME_KIND (ToFunc) = PrettyRep.Ctor ("ToFunc", NONE)
   and cvtFUNC (Func{name=x151, fsig=x152, body=x153, fixtures=opt155}) = PrettyRep.Ctor ("Func", 
          SOME (PrettyRep.Rec [("name", cvtFUNC_NAME x151), ("fsig", cvtFUNC_SIG x152), 
          ("body", cvtBLOCK x153), ("fixtures", 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x154))
       ))]))
   and cvtDEFN (ClassDefn x170) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x170))
     | cvtDEFN (VariableDefn x173) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x173))
     | cvtDEFN (FunctionDefn x176) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x176))
     | cvtDEFN (InterfaceDefn x179) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x179))
     | cvtDEFN (NamespaceDefn x182) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x182))
     | cvtDEFN (TypeDefn x185) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x185))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls189, params=ls194, inits=opt199, 
          returnType=x203, thisType=opt205, hasBoundThis=b209, hasRest=b210}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x188 => cvtIDENT x188
                                   ) ls189)), ("params", PrettyRep.List (List.map (fn x193 => 
                                                                                         cvtVAR_BINDING x193
                                                                                  ) ls194)), 
          ("inits", 
       (case opt199 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x198 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x198))
       )), ("returnType", cvtTYPE_EXPR x203), ("thisType", 
       (case opt205 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x204 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x204))
       )), ("hasBoundThis", PrettyRep.Bool b209), ("hasRest", PrettyRep.Bool b210)]))
   and cvtVAR_BINDING (Binding{pattern=x228, ty=opt230, init=opt235}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x228), ("ty", 
       (case opt230 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x229 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x229))
       )), ("init", 
       (case opt235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x234 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x234))
       ))]))
   and cvtTYPE_EXPR (SpecialType x248) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x248))
     | cvtTYPE_EXPR (UnionType ls252) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x251 => 
                                                                                                           cvtTYPE_EXPR x251
                                                                                                    ) ls252)))
     | cvtTYPE_EXPR (ArrayType ls259) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x258 => 
                                                                                                           cvtTYPE_EXPR x258
                                                                                                    ) ls259)))
     | cvtTYPE_EXPR (NominalType{ident=x265}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x265)]))
     | cvtTYPE_EXPR (FunctionType x271) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x271))
     | cvtTYPE_EXPR (ObjectType ls275) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x274 => 
                                                                                                             cvtFIELD_TYPE x274
                                                                                                      ) ls275)))
     | cvtTYPE_EXPR (AppType{base=x281, args=ls283}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x281), ("args", PrettyRep.List (List.map (fn x282 => 
                                                                                                     cvtTYPE_EXPR x282
                                                                                              ) ls283))]))
     | cvtTYPE_EXPR (NullableType{expr=x294, nullable=b295}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x294), ("nullable", PrettyRep.Bool b295)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls305) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x304 => 
                                                                                                    cvtEXPR x304
                                                                                             ) ls305)))
     | cvtSTMT (ForEachStmt x311) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x311))
     | cvtSTMT (ForInStmt x314) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x314))
     | cvtSTMT (ThrowStmt ls318) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x317 => 
                                                                                                      cvtEXPR x317
                                                                                               ) ls318)))
     | cvtSTMT (ReturnStmt ls325) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x324 => 
                                                                                                        cvtEXPR x324
                                                                                                 ) ls325)))
     | cvtSTMT (BreakStmt opt332) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt332 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x331 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x331))
       ))
     | cvtSTMT (ContinueStmt opt339) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x338 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x338))
       ))
     | cvtSTMT (BlockStmt x345) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x345))
     | cvtSTMT (LabeledStmt(x348, x349)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x348, 
          cvtSTMT x349]))
     | cvtSTMT (LetStmt(ls354, x358)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x353 => 
                                                                                                                          cvtVAR_BINDING x353
                                                                                                                   ) ls354), 
          cvtSTMT x358]))
     | cvtSTMT (SuperStmt ls363) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x362 => 
                                                                                                      cvtEXPR x362
                                                                                               ) ls363)))
     | cvtSTMT (WhileStmt x369) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x369))
     | cvtSTMT (DoWhileStmt x372) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x372))
     | cvtSTMT (ForStmt{defns=ls376, fixtures=opt381, init=ls386, cond=ls391, 
          update=ls396, contLabel=opt401, body=x405}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x375 => 
                                                                         cvtVAR_BINDING x375
                                                                  ) ls376)), 
          ("fixtures", 
       (case opt381 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x380 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x380))
       )), ("init", PrettyRep.List (List.map (fn x385 => cvtEXPR x385
                                             ) ls386)), ("cond", PrettyRep.List (List.map (fn x390 => 
                                                                                                 cvtEXPR x390
                                                                                          ) ls391)), 
          ("update", PrettyRep.List (List.map (fn x395 => cvtEXPR x395
                                              ) ls396)), ("contLabel", 
       (case opt401 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x400 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x400))
       )), ("body", cvtSTMT x405)]))
     | cvtSTMT (IfStmt{cnd=x423, thn=x424, els=x425}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x423), ("thn", cvtSTMT x424), 
          ("els", cvtSTMT x425)]))
     | cvtSTMT (WithStmt{obj=ls436, ty=x440, body=x441}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x435 => 
                                                                       cvtEXPR x435
                                                                ) ls436)), 
          ("ty", cvtTYPE_EXPR x440), ("body", cvtSTMT x441)]))
     | cvtSTMT (TryStmt{body=x451, catches=ls466, finally=opt471}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x451), ("catches", PrettyRep.List (List.map (fn {bind=x452, 
                                                                                                    fixtures=opt454, 
                                                                                                    body=x458} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x452), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt454 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x453 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x453))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x458)]
                                                                                             ) ls466)), 
          ("finally", 
       (case opt471 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x470 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x470))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls485, cases=ls490}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x484 => 
                                                                        cvtEXPR x484
                                                                 ) ls485)), 
          ("cases", PrettyRep.List (List.map (fn x489 => cvtCASE x489
                                             ) ls490))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls502, ty=x506, cases=ls508}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x501 => 
                                                                        cvtEXPR x501
                                                                 ) ls502)), 
          ("ty", cvtTYPE_EXPR x506), ("cases", PrettyRep.List (List.map (fn x507 => 
                                                                               cvtTYPE_CASE x507
                                                                        ) ls508))]))
     | cvtSTMT (Dxns{expr=x521}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x521)]))
   and cvtEXPR (TrinaryExpr(x527, x528, x529, x530)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x527, cvtEXPR x528, cvtEXPR x529, 
          cvtEXPR x530]))
     | cvtEXPR (BinaryExpr(x534, x535, x536)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x534, cvtEXPR x535, cvtEXPR x536]))
     | cvtEXPR (BinaryTypeExpr(x540, x541, x542)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x540, cvtEXPR x541, cvtTYPE_EXPR x542]))
     | cvtEXPR (UnaryExpr(x546, x547)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x546, 
          cvtEXPR x547]))
     | cvtEXPR (TypeExpr x551) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x551))
     | cvtEXPR (NullaryExpr x554) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x554))
     | cvtEXPR (YieldExpr opt562) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt562 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls558 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x557 => 
                                                                                     cvtEXPR x557
                                                                              ) ls558)))
       ))
     | cvtEXPR (SuperExpr opt569) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x568 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x568))
       ))
     | cvtEXPR (LiteralExpr x575) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x575))
     | cvtEXPR (CallExpr{func=x578, actuals=ls580}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x578), ("actuals", PrettyRep.List (List.map (fn x579 => 
                                                                                                   cvtEXPR x579
                                                                                            ) ls580))]))
     | cvtEXPR (ApplyTypeExpr{expr=x591, actuals=ls593}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x591), ("actuals", PrettyRep.List (List.map (fn x592 => 
                                                                                                   cvtTYPE_EXPR x592
                                                                                            ) ls593))]))
     | cvtEXPR (LetExpr{defs=ls605, body=ls610, fixtures=opt615}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x604 => 
                                                                        cvtVAR_BINDING x604
                                                                 ) ls605)), 
          ("body", PrettyRep.List (List.map (fn x609 => cvtEXPR x609
                                            ) ls610)), ("fixtures", 
       (case opt615 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x614 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x614))
       ))]))
     | cvtEXPR (NewExpr{obj=x628, actuals=ls630}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x628), ("actuals", PrettyRep.List (List.map (fn x629 => 
                                                                                                  cvtEXPR x629
                                                                                           ) ls630))]))
     | cvtEXPR (FunExpr{ident=opt642, fsig=x646, body=x647, fixtures=opt649}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt642 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x641 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x641))
       )), ("fsig", cvtFUNC_SIG x646), ("body", cvtBLOCK x647), ("fixtures", 
          
       (case opt649 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x648 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x648))
       ))]))
     | cvtEXPR (ObjectRef{base=x664, ident=x665}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x664), ("ident", cvtIDENT_EXPR x665)]))
     | cvtEXPR (LexicalRef{ident=x673}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x673)]))
     | cvtEXPR (SetExpr(x679, x680, x681)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x679, 
          cvtPATTERN x680, cvtEXPR x681]))
     | cvtEXPR (ListExpr ls686) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x685 => 
                                                                                                    cvtEXPR x685
                                                                                             ) ls686)))
     | cvtEXPR (SliceExpr(ls693, ls698, ls703)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x692 => cvtEXPR x692
                                                          ) ls693), PrettyRep.List (List.map (fn x697 => 
                                                                                                    cvtEXPR x697
                                                                                             ) ls698), 
          PrettyRep.List (List.map (fn x702 => cvtEXPR x702
                                   ) ls703)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x710, ident=x711}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x710), ("ident", cvtUSTRING x711)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x719, expr=x720}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x719), ("expr", cvtEXPR x720)]))
     | cvtIDENT_EXPR (AttributeIdentifier x728) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x728))
     | cvtIDENT_EXPR (Identifier{ident=x731, openNamespaces=ls733}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x731), ("openNamespaces", 
          PrettyRep.List (List.map (fn x732 => cvtNAMESPACE x732
                                   ) ls733))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x744) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x744))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x747, typeParams=ls749}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x747), ("typeParams", 
          PrettyRep.List (List.map (fn x748 => cvtTYPE_EXPR x748
                                   ) ls749))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r762) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r762))
     | cvtLITERAL (LiteralBoolean b765) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b765))
     | cvtLITERAL (LiteralString x768) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x768))
     | cvtLITERAL (LiteralArray{exprs=ls772, ty=opt777}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x771 => 
                                                                         cvtEXPR x771
                                                                  ) ls772)), 
          ("ty", 
       (case opt777 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x776 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x776))
       ))]))
     | cvtLITERAL (LiteralXML ls789) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x788 => 
                                                                                                           cvtEXPR x788
                                                                                                    ) ls789)))
     | cvtLITERAL (LiteralNamespace x795) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x795))
     | cvtLITERAL (LiteralObject{expr=ls799, ty=opt804}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x798 => 
                                                                        cvtFIELD x798
                                                                 ) ls799)), 
          ("ty", 
       (case opt804 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x803 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x803))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x815}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x815)]))
   and cvtBLOCK (Block x821) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x821))
   and cvtPATTERN (ObjectPattern ls825) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x824 => cvtFIELD_PATTERN x824
                                         ) ls825)))
     | cvtPATTERN (ArrayPattern ls832) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x831 => 
                                                                                                               cvtPATTERN x831
                                                                                                        ) ls832)))
     | cvtPATTERN (SimplePattern x838) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x838))
     | cvtPATTERN (IdentifierPattern x841) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x841))
   and cvtFIXTURE (NamespaceFixture x844) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x844))
     | cvtFIXTURE (ClassFixture x847) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x847))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x851) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x851))
     | cvtFIXTURE (ValFixture{ty=x854, readOnly=b855, isOverride=b856}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x854), ("readOnly", PrettyRep.Bool b855), 
          ("isOverride", PrettyRep.Bool b856)]))
     | cvtFIXTURE (VirtualValFixture{ty=x866, getter=opt868, setter=opt873}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x866), ("getter", 
       (case opt868 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x867 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x867))
       )), ("setter", 
       (case opt873 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x872 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x872))
       ))]))
   and cvtFIELD {kind=x886, name=x887, init=x888} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x886), ("name", cvtIDENT x887), ("init", cvtEXPR x888)]
   and cvtFIELD_PATTERN {name=x896, ptrn=x897} = PrettyRep.Rec [("name", cvtIDENT x896), 
          ("ptrn", cvtPATTERN x897)]
   and cvtFIELD_TYPE {name=x903, ty=x904} = PrettyRep.Rec [("name", cvtIDENT x903), 
          ("ty", cvtTYPE_EXPR x904)]
   and cvtTYPED_IDENT {name=x910, ty=opt912} = PrettyRep.Rec [("name", cvtIDENT x910), 
          ("ty", 
       (case opt912 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x911 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x911))
       ))]
   and cvtATTRIBUTES {ns=x921, override=b922, static=b923, final=b924, dynamic=b925, 
          prototype=b926, native=b927, rest=b928} = PrettyRep.Rec [("ns", cvtEXPR x921), 
          ("override", PrettyRep.Bool b922), ("static", PrettyRep.Bool b923), 
          ("final", PrettyRep.Bool b924), ("dynamic", PrettyRep.Bool b925), 
          ("prototype", PrettyRep.Bool b926), ("native", PrettyRep.Bool b927), 
          ("rest", PrettyRep.Bool b928)]
   and cvtFUNC_DEFN {kind=x946, ns=x947, final=b948, native=b949, override=b950, 
          prototype=b951, static=b952, func=x953} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x946), ("ns", cvtEXPR x947), ("final", PrettyRep.Bool b948), 
          ("native", PrettyRep.Bool b949), ("override", PrettyRep.Bool b950), 
          ("prototype", PrettyRep.Bool b951), ("static", PrettyRep.Bool b952), 
          ("func", cvtFUNC x953)]
   and cvtVAR_DEFN {kind=x971, ns=x972, static=b973, prototype=b974, bindings=ls976} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x971), ("ns", cvtEXPR x972), 
          ("static", PrettyRep.Bool b973), ("prototype", PrettyRep.Bool b974), 
          ("bindings", PrettyRep.List (List.map (fn x975 => cvtVAR_BINDING x975
                                                ) ls976))]
   and cvtFIXTURES ls994 = PrettyRep.List (List.map (fn (x991, x992) => PrettyRep.Tuple [cvtNAME x991, 
                                                           cvtFIXTURE x992]
                                                    ) ls994)
   and cvtNAMESPACE_DEFN {ident=x998, ns=x999, init=opt1001} = PrettyRep.Rec [("ident", 
          cvtIDENT x998), ("ns", cvtEXPR x999), ("init", 
       (case opt1001 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1000 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1000))
       ))]
   and cvtCLASS_DEFN {name=x1012, ns=x1013, nonnullable=b1014, dynamic=b1015, 
          final=b1016, params=ls1018, extends=opt1023, implements=ls1028, classFixtures=opt1033, 
          instanceFixtures=opt1038, body=x1042, protoVars=ls1044, protoMethods=ls1049, 
          instanceVars=ls1054, instanceMethods=ls1059, vars=ls1064, methods=ls1069, 
          constructor=opt1074, initializer=ls1079} = PrettyRep.Rec [("name", 
          cvtIDENT x1012), ("ns", cvtEXPR x1013), ("nonnullable", PrettyRep.Bool b1014), 
          ("dynamic", PrettyRep.Bool b1015), ("final", PrettyRep.Bool b1016), 
          ("params", PrettyRep.List (List.map (fn x1017 => cvtIDENT x1017
                                              ) ls1018)), ("extends", 
       (case opt1023 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1022 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1022))
       )), ("implements", PrettyRep.List (List.map (fn x1027 => cvtIDENT_EXPR x1027
                                                   ) ls1028)), ("classFixtures", 
          
       (case opt1033 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1032 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1032))
       )), ("instanceFixtures", 
       (case opt1038 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1037 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1037))
       )), ("body", cvtBLOCK x1042), ("protoVars", PrettyRep.List (List.map (fn x1043 => 
                                                                                   cvtVAR_BINDING x1043
                                                                            ) ls1044)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1048 => cvtFUNC_DEFN x1048
                                                    ) ls1049)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1053 => cvtVAR_BINDING x1053
                                   ) ls1054)), ("instanceMethods", PrettyRep.List (List.map (fn x1058 => 
                                                                                                   cvtFUNC_DEFN x1058
                                                                                            ) ls1059)), 
          ("vars", PrettyRep.List (List.map (fn x1063 => cvtVAR_BINDING x1063
                                            ) ls1064)), ("methods", PrettyRep.List (List.map (fn x1068 => 
                                                                                                    cvtFUNC_DEFN x1068
                                                                                             ) ls1069)), 
          ("constructor", 
       (case opt1074 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1073))
       )), ("initializer", PrettyRep.List (List.map (fn x1078 => cvtSTMT x1078
                                                    ) ls1079))]
   and cvtINTERFACE_DEFN {name=x1122, ns=x1123, nonnullable=b1124, params=ls1126, 
          extends=ls1131, body=x1135} = PrettyRep.Rec [("name", cvtIDENT x1122), 
          ("ns", cvtEXPR x1123), ("nonnullable", PrettyRep.Bool b1124), ("params", 
          PrettyRep.List (List.map (fn x1125 => cvtIDENT x1125
                                   ) ls1126)), ("extends", PrettyRep.List (List.map (fn x1130 => 
                                                                                           cvtIDENT_EXPR x1130
                                                                                    ) ls1131)), 
          ("body", cvtBLOCK x1135)]
   and cvtTYPE_DEFN {name=x1149, ns=x1150, init=x1151} = PrettyRep.Rec [("name", 
          cvtIDENT x1149), ("ns", cvtEXPR x1150), ("init", cvtTYPE_EXPR x1151)]
   and cvtFOR_ENUM_STMT {ptrn=opt1160, obj=ls1165, defns=ls1170, fixtures=opt1175, 
          contLabel=opt1180, body=x1184} = PrettyRep.Rec [("ptrn", 
       (case opt1160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1159 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1159))
       )), ("obj", PrettyRep.List (List.map (fn x1164 => cvtEXPR x1164
                                            ) ls1165)), ("defns", PrettyRep.List (List.map (fn x1169 => 
                                                                                                  cvtVAR_BINDING x1169
                                                                                           ) ls1170)), 
          ("fixtures", 
       (case opt1175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1174 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1174))
       )), ("contLabel", 
       (case opt1180 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1179 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1179))
       )), ("body", cvtSTMT x1184)]
   and cvtWHILE_STMT {cond=x1198, body=x1199, contLabel=opt1201} = PrettyRep.Rec [("cond", 
          cvtEXPR x1198), ("body", cvtSTMT x1199), ("contLabel", 
       (case opt1201 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1200 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1200))
       ))]
   and cvtDIRECTIVES {pragmas=ls1213, defns=ls1218, stmts=ls1223, fixtures=opt1228} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1212 => 
                                                                     cvtPRAGMA x1212
                                                              ) ls1213)), ("defns", 
          PrettyRep.List (List.map (fn x1217 => cvtDEFN x1217
                                   ) ls1218)), ("stmts", PrettyRep.List (List.map (fn x1222 => 
                                                                                         cvtSTMT x1222
                                                                                  ) ls1223)), 
          ("fixtures", 
       (case opt1228 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1227 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1227))
       ))]
   and cvtBINDINGS {b=ls1242, i=ls1247} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1241 => 
                                                                                               cvtVAR_BINDING x1241
                                                                                        ) ls1242)), 
          ("i", PrettyRep.List (List.map (fn x1246 => cvtEXPR x1246
                                         ) ls1247))]
   and cvtCASE {label=opt1261, body=x1265} = PrettyRep.Rec [("label", 
       (case opt1261 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1257 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1256 => 
                                                                                      cvtEXPR x1256
                                                                               ) ls1257)))
       )), ("body", cvtBLOCK x1265)]
   and cvtTYPE_CASE {ptrn=opt1272, body=x1276} = PrettyRep.Rec [("ptrn", 
       (case opt1272 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1271 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1271))
       )), ("body", cvtBLOCK x1276)]
   and cvtFUNC_NAME {kind=x1282, ident=x1283} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1282), 
          ("ident", cvtIDENT x1283)]
   and cvtPACKAGE {name=x1289, body=x1290} = PrettyRep.Rec [("name", cvtUSTRING x1289), 
          ("body", cvtBLOCK x1290)]
   and cvtPROGRAM {packages=ls1297, body=x1301} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1296 => cvtPACKAGE x1296
                                   ) ls1297)), ("body", cvtBLOCK x1301)]
end

