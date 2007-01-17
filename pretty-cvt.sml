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
     | cvtDEFN (VariableDefn ls174) = PrettyRep.Ctor ("VariableDefn", SOME (PrettyRep.List (List.map (fn x173 => 
                                                                                                            cvtVAR_BINDING x173
                                                                                                     ) ls174)))
     | cvtDEFN (FunctionDefn x180) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x180))
     | cvtDEFN (InterfaceDefn x183) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x183))
     | cvtDEFN (NamespaceDefn x186) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x186))
     | cvtDEFN (TypeDefn{attrs=x189, ident=x190, init=x191}) = PrettyRep.Ctor ("TypeDefn", 
          SOME (PrettyRep.Rec [("attrs", cvtATTRIBUTES x189), ("ident", cvtIDENT x190), 
          ("init", cvtTYPE_EXPR x191)]))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls202, params=ls207, inits=opt212, 
          returnType=x216, thisType=opt218, hasBoundThis=b222, hasRest=b223}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x201 => cvtIDENT x201
                                   ) ls202)), ("params", PrettyRep.List (List.map (fn x206 => 
                                                                                         cvtVAR_BINDING x206
                                                                                  ) ls207)), 
          ("inits", 
       (case opt212 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x211 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x211))
       )), ("returnType", cvtTYPE_EXPR x216), ("thisType", 
       (case opt218 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x217 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x217))
       )), ("hasBoundThis", PrettyRep.Bool b222), ("hasRest", PrettyRep.Bool b223)]))
   and cvtATTRIBUTES (Attributes{ns=x241, override=b242, static=b243, final=b244, 
          dynamic=b245, prototype=b246, native=b247, rest=b248}) = PrettyRep.Ctor ("Attributes", 
          SOME (PrettyRep.Rec [("ns", cvtEXPR x241), ("override", PrettyRep.Bool b242), 
          ("static", PrettyRep.Bool b243), ("final", PrettyRep.Bool b244), 
          ("dynamic", PrettyRep.Bool b245), ("prototype", PrettyRep.Bool b246), 
          ("native", PrettyRep.Bool b247), ("rest", PrettyRep.Bool b248)]))
   and cvtVAR_BINDING (Binding{kind=x268, init=opt270, attrs=x274, pattern=x275, 
          ty=opt277}) = PrettyRep.Ctor ("Binding", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x268), ("init", 
       (case opt270 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x269))
       )), ("attrs", cvtATTRIBUTES x274), ("pattern", cvtPATTERN x275), ("ty", 
          
       (case opt277 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x276 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x276))
       ))]))
   and cvtTYPE_EXPR (SpecialType x294) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x294))
     | cvtTYPE_EXPR (UnionType ls298) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x297 => 
                                                                                                           cvtTYPE_EXPR x297
                                                                                                    ) ls298)))
     | cvtTYPE_EXPR (ArrayType ls305) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x304 => 
                                                                                                           cvtTYPE_EXPR x304
                                                                                                    ) ls305)))
     | cvtTYPE_EXPR (NominalType{ident=x311, nullable=opt313}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x311), ("nullable", 
          
       (case opt313 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b312 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b312))
       ))]))
     | cvtTYPE_EXPR (FunctionType x324) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x324))
     | cvtTYPE_EXPR (ObjectType ls328) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x327 => 
                                                                                                             cvtFIELD_TYPE x327
                                                                                                      ) ls328)))
     | cvtTYPE_EXPR (AppType{base=x334, args=ls336}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x334), ("args", PrettyRep.List (List.map (fn x335 => 
                                                                                                     cvtTYPE_EXPR x335
                                                                                              ) ls336))]))
     | cvtTYPE_EXPR (NullableType{expr=x347, nullable=b348}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x347), ("nullable", PrettyRep.Bool b348)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls358) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x357 => 
                                                                                                    cvtEXPR x357
                                                                                             ) ls358)))
     | cvtSTMT (ForEachStmt x364) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x364))
     | cvtSTMT (ForInStmt x367) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x367))
     | cvtSTMT (ThrowStmt ls371) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x370 => 
                                                                                                      cvtEXPR x370
                                                                                               ) ls371)))
     | cvtSTMT (ReturnStmt ls378) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x377 => 
                                                                                                        cvtEXPR x377
                                                                                                 ) ls378)))
     | cvtSTMT (BreakStmt opt385) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt385 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x384 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x384))
       ))
     | cvtSTMT (ContinueStmt opt392) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt392 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x391 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x391))
       ))
     | cvtSTMT (BlockStmt x398) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x398))
     | cvtSTMT (LabeledStmt(x401, x402)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x401, 
          cvtSTMT x402]))
     | cvtSTMT (LetStmt(ls407, x411)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x406 => 
                                                                                                                          cvtVAR_BINDING x406
                                                                                                                   ) ls407), 
          cvtSTMT x411]))
     | cvtSTMT (SuperStmt ls416) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x415 => 
                                                                                                      cvtEXPR x415
                                                                                               ) ls416)))
     | cvtSTMT (WhileStmt x422) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x422))
     | cvtSTMT (DoWhileStmt x425) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x425))
     | cvtSTMT (ForStmt{defns=ls429, init=ls434, cond=ls439, update=ls444, 
          contLabel=opt449, body=x453}) = PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x428 => cvtVAR_BINDING x428
                                   ) ls429)), ("init", PrettyRep.List (List.map (fn x433 => 
                                                                                       cvtEXPR x433
                                                                                ) ls434)), 
          ("cond", PrettyRep.List (List.map (fn x438 => cvtEXPR x438
                                            ) ls439)), ("update", PrettyRep.List (List.map (fn x443 => 
                                                                                                  cvtEXPR x443
                                                                                           ) ls444)), 
          ("contLabel", 
       (case opt449 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x448 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x448))
       )), ("body", cvtSTMT x453)]))
     | cvtSTMT (IfStmt{cnd=x469, thn=x470, els=x471}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x469), ("thn", cvtSTMT x470), 
          ("els", cvtSTMT x471)]))
     | cvtSTMT (WithStmt{obj=ls482, ty=x486, body=x487}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x481 => 
                                                                       cvtEXPR x481
                                                                ) ls482)), 
          ("ty", cvtTYPE_EXPR x486), ("body", cvtSTMT x487)]))
     | cvtSTMT (TryStmt{body=x497, catches=ls505, finally=opt510}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x497), ("catches", PrettyRep.List (List.map (fn {bind=x498, 
                                                                                                    body=x499} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x498), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x499)]
                                                                                             ) ls505)), 
          ("finally", 
       (case opt510 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x509 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x509))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls524, cases=ls529}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x523 => 
                                                                        cvtEXPR x523
                                                                 ) ls524)), 
          ("cases", PrettyRep.List (List.map (fn x528 => cvtCASE x528
                                             ) ls529))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls541, ty=x545, cases=ls547}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x540 => 
                                                                        cvtEXPR x540
                                                                 ) ls541)), 
          ("ty", cvtTYPE_EXPR x545), ("cases", PrettyRep.List (List.map (fn x546 => 
                                                                               cvtTYPE_CASE x546
                                                                        ) ls547))]))
     | cvtSTMT (Dxns{expr=x560}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x560)]))
   and cvtEXPR (TrinaryExpr(x566, x567, x568, x569)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x566, cvtEXPR x567, cvtEXPR x568, 
          cvtEXPR x569]))
     | cvtEXPR (BinaryExpr(x573, x574, x575)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x573, cvtEXPR x574, cvtEXPR x575]))
     | cvtEXPR (BinaryTypeExpr(x579, x580, x581)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x579, cvtEXPR x580, cvtTYPE_EXPR x581]))
     | cvtEXPR (UnaryExpr(x585, x586)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x585, 
          cvtEXPR x586]))
     | cvtEXPR (TypeExpr x590) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x590))
     | cvtEXPR (NullaryExpr x593) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x593))
     | cvtEXPR (YieldExpr opt601) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt601 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls597 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x596 => 
                                                                                     cvtEXPR x596
                                                                              ) ls597)))
       ))
     | cvtEXPR (SuperExpr opt608) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt608 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x607 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x607))
       ))
     | cvtEXPR (LiteralExpr x614) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x614))
     | cvtEXPR (CallExpr{func=x617, actuals=ls619}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x617), ("actuals", PrettyRep.List (List.map (fn x618 => 
                                                                                                   cvtEXPR x618
                                                                                            ) ls619))]))
     | cvtEXPR (ApplyTypeExpr{expr=x630, actuals=ls632}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x630), ("actuals", PrettyRep.List (List.map (fn x631 => 
                                                                                                   cvtTYPE_EXPR x631
                                                                                            ) ls632))]))
     | cvtEXPR (LetExpr{defs=ls644, body=ls649, fixtures=opt654}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x643 => 
                                                                        cvtVAR_BINDING x643
                                                                 ) ls644)), 
          ("body", PrettyRep.List (List.map (fn x648 => cvtEXPR x648
                                            ) ls649)), ("fixtures", 
       (case opt654 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x653 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x653))
       ))]))
     | cvtEXPR (NewExpr{obj=x667, actuals=ls669}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x667), ("actuals", PrettyRep.List (List.map (fn x668 => 
                                                                                                  cvtEXPR x668
                                                                                           ) ls669))]))
     | cvtEXPR (FunExpr{ident=opt681, fsig=x685, body=x686, fixtures=opt688}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt681 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x680 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x680))
       )), ("fsig", cvtFUNC_SIG x685), ("body", cvtBLOCK x686), ("fixtures", 
          
       (case opt688 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x687 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x687))
       ))]))
     | cvtEXPR (ObjectRef{base=x703, ident=x704}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x703), ("ident", cvtIDENT_EXPR x704)]))
     | cvtEXPR (LexicalRef{ident=x712}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x712)]))
     | cvtEXPR (SetExpr(x718, x719, x720)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x718, 
          cvtPATTERN x719, cvtEXPR x720]))
     | cvtEXPR (ListExpr ls725) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x724 => 
                                                                                                    cvtEXPR x724
                                                                                             ) ls725)))
     | cvtEXPR (SliceExpr(ls732, ls737, ls742)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x731 => cvtEXPR x731
                                                          ) ls732), PrettyRep.List (List.map (fn x736 => 
                                                                                                    cvtEXPR x736
                                                                                             ) ls737), 
          PrettyRep.List (List.map (fn x741 => cvtEXPR x741
                                   ) ls742)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x749, ident=x750}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x749), ("ident", cvtUSTRING x750)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x758, expr=x759}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x758), ("expr", cvtEXPR x759)]))
     | cvtIDENT_EXPR (AttributeIdentifier x767) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x767))
     | cvtIDENT_EXPR (Identifier{ident=x770, openNamespaces=ls772}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x770), ("openNamespaces", 
          PrettyRep.List (List.map (fn x771 => cvtNAMESPACE x771
                                   ) ls772))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x783) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x783))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x786, typeParams=ls788}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x786), ("typeParams", 
          PrettyRep.List (List.map (fn x787 => cvtTYPE_EXPR x787
                                   ) ls788))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r801) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r801))
     | cvtLITERAL (LiteralBoolean b804) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b804))
     | cvtLITERAL (LiteralString x807) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x807))
     | cvtLITERAL (LiteralArray{exprs=ls811, ty=opt816}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x810 => 
                                                                         cvtEXPR x810
                                                                  ) ls811)), 
          ("ty", 
       (case opt816 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x815 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x815))
       ))]))
     | cvtLITERAL (LiteralXML ls828) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x827 => 
                                                                                                           cvtEXPR x827
                                                                                                    ) ls828)))
     | cvtLITERAL (LiteralNamespace x834) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x834))
     | cvtLITERAL (LiteralObject{expr=ls838, ty=opt843}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x837 => 
                                                                        cvtFIELD x837
                                                                 ) ls838)), 
          ("ty", 
       (case opt843 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x842 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x842))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x854}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x854)]))
   and cvtBLOCK (Block x860) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x860))
   and cvtPATTERN (ObjectPattern ls870) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn {name=x863, ptrn=x864} => PrettyRep.Rec [("name", 
                                                cvtIDENT_EXPR x863), ("ptrn", 
                                                cvtPATTERN x864)]
                                         ) ls870)))
     | cvtPATTERN (ArrayPattern ls877) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x876 => 
                                                                                                               cvtPATTERN x876
                                                                                                        ) ls877)))
     | cvtPATTERN (SimplePattern x883) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x883))
     | cvtPATTERN (IdentifierPattern x886) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x886))
   and cvtFIXTURE (NamespaceFixture x889) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x889))
     | cvtFIXTURE (ClassFixture x892) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x892))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x896) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x896))
     | cvtFIXTURE (ValFixture{ty=x899, readOnly=b900, isOverride=b901}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x899), ("readOnly", PrettyRep.Bool b900), 
          ("isOverride", PrettyRep.Bool b901)]))
   and cvtFIXTURES (Fixtures{bindings=x911, openNamespaces=ls913, numberType=x917, 
          roundingMode=x918}) = PrettyRep.Ctor ("Fixtures", SOME (PrettyRep.Rec [("bindings", 
          cvtFIXTURE_BINDINGS x911), ("openNamespaces", PrettyRep.List (List.map (fn x912 => 
                                                                                        cvtNAMESPACE x912
                                                                                 ) ls913)), 
          ("numberType", cvtNUMBER_TYPE x917), ("roundingMode", cvtROUNDING_MODE x918)]))
   and cvtFIELD {kind=x930, name=x931, init=x932} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x930), ("name", cvtIDENT_EXPR x931), ("init", cvtEXPR x932)]
   and cvtFIELD_TYPE {name=x940, ty=x941} = PrettyRep.Rec [("name", cvtIDENT_EXPR x940), 
          ("ty", cvtTYPE_EXPR x941)]
   and cvtTYPED_IDENT {name=x947, ty=opt949} = PrettyRep.Rec [("name", cvtIDENT x947), 
          ("ty", 
       (case opt949 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x948 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x948))
       ))]
   and cvtFUNC_DEFN {attrs=x958, kind=x959, func=x960} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x958), ("kind", cvtVAR_DEFN_TAG x959), ("func", cvtFUNC x960)]
   and cvtFIXTURE_BINDINGS ls971 = PrettyRep.List (List.map (fn (x968, x969) => 
                                                                   PrettyRep.Tuple [cvtNAME x968, 
                                                                   cvtFIXTURE x969]
                                                            ) ls971)
   and cvtNAMESPACE_DEFN {attrs=x975, ident=x976, init=opt978} = PrettyRep.Rec [("attrs", 
          cvtATTRIBUTES x975), ("ident", cvtIDENT x976), ("init", 
       (case opt978 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x977 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x977))
       ))]
   and cvtCLASS_DEFN {name=x989, nonnullable=b990, attrs=x991, params=ls993, 
          extends=opt998, implements=ls1003, classFixtures=opt1008, instanceFixtures=opt1013, 
          body=x1017, protoVars=ls1019, protoMethods=ls1024, instanceVars=ls1029, 
          instanceMethods=ls1034, vars=ls1039, methods=ls1044, constructor=opt1049, 
          initializer=ls1054} = PrettyRep.Rec [("name", cvtIDENT x989), ("nonnullable", 
          PrettyRep.Bool b990), ("attrs", cvtATTRIBUTES x991), ("params", PrettyRep.List (List.map (fn x992 => 
                                                                                                          cvtIDENT x992
                                                                                                   ) ls993)), 
          ("extends", 
       (case opt998 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x997 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x997))
       )), ("implements", PrettyRep.List (List.map (fn x1002 => cvtIDENT_EXPR x1002
                                                   ) ls1003)), ("classFixtures", 
          
       (case opt1008 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1007 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1007))
       )), ("instanceFixtures", 
       (case opt1013 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1012 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1012))
       )), ("body", cvtBLOCK x1017), ("protoVars", PrettyRep.List (List.map (fn x1018 => 
                                                                                   cvtVAR_BINDING x1018
                                                                            ) ls1019)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1023 => cvtFUNC x1023
                                                    ) ls1024)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1028 => cvtVAR_BINDING x1028
                                   ) ls1029)), ("instanceMethods", PrettyRep.List (List.map (fn x1033 => 
                                                                                                   cvtFUNC x1033
                                                                                            ) ls1034)), 
          ("vars", PrettyRep.List (List.map (fn x1038 => cvtVAR_BINDING x1038
                                            ) ls1039)), ("methods", PrettyRep.List (List.map (fn x1043 => 
                                                                                                    cvtFUNC x1043
                                                                                             ) ls1044)), 
          ("constructor", 
       (case opt1049 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1048 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC x1048))
       )), ("initializer", PrettyRep.List (List.map (fn x1053 => cvtSTMT x1053
                                                    ) ls1054))]
   and cvtINTERFACE_DEFN {name=x1093, nonnullable=b1094, attrs=x1095, params=ls1097, 
          extends=ls1102, body=x1106} = PrettyRep.Rec [("name", cvtIDENT x1093), 
          ("nonnullable", PrettyRep.Bool b1094), ("attrs", cvtATTRIBUTES x1095), 
          ("params", PrettyRep.List (List.map (fn x1096 => cvtIDENT x1096
                                              ) ls1097)), ("extends", PrettyRep.List (List.map (fn x1101 => 
                                                                                                      cvtIDENT_EXPR x1101
                                                                                               ) ls1102)), 
          ("body", cvtBLOCK x1106)]
   and cvtFOR_ENUM_STMT {ptrn=opt1121, obj=ls1126, defns=ls1131, contLabel=opt1136, 
          body=x1140} = PrettyRep.Rec [("ptrn", 
       (case opt1121 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1120 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1120))
       )), ("obj", PrettyRep.List (List.map (fn x1125 => cvtEXPR x1125
                                            ) ls1126)), ("defns", PrettyRep.List (List.map (fn x1130 => 
                                                                                                  cvtVAR_BINDING x1130
                                                                                           ) ls1131)), 
          ("contLabel", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1135))
       )), ("body", cvtSTMT x1140)]
   and cvtWHILE_STMT {cond=x1152, body=x1153, contLabel=opt1155} = PrettyRep.Rec [("cond", 
          cvtEXPR x1152), ("body", cvtSTMT x1153), ("contLabel", 
       (case opt1155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1154 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1154))
       ))]
   and cvtDIRECTIVES {pragmas=ls1167, defns=ls1172, stmts=ls1177, fixtures=opt1182} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1166 => 
                                                                     cvtPRAGMA x1166
                                                              ) ls1167)), ("defns", 
          PrettyRep.List (List.map (fn x1171 => cvtDEFN x1171
                                   ) ls1172)), ("stmts", PrettyRep.List (List.map (fn x1176 => 
                                                                                         cvtSTMT x1176
                                                                                  ) ls1177)), 
          ("fixtures", 
       (case opt1182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1181 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1181))
       ))]
   and cvtBINDINGS {defns=ls1196, inits=ls1201} = PrettyRep.Rec [("defns", 
          PrettyRep.List (List.map (fn x1195 => cvtVAR_BINDING x1195
                                   ) ls1196)), ("inits", PrettyRep.List (List.map (fn x1200 => 
                                                                                         cvtEXPR x1200
                                                                                  ) ls1201))]
   and cvtCASE {label=opt1215, body=x1219} = PrettyRep.Rec [("label", 
       (case opt1215 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1211 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1210 => 
                                                                                      cvtEXPR x1210
                                                                               ) ls1211)))
       )), ("body", cvtBLOCK x1219)]
   and cvtTYPE_CASE {ptrn=opt1226, body=x1230} = PrettyRep.Rec [("ptrn", 
       (case opt1226 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1225 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1225))
       )), ("body", cvtBLOCK x1230)]
   and cvtFUNC_NAME {kind=x1236, ident=x1237} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1236), 
          ("ident", cvtIDENT x1237)]
   and cvtPACKAGE {name=x1243, body=x1244} = PrettyRep.Rec [("name", cvtUSTRING x1243), 
          ("body", cvtBLOCK x1244)]
   and cvtPROGRAM {packages=ls1251, body=x1255} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1250 => cvtPACKAGE x1250
                                   ) ls1251)), ("body", cvtBLOCK x1255)]
end

