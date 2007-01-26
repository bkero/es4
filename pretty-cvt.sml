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
     | cvtTYPE_EXPR (NominalType{ident=x265, nullable=opt267}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x265), ("nullable", 
          
       (case opt267 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME b266 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Bool b266))
       ))]))
     | cvtTYPE_EXPR (FunctionType x278) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x278))
     | cvtTYPE_EXPR (ObjectType ls282) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x281 => 
                                                                                                             cvtFIELD_TYPE x281
                                                                                                      ) ls282)))
     | cvtTYPE_EXPR (AppType{base=x288, args=ls290}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x288), ("args", PrettyRep.List (List.map (fn x289 => 
                                                                                                     cvtTYPE_EXPR x289
                                                                                              ) ls290))]))
     | cvtTYPE_EXPR (NullableType{expr=x301, nullable=b302}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x301), ("nullable", PrettyRep.Bool b302)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls312) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x311 => 
                                                                                                    cvtEXPR x311
                                                                                             ) ls312)))
     | cvtSTMT (ForEachStmt x318) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x318))
     | cvtSTMT (ForInStmt x321) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x321))
     | cvtSTMT (ThrowStmt ls325) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x324 => 
                                                                                                      cvtEXPR x324
                                                                                               ) ls325)))
     | cvtSTMT (ReturnStmt ls332) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x331 => 
                                                                                                        cvtEXPR x331
                                                                                                 ) ls332)))
     | cvtSTMT (BreakStmt opt339) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt339 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x338 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x338))
       ))
     | cvtSTMT (ContinueStmt opt346) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x345 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x345))
       ))
     | cvtSTMT (BlockStmt x352) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x352))
     | cvtSTMT (LabeledStmt(x355, x356)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x355, 
          cvtSTMT x356]))
     | cvtSTMT (LetStmt(ls361, x365)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x360 => 
                                                                                                                          cvtVAR_BINDING x360
                                                                                                                   ) ls361), 
          cvtSTMT x365]))
     | cvtSTMT (SuperStmt ls370) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x369 => 
                                                                                                      cvtEXPR x369
                                                                                               ) ls370)))
     | cvtSTMT (WhileStmt x376) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x376))
     | cvtSTMT (DoWhileStmt x379) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x379))
     | cvtSTMT (ForStmt{defns=ls383, fixtures=opt388, init=ls393, cond=ls398, 
          update=ls403, contLabel=opt408, body=x412}) = PrettyRep.Ctor ("ForStmt", 
          SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x382 => 
                                                                         cvtVAR_BINDING x382
                                                                  ) ls383)), 
          ("fixtures", 
       (case opt388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x387 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x387))
       )), ("init", PrettyRep.List (List.map (fn x392 => cvtEXPR x392
                                             ) ls393)), ("cond", PrettyRep.List (List.map (fn x397 => 
                                                                                                 cvtEXPR x397
                                                                                          ) ls398)), 
          ("update", PrettyRep.List (List.map (fn x402 => cvtEXPR x402
                                              ) ls403)), ("contLabel", 
       (case opt408 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x407 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x407))
       )), ("body", cvtSTMT x412)]))
     | cvtSTMT (IfStmt{cnd=x430, thn=x431, els=x432}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x430), ("thn", cvtSTMT x431), 
          ("els", cvtSTMT x432)]))
     | cvtSTMT (WithStmt{obj=ls443, ty=x447, body=x448}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x442 => 
                                                                       cvtEXPR x442
                                                                ) ls443)), 
          ("ty", cvtTYPE_EXPR x447), ("body", cvtSTMT x448)]))
     | cvtSTMT (TryStmt{body=x458, catches=ls473, finally=opt478}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x458), ("catches", PrettyRep.List (List.map (fn {bind=x459, 
                                                                                                    fixtures=opt461, 
                                                                                                    body=x465} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x459), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt461 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x460 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x460))
                                                                                                 )), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x465)]
                                                                                             ) ls473)), 
          ("finally", 
       (case opt478 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x477 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x477))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls492, cases=ls497}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x491 => 
                                                                        cvtEXPR x491
                                                                 ) ls492)), 
          ("cases", PrettyRep.List (List.map (fn x496 => cvtCASE x496
                                             ) ls497))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls509, ty=x513, cases=ls515}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x508 => 
                                                                        cvtEXPR x508
                                                                 ) ls509)), 
          ("ty", cvtTYPE_EXPR x513), ("cases", PrettyRep.List (List.map (fn x514 => 
                                                                               cvtTYPE_CASE x514
                                                                        ) ls515))]))
     | cvtSTMT (Dxns{expr=x528}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x528)]))
   and cvtEXPR (TrinaryExpr(x534, x535, x536, x537)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x534, cvtEXPR x535, cvtEXPR x536, 
          cvtEXPR x537]))
     | cvtEXPR (BinaryExpr(x541, x542, x543)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x541, cvtEXPR x542, cvtEXPR x543]))
     | cvtEXPR (BinaryTypeExpr(x547, x548, x549)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x547, cvtEXPR x548, cvtTYPE_EXPR x549]))
     | cvtEXPR (UnaryExpr(x553, x554)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x553, 
          cvtEXPR x554]))
     | cvtEXPR (TypeExpr x558) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x558))
     | cvtEXPR (NullaryExpr x561) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x561))
     | cvtEXPR (YieldExpr opt569) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt569 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls565 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x564 => 
                                                                                     cvtEXPR x564
                                                                              ) ls565)))
       ))
     | cvtEXPR (SuperExpr opt576) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt576 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x575 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x575))
       ))
     | cvtEXPR (LiteralExpr x582) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x582))
     | cvtEXPR (CallExpr{func=x585, actuals=ls587}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x585), ("actuals", PrettyRep.List (List.map (fn x586 => 
                                                                                                   cvtEXPR x586
                                                                                            ) ls587))]))
     | cvtEXPR (ApplyTypeExpr{expr=x598, actuals=ls600}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x598), ("actuals", PrettyRep.List (List.map (fn x599 => 
                                                                                                   cvtTYPE_EXPR x599
                                                                                            ) ls600))]))
     | cvtEXPR (LetExpr{defs=ls612, body=ls617, fixtures=opt622}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x611 => 
                                                                        cvtVAR_BINDING x611
                                                                 ) ls612)), 
          ("body", PrettyRep.List (List.map (fn x616 => cvtEXPR x616
                                            ) ls617)), ("fixtures", 
       (case opt622 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x621 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x621))
       ))]))
     | cvtEXPR (NewExpr{obj=x635, actuals=ls637}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x635), ("actuals", PrettyRep.List (List.map (fn x636 => 
                                                                                                  cvtEXPR x636
                                                                                           ) ls637))]))
     | cvtEXPR (FunExpr{ident=opt649, fsig=x653, body=x654, fixtures=opt656}) = 
          PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
       (case opt649 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x648 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x648))
       )), ("fsig", cvtFUNC_SIG x653), ("body", cvtBLOCK x654), ("fixtures", 
          
       (case opt656 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x655 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x655))
       ))]))
     | cvtEXPR (ObjectRef{base=x671, ident=x672}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x671), ("ident", cvtIDENT_EXPR x672)]))
     | cvtEXPR (LexicalRef{ident=x680}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x680)]))
     | cvtEXPR (SetExpr(x686, x687, x688)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x686, 
          cvtPATTERN x687, cvtEXPR x688]))
     | cvtEXPR (ListExpr ls693) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x692 => 
                                                                                                    cvtEXPR x692
                                                                                             ) ls693)))
     | cvtEXPR (SliceExpr(ls700, ls705, ls710)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x699 => cvtEXPR x699
                                                          ) ls700), PrettyRep.List (List.map (fn x704 => 
                                                                                                    cvtEXPR x704
                                                                                             ) ls705), 
          PrettyRep.List (List.map (fn x709 => cvtEXPR x709
                                   ) ls710)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x717, ident=x718}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x717), ("ident", cvtUSTRING x718)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x726, expr=x727}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x726), ("expr", cvtEXPR x727)]))
     | cvtIDENT_EXPR (AttributeIdentifier x735) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x735))
     | cvtIDENT_EXPR (Identifier{ident=x738, openNamespaces=ls740}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x738), ("openNamespaces", 
          PrettyRep.List (List.map (fn x739 => cvtNAMESPACE x739
                                   ) ls740))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x751) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x751))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x754, typeParams=ls756}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x754), ("typeParams", 
          PrettyRep.List (List.map (fn x755 => cvtTYPE_EXPR x755
                                   ) ls756))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r769) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r769))
     | cvtLITERAL (LiteralBoolean b772) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b772))
     | cvtLITERAL (LiteralString x775) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x775))
     | cvtLITERAL (LiteralArray{exprs=ls779, ty=opt784}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x778 => 
                                                                         cvtEXPR x778
                                                                  ) ls779)), 
          ("ty", 
       (case opt784 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x783 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x783))
       ))]))
     | cvtLITERAL (LiteralXML ls796) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x795 => 
                                                                                                           cvtEXPR x795
                                                                                                    ) ls796)))
     | cvtLITERAL (LiteralNamespace x802) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x802))
     | cvtLITERAL (LiteralObject{expr=ls806, ty=opt811}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x805 => 
                                                                        cvtFIELD x805
                                                                 ) ls806)), 
          ("ty", 
       (case opt811 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x810 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x810))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x822}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x822)]))
   and cvtBLOCK (Block x828) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x828))
   and cvtPATTERN (ObjectPattern ls832) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x831 => cvtFIELD_PATTERN x831
                                         ) ls832)))
     | cvtPATTERN (ArrayPattern ls839) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x838 => 
                                                                                                               cvtPATTERN x838
                                                                                                        ) ls839)))
     | cvtPATTERN (SimplePattern x845) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x845))
     | cvtPATTERN (IdentifierPattern x848) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x848))
   and cvtFIXTURE (NamespaceFixture x851) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x851))
     | cvtFIXTURE (ClassFixture x854) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x854))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x858) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x858))
     | cvtFIXTURE (ValFixture{ty=x861, readOnly=b862, isOverride=b863}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x861), ("readOnly", PrettyRep.Bool b862), 
          ("isOverride", PrettyRep.Bool b863)]))
     | cvtFIXTURE (VirtualValFixture{ty=x873, getter=opt875, setter=opt880}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x873), ("getter", 
       (case opt875 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x874 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x874))
       )), ("setter", 
       (case opt880 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x879 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x879))
       ))]))
   and cvtFIELD {kind=x893, name=x894, init=x895} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x893), ("name", cvtIDENT x894), ("init", cvtEXPR x895)]
   and cvtFIELD_PATTERN {name=x903, ptrn=x904} = PrettyRep.Rec [("name", cvtIDENT x903), 
          ("ptrn", cvtPATTERN x904)]
   and cvtFIELD_TYPE {name=x910, ty=x911} = PrettyRep.Rec [("name", cvtIDENT x910), 
          ("ty", cvtTYPE_EXPR x911)]
   and cvtTYPED_IDENT {name=x917, ty=opt919} = PrettyRep.Rec [("name", cvtIDENT x917), 
          ("ty", 
       (case opt919 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x918 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x918))
       ))]
   and cvtATTRIBUTES {ns=x928, override=b929, static=b930, final=b931, dynamic=b932, 
          prototype=b933, native=b934, rest=b935} = PrettyRep.Rec [("ns", cvtEXPR x928), 
          ("override", PrettyRep.Bool b929), ("static", PrettyRep.Bool b930), 
          ("final", PrettyRep.Bool b931), ("dynamic", PrettyRep.Bool b932), 
          ("prototype", PrettyRep.Bool b933), ("native", PrettyRep.Bool b934), 
          ("rest", PrettyRep.Bool b935)]
   and cvtFUNC_DEFN {kind=x953, ns=x954, final=b955, native=b956, override=b957, 
          prototype=b958, static=b959, func=x960} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x953), ("ns", cvtEXPR x954), ("final", PrettyRep.Bool b955), 
          ("native", PrettyRep.Bool b956), ("override", PrettyRep.Bool b957), 
          ("prototype", PrettyRep.Bool b958), ("static", PrettyRep.Bool b959), 
          ("func", cvtFUNC x960)]
   and cvtVAR_DEFN {kind=x978, ns=x979, static=b980, prototype=b981, bindings=ls983} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x978), ("ns", cvtEXPR x979), 
          ("static", PrettyRep.Bool b980), ("prototype", PrettyRep.Bool b981), 
          ("bindings", PrettyRep.List (List.map (fn x982 => cvtVAR_BINDING x982
                                                ) ls983))]
   and cvtFIXTURES ls1001 = PrettyRep.List (List.map (fn (x998, x999) => PrettyRep.Tuple [cvtNAME x998, 
                                                            cvtFIXTURE x999]
                                                     ) ls1001)
   and cvtNAMESPACE_DEFN {ident=x1005, ns=x1006, init=opt1008} = PrettyRep.Rec [("ident", 
          cvtIDENT x1005), ("ns", cvtEXPR x1006), ("init", 
       (case opt1008 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1007 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1007))
       ))]
   and cvtCLASS_DEFN {name=x1019, ns=x1020, nonnullable=b1021, dynamic=b1022, 
          final=b1023, params=ls1025, extends=opt1030, implements=ls1035, classFixtures=opt1040, 
          instanceFixtures=opt1045, body=x1049, protoVars=ls1051, protoMethods=ls1056, 
          instanceVars=ls1061, instanceMethods=ls1066, vars=ls1071, methods=ls1076, 
          constructor=opt1081, initializer=ls1086} = PrettyRep.Rec [("name", 
          cvtIDENT x1019), ("ns", cvtEXPR x1020), ("nonnullable", PrettyRep.Bool b1021), 
          ("dynamic", PrettyRep.Bool b1022), ("final", PrettyRep.Bool b1023), 
          ("params", PrettyRep.List (List.map (fn x1024 => cvtIDENT x1024
                                              ) ls1025)), ("extends", 
       (case opt1030 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1029 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1029))
       )), ("implements", PrettyRep.List (List.map (fn x1034 => cvtIDENT_EXPR x1034
                                                   ) ls1035)), ("classFixtures", 
          
       (case opt1040 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1039 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1039))
       )), ("instanceFixtures", 
       (case opt1045 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1044 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1044))
       )), ("body", cvtBLOCK x1049), ("protoVars", PrettyRep.List (List.map (fn x1050 => 
                                                                                   cvtVAR_BINDING x1050
                                                                            ) ls1051)), 
          ("protoMethods", PrettyRep.List (List.map (fn x1055 => cvtFUNC_DEFN x1055
                                                    ) ls1056)), ("instanceVars", 
          PrettyRep.List (List.map (fn x1060 => cvtVAR_BINDING x1060
                                   ) ls1061)), ("instanceMethods", PrettyRep.List (List.map (fn x1065 => 
                                                                                                   cvtFUNC_DEFN x1065
                                                                                            ) ls1066)), 
          ("vars", PrettyRep.List (List.map (fn x1070 => cvtVAR_BINDING x1070
                                            ) ls1071)), ("methods", PrettyRep.List (List.map (fn x1075 => 
                                                                                                    cvtFUNC_DEFN x1075
                                                                                             ) ls1076)), 
          ("constructor", 
       (case opt1081 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1080 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1080))
       )), ("initializer", PrettyRep.List (List.map (fn x1085 => cvtSTMT x1085
                                                    ) ls1086))]
   and cvtINTERFACE_DEFN {name=x1129, ns=x1130, nonnullable=b1131, params=ls1133, 
          extends=ls1138, body=x1142} = PrettyRep.Rec [("name", cvtIDENT x1129), 
          ("ns", cvtEXPR x1130), ("nonnullable", PrettyRep.Bool b1131), ("params", 
          PrettyRep.List (List.map (fn x1132 => cvtIDENT x1132
                                   ) ls1133)), ("extends", PrettyRep.List (List.map (fn x1137 => 
                                                                                           cvtIDENT_EXPR x1137
                                                                                    ) ls1138)), 
          ("body", cvtBLOCK x1142)]
   and cvtTYPE_DEFN {name=x1156, ns=x1157, init=x1158} = PrettyRep.Rec [("name", 
          cvtIDENT x1156), ("ns", cvtEXPR x1157), ("init", cvtTYPE_EXPR x1158)]
   and cvtFOR_ENUM_STMT {ptrn=opt1167, obj=ls1172, defns=ls1177, fixtures=opt1182, 
          contLabel=opt1187, body=x1191} = PrettyRep.Rec [("ptrn", 
       (case opt1167 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1166 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1166))
       )), ("obj", PrettyRep.List (List.map (fn x1171 => cvtEXPR x1171
                                            ) ls1172)), ("defns", PrettyRep.List (List.map (fn x1176 => 
                                                                                                  cvtVAR_BINDING x1176
                                                                                           ) ls1177)), 
          ("fixtures", 
       (case opt1182 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1181 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1181))
       )), ("contLabel", 
       (case opt1187 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1186 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1186))
       )), ("body", cvtSTMT x1191)]
   and cvtWHILE_STMT {cond=x1205, body=x1206, contLabel=opt1208} = PrettyRep.Rec [("cond", 
          cvtEXPR x1205), ("body", cvtSTMT x1206), ("contLabel", 
       (case opt1208 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1207 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1207))
       ))]
   and cvtDIRECTIVES {pragmas=ls1220, defns=ls1225, stmts=ls1230, fixtures=opt1235} = 
          PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1219 => 
                                                                     cvtPRAGMA x1219
                                                              ) ls1220)), ("defns", 
          PrettyRep.List (List.map (fn x1224 => cvtDEFN x1224
                                   ) ls1225)), ("stmts", PrettyRep.List (List.map (fn x1229 => 
                                                                                         cvtSTMT x1229
                                                                                  ) ls1230)), 
          ("fixtures", 
       (case opt1235 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1234 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1234))
       ))]
   and cvtBINDINGS {b=ls1249, i=ls1254} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1248 => 
                                                                                               cvtVAR_BINDING x1248
                                                                                        ) ls1249)), 
          ("i", PrettyRep.List (List.map (fn x1253 => cvtEXPR x1253
                                         ) ls1254))]
   and cvtCASE {label=opt1268, body=x1272} = PrettyRep.Rec [("label", 
       (case opt1268 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1264 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1263 => 
                                                                                      cvtEXPR x1263
                                                                               ) ls1264)))
       )), ("body", cvtBLOCK x1272)]
   and cvtTYPE_CASE {ptrn=opt1279, body=x1283} = PrettyRep.Rec [("ptrn", 
       (case opt1279 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1278 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1278))
       )), ("body", cvtBLOCK x1283)]
   and cvtFUNC_NAME {kind=x1289, ident=x1290} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1289), 
          ("ident", cvtIDENT x1290)]
   and cvtPACKAGE {name=x1296, body=x1297} = PrettyRep.Rec [("name", cvtUSTRING x1296), 
          ("body", cvtBLOCK x1297)]
   and cvtPROGRAM {packages=ls1304, body=x1308} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1303 => cvtPACKAGE x1303
                                   ) ls1304)), ("body", cvtBLOCK x1308)]
end

