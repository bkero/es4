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
   and cvtFUNC (Func{name=x151, fsig=x152, body=x153, typeParamFixtures=opt155, 
          paramFixtures=opt160, paramInitializers=opt165, bodyFixtures=opt170, 
          bodyInitializers=opt175}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x151), ("fsig", cvtFUNC_SIG x152), ("body", cvtBLOCK x153), 
          ("typeParamFixtures", 
       (case opt155 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x154 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x154))
       )), ("paramFixtures", 
       (case opt160 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x159))
       )), ("paramInitializers", 
       (case opt165 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x164 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x164))
       )), ("bodyFixtures", 
       (case opt170 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x169 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x169))
       )), ("bodyInitializers", 
       (case opt175 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x174 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x174))
       ))]))
   and cvtDEFN (ClassDefn x198) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x198))
     | cvtDEFN (VariableDefn x201) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x201))
     | cvtDEFN (FunctionDefn x204) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x204))
     | cvtDEFN (InterfaceDefn x207) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x207))
     | cvtDEFN (NamespaceDefn x210) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x210))
     | cvtDEFN (TypeDefn x213) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x213))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls217, params=ls222, inits=opt227, 
          returnType=x231, thisType=opt233, hasBoundThis=b237, hasRest=b238}) = 
          PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x216 => cvtIDENT x216
                                   ) ls217)), ("params", PrettyRep.List (List.map (fn x221 => 
                                                                                         cvtVAR_BINDING x221
                                                                                  ) ls222)), 
          ("inits", 
       (case opt227 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x226 => PrettyRep.Ctor ("SOME", SOME (cvtBINDINGS x226))
       )), ("returnType", cvtTYPE_EXPR x231), ("thisType", 
       (case opt233 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x232 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x232))
       )), ("hasBoundThis", PrettyRep.Bool b237), ("hasRest", PrettyRep.Bool b238)]))
   and cvtVAR_BINDING (Binding{pattern=x256, ty=opt258, init=opt263}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("pattern", cvtPATTERN x256), ("ty", 
       (case opt258 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x257 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x257))
       )), ("init", 
       (case opt263 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x262 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x262))
       ))]))
   and cvtTYPE_EXPR (SpecialType x276) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x276))
     | cvtTYPE_EXPR (UnionType ls280) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x279 => 
                                                                                                           cvtTYPE_EXPR x279
                                                                                                    ) ls280)))
     | cvtTYPE_EXPR (ArrayType ls287) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x286 => 
                                                                                                           cvtTYPE_EXPR x286
                                                                                                    ) ls287)))
     | cvtTYPE_EXPR (NominalType{ident=x293}) = PrettyRep.Ctor ("NominalType", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x293)]))
     | cvtTYPE_EXPR (FunctionType x299) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_SIG x299))
     | cvtTYPE_EXPR (ObjectType ls303) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x302 => 
                                                                                                             cvtFIELD_TYPE x302
                                                                                                      ) ls303)))
     | cvtTYPE_EXPR (AppType{base=x309, args=ls311}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x309), ("args", PrettyRep.List (List.map (fn x310 => 
                                                                                                     cvtTYPE_EXPR x310
                                                                                              ) ls311))]))
     | cvtTYPE_EXPR (NullableType{expr=x322, nullable=b323}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x322), ("nullable", PrettyRep.Bool b323)]))
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
     | cvtSTMT (ExprStmt ls333) = PrettyRep.Ctor ("ExprStmt", SOME (PrettyRep.List (List.map (fn x332 => 
                                                                                                    cvtEXPR x332
                                                                                             ) ls333)))
     | cvtSTMT (ForEachStmt x339) = PrettyRep.Ctor ("ForEachStmt", SOME (cvtFOR_ENUM_STMT x339))
     | cvtSTMT (ForInStmt x342) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x342))
     | cvtSTMT (ThrowStmt ls346) = PrettyRep.Ctor ("ThrowStmt", SOME (PrettyRep.List (List.map (fn x345 => 
                                                                                                      cvtEXPR x345
                                                                                               ) ls346)))
     | cvtSTMT (ReturnStmt ls353) = PrettyRep.Ctor ("ReturnStmt", SOME (PrettyRep.List (List.map (fn x352 => 
                                                                                                        cvtEXPR x352
                                                                                                 ) ls353)))
     | cvtSTMT (BreakStmt opt360) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt360 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x359 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x359))
       ))
     | cvtSTMT (ContinueStmt opt367) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt367 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x366 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x366))
       ))
     | cvtSTMT (BlockStmt x373) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x373))
     | cvtSTMT (LabeledStmt(x376, x377)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x376, 
          cvtSTMT x377]))
     | cvtSTMT (LetStmt(ls382, x386)) = PrettyRep.Ctor ("LetStmt", SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x381 => 
                                                                                                                          cvtVAR_BINDING x381
                                                                                                                   ) ls382), 
          cvtSTMT x386]))
     | cvtSTMT (SuperStmt ls391) = PrettyRep.Ctor ("SuperStmt", SOME (PrettyRep.List (List.map (fn x390 => 
                                                                                                      cvtEXPR x390
                                                                                               ) ls391)))
     | cvtSTMT (WhileStmt x397) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x397))
     | cvtSTMT (DoWhileStmt x400) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x400))
     | cvtSTMT (ForStmt{defns=ls404, init=ls409, cond=ls414, update=ls419, 
          contLabel=opt424, body=x428, fixtures=opt430, initializers=opt435}) = 
          PrettyRep.Ctor ("ForStmt", SOME (PrettyRep.Rec [("defns", PrettyRep.List (List.map (fn x403 => 
                                                                                                    cvtVAR_BINDING x403
                                                                                             ) ls404)), 
          ("init", PrettyRep.List (List.map (fn x408 => cvtEXPR x408
                                            ) ls409)), ("cond", PrettyRep.List (List.map (fn x413 => 
                                                                                                cvtEXPR x413
                                                                                         ) ls414)), 
          ("update", PrettyRep.List (List.map (fn x418 => cvtEXPR x418
                                              ) ls419)), ("contLabel", 
       (case opt424 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x423 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x423))
       )), ("body", cvtSTMT x428), ("fixtures", 
       (case opt430 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x429 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x429))
       )), ("initializers", 
       (case opt435 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x434 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x434))
       ))]))
     | cvtSTMT (IfStmt{cnd=x458, thn=x459, els=x460}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x458), ("thn", cvtSTMT x459), 
          ("els", cvtSTMT x460)]))
     | cvtSTMT (WithStmt{obj=ls471, ty=x475, body=x476}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", PrettyRep.List (List.map (fn x470 => 
                                                                       cvtEXPR x470
                                                                ) ls471)), 
          ("ty", cvtTYPE_EXPR x475), ("body", cvtSTMT x476)]))
     | cvtSTMT (TryStmt{body=x486, catches=ls508, finally=opt513}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("body", cvtBLOCK x486), ("catches", PrettyRep.List (List.map (fn {bind=x487, 
                                                                                                    body=x488, 
                                                                                                    fixtures=opt490, 
                                                                                                    initializers=opt495} => 
                                                                                                    PrettyRep.Rec [("bind", 
                                                                                                    cvtVAR_BINDING x487), 
                                                                                                    ("body", 
                                                                                                    cvtBLOCK x488), 
                                                                                                    ("fixtures", 
                                                                                                    
                                                                                                 (case opt490 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x489 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtFIXTURES x489))
                                                                                                 )), 
                                                                                                    ("initializers", 
                                                                                                    
                                                                                                 (case opt495 of
                                                                                                   NONE => 
                                                                                                      PrettyRep.Ctor ("NONE", 
                                                                                                      NONE)
                                                                                                 | SOME x494 => 
                                                                                                      PrettyRep.Ctor ("SOME", 
                                                                                                      SOME (cvtINITIALIZERS x494))
                                                                                                 ))]
                                                                                             ) ls508)), 
          ("finally", 
       (case opt513 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x512 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x512))
       ))]))
     | cvtSTMT (SwitchStmt{cond=ls527, cases=ls532}) = PrettyRep.Ctor ("SwitchStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x526 => 
                                                                        cvtEXPR x526
                                                                 ) ls527)), 
          ("cases", PrettyRep.List (List.map (fn x531 => cvtCASE x531
                                             ) ls532))]))
     | cvtSTMT (SwitchTypeStmt{cond=ls544, ty=x548, cases=ls550}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", PrettyRep.List (List.map (fn x543 => 
                                                                        cvtEXPR x543
                                                                 ) ls544)), 
          ("ty", cvtTYPE_EXPR x548), ("cases", PrettyRep.List (List.map (fn x549 => 
                                                                               cvtTYPE_CASE x549
                                                                        ) ls550))]))
     | cvtSTMT (Dxns{expr=x563}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x563)]))
   and cvtEXPR (TrinaryExpr(x569, x570, x571, x572)) = PrettyRep.Ctor ("TrinaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x569, cvtEXPR x570, cvtEXPR x571, 
          cvtEXPR x572]))
     | cvtEXPR (BinaryExpr(x576, x577, x578)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x576, cvtEXPR x577, cvtEXPR x578]))
     | cvtEXPR (BinaryTypeExpr(x582, x583, x584)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x582, cvtEXPR x583, cvtTYPE_EXPR x584]))
     | cvtEXPR (UnaryExpr(x588, x589)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x588, 
          cvtEXPR x589]))
     | cvtEXPR (TypeExpr x593) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x593))
     | cvtEXPR (NullaryExpr x596) = PrettyRep.Ctor ("NullaryExpr", SOME (cvtNULOP x596))
     | cvtEXPR (YieldExpr opt604) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt604 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls600 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x599 => 
                                                                                     cvtEXPR x599
                                                                              ) ls600)))
       ))
     | cvtEXPR (SuperExpr opt611) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt611 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x610 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x610))
       ))
     | cvtEXPR (LiteralExpr x617) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x617))
     | cvtEXPR (CallExpr{func=x620, actuals=ls622}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x620), ("actuals", PrettyRep.List (List.map (fn x621 => 
                                                                                                   cvtEXPR x621
                                                                                            ) ls622))]))
     | cvtEXPR (ApplyTypeExpr{expr=x633, actuals=ls635}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x633), ("actuals", PrettyRep.List (List.map (fn x634 => 
                                                                                                   cvtTYPE_EXPR x634
                                                                                            ) ls635))]))
     | cvtEXPR (LetExpr{defs=ls647, body=ls652, fixtures=opt657, initializers=opt662}) = 
          PrettyRep.Ctor ("LetExpr", SOME (PrettyRep.Rec [("defs", PrettyRep.List (List.map (fn x646 => 
                                                                                                   cvtVAR_BINDING x646
                                                                                            ) ls647)), 
          ("body", PrettyRep.List (List.map (fn x651 => cvtEXPR x651
                                            ) ls652)), ("fixtures", 
       (case opt657 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x656 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x656))
       )), ("initializers", 
       (case opt662 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x661))
       ))]))
     | cvtEXPR (NewExpr{obj=x677, actuals=ls679}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x677), ("actuals", PrettyRep.List (List.map (fn x678 => 
                                                                                                  cvtEXPR x678
                                                                                           ) ls679))]))
     | cvtEXPR (FunExpr{ident=opt691, fsig=x695, body=x696, typeParamFixtures=opt698, 
          paramFixtures=opt703, paramInitializers=opt708, bodyFixtures=opt713, 
          bodyInitializers=opt718}) = PrettyRep.Ctor ("FunExpr", SOME (PrettyRep.Rec [("ident", 
          
       (case opt691 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x690 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x690))
       )), ("fsig", cvtFUNC_SIG x695), ("body", cvtBLOCK x696), ("typeParamFixtures", 
          
       (case opt698 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x697 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x697))
       )), ("paramFixtures", 
       (case opt703 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x702 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x702))
       )), ("paramInitializers", 
       (case opt708 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x707 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x707))
       )), ("bodyFixtures", 
       (case opt713 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x712 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x712))
       )), ("bodyInitializers", 
       (case opt718 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x717 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x717))
       ))]))
     | cvtEXPR (ObjectRef{base=x741, ident=x742}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x741), ("ident", cvtIDENT_EXPR x742)]))
     | cvtEXPR (LexicalRef{ident=x750}) = PrettyRep.Ctor ("LexicalRef", SOME (PrettyRep.Rec [("ident", 
          cvtIDENT_EXPR x750)]))
     | cvtEXPR (SetExpr(x756, x757, x758)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x756, 
          cvtPATTERN x757, cvtEXPR x758]))
     | cvtEXPR (ListExpr ls763) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x762 => 
                                                                                                    cvtEXPR x762
                                                                                             ) ls763)))
     | cvtEXPR (SliceExpr(ls770, ls775, ls780)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x769 => cvtEXPR x769
                                                          ) ls770), PrettyRep.List (List.map (fn x774 => 
                                                                                                    cvtEXPR x774
                                                                                             ) ls775), 
          PrettyRep.List (List.map (fn x779 => cvtEXPR x779
                                   ) ls780)]))
   and cvtIDENT_EXPR (QualifiedIdentifier{qual=x787, ident=x788}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x787), ("ident", cvtUSTRING x788)]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x796, expr=x797}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x796), ("expr", cvtEXPR x797)]))
     | cvtIDENT_EXPR (AttributeIdentifier x805) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x805))
     | cvtIDENT_EXPR (Identifier{ident=x808, openNamespaces=ls810}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x808), ("openNamespaces", 
          PrettyRep.List (List.map (fn x809 => cvtNAMESPACE x809
                                   ) ls810))]))
     | cvtIDENT_EXPR (ExpressionIdentifier x821) = PrettyRep.Ctor ("ExpressionIdentifier", 
          SOME (cvtEXPR x821))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x824, typeParams=ls826}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x824), ("typeParams", 
          PrettyRep.List (List.map (fn x825 => cvtTYPE_EXPR x825
                                   ) ls826))]))
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
     | cvtLITERAL (LiteralNumber r839) = PrettyRep.Ctor ("LiteralNumber", SOME (PrettyRep.Real r839))
     | cvtLITERAL (LiteralBoolean b842) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b842))
     | cvtLITERAL (LiteralString x845) = PrettyRep.Ctor ("LiteralString", SOME (cvtUSTRING x845))
     | cvtLITERAL (LiteralArray{exprs=ls849, ty=opt854}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x848 => 
                                                                         cvtEXPR x848
                                                                  ) ls849)), 
          ("ty", 
       (case opt854 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x853 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x853))
       ))]))
     | cvtLITERAL (LiteralXML ls866) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x865 => 
                                                                                                           cvtEXPR x865
                                                                                                    ) ls866)))
     | cvtLITERAL (LiteralNamespace x872) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x872))
     | cvtLITERAL (LiteralObject{expr=ls876, ty=opt881}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x875 => 
                                                                        cvtFIELD x875
                                                                 ) ls876)), 
          ("ty", 
       (case opt881 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x880 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x880))
       ))]))
     | cvtLITERAL (LiteralRegExp{str=x892}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", cvtUSTRING x892)]))
   and cvtBLOCK (Block x898) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x898))
   and cvtPATTERN (ObjectPattern ls902) = PrettyRep.Ctor ("ObjectPattern", 
          SOME (PrettyRep.List (List.map (fn x901 => cvtFIELD_PATTERN x901
                                         ) ls902)))
     | cvtPATTERN (ArrayPattern ls909) = PrettyRep.Ctor ("ArrayPattern", SOME (PrettyRep.List (List.map (fn x908 => 
                                                                                                               cvtPATTERN x908
                                                                                                        ) ls909)))
     | cvtPATTERN (SimplePattern x915) = PrettyRep.Ctor ("SimplePattern", SOME (cvtEXPR x915))
     | cvtPATTERN (IdentifierPattern x918) = PrettyRep.Ctor ("IdentifierPattern", 
          SOME (cvtIDENT_EXPR x918))
   and cvtFIXTURE (NamespaceFixture x921) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x921))
     | cvtFIXTURE (ClassFixture x924) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLASS_DEFN x924))
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
     | cvtFIXTURE (TypeFixture x928) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x928))
     | cvtFIXTURE (ValFixture{ty=x931, readOnly=b932, isOverride=b933}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x931), ("readOnly", PrettyRep.Bool b932), 
          ("isOverride", PrettyRep.Bool b933)]))
     | cvtFIXTURE (VirtualValFixture{ty=x943, getter=opt945, setter=opt950}) = 
          PrettyRep.Ctor ("VirtualValFixture", SOME (PrettyRep.Rec [("ty", 
          cvtTYPE_EXPR x943), ("getter", 
       (case opt945 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x944 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x944))
       )), ("setter", 
       (case opt950 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x949 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x949))
       ))]))
   and cvtINITIALIZER (VarInit(x963, x964)) = PrettyRep.Ctor ("VarInit", SOME (PrettyRep.Tuple [cvtNAME x963, 
          cvtEXPR x964]))
     | cvtINITIALIZER (FunInit(x968, x969)) = PrettyRep.Ctor ("FunInit", SOME (PrettyRep.Tuple [cvtNAME x968, 
          cvtFUNC x969]))
   and cvtFIELD {kind=x973, name=x974, init=x975} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x973), ("name", cvtIDENT x974), ("init", cvtEXPR x975)]
   and cvtFIELD_PATTERN {name=x983, ptrn=x984} = PrettyRep.Rec [("name", cvtIDENT x983), 
          ("ptrn", cvtPATTERN x984)]
   and cvtFIELD_TYPE {name=x990, ty=x991} = PrettyRep.Rec [("name", cvtIDENT x990), 
          ("ty", cvtTYPE_EXPR x991)]
   and cvtTYPED_IDENT {name=x997, ty=opt999} = PrettyRep.Rec [("name", cvtIDENT x997), 
          ("ty", 
       (case opt999 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x998 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x998))
       ))]
   and cvtATTRIBUTES {ns=x1008, override=b1009, static=b1010, final=b1011, 
          dynamic=b1012, prototype=b1013, native=b1014, rest=b1015} = PrettyRep.Rec [("ns", 
          cvtEXPR x1008), ("override", PrettyRep.Bool b1009), ("static", PrettyRep.Bool b1010), 
          ("final", PrettyRep.Bool b1011), ("dynamic", PrettyRep.Bool b1012), 
          ("prototype", PrettyRep.Bool b1013), ("native", PrettyRep.Bool b1014), 
          ("rest", PrettyRep.Bool b1015)]
   and cvtFUNC_DEFN {kind=x1033, ns=x1034, final=b1035, native=b1036, override=b1037, 
          prototype=b1038, static=b1039, func=x1040} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1033), ("ns", cvtEXPR x1034), ("final", PrettyRep.Bool b1035), 
          ("native", PrettyRep.Bool b1036), ("override", PrettyRep.Bool b1037), 
          ("prototype", PrettyRep.Bool b1038), ("static", PrettyRep.Bool b1039), 
          ("func", cvtFUNC x1040)]
   and cvtVAR_DEFN {kind=x1058, ns=x1059, static=b1060, prototype=b1061, bindings=ls1063} = 
          PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1058), ("ns", cvtEXPR x1059), 
          ("static", PrettyRep.Bool b1060), ("prototype", PrettyRep.Bool b1061), 
          ("bindings", PrettyRep.List (List.map (fn x1062 => cvtVAR_BINDING x1062
                                                ) ls1063))]
   and cvtINITIALIZERS ls1079 = PrettyRep.List (List.map (fn x1078 => cvtINITIALIZER x1078
                                                         ) ls1079)
   and cvtFIXTURES ls1086 = PrettyRep.List (List.map (fn (x1083, x1084) => 
                                                            PrettyRep.Tuple [cvtNAME x1083, 
                                                            cvtFIXTURE x1084]
                                                     ) ls1086)
   and cvtNAMESPACE_DEFN {ident=x1090, ns=x1091, init=opt1093} = PrettyRep.Rec [("ident", 
          cvtIDENT x1090), ("ns", cvtEXPR x1091), ("init", 
       (case opt1093 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1092 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1092))
       ))]
   and cvtCLASS_DEFN {ident=x1104, ns=x1105, nonnullable=b1106, dynamic=b1107, 
          final=b1108, params=ls1110, extends=opt1115, implements=ls1120, body=x1124, 
          classFixtures=opt1126, instanceFixtures=opt1131, classInitializers=opt1136, 
          protoInitializers=opt1141, instanceInitializers=opt1146, constructor=opt1151, 
          initializer=ls1156, protoVars=ls1161, protoMethods=ls1166, instanceVars=ls1171, 
          instanceMethods=ls1176, vars=ls1181, methods=ls1186} = PrettyRep.Rec [("ident", 
          cvtIDENT x1104), ("ns", cvtEXPR x1105), ("nonnullable", PrettyRep.Bool b1106), 
          ("dynamic", PrettyRep.Bool b1107), ("final", PrettyRep.Bool b1108), 
          ("params", PrettyRep.List (List.map (fn x1109 => cvtIDENT x1109
                                              ) ls1110)), ("extends", 
       (case opt1115 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1114 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1114))
       )), ("implements", PrettyRep.List (List.map (fn x1119 => cvtIDENT_EXPR x1119
                                                   ) ls1120)), ("body", cvtBLOCK x1124), 
          ("classFixtures", 
       (case opt1126 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1125 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1125))
       )), ("instanceFixtures", 
       (case opt1131 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1130 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1130))
       )), ("classInitializers", 
       (case opt1136 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1135 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x1135))
       )), ("protoInitializers", 
       (case opt1141 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1140 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x1140))
       )), ("instanceInitializers", 
       (case opt1146 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1145 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x1145))
       )), ("constructor", 
       (case opt1151 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1150 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1150))
       )), ("initializer", PrettyRep.List (List.map (fn x1155 => cvtSTMT x1155
                                                    ) ls1156)), ("protoVars", 
          PrettyRep.List (List.map (fn x1160 => cvtVAR_DEFN x1160
                                   ) ls1161)), ("protoMethods", PrettyRep.List (List.map (fn x1165 => 
                                                                                                cvtFUNC_DEFN x1165
                                                                                         ) ls1166)), 
          ("instanceVars", PrettyRep.List (List.map (fn x1170 => cvtVAR_DEFN x1170
                                                    ) ls1171)), ("instanceMethods", 
          PrettyRep.List (List.map (fn x1175 => cvtFUNC_DEFN x1175
                                   ) ls1176)), ("vars", PrettyRep.List (List.map (fn x1180 => 
                                                                                        cvtVAR_DEFN x1180
                                                                                 ) ls1181)), 
          ("methods", PrettyRep.List (List.map (fn x1185 => cvtFUNC_DEFN x1185
                                               ) ls1186))]
   and cvtINTERFACE_DEFN {ident=x1235, ns=x1236, nonnullable=b1237, params=ls1239, 
          extends=ls1244, body=x1248} = PrettyRep.Rec [("ident", cvtIDENT x1235), 
          ("ns", cvtEXPR x1236), ("nonnullable", PrettyRep.Bool b1237), ("params", 
          PrettyRep.List (List.map (fn x1238 => cvtIDENT x1238
                                   ) ls1239)), ("extends", PrettyRep.List (List.map (fn x1243 => 
                                                                                           cvtIDENT_EXPR x1243
                                                                                    ) ls1244)), 
          ("body", cvtBLOCK x1248)]
   and cvtTYPE_DEFN {ident=x1262, ns=x1263, init=x1264} = PrettyRep.Rec [("ident", 
          cvtIDENT x1262), ("ns", cvtEXPR x1263), ("init", cvtTYPE_EXPR x1264)]
   and cvtFOR_ENUM_STMT {ptrn=opt1273, obj=ls1278, defns=ls1283, contLabel=opt1288, 
          body=x1292, fixtures=opt1294, initializers=opt1299} = PrettyRep.Rec [("ptrn", 
          
       (case opt1273 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1272 => PrettyRep.Ctor ("SOME", SOME (cvtPATTERN x1272))
       )), ("obj", PrettyRep.List (List.map (fn x1277 => cvtEXPR x1277
                                            ) ls1278)), ("defns", PrettyRep.List (List.map (fn x1282 => 
                                                                                                  cvtVAR_BINDING x1282
                                                                                           ) ls1283)), 
          ("contLabel", 
       (case opt1288 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1287 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1287))
       )), ("body", cvtSTMT x1292), ("fixtures", 
       (case opt1294 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1293 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1293))
       )), ("initializers", 
       (case opt1299 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1298 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x1298))
       ))]
   and cvtWHILE_STMT {cond=x1318, body=x1319, contLabel=opt1321} = PrettyRep.Rec [("cond", 
          cvtEXPR x1318), ("body", cvtSTMT x1319), ("contLabel", 
       (case opt1321 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x1320))
       ))]
   and cvtDIRECTIVES {pragmas=ls1333, defns=ls1338, stmts=ls1343, fixtures=opt1348, 
          initializers=opt1353} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1332 => 
                                                                                             cvtPRAGMA x1332
                                                                                      ) ls1333)), 
          ("defns", PrettyRep.List (List.map (fn x1337 => cvtDEFN x1337
                                             ) ls1338)), ("stmts", PrettyRep.List (List.map (fn x1342 => 
                                                                                                   cvtSTMT x1342
                                                                                            ) ls1343)), 
          ("fixtures", 
       (case opt1348 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1347 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1347))
       )), ("initializers", 
       (case opt1353 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1352 => PrettyRep.Ctor ("SOME", SOME (cvtINITIALIZERS x1352))
       ))]
   and cvtBINDINGS {b=ls1369, i=ls1374} = PrettyRep.Rec [("b", PrettyRep.List (List.map (fn x1368 => 
                                                                                               cvtVAR_BINDING x1368
                                                                                        ) ls1369)), 
          ("i", PrettyRep.List (List.map (fn x1373 => cvtEXPR x1373
                                         ) ls1374))]
   and cvtCASE {label=opt1388, body=x1392} = PrettyRep.Rec [("label", 
       (case opt1388 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME ls1384 => PrettyRep.Ctor ("SOME", SOME (PrettyRep.List (List.map (fn x1383 => 
                                                                                      cvtEXPR x1383
                                                                               ) ls1384)))
       )), ("body", cvtBLOCK x1392)]
   and cvtTYPE_CASE {ptrn=opt1399, body=x1403} = PrettyRep.Rec [("ptrn", 
       (case opt1399 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1398 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_BINDING x1398))
       )), ("body", cvtBLOCK x1403)]
   and cvtFUNC_NAME {kind=x1409, ident=x1410} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1409), 
          ("ident", cvtIDENT x1410)]
   and cvtPACKAGE {name=x1416, body=x1417} = PrettyRep.Rec [("name", cvtUSTRING x1416), 
          ("body", cvtBLOCK x1417)]
   and cvtPROGRAM {packages=ls1424, body=x1428} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1423 => cvtPACKAGE x1423
                                   ) ls1424)), ("body", cvtBLOCK x1428)]
end

