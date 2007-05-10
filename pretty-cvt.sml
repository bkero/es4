structure PrettyCvt = struct
   open Ast
   fun cvtSOURCE_POS {line=n0, col=n1} = PrettyRep.Rec [("line", PrettyRep.Int n0), 
          ("col", PrettyRep.Int n1)]
   and cvtLOC {file=s7, span=(x8, x9), post_newline=b11} = PrettyRep.Rec [("file", 
          PrettyRep.String s7), ("span", PrettyRep.Tuple [cvtSOURCE_POS x8, 
          cvtSOURCE_POS x9]), ("post_newline", PrettyRep.Bool b11)]
   and cvtIDENT s19 = PrettyRep.UniStr s19
   and cvtNAMESPACE (Intrinsic) = PrettyRep.Ctor ("Intrinsic", NONE)
     | cvtNAMESPACE (OperatorNamespace) = PrettyRep.Ctor ("OperatorNamespace", 
          NONE)
     | cvtNAMESPACE (Private x22) = PrettyRep.Ctor ("Private", SOME (cvtIDENT x22))
     | cvtNAMESPACE (Protected x25) = PrettyRep.Ctor ("Protected", SOME (cvtIDENT x25))
     | cvtNAMESPACE (Public x28) = PrettyRep.Ctor ("Public", SOME (cvtIDENT x28))
     | cvtNAMESPACE (Internal x31) = PrettyRep.Ctor ("Internal", SOME (cvtIDENT x31))
     | cvtNAMESPACE (UserNamespace s34) = PrettyRep.Ctor ("UserNamespace", 
          SOME (PrettyRep.UniStr s34))
     | cvtNAMESPACE (AnonUserNamespace n37) = PrettyRep.Ctor ("AnonUserNamespace", 
          SOME (PrettyRep.Int n37))
     | cvtNAMESPACE (LimitedNamespace(x40, x41)) = PrettyRep.Ctor ("LimitedNamespace", 
          SOME (PrettyRep.Tuple [cvtIDENT x40, cvtNAMESPACE x41]))
   and cvtNAME {ns=x45, id=x46} = PrettyRep.Rec [("ns", cvtNAMESPACE x45), 
          ("id", cvtIDENT x46)]
   and cvtMULTINAME {nss=ls57, id=x61} = PrettyRep.Rec [("nss", PrettyRep.List (List.map (fn ls53 => 
                                                                                                PrettyRep.List (List.map (fn x52 => 
                                                                                                                                cvtNAMESPACE x52
                                                                                                                         ) ls53)
                                                                                         ) ls57)), 
          ("id", cvtIDENT x61)]
   and cvtNUMBER_TYPE (Decimal) = PrettyRep.Ctor ("Decimal", NONE)
     | cvtNUMBER_TYPE (Double) = PrettyRep.Ctor ("Double", NONE)
     | cvtNUMBER_TYPE (Int) = PrettyRep.Ctor ("Int", NONE)
     | cvtNUMBER_TYPE (UInt) = PrettyRep.Ctor ("UInt", NONE)
     | cvtNUMBER_TYPE (Number) = PrettyRep.Ctor ("Number", NONE)
   and cvtNUMERIC_MODE {numberType=x72, roundingMode=r73, precision=n74} = 
          PrettyRep.Rec [("numberType", cvtNUMBER_TYPE x72), ("roundingMode", 
          PrettyRep.DecRm r73), ("precision", PrettyRep.Int n74)]
   and cvtBINTYPEOP (Cast) = PrettyRep.Ctor ("Cast", NONE)
     | cvtBINTYPEOP (Is) = PrettyRep.Ctor ("Is", NONE)
     | cvtBINTYPEOP (To) = PrettyRep.Ctor ("To", NONE)
<<<<<<< .mine
   and cvtBINOP (Plus opt80) = PrettyRep.Ctor ("Plus", SOME 
       (case opt80 of
=======
   and cvtBINOP (Plus opt87) = PrettyRep.Ctor ("Plus", SOME 
       (case opt87 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x79 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x79))
=======
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Minus opt87) = PrettyRep.Ctor ("Minus", SOME 
       (case opt87 of
=======
     | cvtBINOP (Minus opt94) = PrettyRep.Ctor ("Minus", SOME 
       (case opt94 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x86 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x86))
=======
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Times opt94) = PrettyRep.Ctor ("Times", SOME 
       (case opt94 of
=======
     | cvtBINOP (Times opt101) = PrettyRep.Ctor ("Times", SOME 
       (case opt101 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x93 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x93))
=======
       | SOME x100 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x100))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Divide opt101) = PrettyRep.Ctor ("Divide", SOME 
       (case opt101 of
=======
     | cvtBINOP (Divide opt108) = PrettyRep.Ctor ("Divide", SOME 
       (case opt108 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x100 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x100))
=======
       | SOME x107 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x107))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Remainder opt108) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt108 of
=======
     | cvtBINOP (Remainder opt115) = PrettyRep.Ctor ("Remainder", SOME 
       (case opt115 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x107 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x107))
=======
       | SOME x114 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x114))
>>>>>>> .theirs
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
<<<<<<< .mine
     | cvtBINOP (Equals opt125) = PrettyRep.Ctor ("Equals", SOME 
       (case opt125 of
=======
     | cvtBINOP (Equals opt132) = PrettyRep.Ctor ("Equals", SOME 
       (case opt132 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x124 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x124))
=======
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (NotEquals opt132) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt132 of
=======
     | cvtBINOP (NotEquals opt139) = PrettyRep.Ctor ("NotEquals", SOME 
       (case opt139 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x131 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x131))
=======
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (StrictEquals opt139) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt139 of
=======
     | cvtBINOP (StrictEquals opt146) = PrettyRep.Ctor ("StrictEquals", SOME 
       (case opt146 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x138 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x138))
=======
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (StrictNotEquals opt146) = PrettyRep.Ctor ("StrictNotEquals", 
=======
     | cvtBINOP (StrictNotEquals opt153) = PrettyRep.Ctor ("StrictNotEquals", 
>>>>>>> .theirs
          SOME 
<<<<<<< .mine
       (case opt146 of
=======
       (case opt153 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x145 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x145))
=======
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Less opt153) = PrettyRep.Ctor ("Less", SOME 
       (case opt153 of
=======
     | cvtBINOP (Less opt160) = PrettyRep.Ctor ("Less", SOME 
       (case opt160 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x152 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x152))
=======
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (LessOrEqual opt160) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt160 of
=======
     | cvtBINOP (LessOrEqual opt167) = PrettyRep.Ctor ("LessOrEqual", SOME 
       (case opt167 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x159 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x159))
=======
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x166))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (Greater opt167) = PrettyRep.Ctor ("Greater", SOME 
       (case opt167 of
=======
     | cvtBINOP (Greater opt174) = PrettyRep.Ctor ("Greater", SOME 
       (case opt174 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x166 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x166))
=======
       | SOME x173 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x173))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtBINOP (GreaterOrEqual opt174) = PrettyRep.Ctor ("GreaterOrEqual", 
=======
     | cvtBINOP (GreaterOrEqual opt181) = PrettyRep.Ctor ("GreaterOrEqual", 
>>>>>>> .theirs
          SOME 
<<<<<<< .mine
       (case opt174 of
=======
       (case opt181 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x173 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x173))
=======
       | SOME x180 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x180))
>>>>>>> .theirs
       ))
     | cvtBINOP (Comma) = PrettyRep.Ctor ("Comma", NONE)
   and cvtASSIGNOP (Assign) = PrettyRep.Ctor ("Assign", NONE)
<<<<<<< .mine
     | cvtASSIGNOP (AssignPlus opt183) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt183 of
=======
     | cvtASSIGNOP (AssignPlus opt190) = PrettyRep.Ctor ("AssignPlus", SOME 
       (case opt190 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x182 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x182))
=======
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtASSIGNOP (AssignMinus opt190) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt190 of
=======
     | cvtASSIGNOP (AssignMinus opt197) = PrettyRep.Ctor ("AssignMinus", SOME 
       (case opt197 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x189 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x189))
=======
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtASSIGNOP (AssignTimes opt197) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt197 of
=======
     | cvtASSIGNOP (AssignTimes opt204) = PrettyRep.Ctor ("AssignTimes", SOME 
       (case opt204 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x196 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x196))
=======
       | SOME x203 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x203))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtASSIGNOP (AssignDivide opt204) = PrettyRep.Ctor ("AssignDivide", 
=======
     | cvtASSIGNOP (AssignDivide opt211) = PrettyRep.Ctor ("AssignDivide", 
>>>>>>> .theirs
          SOME 
<<<<<<< .mine
       (case opt204 of
=======
       (case opt211 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x203 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x203))
=======
       | SOME x210 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x210))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtASSIGNOP (AssignRemainder opt211) = PrettyRep.Ctor ("AssignRemainder", 
=======
     | cvtASSIGNOP (AssignRemainder opt218) = PrettyRep.Ctor ("AssignRemainder", 
>>>>>>> .theirs
          SOME 
<<<<<<< .mine
       (case opt211 of
=======
       (case opt218 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x210 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x210))
=======
       | SOME x217 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x217))
>>>>>>> .theirs
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
<<<<<<< .mine
     | cvtUNOP (PreIncrement opt229) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt229 of
=======
     | cvtUNOP (PreIncrement opt236) = PrettyRep.Ctor ("PreIncrement", SOME 
       (case opt236 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x228 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x228))
=======
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtUNOP (PreDecrement opt236) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt236 of
=======
     | cvtUNOP (PreDecrement opt243) = PrettyRep.Ctor ("PreDecrement", SOME 
       (case opt243 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x235 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x235))
=======
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtUNOP (PostIncrement opt243) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt243 of
=======
     | cvtUNOP (PostIncrement opt250) = PrettyRep.Ctor ("PostIncrement", SOME 
       (case opt250 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x242 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x242))
=======
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtUNOP (PostDecrement opt250) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt250 of
=======
     | cvtUNOP (PostDecrement opt257) = PrettyRep.Ctor ("PostDecrement", SOME 
       (case opt257 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x249 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x249))
=======
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x256))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtUNOP (UnaryPlus opt257) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt257 of
=======
     | cvtUNOP (UnaryPlus opt264) = PrettyRep.Ctor ("UnaryPlus", SOME 
       (case opt264 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x256 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x256))
=======
       | SOME x263 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x263))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtUNOP (UnaryMinus opt264) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt264 of
=======
     | cvtUNOP (UnaryMinus opt271) = PrettyRep.Ctor ("UnaryMinus", SOME 
       (case opt271 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x263 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x263))
=======
       | SOME x270 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x270))
>>>>>>> .theirs
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
<<<<<<< .mine
   and cvtPRAGMA (UseNamespace x281) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x281))
     | cvtPRAGMA (UseDefaultNamespace x284) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x284))
     | cvtPRAGMA (UseNumber x287) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x287))
     | cvtPRAGMA (UseRounding r290) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r290))
     | cvtPRAGMA (UsePrecision n293) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n293))
=======
   and cvtPRAGMA (UseNamespace x288) = PrettyRep.Ctor ("UseNamespace", SOME (cvtEXPR x288))
     | cvtPRAGMA (UseDefaultNamespace x291) = PrettyRep.Ctor ("UseDefaultNamespace", 
          SOME (cvtEXPR x291))
     | cvtPRAGMA (UseNumber x294) = PrettyRep.Ctor ("UseNumber", SOME (cvtNUMBER_TYPE x294))
     | cvtPRAGMA (UseRounding r297) = PrettyRep.Ctor ("UseRounding", SOME (PrettyRep.DecRm r297))
     | cvtPRAGMA (UsePrecision n300) = PrettyRep.Ctor ("UsePrecision", SOME (PrettyRep.Int n300))
>>>>>>> .theirs
     | cvtPRAGMA (UseStrict) = PrettyRep.Ctor ("UseStrict", NONE)
     | cvtPRAGMA (UseStandard) = PrettyRep.Ctor ("UseStandard", NONE)
<<<<<<< .mine
     | cvtPRAGMA (Import{package=ls299, name=x303, alias=opt305}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x298 => 
                                                                           cvtIDENT x298
                                                                    ) ls299)), 
          ("name", cvtIDENT x303), ("alias", 
       (case opt305 of
=======
     | cvtPRAGMA (Import{package=ls306, name=x310, alias=opt312}) = PrettyRep.Ctor ("Import", 
          SOME (PrettyRep.Rec [("package", PrettyRep.List (List.map (fn x305 => 
                                                                           cvtIDENT x305
                                                                    ) ls306)), 
          ("name", cvtIDENT x310), ("alias", 
       (case opt312 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x304 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x304))
=======
       | SOME x311 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x311))
>>>>>>> .theirs
       ))]))
   and cvtFUNC_NAME_KIND (Ordinary) = PrettyRep.Ctor ("Ordinary", NONE)
     | cvtFUNC_NAME_KIND (Operator) = PrettyRep.Ctor ("Operator", NONE)
     | cvtFUNC_NAME_KIND (Get) = PrettyRep.Ctor ("Get", NONE)
     | cvtFUNC_NAME_KIND (Set) = PrettyRep.Ctor ("Set", NONE)
     | cvtFUNC_NAME_KIND (Call) = PrettyRep.Ctor ("Call", NONE)
     | cvtFUNC_NAME_KIND (Has) = PrettyRep.Ctor ("Has", NONE)
<<<<<<< .mine
   and cvtCLS (Cls{name=x324, nonnullable=b325, extends=opt327, implements=ls332, 
          classFixtures=x336, instanceFixtures=x337, instanceInits=x338, constructor=opt340, 
          classType=x344, instanceType=x345}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x324), ("nonnullable", PrettyRep.Bool b325), ("extends", 
          
       (case opt327 of
=======
   and cvtCLS (Cls{name=x331, extends=opt333, implements=ls338, classFixtures=x342, 
          instanceFixtures=x343, instanceInits=x344, constructor=opt346, classType=x350, 
          instanceType=x351}) = PrettyRep.Ctor ("Cls", SOME (PrettyRep.Rec [("name", 
          cvtNAME x331), ("extends", 
       (case opt333 of

>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x332 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x332))
       )), ("implements", PrettyRep.List (List.map (fn x337 => cvtNAME x337
                                                   ) ls338)), ("classFixtures", 
          cvtFIXTURES x342), ("instanceFixtures", cvtFIXTURES x343), ("instanceInits", 
          cvtHEAD x344), ("constructor", 
       (case opt346 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x339 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x339))
       )), ("classType", cvtTYPE_EXPR x344), ("instanceType", cvtTYPE_EXPR x345)]))
   and cvtCTOR (Ctor{settings=x369, superArgs=ls371, func=x375}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x369), ("superArgs", PrettyRep.List (List.map (fn x370 => 
                                                                                                         cvtEXPR x370
                                                                                                  ) ls371)), 
          ("func", cvtFUNC x375)]))
   and cvtFUNC (Func{name=x385, fsig=x386, isNative=b387, block=x388, param=x389, 
          defaults=ls391, ty=x395}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x385), ("fsig", cvtFUNC_SIG x386), ("isNative", PrettyRep.Bool b387), 
          ("block", cvtBLOCK x388), ("param", cvtHEAD x389), ("defaults", PrettyRep.List (List.map (fn x390 => 
                                                                                                          cvtEXPR x390
                                                                                                   ) ls391)), 
          ("ty", cvtFUNC_TYPE x395)]))
   and cvtDEFN (ClassDefn x413) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x413))
     | cvtDEFN (VariableDefn x416) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x416))
     | cvtDEFN (FunctionDefn x419) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x419))
     | cvtDEFN (ConstructorDefn x422) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x422))
     | cvtDEFN (InterfaceDefn x425) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x425))
     | cvtDEFN (NamespaceDefn x428) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x428))
     | cvtDEFN (TypeDefn x431) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x431))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls435, params=x439, paramTypes=ls441, 
          defaults=ls446, ctorInits=opt457, returnType=x461, thisType=opt463, 
          hasRest=b467}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x434 => cvtIDENT x434
                                   ) ls435)), ("params", cvtBINDINGS x439), 
          ("paramTypes", PrettyRep.List (List.map (fn x440 => cvtTYPE_EXPR x440
                                                  ) ls441)), ("defaults", PrettyRep.List (List.map (fn x445 => 
                                                                                                          cvtEXPR x445
                                                                                                   ) ls446)), 
=======
       | SOME x345 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x345))
       )), ("classType", cvtTYPE_EXPR x350), ("instanceType", cvtTYPE_EXPR x351)]))
   and cvtCTOR (Ctor{settings=x373, superArgs=ls375, func=x379}) = PrettyRep.Ctor ("Ctor", 
          SOME (PrettyRep.Rec [("settings", cvtHEAD x373), ("superArgs", PrettyRep.List (List.map (fn x374 => 
                                                                                                         cvtEXPR x374
                                                                                                  ) ls375)), 
          ("func", cvtFUNC x379)]))
   and cvtFUNC (Func{name=x389, fsig=x390, isNative=b391, block=x392, param=x393, 
          defaults=ls395, ty=x399}) = PrettyRep.Ctor ("Func", SOME (PrettyRep.Rec [("name", 
          cvtFUNC_NAME x389), ("fsig", cvtFUNC_SIG x390), ("isNative", PrettyRep.Bool b391), 
          ("block", cvtBLOCK x392), ("param", cvtHEAD x393), ("defaults", PrettyRep.List (List.map (fn x394 => 
                                                                                                          cvtEXPR x394
                                                                                                   ) ls395)), 
          ("ty", cvtFUNC_TYPE x399)]))
   and cvtDEFN (ClassDefn x417) = PrettyRep.Ctor ("ClassDefn", SOME (cvtCLASS_DEFN x417))
     | cvtDEFN (VariableDefn x420) = PrettyRep.Ctor ("VariableDefn", SOME (cvtVAR_DEFN x420))
     | cvtDEFN (FunctionDefn x423) = PrettyRep.Ctor ("FunctionDefn", SOME (cvtFUNC_DEFN x423))
     | cvtDEFN (ConstructorDefn x426) = PrettyRep.Ctor ("ConstructorDefn", 
          SOME (cvtCTOR_DEFN x426))
     | cvtDEFN (InterfaceDefn x429) = PrettyRep.Ctor ("InterfaceDefn", SOME (cvtINTERFACE_DEFN x429))
     | cvtDEFN (NamespaceDefn x432) = PrettyRep.Ctor ("NamespaceDefn", SOME (cvtNAMESPACE_DEFN x432))
     | cvtDEFN (TypeDefn x435) = PrettyRep.Ctor ("TypeDefn", SOME (cvtTYPE_DEFN x435))
   and cvtFUNC_SIG (FunctionSignature{typeParams=ls439, params=x443, paramTypes=ls445, 
          defaults=ls450, ctorInits=opt461, returnType=x465, thisType=opt467, 
          hasRest=b471}) = PrettyRep.Ctor ("FunctionSignature", SOME (PrettyRep.Rec [("typeParams", 
          PrettyRep.List (List.map (fn x438 => cvtIDENT x438
                                   ) ls439)), ("params", cvtBINDINGS x443), 
          ("paramTypes", PrettyRep.List (List.map (fn x444 => cvtTYPE_EXPR x444
                                                  ) ls445)), ("defaults", PrettyRep.List (List.map (fn x449 => 
                                                                                                          cvtEXPR x449
                                                                                                   ) ls450)), 
>>>>>>> .theirs
          ("ctorInits", 
<<<<<<< .mine
       (case opt457 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME(x450, ls452) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x450, 
            PrettyRep.List (List.map (fn x451 => cvtEXPR x451
                                     ) ls452)]))
       )), ("returnType", cvtTYPE_EXPR x461), ("thisType", 
       (case opt463 of
=======
       (case opt461 of






>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x462 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x462))
       )), ("hasRest", PrettyRep.Bool b467)]))
   and cvtBINDING (Binding{ident=x487, ty=x488}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x487), ("ty", cvtTYPE_EXPR x488)]))
   and cvtBINDING_IDENT (TempIdent n496) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n496))
     | cvtBINDING_IDENT (ParamIdent n499) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n499))
     | cvtBINDING_IDENT (PropIdent x502) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x502))
   and cvtINIT_STEP (InitStep(x505, x506)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x505, 
          cvtEXPR x506]))
     | cvtINIT_STEP (AssignStep(x510, x511)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x510, cvtEXPR x511]))
   and cvtTYPE_EXPR (SpecialType x515) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x515))
     | cvtTYPE_EXPR (UnionType ls519) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x518 => 
                                                                                                           cvtTYPE_EXPR x518
                                                                                                    ) ls519)))
     | cvtTYPE_EXPR (ArrayType ls526) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x525 => 
                                                                                                           cvtTYPE_EXPR x525
                                                                                                    ) ls526)))
     | cvtTYPE_EXPR (TypeName x532) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x532))
     | cvtTYPE_EXPR (ElementTypeRef(x535, n536)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x535, PrettyRep.Int n536]))
     | cvtTYPE_EXPR (FieldTypeRef(x540, x541)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x540, cvtIDENT x541]))
     | cvtTYPE_EXPR (FunctionType x545) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x545))
     | cvtTYPE_EXPR (ObjectType ls549) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x548 => 
                                                                                                             cvtFIELD_TYPE x548
                                                                                                      ) ls549)))
     | cvtTYPE_EXPR (AppType{base=x555, args=ls557}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x555), ("args", PrettyRep.List (List.map (fn x556 => 
                                                                                                     cvtTYPE_EXPR x556
                                                                                              ) ls557))]))
     | cvtTYPE_EXPR (NullableType{expr=x568, nullable=b569}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x568), ("nullable", PrettyRep.Bool b569)]))
     | cvtTYPE_EXPR (InstanceType{name=x577, typeParams=ls579, ty=x583, isDynamic=b584}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x577), 
          ("typeParams", PrettyRep.List (List.map (fn x578 => cvtIDENT x578
                                                  ) ls579)), ("ty", cvtTYPE_EXPR x583), 
          ("isDynamic", PrettyRep.Bool b584)]))







=======
       | SOME(x454, ls456) => PrettyRep.Ctor ("SOME", SOME (PrettyRep.Tuple [cvtBINDINGS x454, 
            PrettyRep.List (List.map (fn x455 => cvtEXPR x455
                                     ) ls456)]))
       )), ("returnType", cvtTYPE_EXPR x465), ("thisType", 
       (case opt467 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x466 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x466))
       )), ("hasRest", PrettyRep.Bool b471)]))
   and cvtBINDING (Binding{ident=x491, ty=x492}) = PrettyRep.Ctor ("Binding", 
          SOME (PrettyRep.Rec [("ident", cvtBINDING_IDENT x491), ("ty", cvtTYPE_EXPR x492)]))
   and cvtBINDING_IDENT (TempIdent n500) = PrettyRep.Ctor ("TempIdent", SOME (PrettyRep.Int n500))
     | cvtBINDING_IDENT (ParamIdent n503) = PrettyRep.Ctor ("ParamIdent", SOME (PrettyRep.Int n503))
     | cvtBINDING_IDENT (PropIdent x506) = PrettyRep.Ctor ("PropIdent", SOME (cvtIDENT x506))
   and cvtINIT_STEP (InitStep(x509, x510)) = PrettyRep.Ctor ("InitStep", SOME (PrettyRep.Tuple [cvtBINDING_IDENT x509, 
          cvtEXPR x510]))
     | cvtINIT_STEP (AssignStep(x514, x515)) = PrettyRep.Ctor ("AssignStep", 
          SOME (PrettyRep.Tuple [cvtEXPR x514, cvtEXPR x515]))
   and cvtTYPE_EXPR (SpecialType x519) = PrettyRep.Ctor ("SpecialType", SOME (cvtSPECIAL_TY x519))
     | cvtTYPE_EXPR (UnionType ls523) = PrettyRep.Ctor ("UnionType", SOME (PrettyRep.List (List.map (fn x522 => 
                                                                                                           cvtTYPE_EXPR x522
                                                                                                    ) ls523)))
     | cvtTYPE_EXPR (ArrayType ls530) = PrettyRep.Ctor ("ArrayType", SOME (PrettyRep.List (List.map (fn x529 => 
                                                                                                           cvtTYPE_EXPR x529
                                                                                                    ) ls530)))
     | cvtTYPE_EXPR (TypeName x536) = PrettyRep.Ctor ("TypeName", SOME (cvtIDENT_EXPR x536))
     | cvtTYPE_EXPR (ElementTypeRef(x539, n540)) = PrettyRep.Ctor ("ElementTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x539, PrettyRep.Int n540]))
     | cvtTYPE_EXPR (FieldTypeRef(x544, x545)) = PrettyRep.Ctor ("FieldTypeRef", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x544, cvtIDENT x545]))
     | cvtTYPE_EXPR (FunctionType x549) = PrettyRep.Ctor ("FunctionType", SOME (cvtFUNC_TYPE x549))
     | cvtTYPE_EXPR (ObjectType ls553) = PrettyRep.Ctor ("ObjectType", SOME (PrettyRep.List (List.map (fn x552 => 
                                                                                                             cvtFIELD_TYPE x552
                                                                                                      ) ls553)))
     | cvtTYPE_EXPR (AppType{base=x559, args=ls561}) = PrettyRep.Ctor ("AppType", 
          SOME (PrettyRep.Rec [("base", cvtTYPE_EXPR x559), ("args", PrettyRep.List (List.map (fn x560 => 
                                                                                                     cvtTYPE_EXPR x560
                                                                                              ) ls561))]))
     | cvtTYPE_EXPR (NullableType{expr=x572, nullable=b573}) = PrettyRep.Ctor ("NullableType", 
          SOME (PrettyRep.Rec [("expr", cvtTYPE_EXPR x572), ("nullable", PrettyRep.Bool b573)]))
     | cvtTYPE_EXPR (InstanceType{name=x581, typeParams=ls583, ty=x587, isDynamic=b588}) = 
          PrettyRep.Ctor ("InstanceType", SOME (PrettyRep.Rec [("name", cvtNAME x581), 
          ("typeParams", PrettyRep.List (List.map (fn x582 => cvtIDENT x582
                                                  ) ls583)), ("ty", cvtTYPE_EXPR x587), 
          ("isDynamic", PrettyRep.Bool b588)]))
     | cvtTYPE_EXPR (NominalType x600) = PrettyRep.Ctor ("NominalType", SOME (cvtNAME x600))
>>>>>>> .theirs
   and cvtSTMT (EmptyStmt) = PrettyRep.Ctor ("EmptyStmt", NONE)
<<<<<<< .mine
     | cvtSTMT (ExprStmt x597) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x597))
     | cvtSTMT (InitStmt{kind=x600, ns=opt602, prototype=b606, static=b607, 
          temps=x608, inits=ls610}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x600), ("ns", 
       (case opt602 of
=======
     | cvtSTMT (ExprStmt x604) = PrettyRep.Ctor ("ExprStmt", SOME (cvtEXPR x604))
     | cvtSTMT (InitStmt{kind=x607, ns=opt609, prototype=b613, static=b614, 
          temps=x615, inits=ls617}) = PrettyRep.Ctor ("InitStmt", SOME (PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x607), ("ns", 
       (case opt609 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x601 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x601))
       )), ("prototype", PrettyRep.Bool b606), ("static", PrettyRep.Bool b607), 
          ("temps", cvtBINDINGS x608), ("inits", PrettyRep.List (List.map (fn x609 => 
                                                                                 cvtINIT_STEP x609
                                                                          ) ls610))]))
     | cvtSTMT (ClassBlock{ns=opt630, ident=x634, name=opt636, block=x640}) = 
=======
       | SOME x608 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x608))
       )), ("prototype", PrettyRep.Bool b613), ("static", PrettyRep.Bool b614), 
          ("temps", cvtBINDINGS x615), ("inits", PrettyRep.List (List.map (fn x616 => 
                                                                                 cvtINIT_STEP x616
                                                                          ) ls617))]))
     | cvtSTMT (ClassBlock{ns=opt637, ident=x641, name=opt643, block=x647}) = 
>>>>>>> .theirs
          PrettyRep.Ctor ("ClassBlock", SOME (PrettyRep.Rec [("ns", 
<<<<<<< .mine
       (case opt630 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x629 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x629))
       )), ("ident", cvtIDENT x634), ("name", 
       (case opt636 of
=======
       (case opt637 of




>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x635 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x635))
       )), ("block", cvtBLOCK x640)]))
     | cvtSTMT (ForInStmt x652) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x652))
     | cvtSTMT (ThrowStmt x655) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x655))
     | cvtSTMT (ReturnStmt x658) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x658))
     | cvtSTMT (BreakStmt opt662) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt662 of
=======
       | SOME x636 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x636))
       )), ("ident", cvtIDENT x641), ("name", 
       (case opt643 of




>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x661 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x661))








=======
       | SOME x642 => PrettyRep.Ctor ("SOME", SOME (cvtNAME x642))
       )), ("block", cvtBLOCK x647)]))
     | cvtSTMT (ForInStmt x659) = PrettyRep.Ctor ("ForInStmt", SOME (cvtFOR_ENUM_STMT x659))
     | cvtSTMT (ThrowStmt x662) = PrettyRep.Ctor ("ThrowStmt", SOME (cvtEXPR x662))
     | cvtSTMT (ReturnStmt x665) = PrettyRep.Ctor ("ReturnStmt", SOME (cvtEXPR x665))
     | cvtSTMT (BreakStmt opt669) = PrettyRep.Ctor ("BreakStmt", SOME 
       (case opt669 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x668))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtSTMT (ContinueStmt opt669) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt669 of
=======
     | cvtSTMT (ContinueStmt opt676) = PrettyRep.Ctor ("ContinueStmt", SOME 
       (case opt676 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x668 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x668))
=======
       | SOME x675 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT x675))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtSTMT (BlockStmt x675) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x675))
     | cvtSTMT (LabeledStmt(x678, x679)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x678, 
          cvtSTMT x679]))
     | cvtSTMT (LetStmt x683) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x683))
     | cvtSTMT (WhileStmt x686) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x686))
     | cvtSTMT (DoWhileStmt x689) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x689))
     | cvtSTMT (ForStmt x692) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x692))
     | cvtSTMT (IfStmt{cnd=x695, thn=x696, els=x697}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x695), ("thn", cvtSTMT x696), 
          ("els", cvtSTMT x697)]))
     | cvtSTMT (WithStmt{obj=x707, ty=x708, body=x709}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x707), ("ty", cvtTYPE_EXPR x708), 
          ("body", cvtSTMT x709)]))
     | cvtSTMT (TryStmt{block=x719, catches=ls721, finally=opt726}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x719), ("catches", PrettyRep.List (List.map (fn x720 => 
                                                                                                     cvtCATCH_CLAUSE x720
                                                                                              ) ls721)), 
=======
     | cvtSTMT (BlockStmt x682) = PrettyRep.Ctor ("BlockStmt", SOME (cvtBLOCK x682))
     | cvtSTMT (LabeledStmt(x685, x686)) = PrettyRep.Ctor ("LabeledStmt", SOME (PrettyRep.Tuple [cvtIDENT x685, 
          cvtSTMT x686]))
     | cvtSTMT (LetStmt x690) = PrettyRep.Ctor ("LetStmt", SOME (cvtBLOCK x690))
     | cvtSTMT (WhileStmt x693) = PrettyRep.Ctor ("WhileStmt", SOME (cvtWHILE_STMT x693))
     | cvtSTMT (DoWhileStmt x696) = PrettyRep.Ctor ("DoWhileStmt", SOME (cvtWHILE_STMT x696))
     | cvtSTMT (ForStmt x699) = PrettyRep.Ctor ("ForStmt", SOME (cvtFOR_STMT x699))
     | cvtSTMT (IfStmt{cnd=x702, thn=x703, els=x704}) = PrettyRep.Ctor ("IfStmt", 
          SOME (PrettyRep.Rec [("cnd", cvtEXPR x702), ("thn", cvtSTMT x703), 
          ("els", cvtSTMT x704)]))
     | cvtSTMT (WithStmt{obj=x714, ty=x715, body=x716}) = PrettyRep.Ctor ("WithStmt", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x714), ("ty", cvtTYPE_EXPR x715), 
          ("body", cvtSTMT x716)]))
     | cvtSTMT (TryStmt{block=x726, catches=ls728, finally=opt733}) = PrettyRep.Ctor ("TryStmt", 
          SOME (PrettyRep.Rec [("block", cvtBLOCK x726), ("catches", PrettyRep.List (List.map (fn x727 => 
                                                                                                     cvtCATCH_CLAUSE x727
                                                                                              ) ls728)), 
>>>>>>> .theirs
          ("finally", 
<<<<<<< .mine
       (case opt726 of
=======
       (case opt733 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x725 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x725))
=======
       | SOME x732 => PrettyRep.Ctor ("SOME", SOME (cvtBLOCK x732))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
     | cvtSTMT (SwitchStmt{mode=opt740, cond=x744, labels=ls746, cases=ls751}) = 
=======
     | cvtSTMT (SwitchStmt{mode=opt747, cond=x751, labels=ls753, cases=ls758}) = 
>>>>>>> .theirs
          PrettyRep.Ctor ("SwitchStmt", SOME (PrettyRep.Rec [("mode", 
<<<<<<< .mine
       (case opt740 of
=======
       (case opt747 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x739 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x739))
       )), ("cond", cvtEXPR x744), ("labels", PrettyRep.List (List.map (fn x745 => 
                                                                              cvtIDENT x745
                                                                       ) ls746)), 
          ("cases", PrettyRep.List (List.map (fn x750 => cvtCASE x750
                                             ) ls751))]))
     | cvtSTMT (SwitchTypeStmt{cond=x766, ty=x767, cases=ls769}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x766), ("ty", cvtTYPE_EXPR x767), 
          ("cases", PrettyRep.List (List.map (fn x768 => cvtTYPE_CASE x768
                                             ) ls769))]))
     | cvtSTMT (DXNStmt{expr=x782}) = PrettyRep.Ctor ("DXNStmt", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x782)]))
   and cvtEXPR (TernaryExpr(x788, x789, x790)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x788, cvtEXPR x789, cvtEXPR x790]))
     | cvtEXPR (BinaryExpr(x794, x795, x796)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x794, cvtEXPR x795, cvtEXPR x796]))
     | cvtEXPR (BinaryTypeExpr(x800, x801, x802)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x800, cvtEXPR x801, cvtTYPE_EXPR x802]))
     | cvtEXPR (ExpectedTypeExpr(x806, x807)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x806, cvtEXPR x807]))
     | cvtEXPR (UnaryExpr(x811, x812)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x811, 
          cvtEXPR x812]))
     | cvtEXPR (TypeExpr x816) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x816))

=======
       | SOME x746 => PrettyRep.Ctor ("SOME", SOME (cvtNUMERIC_MODE x746))
       )), ("cond", cvtEXPR x751), ("labels", PrettyRep.List (List.map (fn x752 => 
                                                                              cvtIDENT x752
                                                                       ) ls753)), 
          ("cases", PrettyRep.List (List.map (fn x757 => cvtCASE x757
                                             ) ls758))]))
     | cvtSTMT (SwitchTypeStmt{cond=x773, ty=x774, cases=ls776}) = PrettyRep.Ctor ("SwitchTypeStmt", 
          SOME (PrettyRep.Rec [("cond", cvtEXPR x773), ("ty", cvtTYPE_EXPR x774), 
          ("cases", PrettyRep.List (List.map (fn x775 => cvtTYPE_CASE x775
                                             ) ls776))]))
     | cvtSTMT (Dxns{expr=x789}) = PrettyRep.Ctor ("Dxns", SOME (PrettyRep.Rec [("expr", 
          cvtEXPR x789)]))
   and cvtEXPR (TernaryExpr(x795, x796, x797, x798)) = PrettyRep.Ctor ("TernaryExpr", 
          SOME (PrettyRep.Tuple [cvtTRIOP x795, cvtEXPR x796, cvtEXPR x797, 
          cvtEXPR x798]))
     | cvtEXPR (BinaryExpr(x802, x803, x804)) = PrettyRep.Ctor ("BinaryExpr", 
          SOME (PrettyRep.Tuple [cvtBINOP x802, cvtEXPR x803, cvtEXPR x804]))
     | cvtEXPR (BinaryTypeExpr(x808, x809, x810)) = PrettyRep.Ctor ("BinaryTypeExpr", 
          SOME (PrettyRep.Tuple [cvtBINTYPEOP x808, cvtEXPR x809, cvtTYPE_EXPR x810]))
     | cvtEXPR (ExpectedTypeExpr(x814, x815)) = PrettyRep.Ctor ("ExpectedTypeExpr", 
          SOME (PrettyRep.Tuple [cvtTYPE_EXPR x814, cvtEXPR x815]))
     | cvtEXPR (UnaryExpr(x819, x820)) = PrettyRep.Ctor ("UnaryExpr", SOME (PrettyRep.Tuple [cvtUNOP x819, 
          cvtEXPR x820]))
     | cvtEXPR (TypeExpr x824) = PrettyRep.Ctor ("TypeExpr", SOME (cvtTYPE_EXPR x824))
>>>>>>> .theirs
     | cvtEXPR (ThisExpr) = PrettyRep.Ctor ("ThisExpr", NONE)
<<<<<<< .mine
     | cvtEXPR (YieldExpr opt821) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt821 of
=======
     | cvtEXPR (YieldExpr opt829) = PrettyRep.Ctor ("YieldExpr", SOME 
       (case opt829 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x820 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x820))
=======
       | SOME x828 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x828))
>>>>>>> .theirs
       ))
<<<<<<< .mine
     | cvtEXPR (SuperExpr opt828) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt828 of
=======
     | cvtEXPR (SuperExpr opt836) = PrettyRep.Ctor ("SuperExpr", SOME 
       (case opt836 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x827 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x827))
=======
       | SOME x835 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x835))
>>>>>>> .theirs
       ))
<<<<<<< .mine
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
=======
     | cvtEXPR (LiteralExpr x842) = PrettyRep.Ctor ("LiteralExpr", SOME (cvtLITERAL x842))
     | cvtEXPR (CallExpr{func=x845, actuals=ls847}) = PrettyRep.Ctor ("CallExpr", 
          SOME (PrettyRep.Rec [("func", cvtEXPR x845), ("actuals", PrettyRep.List (List.map (fn x846 => 
                                                                                                   cvtEXPR x846
                                                                                            ) ls847))]))
     | cvtEXPR (ApplyTypeExpr{expr=x858, actuals=ls860}) = PrettyRep.Ctor ("ApplyTypeExpr", 
          SOME (PrettyRep.Rec [("expr", cvtEXPR x858), ("actuals", PrettyRep.List (List.map (fn x859 => 
                                                                                                   cvtTYPE_EXPR x859
                                                                                            ) ls860))]))
     | cvtEXPR (LetExpr{defs=x871, body=x872, head=opt874}) = PrettyRep.Ctor ("LetExpr", 
          SOME (PrettyRep.Rec [("defs", cvtBINDINGS x871), ("body", cvtEXPR x872), 
>>>>>>> .theirs
          ("head", 
<<<<<<< .mine
       (case opt866 of
=======
       (case opt874 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x865 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x865))
=======
       | SOME x873 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x873))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
     | cvtEXPR (NewExpr{obj=x879, actuals=ls881}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x879), ("actuals", PrettyRep.List (List.map (fn x880 => 
                                                                                                  cvtEXPR x880
                                                                                           ) ls881))]))
     | cvtEXPR (ObjectRef{base=x892, ident=x893, pos=opt895}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x892), ("ident", cvtIDENT_EXPR x893), 
          ("pos", 
       (case opt895 of
=======
     | cvtEXPR (NewExpr{obj=x887, actuals=ls889}) = PrettyRep.Ctor ("NewExpr", 
          SOME (PrettyRep.Rec [("obj", cvtEXPR x887), ("actuals", PrettyRep.List (List.map (fn x888 => 
                                                                                                  cvtEXPR x888
                                                                                           ) ls889))]))
     | cvtEXPR (ObjectRef{base=x900, ident=x901, loc=opt903}) = PrettyRep.Ctor ("ObjectRef", 
          SOME (PrettyRep.Rec [("base", cvtEXPR x900), ("ident", cvtIDENT_EXPR x901), 
          ("loc", 
       (case opt903 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x894 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x894))
=======
       | SOME x902 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x902))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
     | cvtEXPR (LexicalRef{ident=x908, pos=opt910}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x908), ("pos", 
       (case opt910 of
=======
     | cvtEXPR (LexicalRef{ident=x916, loc=opt918}) = PrettyRep.Ctor ("LexicalRef", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x916), ("loc", 
       (case opt918 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x909 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x909))
=======
       | SOME x917 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x917))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
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
=======
     | cvtEXPR (SetExpr(x929, x930, x931)) = PrettyRep.Ctor ("SetExpr", SOME (PrettyRep.Tuple [cvtASSIGNOP x929, 
          cvtEXPR x930, cvtEXPR x931]))
     | cvtEXPR (ListExpr ls936) = PrettyRep.Ctor ("ListExpr", SOME (PrettyRep.List (List.map (fn x935 => 
                                                                                                    cvtEXPR x935
                                                                                             ) ls936)))
     | cvtEXPR (InitExpr(x942, x943, x944)) = PrettyRep.Ctor ("InitExpr", SOME (PrettyRep.Tuple [cvtINIT_TARGET x942, 
          cvtHEAD x943, cvtINITS x944]))
     | cvtEXPR (SliceExpr(x948, x949, x950)) = PrettyRep.Ctor ("SliceExpr", 
          SOME (PrettyRep.Tuple [cvtEXPR x948, cvtEXPR x949, cvtEXPR x950]))
     | cvtEXPR (GetTemp n954) = PrettyRep.Ctor ("GetTemp", SOME (PrettyRep.Int n954))
     | cvtEXPR (GetParam n957) = PrettyRep.Ctor ("GetParam", SOME (PrettyRep.Int n957))
>>>>>>> .theirs
   and cvtINIT_TARGET (Hoisted) = PrettyRep.Ctor ("Hoisted", NONE)
     | cvtINIT_TARGET (Local) = PrettyRep.Ctor ("Local", NONE)
     | cvtINIT_TARGET (Prototype) = PrettyRep.Ctor ("Prototype", NONE)
<<<<<<< .mine
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
=======
   and cvtFIXTURE_NAME (TempName n963) = PrettyRep.Ctor ("TempName", SOME (PrettyRep.Int n963))
     | cvtFIXTURE_NAME (PropName x966) = PrettyRep.Ctor ("PropName", SOME (cvtNAME x966))
   and cvtIDENT_EXPR (Identifier{ident=x969, openNamespaces=ls975}) = PrettyRep.Ctor ("Identifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT x969), ("openNamespaces", 
          PrettyRep.List (List.map (fn ls971 => PrettyRep.List (List.map (fn x970 => 
                                                                                cvtNAMESPACE x970
                                                                         ) ls971)
                                   ) ls975))]))
     | cvtIDENT_EXPR (QualifiedExpression{qual=x986, expr=x987}) = PrettyRep.Ctor ("QualifiedExpression", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x986), ("expr", cvtEXPR x987)]))
     | cvtIDENT_EXPR (AttributeIdentifier x995) = PrettyRep.Ctor ("AttributeIdentifier", 
          SOME (cvtIDENT_EXPR x995))
     | cvtIDENT_EXPR (ExpressionIdentifier{expr=x998, openNamespaces=ls1004}) = 
>>>>>>> .theirs
          PrettyRep.Ctor ("ExpressionIdentifier", SOME (PrettyRep.Rec [("expr", 
<<<<<<< .mine
          cvtEXPR x990), ("openNamespaces", PrettyRep.List (List.map (fn ls992 => 
                                                                            PrettyRep.List (List.map (fn x991 => 
                                                                                                            cvtNAMESPACE x991
                                                                                                     ) ls992)
                                                                     ) ls996))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1007, ident=s1008}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1007), ("ident", PrettyRep.UniStr s1008)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1016, typeArgs=ls1018}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1016), ("typeArgs", 
          PrettyRep.List (List.map (fn x1017 => cvtTYPE_EXPR x1017
                                   ) ls1018))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1030, x1034)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1029 => cvtIDENT x1029
                                                          ) ls1030), cvtIDENT_EXPR x1034]))
=======
          cvtEXPR x998), ("openNamespaces", PrettyRep.List (List.map (fn ls1000 => 
                                                                            PrettyRep.List (List.map (fn x999 => 
                                                                                                            cvtNAMESPACE x999
                                                                                                     ) ls1000)
                                                                     ) ls1004))]))
     | cvtIDENT_EXPR (QualifiedIdentifier{qual=x1015, ident=s1016}) = PrettyRep.Ctor ("QualifiedIdentifier", 
          SOME (PrettyRep.Rec [("qual", cvtEXPR x1015), ("ident", PrettyRep.UniStr s1016)]))
     | cvtIDENT_EXPR (TypeIdentifier{ident=x1024, typeArgs=ls1026}) = PrettyRep.Ctor ("TypeIdentifier", 
          SOME (PrettyRep.Rec [("ident", cvtIDENT_EXPR x1024), ("typeArgs", 
          PrettyRep.List (List.map (fn x1025 => cvtTYPE_EXPR x1025
                                   ) ls1026))]))
     | cvtIDENT_EXPR (UnresolvedPath(ls1038, x1042)) = PrettyRep.Ctor ("UnresolvedPath", 
          SOME (PrettyRep.Tuple [PrettyRep.List (List.map (fn x1037 => cvtIDENT x1037
                                                          ) ls1038), cvtIDENT_EXPR x1042]))
>>>>>>> .theirs
     | cvtIDENT_EXPR (WildcardIdentifier) = PrettyRep.Ctor ("WildcardIdentifier", 
          NONE)
   and cvtLITERAL (LiteralNull) = PrettyRep.Ctor ("LiteralNull", NONE)
     | cvtLITERAL (LiteralUndefined) = PrettyRep.Ctor ("LiteralUndefined", 
          NONE)
<<<<<<< .mine
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
     | cvtLITERAL (LiteralString s1065) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1065))
     | cvtLITERAL (LiteralArray{exprs=ls1069, ty=opt1074}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1068 => 
                                                                         cvtEXPR x1068
                                                                  ) ls1069)), 
=======
     | cvtLITERAL (LiteralContextualDecimal s1049) = PrettyRep.Ctor ("LiteralContextualDecimal", 
          SOME (PrettyRep.String s1049))
     | cvtLITERAL (LiteralContextualDecimalInteger s1052) = PrettyRep.Ctor ("LiteralContextualDecimalInteger", 
          SOME (PrettyRep.String s1052))
     | cvtLITERAL (LiteralContextualHexInteger s1055) = PrettyRep.Ctor ("LiteralContextualHexInteger", 
          SOME (PrettyRep.String s1055))
     | cvtLITERAL (LiteralDouble r1058) = PrettyRep.Ctor ("LiteralDouble", 
          SOME (PrettyRep.Real64 r1058))
     | cvtLITERAL (LiteralDecimal d1061) = PrettyRep.Ctor ("LiteralDecimal", 
          SOME (PrettyRep.Dec d1061))
     | cvtLITERAL (LiteralInt i1064) = PrettyRep.Ctor ("LiteralInt", SOME (PrettyRep.Int32 i1064))
     | cvtLITERAL (LiteralUInt u1067) = PrettyRep.Ctor ("LiteralUInt", SOME (PrettyRep.UInt32 u1067))
     | cvtLITERAL (LiteralBoolean b1070) = PrettyRep.Ctor ("LiteralBoolean", 
          SOME (PrettyRep.Bool b1070))
     | cvtLITERAL (LiteralString s1073) = PrettyRep.Ctor ("LiteralString", 
          SOME (PrettyRep.UniStr s1073))
     | cvtLITERAL (LiteralArray{exprs=ls1077, ty=opt1082}) = PrettyRep.Ctor ("LiteralArray", 
          SOME (PrettyRep.Rec [("exprs", PrettyRep.List (List.map (fn x1076 => 
                                                                         cvtEXPR x1076
                                                                  ) ls1077)), 
>>>>>>> .theirs
          ("ty", 
<<<<<<< .mine
       (case opt1074 of
=======
       (case opt1082 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1073 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1073))
=======
       | SOME x1081 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1081))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
     | cvtLITERAL (LiteralXML ls1086) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1085 => 
                                                                                                            cvtEXPR x1085
                                                                                                     ) ls1086)))
     | cvtLITERAL (LiteralNamespace x1092) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1092))
     | cvtLITERAL (LiteralObject{expr=ls1096, ty=opt1101}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1095 => 
                                                                        cvtFIELD x1095
                                                                 ) ls1096)), 
=======
     | cvtLITERAL (LiteralXML ls1094) = PrettyRep.Ctor ("LiteralXML", SOME (PrettyRep.List (List.map (fn x1093 => 
                                                                                                            cvtEXPR x1093
                                                                                                     ) ls1094)))
     | cvtLITERAL (LiteralNamespace x1100) = PrettyRep.Ctor ("LiteralNamespace", 
          SOME (cvtNAMESPACE x1100))
     | cvtLITERAL (LiteralObject{expr=ls1104, ty=opt1109}) = PrettyRep.Ctor ("LiteralObject", 
          SOME (PrettyRep.Rec [("expr", PrettyRep.List (List.map (fn x1103 => 
                                                                        cvtFIELD x1103
                                                                 ) ls1104)), 
>>>>>>> .theirs
          ("ty", 
<<<<<<< .mine
       (case opt1101 of
=======
       (case opt1109 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1100 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1100))
=======
       | SOME x1108 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1108))
>>>>>>> .theirs
       ))]))
<<<<<<< .mine
     | cvtLITERAL (LiteralFunction x1112) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1112))
     | cvtLITERAL (LiteralRegExp{str=s1115}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1115)]))
   and cvtBLOCK (Block x1121) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1121))
   and cvtFIXTURE (NamespaceFixture x1124) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1124))
     | cvtFIXTURE (ClassFixture x1127) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1127))
=======
     | cvtLITERAL (LiteralFunction x1120) = PrettyRep.Ctor ("LiteralFunction", 
          SOME (cvtFUNC x1120))
     | cvtLITERAL (LiteralRegExp{str=s1123}) = PrettyRep.Ctor ("LiteralRegExp", 
          SOME (PrettyRep.Rec [("str", PrettyRep.UniStr s1123)]))
   and cvtBLOCK (Block x1129) = PrettyRep.Ctor ("Block", SOME (cvtDIRECTIVES x1129))
   and cvtFIXTURE (NamespaceFixture x1132) = PrettyRep.Ctor ("NamespaceFixture", 
          SOME (cvtNAMESPACE x1132))
     | cvtFIXTURE (ClassFixture x1135) = PrettyRep.Ctor ("ClassFixture", SOME (cvtCLS x1135))
>>>>>>> .theirs
     | cvtFIXTURE (InterfaceFixture) = PrettyRep.Ctor ("InterfaceFixture", 
          NONE)
     | cvtFIXTURE (TypeVarFixture) = PrettyRep.Ctor ("TypeVarFixture", NONE)
<<<<<<< .mine
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
   and cvtFUNC_TYPE {typeParams=ls1211, params=ls1216, result=x1220, thisType=opt1222, 
          hasRest=b1226, minArgs=n1227} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1210 => 
                                                                                                        cvtIDENT x1210
                                                                                                 ) ls1211)), 
          ("params", PrettyRep.List (List.map (fn x1215 => cvtTYPE_EXPR x1215
                                              ) ls1216)), ("result", cvtTYPE_EXPR x1220), 






=======
     | cvtFIXTURE (TypeFixture x1140) = PrettyRep.Ctor ("TypeFixture", SOME (cvtTYPE_EXPR x1140))
     | cvtFIXTURE (MethodFixture{func=x1143, ty=x1144, readOnly=b1145, override=b1146, 
          final=b1147}) = PrettyRep.Ctor ("MethodFixture", SOME (PrettyRep.Rec [("func", 
          cvtFUNC x1143), ("ty", cvtTYPE_EXPR x1144), ("readOnly", PrettyRep.Bool b1145), 
          ("override", PrettyRep.Bool b1146), ("final", PrettyRep.Bool b1147)]))
     | cvtFIXTURE (ValFixture{ty=x1161, readOnly=b1162}) = PrettyRep.Ctor ("ValFixture", 
          SOME (PrettyRep.Rec [("ty", cvtTYPE_EXPR x1161), ("readOnly", PrettyRep.Bool b1162)]))
     | cvtFIXTURE (VirtualValFixture x1170) = PrettyRep.Ctor ("VirtualValFixture", 
          SOME (cvtVIRTUAL_VAL_FIXTURE x1170))
   and cvtBINDINGS (ls1174, ls1179) = PrettyRep.Tuple [PrettyRep.List (List.map (fn x1173 => 
                                                                                       cvtBINDING x1173
                                                                                ) ls1174), 
          PrettyRep.List (List.map (fn x1178 => cvtINIT_STEP x1178
                                   ) ls1179)]
   and cvtFIXTURES ls1187 = PrettyRep.List (List.map (fn (x1184, x1185) => 
                                                            PrettyRep.Tuple [cvtFIXTURE_NAME x1184, 
                                                            cvtFIXTURE x1185]
                                                     ) ls1187)
   and cvtINITS ls1194 = PrettyRep.List (List.map (fn (x1191, x1192) => PrettyRep.Tuple [cvtFIXTURE_NAME x1191, 
                                                         cvtEXPR x1192]
                                                  ) ls1194)
   and cvtHEAD (x1198, x1199) = PrettyRep.Tuple [cvtFIXTURES x1198, cvtINITS x1199]
   and cvtFIELD {kind=x1201, name=x1202, init=x1203} = PrettyRep.Rec [("kind", 
          cvtVAR_DEFN_TAG x1201), ("name", cvtIDENT_EXPR x1202), ("init", cvtEXPR x1203)]
   and cvtFIELD_TYPE {name=x1211, ty=x1212} = PrettyRep.Rec [("name", cvtIDENT x1211), 
          ("ty", cvtTYPE_EXPR x1212)]
   and cvtTYPED_IDENT {name=x1218, ty=opt1220} = PrettyRep.Rec [("name", cvtIDENT x1218), 
          ("ty", 
       (case opt1220 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1219 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1219))
       ))]
   and cvtFUNC_TYPE {typeParams=ls1230, params=ls1235, result=x1239, thisType=opt1241, 
          hasRest=b1245, minArgs=n1246} = PrettyRep.Rec [("typeParams", PrettyRep.List (List.map (fn x1229 => 
                                                                                                        cvtIDENT x1229
                                                                                                 ) ls1230)), 
          ("params", PrettyRep.List (List.map (fn x1234 => cvtTYPE_EXPR x1234
                                              ) ls1235)), ("result", cvtTYPE_EXPR x1239), 
>>>>>>> .theirs
          ("thisType", 
<<<<<<< .mine
       (case opt1222 of
=======
       (case opt1241 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1221 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1221))
       )), ("hasRest", PrettyRep.Bool b1226), ("minArgs", PrettyRep.Int n1227)]
   and cvtFUNC_DEFN {kind=x1241, ns=opt1243, final=b1247, override=b1248, prototype=b1249, 
          static=b1250, func=x1251} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1241), 
=======
       | SOME x1240 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1240))
       )), ("hasRest", PrettyRep.Bool b1245), ("minArgs", PrettyRep.Int n1246)]
   and cvtFUNC_DEFN {kind=x1260, ns=opt1262, final=b1266, override=b1267, prototype=b1268, 
          static=b1269, func=x1270} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1260), 
>>>>>>> .theirs
          ("ns", 
<<<<<<< .mine
       (case opt1243 of
=======
       (case opt1262 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1242 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1242))
       )), ("final", PrettyRep.Bool b1247), ("override", PrettyRep.Bool b1248), 
          ("prototype", PrettyRep.Bool b1249), ("static", PrettyRep.Bool b1250), 
          ("func", cvtFUNC x1251)]
   and cvtCTOR_DEFN x1267 = cvtCTOR x1267
   and cvtVAR_DEFN {kind=x1268, ns=opt1270, static=b1274, prototype=b1275, 
          bindings=x1276} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1268), 
=======
       | SOME x1261 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1261))
       )), ("final", PrettyRep.Bool b1266), ("override", PrettyRep.Bool b1267), 
          ("prototype", PrettyRep.Bool b1268), ("static", PrettyRep.Bool b1269), 
          ("func", cvtFUNC x1270)]
   and cvtCTOR_DEFN x1286 = cvtCTOR x1286
   and cvtVAR_DEFN {kind=x1287, ns=opt1289, static=b1293, prototype=b1294, 
          bindings=x1295} = PrettyRep.Rec [("kind", cvtVAR_DEFN_TAG x1287), 
>>>>>>> .theirs
          ("ns", 
<<<<<<< .mine
       (case opt1270 of
=======
       (case opt1289 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1269 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1269))
       )), ("static", PrettyRep.Bool b1274), ("prototype", PrettyRep.Bool b1275), 
          ("bindings", cvtBINDINGS x1276)]
   and cvtNAMESPACE_DEFN {ident=x1288, ns=opt1290, init=opt1295} = PrettyRep.Rec [("ident", 
          cvtIDENT x1288), ("ns", 
       (case opt1290 of
=======
       | SOME x1288 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1288))
       )), ("static", PrettyRep.Bool b1293), ("prototype", PrettyRep.Bool b1294), 
          ("bindings", cvtBINDINGS x1295)]
   and cvtNAMESPACE_DEFN {ident=x1307, ns=opt1309, init=opt1314} = PrettyRep.Rec [("ident", 
          cvtIDENT x1307), ("ns", 
       (case opt1309 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1289 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1289))
=======
       | SOME x1308 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1308))
>>>>>>> .theirs
       )), ("init", 
<<<<<<< .mine
       (case opt1295 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1294 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1294))
       ))]
   and cvtCLASS_DEFN {ident=x1306, ns=opt1308, nonnullable=b1312, dynamic=b1313, 
          final=b1314, params=ls1316, extends=opt1321, implements=ls1326, classDefns=ls1331, 
          instanceDefns=ls1336, instanceStmts=ls1341, ctorDefn=opt1346} = PrettyRep.Rec [("ident", 
          cvtIDENT x1306), ("ns", 
       (case opt1308 of
=======
       (case opt1314 of








>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1313 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1313))
<<<<<<< .mine
       )), ("nonnullable", PrettyRep.Bool b1312), ("dynamic", PrettyRep.Bool b1313), 
          ("final", PrettyRep.Bool b1314), ("params", PrettyRep.List (List.map (fn x1315 => 
                                                                                      cvtIDENT x1315
                                                                               ) ls1316)), 
          ("extends", 
       (case opt1321 of
=======
       ))]
   and cvtCLASS_DEFN {ident=x1325, ns=opt1327, nonnullable=b1331, dynamic=b1332, 
          final=b1333, params=ls1335, extends=opt1340, implements=ls1345, classDefns=ls1350, 
          instanceDefns=ls1355, instanceStmts=ls1360, ctorDefn=opt1365} = PrettyRep.Rec [("ident", 
          cvtIDENT x1325), ("ns", 
       (case opt1327 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1320 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1320))
       )), ("implements", PrettyRep.List (List.map (fn x1325 => cvtIDENT_EXPR x1325
                                                   ) ls1326)), ("classDefns", 
          PrettyRep.List (List.map (fn x1330 => cvtDEFN x1330
                                   ) ls1331)), ("instanceDefns", PrettyRep.List (List.map (fn x1335 => 
                                                                                                 cvtDEFN x1335
                                                                                          ) ls1336)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1340 => cvtSTMT x1340
                                                     ) ls1341)), ("ctorDefn", 








=======
       | SOME x1326 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1326))
       )), ("nonnullable", PrettyRep.Bool b1331), ("dynamic", PrettyRep.Bool b1332), 
          ("final", PrettyRep.Bool b1333), ("params", PrettyRep.List (List.map (fn x1334 => 
                                                                                      cvtIDENT x1334
                                                                               ) ls1335)), 
          ("extends", 
       (case opt1340 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1339 => PrettyRep.Ctor ("SOME", SOME (cvtIDENT_EXPR x1339))
       )), ("implements", PrettyRep.List (List.map (fn x1344 => cvtIDENT_EXPR x1344
                                                   ) ls1345)), ("classDefns", 
          PrettyRep.List (List.map (fn x1349 => cvtDEFN x1349
                                   ) ls1350)), ("instanceDefns", PrettyRep.List (List.map (fn x1354 => 
                                                                                                 cvtDEFN x1354
                                                                                          ) ls1355)), 
          ("instanceStmts", PrettyRep.List (List.map (fn x1359 => cvtSTMT x1359
                                                     ) ls1360)), ("ctorDefn", 
>>>>>>> .theirs
          
<<<<<<< .mine
       (case opt1346 of
=======
       (case opt1365 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1345 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1345))
=======
       | SOME x1364 => PrettyRep.Ctor ("SOME", SOME (cvtCTOR x1364))
>>>>>>> .theirs
       ))]
<<<<<<< .mine
   and cvtINTERFACE_DEFN {ident=x1375, ns=opt1377, nonnullable=b1381, params=ls1383, 
          extends=ls1388, block=x1392} = PrettyRep.Rec [("ident", cvtIDENT x1375), 
=======
   and cvtINTERFACE_DEFN {ident=x1394, ns=opt1396, nonnullable=b1400, params=ls1402, 
          extends=ls1407, block=x1411} = PrettyRep.Rec [("ident", cvtIDENT x1394), 
>>>>>>> .theirs
          ("ns", 
<<<<<<< .mine
       (case opt1377 of
=======
       (case opt1396 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1376 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1376))
       )), ("nonnullable", PrettyRep.Bool b1381), ("params", PrettyRep.List (List.map (fn x1382 => 
                                                                                             cvtIDENT x1382
                                                                                      ) ls1383)), 
          ("extends", PrettyRep.List (List.map (fn x1387 => cvtIDENT_EXPR x1387
                                               ) ls1388)), ("block", cvtBLOCK x1392)]
   and cvtTYPE_DEFN {ident=x1406, ns=opt1408, init=x1412} = PrettyRep.Rec [("ident", 
          cvtIDENT x1406), ("ns", 
       (case opt1408 of
=======
       | SOME x1395 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1395))
       )), ("nonnullable", PrettyRep.Bool b1400), ("params", PrettyRep.List (List.map (fn x1401 => 
                                                                                             cvtIDENT x1401
                                                                                      ) ls1402)), 
          ("extends", PrettyRep.List (List.map (fn x1406 => cvtIDENT_EXPR x1406
                                               ) ls1407)), ("block", cvtBLOCK x1411)]
   and cvtTYPE_DEFN {ident=x1425, ns=opt1427, init=x1431} = PrettyRep.Rec [("ident", 
          cvtIDENT x1425), ("ns", 
       (case opt1427 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1407 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1407))
       )), ("init", cvtTYPE_EXPR x1412)]
   and cvtFOR_ENUM_STMT {isEach=b1420, defn=opt1422, obj=x1426, fixtures=opt1428, 
          next=x1432, labels=ls1434, body=x1438} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1420), ("defn", 
       (case opt1422 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1421 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1421))
       )), ("obj", cvtEXPR x1426), ("fixtures", 
       (case opt1428 of
=======
       | SOME x1426 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1426))
       )), ("init", cvtTYPE_EXPR x1431)]
   and cvtFOR_ENUM_STMT {isEach=b1439, defn=opt1441, obj=x1445, fixtures=opt1447, 
          next=x1451, labels=ls1453, body=x1457} = PrettyRep.Rec [("isEach", 
          PrettyRep.Bool b1439), ("defn", 
       (case opt1441 of




>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1427 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1427))
       )), ("next", cvtSTMT x1432), ("labels", PrettyRep.List (List.map (fn x1433 => 
                                                                               cvtIDENT x1433
                                                                        ) ls1434)), 
          ("body", cvtSTMT x1438)]
   and cvtFOR_STMT {fixtures=opt1455, defn=opt1460, init=ls1465, cond=x1469, 
          update=x1470, labels=ls1472, body=x1476} = PrettyRep.Rec [("fixtures", 




=======
       | SOME x1440 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1440))
       )), ("obj", cvtEXPR x1445), ("fixtures", 
       (case opt1447 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1446 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1446))
       )), ("next", cvtSTMT x1451), ("labels", PrettyRep.List (List.map (fn x1452 => 
                                                                               cvtIDENT x1452
                                                                        ) ls1453)), 
          ("body", cvtSTMT x1457)]
   and cvtFOR_STMT {fixtures=opt1474, defn=opt1479, init=ls1484, cond=x1488, 
          update=x1489, labels=ls1491, body=x1495} = PrettyRep.Rec [("fixtures", 
>>>>>>> .theirs
          
<<<<<<< .mine
       (case opt1455 of
=======
       (case opt1474 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1454 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1454))
=======
       | SOME x1473 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1473))
>>>>>>> .theirs
       )), ("defn", 
<<<<<<< .mine
       (case opt1460 of
=======
       (case opt1479 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1459 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1459))
       )), ("init", PrettyRep.List (List.map (fn x1464 => cvtSTMT x1464
                                             ) ls1465)), ("cond", cvtEXPR x1469), 
          ("update", cvtEXPR x1470), ("labels", PrettyRep.List (List.map (fn x1471 => 
                                                                                cvtIDENT x1471
                                                                         ) ls1472)), 
          ("body", cvtSTMT x1476)]
   and cvtWHILE_STMT {cond=x1492, fixtures=opt1494, body=x1498, labels=ls1500} = 
          PrettyRep.Rec [("cond", cvtEXPR x1492), ("fixtures", 
       (case opt1494 of
=======
       | SOME x1478 => PrettyRep.Ctor ("SOME", SOME (cvtVAR_DEFN x1478))
       )), ("init", PrettyRep.List (List.map (fn x1483 => cvtSTMT x1483
                                             ) ls1484)), ("cond", cvtEXPR x1488), 
          ("update", cvtEXPR x1489), ("labels", PrettyRep.List (List.map (fn x1490 => 
                                                                                cvtIDENT x1490
                                                                         ) ls1491)), 
          ("body", cvtSTMT x1495)]
   and cvtWHILE_STMT {cond=x1511, fixtures=opt1513, body=x1517, labels=ls1519} = 
          PrettyRep.Rec [("cond", cvtEXPR x1511), ("fixtures", 
       (case opt1513 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1493 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1493))
       )), ("body", cvtSTMT x1498), ("labels", PrettyRep.List (List.map (fn x1499 => 
                                                                               cvtIDENT x1499
                                                                        ) ls1500))]
   and cvtDIRECTIVES {pragmas=ls1514, defns=ls1519, head=opt1524, body=ls1529, 
          pos=opt1534} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1513 => 
                                                                                    cvtPRAGMA x1513
                                                                             ) ls1514)), 
          ("defns", PrettyRep.List (List.map (fn x1518 => cvtDEFN x1518
                                             ) ls1519)), ("head", 
       (case opt1524 of
=======
       | SOME x1512 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1512))
       )), ("body", cvtSTMT x1517), ("labels", PrettyRep.List (List.map (fn x1518 => 
                                                                               cvtIDENT x1518
                                                                        ) ls1519))]
   and cvtDIRECTIVES {pragmas=ls1533, defns=ls1538, head=opt1543, body=ls1548, 
          loc=opt1553} = PrettyRep.Rec [("pragmas", PrettyRep.List (List.map (fn x1532 => 
                                                                                    cvtPRAGMA x1532
                                                                             ) ls1533)), 
          ("defns", PrettyRep.List (List.map (fn x1537 => cvtDEFN x1537
                                             ) ls1538)), ("head", 
       (case opt1543 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1523 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1523))
       )), ("body", PrettyRep.List (List.map (fn x1528 => cvtSTMT x1528
                                             ) ls1529)), ("pos", 
       (case opt1534 of
=======
       | SOME x1542 => PrettyRep.Ctor ("SOME", SOME (cvtHEAD x1542))
       )), ("body", PrettyRep.List (List.map (fn x1547 => cvtSTMT x1547
                                             ) ls1548)), ("loc", 
       (case opt1553 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1533 => PrettyRep.Ctor ("SOME", SOME (cvtPOS x1533))
=======
       | SOME x1552 => PrettyRep.Ctor ("SOME", SOME (cvtLOC x1552))
>>>>>>> .theirs
       ))]
<<<<<<< .mine
   and cvtCASE {label=opt1550, inits=opt1555, body=x1559} = PrettyRep.Rec [("label", 
=======
   and cvtCASE {label=opt1569, inits=opt1574, body=x1578} = PrettyRep.Rec [("label", 
>>>>>>> .theirs
          
<<<<<<< .mine
       (case opt1550 of
=======
       (case opt1569 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1549 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1549))
=======
       | SOME x1568 => PrettyRep.Ctor ("SOME", SOME (cvtEXPR x1568))
>>>>>>> .theirs
       )), ("inits", 
<<<<<<< .mine
       (case opt1555 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1554 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1554))
       )), ("body", cvtBLOCK x1559)]
   and cvtTYPE_CASE {ty=opt1568, body=x1572} = PrettyRep.Rec [("ty", 
       (case opt1568 of
=======
       (case opt1574 of





>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1567 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1567))
       )), ("body", cvtSTMT x1572)]
   and cvtCATCH_CLAUSE {bindings=x1578, ty=x1579, fixtures=opt1581, block=x1585} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1578), ("ty", cvtTYPE_EXPR x1579), 
          ("fixtures", 
       (case opt1581 of
=======
       | SOME x1573 => PrettyRep.Ctor ("SOME", SOME (cvtINITS x1573))
       )), ("body", cvtBLOCK x1578)]
   and cvtTYPE_CASE {ty=opt1587, body=x1591} = PrettyRep.Rec [("ty", 
       (case opt1587 of


>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1580 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1580))
       )), ("block", cvtBLOCK x1585)]
   and cvtFUNC_NAME {kind=x1595, ident=x1596} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1595), 
          ("ident", cvtIDENT x1596)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1602, getter=opt1604, setter=opt1609} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1602), ("getter", 
       (case opt1604 of
=======
       | SOME x1586 => PrettyRep.Ctor ("SOME", SOME (cvtTYPE_EXPR x1586))
       )), ("body", cvtSTMT x1591)]
   and cvtCATCH_CLAUSE {bindings=x1597, ty=x1598, fixtures=opt1600, block=x1604} = 
          PrettyRep.Rec [("bindings", cvtBINDINGS x1597), ("ty", cvtTYPE_EXPR x1598), 
          ("fixtures", 
       (case opt1600 of

>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1603 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1603))








=======
       | SOME x1599 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1599))
       )), ("block", cvtBLOCK x1604)]
   and cvtFUNC_NAME {kind=x1614, ident=x1615} = PrettyRep.Rec [("kind", cvtFUNC_NAME_KIND x1614), 
          ("ident", cvtIDENT x1615)]
   and cvtVIRTUAL_VAL_FIXTURE {ty=x1621, getter=opt1623, setter=opt1628} = 
          PrettyRep.Rec [("ty", cvtTYPE_EXPR x1621), ("getter", 
       (case opt1623 of
         NONE => PrettyRep.Ctor ("NONE", NONE)
       | SOME x1622 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1622))
>>>>>>> .theirs
       )), ("setter", 
<<<<<<< .mine
       (case opt1609 of
=======
       (case opt1628 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1608 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1608))
=======
       | SOME x1627 => PrettyRep.Ctor ("SOME", SOME (cvtFUNC_DEFN x1627))
>>>>>>> .theirs
       ))]
<<<<<<< .mine
   and cvtPACKAGE {name=ls1621, block=x1625} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1620 => 
                                                                                                       cvtIDENT x1620
                                                                                                ) ls1621)), 
          ("block", cvtBLOCK x1625)]
   and cvtPROGRAM {packages=ls1632, fixtures=opt1637, block=x1641} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1631 => cvtPACKAGE x1631
                                   ) ls1632)), ("fixtures", 
       (case opt1637 of
=======
   and cvtPACKAGE {name=ls1640, block=x1644} = PrettyRep.Rec [("name", PrettyRep.List (List.map (fn x1639 => 
                                                                                                       cvtIDENT x1639
                                                                                                ) ls1640)), 
          ("block", cvtBLOCK x1644)]
   and cvtPROGRAM {packages=ls1651, fixtures=opt1656, block=x1660} = PrettyRep.Rec [("packages", 
          PrettyRep.List (List.map (fn x1650 => cvtPACKAGE x1650
                                   ) ls1651)), ("fixtures", 
       (case opt1656 of
>>>>>>> .theirs
         NONE => PrettyRep.Ctor ("NONE", NONE)
<<<<<<< .mine
       | SOME x1636 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1636))
       )), ("block", cvtBLOCK x1641)]
=======
       | SOME x1655 => PrettyRep.Ctor ("SOME", SOME (cvtFIXTURES x1655))
       )), ("block", cvtBLOCK x1660)]
>>>>>>> .theirs
end

