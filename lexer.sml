

structure Lexer = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[lex] " :: ss) else ()
fun error ss = LogErr.lexError ss

open Token

exception LexChoicePoint of TOKEN

datatype NumericLiteralGroup = 
    HexIntLit | DecIntLit | DecLit

fun tname t = if !doTrace then tokenname t else ""

fun makeTokenList (filename : string, reader : unit -> Ustring.SOURCE) : ((TOKEN * Ast.LOC) list) =
    let
        val justFoundNewline  = ref false
        val src              = ref (reader ())
        val colNum          = ref 1
        val lineNum        = ref 1
        val toks          = ref []
        val idCharRanges = [(UTF8.fromAscii #"a", UTF8.fromAscii #"z"), (* ranges from http://www.codeproject.com/dotnet/UnicodeCharCatHelper.asp *)
                            (UTF8.fromAscii #"A", UTF8.fromAscii #"Z"),
                            (UTF8.fromAscii #"0", UTF8.fromAscii #"9"),
                            (0wx00aa            , 0wx00aa            ),
                            (0wx00b5            , 0wx00b5            ),
                            (0wx00ba            , 0wx00ba            ),
                            (0wx00c0            , 0wx00d6            ),
                            (0wx00d8            , 0wx00f6            ),
                            (0wx00f8            , 0wx01ba            ),
                            (0wx01bc            , 0wx01bf            ),
                            (0wx01c4            , 0wx02ad            ),
                            (0wx0386            , 0wx0386            ),
                            (0wx0388            , 0wx0481            ),
                            (0wx048c            , 0wx0556            ),
                            (0wx0561            , 0wx0587            ),
                            (0wx10a0            , 0wx10c5            ),
                            (0wx1e00            , 0wx1fbc            ),
                            (0wx1fbe            , 0wx1fbe            ),
                            (0wx1fc2            , 0wx1fcc            ),
                            (0wx1fd0            , 0wx1fdb            ),
                            (0wx1fe0            , 0wx1fec            ),
                            (0wx1ff2            , 0wx1ffc            ),
                            (0wx207f            , 0wx207f            ),
                            (0wx2102            , 0wx2102            ),
                            (0wx2107            , 0wx2107            ),
                            (0wx210a            , 0wx2113            ),
                            (0wx2115            , 0wx2115            ),
                            (0wx2119            , 0wx211d            ),
                            (0wx2124            , 0wx2124            ),
                            (0wx2126            , 0wx2126            ),
                            (0wx2128            , 0wx2128            ),
                            (0wx212a            , 0wx212d            ),
                            (0wx212f            , 0wx2131            ),
                            (0wx2133            , 0wx2134            ),
                            (0wx2139            , 0wx2139            ),
                            (0wxfb00            , 0wxfb17            ),
                            (0wxff21            , 0wxff3a            ),
                            (0wxff41            , 0wxff5a            ),
                            (UTF8.fromAscii #"$", UTF8.fromAscii #"$"),
                            (UTF8.fromAscii #"_", UTF8.fromAscii #"_")]
        
        fun numInRanges n ((lo,hi)::ranges) : bool =
            if (lo <= n) andalso (n <= hi)
            then true
            else numInRanges n ranges
          | numInRanges n [] =
            false
        
        fun countInRanges {min=min} ranges nums : int =
        let
            fun counter count [     ] = count
              | counter count (n::ns) =
                    if numInRanges n ranges
                    then counter (count+1) ns
                    else count
            val totalCount = counter 0 nums
        in
            if totalCount < min
            then error ["LexError:  illegal token"]
            else totalCount
        end
        
        fun isIdentifierChar c : bool =
            numInRanges c idCharRanges
        
        fun advanceIndex (adv : int) : unit =
        let
            val newSrc = List.drop (!src, adv)
        in
            src := (case newSrc of
                [] => (colNum  := 1;
                       lineNum := !lineNum + 1;
                       reader ())
              | _  => (colNum  := !colNum + adv;
                       newSrc)
            )
        end
        
        fun pushWithSpan tokSpan tok : unit =
        let
            val loc = {file = filename, span = tokSpan, post_newline = !justFoundNewline}
        in
            justFoundNewline := false;
            LogErr.setLoc (SOME loc);
	    trace [tname (tok,loc)];
            toks := (tok, loc) :: (!toks)
        end
        
        fun push (adv : int) (tok : TOKEN) : unit =
        let
            val tokSpan = ({line = !lineNum, col = !colNum          },
                           {line = !lineNum, col = !colNum + adv - 1})
        in
            pushWithSpan tokSpan tok;
            advanceIndex adv
        end
        
        fun pushEolAdv adv =
            (justFoundNewline := true;
             advanceIndex adv)
        
        fun lookahead k = 
	    let 
		fun step 0 (x::xs) = x
		  | step n (x::xs) = step (n-1) xs
		  | step _ [] = 0wx0
	    in
		step k (!src)
	    end

        fun lexEscapedChar () : Word.word =
        let
            fun hexToWord hexDigits =
            let
                val hexString = implode (map UTF8.toAscii ((UTF8.fromAscii #"0") :: (UTF8.fromAscii #"x") :: hexDigits))
                val codePoint = StringCvt.scanString (Int.scan StringCvt.HEX) hexString
            in
                case codePoint of
                    SOME cp => Word.fromInt cp
                  | NONE    => error ["LexError:  illegal escape sequence"]
            end
            
            val (adv, codepoint) = case !src of
               (0wx5C::0wx75::0wx7B::_     (* \u{ *)
              | 0wx5C::0wx78::0wx7B::_) => (* \x{ *)
                let
                    fun getHexDigits digits =
                       (case lookahead 0 of
                            0wx7D(* } *) => (advanceIndex 1; rev digits)
			  | 0wx0         => digits
                          | a            => (advanceIndex 1; getHexDigits (a::digits)))
                    
                    val digits = (advanceIndex 3; getHexDigits [])
                    val numDigits = length digits
                in
                    if (numDigits < 1) orelse (5 < numDigits)
                    then error ["LexError:  illegal size for bracketed escape sequence"]
                    else (0, hexToWord digits)
                end
              | 0wx5C::0wx78::a::b::_ =>       (* \xFF *)
                (4, hexToWord [a, b])
              | 0wx5C::0wx75::a::b::c::d::_ => (* \uFFFF *)
                (6, hexToWord [a, b, c, d])
              | 0wx5C::0wx62::_ =>             (* \b *)
                (2, 0wx08)
              | 0wx5C::0wx74::_ =>             (* \t *)
                (2, 0wx09)
              | 0wx5C::0wx6E::_ =>             (* \n *)
                (2, 0wx0A)
              | 0wx5C::0wx76::_ =>             (* \v *)
                (2, 0wx0B)
              | 0wx5C::0wx66::_ =>             (* \f *)
                (2, 0wx0C)
              | 0wx5C::0wx72::_ =>             (* \r *)
                (2, 0wx0D)
              | 0wx5C::0wx22::_ =>             (* \" *)
                (2, 0wx22)
              | 0wx5C::0wx27::_ =>             (* \' *)
                (2, 0wx27)
              | 0wx5C::0wx5C::_ =>             (* \\ *)
                (2, 0wx5C)
              | 0wx5C::
                (0wx000A | 0wx000D | 0wx2028 | 0wx2029)::_ => (* \<line terminator> *)
                error ["LexError:  illegal line-terminating escape sequence"]
              | 0wx5C::c::_ =>                 (* \<whatever> *)
                (2, c)
              | _ => error ["LexError:  LEXER BUG -- illegal escape sequence"]  (* should not be possible to get here *)
        in
            advanceIndex adv;
            codepoint
        end
        
        fun lexIdentifier id : unit =
	    case (!src) of 
		[] => 
		if id = [] 
		then ()
		else push 0 (Identifier (Vector.fromList (rev id)))

	      | 0wx5C::0wx75::a::b::c::d::_ => (* \uFFFF *)
		lexIdentifier ((lexEscapedChar ())::id)

	      | 0wx5C :: _ =>  
		error ["LexError:  illegal escape sequence for identifier"]

	      | c :: _ => 
		if not (isIdentifierChar c)
		then push 0 (Identifier (Vector.fromList (rev id)))
		else (advanceIndex 1;  lexIdentifier (c::id))
        
        fun matchOp (x::xs : Ustring.SOURCE) (y::ys : Ustring.SOURCE) : bool =
            (x = y) andalso (matchOp xs ys)
          | matchOp [] _ =
            true (* all of str was matched *)
          | matchOp (x::xs) [] =
            false (* should not be possible to reach here if all src lines end in \n *)
        
        fun lexOp ((str,tok)::rest : (Ustring.SOURCE * TOKEN) list) : unit =
            let
                val src_ = tl (!src)  (* we don't get here unless the first char matched already *)
                val strlen = 1 + (length str)  (* we add one because the first char is not in str *)
            in
                if matchOp str src_
                then push strlen tok
                else lexOp rest
            end
          | lexOp [] =
            error ["LexError:  LEXER BUG in lexOp"]

        fun lexResOrId ((str,tok)::rest : (Ustring.SOURCE * TOKEN) list) : unit =
            let
                val src_ = tl (!src)  (* we don't get here unless the first char matched already *)
                val strlen = 1 + (length str)  (* we add one because the first char is not in str *)
            in
                if (matchOp str src_) andalso (not (isIdentifierChar (lookahead strlen)))
                then push strlen tok
                else lexResOrId rest
            end
          | lexResOrId [] =
            lexIdentifier [] (* No matching operator or reserved word, so lex as an identifier. *)
        
        fun lexString delim : unit =
        let
            fun lexStr str  =
            let
                val c = lookahead 0
            in
                if (c = 0wx000A) orelse (c = 0wx000D) orelse (c = 0wx2028) orelse (c = 0wx2029) (* newlines *)
                then error ["LexError:  no newlines in string literals"]
		else if c = 0wx0
		then error ["LexError:  end of input in string literal"]
                else if c = 0wx005C (* backslash *)
                then lexStr ((lexEscapedChar ())::str)
                else if c = delim
                then push 1 (StringLiteral (Vector.fromList (rev str))) (* skips the final quote char *)
                else (advanceIndex 1;  lexStr (c::str))
            end
        in
            advanceIndex 1;  (* skip initial quote char *)
            lexStr []
        end
        
        fun lexNumber () : unit =
        (*
            The 7 token types and their patterns:
              
              HexIntegerLiteral
                0 [xX] [0-9a-fA-F]+
              
              DecimalIntegerLiteral
                0
                [1-9] [0-9]*
              
              DecimalLiteral
                0                        [eE] [+-]? [0-9]+
                [1-9] [0-9]*             [eE] [+-]? [0-9]+
                [1-9] [0-9]* . [0-9]*  ( [eE] [+-]? [0-9]+ )?
                             . [0-9]+  ( [eE] [+-]? [0-9]+ )?
                0            . [0-9]*  ( [eE] [+-]? [0-9]+ )?
              
              ExplicitIntLiteral
                {HexIntegerLiteral    } i
                {DecimalIntegerLiteral} i
              
              ExplicitUIntLiteral
                {HexIntegerLiteral    } u
                {DecimalIntegerLiteral} u
              
              ExplicitDoubleLiteral
                {DecimalLiteral       } d
                {DecimalIntegerLiteral} d
              
              ExplicitDecimalLiteral
                {DecimalLiteral       } m
                {DecimalIntegerLiteral} d
              
            So we search for the first 3, then see at the end if there's a
            trailing [iudm] so we can give an explicit type instead.
        *)
        let
            fun countExpChars chars =
            let
                val numSignChars = countInRanges {min=0} [(0wx2B,0wx2B) , (0wx2D,0wx2D)] chars (* [+-] *)
                val rest  = case numSignChars of
                                0 => chars
                              | 1 => tl chars
                              | _ => error ["LexError:  illegal characters in number literal"]
                val numExpDigits = countInRanges {min=1} [(UTF8.fromAscii #"0", UTF8.fromAscii #"9")] rest
            in
                numSignChars + numExpDigits
            end
            
            val (tokType, tokLen) = case !src of
                0wx2E::rest        (* .  =>  DecimalLiteral {   . [0-9]+  ( [eE] [+-]? [0-9]+ )? } *)
                =>  let
                        val numDigits = countInRanges {min=1} [(UTF8.fromAscii #"0", UTF8.fromAscii #"9")] rest
                        val numExpChars = case List.drop (rest, numDigits) of
                                             (0wx65
                                            | 0wx45)::rest_ (* [eE] *)
                                                => 1 + (countExpChars rest_)
                                            | _ => 0
                    in
                        (DecLit, 1 + numDigits + numExpChars)
                    end
              | 0wx30::0wx2E::rest  (* 0. =>  DecimalLiteral { 0 . [0-9]*  ( [eE] [+-]? [0-9]+ )? } *)
                =>  let
                        val numDigits = countInRanges {min=0} [(UTF8.fromAscii #"0", UTF8.fromAscii #"9")] rest
                        val numExpChars = case List.drop (rest, numDigits) of
                                             (0wx65
                                            | 0wx45)::rest_ (* [eE] *)
                                                => 1 + (countExpChars rest_)
                                            | _ => 0
                    in
                        (DecLit, 2 + numDigits + numExpChars)
                    end
              |(0wx30::0wx65::rest  (* 0e | 0E *)
              | 0wx30::0wx45::rest) (*    =>  DecimalLiteral { 0 [eE] [+-]? [0-9]+ } *)
                => (DecLit, 2 + (countExpChars rest))
              |(0wx30::0wx78::rest  (* 0x | 0X *)
              | 0wx30::0wx58::rest) (*    =>  HexIntegerLiteral { 0 [xX] [0-9a-fA-F]+ } *)
                =>  let
                        val hexDigitRanges = [(UTF8.fromAscii #"a", UTF8.fromAscii #"f"),
                                              (UTF8.fromAscii #"A", UTF8.fromAscii #"F"),
                                              (UTF8.fromAscii #"0", UTF8.fromAscii #"9")]
                        val numHexDigits = countInRanges {min=1} hexDigitRanges rest
                    in
                        (HexIntLit, 2 + numHexDigits)
                    end
              | 0wx30::rest        (* 0 => DecimalIntegerLiteral { 0 } *)
                => (DecIntLit, 1)
              |(0wx31
              | 0wx32
              | 0wx33
              | 0wx34
              | 0wx35  (* [1-9] *)
              | 0wx36  (*   =>  DecimalIntegerLiteral { [1-9] [0-9]*                                  } *)
              | 0wx37  (*   |   DecimalLiteral        { [1-9] [0-9]*             [eE] [+-]? [0-9]+    } *)
              | 0wx38  (*   |   DecimalLiteral        { [1-9] [0-9]* . [0-9]*  ( [eE] [+-]? [0-9]+ )? } *)
              | 0wx39)::rest
                =>  let
                        val numDigits = countInRanges {min=0} [(UTF8.fromAscii #"0", UTF8.fromAscii #"9")] rest
                    in
                        case List.drop (rest, numDigits) of
                           (0wx65
                          | 0wx45)::rest_ (* [eE] *)
                            => (DecLit, 1 + numDigits + 1 + (countExpChars rest_))
                          | 0wx2E::rest_  (* . *)
                            =>  let
                                    val numMoreDigits = countInRanges {min=0} [(UTF8.fromAscii #"0", UTF8.fromAscii #"9")] rest_
                                    val numExpChars = case List.drop (rest_, numMoreDigits) of
                                                         (0wx65
                                                        | 0wx45)::rest__ (* [eE] *)
                                                            => 1 + (countExpChars rest__)
                                                        | _ => 0
                                in
                                    (DecLit, 1 + numDigits + 1 + numMoreDigits + numExpChars)
                                end
                          | _ => (DecIntLit, 1 + numDigits)
                    end
              | _ => error ["LexError:  illegal character in numeric literal (BUG IN LEXER!)"] (* should not be possible to get here *)
            
            val numberAscii = implode (map UTF8.toAscii (List.take (!src, tokLen)))  (* should be safe, since we just lexed each char as being in the ascii range *)
        in
            case (tokType, lookahead tokLen) of
                (HexIntLit, 0wx69 (* i *)) => (case Int32.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitIntLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing HexIntLit(i)"] (* should not be possible to get here *))
              | (HexIntLit, 0wx75 (* u *)) => (case Word32.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitUIntLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing HexIntLit(u)"] (* should not be possible to get here *))
              | (DecIntLit, 0wx69 (* i *)) => (case Int32.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitIntLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(i)"] (* should not be possible to get here *))
              | (DecIntLit, 0wx75 (* u *)) => (case LargeInt.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitUIntLiteral (Word32.fromLargeInt i))
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(i)"] (* should not be possible to get here *))
              | (DecIntLit, 0wx64 (* d *)) => (case Real64.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitDoubleLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(d)"] (* should not be possible to get here *))
              | (DecIntLit, 0wx6D (* m *)) => (case Decimal.fromStringDefault numberAscii of
                        SOME i => push (tokLen+1) (ExplicitDecimalLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(m)"] (* should not be possible to get here *))
              | (DecLit   , 0wx64 (* d *)) => (case Real64.fromString numberAscii of
                        SOME i => push (tokLen+1) (ExplicitDoubleLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecLit(d)"   ] (* should not be possible to get here *))
              | (DecLit   , 0wx6D (* m *)) => (case Decimal.fromStringDefault numberAscii of
                        SOME i => push (tokLen+1) (ExplicitDecimalLiteral i)
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecLit(m)"   ] (* should not be possible to get here *))
              | (HexIntLit, _) => push tokLen (HexIntegerLiteral     numberAscii)
              | (DecIntLit, _) => push tokLen (DecimalIntegerLiteral numberAscii)
              | (DecLit   , _) => push tokLen (DecimalLiteral        numberAscii)
            ;
            (* next char must NOT be isIdentifierChar *)
            if isIdentifierChar (lookahead 0)
            then error ["LexError:  illegal character <",
                        Ustring.toAscii (Vector.fromList [lookahead 0]),
                        "> after numeric literal <",numberAscii,">"]
            else ()  (* cool *)
        end
        
        fun lexFromSlash () : unit = (* lexes comments or raises LexChoicePoint of LexBreakDiv *)
        let
            fun lexSingleLineComment () =
                case lookahead 0 of
                    (0wx000A | 0wx000D | 0wx2028 | 0wx2029) (* line terminators *)
                      => pushEolAdv 1
		  | 0wx0 => ()
                  | _ => (advanceIndex 1; lexSingleLineComment ())
            
            fun lexMultiLineComment {newline=false} {asterisk=asterisk} = (* have not encountered a newline yet *)
                (case lookahead 0 of
                    (0wx000A | 0wx000D | 0wx2028 | 0wx2029) (* line terminators *)
                                  => (pushEolAdv   1; lexMultiLineComment {newline=true } {asterisk=false})
                  | 0wx2A (* * *) => (advanceIndex 1; lexMultiLineComment {newline=false} {asterisk=true })
                  | 0wx2F (* / *) => (advanceIndex 1; if asterisk then () else
                                                      lexMultiLineComment {newline=false} {asterisk=false})
                  | 0wx0          => ()
                  | _             => (advanceIndex 1; lexMultiLineComment {newline=false} {asterisk=false}))
              | lexMultiLineComment {newline=true} {asterisk=asterisk} = (* have already encountered a newline *)
                (case lookahead 0 of
                    0wx2A (* * *) => (advanceIndex 1; lexMultiLineComment {newline=true} {asterisk=true })
                  | 0wx2F (* / *) => (advanceIndex 1; if asterisk then () else
                                                      lexMultiLineComment {newline=true} {asterisk=false})
                  | 0wx0          => ()
                  | _             => (advanceIndex 1; lexMultiLineComment {newline=true} {asterisk=false}))
            
            val nextChar = lookahead 1
        in
            case nextChar  of
                0wx2F (* / *) => (advanceIndex 2; lexSingleLineComment ())
              | 0wx2A (* * *) => (advanceIndex 2; lexMultiLineComment {newline=false} {asterisk=false})
              | _ =>  (* LexBreakDiv *)
                let
                    fun lexDivAndBeyond () : ((TOKEN * Ast.LOC) list) =
                    let
                        val (tok, tokLen) = if nextChar = 0wx3D (* = *)
                                            then (DivAssign, 2)
                                            else (Div      , 1)
                    in
                        toks := [];
                        push tokLen tok;
                        lexAndGetTokensToChoicePoint ()
                    end
                    
                    fun lexRegexpAndBeyond () : ((TOKEN * Ast.LOC) list) =
                    let
                        val startLine = !lineNum
                        val startCol  = !colNum
                        
                        fun lexRegexp reSrc {newline=newline} {charset=charset} =
                        let
                            val c = lookahead 0
                            val _ = advanceIndex 1
                        in
                            case (charset, c) of
                                (  _  , (0wx000A | 0wx000D | 0wx2028 | 0wx2029) (* line terminators *))
                                                       => lexRegexp (c::reSrc) {newline=true   } {charset=charset}
                              | (false, 0wx5B (* [ *)) => lexRegexp (c::reSrc) {newline=newline} {charset=true   }
                              | (true , 0wx5D (* ] *)) => lexRegexp (c::reSrc) {newline=newline} {charset=false  }
                              | (  _  , 0wx5C (* \ *)) => 
				(case lookahead 0 of
                                     (0wx000A | 0wx000D | 0wx2028 | 0wx2029) (* line terminators *)
                                     => (advanceIndex 1; lexRegexp (      reSrc) {newline=newline} {charset=charset})
				   | 0wx0 => error ["LexError: end of input in regexp"]
                                   | d => (advanceIndex 1; lexRegexp (d::c::reSrc) {newline=newline} {charset=charset})
                                )
                              | (false, 0wx2F (* / *)) =>
                                let
                                    val numFlags = countInRanges {min=0} [(UTF8.fromAscii #"a", UTF8.fromAscii #"z"),
                                                                          (UTF8.fromAscii #"A", UTF8.fromAscii #"Z")] (!src)
                                    val regexpFlags = List.take (!src, numFlags)
                                in
                                    advanceIndex numFlags;
                                    if newline andalso NONE = (List.find (fn flag => flag = 0wx78 (* x *)) regexpFlags)
                                    then error ["LexError:  illegal line terminator in regexp literal"]
                                    else List.revAppend (c::reSrc, regexpFlags)
                                end
                              | _ => lexRegexp (c::reSrc) {newline=newline} {charset=charset}
                        end
                        
                        val _ = advanceIndex 1 (* to move past the initial slash *)
                        val regexpStr = Vector.fromList (lexRegexp [0wx2F (* / *)] {newline=false} {charset=false})
                        val tokSpan = ({line = startLine, col = startCol   },
                                       {line = !lineNum , col = !colNum - 1})
                    in
                        toks := [];
                        pushWithSpan tokSpan (RegexpLiteral regexpStr);
                        
                        lexAndGetTokensToChoicePoint ()
                    end
                in
                    raise LexChoicePoint (LexBreakDiv {lex_initial = lexDivAndBeyond,
                                                       lex_regexp  = lexRegexpAndBeyond})
                end
        end
        
        and lex () : unit =
            if !src = []
            then push 0 Eof
            else let
                val c = hd (!src)
            in
                if UTF8.isAscii c then
                    case UTF8.toAscii c of
                    (* line terminators *)
                       (#"\n"
                      | #"\r")=> pushEolAdv 1
                    (* operators *)
                      | #"("  => push 1 LeftParen
                      | #")"  => push 1 RightParen
                      | #"["  => push 1 LeftBracket
                      | #"]"  => push 1 RightBracket
                      | #"{"  => push 1 LeftBrace
                      | #"}"  => push 1 RightBrace
                      | #","  => push 1 Comma
                      | #";"  => push 1 SemiColon
                      | #"?"  => push 1 QuestionMark
                      | #"@"  => push 1 At
                      | #"~"  => push 1 BitwiseNot
                      | #"-"  => lexOp [(UTF8.explode "-"  , MinusMinus ),
                                        (UTF8.explode "="  , MinusAssign),
                                        (UTF8.explode ""   , Minus      )]
                      | #"+"  => lexOp [(UTF8.explode "+"  , PlusPlus  ),
                                        (UTF8.explode "="  , PlusAssign),
                                        (UTF8.explode ""   , Plus      )]
                      | #"*"  => lexOp [(UTF8.explode "="  , MultAssign),
                                        (UTF8.explode ""   , Mult      )]
                      | #"^"  => lexOp [(UTF8.explode "="  , BitwiseXorAssign),
                                        (UTF8.explode ""   , BitwiseXor      )]
                      | #"%"  => lexOp [(UTF8.explode "="  , ModulusAssign),
                                        (UTF8.explode ""   , Modulus      )]
                      | #":"  => lexOp [(UTF8.explode ":"  , DoubleColon),
                                        (UTF8.explode ""   , Colon      )]
                      | #"!"  => lexOp [(UTF8.explode "==" , StrictNotEquals),
                                        (UTF8.explode "="  , NotEquals      ),
                                        (UTF8.explode ""   , Not            )]
                      | #"&"  => lexOp [(UTF8.explode "&=" , LogicalAndAssign),
                                        (UTF8.explode "&"  , LogicalAnd      ),
                                        (UTF8.explode "="  , BitwiseAndAssign),
                                        (UTF8.explode ""   , BitwiseAnd      )]
                      | #"|"  => lexOp [(UTF8.explode "|=" , LogicalOrAssign),
                                        (UTF8.explode "|"  , LogicalOr      ),
                                        (UTF8.explode "="  , BitwiseOrAssign),
                                        (UTF8.explode ""   , BitwiseOr      )]
                      | #"="  => lexOp [(UTF8.explode "==" , StrictEquals),
                                        (UTF8.explode "="  , Equals      ),
                                        (UTF8.explode ""   , Assign      )]
                      | #">"  => lexOp [(UTF8.explode ">>=", UnsignedRightShiftAssign),
                                        (UTF8.explode ">>" , UnsignedRightShift      ),
                                        (UTF8.explode ">=" , RightShiftAssign        ),
                                        (UTF8.explode ">"  , RightShift              ),
                                        (UTF8.explode "="  , GreaterThanOrEquals     ),
                                        (UTF8.explode ""   , GreaterThan             )]
                      | #"<"  => lexOp [(UTF8.explode "<=" , LeftShiftAssign ),
                                        (UTF8.explode "<"  , LeftShift       ),
                                        (UTF8.explode "="  , LessThanOrEquals),
                                        (UTF8.explode ""   , LessThan        )]  (* TODO:  LexBreakLessThan  {lex_initial = fn _ => ???, lex_xml = fn _ => ???} *)
                    (* slash:  Div, DivAssign, RegexpLiteral, and comments *)
                      | #"/"  => lexFromSlash ()
                    (* reserved words *)
                      | #"a"  => lexResOrId [(UTF8.explode "s"        , As)]
                      | #"b"  => lexResOrId [(UTF8.explode "reak"     , Break)]
                      | #"c"  => lexResOrId [(UTF8.explode "ontinue"  , Continue),
                                             (UTF8.explode "atch"     , Catch),
                                             (UTF8.explode "lass"     , Class),
                                             (UTF8.explode "onst"     , Const),
                                             (UTF8.explode "ase"      , Case),
                                             (UTF8.explode "ast"      , Cast),
                                             (UTF8.explode "all"      , Call)]
                      | #"d"  => lexResOrId [(UTF8.explode "ebugger"  , Debugger),
                                             (UTF8.explode "efault"   , Default),
                                             (UTF8.explode "ecimal"   , Decimal),
                                             (UTF8.explode "ynamic"   , Dynamic),
                                             (UTF8.explode "elete"    , Delete),
                                             (UTF8.explode "ouble"    , Double),
                                             (UTF8.explode "o"        , Do)]
                      | #"e"  => lexResOrId [(UTF8.explode "xtends"   , Extends),
                                             (UTF8.explode "lse"      , Else),
                                             (UTF8.explode "num"      , Enum),
                                             (UTF8.explode "ach"      , Each)]
                      | #"f"  => lexResOrId [(UTF8.explode "unction"  , Function),
                                             (UTF8.explode "inally"   , Finally),
                                             (UTF8.explode "alse"     , False),
                                             (UTF8.explode "inal"     , Final),
                                             (UTF8.explode "or"       , For)]
                      | #"g"  => lexResOrId [(UTF8.explode "oto"      , Goto),
                                             (UTF8.explode "et"       , Get)]
                      | #"h"  => lexResOrId [(UTF8.explode "as"       , Has)]
                      | #"i"  => lexResOrId [(UTF8.explode "mplements", Implements),
                                             (UTF8.explode "nstanceof", InstanceOf),
                                             (UTF8.explode "nterface" , Interface),
                                             (UTF8.explode "ntrinsic" , Intrinsic),
                                             (UTF8.explode "nternal"  , Internal),
                                             (UTF8.explode "nclude"   , Include),
                                             (UTF8.explode "mport"    , Import),
                                             (UTF8.explode "nvoke"    , Invoke),
                                             (UTF8.explode "nt"       , Int),
                                             (UTF8.explode "s"        , Is),
                                             (UTF8.explode "f"        , If),
                                             (UTF8.explode "n"        , In)]
                      | #"l"  => lexResOrId [(UTF8.explode "et"       , Let)]
                      | #"n"  => lexResOrId [(UTF8.explode "amespace" , Namespace),
                                             (UTF8.explode "ative"    , Native),
                                             (UTF8.explode "umber"    , Number),
                                             (UTF8.explode "ull"      , Null),
                                             (UTF8.explode "ew"       , New)]
                      | #"o"  => lexResOrId [(UTF8.explode "verride"  , Override)]
                      | #"p"  => lexResOrId [(UTF8.explode "rotected" , Protected),
                                             (UTF8.explode "recision" , Precision),
                                             (UTF8.explode "rototype" , Prototype),
                                             (UTF8.explode "ackage"   , Package),
                                             (UTF8.explode "rivate"   , Private),
                                             (UTF8.explode "ublic"    , Public)]
                      | #"r"  => lexResOrId [(UTF8.explode "ounding"  , Rounding),
                                             (UTF8.explode "eturn"    , Return)]
                      | #"s"  => lexResOrId [(UTF8.explode "tandard"  , Standard),
                                             (UTF8.explode "witch"    , Switch),
                                             (UTF8.explode "trict"    , Strict),
                                             (UTF8.explode "tatic"    , Static),
                                             (UTF8.explode "uper"     , Super),
                                             (UTF8.explode "et"       , Set)]
                      | #"t"  => lexResOrId [(UTF8.explode "ypeof"    , TypeOf),
                                             (UTF8.explode "hrow"     , Throw),
                                             (UTF8.explode "his"      , This),
                                             (UTF8.explode "ype"      , Type),
                                             (UTF8.explode "rue"      , True),
                                             (UTF8.explode "ry"       , Try),
                                             (UTF8.explode "o"        , To)]
                      | #"u"  => lexResOrId [(UTF8.explode "ndefined" , Undefined),
                                             (UTF8.explode "int"      , UInt),
                                             (UTF8.explode "se"       , Use)]
                      | #"v"  => lexResOrId [(UTF8.explode "oid"      , Void),
                                             (UTF8.explode "ar"       , Var)]
                      | #"w"  => lexResOrId [(UTF8.explode "hile"     , While),
                                             (UTF8.explode "ith"      , With)]
                      | #"x"  => lexResOrId [(UTF8.explode "ml"       , Token.Xml)]
                      | #"y"  => lexResOrId [(UTF8.explode "ield"     , Yield)]
                    (* numbers *)
                      |(#"0"
                      | #"1"
                      | #"2"
                      | #"3"
                      | #"4"
                      | #"5"
                      | #"6"
                      | #"7"
                      | #"8"
                      | #"9") => lexNumber ()
                    (* numbers ... .. .< . *)
                      | #"." => if (0wx30 <= (lookahead 1) andalso
                                             (lookahead 1) <= 0wx39) (* 0-9 *)
                                then lexNumber ()
                                else lexOp [(UTF8.explode ".." , TripleDot   ),
                                            (UTF8.explode "."  , DoubleDot   ),
                                            (UTF8.explode "<"  , LeftDotAngle),
                                            (UTF8.explode ""   , Dot         )]
                    (* string literal *)
                      (* TODO:  triple-quotes (""" and ''') and "backslash-newline" *)
                      |(#"\""
                      | #"'") => lexString c
                    (* whitespace *)
                      |(#"\t"
                      | #"\v"
                      | #"\f"
                      | #" ") => advanceIndex 1
                    (* identifier *)
                      | _ => if isIdentifierChar c
                             then lexIdentifier []
                             else error ["LexError:  illegal identifier character"]
                else
                    case c of
                    (* line terminators *)
                       (0wx2028
                      | 0wx2029) => pushEolAdv 1
                    (* whitespace *)
                      |(0wx00A0
                      | 0wx1680
                      | 0wx180E
                      | 0wx2000
                      | 0wx2001
                      | 0wx2002
                      | 0wx2003
                      | 0wx2004
                      | 0wx2005
                      | 0wx2006
                      | 0wx2007
                      | 0wx2008
                      | 0wx2009
                      | 0wx200A
                      | 0wx202F
                      | 0wx205F
                      | 0wx3000) => advanceIndex 1
                    (* identifier *)
                      | _        => lexIdentifier []
                ;
                lex ()
            end (* lex *)
        
        and lexAndGetTokensToChoicePoint () : ((TOKEN * Ast.LOC) list) =
            (lex () ; rev (!toks))
            handle LexChoicePoint tok => let
                val tokSpan = ({line = !lineNum , col = !colNum  },
                               {line = !lineNum , col = !colNum-1})
                val loc = {file = filename, span = tokSpan, post_newline = false}
            in
                rev ((tok,loc) :: (!toks))
            end
    in
        lexAndGetTokensToChoicePoint ()
    end (* makeTokenList *)


fun dumpTokens (h::t, lst) = dumpTokens(t, tokenname h :: "\n  " :: lst)
  | dumpTokens ([  ], lst) = rev lst


fun lex (filename : string, reader : unit -> Ustring.SOURCE) : ((TOKEN * Ast.LOC) list) =
    let
        val tokens = makeTokenList (filename, reader)
    in
        trace ("tokens: " :: dumpTokens(tokens,[])); 
        tokens
    end


end
