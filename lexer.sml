

structure Lexer = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[lex] " :: ss) else ()
fun error ss = LogErr.lexError ss

open Token

exception LexChoicePoint of TOKEN

datatype NumericLiteralGroup = 
    HexIntLit | DecIntLit | DecLit | OctIntLit

fun tname t = if !doTrace then tokenname t else ""

fun makeTokenList (filename : string, reader : unit -> Ustring.SOURCE) : ((TOKEN * Ast.LOC) list) =
    let
        val justFoundNewline  = ref false
        val src              = ref (reader ())
        val colNum          = ref 1
        val lineNum        = ref 1
        val toks          = ref []
        val idCharRanges = [(Ustring.wcharFromChar #"a", Ustring.wcharFromChar #"z"), (* ranges from http://www.codeproject.com/dotnet/UnicodeCharCatHelper.asp *)
                            (Ustring.wcharFromChar #"A", Ustring.wcharFromChar #"Z"),
                            (Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9"),
                            (0wx00aa                   , 0wx00aa                   ),
                            (0wx00b5                   , 0wx00b5                   ),
                            (0wx00ba                   , 0wx00ba                   ),
                            (0wx00c0                   , 0wx00d6                   ),
                            (0wx00d8                   , 0wx00f6                   ),
                            (0wx00f8                   , 0wx01ba                   ),
                            (0wx01bc                   , 0wx01bf                   ),
                            (0wx01c4                   , 0wx02ad                   ),
                            (0wx0386                   , 0wx0386                   ),
                            (0wx0388                   , 0wx0481                   ),
                            (0wx048c                   , 0wx0556                   ),
                            (0wx0561                   , 0wx0587                   ),
                            (0wx10a0                   , 0wx10c5                   ),
                            (0wx1e00                   , 0wx1fbc                   ),
                            (0wx1fbe                   , 0wx1fbe                   ),
                            (0wx1fc2                   , 0wx1fcc                   ),
                            (0wx1fd0                   , 0wx1fdb                   ),
                            (0wx1fe0                   , 0wx1fec                   ),
                            (0wx1ff2                   , 0wx1ffc                   ),
                            (0wx207f                   , 0wx207f                   ),
                            (0wx2102                   , 0wx2102                   ),
                            (0wx2107                   , 0wx2107                   ),
                            (0wx210a                   , 0wx2113                   ),
                            (0wx2115                   , 0wx2115                   ),
                            (0wx2119                   , 0wx211d                   ),
                            (0wx2124                   , 0wx2124                   ),
                            (0wx2126                   , 0wx2126                   ),
                            (0wx2128                   , 0wx2128                   ),
                            (0wx212a                   , 0wx212d                   ),
                            (0wx212f                   , 0wx2131                   ),
                            (0wx2133                   , 0wx2134                   ),
                            (0wx2139                   , 0wx2139                   ),
                            (0wxfb00                   , 0wxfb17                   ),
                            (0wxff21                   , 0wxff3a                   ),
                            (0wxff41                   , 0wxff5a                   ),
                            (Ustring.wcharFromChar #"$", Ustring.wcharFromChar #"$"),
                            (Ustring.wcharFromChar #"_", Ustring.wcharFromChar #"_")]
        val hexDigitRanges=[(Ustring.wcharFromChar #"a", Ustring.wcharFromChar #"f"),
                            (Ustring.wcharFromChar #"A", Ustring.wcharFromChar #"F"),
                            (Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")]
        
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
        
        fun pushEolAdv () =
        (justFoundNewline := true;
         case !src of
            0wx000D::0wx000A::_ => advanceIndex 2
          | 0wx000D::_ => advanceIndex 1
          | 0wx000A::_ => advanceIndex 1
          | 0wx2028::_ => advanceIndex 1
          | 0wx2029::_ => advanceIndex 1
          | _ => error ["LexError:  LEXER BUG! -- not a line terminator"]  (* should not be possible to get here *)
        )
        
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
                val numHexDigits = countInRanges {min=length hexDigits} hexDigitRanges hexDigits
                val hexString = implode (map Ustring.wcharToChar ((Ustring.wcharFromChar #"0") :: (Ustring.wcharFromChar #"x") :: hexDigits))
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
			  | 0wx0         => rev digits
                          | a            => (advanceIndex 1; getHexDigits (a::digits)))
                    
                    val digits = (advanceIndex 3; getHexDigits [])
                    val numDigits = length digits
                in
                    if (numDigits < 1) orelse (6 < numDigits)
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

	      | 0wx5C::0wx75::_ => (* \uFFFF *)
		lexIdentifier ((lexEscapedChar ())::id)

	      | 0wx5C::0wx78::_ => (* \xFF *)
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
            fun lexStr1 str  = (* For singly quoted strings '...' and "..." *)
            let
                val c  = lookahead 0
                val c1 = lookahead 1
                val c2 = lookahead 2
            in
                if (c = 0wx000A) orelse (c = 0wx000D) orelse (c = 0wx2028) orelse (c = 0wx2029) (* newlines *)
                then error ["LexError:  no newlines in string literals"]
		else if c = 0wx0
		then error ["LexError:  end of input in string literal"]
                else if c = 0wx005C andalso c1 = 0wx0D andalso c2 = 0wx0A (* backslash-CRLF *)
                then (advanceIndex 3;  lexStr1 str) (* skip backslash-newline *)
                else if c = 0wx005C andalso c1 = 0wx0D (* backslash-CR *)
                then (advanceIndex 2;  lexStr1 str) (* skip backslash-newline *)
                else if c = 0wx005C andalso c1 = 0wx0A (* backslash-LF *)
                then (advanceIndex 2;  lexStr1 str) (* skip backslash-newline *)
                else if c = 0wx005C (* backslash *)
                then lexStr1 ((lexEscapedChar ())::str)
                else if c = delim
                then push 1 (StringLiteral (Vector.fromList (rev str))) (* skips the final quote char *)
                else (advanceIndex 1;  lexStr1 (c::str))
            end
            
            fun lexStr3 str = (* For triply quoted strings '''...''' and """...""" *)
            let
                val c  = lookahead 0
                val c1 = lookahead 1
                val c2 = lookahead 2
                val c3 = lookahead 3
            in
		if c = 0wx0
		then error ["LexError:  end of input in string literal"]
                else if c = 0wx005C andalso c1 = 0wx0D andalso c2 = 0wx0A (* backslash-CRLF *)
                then (advanceIndex 3;  lexStr3 str) (* skip backslash-newline *)
                else if c = 0wx005C andalso c1 = 0wx0D (* backslash-CR *)
                then (advanceIndex 2;  lexStr3 str) (* skip backslash-newline *)
                else if c = 0wx005C andalso c1 = 0wx0A (* backslash-LF *)
                then (advanceIndex 2;  lexStr3 str) (* skip backslash-newline *)
                else if c = 0wx005C (* backslash *)
                then lexStr3 ((lexEscapedChar ())::str)
                else if c = delim andalso c1 = delim andalso c2 = delim andalso not (c3 = delim)
                then push 3 (StringLiteral (Vector.fromList (rev str))) (* skips the final quote chars *)
                else (advanceIndex 1;  lexStr3 (c::str))
            end
            
            val c1 = lookahead 1
            val c2 = lookahead 2
        in
            if c1 = delim andalso c2 = delim
            then (advanceIndex 3;  lexStr3 []) (* skip initial 3 quote chars *)
            else (advanceIndex 1;  lexStr1 []) (* skip initial quote char *)
        end
        
        fun lexNumber () : unit =
        (*
            The 7 token types and their patterns:
              
              HexIntegerLiteral
                0 [xX] [0-9a-fA-F]+
              
              OctIntegerLiteral
                0 [0-7]+
              
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
                {DecimalIntegerLiteral} m
              
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
                val numExpDigits = countInRanges {min=1} [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")] rest
            in
                numSignChars + numExpDigits
            end
            
            val (tokType, tokLen) = case !src of
                0wx2E::rest        (* .  =>  DecimalLiteral {   . [0-9]+  ( [eE] [+-]? [0-9]+ )? } *)
                =>  let
                        val numDigits = countInRanges {min=1} [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")] rest
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
                        val numDigits = countInRanges {min=0} [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")] rest
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
                        val numHexDigits = countInRanges {min=1} hexDigitRanges rest
                    in
                        (HexIntLit, 2 + numHexDigits)
                    end
              | 0wx30::(0wx30
                      | 0wx31
                      | 0wx32
                      | 0wx33
                      | 0wx34
                      | 0wx35         (* [0-7] *)
                      | 0wx36         (*   =>  OctalIntegerLiteral { 0 [0-7]* } *)
                      | 0wx37)::rest
                => let
                        val octDigitRanges = [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"7")]
                        val numOctDigits = countInRanges {min=0} octDigitRanges rest
                    in
                        (OctIntLit, 2 + numOctDigits)
                    end
              | 0wx30::rest           (* 0 => DecimalIntegerLiteral { 0 } *)
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
                        val numDigits = countInRanges {min=0} [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")] rest
                    in
                        case List.drop (rest, numDigits) of
                           (0wx65
                          | 0wx45)::rest_ (* [eE] *)
                            => (DecLit, 1 + numDigits + 1 + (countExpChars rest_))
                          | 0wx2E::rest_  (* . *)
                            =>  let
                                    val numMoreDigits = countInRanges {min=0} [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"9")] rest_
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
            
            val numberAscii = implode (map Ustring.wcharToChar (List.take (!src, tokLen)))  (* should be safe, since we just lexed each char as being in the ascii range *)
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
                      | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(u)"] (* should not be possible to get here *))
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
              | (OctIntLit, _) => push tokLen (OctIntegerLiteral     numberAscii)
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
                      => pushEolAdv ()
		  | 0wx0 => ()
                  | _ => (advanceIndex 1; lexSingleLineComment ())
            
            fun lexMultiLineComment {newline=false} {asterisk=asterisk} = (* have not encountered a newline yet *)
                (case lookahead 0 of
                    (0wx000A | 0wx000D | 0wx2028 | 0wx2029) (* line terminators *)
                                  => (pushEolAdv  (); lexMultiLineComment {newline=true } {asterisk=false})
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
                  | 0wx0          => error ["LexError: slash at end of multi-line comment"]
                  | _             => (advanceIndex 1; lexMultiLineComment {newline=true} {asterisk=false}))
            
            val nextChar = lookahead 1
        in
            case nextChar  of
                0wx2F (* / *) => (advanceIndex 2; lexSingleLineComment ())
              | 0wx2A (* * *) => (advanceIndex 2; lexMultiLineComment {newline=false} {asterisk=false})
              | 0wx0 => error ["LexError: slash at end of input"]
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
                            val _ = if c = 0wx0 then () else advanceIndex 1
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
                                    val numFlags = countInRanges {min=0} [(Ustring.wcharFromChar #"a", Ustring.wcharFromChar #"z"),
                                                                          (Ustring.wcharFromChar #"A", Ustring.wcharFromChar #"Z")] (!src)
                                    val regexpFlags = List.take (!src, numFlags)
                                in
                                    advanceIndex numFlags;
                                    if newline andalso NONE = (List.find (fn flag => flag = 0wx78 (* x *)) regexpFlags)
                                    then error ["LexError:  illegal line terminator in regexp literal"]
                                    else List.revAppend (c::reSrc, regexpFlags)
                                end
                              | (_, 0wx0) => error ["LexError: end of input in regexp"]
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
                if Ustring.wcharIsChar c then
                    case Ustring.wcharToChar c of
                    (* line terminators *)
                       (#"\n"
                      | #"\r")=> pushEolAdv ()
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
                      | #"-"  => lexOp [(Ustring.fromSource "-"  , MinusMinus ),
                                        (Ustring.fromSource "="  , MinusAssign),
                                        (Ustring.fromSource ""   , Minus      )]
                      | #"+"  => lexOp [(Ustring.fromSource "+"  , PlusPlus  ),
                                        (Ustring.fromSource "="  , PlusAssign),
                                        (Ustring.fromSource ""   , Plus      )]
                      | #"*"  => lexOp [(Ustring.fromSource "="  , MultAssign),
                                        (Ustring.fromSource ""   , Mult      )]
                      | #"^"  => lexOp [(Ustring.fromSource "="  , BitwiseXorAssign),
                                        (Ustring.fromSource ""   , BitwiseXor      )]
                      | #"%"  => lexOp [(Ustring.fromSource "="  , ModulusAssign),
                                        (Ustring.fromSource ""   , Modulus      )]
                      | #":"  => lexOp [(Ustring.fromSource ":"  , DoubleColon),
                                        (Ustring.fromSource ""   , Colon      )]
                      | #"!"  => lexOp [(Ustring.fromSource "==" , StrictNotEquals),
                                        (Ustring.fromSource "="  , NotEquals      ),
                                        (Ustring.fromSource ""   , Not            )]
                      | #"&"  => lexOp [(Ustring.fromSource "&=" , LogicalAndAssign),
                                        (Ustring.fromSource "&"  , LogicalAnd      ),
                                        (Ustring.fromSource "="  , BitwiseAndAssign),
                                        (Ustring.fromSource ""   , BitwiseAnd      )]
                      | #"|"  => lexOp [(Ustring.fromSource "|=" , LogicalOrAssign),
                                        (Ustring.fromSource "|"  , LogicalOr      ),
                                        (Ustring.fromSource "="  , BitwiseOrAssign),
                                        (Ustring.fromSource ""   , BitwiseOr      )]
                      | #"="  => lexOp [(Ustring.fromSource "==" , StrictEquals),
                                        (Ustring.fromSource "="  , Equals      ),
                                        (Ustring.fromSource ""   , Assign      )]
                      | #">"  => lexOp [(Ustring.fromSource ">>=", UnsignedRightShiftAssign),
                                        (Ustring.fromSource ">>" , UnsignedRightShift      ),
                                        (Ustring.fromSource ">=" , RightShiftAssign        ),
                                        (Ustring.fromSource ">"  , RightShift              ),
                                        (Ustring.fromSource "="  , GreaterThanOrEquals     ),
                                        (Ustring.fromSource ""   , GreaterThan             )]
                      | #"<"  => lexOp [(Ustring.fromSource "<=" , LeftShiftAssign ),
                                        (Ustring.fromSource "<"  , LeftShift       ),
                                        (Ustring.fromSource "="  , LessThanOrEquals),
                                        (Ustring.fromSource ""   , LessThan        )]  (* TODO:  LexBreakLessThan  {lex_initial = fn _ => ???, lex_xml = fn _ => ???} *)
                    (* slash:  Div, DivAssign, RegexpLiteral, and comments *)
                      | #"/"  => lexFromSlash ()
                    (* reserved words *)
                      | #"a"  => lexResOrId [(Ustring.fromSource "s"        , As)]
                      | #"b"  => lexResOrId [(Ustring.fromSource "reak"     , Break)]
                      | #"c"  => lexResOrId [(Ustring.fromSource "ontinue"  , Continue),
                                             (Ustring.fromSource "atch"     , Catch),
                                             (Ustring.fromSource "lass"     , Class),
                                             (Ustring.fromSource "onst"     , Const),
                                             (Ustring.fromSource "ase"      , Case),
                                             (Ustring.fromSource "ast"      , Cast),
                                             (Ustring.fromSource "all"      , Call)]
                      | #"d"  => lexResOrId [(Ustring.fromSource "ebugger"  , Debugger),
                                             (Ustring.fromSource "efault"   , Default),
                                             (Ustring.fromSource "ecimal"   , Decimal),
                                             (Ustring.fromSource "ynamic"   , Dynamic),
                                             (Ustring.fromSource "elete"    , Delete),
                                             (Ustring.fromSource "ouble"    , Double),
                                             (Ustring.fromSource "o"        , Do)]
                      | #"e"  => lexResOrId [(Ustring.fromSource "xtends"   , Extends),
                                             (Ustring.fromSource "lse"      , Else),
                                             (Ustring.fromSource "num"      , Enum),
                                             (Ustring.fromSource "ach"      , Each)]
                      | #"f"  => lexResOrId [(Ustring.fromSource "unction"  , Function),
                                             (Ustring.fromSource "inally"   , Finally),
                                             (Ustring.fromSource "alse"     , False),
                                             (Ustring.fromSource "inal"     , Final),
                                             (Ustring.fromSource "or"       , For)]
                      | #"g"  => lexResOrId [(Ustring.fromSource "oto"      , Goto),
                                             (Ustring.fromSource "et"       , Get)]
                      | #"h"  => lexResOrId [(Ustring.fromSource "as"       , Has)]
                      | #"i"  => lexResOrId [(Ustring.fromSource "mplements", Implements),
                                             (Ustring.fromSource "nstanceof", InstanceOf),
                                             (Ustring.fromSource "nterface" , Interface),
                                             (Ustring.fromSource "ntrinsic" , Intrinsic),
                                             (Ustring.fromSource "nternal"  , Internal),
                                             (Ustring.fromSource "nclude"   , Include),
                                             (Ustring.fromSource "mport"    , Import),
                                             (Ustring.fromSource "nvoke"    , Invoke),
                                             (Ustring.fromSource "nt"       , Int),
                                             (Ustring.fromSource "s"        , Is),
                                             (Ustring.fromSource "f"        , If),
                                             (Ustring.fromSource "n"        , In)]
                      | #"l"  => lexResOrId [(Ustring.fromSource "et"       , Let)]
                      | #"n"  => lexResOrId [(Ustring.fromSource "amespace" , Namespace),
                                             (Ustring.fromSource "ative"    , Native),
                                             (Ustring.fromSource "umber"    , Number),
                                             (Ustring.fromSource "ull"      , Null),
                                             (Ustring.fromSource "ew"       , New)]
                      | #"o"  => lexResOrId [(Ustring.fromSource "verride"  , Override)]
                      | #"p"  => lexResOrId [(Ustring.fromSource "rotected" , Protected),
                                             (Ustring.fromSource "recision" , Precision),
                                             (Ustring.fromSource "rototype" , Prototype),
                                             (Ustring.fromSource "ackage"   , Package),
                                             (Ustring.fromSource "rivate"   , Private),
                                             (Ustring.fromSource "ublic"    , Public)]
                      | #"r"  => lexResOrId [(Ustring.fromSource "ounding"  , Rounding),
                                             (Ustring.fromSource "eturn"    , Return)]
                      | #"s"  => lexResOrId [(Ustring.fromSource "tandard"  , Standard),
                                             (Ustring.fromSource "witch"    , Switch),
                                             (Ustring.fromSource "trict"    , Strict),
                                             (Ustring.fromSource "tatic"    , Static),
                                             (Ustring.fromSource "uper"     , Super),
                                             (Ustring.fromSource "et"       , Set)]
                      | #"t"  => lexResOrId [(Ustring.fromSource "ypeof"    , TypeOf),
                                             (Ustring.fromSource "hrow"     , Throw),
                                             (Ustring.fromSource "his"      , This),
                                             (Ustring.fromSource "ype"      , Type),
                                             (Ustring.fromSource "rue"      , True),
                                             (Ustring.fromSource "ry"       , Try),
                                             (Ustring.fromSource "o"        , To)]
                      | #"u"  => lexResOrId [(Ustring.fromSource "ndefined" , Undefined),
                                             (Ustring.fromSource "int"      , UInt),
                                             (Ustring.fromSource "se"       , Use)]
                      | #"v"  => lexResOrId [(Ustring.fromSource "oid"      , Void),
                                             (Ustring.fromSource "ar"       , Var)]
                      | #"w"  => lexResOrId [(Ustring.fromSource "hile"     , While),
                                             (Ustring.fromSource "ith"      , With)]
                      | #"x"  => lexResOrId [(Ustring.fromSource "ml"       , Token.Xml)]
                      | #"y"  => lexResOrId [(Ustring.fromSource "ield"     , Yield)]
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
                                else lexOp [(Ustring.fromSource ".." , TripleDot   ),
                                            (Ustring.fromSource "."  , DoubleDot   ),
                                            (Ustring.fromSource "<"  , LeftDotAngle),
                                            (Ustring.fromSource ""   , Dot         )]
                    (* string literal *)
                      |(#"\""
                      | #"'") => lexString c
                    (* whitespace *)
                      |(#"\t"
                      | #"\v"
                      | #"\f"
                      | #" ") => advanceIndex 1
                    (* identifier *)
                      | #"\\" => lexIdentifier []
                      | _     => if isIdentifierChar c
                                 then lexIdentifier []
                                 else error ["LexError:  illegal identifier character"]
                else
                    case c of
                    (* line terminators *)
                       (0wx2028
                      | 0wx2029) => pushEolAdv ()
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
                      | _     => if isIdentifierChar c
                                 then lexIdentifier []
                                 else error ["LexError:  illegal identifier character"]
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
