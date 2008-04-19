(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)

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

            fun readHexEscape () =
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

            val (adv, codepoint) = case !src of
                (0wx5C::0wx75::0wx7B::_) => (* \u{ *)
                readHexEscape ()
              | (0wx5C::0wx78::0wx7B::_) => (* \x{ *)
                readHexEscape ()
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
              | 0wx5C::0wx000A::_ =>         (* \<line terminator> *)
                error ["LexError:  illegal line-terminating escape sequence"]
              | 0wx5C::0wx000D::_ =>         (* \<line terminator> *)
                error ["LexError:  illegal line-terminating escape sequence"]
              | 0wx5C::0wx2028::_ =>         (* \<line terminator> *)
                error ["LexError:  illegal line-terminating escape sequence"]
              | 0wx5C::0wx2029::_ =>         (* \<line terminator> *)
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
            The 3 lexical digit patterns fall into 2 final token types
            (including explicit qualifiers)

              HexIntLit
                0 [xX] [0-9a-fA-F]+

              DecIntLit
                0
                [1-9] [0-9]*

              DecLit
                0                        [eE] [+-]? [0-9]+
                [1-9] [0-9]*             [eE] [+-]? [0-9]+
                [1-9] [0-9]* . [0-9]*  ( [eE] [+-]? [0-9]+ )?
                             . [0-9]+  ( [eE] [+-]? [0-9]+ )?
                0            . [0-9]*  ( [eE] [+-]? [0-9]+ )?


              DoubleLiteral
				{HexIntLit}
                {DecIntLit}
                {DecLit}
				{HexIntLit} d
                {DecIntLit} d
                {DecLit} d

              DecimalLiteral
                {DecIntLit} m
                {DecLit} m

            So we search for the first 3, then see at the end if there's a
            trailing [dm], and decide which of the 2 categories to convert to
        *)
        let
	    fun readDouble (isHex:bool) (str:string) = 
			if isHex 
			then 
				case Word32.fromString str of 
				    SOME w => DoubleLiteral (Real64.fromLargeInt (Word32.toLargeInt w))
				  | NONE => error ["LexError: error converting hex literal"]
			else 
				case Real64.fromString str of
				    SOME li => DoubleLiteral li
				  | NONE => error ["LexError: error converting double literal"]
							
        fun countExpChars chars =
            let
                val numSignChars = countInRanges {min=0} [(0wx2B,0wx2B) , (0wx2D,0wx2D)] chars (* [+-] *)
                val rest  = case numSignChars of
                                0 => chars
                              | 1 => tl chars
                              | _ => error ["LexError:  illegal characters in number literal"]
                val numExpDigits = countInRanges {min=1} [(Ustring.wcharFromChar #"0",
							   Ustring.wcharFromChar #"9")] rest
            in
                numSignChars + numExpDigits
            end

            fun countOctalDigits rest =
            let
                val octDigitRanges = [(Ustring.wcharFromChar #"0", Ustring.wcharFromChar #"7")]
            in
                countInRanges {min=0} octDigitRanges rest
            end

	    fun countPastDecimalPoint rest =
		let
                    val numMoreDigits = countInRanges {min=0} [(Ustring.wcharFromChar #"0",
								Ustring.wcharFromChar #"9")] rest
                    val numExpChars = case List.drop (rest, numMoreDigits) of
                                          (* [eE] *)
                                          0wx65::rest_
                                          => 1 + (countExpChars rest_)
                                        | 0wx45::rest_
                                          => 1 + (countExpChars rest_)
                                        | _ => 0
                in
                    numMoreDigits + numExpChars
                end
            fun lexDecimalPastFirstDigit rest =
            let
                val numDigits = countInRanges {min=0} [(Ustring.wcharFromChar #"0",
							Ustring.wcharFromChar #"9")] rest
            in
                case List.drop (rest, numDigits) of
                    0wx65::rest_ (* e *)
                    => (DecLit, 1 + numDigits + 1 + (countExpChars rest_))
                  | 0wx45::rest_ (* E *)
                    => (DecLit, 1 + numDigits + 1 + (countExpChars rest_))
                  | 0wx2E::rest_ (* . *)
                    => (DecLit, 1 + numDigits + 1 + countPastDecimalPoint rest_)
                  | _
		    => (DecIntLit, 1 + numDigits)
            end

            val (tokType, tokLen) = case !src of
              (* .  =>  DecLit {   . [0-9]+  ( [eE] [+-]? [0-9]+ )? } *)
                0wx2E::rest =>  
		(DecLit, 1 + countPastDecimalPoint rest)
              (* 0. =>  DecLit { 0 . [0-9]*  ( [eE] [+-]? [0-9]+ )? } *)
              | 0wx30::0wx2E::rest =>  
		(DecLit, 2 + countPastDecimalPoint rest)
            (* 0[eE]  =>  DecLit { 0 [eE] [+-]? [0-9]+ } *)
              | 0wx30::0wx65::rest => (DecLit, 2 + (countExpChars rest))
              | 0wx30::0wx45::rest => (DecLit, 2 + (countExpChars rest))
            (* 0[xX]  =>  HexIntLit { 0 [xX] [0-9a-fA-F]+ } *)
              | 0wx30::0wx78::rest => 
		(HexIntLit, 2 + (countInRanges {min=1} hexDigitRanges rest))
              | 0wx30::0wx58::rest => 
		(HexIntLit, 2 + (countInRanges {min=1} hexDigitRanges rest))
            (* 0   =>  DecIntLit { 0 } *)
              | 0wx30::rest => 
		(DecIntLit, 1)
            (* [1-9] *)
            (*   =>  DecIntLit { [1-9] [0-9]*                                  } *)
            (*   |   DecLit    { [1-9] [0-9]*             [eE] [+-]? [0-9]+    } *)
            (*   |   DecLit    { [1-9] [0-9]* . [0-9]*  ( [eE] [+-]? [0-9]+ )? } *)
              | 0wx31::rest => lexDecimalPastFirstDigit rest
              | 0wx32::rest => lexDecimalPastFirstDigit rest
              | 0wx33::rest => lexDecimalPastFirstDigit rest
              | 0wx34::rest => lexDecimalPastFirstDigit rest
              | 0wx35::rest => lexDecimalPastFirstDigit rest
              | 0wx36::rest => lexDecimalPastFirstDigit rest
              | 0wx37::rest => lexDecimalPastFirstDigit rest
              | 0wx38::rest => lexDecimalPastFirstDigit rest
              | 0wx39::rest => lexDecimalPastFirstDigit rest
              | _ => error ["LexError:  illegal character in numeric literal (BUG IN LEXER!)"]

            val numberAscii = implode (map Ustring.wcharToChar (List.take (!src, tokLen)))  
	(* should be safe, since we just lexed each char as being in the ascii range *)
        in
            case (tokType, lookahead tokLen) of
		
				(HexIntLit, 0wx64 (* d *)) =>  push (tokLen+1) (readDouble true numberAscii)
			  | (HexIntLit, _) =>  push tokLen (readDouble true numberAscii)
			  | (_, 0wx64 (* d *)) => push (tokLen+1) (readDouble false numberAscii)
									  
			  | (DecIntLit, 0wx6D (* m *)) => 
				(case Decimal.fromStringDefault numberAscii of
					 SOME i => push (tokLen+1) (DecimalLiteral i)
				   | NONE   => error ["LexError:  LEXER BUG in lexing DecIntLit(m)"])		  
				
			  | (DecLit   , 0wx6D (* m *)) => 
				(case Decimal.fromStringDefault numberAscii of
					 SOME i => push (tokLen+1) (DecimalLiteral i)
				   | NONE   => error ["LexError:  LEXER BUG in lexing DecLit(m)"   ])
				
			  | (_, _) => push tokLen (readDouble false numberAscii)
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
                (* line terminators *)
                    0wx000A => pushEolAdv ()
                  | 0wx000D => pushEolAdv ()
                  | 0wx2028 => pushEolAdv ()
                  | 0wx2029 => pushEolAdv ()
		  | 0wx0 => ()
                  | _ => (advanceIndex 1; lexSingleLineComment ())

            fun lexMultiLineComment {newline=false} {asterisk=asterisk} = (* have not encountered a newline yet *)
                (case lookahead 0 of
                (* line terminators *)
                    0wx000A => (pushEolAdv  (); lexMultiLineComment {newline=true } {asterisk=false})
                  | 0wx000D => (pushEolAdv  (); lexMultiLineComment {newline=true } {asterisk=false})
                  | 0wx2028 => (pushEolAdv  (); lexMultiLineComment {newline=true } {asterisk=false})
                  | 0wx2029 => (pushEolAdv  (); lexMultiLineComment {newline=true } {asterisk=false})
                (* * *)
                  | 0wx2A => (advanceIndex 1; lexMultiLineComment {newline=false} {asterisk=true })
                (* / *)
                  | 0wx2F => (advanceIndex 1; if asterisk then () else
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

                        fun lexRegexp reSrc {newline=newline} {depth=depth} =
                        let
                            val c = lookahead 0
                            val _ = if c = 0wx0 then () else advanceIndex 1
                        in
                            case (depth, c) of
                            (* line terminators *)
                                (_, 0wx000A)   => lexRegexp (c::reSrc) {newline=true   } {depth=depth}
                              | (_, 0wx000D)   => lexRegexp (c::reSrc) {newline=true   } {depth=depth}
                              | (_, 0wx2028)   => lexRegexp (c::reSrc) {newline=true   } {depth=depth}
                              | (_, 0wx2029)   => lexRegexp (c::reSrc) {newline=true   } {depth=depth}
                            (* [ *)
                              | (0, 0wx5B) => lexRegexp (c::reSrc) {newline=newline} {depth=1}
                            (* ] *)
                              | (0, 0wx5D) => lexRegexp (c::reSrc) {newline=newline} {depth=0}
                              | (_, 0wx5D) => lexRegexp (c::reSrc) {newline=newline} {depth=depth-1}
                            (* & *)
                              | (0, 0wx26) => lexRegexp (c::reSrc) {newline=newline} {depth=depth}
                              | (_, 0wx26) =>
                                (case (lookahead 0, lookahead 1) of
                                    (0wx26, 0wx5B) => (advanceIndex 2; lexRegexp (0wx5B::0wx26::c::reSrc) {newline=newline} {depth=depth+1})
                                  | (_,_) => lexRegexp (c::reSrc) {newline=newline} {depth=depth})
                            (* \ *)
                              | (_, 0wx5C) =>
				(case lookahead 0 of
                                 (* line terminators *)
                                     0wx000A
                                     =>   (advanceIndex 1; lexRegexp (      reSrc) {newline=newline} {depth=depth})
                                   | 0wx000D
                                     =>   (advanceIndex 1; lexRegexp (      reSrc) {newline=newline} {depth=depth})
                                   | 0wx2028
                                     =>   (advanceIndex 1; lexRegexp (      reSrc) {newline=newline} {depth=depth})
                                   | 0wx2029
                                     =>   (advanceIndex 1; lexRegexp (      reSrc) {newline=newline} {depth=depth})
				   | 0wx0 => error ["LexError: end of input in regexp"]
                                   | d => (advanceIndex 1; lexRegexp (d::c::reSrc) {newline=newline} {depth=depth}))
                              | (0, 0wx2F (* / *)) =>
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
                              | _ => lexRegexp (c::reSrc) {newline=newline} {depth=depth}
                        end

                        val _ = advanceIndex 1 (* to move past the initial slash *)
                        val regexpStr = Vector.fromList (lexRegexp [0wx2F (* / *)] {newline=false} {depth=0})
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
                        #"\n" => pushEolAdv ()
                      | #"\r" => pushEolAdv ()
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
                      | #"b"  => lexResOrId [(Ustring.fromSource "reak"     , Break)]
                      | #"c"  => lexResOrId [(Ustring.fromSource "ontinue"  , Continue),
                                             (Ustring.fromSource "atch"     , Catch),
                                             (Ustring.fromSource "lass"     , Class),
                                             (Ustring.fromSource "onst"     , Const),
                                             (Ustring.fromSource "ase"      , Case),
                                             (Ustring.fromSource "ast"      , Cast)]
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
                      | #"g"  => lexResOrId [(Ustring.fromSource "et"       , Get)]
                      | #"h"  => lexResOrId [(Ustring.fromSource "as"       , Has)]
                      | #"i"  => lexResOrId [(Ustring.fromSource "mplements", Implements),
                                             (Ustring.fromSource "nstanceof", InstanceOf),
                                             (Ustring.fromSource "nterface" , Interface),
                                             (Ustring.fromSource "ntrinsic" , Intrinsic),
                                             (Ustring.fromSource "nternal"  , Internal),
                                             (Ustring.fromSource "mport"    , Import),
                                             (Ustring.fromSource "s"        , Is),
                                             (Ustring.fromSource "f"        , If),
                                             (Ustring.fromSource "n"        , In)]
                      | #"l"  => lexResOrId [(Ustring.fromSource "et"       , Let),
					     (Ustring.fromSource "ike"      , Like)]
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
                                             (Ustring.fromSource "ry"       , Try)]
                      | #"u"  => lexResOrId [(Ustring.fromSource "ndefined" , Undefined),
                                             (Ustring.fromSource "se"       , Use)]
                      | #"v"  => lexResOrId [(Ustring.fromSource "oid"      , Void),
                                             (Ustring.fromSource "ar"       , Var)]
                      | #"w"  => lexResOrId [(Ustring.fromSource "hile"     , While),
                                             (Ustring.fromSource "ith"      , With)]
                      | #"x"  => lexResOrId [(Ustring.fromSource "ml"       , Token.Xml)]
                      | #"y"  => lexResOrId [(Ustring.fromSource "ield"     , Yield)]
                    (* numbers *)
                      | #"0" => lexNumber ()
                      | #"1" => lexNumber ()
                      | #"2" => lexNumber ()
                      | #"3" => lexNumber ()
                      | #"4" => lexNumber ()
                      | #"5" => lexNumber ()
                      | #"6" => lexNumber ()
                      | #"7" => lexNumber ()
                      | #"8" => lexNumber ()
                      | #"9" => lexNumber ()
                    (* numbers ... .. .< . *)
                      | #"." => if (0wx30 <= (lookahead 1) andalso
                                             (lookahead 1) <= 0wx39) (* 0-9 *)
                                then lexNumber ()
                                else lexOp [(Ustring.fromSource ".." , TripleDot   ),
                                            (Ustring.fromSource "."  , DoubleDot   ),
                                            (Ustring.fromSource "<"  , LeftDotAngle),
                                            (Ustring.fromSource ""   , Dot         )]
                    (* string literal *)
                      | #"\"" => lexString c
                      | #"'"  => lexString c
                    (* whitespace *)
                      | #"\t" => advanceIndex 1
                      | #"\v" => advanceIndex 1
                      | #"\f" => advanceIndex 1
                      | #" "  => advanceIndex 1
                    (* identifier *)
                      | #"\\" => lexIdentifier []
                      | _     => if isIdentifierChar c
                                 then lexIdentifier []
                                 else error ["LexError:  illegal identifier character"]
                else
                    case c of
                    (* line terminators *)
                        0wx2028 => pushEolAdv ()
                      | 0wx2029 => pushEolAdv ()
                    (* whitespace *)
                      | 0wx00A0 => advanceIndex 1
                      | 0wx1680 => advanceIndex 1
                      | 0wx180E => advanceIndex 1
                      | 0wx2000 => advanceIndex 1
                      | 0wx2001 => advanceIndex 1
                      | 0wx2002 => advanceIndex 1
                      | 0wx2003 => advanceIndex 1
                      | 0wx2004 => advanceIndex 1
                      | 0wx2005 => advanceIndex 1
                      | 0wx2006 => advanceIndex 1
                      | 0wx2007 => advanceIndex 1
                      | 0wx2008 => advanceIndex 1
                      | 0wx2009 => advanceIndex 1
                      | 0wx200A => advanceIndex 1
                      | 0wx202F => advanceIndex 1
                      | 0wx205F => advanceIndex 1
                      | 0wx3000 => advanceIndex 1
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
