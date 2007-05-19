/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 *
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 * E262-4 proposals:update_unicode
 *
 * Regular expression compiler.
 *
 * Status: Complete; Not reviewed; Not tested.
 */

package RegExpInternals
{
    import Unicode.*;
    use namespace intrinsic;
    use strict;

    intrinsic class RegExpCompiler
    {
        /* Invariant for token handling: either idx==source.length or source[idx] is a significant char */

        var   source : string;         // expression source, sans leading and trailing /  // FIXME: const
        var   slen : uint;             // source length, retrieved once
        var   idx : uint;              // current character in the source
        var   largest_backref : uint;  // largest back reference seen
        var   extended : boolean;      // true iff expression has /x flag  // FIXME: const
        var   names : [string?] = [];  // capturing names, or null for capturing exprs that are not named  // FIXME: const
        var   parenIndex : uint = 0;   // number of capturing parens (including those that are named) // FIXME: const
        var   parenCount : uint = 0;   // current depth of capture nesting // FIXME: const 

        function RegExpCompiler( source : string, flags  )
            : extended = flags.x
            , source = source
            , slen = source.length
            , idx = 0              /* FIXME: redundant */
            , largest_backref = 0  /* FIXME: redundant */
        {
            skip();
        }

        public function compile() : RegExpMatcher {
            let p : Matcher = pattern();
            if (idx !== slen)
                fail( SyntaxError, "Invalid character in input \"" + source + "\", position " + idx );
            if (largest_backref > parenIndex && largest_backref > 0)
                fail( SyntaxError, "Reference to undefined capture " + largest_backref );
            return new RegExpMatcher(p, parenIndex, names);
        }

        function pattern() : Matcher {
            return disjunction();
        }

        function disjunction() : Matcher {
            let alt : Matcher = alternative();
            if (alt == null)
                return new Empty;
            if (peekCharCode() == 0x7Cu /* "|" */) {
                advance();
                return new Disjunct(alt, disjunction());
            } else
                return alt;
        }

        function alternative() : Matcher? {
            let t : Matcher? = term();
            if (t === null)
                return null;
            while (true) {
                let p : Matcher? = term();
                if (p === null)
                    return t;
                t = new Alternative(t, p);
            }
        }

        function term() : Matcher? {
            let x : Matcher? = assertion();
            if (x !== null)
                return x;
            let xx : Matcher = atom();
            let y : [double, double, boolean]? = quantifier();
            if (y === null)
                return xx;
            let [min, max, greedy] : [double,double,boolean] = y;
            return new Quantified(parenIndex, parenCount, xx, min, max, greedy);
        }

        function assertion() : Matcher? {
            switch (peekCharCode()) { 

            case 0x5Eu /* "^" */ :
                advance();
                return new AssertStartOfInput;
                
            case 0x24u /* "$" */ :
                advance();
                return new AssertEndOfInput;

            case 0x5Cu /* "\\" */:
                if (eat("\\b")) return new AssertWordBoundary;
                else if (eat("\\B")) return new AssertNotWordBoundary;

            default: 
                return null;
            }
        }

        function quantifier() : [double,double,boolean]? {
            let x : [double,double]? = quantifierPrefix();
            if (x == null)
                return x;
            let [min,max] : [double,double] = x;
            let greedy : boolean = true;
            if (peekCharCode() == 0x3Fu /* "?" */) {
                greedy = false;
                advance();
            }
            return [min,max,greedy];
        }

        static const star = [0,Infinity];
        static const plus = [1,Infinity];
        static const ques = [0,1];

        function quantifierPrefix() : [double, double]? {
            switch (peekCharCode()) {
            case 0x2Au /* "*" */:
                advance();
                return star;

            case 0x2Bu /* "+" */:
                advance();
                return plus;

            case 0x3Fu /* "?" */:
                advance();
                return ques;

            case 0x7Bu /* "{" */:
                advance();
                {
                    let min : double = decimalDigits();
                    let max : double = min;
                    if (eat(",")) {
                        if (eat("}"))
                            max = Infinity;
                        else {
                            max = decimalDigits();
                            match("}");
                        }
                    } else {
                        match("}");
                    }
                    if (isFinite(max) && max < min)
                        fail( SyntaxError, "max quant must be at least as large as min" );
                    return [min,max];
                }

            default:
                return null;
            }
        }

        function atom() : Matcher? {
            if (atEnd())
                return null;

            switch (peekCharCode()) {
            case 0x29u /* ")" */:
                return null;

            case 0x2Eu /* "." */:
                advance();
                return new CharsetMatcher(charset_notlinebreak);

            case 0x28u /* "(" */:
                advance();
                
                if (peekCharCode() == 0x3Fu /* "?" */) {
                    advance();
                    switch (peekChar()) {
                    case ":":
                        advance();
                        let d : Matcher = disjunction();
                        match(")");
                        return d;
                        
                    case "=":
                        advance();
                        let d : Matcher = disjunction();
                        match(")");
                        intrinsic::assert(d !== null);
                        return new PositiveLookahead(d);
                        
                    case "!":
                        advance();
                        let d : Matcher = disjunction();
                        match(")");
                        return new NegativeLookahead(d);
                        
                    case "#":
                        advance();
                        consumeUntil(")");
                        match(")");
                        return new Empty;
                        
                    case "P":
                        advance();
                        if (eat("<")) {
                            let name : string = identifier();
                            match(">");
                            let capno : uint = parenIndex++;
                            parenCount++;
                            let d : Matcher = disjunction();
                            parenCount--;
                            match(")");
                            for each ( let n : string in names ) {
                                if (n === name)
                                    fail( SyntaxError, "Multiply defined capture name: " + name );
                            }
                            names[capno] = name;
                            return new Capturing(d, capno);
                        }
                        
                        if (eat("=")) {
                            let name : string = identifier();
                            match(")");
                            for ( let [i,n] : [string,string?] in names ) {
                                if (n === name)
                                    return new Backref(uint(i));
                            }
                            fail( SyntaxError, "Unknown backref name " + name );
                        }
                        
                    default:
                        fail( SyntaxError, "Bogus (? pattern" );
                    }
                } // peekChar() != "?"
                    
                let capno : uint = parenIndex++;
                parenCount++;
                let d : Matcher = disjunction();
                parenCount--;
                match(")");
                return new Capturing(d, capno);
                
            case 0x5Bu /* "[" */:
                return characterClass();

            case 0x5Cu /* "\\" */:
                return atomEscape();

            case 0x5Eu /* "^" */:
            case 0x24u /* "$" */:
            case 0x2Au /* "*" */:
            case 0x2Bu /* "+" */:
            case 0x3Fu /* "?" */:
            case 0x7Bu /* "{" */:
            case 0x7Cu /* "|" */:
            case 0x7Du /* "}" */:
            case 0x5Du /* "]" */:
                return null;
                
            default:                
                return new CharsetMatcher(new CharsetAdhoc(consumeChar()));
            }
        }

        function atomEscape() : Matcher {

            function decimalEscape(t : double) : Matcher {
                if (t === 0)
                    return new CharsetMatcher(new CharsetAdhoc("\x00"));
                else {
                    largest_backref = Math.max(largest_backref, t);  // Will check validity later
                    return new Backref(t);
                }
            }

            function characterClassEscape(t : Charset) : Matcher {
                return new CharsetMatcher(t);
            }

            function characterEscape(t : string) : Matcher {
                return new CharsetMatcher(new CharsetAdhoc(t));
            }

            return escape( decimalEscape, characterClassEscape, characterEscape, false );
        }

        function characterClass() : Matcher {
            match("[");
            let inverted : boolean = false;
            if (peekChar() == "^") {
                advance();
                inverted = true;
            }
            let ranges : Charset = classRanges();
            match("]");
            return new CharsetMatcher(inverted ? new CharsetComplement(ranges) : ranges);
        }

        /* The grammar for class ranges looks hairy.  The trick is to
           rewrite the grammar.  See ClassRanges.txt for an explanation.
           The final grammar is:

             ClassRanges ::= [empty] | NonemptyClassRanges ;

             NonemptyClassRanges ::= ClassAtom
                                   | ClassAtom "-" 
                                   | ClassAtom "-" ClassAtom
                                   | ClassAtom "-" ClassAtom NonemptyClassRanges
                                   | ClassAtom [lookahead != "-"] NonemptyClassRanges ;

             ClassAtom ::= "-" | ClassAtomNoDash ;
                           
          To this we add the intersection and subtraction operations.
          I'm guessing left associative is right, but we need to check
          this:

             ClassRanges ::= [empty] 
                           | ClassRanges2

             ClassRanges2 ::= NonemptyClassRanges[true]
                            | ClassRanges2 "\-" NonemptyClassRanges[true]
                            | ClassRanges2 "\&" NonemptyClassRanges[true] ;

        */

        function classRanges() : Charset {
            if (lookingAt("]"))
                return new CharsetEmpty;

            let s : Charset = nonemptyClassRanges();
            while (true) {
                if (eat("\\&")) {
                    let t : Charset = nonemptyClassRanges();
                    s = new CharsetIntersect(s, t);
                }
                else if (eat("\\-")) {
                    let t : Charset = nonemptyClassRanges();
                    s = new CharsetSubtract(s, t);
                }
                else
                    break;
            }
            return s;
        }

        function nonemptyClassRanges(acc : Charset? = null) : Charset {

            function accumulate(acc : Charset?, x : Charset) : Charset {
                return acc === null ? x : new CharsetUnion(acc, x);
            }

            let a1 : Charset = classAtom();

            if (lookingAt("]"))
                return accumulate(acc,a1);

            if (lookingAt("-")) {
                consumeChar();
                if (lookingAt("]"))
                    return accumulate(acc, new CharsetAdhoc("-"));

                if (a1.hasOneCharacter()) {
                    let a2 : Charset = classAtom();
                    if (a2.hasOneCharacter()) {
                        let a3 : Charset = accumulate(acc, 
                                                      new CharsetRange(a1.singleCharacter(), 
                                                                       a2.singleCharacter()));
                        if (lookingAt("]"))
                            return a3;

                        return nonemptyClassRanges(a3);
                    }
                    else
                        return nonemptyClassRanges(accumulate(accumulate(accumulate(acc,a1),
                                                                         new CharsetAdhoc("-")),
                                                              a2));
                }
                else
                    return nonemptyClassRanges(accumulate(accumulate(acc,a1),
                                                          new CharsetAdhoc("-")));
            }
            
            return nonemptyClassRanges(accumulate(acc,a1));
        }

        function classAtom() : Charset {
            if (lookingAt("]") || atEnd())
                fail( SyntaxError, "Premature end of input" );

            if (lookingAt("\\"))
                return classEscape();

            return new CharsetAdhoc(consumeChar());
        }

        function classEscape() : Charset {
            return escape( (function(t : double) : Charset new CharsetAdhoc(string.fromCharCode(t))),
                           (function(t : Charset) : Charset t),
                           (function(t : string) : Charset new CharsetAdhoc(t)),
                           true );
        }

        /* Parse an escape sequence. */
        function escape( de : function (double) : (Matcher,Charset),
                         ce : function (Charset) : (Matcher,Charset),
                         ch : function (string) : (Matcher,Charset),
                         allow_b : boolean ) : (Matcher,Charset) {

            let (t : Charset? = characterClassEscape()) {
                if (t !== null)
                    return ce(t);
            }

            let (t : string? = characterEscape(allow_b)) {
                if (t !== null)
                    return ch(t);
            }

            let (t : double? = decimalEscape()) {
                if (t !== null) 
                    return de(t);
            }

            eat("\\");
            fail( SyntaxError, "Failed to match escape sequence " + peekChar() );
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails. 
        */
        function decimalEscape() : double? {
            if (peekChar() != "\\")
                return null;
            let saved : uint = idx;
            advance();
            let c : uint = peekCharCode();
            if (c >= 0x30u && c <= 0x39u) {
                return decimalDigits();
            }
            idx = saved;
            return null;
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails.  
        */
        function characterClassEscape() : Charset? {

            function unicodeSet(invert : boolean) : Charset {
                let name : string = identifier();
                match("}");
                let (cls : Charset? = unicodeClass(name, invert));
                if (cls === null)
                    fail( ReferenceError, "Unsupported unicode character class " + name );
                return cls;
            }

            if (peekCharCode() != 0x5Cu /* "\\" */)
                return null;

            let saved : uint = idx;
            advance();

            let invert : boolean = true;

            switch (peekCharCode()) {
                
            case 0x64u /* "d" */: advance(); return charset_digit;
            case 0x44u /* "D" */: advance(); return charset_nondigit;
            case 0x73u /* "s" */: advance(); return charset_space;
            case 0x53u /* "S" */: advance(); return charset_notspace;
            case 0x77u /* "w" */: advance(); return charset_word;
            case 0x57u /* "W" */: advance(); return charset_notword;

            case 0x70u /* "p" */:
                invert = false;
            case 0x50u /* "P" */:
                {
                    let saved : uint = idx;
                    advance();
                    if (peekChar() == "{") {
                        advance();
                        return unicodeSet(invert);
                    }
                    idx = saved;
                }
            }
            idx = saved;
            return null;
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails. 
        */
        function characterEscape(allow_b : boolean) : string? {

            function hexValue(c) : double {
                if (c >= "0" && c <= "9")
                    return c.charCodeAt(0) - "0".charCodeAt(0);
                else
                    return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
            }

            function hexDigits(n : uint? = null) : string {
                let k : double = 0;
                let c : string;
                let m : uint = n === null ? 100000 : n;
                let i : uint;
                for ( i=0 ; i < m ; i++ ) {
                    let (c = peekChar()) {
                        if (!isHexdigit(c)) 
                            break;
                        k = k*16 + hexValue(consumeChar(c));
                    }
                }
                if (n !== null && i < m || i == 0)
                    fail( SyntaxError, "hex sequence too short" );
                skip();
                return string.fromCharCode(k);
            }

            let c : uint = peekCharCode();

            if (c != 0x5Cu /* "\\" */)
                return null;
            
            advance();
            c = peekCharCode();
            
            switch (c) {                
            case 0x62u /* "b" */:
                if (allow_b) {
                    advance();
                    return "\\b";
                }
                break;
                
            case 0x66u /* "f" */: advance(); return "\f";
            case 0x6Eu /* "n" */: advance(); return "\n";
            case 0x72u /* "r" */: advance(); return "\r";
            case 0x74u /* "t" */: advance(); return "\t";
                
            case 0x63u /* "c" */:
                advance();
                let (c : string = consumeChar()) {
                    if (c >= "A" && c <= "Z")
                        return string.fromCharCode(c.charCodeAt(0) - "A".charCodeAt(0));
                    if (c >= "a" && c <= "z")
                        return string.fromCharCode(c.charCodeAt(0) - "a".charCodeAt(0));
                    fail( SyntaxError, "Bogus \\c sequence: " + c );
                }
                
            case 0x77u /* "x" */: 
            case 0x57u /* "X" */: 
            case 0x75u /* "u" */: 
            case 0x55u /* "U" */:
                advance();
                if (peekCharCode() == 0x7Bu /* "{" */) {
                    advance();
                    let s : string = hexDigits();
                    match("}");
                    return s;
                } else if (c == 0x77u /* "x" */ || c == 0x57u /* "X" */) {
                    return hexDigits(2);
                } else {
                    return hexDigits(4);
                }
            }
            
            if (atEnd())
                fail( SyntaxError, "EOF inside escape sequence" );
            
            return consumeChar();
        }

        /*** Token handling ***/

        /* If c matches the next characters (not skipping blanks), consume those
           characters and any intertoken space following, and return true.
           Otherwise return false.  */

        function match(c : string) : void {
            if (!eat(c))
                fail( SyntaxError, "Expected token here: " + c );
        }

        function eat(c : string) : boolean {
            if (!lookingAt(c))
                return false;
            idx += c.length;
            skip();
            return true;
        }

        function lookingAt(c : string) : void {
            if (atEnd())
                return false;
            let i : uint = 0;
            let j : uint = idx;
            let ilim = i + c.length;
            let jlim = j + slen;
            for ( ; i < ilim && j < jlim ; i++, j++ )
                if (c[i] !== source[j])
                    return false;
            return true;
        }

        function identifier() : string {
            let name : string? = null;
            if (idx < slen) {
                let c : string = source[idx++];
                if (!isIdentifierStart(c))
                    fail( SyntaxError, "Expected identifier" );
                let name = c;
                while (idx < slen && isIdentifierPart(source[idx]))
                    name += source[idx++];
                skip();
                return name;
            }
            else
                fail( SyntaxError, "Expected identifier" );
        }

        function decimalDigits() : double {
            let k : double = 0;
            while (isDecimalDigit(c = peekChar()))
                k = k*10 + consumeChar(c).charCodeAt(0) - "0".charCodeAt(0);
            skip();
            return k;
        }

        function atEnd()
            idx >= slen;

        function peekChar() {
            if (idx < slen)
                return source[idx]
            else
                return "*END*";
        }

        function peekCharCode() { 
            // In a production implementation, this would probably be
            // no faster than peekChar. In our reference
            // implementation, it is substantially faster.
            if (idx < slen)
                return magic::charCodeAt(source, uint(idx));
            else
                return 0x0u;
        }

        function consumeChar(c : string? = null) : string {
            if (!atEnd() && (c === null || source[idx] == c)) 
                return source[idx++];
            if (c !== null)
                fail( SyntaxError, "Expected character " + c );
            else
                fail( SyntaxError, "Unexected EOF" );
        }

        function advance() {
            if (idx + 1 > slen)
                fail( SyntaxError, "advancing beyond end of regexp");
            idx++;
            if (extended)
                skip();
        }

        function consumeUntil(c : string) : void {
            while (!atEnd() && source[idx] != c)
                ++idx;
        }

        function skip() : void {
            if (!extended)
                return;

            while (!atEnd()) {
                let c : uint = peekCharCode();
                if (c == 0x22u /* '#' */) {                    
                    ++idx;
                    while (!atEnd() && !isTerminator(peekCharCode()))
                        ++idx;
                }
                else if (isBlankCode(c) || isTerminatorCode(c) || isFormatControlCode(c)) 
                    ++idx;
                else
                    return;
            }
        }

        function fail( err, msg ) {
            throw new err( "Error in RegExp compiler:\n" +
                           "  input: " + source + "\n" +
                           "  pos: " + idx + "\n" +
                           "  msg: " + msg );
        }
    }
}
