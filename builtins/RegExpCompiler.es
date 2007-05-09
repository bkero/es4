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

        const source : string;         // expression source, sans leading and trailing /
        var   idx : uint;              // current character in the source
        var   largest_backref : uint;  // largest back reference seen
        const extended : Boolean;      // true iff expression has /x flag
        const names : [string?] = [];  // capturing names, or null for capturing exprs that are not named
        const parenIndex : uint = 0;   // number of capturing parens (including those that are named)
        const parenCount : uint = 0;   // current depth of capture nesting

        function RegExpCompiler( source : string, flags : string )
            : extended = flags.indexOf("x") != -1
            , source = source
            , idx = 0              /* FIXME: redundant */
            , largest_backref = 0  /* FIXME: redundant */
        {
            skip();
        }

        public function compile() : RegExpMatcher {
            let p : Matcher = pattern();
            if (idx !== source.length)
                fail( SyntaxError, "Invalid character in input \"" + source + "\", position " + idx );
            if (largest_backref >= parenIndex && largest_backref > 0)
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
            if (eat("|"))
                return new Disjunct(alt, disjunction());
            else
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
            let y : [double, double, Boolean]? = quantifier();
            if (y === null)
                return xx;
            let [min, max, greedy] : [double,double,Boolean] = y;
            return new Quantified(parenIndex, parenCount, min, max, greedy);
        }

        function assertion() : Matcher? {
                 if (eat("^"))   return new AssertStartOfInput;
            else if (eat("$"))   return new AssertEndOfInput;
            else if (eat("\\b")) return new AssertWordBoundary;
            else if (eat("\\B")) return new AssertNotWordBoundary;
            else                 return null;
        }

        function quantifier() : [double,double,Boolean]? {
            let x : [double,double]? = quantifierPrefix();
            if (x == null)
                return x;
            let [min,max] : [double,double] = x;
            let greedy : Boolean = eat("?");
            return [min,max,greedy];
        }

        function quantifierPrefix() : [double, double]? {
                 if (eat("*")) return [0,Infinity];
            else if (eat("+")) return [1,Infinity];
            else if (eat("?")) return [0,1];
            else if (eat("{")) {
                let min : double = decimalDigits();
                let max : double = n;
                if (eat(",")) {
                    if (eat("}"))
                        max = Infinity;
                    else {
                        max = decimalDigits();
                        match("}");
                    }
                }
                if (isFinite(max) && max < min)
                    fail( SyntaxError, "max quant must be at least as large as min" );
                return [min,max];
            }
            else 
                return null;
        }

        function atom() : Matcher? {
            if (atEnd())
                return null;

            if (lookingAt(")"))
                return null;

            if (eat("."))
                return new CharsetMatcher(charset_notlinebreak);

            if (eat("(?:")) {
                let d : Matcher = disjunction();
                match(")");
                return d;
            }

            if (eat("(?=")) {
                let d : Matcher = disjunction();
                match(")");
                return new PositiveLookahead(d);
            }

            if (eat("(?!")) {
                let d : Matcher = disjunction();
                match(")");
                return new NegativeLookahead(d);
            }

            if (eat("(?#")) {
                consumeUntil(")");
                match(")");
                return new Empty;
            }
            
            if (eat("(?P<")) {
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

            if (eat("(?P=")) {
                let name : string = identifier();
                match(")");
                for ( let [i,n] : [string,string?] in names ) {
                    if (n === name)
                        return new Backref(uint(i));
                }
                fail( SyntaxError, "Unknown backref name " + name );
            }

            if (eat("(?"))
                fail( SyntaxError, "Bogus (? pattern" );

            if (eat("(")) {
                let capno : uint = parenIndex++;
                parenCount++;
                let d : Matcher = disjunction();
                parenCount--;
                match(")");
                return new Capturing(d, capno);
            }

            if (lookingAt("[")) 
                return characterClass();

            if (lookingAt("\\"))
                return atomEscape();

            if (lookingAt("^") || 
                lookingAt("$") || 
                lookingAt("*") || 
                lookingAt("+") || 
                lookingAt("?") || 
                lookingAt("{") ||
                lookingAt("}") ||
                lookingAt("]") ||
                lookingAt("|"))
                fail( SyntaxError, "Illegal character in expression." );

            return new CharsetMatcher(new CharsetAdhoc(consumeChar()));
        }

        function atomEscape() : Matcher {

            function decimalEscape(t : double) : Matcher {
                if (t >= nCapturingParens)
                    fail( SyntaxError, "Illegal backreference " + t );
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
            let inverted : Boolean = false;
            if (eat("^"))
                inverted = true;
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
                    s = CharsetIntersect(s, t);
                }
                else if (eat("\\-")) {
                    let t : Charset = nonemptyClassRanges();
                    s = CharsetSubtract(s, t);
                }
                else
                    break;
            }
            return s;
        }

        function nonemptyClassRanges(acc : Charset? = null) : Charset {

            function accumulate(acc : Charset?, x : Charset) : Charset {
                return acc === null ? x : CharsetUnion(acc, x);
            }

            let a1 : Charset = classAtom();

            if (lookingAt("]"))
                return a1;

            if (lookingAt("-")) {
                consumeChar();
                if (lookingAt("]"))
                    return accumulate(acc, CharsetAdhoc("-"));

                let a2 : Charset = classAtom();
                let a3 : Charset = accumulate(acc, CharsetRange(a1, a2));
                if (lookingAt("]"))
                    return a3;

                return nonemptyClassRanges(true, a3);
            }
            
            return nonemptyClassRanges(false, accumulate(acc,a1));
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
                         allow_b : Boolean ) : (Matcher,Charset) {
            let (t : double? = decimalEscape()) {
                if (t !== null) 
                    return de(t);
            }

            let (t : Charset? = characterClassEscape()) {
                if (t !== null)
                    return ce(t);
            }

            let (t : string? = characterEscape(allow_b)) {
                if (t !== null)
                    return ch(t);
            }

            fail( SyntaxError, "Failed to match escape sequence" );
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails. 
        */
        function decimalEscape() : double? {
            if (lookingAt("\\0") || lookingAt("\\1") || lookingAt("\\2") || lookingAt("\\3") || 
                lookingAt("\\4") || lookingAt("\\5") || lookingAt("\\6") || lookingAt("\\7") || 
                lookingAt("\\8") || lookingAt("\\9")) {
                consumeChar("\\");
                return decimalDigits();
            }
            else
                return null;
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails.  
        */
        function characterClassEscape() : Charset? {

            function unicodeSet(invert : Boolean) : Charset {
                let name : string = identifier();
                match("}");
                let (cls : Charset? = unicodeClass(name, invert));
                if (cls === null)
                    fail( ReferenceError, "Unsupported unicode character class " + name );
                return cls;
            }

            if (eat("\\d")) return charset_digit;
            if (eat("\\D")) return charset_notdigit;
            if (eat("\\s")) return charset_space;
            if (eat("\\S")) return charset_notspace;
            if (eat("\\w")) return charset_word;
            if (eat("\\W")) return charset_notword;
            if (eat("\\p{")) return unicodeSet(false);
            if (eat("\\P{")) return unicodeSet(true);

            return null;
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails. 
        */
        function characterEscape(allow_b : Boolean) : string? {

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

            if (allow_b && eat("\\b")) 
                return "\\b";
            if (eat("\\f")) 
                return "\f";
            if (eat("\\n")) 
                return "\n";
            if (eat("\\r")) 
                return "\r";
            if (eat("\\t")) 
                return "\t";
            if (eat("\\c"))
                let (c : string = consumeChar()) {
                    if (c >= "A" && c <= "Z")
                        return string.fromCharCode(c.charCodeAt(0) - "A".charCodeAt(0));
                    if (c >= "a" && c <= "z")
                        return string.fromCharCode(c.charCodeAt(0) - "a".charCodeAt(0));
                    fail( SyntaxError, "Bogus \\c sequence: " + c );
                }
            if (eat("\\x{") || eat("\\X{") || eat("\\u{") || eat("\\U{")) {
                let s : string = hexDigits();
                match("}");
                return s;
            }
            if (eat("\\x") || eat("\\X"))
                return hexDigits(2);
            if (eat("\\u") || eat("\\U"))
                return hexDigits(4);
            if (isIdentifierPart(peekChar()))
                return null;
            consumeChar("\\");
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

        function eat(c : string) : Boolean {
            if (!lookingAt(c))
                return false;
            idx += c.length;
            skip();
            return true;
        }

        function lookingAt(c : string) : void {
            if (atEnd())
                return false;
            for ( let i : uint=0 ; i < c.length && i+idx < source.length ; i++ )
                if (c[i] !== source[i+idx])
                    return false;
            return true;
        }

        function identifier() : string {
            let name : string? = null;
            if (idx < source.length) {
                let c : string = source[idx++];
                if (!isIdentifierStart(c))
                    fail( SyntaxError, "Expected identifier" );
                let name = c;
                while (idx < source.length && isIdentPart(source[idx]))
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
            idx >= source.length;

        function peekChar() {
            if (!atEnd())
                return source[idx];
            else
                return "*END*";
        }

        function consumeChar(c : string? = null) : string {
            if (!atEnd() && (c === null || source[idx] == c)) 
                return source[idx++];
            if (c !== null)
                fail( SyntaxError, "Expected character " + c );
            else
                fail( SyntaxError, "Unexected EOF" );
        }

        function consumeUntil(c : string) : void {
            while (!atEnd() && source[idx] != c)
                ++idx;
        }

        function skip() : void {
            if (!extended)
                return;

            while (!atEnd()) {
                if (source[idx] == '#') {
                    while (!atEnd() && !isTerminator(source[idx]))
                        ++idx;
                }
                else if (isBlank(source[idx]) || isTerminator(source[idx]) || isFormatControl(source[idx])) 
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
