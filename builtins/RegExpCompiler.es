/* -*- indent-tabs-mode: nil -*- 
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

package RegExp
{
    import Unicode.*;
    use strict;

    class RegExpCompiler
    {
        /* Invariant for token handling: either idx==source.length or source[idx] is a significant char */

        const source : String!;        // expression source, sans leading and trailing /
        var   idx : uint;              // current character in the source
        var   largest_backref : uint;  // largest back reference seen
        const extended : Boolean;      // true iff expression has /x flag
        const names : [String] = [];   // capturing names, or null for capturing exprs that are not named
        const parenIndex : uint = 0;   // number of capturing parens (including those that are named)
        const parenCount : uint = 0;   // current depth of capture nesting

        function RegExpCompiler( source : String!, flags : String! )
            : extended = flags.indexOf("x") != -1
            , source = source
            , idx = 0
        {
            skip();
        }

        public function compile() : RegExpMatcher {
            let p : Matcher = pattern();
            if (idx !== source.length)
                throw new SyntaxError("Invalid character in input \"" + source + "\", position " + idx);
            if (largest_backref >= parenIndex)
                throw new SyntaxError("Reference to undefined capture " + largest_backref);
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
            let x : Matcher = atom();
            let y : [Number, Number, Boolean]? = quantifier();
            if (y === null)
                return x;
            let [min, max, greedy] : [Number,Number,Boolean] = y;
            return new Quantified(parenIndex, parenCount, min, max, greedy);
        }

        function assertion() : Matcher? {
                 if (eat("^"))   return new AssertStartOfInput;
            else if (eat("$"))   return new AssertEndOfInput;
            else if (eat("\\b")) return new AssertWordBoundary;
            else if (eat("\\B")) return new AssertNotWordBoundary;
            else                 return null;
        }

        function quantifier() : [Number,Number,Boolean]? {
            let x : [Number,Number]? = quantifierPrefix();
            if (x == null)
                return x;
            let [min,max] : [Number,Number] = x;
            let greedy : Boolean = eat("?");
            return [min,max,greedy];
        }

        function quantifierPrefix() : [Number, Number]? {
                 if (eat("*")) return [0,intrinsic::Infinity];
            else if (eat("+")) return [1,intrinsic::Infinity];
            else if (eat("?")) return [0,1];
            else if (eat("{")) {
                let min : Number = decimalDigits();
                let max : Number = n;
                if (eat(",")) {
                    if (eat("}"))
                        max = intrinsic::Infinity;
                    else {
                        max = decimalDigits();
                        match("}");
                    }
                }
                if (intrinsic::isFinite(max) && max < min)
                    throw new SyntaxError("max must be at least as large as min");
                return [min,max];
            }
            else 
                return null;
        }

        function atom() : Matcher {
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
                let name : String! = identifier();
                match(">");
                let capno : uint = parenIndex++;
                parenCount++;
                let d : Matcher = disjunction();
                parenCount--;
                match(")");
                for each ( let n : String! in names ) {
                    if (n === name)
                        throw new SyntaxError("Multiply defined capture name: " + name );
                }
                names[capno] = name;
                return new Capturing(d, capno);
            }

            if (eat("(?P=")) {
                let name : String! = identifier();
                match(")");
                for ( let [i,n] : [String!,String] in names ) {
                    if (n === name)
                        return new Backref(uint(i));
                }
                throw new SyntaxError("Unknown backref name " + name );
            }

            if (eat("(?"))
                throw new SyntaxError("Bogus (? pattern");

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
                lookingAt(")") ||
                lookingAt("|"))
                throw new SyntaxError("Illegal character in expression.");

            return new CharsetMatcher(new CharsetAdhoc[consume()]);
        }

        function atomEscape() : Matcher {

            function decimalEscape(t : Number) : Matcher {
                if (t >= nCapturingParens)
                    throw new SyntaxError("Illegal backreference " + t);
                if (t === 0)
                    return new CharsetMatcher(new CharsetAdhoc(["\x00"]));
                else {
                    largest_backref = Math.max(largest_backref, t);  // Will check validity later
                    return new Backref(t);
                }
            }

            function characterClassEscape(t : Charset) : Matcher {
                return new CharsetMatcher(t);
            }

            function characterEscape(t : String) : Matcher {
                return new CharsetMatcher(new CharsetAdhoc([t]));
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
                consume();
                if (lookingAt("]"))
                    return accumulate(acc, CharsetAdhoc(["-"]));

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
                throw SyntaxError("Premature end of input");

            if (lookingAt("\\"))
                return classEscape();

            return new CharsetAdhoc(consume());
        }

        function classEscape() : Charset {
            return escape( (function(t : Number) : Charset new CharsetAdhoc([String.fromCharCode(t)])),
                           (function(t : Charset) : Charset t),
                           (function(t : String) : Charset new CharsetAdhoc(t)),
                           true );
        }

        /* Parse an escape sequence. */
        function escape( de : function (Number) : (Matcher,Charset),
                         ce : function (Charset) : (Matcher,Charset),
                         ch : function (String) : (Matcher,Charset),
                         allow_b : Boolean ) : (Matcher,Charset) {
            let (t : Number? = decimalEscape()) {
                if (t !== null) 
                    return de(t);
            }

            let (t : Charset? = characterClassEscape()) {
                if (t !== null)
                    return ce(t);
            }

            let (t : String = characterEscape(allow_b)) {
                if (t !== null)
                    return ch(t);
            }

            throw new SyntaxError("Failed to match escape sequence");
        }

        /* Returns null if it does not consume anything but fails;
           throws an error if it consumes and then fails. 
        */
        function decimalEscape() : Number? {
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
                let name : String! = identifier();
                match("}");
                let (cls : Charset? = unicodeClass(name, invert));
                if (cls === null)
                    throw new ReferenceError("Unsupported unicode character class " + name);
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
        function characterEscape(allow_b : Boolean) : String? {

            function hexValue(c) : Number {
                if (c >= "0" && c <= "9")
                    return c.charCodeAt(0) - "0".charCodeAt(0);
                else
                    return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
            }

            function hexDigits(n : uint? = null) : String! {
                let k : Number = 0;
                let c : String!;
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
                    throw new SyntaxError("hex sequence too short");
                skip();
                return String.fromCharCode(k);
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
                let (c : String! = consumeChar()) {
                    if (c >= "A" && c <= "Z")
                        return String.fromCharCode(c.charCodeAt(0) - "A".charCodeAt(0));
                    if (c >= "a" && c <= "z")
                        return String.fromCharCode(c.charCodeAt(0) - "a".charCodeAt(0));
                    throw new SyntaxError("Bogus \\c sequence: " + c);
                }
            if (eat("\\x{") || eat("\\X{") || eat("\\u{") || eat("\\U{")) {
                let s : String! = hexDigits();
                match("}");
                return s;
            }
            if (eat("\\x") || eat("\\X"))
                return hexDigits(2);
            if (eat("\\u") || eat("\\U"))
                return hexDigits(4);
            if (isIdentifierPart(c))
                return null;
            consumeChar("\\");
            if (atEnd())
                throw new SyntaxError("EOF inside escape sequence");
            return consumeChar();
        }

        /*** Token handling ***/

        /* If c matches the next characters (not skipping blanks), consume those
           characters and any intertoken space following, and return true.
           Otherwise return false.  */

        function match(c : String!) : void {
            if (!eat(c))
                throw new SyntaxError("Expected token here: " + c);
        }

        function eat(c : String!) : Boolean {
            if (!lookingAt(c))
                return false;
            idx += c.length;
            skip();
            return true;
        }

        function lookingAt(c : String!) : void {
            for ( let i : uint=0 ; i < c.length && i+idx < source.length ; i++ )
                if (c[i] != source[i+idx])
                    return false;
            return true;
        }

        function identifier() : String! {
            let name : String = null;
            if (idx < source.length) {
                let c : String! = source[idx++];
                if (!isIdentifierStart(c))
                    throw new SyntaxError("Expected identifier");
                let name = c;
                while (idx < source.length && isIdentPart(source[idx]))
                    name += source[idx++];
                skip();
                return name;
            }
            else
                throw new SyntaxError("Expected identifier");
        }

        function decimalDigits() : Number {
            let k : Number = 0;
            while (isDecimalDigit(c = peekChar()))
                k = k*10 + consumeChar(c).charCodeAt(0) - "0".charCodeAt(0);
            skip();
            return k;
        }

        function atEnd() {
            return idx < source.length;
        }

        function peekChar() {
            if (!atEnd())
                return source[idx];
            else
                return "*END*";
        }

        function consumeChar(c : String = null) : String! {
            if (!atEnd() && c === null || source[idx] == c)
                return source[idx++];
            if (c !== null)
                throw new SyntaxError("Expected character " + c);
            else
                throw new SyntaxError("Unexected EOF");
        }

        function consumeUntil(c : String!) : void {
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
    }
}
