/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "RegExp" object
 *
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 * E262-4 proposals:update_unicode
 *
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
 *
 *
 * Regular expression compiler.
 *
 * Status: Complete; Not reviewed; Not tested.
 *
 * The committee agreed in its 2007-06-05 phone conference to remove
 * support for octal literals from the reference implementation.
 * Following the grammar of E262-3, numbers other than zero may not
 * start with the digit '0'.
 */

    use namespace helper;
    use namespace intrinsic;
    use namespace Unicode;
    use namespace RegExpInternals;
    use default namespace RegExpInternals;

    intrinsic class RegExpCompiler
    {
        /* Invariant for token handling: either idx==source.length or source[idx] is a significant char */

        const source : string;         // expression source, sans leading and trailing / 
        var   slen : double;             // source length, retrieved once
        var   idx : double;              // current character in the source
        var   largest_backref : double;  // largest back reference seen
        const extended : boolean;      // true iff expression has /x flag
        const names : [string?] = []:[string?];  // capturing names, or null for capturing exprs that are not named
        var parenIndex : double = 0;     // number of capturing parens (including those that are named)

        function RegExpCompiler( source : string, flags  )
            : extended = flags.x
            , source = source
            , slen = source.length
        {
            skip();
        }

        public function compile() : [RegExpMatcher, [string?]] {
            let p : Matcher = pattern();
            if (idx !== slen)
                fail( SyntaxError, "Invalid character in input \"" + source + "\", position " + idx );
            if (largest_backref > parenIndex && largest_backref > 0)
                fail( SyntaxError, "Reference to undefined capture " + largest_backref );
            return [new RegExpMatcher(p, parenIndex), names]
        }

        function pattern() : Matcher {
            return disjunction();
        }

        function disjunction() : Matcher {
            let alt : Matcher = alternative();
            if (alt == null)
                alt = new Empty;
            if (peekCharCode() == 0x7C /* "|" */) {
                advance();
                return new Disjunct(alt, disjunction());
            }
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
                t = new Conjunct(t, p);
            }
        }

        function term() : Matcher? {
            let savedParenIndex = parenIndex;
            let (a : Matcher? = assertion()) {
                if (a !== null)
                    return a;
            }
            let m : Matcher? = atom();
            if (m === null)
                return m;
            let q : [double, double, boolean]? = quantifier();
            if (q === null)
                return m;
            let [min, max, greedy] : [double,double,boolean] = q;
            return new Quantified(savedParenIndex, parenIndex - savedParenIndex, m, min, max, greedy);
        }

        function assertion() : Matcher? {
            switch (peekCharCode()) {

            case 0x5E /* "^" */ :
                advance();
                return new AssertStartOfInput;

            case 0x24 /* "$" */ :
                advance();
                return new AssertEndOfInput;

            case 0x5C /* "\\" */:
                if (eat("\\b"))
                    return new AssertWordBoundary;
                if (eat("\\B"))
                    return new AssertNotWordBoundary;
                return null;

            default:
                return null;
            }
        }

        function quantifier() : [double,double,boolean]? {
            let qp : [double,double]? = quantifierPrefix();
            if (qp === null)
                return qp;
            let [min,max] : [double,double] = qp;
            let greedy : boolean = true;
            if (peekCharCode() == 0x3F /* "?" */) {
                greedy = false;
                advance();
            }
            return [min,max,greedy] : [double,double,boolean];
        }

        static const star = [0,Infinity] : [double,double];
        static const plus = [1,Infinity] : [double,double];
        static const ques = [0,1] : [double,double];

        function quantifierPrefix() : [double, double]? {
            switch (peekCharCode()) {
            case 0x2A /* "*" */:
                advance();
                return star;

            case 0x2B /* "+" */:
                advance();
                return plus;

            case 0x3F /* "?" */:
                advance();
                return ques;

            case 0x7B /* "{" */:
                advance();
                {
                    let min : double = number();
                    let max : double = min;
                    if (eat(",")) {
                        if (eat("}"))
                            max = Infinity;
                        else {
                            max = number();
                            match("}");
                        }
                    }
                    else
                        match("}");
                    if (isFinite(max) && max < min)
                        fail( SyntaxError, "max quant must be at least as large as min" );
                    return [min,max] : [double,double];
                }

            default:
                return null;
            }
        }

        function atom() : Matcher? {
            if (atEnd())
                return null;

            switch (peekCharCode()) {
            case 0x29 /* ")" */:
                skip();
                return null;

            case 0x2E /* "." */:
                advance();
                return new CharacterSet(charset_notlinebreak);

            case 0x28 /* "(" */:
                consumeChar("(");

                if (peekCharCode() == 0x3F /* "?" */) {
                    consumeChar("?");
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
                            let capno : double = parenIndex++;
                            let d : Matcher = disjunction();
                            match(")");
                            for ( let i:double=1 ; i < names.length ; i++ ) {
                                if (names[i] === name)
                                    fail( SyntaxError, "Multiply defined capture name: " + name );
                            }
                            names[capno+1] = name;
                            return new Capturing(d, capno);
                        }

                        if (eat("=")) {
                            let name : string = identifier();
                            match(")");
                            for ( let i:double=1 ; i < names.length ; i++ )
                                if (names[i] === name)
                                    return new Backref(intrinsic::toUint(i));
                            fail( SyntaxError, "Unknown backref name " + name );
                        }

                    default:
                        fail( SyntaxError, "Invalid (? pattern: next char=" + peekChar() );
                    }
                } // peekChar() != "?"

                let capno : double = parenIndex++;
                let d : Matcher = disjunction();
                match(")");
                return new Capturing(d, capno);

            case 0x5B /* "[" */: {
                let m : Matcher = characterClass();
                skip();
                return m;
            }

            case 0x5C /* "\\" */: {
                let m : Matcher = atomEscape();
                skip();
                return m;
            }

            case 0x5E /* "^" */:
            case 0x24 /* "$" */:
            case 0x2A /* "*" */:
            case 0x2B /* "+" */:
            case 0x3F /* "?" */:
            case 0x7B /* "{" */:
            case 0x7C /* "|" */:
                // case 0x7D /* "}" */:
                // case 0x5D /* "]" */:
                skip();
                return null;

            default: {
                let m = new CharacterSet(new CharsetAdhoc(consumeChar()));
                skip();
                return m;
            }
            }
        }

        function atomEscape() : Matcher {
            if (!eat("\\"))
                fail( SyntaxError, "Backslash required here (internal compiler error)" );

            let (t : CharsetMatcher? = characterClassEscape()) {
                if (t !== null)
                    return new CharacterSet(t);
            }

            let (t : string? = characterEscape(false)) {
                if (t !== null)
                    return new CharacterSet(new CharsetAdhoc(t));
            }

            let (t : double? = decimalEscape()) {
                if (t !== null) {
                    largest_backref = Math.max(largest_backref, t);  // Will check validity later
                    return new Backref(t);
                }
            }

            fail( SyntaxError, "Failed to match escape sequence \\" + peekChar() );
        }

        function characterClass() : Matcher {
            return new CharacterSet(characterClassHelper());
        }

        function characterClassHelper() : CharsetMatcher {
            match("[");
            let inverted : boolean = false;
            if (peekChar() == "^") {
                advance();
                inverted = true;
            }
            let ranges : CharsetMatcher = classRanges();
            match("]");
            return inverted ? new CharsetComplement(ranges) : ranges;
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

             NonemptyClassRanges ::= "&&[" CharacterClass "]"
                                   | "&&[" CharacterClass "]" NonemptyClassRanges

        */

        function classRanges() : CharsetMatcher {
            if (lookingAt("]"))
                return new CharsetEmpty;

            return nonemptyClassRanges() cast CharsetMatcher;
        }

        function nonemptyClassRanges(acc: CharsetMixin? = null, also: CharsetMixin? = null) : CharsetMixin {

            function accumulate(acc : CharsetMixin?, x : CharsetMixin) : CharsetMixin {
                return acc === null ? x : new CharsetUnion(acc, x);
            }

            function intersect(cs: CharsetMixin?, x: CharsetMixin) : CharsetMixin {
                return cs === null ? x : new CharsetIntersection(cs, x);
            }

            if (lookingAt("&&[")) {
                eat("&&");
                let s = characterClassHelper();
                if (lookingAt("]"))
                    return new intersect(acc, intersect(also,s));
                else
                    return nonemptyClassRanges(acc, intersect(also,s));
            }

            let a1 : CharsetMixin = classAtom();

            if (lookingAt("]"))
                return intersect(also,accumulate(acc,a1));

            if (lookingAt("-")) {
                consumeChar();
                if (lookingAt("]"))
                    return intersect(also,accumulate(acc, new CharsetAdhoc("-")));

                if (a1.hasOneCharacter()) {
                    let a2 : CharsetMixin = classAtom();
                    if (a2.hasOneCharacter()) {
                        let a3 : CharsetMixin = accumulate(acc,
                                                           new CharsetRange(a1.singleCharacter(),
                                                                            a2.singleCharacter()));
                        if (lookingAt("]"))
                            return intersect(also,a3);

                        return nonemptyClassRanges(a3, also);
                    }
                    else
                        return nonemptyClassRanges(accumulate(accumulate(accumulate(acc,a1),
                                                                         new CharsetAdhoc("-")),
                                                              a2),
                                                   also);
                }
                else
                    return nonemptyClassRanges(accumulate(accumulate(acc,a1),
                                                          new CharsetAdhoc("-")),
                                               also);
            }

            return nonemptyClassRanges(accumulate(acc,a1), also);
        }

        function classAtom() : CharsetMatcher {
            if (lookingAt("]") || atEnd())
                fail( SyntaxError, "Premature end of input" );

            if (lookingAt("\\"))
                return classEscape();

            return new CharsetAdhoc(consumeChar());
        }

        function classEscape() : CharsetMatcher {
            if (!eat("\\"))
                fail( SyntaxError, "Backslash required here (internal compiler error)" );

            let (t : CharsetMatcher? = characterClassEscape()) {
                if (t !== null)
                    return t;
            }

            let (t : string? = characterEscape(true)) {
                if (t !== null)
                    return new CharsetAdhoc(t);
            }

            let (t : double? = decimalEscape()) {
                if (t !== null)
                    return new CharsetAdhoc(string.intrinsic::fromCharCode(t));
            }

            fail( SyntaxError, "Failed to match escape sequence " + peekChar() );
        }

        /* Returns null if it does not consume anything but fails;
         * throws an error if it consumes and then fails.
         */
        function characterClassEscape() : CharsetMatcher? {

            function unicodeSet(invert : boolean) : CharsetMatcher {
                let name : string = identifier();
                match("}");
                let cls : CharsetMatcher? = unicodeClass(name, invert);
                if (cls === null)
                    fail( ReferenceError, "Unsupported unicode character class " + name );
                return cls;
            }

            let invert : boolean = true;

            switch (peekCharCode()) {

            case 0x64 /* "d" */: advance(); return charset_digit;
            case 0x44 /* "D" */: advance(); return charset_notdigit;
            case 0x73 /* "s" */: advance(); return charset_space;
            case 0x53 /* "S" */: advance(); return charset_notspace;
            case 0x77 /* "w" */: advance(); return charset_word;
            case 0x57 /* "W" */: advance(); return charset_notword;

            case 0x70 /* "p" */:
                invert = false;
            case 0x50 /* "P" */:
                {
                    if (peekChar2() == "{") {
                        advance();
                        advance();
                        return unicodeSet(invert);
                    }
                }
            }

            return null;
        }

        /* Returns null if it does not consume anything but fails;
         * throws an error if it consumes and then fails.
         *
         * Handles hex escapes.
         */
        function characterEscape(allow_b : boolean) : string? {

            let c : double = peekCharCode();

            switch (c) {
            case 0x30:
            case 0x31:
            case 0x32:
            case 0x33:
            case 0x34:
            case 0x35:
            case 0x36:
            case 0x37:
            case 0x38:
            case 0x39:
                return null;

            case 0x62 /* "b" */:
                if (allow_b) {
                    advance();
                    return "\\b";
                }
                break;

            case 0x66 /* "f" */: advance(); return "\f";
            case 0x6E /* "n" */: advance(); return "\n";
            case 0x72 /* "r" */: advance(); return "\r";
            case 0x74 /* "t" */: advance(); return "\t";

            case 0x63 /* "c" */:
                consumeChar();
                let (c : string = peekChar()) {
                    if (c >= "A" && c <= "Z") {
                        eat(c);
                        return string.intrinsic::fromCharCode(c.charCodeAt(0) - "A".charCodeAt(0) + 1);
                    }
                    if (c >= "a" && c <= "z") {
                        eat(c);
                        return string.intrinsic::fromCharCode(c.charCodeAt(0) - "a".charCodeAt(0) + 1);
                    }
                    else
                        return "c";
                }

            case 0x78 /* "x" */:
            case 0x58 /* "X" */:
            case 0x75 /* "u" */:
            case 0x55 /* "U" */:
                consumeChar();
                if (peekCharCode() == 0x7B /* "{" */) {
                    advance();
                    let s = hexDigits();
                    if (s == null)
                        fail( SyntaxError, "Non-empty sequence of hexadecimal digits expected" );
                    match("}");
                    return s;
                }
                else {
                    let saved = idx;
                    if (c == 0x78 /* "x" */ || c == 0x58 /* "X" */)
                        res = hexDigits(2);
                    else
                        res = hexDigits(4);
                    if (res === null) {
                        idx = saved;
                        res = string.intrinsic::fromCharCode(c);
                    }
                    return res;
                }
            }

            if (atEnd())
                fail( SyntaxError, "EOF inside escape sequence" );

            let (c = consumeChar()) {
                return c;
            }
        }

        /* Returns null if it does not consume anything but fails;
         * throws an error if it consumes and then fails.
         */
        function decimalEscape() : double? {
            let c : double = peekCharCode();
            if (c == 0x30)
                return null;
            if (c >= 0x31 && c <= 0x39)
                return decimalDigits();
            else
                return null;
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
            let i : double = 0;
            let j : double = idx;
            let ilim = i + c.length;
            let jlim = j + c.length;
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

        function number() {
            if (eat("0"))
                return 0;
            else
                return decimalDigits();
        }

        function hexDigits(n : double? = null) : string {
            let k : double = 0;
            let c : string;
            let m : double = n === null ? 100000 : n;
            let i : double;
            for ( i=0 ; i < m ; i++ ) {
                let (c = peekChar()) {
                    if (!isHexDigit(c))
                        break;
                    k = k*16 + hexValue(consumeChar(c));
                }
            }
            if (n !== null && i < m || i == 0)
                return null;
            skip();
            return string.intrinsic::fromCharCode(k);
        }

        function decimalDigits() : double {
            let k : double = 0;
            let c : string;
            while (isDecimalDigit(c = peekChar()))
                k = k*10 + decimalValue(consumeChar(c));
            skip();
            return k;
        }

        function atEnd()
            idx >= slen;

        function peekChar() : string
            idx < slen ? source[idx] : "*END*";

        function peekChar2() : string
            idx+1 < slen ? source[idx+1] : "*END*";

        function peekCharCode() : double {
            // In a production implementation, this would probably be
            // no faster than peekChar. In our reference
            // implementation, it is substantially faster.
            if (idx < slen)
                return informative::charCodeAt(source, intrinsic::toUint(idx));
            else
                return 0x0;
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
                let c : double = peekCharCode();
                if (c == 0x23 /* '#' */) {
                    ++idx;
                    while (!atEnd() && !isTerminatorCode(peekCharCode()))
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
