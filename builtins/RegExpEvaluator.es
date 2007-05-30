/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 *
 * Representation of compiled code, plus evaluation.
 *
 * Status: Complete, not reviewed, not tested.
 */

package RegExpInternals
{
    import Unicode.*;
    use namespace intrinsic;
    use strict;

    /* Encapsulation of compiled regular expression as returned by the
       compiler.  
    */
    intrinsic class RegExpMatcher!
    {
        public function RegExpMatcher(matcher : Matcher, nCapturingParens : uint, names : [string?]!) 
            : matcher = matcher
            , nCapturingParens = nCapturingParens
            , names = names
        {
        }

        /* Returns an array of matches, with additional named properties
           on the array for named submatches 
        */
        public function match( input : string, endIndex : int, multiline: boolean, ignoreCase: boolean ) : MatchResult {
            return matcher.match(new Context(input, multiline, ignoreCase),
                                 new State(endIndex, makeCapArray(nCapturingParens+1)), 
                                 function (ctx : Context, x : State) : State? { return x } );
        }

        public var nCapturingParens : uint;
        var matcher : Matcher;
        var names : [string?]!;
    }

    /* The Context contains static data for the matching, we use this
       instead of package-global variables in order to make the
       matcher reentrant.
    */
    class Context!
    {
        function Context(input : string, multiline: boolean, ignoreCase: boolean) 
            : input = input
            , inputLength = input.length
            , ignoreCase = ignoreCase
            , multiline = multiline
        {
        }

        var input       : string;            // FIXME: const (all four).  Ticket #24.
        var inputLength : int;
        var ignoreCase  : boolean;   // i
        var multiline   : boolean;   // m
    }

    /* MatchResult and State. 
     */

    public type MatchResult = State?;
    public const failure : State? = null;
    
    class State!
    {
        function State(endIndex : int, cap : CapArray) 
            : endIndex = endIndex
            , cap = cap
        {
        }

        public var endIndex : int;
        public var cap : CapArray;
    }

    /* Captures array.

       This captures array can be an array that's copied like the
       E262-3 states, or it could be a functional data structure.  
    */
    public type CapArray = [(string,undefined)];

    function makeCapArray(len: uint) : CapArray {
        let a = [] : CapArray;
        for ( let i : uint = 0 ; i < len ; i++ )
            a[i] = undefined;
        return a;
    }

    function copyCapArray(a : CapArray, parenIndex : uint, parenCount : uint) : CapArray {
        let b : CapArray = makeCapArray(a.length);
        for ( let i : uint = 0 ; i < a.length ; i++ )
            b[i] = a[i];
        for ( let k : uint = parenIndex ; k < parenIndex+parenCount ; k++ )
            b[k] = undefined;
        return b;
    }


    /* The matcher is a single object that implements the Matcher
       interface.  Normally a Matcher object references other Matcher
       objects.  */
    // FIXME: interfaces are completely missing.  Ticket #46.
    /*public interface Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult;
    }*/
    type Matcher = *;

    type Continuation = function(Context, State) : MatchResult;

    class Empty! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            return c(ctx, x);
        }
    }

    class Disjunct! implements Matcher
    {
        function Disjunct(m1 : Matcher, m2 : Matcher) : m1=m1, m2=m2 {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m1.match(ctx, x, c);
            if (r !== failure)
                return r;
            return m2.match(ctx, x, c);
        }

        var m1 : Matcher, m2 : Matcher; // FIXME: const.  Ticket #24.
    }

    class Alternative! implements Matcher
    {
        function Alternative(m1 : Matcher, m2 : Matcher) : m1=m1, m2=m2 {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            return m1.match(ctx, x, function (ctx : Context, y : State) { return m2.match(ctx, y, c) } );
        }

        var m1 : Matcher, m2 : Matcher; // FIXME: const.  Ticket #24.
    }

    class Assertion! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            if (!testAssertion(ctx, x))
                return failure;
            return c(ctx, x);
        }

        function testAssertion(ctx : Context, x : State) : boolean { return false; }
    }

    class AssertStartOfInput extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : boolean {
            let e : int = x.endIndex;
            if (e === 0)
                return true;
            if (ctx.multiline)
                return isTerminator(ctx.input[e-1]);
            return false;
        }
    }

    class AssertEndOfInput extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : boolean {
            let e : int = x.endIndex;
            if (e === ctx.inputLength)
                return true;
            if (ctx.multiline)
                return isTerminator(ctx.input[e-1]);
            return false;
        }
    }

    class AssertWordBoundary extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : boolean {
            let e : int = x.endIndex;
            return isREWordChar(ctx, e-1) !== isREWordChar(ctx, e);
        }
    }

    class AssertNotWordBoundary extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : boolean {
            let e : int = x.endIndex;
            return isREWordChar(ctx, e-1) === isREWordChar(ctx, e);
        }
    }

    function isREWordChar(ctx : Context, e : int) : boolean {
        if (e === -1 || e === ctx.inputLength)
            return false;
        let c = ctx.input[e];
        return isWordChar(ctx.input[e]);
    }

    class Quantified! implements Matcher
    {
        function Quantified(parenIndex:uint, parenCount:uint, m:Matcher, min:double, max:double, 
                            greedy:boolean) 
            : parenIndex = parenIndex
            , parenCount = parenCount
            , m = m
            , min = min
            , max = max
            , greedy = greedy
        {
        }

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {

            function RepeatMatcher(min : double, max : double, x : State) : MatchResult {
                function d(ctx: Context, y : State) : MatchResult {
                    if (min === 0 && y.endIndex === x.endIndex)
                        return failure;
                    else
                        return RepeatMatcher(Math.max(0, min-1), max-1, y);
                }

                if (max === 0)
                    return c(ctx, x);

                let xr = new State(x.endIndex, copyCapArray(x.cap, parenIndex, parenCount));

                if (min !== 0)
                    return m.match(ctx, xr, d);

                if (!greedy) {
                    let z : MatchResult = c(ctx, x);
                    if (z !== failure)
                        return z;
                    return m.match(ctx, xr, d);
                }
                else {
                    let z : MatchResult = m.match(ctx, xr, d);
                    if (z !== failure)
                        return z;
                    return c(ctx, x);
                }
            }

            return RepeatMatcher(min, max, x);
        }

        var parenIndex : uint;  // FIXME: const (all of these).  Ticket #24.
        var parenCount : uint;
        var m : Matcher;
        var min : double;
        var max : double;
        var greedy : boolean;
    }

    class Capturing! implements Matcher
    {
        function Capturing(m : Matcher, parenIndex : uint) : m=m, parenIndex=parenIndex {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {

            let function d( ctx: Context, y : State ) : MatchResult {
                let cap : CapArray = copyCapArray( y.cap, 0, 0 );
                let xe : int = x.endIndex;
                let ye : int = y.endIndex;
                cap[parenIndex+1] = ctx.input.substring(xe, ye);
                return c(ctx, new State(ye, cap));
            }

            return m.match(ctx, x, d);
        }

        var m : Matcher, parenIndex : uint; // FIXME: const.  Ticket #24.
    }

    class Backref! implements Matcher
    {
        function Backref(capno : uint) : capno=capno {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let cap = x.cap;
            let s = cap[capno];
            if (s == null)
                return c(ctx, x);
            let e = x.endIndex;
            let len = s.length;
            let f = e+len;
            if (f > ctx.inputLength)
                return failure;
            for ( let i=0 ; i < len ; i++ )
                if (Canonicalize(ctx, s[i]) !== Canonicalize(ctx, ctx.input[e+i]))
                    return failure;
            return c(ctx, new State(f, cap));
        }

        var capno : uint; // FIXME: const.  Ticket #24.
    }

    class PositiveLookahead! implements Matcher
    {
        function PositiveLookahead(m : Matcher) : m=m {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m.match(ctx, x, function (ctx, y : State) : MatchResult { return y } );
            if (r === failure)
                return failure;
            return c(ctx, new State(x.endIndex, r.cap));
        }

        var m: Matcher; // FIXME: const.  Ticket #24.
    }

    class NegativeLookahead! implements Matcher
    {
        function NegativeLookahead(m : Matcher) : m=m {}
            
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m.match(ctx, x, function (ctx, y : State) : MatchResult { return y } );
            if (r !== failure)
                return failure;
            return c(ctx, x);
        }

        var m: Matcher; // FIXME: const.  Ticket #24.
    }

    class CharsetMatcher! implements Matcher
    {
        function CharsetMatcher(cs : Charset) : cs=cs {}

        function match(ctx : Context, x : State, c : Continuation) /* : MatchResult */ {
            let e = x.endIndex;
            let cap = x.cap;
            if (e === ctx.inputLength)
                return failure;
            let ch = ctx.input[e];
            let cc = Canonicalize(ctx, ch);
            let res = cs.match(ctx, cc);
            if (!res)
                return failure;
            return c(ctx, new State(e+1, cap));
        }

        var cs : Charset;
    }

    function Canonicalize(ctx, ch) {
        if (!ctx.ignoreCase)
            return ch;
        let u = ch.toUpperCase();
        if (u.length != 1)
            return ch;
        if (ch.charCodeAt(0) >= 128 && u.charCodeAt(0) < 128)
            return ch;
        return u;
    }


    /*** Character sets ***/

    class Charset!
    {
        function match(ctx: Context, c : string) : boolean { throw "Abstract"; }
        function hasOneCharacter() : boolean { return false }
        function singleCharacter() : string { return " " };
    }

    class CharsetEmpty extends Charset
    {
        override function match(ctx: Context, c: string) : boolean {
            return false;
        }
    }

    class CharsetUnion! extends Charset 
    {
        function CharsetUnion(m1: Charset, m2 : Charset) : m1=m1, m2=m2 {}

        override function match(ctx: Context, c : string) : boolean {
            return m1.match(ctx, c) || m2.match(ctx, c);
        }

        var m1 : Charset, m2 : Charset; // FIXME: const.  Ticket #24.
    }

    class CharsetIntersection! extends Charset 
    {
        function CharsetIntersection(m1 : Charset, m2 : Charset) : m1=m1, m2=m2 {}

        override function match(ctx: Context, c : string) : boolean {
            return m1.match(ctx, c) && m2.match(ctx, c);
        }

        var m1 : Charset, m2 : Charset; // FIXME: const.  Ticket #24.
    }

    class CharsetComplement! extends Charset
    {
        function CharsetComplement(m : Charset) : m=m { }

        override function match(ctx: Context, c : string) : boolean {
            return !m.match(ctx, c);
        }

        var m : Charset; // FIXME: const.  Ticket #24.
    }

    class CharsetRange! extends Charset 
    {
        function CharsetRange(lo : string, hi : string) : lo=lo, hi=hi { }

        override function match(ctx: Context, c : string) : boolean {
            let lo_code = lo.charCodeAt(0);
            let hi_code = hi.charCodeAt(0);
            for ( let i=lo_code ; i <= hi_code ; i++ )
                if (Canonicalize(ctx, string.fromCharCode(i)) === c)
                    return true;
            return false;
        }

        var lo : string, hi : string; // FIXME: const.  Ticket #24.
    }

    class CharsetAdhoc! extends Charset 
    {
        function CharsetAdhoc(s : string) {
            cs = explodeString(s);
        }

        override function match(ctx: Context, c : string) : boolean {
            for ( let i=0 ; i < cs.length ; i++ ) {
                if (Canonicalize(ctx, cs[i]) === c)
                    return true;
            }
            return false;
        }

        override function hasOneCharacter() : boolean {
            return cs.length == 1;
        }

        override function singleCharacter() : string {
            return cs[0];
        }

        var cs : [string] = [] : [string];
    }

    class CharsetUnicodeClass! extends Charset
    {
        function CharsetUnicodeClass(name : string) : name=name {}

        override function match(ctx: Context, c : string) : boolean {
            // FIXME: implement this.  Ticket #45.
            throw new Error("character set not yet implemented: " + name);
        }

        var name : string; // FIXME: const.  Ticket #24.
    }

    const charset_linebreak : Charset = new CharsetAdhoc(Unicode.linebreaks);
    const charset_notlinebreak : Charset = new CharsetComplement(charset_linebreak);

    const charset_space : Charset = new CharsetAdhoc(Unicode.blanks + Unicode.linebreaks);
    const charset_notspace : Charset = new CharsetComplement(charset_space);

    const charset_digit : Charset = new CharsetAdhoc(Unicode.decimal_digits);
    const charset_notdigit : Charset = new CharsetComplement(charset_digit);

    const charset_word : Charset = new CharsetAdhoc(Unicode.alphanumerics);
    const charset_notword : Charset = new CharsetComplement(charset_word);

    const unicode_named_classes = {
        "L":  new CharsetUnicodeClass("Letter"),
        "Lu": new CharsetUnicodeClass("Letter, Uppercase"),
        "Ll": new CharsetUnicodeClass("Letter, Lowercase"),
        "Lt": new CharsetUnicodeClass("Letter, Titlecase"),
        "Lm": new CharsetUnicodeClass("Letter, Modifier"),
        "Lo": new CharsetUnicodeClass("Letter, Other"),
        "M":  new CharsetUnicodeClass("Mark"),
        "Mn": new CharsetUnicodeClass("Mark, Nonspacing"),
        "Mc": new CharsetUnicodeClass("Mark, Spacing Combining"),
        "Me": new CharsetUnicodeClass("Mark, Enclosing"),
        "N":  new CharsetUnicodeClass("Number"),
        "Nd": new CharsetUnicodeClass("Number, Decimal Digit"),
        "Nl": new CharsetUnicodeClass("Number, Letter"),
        "No": new CharsetUnicodeClass("Number, Other"),
        "P":  new CharsetUnicodeClass("Punctuation"),
        "Pc": new CharsetUnicodeClass("Punctuation, Connector"),
        "Pd": new CharsetUnicodeClass("Punctuation, Dash"),
        "Ps": new CharsetUnicodeClass("Punctuation, Open"),
        "Pe": new CharsetUnicodeClass("Punctuation, Close"),
        "Pi": new CharsetUnicodeClass("Punctuation, Initial quote (may behave like Ps or Pe depending on usage)"),
        "Pf": new CharsetUnicodeClass("Punctuation, Final quote (may behave like Ps or Pe depending on usage)"),
        "Po": new CharsetUnicodeClass("Punctuation, Other"),
        "S":  new CharsetUnicodeClass("Symbol"),
        "Sm": new CharsetUnicodeClass("Symbol, Math"),
        "Sc": new CharsetUnicodeClass("Symbol, Currency"),
        "Sk": new CharsetUnicodeClass("Symbol, Modifier"),
        "So": new CharsetUnicodeClass("Symbol, Other"),
        "Z":  new CharsetUnicodeClass("Separator"),
        "Zs": new CharsetUnicodeClass("Separator, Space"),
        "Zl": new CharsetUnicodeClass("Separator, Line"),
        "Zp": new CharsetUnicodeClass("Separator, Paragraph"),
        "C":  new CharsetUnicodeClass("Other"),
        "Cc": new CharsetUnicodeClass("Other, Control"),
        "Cf": new CharsetUnicodeClass("Other, Format"),
        "Cs": new CharsetUnicodeClass("Other, Surrogate"),
        "Co": new CharsetUnicodeClass("Other, Use"),
        "Cn": new CharsetUnicodeClass("Other, Not Assigned (no characters in the file have this property)") 
    };

    function unicodeClass(name : string, complement : boolean) : Charset? {
        let c = unicode_named_classes[name];
        if (!c)
            return null;
        return complement ? new CharsetComplement(c) : c;
    }
}
