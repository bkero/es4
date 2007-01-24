/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 *
 * Representation of compiled code, plus evaluation.
 *
 * Status: Complete, not reviewed, not tested.
 */

package RegExp
{
    import Unicode.*;
    use namespace intrinsic;
    use strict;

    /* Encapsulation of compiled regular expression as returned by the
       compiler.  
    */
    class RegExpMatcher!
    {
        public function RegExpMatcher(matcher : Matcher, nCapturingParens : int, names : [String]!) 
            : matcher = matcher
            , nCapturingParens = nCapturingParens
            , names = names
            
        {
        }

        /* Returns an array of matches, with additional named properties
           on the array for named submatches 
        */
        public function match( input : String!, endIndex : int ) : MatchResult {
            return matcher.match(new Context(input, flags),
                                 new State(endIndex, makeCapArray(nCapturingParens)), 
                                 function (x : State) : State? { return x });
        }

        var matcher : Matcher;
        var nCapturingParens : int;
        var names : [String]!;
    }

    /* The Context contains static data for the matching, we use this
       instead of package-global variables in order to make the
       matcher reentrant.
    */
    class Context!
    {
        function Context(input : String!, flags : String!) 
            : input = input
            , inputLength = input.length
            , ignoreCase = flags.indexOf("i") != -1
            , multiline = flags.indexOf("m") != -1
        {
        }

        const input       : String!;
        const inputLength : int;
        const ignoreCase  : Boolean;   // i
        const multiline   : Boolean;   // m
    }

    /* MatchResult and State. 
     */
    type MatchResult = State?;

    const failure : State? = null;

    class State!
    {
        function State(endIndex : int, cap : CapArray) 
            : endIndex = endIndex
            , cap = cap
        {
        }

        var endIndex : int;
        var cap : CapArray;
    }

    /* Captures array.

       This captures array can be an array that's copied like the
       E262-3 states, or it could be a functional data structure.  
    */
    type CapArray = Array!;  /* Really [(String!,Undefined)]! but we can't express that yet */

    function makeCapArray(nCapturingParens : uint) : CapArray {
        var a = [] : CapArray;
        for ( let i : uint = 0 ; i < nCapturingParens ; i++ )
            a[i] = undefined;
        return a;
    }

    function copyCapArray(a : CapArray, parenIndex : uint, parenCount : uint) : CapArray {
        let b : CapArray = makeCapArray(a.length);
        for ( let i : uint = 0 ; i < a.length ; i++ )
            b[i] = a[i];
        for ( let k : uint = parenIndex ; k < parenIndex+parenCount ; k++ )
            b[i] = undefined;
        return b;
    }


    /* The matcher is a single object that implements the Matcher
       interface.  Normally a Matcher object references other Matcher
       objects.  */
    interface Matcher!
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult;
    }

    type Continuation = function(ctx : Context, x : State) : MatchResult;

    class Empty! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult
            c(x);
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

        var m1 : Matcher, m2 : Matcher;
    }

    class Alternative! implements Matcher
    {
        function Alternative(m1 : Matcher, m2 : Matcher) : m1=m1, m2=m2 {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult
            m1.match(ctx, x, (function (ctx : Context, y : State) m2.match(ctx, y, c)) );

        var m1 : Matcher, m2 : Matcher;
    }

    class Assertion! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            if (!testAssertion(ctx, x))
                return failure;
            return c(ctx, x);
        }

        function testAssertion(ctx : Context, x : State) : Boolean { return false; }
    }

    class AssertStartOfInput extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : Boolean {
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
        override function testAssertion(ctx : Context, x : State) : Boolean {
            let e : int = x.endIndex;
            if (e === ctx.inputLength)
                return true;
            if (ctx.multiline)
                return isTerminator(ctx.input[e-1]);
            return false;
        }
    }

    class AssertWordboundary extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : Boolean
            let (e : int = x.endIndex)
                isREWordChar(ctx, e-1) !== isREWordChar(ctx, e);
    }

    class AssertNotWordboundary extends Assertion
    {
        override function testAssertion(ctx : Context, x : State) : Boolean 
            let (e : int = x.endIndex)
                isREWordChar(ctx, e-1) === isREWordChar(ctx, e);
    }

    function isREWordChar(ctx : Context, e : int) : Boolean {
        if (e === -1 || e === ctx.inputLength)
            return false;
        let c = ctx.input[e];
        return isWordChar(ctx.input[e]);
    }

    class Quantified! implements Matcher
    {
        function Quantified(parenIndex:uint, parenCount:uint, m:Matcher, min:Number, max:Number, 
                            greedy:Boolean) 
            : parenIndex = parenIndex
            , parenCount = parenCount
            , m = m
            , min = min
            , max = max
            , greedy = greedy
        {
        }

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {

            function RepeatMatcher(min : Number, max : Number, x : State) : MatchResult {
                if (max === 0)
                    return c(x);

                let function d(y : State) : MatchResult {
                    if (min === 0 && y.endIndex === x.endIndex)
                        return failure;
                    else
                        return RepeatMatcher(Math.max(0, min-1), max-1, y);
                }
                let xr = new State(x.endIndex, cloneCaptures(x.cap, parenIndex, parenCount));

                if (min !== 0)
                    return m.match(xr, d);

                if (!greedy) {
                    let z : MatchResult = c(x);
                    if (z !== failure)
                        return z;
                    return m.match(xr, d);
                }
                else {
                    let z : MatchResult = m.match(xr, d);
                    if (z !== failure)
                        return z;
                    return c(x);
                }
            }

            return RepeatMatcher(min, max, x);
        }

        var parenIndex : uint;
        var parenCount : uint;
        var m : Matcher;
        var min : Number;
        var max : Number;
        var greedy : Boolean;
    }

    class Capturing! implements Matcher
    {
        function Capturing(m : Matcher, parenIndex : uint) : m=m, parenIndex=parenIndex {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {

            let function d( y : State ) : MatchResult {
                let cap : CapArray = copyCapArray( y.cap, 0, 0 );
                let xe : int = x.endIndex;
                let ye : int = y.endIndex;
                cap[parenIndex] = ctx.input.substring(xe, ye);
                return c(new State(ye, cap));
            }

            return m.match(x, d);
        }

        var m : Matcher, parenIndex : uint;
    }

    class Backref! implements Matcher
    {
        function Backref(capno : uint) : capno=capno {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let cap = x.cap;
            let s = cap[capno];
            if (s == null)
                return c(x);
            let e = x.endIndex;
            let len = s.length;
            let f = e+len;
            if (f > ctx.inputLength)
                return failure;
            for ( let i=0 ; i < len ; i++ )
                if (Canonicalize(ctx, s[i]) !== Canonicalize(ctx, ctx.input[e+i]))
                    return failure;
            return c(new State(f, cap));
        }

        var capno : uint;
    }

    class PositiveLookahead! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m(ctx, x, function (y : State) : MatchResult { return y });
            if (r === failure)
                return failure;
            return c(new State(x.endIndex, r.cap));
        }
    }

    class NegativeLookahead! implements Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m(ctx, x, function (y : State) : MatchResult { return y });
            if (r !== failure)
                return failure;
            return c(x);
        }
    }

    class CharsetMatcher! implements Matcher
    {
        function CharsetMatcher(cs : Charset) : cs=cs {}

        function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let e = x.endIndex;
            if (e === ctx.inputLength)
                return failure;
            let c = ctx.input[e];
            let cc = Canonicalize(c);
            let res = cs.match(cc);
            if (res === failure)
                return failure;
            return c(new State(e+1, x.cap));
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

    interface Charset!
    {
        function match(c : String!) : Boolean;
    }

    class CharsetUnion implements Charset 
    {
        function CharsetUnion(m1 : Charset, m2 : Charset) : m1=m1, m2=m2 {}

        function match(c : String!) : Boolean
            m1.match(c, true) || m2.match(c, true);

        var m1 : Charset, m2 : Charset;
    }

    class CharsetIntersection implements Charset 
    {
        function CharsetIntersection(m1 : Charset, m2 : Charset) : m1=m1, m2=m2 {}

        function match(c : String!) : Boolean
            m1.match(c, true) && m2.match(c, true);

        var m1 : Charset, m2 : Charset;
    }

    class CharsetComplement implements Charset 
    {
        function CharsetComplement(cs : Charset) : cs=cs {}

        function match(c : String!) : Boolean
            m.match(c) === failure;

        var cs : Charset;
    }

    class CharsetRange implements Charset 
    {
        function CharsetRange(lo : String!, hi : String!) : lo=lo, hi=hi {}

        function match(c : String!) : Boolean {
            let lo_code = lo.charCodeAt(0);
            let hi_code = hi.charCodeAt(0);
            for ( let i=lo_code ; i <= hi_code ; i++ )
                if (Canonicalize(String.fromCharCode(i)) === c)
                    return true;
            return false;
        }

        var lo : String!, hi : String!;
    }

    class CharsetAdhoc implements Charset 
    {
        function CharsetAdhoc(cs : [String!]) : cs=cs {}

        function match(c : String!) : Boolean {
            for each ( let d in cs ) {
                if (Canonicalize(d) === c)
                    return true;
            }
            return false;
        }

        var cs : [String!];
    }

    class CharsetUnicodeClass implements Charset
    {
        function CharsetUnicodeClass(name : String!) : name=name {}

        function match(c : String!) : Boolean {
            throw new Error("character set not yet implemented: " + name);
        }

        var name : String!;
    }

    const charset_linebreak : Charset = new CharsetAdhoc("\u000A\u000D\u0085\u2028\u2029".split(""));
    const charset_notlinebreak : Charset = new CharsetComplement(charset_linebreak);

    const charset_space : Charset = 
        new CharsetAdhoc(("\u0009\u000B\u000C\u0020\u00A0\u1680\u180E\u2000\u2001\u2002" +
                          "\u203\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F" +
                          "\u3000\u000A\u000D\u0085\u2028\u2029").split(""));
    const charset_notspace : Charset = new CharsetComplement(charset_space);

    const charset_digits : Charset = new CharsetAdhoc("0123456789".split(""));
    const charset_notdigits : Charset = new CharsetComplement(charset_digits);

    const charset_word : Charset = 
        new CharsetAdhoc("abcdefghijklmnopqrstuvwzyzABCDEFGHIJKLMNOPQRSTUVWZYZ0123456789_".split(""));
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

    function unicodeClass(name : String!, complement : Boolean) : Charset? {
        let c = unicode_named_classes[name];
        if (!c)
            return null;
        return complement ? new CharsetComplement(c) : c;
    }
}
