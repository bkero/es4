/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
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
        public function RegExpMatcher(matcher : Matcher, nCapturingParens : uint)
            : matcher = matcher
            , nCapturingParens = nCapturingParens
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

        const input       : string;
        const inputLength : int;
        const ignoreCase  : boolean;   // i
        const multiline   : boolean;   // m
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
        // E262-3 says k >= parenIndex here, but that appears clearly to be wrong.
        for ( let k : uint = parenIndex+1 ; k <= parenIndex+parenCount ; k++ )
            b[k] = undefined;
        return b;
    }


    /* The matcher is a single object that implements the Matcher
       interface.  Normally a Matcher object references other Matcher
       objects.  */
    public interface Matcher
    {
        function match(ctx : Context, x : State, c : Continuation) : MatchResult;
    }

    // type Matcher = *;

    type Continuation = function(Context, State) : MatchResult;

    class Empty! implements Matcher
    {
        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            return c(ctx, x);
        }
    }

    class Disjunct! implements Matcher
    {
        function Disjunct(m1 : Matcher, m2 : Matcher) : m1=m1, m2=m2 {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m1.match(ctx, x, c);
            if (r !== failure)
                return r;
            return m2.match(ctx, x, c);
        }

        const m1 : Matcher, m2 : Matcher;
    }

    class Alternative! implements Matcher
    {
        function Alternative(m1 : Matcher, m2 : Matcher) : m1=m1, m2=m2 {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            return m1.match(ctx, x, function (ctx : Context, y : State) { return m2.match(ctx, y, c) } );
        }

        const m1 : Matcher, m2 : Matcher;
    }

    class Assertion! implements Matcher
    {
        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
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
                return isTerminator(ctx.input[e]);
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

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {

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

        const parenIndex : uint;
        const parenCount : uint;
        const m : Matcher;
        const min : double;
        const max : double;
        const greedy : boolean;
    }

    class Capturing! implements Matcher
    {
        function Capturing(m : Matcher, parenIndex : uint) : m=m, parenIndex=parenIndex {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {

            let function d( ctx: Context, y : State ) : MatchResult {
                let cap : CapArray = copyCapArray( y.cap, 0, 0 );
                let xe : int = x.endIndex;
                let ye : int = y.endIndex;
                cap[parenIndex+1] = ctx.input.substring(xe, ye);
                return c(ctx, new State(ye, cap));
            }

            return m.match(ctx, x, d);
        }

        const m : Matcher, parenIndex : uint;
    }

    class Backref! implements Matcher
    {
        function Backref(capno : uint) : capno=capno {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
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

        const capno : uint;
    }

    class PositiveLookahead! implements Matcher
    {
        function PositiveLookahead(m : Matcher) : m=m {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m.match(ctx, x, function (ctx, y : State) : MatchResult { return y } );
            if (r === failure)
                return failure;
            return c(ctx, new State(x.endIndex, r.cap));
        }

        const m: Matcher;
    }

    class NegativeLookahead! implements Matcher
    {
        function NegativeLookahead(m : Matcher) : m=m {}

        public function match(ctx : Context, x : State, c : Continuation) : MatchResult {
            let r : MatchResult = m.match(ctx, x, function (ctx, y : State) : MatchResult { return y } );
            if (r !== failure)
                return failure;
            return c(ctx, x);
        }

        const m: Matcher;
    }

    class CharsetMatcher! implements Matcher
    {
        function CharsetMatcher(cs : Charset) : cs=cs {}

        public function match(ctx : Context, x : State, c : Continuation) /* : MatchResult */ {
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

        const cs : Charset;
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

        const m1 : Charset, m2 : Charset;
    }

    class CharsetIntersection! extends Charset
    {
        function CharsetIntersection(m1 : Charset, m2 : Charset) : m1=m1, m2=m2 {}

        override function match(ctx: Context, c : string) : boolean {
            return m1.match(ctx, c) && m2.match(ctx, c);
        }

        const m1 : Charset, m2 : Charset;
    }

    class CharsetComplement! extends Charset
    {
        function CharsetComplement(m : Charset) : m=m { }

        override function match(ctx: Context, c : string) : boolean {
            return !m.match(ctx, c);
        }

        const m : Charset;
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

        const lo : string, hi : string;
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

        var cs : [string] = [] : [string]; // FIXME: const.  Ticket #24.
    }

    class CharsetUnicodeClass! extends Charset
    {
        function CharsetUnicodeClass(tester: function(uint): boolean) : tester=tester {}

        override function match(ctx: Context, c : string) : boolean
            tester(c.charCodeAt(0));

        const tester: function(uint): boolean;
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
        "L":  new CharsetUnicodeClass(Unicode.isUnicodeL),
        "Lu": new CharsetUnicodeClass(Unicode.isUnicodeLu),
        "Ll": new CharsetUnicodeClass(Unicode.isUnicodeLl),
        "Lt": new CharsetUnicodeClass(Unicode.isUnicodeLt),
        "Lm": new CharsetUnicodeClass(Unicode.isUnicodeLm),
        "Lo": new CharsetUnicodeClass(Unicode.isUnicodeLo),
        "M":  new CharsetUnicodeClass(Unicode.isUnicodeM),
        "Mn": new CharsetUnicodeClass(Unicode.isUnicodeMn),
        "Mc": new CharsetUnicodeClass(Unicode.isUnicodeMc),
        "Me": new CharsetUnicodeClass(Unicode.isUnicodeMe),
        "N":  new CharsetUnicodeClass(Unicode.isUnicodeN),
        "Nd": new CharsetUnicodeClass(Unicode.isUnicodeNd),
        "Nl": new CharsetUnicodeClass(Unicode.isUnicodeNl),
        "No": new CharsetUnicodeClass(Unicode.isUnicodeNo),
        "P":  new CharsetUnicodeClass(Unicode.isUnicodeP),
        "Pc": new CharsetUnicodeClass(Unicode.isUnicodePc),
        "Pd": new CharsetUnicodeClass(Unicode.isUnicodePd),
        "Ps": new CharsetUnicodeClass(Unicode.isUnicodePs),
        "Pe": new CharsetUnicodeClass(Unicode.isUnicodePe),
        "Pi": new CharsetUnicodeClass(Unicode.isUnicodePi),
        "Pf": new CharsetUnicodeClass(Unicode.isUnicodePf),
        "Po": new CharsetUnicodeClass(Unicode.isUnicodePo),
        "S":  new CharsetUnicodeClass(Unicode.isUnicodeS),
        "Sm": new CharsetUnicodeClass(Unicode.isUnicodeSm),
        "Sc": new CharsetUnicodeClass(Unicode.isUnicodeSc),
        "Sk": new CharsetUnicodeClass(Unicode.isUnicodeSk),
        "So": new CharsetUnicodeClass(Unicode.isUnicodeSo),
        "Z":  new CharsetUnicodeClass(Unicode.isUnicodeZ),
        "Zs": new CharsetUnicodeClass(Unicode.isUnicodeZs),
        "Zl": new CharsetUnicodeClass(Unicode.isUnicodeZl),
        "Zp": new CharsetUnicodeClass(Unicode.isUnicodeZp),
        "C":  new CharsetUnicodeClass(Unicode.isUnicodeC),
        "Cc": new CharsetUnicodeClass(Unicode.isUnicodeCc),
        "Cf": new CharsetUnicodeClass(Unicode.isUnicodeCf),
        "Cs": new CharsetUnicodeClass(Unicode.isUnicodeCs),
        "Co": new CharsetUnicodeClass(Unicode.isUnicodeCo),
        "Cn": new CharsetUnicodeClass(Unicode.isUnicodeCn)
    };

    function unicodeClass(name : string, complement : boolean) : Charset? {
        let c = unicode_named_classes[name];
        if (!c)
            return null;
        return complement ? new CharsetComplement(c) : c;
    }
}
