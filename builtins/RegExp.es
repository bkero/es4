/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
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
 * Status: complete; not reviewed; not tested.
 *
 * See RegExpCompiler.es for the compiler.
 * See RegExpEvaluator.es for the evaluator and compiled code representation.
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use namespace __ES4__;

    import Unicode.*;
    import RegExpInternals.*;
    import ECMAScript4_Internal.*;

    /* E262-3 15.10: Regular expression object */
    public dynamic class RegExp
    {
        static const length = 2;

        /* E262-3 15.10.3.1: The RegExp constructor called as a function */
        meta static function invoke( pattern, flags ) {
            if (pattern is RegExp && flags === undefined)
                return pattern;
            else
                return new RegExp(pattern, flags);
        }

        static function analyzePatternAndFlags (pattern, flags)
        {
            let src : string = "";

            if (pattern is RegExp) {
                if (flags === undefined) {
                    src = pattern.source;
                    flags = pattern.flags;
                }
                else
                    throw new TypeError("Illegal construction of regular expression");
            }
            else {
                src = pattern === undefined ? "" : string(pattern);
                flags = flags === undefined ? "" : string(flags);
            }

            let usedflags = { m: false, i: false, g: false, x: false, y: false };

            for ( let i=0 ; i < flags.length ; i++ ) {
                let f = flags[i];
                if (!(f in usedflags))
                    throw new SyntaxError("Invalid flag: " + f);
                if (usedflags[f])
                    throw new SyntaxError("Duplicated flag: " + f);
                usedflags[f] = true;
            }

            [matcher,names] = (new RegExpCompiler(src, usedflags)).compile();
            return {matcher: matcher,
                    names: names,
                    source: src,
                    multiline: usedflags.m,
                    ignoreCase: usedflags.i,
                    global: usedflags.g,
                    extended: usedflags.e,
                    sticky: usedflags.y}
        }

        /* E262-3 15.10.4.1: The RegExp constructor */
        function RegExp( pattern, flags )
            : { matcher: matcher, 
                names: names, 
                source: source, 
                multiline: multiline, 
                ignoreCase: ignoreCase, 
                global: global, 
                extended: extended, 
                sticky: sticky } = analyzePatternAndFlags(pattern, flags),
            lastIndex = 0
        {
        }

        /* E262-4 proposals:extend_regexps: RegExp instances are
           callable, and a call to an instance is equivalent to
           calling its exec() method.
        */
        meta function invoke(s : string) : Array
            exec(s);

        /* E262-3 15.10.6.2: RegExp.prototype.exec */
        intrinsic function exec(s : string) : Array {
            let length = s.length;
            let i = lastIndex;
            if (!global)
                i = 0;
            let res = failure;
            while (true) {
                if (i < 0 || i > length) {
                    lastIndex = 0;
                    return null;
                }
                res = matcher.match(s, i, multiline, ignoreCase);
                if (res !== failure)
                    break;
                ++i;
            }
            if (global)
                lastIndex = res.endIndex;
            let a = new Array(res.captures.length);
            a.index = i;
            a.input = s;
            a.length = res.captures.length;
            a[0] = s.substring(i,res.endIndex);
            for ( let j=1 ; j < res.captures.length ; j++ )
                a[j] = res.captures[j];
            for ( let j=1 ; j < names.length ; j++ )
                if (names[j] !== null)
                    a[names[j]] = res.captures[j];
            return a;
        }

        prototype function exec(this:RegExp, s)
            this.exec(string(s));

        /* E262-3 15.10.6.3: RegExp.prototype.test */
        intrinsic function test(s : string) : boolean
             exec(s) !== null;

        prototype function test(this:RegExp, s)
            this.test(string(s));

        /* E262-3 15.10.6.4: RegExp.prototype.toString */
        override intrinsic function toString() : string
            "/" + (source.length == 0 ? "(?:)" : source) + "/" + flags;

        prototype function toString(this:RegExp)
            this.intrinsic::toString();

        /* E262-3 15.10.7: properties of regexp instances */
        const multiline  : boolean;
        const ignoreCase : boolean;
        const global     : boolean;
        const extended   : boolean; // E262-4 proposals:extend_regexps
        const sticky     : boolean; // E262-4 proposals:extend_regexps
        const source     : string;

        final function get lastIndex() 
            private::lastIndex;

        final function set lastIndex(x) 
            private::lastIndex = helper::toInteger(x);

        private var lastIndex : double;

        /* E262-4 - [[Match]] may not *have* to be public, but String
         * uses it, and if we want to model the language in the
         * language we should expose it -- it's benign.
         */
        helper function match(s : string, i : double) : MatchResult
            matcher.match(s, i, multiline, ignoreCase);

        /* E262-4 - nCapturingParens used by String.prototype.replace.
         */
        helper function get nCapturingParens() : double
            matcher.nCapturingParens;

        /* Internal */
        private const matcher : RegExpMatcher?;      // The [[Match]] property
        private const names : [string?];            // Named submatches

        /* E262-3 15.10.6.4 probably is meant to require the flags to
         * be returned in lexicographic order for the purposes of
         * toString().
         */
        private function get flags() : string {
            return (global ? "g" : "") +
                   (ignoreCase ? "i" : "") +
                   (multiline ? "m" : "") +
                   (extended ? "x" : "") +
                   (sticky ? "y" : "");
        }
    }
}
