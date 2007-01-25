/* -*- indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 *
 * Status: complete; not reviewed; not tested.
 *
 * See RegExpCompiler.es for the compiler.
 * See RegExpEvaluator.es for the evaluator and compiled code representation.
 */

package RegExp
{
    import Unicode.*;
    use namespace intrinsic;
    use strict;

    /* E262-3 15.10: Regular expression object */
    public dynamic class RegExp
    {
        /* E262-3 15.10.3.1: The RegExp constructor called as a function */
        static intrinsic function call( pattern, flags ) {
            if (pattern is RegExp && flags === undefined)
                return pattern;
            else
                return new RegExp(pattern, flags);
        }

        /* E262-3 15.10.4.1: The RegExp constructor */
        public function RegExp( pattern, flags ) {
            let source : String! = "";

            if (pattern is RegExp) {
                if (flags === undefined) {
                    source = pattern.source;
                    flags = pattern.flags;
                }
                else 
                    throw new TypeError("Illegal construction of regular expression");
            }
            else {
                source = pattern === undefined ? "" : String(pattern);
                flags = flags === undefined ? "" : String(flags);
            }

            let usedflags : Object! = { m: false, i: false, g: false, x: false, y: false };

            for each ( let f : String! in flags.split("") ) {
                if (!(f in usedflags))
                    throw new SyntaxError("Invalid flag: " + f);
                if (usedflags.f)
                    throw new SyntaxError("Duplicated flag: " + f);
                usedflags[f] = true;
            }

            matcher = (new RegExpCompiler(source, flags)).compile();

            multiline = usedflags.m;
            ignoreCase = usedflags.i;
            global = usedflags.g;
            extended = usedflags.x;
            sticky = usedflags.y;
            lastIndex = 0;
            source = source;
        }

        /* E262-4 proposals:extend_regexps: RegExp instances are
           callable, and a call to an instance is equivalent to
           calling its exec() method.
        */
        public function call(s) : Array
            this.exec(s);

        intrinsic function call(s : String!) : Array
            exec(s);

        /* E262-3 15.10.6.2: RegExp.prototype.exec */
        intrinsic function exec(s : String!) : Array {
            let S : String! = ToString(s);
            let length : uint = S.length;
            let i : Number = ToInteger(lastIndex);
            if (!global)
                i = 0;
            let res : MatchResult = failure;
            while (true) {
                if (i < 0 || i > length) {
                    lastIndex = 0;
                    return null;
                }
                res = matcher.match(s, i);
                if (res !== failure)
                    break;
                ++i;
            }
            if (global)
                lastIndex = res.endIndex;
            let a : Array = new Array(res.cap.length+1);
            a.index = i;
            a.input = S;
            a.length = res.cap.length+1;
            a[0] = S.substring(i,res.endIndex);
            for ( var j=1 ; j <= res.cap.length ; j++ )
                a[j] = res.cap[j];
            return a;
        }

        prototype function exec(s)
            this.exec(s);

        /* E262-3 15.10.6.3: RegExp.prototype.test */
        intrinsic function test(s : String!) : Boolean
            exec(s) !== null;

        prototype function test(s)
            this.test(s);

        /* E262-3 15.10.6.4: RegExp.prototype.toString */
        intrinsic function toString() : String!
            "/" + (source.length == 0 ? "(?:)" : source) + "/" + flags;

        prototype function toString()
            this.toString();

        /* E262-3 15.10.7: properties of regexp instances */
        public const multiline  : Boolean;
        public const ignoreCase : Boolean;
        public const global     : Boolean;
        public const extended   : Boolean;  // E262-4 proposals:extend_regexps
        public const sticky     : Boolean;  // E262-4 proposals:extend_regexps
        public const source     : String!;
        public var   lastIndex  : Number;

        /* E262-4 - [[Match]] may not *have* to be public, but String
           uses it, and if we want to model the language in the
           language we should expose it -- it's benign. 
        */
        intrinsic function match(s : String!, i : uint) : MatchResult
            matcher.match(s, i);

        /* E262-4 - nCapturingParens used by String.prototype.replace 
         */
        intrinsic function get nCapturingParens() : uint
            matcher.nCapturingParens;

        /* Internal */
        const matcher : RegExpMatcher;      // The [[Match]] property

        function get flags() : String! {
            return (multiline ? "m" : "") +
                   (ignoreCase ? "i" : "") +
                   (global ? "g" : "") +
                   (extended ? "x" : "") +
                   (sticky ? "y" : "");
        }
    }
}
