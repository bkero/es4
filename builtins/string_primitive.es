/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "string" object
 *
 * E262-3 15.5
 * E262-4 proposals:json_encoding_and_decoding
 * E262-4 proposals:string.prototype.trim
 * E262-4 proposals:bug_fixes
 * E262-4 proposals:static_generics
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
 * FIXME: add "substr"
 *
 * Status: incomplete; not reviewed; not tested.
 *
 * Strings are constructed by:
 *    new string + magic::setStringValue
 *    magic::fromCharCode
 *    magic::stringAppend
 *
 * A string's length is obtained by:
 *    magic::stringLength
 *
 * strings are picked apart by:
 *    magic::charCodeAt
 *
 * (We would expect "+" to map to magic::stringAppend().)
 *
 * Note, most string functions are "generic".  This is implemented as
 * either a static method on the "string" class that treats a "self"
 * argument generically according to the spec, or as a private static
 * method ditto.  We avoid implementing generic behavior in instance
 * methods of the "string" class because it implies too much about the
 * type of "this".
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use strict;
    import Unicode.*
    import RegExpInternals.*

    intrinsic final class string! extends String
    {
        /* E262-4 draft */
        meta static function convert(x)
            string(x);

        /* E262-3 15.5.1: The String Constructor Called as a Function */
        meta static function invoke(x="")
            x is string ? x : magic::newString(x);

        /* 15.5.2 The string Constructor */
        function string(x="") : super(x)
        {
            // No need to magic::bindString a second time,
            // since our super(x) call did it for us.
        }


        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function fromCharCode(...args)
            fromCharCodeHelper(args);

        intrinsic static function fromCharCode(...args) : double
            fromCharCodeHelper(args);

        /* IMPLEMENTATION DEVICE.  This method is not private,
         * it is called from methods in 'String'.
         */
        static function fromCharCodeHelper(codes : Array) : double {
            let s : string = "";
            let n : uint = codes.length;
            for (let i : uint = 0 ; i < n ; ++i)
                s += magic::fromCharCode(uint(codes[i]));
            return s;
        }


        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : string)
            this;

        override intrinsic function toString() : string
            this;


        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : string)
            this;

        override intrinsic function valueOf() : string
            this;


        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        prototype function charAt(pos)
            string.charAt(this, pos);

        override intrinsic function charAt(pos: double = 0) : string
            string.charAt(this, pos);

        static function charAt(self, pos) : string {
            let S    : string = ToString(self);
            let ipos : double = ToInteger(pos);
            if (ipos < 0 || ipos >= S.length)
                return "";
            return magic::fromCharCode(magic::charCodeAt(S, ToUint(ipos)));
        }


        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        prototype function charCodeAt(pos)
            string.charCodeAt(this, ToDouble(pos));

        override intrinsic function charCodeAt(pos: double = 0) : double
            string.charCodeAt(this, pos);

        static function charCodeAt(self, pos) : double {
            let S : string = ToString(self);
            let ipos: double = ToInteger(pos);
            if (ipos < 0 || ipos >= S.length)
                return NaN;
            return magic::charCodeAt(S, ToUint(ipos));
        }


        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            string.concatHelper(this, args);

        override intrinsic function concat(...args) : string
            string.concatHelper(this, args);

        static function concat(self, ...args)
            string.concatHelper(self, args);

        /* IMPLEMENTATION DEVICE.  This method is not private,
         * it is called from methods in 'String'.
         */
        static function concatHelper(self, strings) : string {
            let S : string = ToString(self);
            let n : uint = strings.length;
            for (let i : uint = 0; i < n ; i++)
                S += ToString(strings[i]);
            return S;
        }


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position)
            string.indexOf(this, searchString, position);

        override intrinsic function indexOf(searchString: string, position: double = 0.0) : double
            string.indexOf(this, searchString, position);

        static function indexOf(self, searchString, position) : double {
            let S     : string = ToString(self);
            let SS    : string = ToString(searchString);
            let pos   : double = ToInteger(position);
            let slen  : uint = S.length;
            let m     : uint = Math.min(Math.max(pos, 0), slen);
            let sslen : uint = SS.length;
            let lim          = slen - sslen + 1;  /* Beware of uint optimization... */

            outer:
            for ( let k = m ; k < lim ; k++ ) {
                for ( let w = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(S, uint(k+w)) !== magic::charCodeAt(SS, uint(w)))
                        continue outer;
                }
                return k;
            }
            return -1;
        }


        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function lastIndexOf(searchString, position)
            string.lastIndexOf(this, searchString, position);

        override intrinsic function lastIndexOf(searchString: string, position: double) : double
            string.lastIndexOf(self, searchString, position);

        static function lastIndexOf(self, searchString, position) : double {
            let S     : string = ToString(self);
            let SS    : string = ToString(searchString);
            let pos   : double = let (tmp = ToDouble(position)) isNaN(tmp) ? Infinity : ToInteger(tmp);
            let slen  : uint = S.length;
            let sslen : uint = SS.length;
            let m     : uint = Math.min(Math.max(pos, 0), slen);

            if (sslen > slen)
                return -1;

            outer:
            for ( let k = Math.min(m, slen-sslen) ; k >= 0 ; k-- ) {  /* Beware of uint optimization... */
                for ( let w = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(S, uint(k+w)) !== magic::charCodeAt(SS, uint(w)))
                        continue outer;
                }
                return k;
            }
            return -1;
        }

        /* E262-3 15.5.4.9: String.prototype.localeCompare
           E262-4 draft proposals:static_generics
         */
        prototype function localeCompare(that)
            string.localeCompare(this, that);

        override intrinsic function localeCompare(that : string) : double
            string.localeCompare(self, that);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function localeCompare(self, that) : double {
            let A  : string = ToString(self);
            let B  : string = ToString(that);
            let la : uint = A.length;
            let lb : uint = B.length;
            let l  : uint = la < lb ? la : lb;

            for ( let i=0 ; i < l ; i++ ) {
                let a = A.charCodeAt(i),
                    b = B.charCodeAt(i);
                if (a !== b)
                    return a - b;
            }
            return la - lb;
        }


        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        prototype function match(regexp)
            string.match(this, regexp);

        override intrinsic function match(regexp: RegExp) : Array
            string.match(this, regexp);

        static function match(self, regexp): Array {
            let S : string = ToString(self);
            let R : RegExp = regexp is RegExp ? regexp : new RegExp(regexp);

            if (!R.global)
                return R.exec(S);  // ie, intrinsic::exec

            let matches = [];

            R.lastIndex = 0;
            while (true) {
                let oldLastIndex : double = R.lastIndex;
                let res : Array = R.exec(S);

                if (res === null)
                    break;

                matches.push(res[0]);
                if (R.lastIndex === oldLastIndex)
                    ++R.lastIndex;
            }
            if (matches.length == 0)
                return null;      /* Cf errata */
            else
                return matches;
        }


        /* E262-3 15.5.4.11: String.prototype.replace
           E262-4 draft proposals:static_generics
         */
        prototype function replace(searchValue, replaceValue)
            string.replace(this, searchValue, replaceValue);

        /* FIXME: it's an open question if the interface here should use (String!,RegExp!)
           or (string,RegExp!), and whether the replace function should return "string" or
           "String!".  This has implications for strict mode only.
           
           Note that the superclass uses String!, and will need to be involved in the fix
           somehow.
        */
        override
        intrinsic function replace(searchValue: (String!,RegExp!),
                                   replaceValue: (String!,function(...):String!)) : string
            string.replace(this, searchValue, replaceValue);

        static function replace(self, s, r): string {

            /* paragraph 4 */
            let function substituteFunction(start: uint, end: uint, m: uint, cap: Array) : string {
                let A : Array = [];
                A[0] = S.substring(start, end);
                for ( let i : uint=0 ; i < m ; i++ )
                    A[i+1] = cap[i+1];
                A[m+2] = start;
                A[m+3] = S;
                return ToString(replaceFun.apply(null, A));
            }

            /* paragraph 5 */
            let function substituteString(start: uint, end: uint, m: uint, cap: Array) : string {
                let s   : string = "";
                let i   : uint = 0;
                let r   : RegExp = /\$(?:(\$)|(\&)|(\`)|(\')|([0-9]{1,2}))/g;
                let res : Array;

                while ((res = r.exec(replaceString)) !== null) {
                    s += replaceString.substring(i, r.lastIndex - res[0].length);
                    i = r.lastIndex;

                    if (res[1])      s += "$";
                    else if (res[2]) s += S.substring(start, end);
                    else if (res[3]) s += S.substring(0, start);
                    else if (res[4]) s += S.substring(end);
                    else {
                        let n : int = parseInt(res[5]);
                        if (n <= m && cap[n] !== undefined)
                            s += cap[n];
                    }
                }
                s += replaceString.substring(i);

                return s;
            }

            let function match( regexp, i : uint ) : [uint, CapArray]  {
                while (i <= S.length) {
                    let res : MatchResult = regexp.match(S, i);
                    if (res !== null) {
                        res.cap[0] = S.substring(i,res.endIndex);
                        return [i, res.cap];
                    }
                    ++i;
                }
                return [0, null];
            }

            let S             : string = ToString(self);
            let replaceString : string? = r is string ? r cast string : null;
            let replaceFun    : Function = r is Function ? r cast Function : null;

            let substitute : function (uint, uint, uint, Array) : string =
                replaceFun !== null ? substituteFunction : substituteString;

            if (s !== null && s is RegExp) {
                /* paragraph 2 */

                let regexp : RegExp = s cast RegExp;
                let m : uint = regexp.nCapturingParens;

                if (!regexp.global) {
                    let [i, res] : [uint, CapArray] = match(regexp, 0);

                    if (res === null)
                        return S;

                    let end : uint = i + res[0].length;
                    return S.substring(0,i) + substitute(i, end, m, res) + S.substring(end);
                }
                else {
                    /* Note that regexp.lastIndex is visible to the
                     * user's replace function, if any, and should be
                     * updated before that function is called and read
                     * anew on each iteration, in case it was updated.
                     */
                    let newstring : string = "";
                    let prevEnd : uint = 0;

                    regexp.lastIndex = 0;
                    while (true) {
                        let oldLastIndex : double = regexp.lastIndex;
                        let [i,res] : [uint, CapArray?] = match(regexp, uint(oldLastIndex));

                        if (res === null)
                            break;

                        newstring += S.substring(prevEnd, i);

                        let end : uint = i + res[0].length;
                        regexp.lastIndex = end;
                        if (regexp.lastIndex == oldLastIndex)
                            regexp.lastIndex++;
                        newstring += substitute(i, end, m, res);
                        prevEnd = end;
                    }
                    newstring += S.substring(prevEnd, S.length);

                    return newstring;
                }
            }
            else {
                /* paragraph 3 */

                let searchString : string = ToString(s);
                let pos : double = S.indexOf(searchString, 0);

                /* paragraph 6: the new string is derived from the old string by
                   making replacements for each match.  If there are no matches,
                   then the new string must therefore be the old string.  */
                if (pos === -1)
                    return S;

                let end : uint = pos + searchString.length;
                return S.substring(0,pos) + substitute(pos, end, 0, []) + S.substring(end);
            }
        }


        /* E262-3 15.5.4.12: String.prototype.search
           E262-4 draft proposals:static_generics
         */
        prototype function search(regexp)
            string.search(this, regexp);

        override intrinsic function search(regexp: RegExp!) : double
            string.search(this, regexp);

        static function search(self, regexp): double {
            let S   : string = ToString(self);
            let R   : RegExp = regexp is RegExp ? regexp : new RegExp(regexp);
            let lim : uint = S.length;

            for ( let i : uint=0 ; i < lim ; i++ )
                if (R.match(S, i) !== null)
                    return i;
            return -1;
        }


        /* E262-3 15.5.4.13: String.prototype.slice
           E262-4 draft proposals:static_generics
         */
        prototype function slice(start, end)
            string.slice(this, start, end);

        override intrinsic function slice(start: double, end: double): Array
            string.slice(this, start, end);

        static function slice(self, s, e): Array {
            let S     : string = ToString(self);
            let len   : double = S.length;
            let start : double = ToInteger(s);
            let end   : double = e === undefined ? len : ToInteger(e);

            let startpos = start < 0 ? Math.max(len+start,0) : Math.min(start,len);
            let endpos = end < 0 ? Math.max(len+end,0) : Math.min(end,len);
            let n = Math.max(endpos-startpos,0);

            return S.substring(startpos, startpos+n);
        }


        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        prototype function split(separator, limit)
            string.split(this, separator, limit);

        /* FIXME: it's an open question if the interface here should use (String!,RegExp!) or
           (string,RegExp!).  

           Note that the superclass uses String! and that that has an impact here.
        */
        override
        intrinsic function split(separator:(String!,RegExp!), limit: uint = uint.MAX_VALUE): Array!
            string.split(this, separator, limit)

        static function split(self, separator, limit) : Array! {

            type matcher = (string,RegExp!);

            function splitMatch(R: matcher, S: string, q: uint) : [uint, [string]]? {
                /* FIXME (Ticket #70): type annotation on expression in 'switch type'
                 * should not be necessary.
                 */
                switch type (R):* {
                case (x: string) {
                    let r : uint = x.length;
                    if (q + r <= S.length && S.substring(q, q + r) === R)
                        return [q+r, []];
                    else
                        return null;
                }
                case (x: RegExp!) {
                    let mr : MatchResult = x.match(S, q);
                    if (mr === null)
                        return null;
                    else
                        return [mr.endIndex, mr.cap];
                }
                }
            }

            /* 1-6 */
            let A   = new Array;
            let lim : uint = limit === undefined ? uint.MAX_VALUE : ToUint(limit);
            let S   : string = ToString(self);
            let s   : uint = S.length;
            let p   : uint = 0;
            let R   : matcher;

            /* FIXME: Ticket #72 / #73: ought to be enough to use `separator is RegExp!` */
            if (separator !== null && separator is RegExp)
                R = separator;
            else
                R = ToString(separator);

            /* 7 */
            if (lim === 0)
                return A;

            /* 8; 33-34 */
            if (separator === undefined) {
                A[0] = S;
                return A;
            }

            /* 9; 31-34 */
            if (s === 0) {
                let z = splitMatch(R, S, 0);
                if (z === null)
                    A[0] = S;
                return A;
            }

            /* 10-27 */
            for ( let q : uint = p ; q !== s ; ) {
                /* 12-13; 26-27 */
                let z = splitMatch(R, S, q);
                if (z === null) {
                    ++q;
                    continue;
                }

                /* 14-15 */
                let [e,cap] : [uint,[*]] = z;
                if (e === p) {
                    ++q;
                    continue;
                }

                /* 16-25 */
                A[A.length] = S.substring(p, q);
                if (A.length === lim)
                    return A;

                p = e;

                for ( let i : uint = 1 ; i < cap.length ; i++ ) {
                    A[A.length] = cap[i];
                    if (A.length === lim)
                        return A;
                }

                q = p;
            }

            /* 28-30 */
            A[A.length] = S.substring(p, s);
            return A;
        }

        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        prototype function substring(start, end)
            string.substring(this, start, end);

        override intrinsic function substring(start: double, end: double=this.length) : string
            string.substring(this, start, end);

        static function substring(self, start, end) : string {
            let S   : string = ToString(self);
            let len : double = S.length;

            start = ToInteger(start);
            end = end === undefined ? len : ToInteger(end);

            start = Math.min(Math.max(start, 0), len);
            end = Math.min(Math.max(end, 0), len);

            if (start > end)
                [start, end] = [end, start];

            let s : string = "";
            let a : uint = start;
            let b : uint = end;

            for ( let i : uint = a ; i < b ; i++ )
                s += S[i];

            return s;
        }


        /* E262-3 15.5.4.16: String.prototype.toLowerCase
           E262-4 draft proposals:static_generics
         */
        prototype function toLowerCase()
            string.toLowerCase(this);

        override intrinsic function toLowerCase() : string
            string.toLowerCase(this);

        static function toLowerCase(self): string {
            let S   : string = ToString(self);
            let s   : string = "";
            let len : uint = S.length;
            for ( let i : uint = 0u ; i < len ; i++ ) {
                let u = Unicode.toLowerCaseCharCode(magic::charCodeAt(S,i));
                if (u is uint)
                    s += magic::fromCharCode(u);
                else {
                    for ( let j=0 ; j < u.length ; j++ )
                        s += magic::fromCharCode(u[j]);
                }
            }
            return s;
        }


        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleLowerCase()
            string.toLocaleLowerCase(this);

        override intrinsic function toLocaleLowerCase() : string
            string.toLowerCase(this);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function toLocaleLowerCase(self): string
            string.toLowerCase();


        /* E262-3 15.5.4.18: String.prototype.toUpperCase
           E262-4 draft proposals:static_generics
         */
        prototype function toUpperCase()
            string.toUpperCase(this);

        override intrinsic function toUpperCase() : string
            string.toUpperCase(this);

        static function toUpperCase(self): string {
            let S   : string = ToString(self);
            let s   : string = "";
            let len : uint = S.length;
            for ( let i : uint = 0u ; i < len ; i++ ) {
                let u = Unicode.toUpperCaseCharCode(magic::charCodeAt(S,i));
                if (u is uint)
                    s += magic::fromCharCode(u);
                else {
                    for ( let j=0 ; j < u.length ; j++ )
                        s += magic::fromCharCode(u[j]);
                }
            }
            return s;
        }


        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleUpperCase()
            string.toLocaleUpperCase(this);

        override intrinsic function toLocaleUpperCase() : string
            string.toLocaleUpperCase(this);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function toLocaleUpperCase(self)
            string.toUpperCase(self);


        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON()
            this.parseJSON();

        override intrinsic function parseJSON(...args)
            JSON.parse.apply(null, args.unshift(this));


        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            ToString(this).trim();

        override intrinsic function trim() : string {
            let len  : uint = this.length;
            let i, j : uint;

            for ( i=0 ; i < len && Unicode.isTrimmableSpace(charAt(i)) ; i++ )
                ;
            for ( j=len-1 ; j >= i && Unicode.isTrimmableSpace(charAt(j)) ; j-- )
                ;
            return substring(i,j+1);
        }


        /* E262-3 15.5.5.1: length. */
        override function get length() : uint
            magic::stringLength(this);

        /* The E262-3 string primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
}
