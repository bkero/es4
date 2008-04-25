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

    use default namespace public;
    use namespace intrinsic;
    use namespace ECMAScript4_Internal;
    use namespace Unicode;
    use namespace RegExpInternals;

    __ES4__ final class string!
    {
        // A getter because string is loaded before int
        static function get length() { return 1 }

        /* E262-3 15.5.1: The String Constructor Called as a Function */
        meta static function invoke(value="")
            (value is string) ? value : new string(value);

        /* Don't remove this
        function string(value="")
            magic::newString(value)
        */

        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        static function fromCharCode(...codes)
            string.helper::fromCharCode(codes);

        intrinsic static function fromCharCode(...codes): string
            string.helper::fromCharCode(codes);

        /* IMPLEMENTATION DEVICE.  This method is not private,
         * it is called from methods in 'String'.
         */
        helper static function fromCharCode(codes: Array): string {
            let s = "";
            for (let i=0, limit=codes.length ; i < limit ; ++i)
                s += magic::fromCharCode(intrinsic::toUint(codes[i] & 0x1FFFFF));
            return s;
        }


        /* E262-3 15.5.4.2: String.prototype.toString */
        /*
        prototype function toString(this : string)
            this;
        */

        override intrinsic function toString() : string
            this;

        override intrinsic function toJSONString(pretty:boolean=false) : string
            JSON.formatString(this, pretty);

        /* E262-3 15.5.4.3: String.prototype.valueOf */
        /*
        prototype function valueOf(this : string)
            this;
        */

        override intrinsic function valueOf() : string
            this;

        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        /*
        prototype function charAt(pos)
            string.charAt(this, pos);
        */

        intrinsic function charAt(pos: double = 0) : string
            string.charAt(this, pos);

        static function charAt(self, pos) : string {
            let S    = string(self);
            let ipos = helper::toInteger(pos);
            if (ipos < 0 || ipos >= S.length)
                return "";
            return magic::fromCharCode(magic::charCodeAt(S, intrinsic::toUint(ipos)));
        }


        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */

        intrinsic function charCodeAt(pos: double = 0) : double
            string.charCodeAt(this, pos);

        static function charCodeAt(self, pos) : double {
            let S = string(self);
            let ipos = helper::toInteger(pos);
            if (ipos < 0 || ipos >= S.length)
                return NaN;
            return magic::charCodeAt(S, intrinsic::toUint(ipos));
        }


        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        /*
        prototype function concat(...args)
            string.helper::concat(this, args);
        */

        intrinsic function concat(...args) : string
            string.helper::concat(this, args);

        static function concat(self, ...args)
            string.helper::concat(self, args);

        /* IMPLEMENTATION DEVICE.  This method is not private,
         * it is called from methods in 'String'.
         */
        helper static function concat(self, strings) : string {
            let S = string(self);
            let n = strings.length;
            for (let i=0; i < n ; i++)
                S += string(strings[i]);
            return S;
        }


        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        /*
        prototype function indexOf(searchString, position)
            string.indexOf(this, searchString, position);
        */

        intrinsic function indexOf(searchString: string, position: double = 0.0) : double
            string.indexOf(this, searchString, position);

        static function indexOf(self, searchString, position) : double {
            let S     = string(self);
            let SS    = string(searchString);
            let pos   = helper::toInteger(position);
            let slen  = S.length;
            let m     = Math.min(Math.max(pos, 0), slen);
            let sslen = SS.length;
            let lim   = slen - sslen + 1;

            outer:
            for ( let k = m ; k < lim ; k++ ) {
                for ( let w = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(S, intrinsic::toUint(k+w)) !== magic::charCodeAt(SS, intrinsic::toUint(w)))
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
        /*
        prototype function lastIndexOf(searchString, position)
            string.lastIndexOf(this, searchString, position);
        */

        intrinsic function lastIndexOf(searchString: string, position: double) : double
            string.lastIndexOf(this, searchString, position);

        static function lastIndexOf(self, searchString, position) : double {
            let S     = string(self);
            let SS    = string(searchString);
            let pos   = let (tmp = double(position)) isNaN(tmp) ? Infinity : helper::toInteger(tmp);
            let slen  = S.length;
            let sslen = SS.length;
            let m     = Math.min(Math.max(pos, 0), slen);

            if (sslen > slen)
                return -1;

            outer:
            for ( let k = Math.min(m, slen-sslen) ; k >= 0 ; k-- ) {
                for ( let w = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(S, intrinsic::toUint(k+w)) !== magic::charCodeAt(SS, intrinsic::toUint(w)))
                        continue outer;
                }
                return k;
            }
            return -1;
        }

        /* E262-3 15.5.4.9: String.prototype.localeCompare
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function localeCompare(that)
            string.localeCompare(this, that);
        */

        intrinsic function localeCompare(that : string) : double
            string.localeCompare(this, that);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function localeCompare(self, that) : double {
            let A  = string(self);
            let B  = string(that);
            let la = A.length;
            let lb = B.length;
            let l  = la < lb ? la : lb;

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
        /*
        prototype function match(regexp)
            string.match(this, regexp);
        */

        intrinsic function match(regexp: RegExp) : Array
            string.match(this, regexp);

        static function match(self, regexp): Array {
            let S = string(self);
            let R = (regexp is RegExp) ? regexp : new RegExp(regexp);

            if (!R.global)
                return R.exec(S);  // ie, intrinsic::exec

            let matches = [];

            R.lastIndex = 0;
            while (true) {
                let oldLastIndex = R.lastIndex;
                let res = R.exec(S);

                if (res === null)
                    break;

                matches.push(res[0]);
                if (R.lastIndex === oldLastIndex)
                    ++R.lastIndex;
            }
            if (matches.length == 0)
                return null;      // Cf errata
            else
                return matches;
        }


        /* E262-3 15.5.4.11: String.prototype.replace
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function replace(searchValue, replaceValue)
            string.replace(this, searchValue, replaceValue);
        */

        intrinsic function replace(searchValue: (string|RegExp!),
                                   replaceValue: (string|function(...):string)) : string
            string.replace(this, searchValue, replaceValue);

        static function replace(self, s, r): string {

            // paragraph 4
            let function substituteFunction(start: double, end: double, m: double, cap: Array) : string {
                let A = [];
                A[0] = S.substring(start, end);
                for ( let i=0 ; i < m ; i++ )
                    A[i+1] = cap[i+1];
                A[m+2] = start;
                A[m+3] = S;
                return string(replaceFun.apply(null, A));
            }

            // paragraph 5
            let function substituteString(start: double, end: double, m: double, cap: Array) : string {
                let s   = "";
                let i   = 0;
                let r   = /\$(?:(\$)|(\&)|(\`)|(\')|([0-9]{1,2}))/g;
                let res;

                while ((res = r.exec(replaceString)) !== null) {
                    s += replaceString.substring(i, r.lastIndex - res[0].length);
                    i = r.lastIndex;

                    if (res[1])      s += "$";
                    else if (res[2]) s += S.substring(start, end);
                    else if (res[3]) s += S.substring(0, start);
                    else if (res[4]) s += S.substring(end);
                    else {
                        let n = parseInt(res[5]);
                        if (n <= m && cap[n] !== undefined)
                            s += cap[n];
                    }
                }
                s += replaceString.substring(i);

                return s;
            }

            let function match( regexp, i : double ) : [double, CapArray]  {
                while (i <= S.length) {
                    let res : MatchResult = regexp.helper::match(S, i);
                    if (res !== null) {
                        res.captures[0] = S.substring(i,res.endIndex);
                        return [i, res.captures];
                    }
                    ++i;
                }
                return [0, null];
            }

            let S             = string(self);
            let replaceString = (r is string) ? r cast string : null;
            let replaceFun    = (r is Function) ? r cast Function : null;

            let substitute : function (double, double, double, Array) : string =
                replaceFun !== null ? substituteFunction : substituteString;

            if (s !== null && s is RegExp) {
                // paragraph 2

                let regexp = s cast RegExp;
                let m      = regexp.helper::nCapturingParens;

                if (!regexp.global) {
                    let [i, res] = match(regexp, 0);

                    if (res === null)
                        return S;

                    let end = i + res[0].length;
                    return S.substring(0,i) + substitute(i, end, m, res) + S.substring(end);
                }
                else {
                    // Note that regexp.lastIndex is visible to the
                    // user's replace function, if any, and should be
                    // updated before that function is called and read
                    // anew on each iteration, in case it was updated.

                    let newstring = "";
                    let prevEnd   = 0;

                    regexp.lastIndex = 0;
                    while (true) {
                        let oldLastIndex : double = regexp.lastIndex;
                        let [i,res] = match(regexp, intrinsic::toUint(oldLastIndex));

                        if (res === null)
                            break;

                        newstring += S.substring(prevEnd, i);

                        let end = i + res[0].length;
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
                // paragraph 3

                let searchString = string(s);
                let pos          = S.indexOf(searchString, 0);

                // paragraph 6: the new string is derived from the old string by
                // making replacements for each match.  If there are no matches,
                // then the new string must therefore be the old string.
                if (pos === -1)
                    return S;

                let end = pos + searchString.length;
                return S.substring(0,pos) + substitute(pos, end, 0, []) + S.substring(end);
            }
        }


        /* E262-3 15.5.4.12: String.prototype.search
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function search(regexp)
            string.search(this, regexp);
        */

        intrinsic function search(regexp: RegExp!) : double
            string.search(this, regexp);

        static function search(self, regexp): double {
            let S = string(self);
            let R = (regexp is RegExp) ? regexp : new RegExp(regexp);

            for ( let i=0, limit=S.length ; i < limit ; i++ )
                if (R.helper::match(S, i) !== null)
                    return i;
            return -1;
        }


        /* E262-3 15.5.4.13: String.prototype.slice
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function slice(start, end, step)
            string.slice(this, start, end, step);
        */

        intrinsic function slice(start: AnyNumber=NaN, end: AnyNumber=NaN, step: AnyNumber=1): string
            string.slice(this, start, end, step);

        static function slice(object, start: AnyNumber=NaN, end: AnyNumber=NaN, step: AnyNumber=1) {

            let len = intrinsic::toUint(object.length);

            step = int(step);
            if (step == 0)
                step = 1;

            if (intrinsic::isNaN(start))
                start = step > 0 ? 0 : (len-1);
            else
                start = helper::clamp(start, len);
            
            if (intrinsic::isNaN(end))
                end = step > 0 ? len : (-1);
            else
                end = helper::clamp(end, len);
            
            let out = new string();
            for (let i = start; step > 0 ? i < end : i > end; i += step)
                out += object[i];

            return out;
        }

        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        /*
        prototype function split(separator, limit)
            string.split(this, separator, limit);
        */

        intrinsic function split(separator:(string|RegExp!), limit: double = double.MAX_VALUE): Array!
            string.split(this, separator, limit)

        static function split(self, separator, limit) : Array! {

            type matcher = (string|RegExp!);

            // FIXME: when 'let function' goes away, ensure that this doesn't hoist away from
            // the definition of type matcher.

            let function splitMatch(R: matcher, S: string, q: double) : [double, [string]]? {
                switch type (R) {
                case (x: string) {
                    let r = x.length;
                    if (q + r <= S.length && S.substring(q, q + r) === R)
                        return [q+r, []];
                    else
                        return null;
                }
                case (x: RegExp!) {
                    let mr: MatchResult = x.helper::match(S, q);
                    if (mr === null)
                        return null;
                    else
                        return [mr.endIndex, mr.captures];
                }
                }
            }

            // 1-6
            let A   = new Array;
            let lim = limit === undefined ? double.MAX_VALUE : double(limit);
            let S   = string(self);
            let s   = S.length;
            let p   = 0;
            let R;

            // FIXME: Ticket #72 / #73: ought to be enough to use `separator is RegExp!`
            if (separator !== null && separator is RegExp)
                R = separator;
            else
                R = string(separator);

            // 7
            if (lim === 0)
                return A;

            // 8; 33-34
            if (separator === undefined) {
                A[0] = S;
                return A;
            }

            // 9; 31-34
            if (s === 0) {
                let z = splitMatch(R, S, 0);
                if (z === null)
                    A[0] = S;
                return A;
            }

            // 10-27
            for ( let q = p ; q !== s ; ) {
                // 12-13; 26-27
                let z = splitMatch(R, S, q);
                if (z === null) {
                    ++q;
                    continue;
                }

                // 14-15
                let [e,cap] = z;
                if (e === p) {
                    ++q;
                    continue;
                }

                // 16-25
                A[A.length] = S.substring(p, q);
                if (A.length === lim)
                    return A;

                p = e;

                for ( let i=1 ; i < cap.length ; i++ ) {
                    A[A.length] = cap[i];
                    if (A.length === lim)
                        return A;
                }

                q = p;
            }

            // 28-30
            A[A.length] = S.substring(p, s);
            return A;
        }

        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function substring(start, end)
            string.substring(this, start, end);
        */

        intrinsic function substring(start: double, end: double=this.length) : string
            string.substring(this, start, end);

        static function substring(self, start, end) : string {
            let S   = string(self);
            let len = S.length;

            start = helper::toInteger(start);
            end = end === undefined ? len : helper::toInteger(end);

            start = Math.min(Math.max(start, 0), len);
            end = Math.min(Math.max(end, 0), len);

            if (start > end)
                [start, end] = [end, start];

            let s = "";
            for ( let i=start ; i < end ; i++ )
                s += S[i];

            return s;
        }


        /* E262-3 15.5.4.16: String.prototype.toLowerCase
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function toLowerCase()
            string.toLowerCase(this);
        */

        intrinsic function toLowerCase() : string
            string.toLowerCase(this);

        static function toLowerCase(self): string {
            let S = string(self);
            let s = "";

            for ( let i=0, limit=S.length ; i < limit ; i++ ) {
                let u = Unicode.toLowerCaseCharCode(magic::charCodeAt(S,intrinsic::toUint(i)));
                if (u is double)
                    s += magic::fromCharCode(intrinsic::toUint(u));
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
        /*
        prototype function toLocaleLowerCase()
            string.toLocaleLowerCase(this);
        */

        intrinsic function toLocaleLowerCase() : string
            string.toLowerCase(this);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function toLocaleLowerCase(self): string
            string.toLowerCase(self);


        /* E262-3 15.5.4.18: String.prototype.toUpperCase
           E262-4 draft proposals:static_generics
         */
        /*
        prototype function toUpperCase()
            string.toUpperCase(this);
        */

        intrinsic function toUpperCase() : string
            string.toUpperCase(this);

        static function toUpperCase(self): string {
            let S   = string(self);
            let s   = "";

            for ( let i=0, limit=S.length ; i < limit ; i++ ) {
                let u = Unicode.toUpperCaseCharCode(magic::charCodeAt(S,intrinsic::toUint(i)));
                if (u is double)
                    s += magic::fromCharCode(intrinsic::toUint(u));
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
        /*
        prototype function toLocaleUpperCase()
            string.toLocaleUpperCase(this);
        */

        intrinsic function toLocaleUpperCase() : string
            string.toLocaleUpperCase(this);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        static function toLocaleUpperCase(self)
            string.toUpperCase(self);


        /* E262-4 draft proposals:json_encoding_and_decoding */
        /*
        prototype function parseJSON()
            this.parseJSON();
        */

        intrinsic function parseJSON(filter=undefined)
            string.parseJSON(this, filter);

        static function parseJSON(self, filter=undefined)
            JSON.parse(self, filter);

        intrinsic function trim() : string
            string.trim(this);

        static function trim(s): string {
            s = string(s);

            let len = s.length;
            let i, j;

            for ( i=0 ; i < len && Unicode.isTrimmableSpace(s.charAt(i)) ; i++ )
                ;
            for ( j=len-1 ; j >= i && Unicode.isTrimmableSpace(s.charAt(j)) ; j-- )
                ;
            return s.substring(i,j+1);
        }


        /* E262-3 15.5.5.1: length. */
        function get length() : double
            magic::stringLength(this);

        /* Catchall indexing operation. */
        meta function get(pos) {
            let x = double(pos);
            if (isNaN(x))
                return undefined;
            return charAt(x);
        }

        /* The E262-3 string primitive consumes all additional [[set]] operations. */
        meta function set(n,v) : void
        {
        }
    }
