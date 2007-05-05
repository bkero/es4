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
 * FIXME: add "substr"
 *
 * FIXME: if String is subclassable then what do we say about overriding
 *        methods -- how does that impact other methods?
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
 */

package
{
    use default namespace public;
    use namespace intrinsic;
    use strict;

    intrinsic final class string! extends String
    {       
        /* E262-3 15.5.1: The String Constructor Called as a Function */
        static meta function invoke(x="")
            x is string ? x : new string(x);

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

        static function fromCharCodeHelper(codes : Array) : double {
            let s : string = "";
            let n : uint = codes.length;
            for (let i : uint = 0 ; i < n ; ++i)
                s += magic::fromCharCode(uint(codes[i]));
            return s;
        }

        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : string)
            private::toString(this);

        override intrinsic function toString() : string
            private::toString(this);

        private function toString() : string
            this;
        
        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : string)
            this;

        override intrinsic function valueOf() : string
            private::valueOf();

        private final function valueOf() : string
            this;

        /* E262-3 15.5.4.4: String.prototype.charAt
           E262-4 draft proposals:static_generics
        */
        prototype function charAt(pos)
            ToString(this).charAt(pos);

        static function charAt(self, pos)
            ToString(self).charAt(pos);
            
        override intrinsic function charAt(pos: double = 0) : string
            let (ipos = ToInteger(pos))
                (ipos < 0 || ipos >= length) ? "" : magic::fromCharCode(magic::charCodeAt(this, ToUint(ipos)));

        /* E262-3 15.5.4.5: String.prototype.charCodeAt
           E262-4 draft proposals:static_generics
        */
        prototype function charCodeAt(pos)
            ToString(this).charCodeAt(ToDouble(pos));

        static function charCodeAt(self, pos)
            ToString(self).charCodeAt(ToDouble(pos));

        override intrinsic function charCodeAt(pos: double = 0) : uint
            let (ipos: double = ToInteger(pos))
                (ipos < 0 || ipos >= length) ? NaN : magic::charCodeAt(this, ToUint(ipos));

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            ToString(this).concatHelper(args);

        static function concat(self, ...args)
            ToString(self).concatHelper(args);

        override intrinsic function concat(...args) : string
            concatHelper(args);

        function concatHelper(strings : Array!) : string {
            let s : string = this;
            let n : uint = ToUint(strings.length);
            for (let i : uint = 0; i < n ; i++)
                s += ToString(strings[i]);
            return s;
        }

        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:static_generics
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position = 0.0)
            ToString(this).indexOf(ToString(searchString), ToDouble(position));

        static function indexOf(self, searchString, position = 0.0)
            ToString(self).indexOf(ToString(searchString), ToDouble(position));

        override intrinsic function indexOf(searchString: string, position: double = 0.0) : double {
            position = ToInteger(position);

            let slen  : uint = length;
            let m     : uint = Math.min(Math.max(position, 0), slen);
            let sslen : uint = searchString.length;
            let lim   : uint = slen - sslen + 1;

            outer:
            for ( let k : uint = m ; k < lim ; k++ ) {
                for ( let w : uint = 0u ; w < sslen ; w++ ) {
                    /* FIXME: casts redundant */
                    if (magic::charCodeAt(this, uint(k+w)) !== magic::charCodeAt(searchString, uint(w))) 
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
            ToString(this).lastIndexOf(searchString, position);

        static function lastIndexOf(self, searchString, position)
            ToString(self).lastIndexOf(searchString, position);

        override intrinsic function lastIndexOf(searchString: string, position: double) : double {
            position = isNaN(position) ? Infinity : ToInteger(position);

            let slen  : uint = length;
            let m     : uint = Math.min(Math.max(position, 0), slen);
            let sslen : uint = searchString.length;

            outer:
            for ( let k : uint = m ; k >= 0u ; k-- ) {
                for ( let w : uint = 0u ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(this, k+w) !== magic::charCodeAt(searchString, w)) 
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
            ToString(this).localeCompare(that);

        static function localeCompare(self, that)
            ToString(self).localeCompare(that);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        override intrinsic function localeCompare(that : string) : double {
            let la : uint = length;
            let lb : uint = that.length;
            let l  : uint = la < lb ? la : lb;

            for ( let i=0 ; i < l ; i++ ) {
                let a : uint = charCodeAt(i),
                    b : uint = that.charCodeAt(i);
                if (a !== b)
                    return a - b;
            }
            return la - lb;
        }

        /* E262-3 15.5.4.10: String.prototype.match
           E262-4 draft proposals:static_generics
        */
        prototype function match(regexp)
            ToString(this).match(regexp);

        static function match(self, regexp)
            ToString(self).match(regexp);

        override intrinsic function match(r) : Array {
            let regexp : RegExp! = r instanceof RegExp ? r : new RegExp(r);

            if (!regexp.global)
                return regexp.exec(this);  // ie, intrinsic::exec

            let matches : [string] = [] : [string];

            regexp.lastIndex = 0;
            while (true) {
                let oldLastIndex : double = re.lastIndex;
                let res : Array = re.exec(this);

                if (res === null) 
                    break;

                matches.push(res[0]);
                if (regexp.lastIndex === oldLastIndex)
                    ++regexp.lastIndex;
            }
            return matches;
        }

        /* E262-3 15.5.4.11: String.prototype.replace 
           E262-4 draft proposals:static_generics
         */
        prototype function replace(searchValue, replaceValue)
            ToString(this).replace(searchValue, replaceValue);

        static function replace(self, searchValue, replaceValue)
            ToString(self).replace(searchValue, replaceValue);

        override intrinsic function replace(s, r) : string {

            /* paragraph 4 */
            function substituteFunction(start: uint, end: uint, m: uint, cap: Array) : string {
                let A : Array = [];
                A[0] = this.substring(start, end);
                for ( let i : uint=0 ; i < m ; i++ )
                    A[i+1] = cap[i+1];
                A[m+2] = start;
                A[m+3] = this;
                return ToString(replaceFun.apply(null, A));
            }

            /* paragraph 5 */
            function substituteString(start: uint, end: uint, m: uint, cap: Array) : string {
                let s   : string = "";
                let i   : uint = 0;
                /* FIXME: use regex literal here when lexer can handle them */
                /* let r   : RegExp = /\$(?:(\$)|(\&)|(\`)|(\')|([0-9]{1,2}))/g; */
                let r   : RegExp = new RegExp("\\$(?:(\\$)|(\\&)|(\\`)|(\\')|([0-9]{1,2}))","g");
                let res : Array;

                while ((res = r.exec(replaceString)) !== null) {
                    s += replaceString.substring(i, r.lastIndex - res[0].length);
                    i = r.lastIndex;

                    if (res[1])      s += "$";
                    else if (res[2]) s += substring(start, end);
                    else if (res[3]) s += substring(0, start);
                    else if (res[4]) s += substring(end);
                    else             
                        let (c : int = parseInt(res[5])+1) { 
                            if (c in cap) s += cap[c]; 
                        }
                }
                s += replaceString.substring(i);

                return s;
            }

            function match( i : uint ) : [uint, Array]  {
                while (i < length) {
                    let res : MatchResult = regexp.match(this, i);
                    if (res !== null)
                        return [i, res.cap];
                    ++i;
                }
                return [0, null];
            }

            let replaceString : string? = r instanceof string ? r /* cast string */ : null;  /* FIXME: cast not implemented */
            let replaceFun    : Function = r instanceof Function ? r /* cast Function */ : null;  /* FIXME: cast not implemented */

            let substitute : function (uint, uint, uint, Array) : string =
                replaceFun !== null ? substituteFunction : substituteString;

            if (s instanceof RegExp) {
                /* paragraph 2 */
                let regexp : RegExp = s /* cast RegExp */ ;  /* FIXME: cast not implemented */
                let res : Array = null;
                let m : uint = regexp.nCapturingParens;

                if (!regexp.global) {
                    let [i, res] : [uint, Array] = match(0);

                    if (res === null)
                        return this;

                    let end : uint = i + res[0].length;
                    return substring(0,pos) + substitute(pos, end, m, res) + substring(end);
                }
                else {
                    let s : string;

                    while (true) {
                        let oldi : uint = i;
                        let [i, res] : [uint, [string]] = match(i);

                        if (res === null)
                            return s + substring(oldi);

                        //...;
                    }
                }
            }
            else {
                /* paragraph 3 */
                let searchString : string = ToString(s);
                let pos : double = indexOf(searchString, 0);

                /* paragraph 6: the new string is derived from the old string by
                   making replacements for each match.  If there are no matches,
                   then the new string must therefore be the old string.  */
                if (pos === -1)
                    return this;

                let end : uint = pos + searchString.length;
                return substring(0,pos) + substitute(pos, end, 0, []) + substring(end);
            }
        }
        
        /* E262-3 15.5.4.12: String.prototype.search 
           E262-4 draft proposals:static_generics
         */
        prototype function search(regexp)
            ToString(this).search(regexp);

        static function search(self, regexp)
            ToString(self).search(regexp);

        override intrinsic function search(r) : double {
            let regexp : RegExp = r instanceof RegExp ? r : new RegExp(r);
            let lim    : uint = length;

            for ( let i : uint=0 ; i < lim ; i++ )
                if (regexp.match(this, i) !== null)
                    return i;
            return -1;
        }

        /* E262-3 15.5.4.13: String.prototype.slice 
           E262-4 draft proposals:static_generics
         */
        prototype function slice(start, end)
            ToString(this).slice(start, end);

        static function slice(self, start, end)
            ToString(self).slice(start, end);

        override intrinsic function slice(s, e) : Array {
            let len   : double = length;
            let start : double = ToInteger(s);
            let end   : double = e === undefined ? len : ToInteger(e);

            let startpos = start < 0 ? Math.max(len+start,0) : Math.min(start,len);
            let endpos = end < 0 ? Math.max(len+end,0) : Math.min(end,len);
            let n = Math.max(endpos-startpos,0);

            return substring(startpos, startpos+n);
        }
        
        /* ES262-3 15.5.4.14: String.prototype.split
           E262-4 draft proposals:static_generics
        */
        prototype function split(separator, limit)
            ToString(this).split(separator, limit);

        static function split(self, separator, limit)
            ToString(self).split(separator, limit);

        override intrinsic function split(separator, limit) : Array {

            type matcher = (string,RegExp!);

            function splitMatch(R: matcher, S: string, q: uint) : [uint, [string]] {
                /* FIXME: use "switch type" when it works */
                if (R is string) {
                    let x : string = R /* cast string */;  /* FIXME: cast not implemented */
                    let r : uint = x.length;
                    if (q + r <= S.length && S.substring(q, q + r) === R)
                        return [q+r, []];
                    else
                        return null;
                }

                if (R is RegExp) {
                    let x : RegExp! = R /* cast RegExp */;  /* FIXME: cast not implemented */
                    let mr : MatchResult = x.match(S, q);
                    if (mr === null)
                        return null;
                    else
                        return [mr.endIndex, mr.cap];
                }
            }

            /* 1-6 */
            let A   : [string] = [] : [string];
            let lim : uint = limit === undefined ? 0xFFFFFFFFu : ToUint(limit);
            let s   : uint = length;
            let p   : uint = 0;
            let R   : matcher = separator instanceof RegExp ? separator : ToString(separator);
            
            /* 7 */
            if (lim === 0)
                return A;

            /* 8; 33-34 */
            if (separator === undefined) {
                A[0] = this;
                return A;
            }

            /* 9; 31-34 */
            if (s === 0) {
                let z : [string]? = splitMatch(R, this, 0);
                if (z === null)
                    A[0] = this;
                return A;
            }

            /* 10-27 */
            for ( let q : uint = p ; q !== s ; ) {
                /* 12-13; 26-27 */
                let z : [string]? = splitMatch(R, this, q);
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
                A[A.length] = substring(p, q);
                if (A.length === lim) 
                    return A;

                for ( let i : uint = 0 ; i < cap.length ; i++ ) {
                    A[A.length] = cap[i+1];
                    if (A.length === lim)
                        return A;
                }

                q = e;
            }

            /* 28-30 */
            A[A.length] = substring(p, s);
            return A;
        }

        /* E262-3 15.5.4.15: String.prototype.substring
           E262-4 draft proposals:static_generics
         */
        prototype function substring(start=0.0, end=undefined)
            ToString(this).substring(start, end == undefined ? this.length : end);

        static function substring(self, start, end)
            ToString(self).substring(start, end);

        override intrinsic function substring(start: double, end: double) : string {
            
            let len : double = length;

            start = ToInteger(start);
            end = isNaN(end) ? len : ToInteger(end);

            start = Math.min(Math.max(start, 0), len);
            end = Math.min(Math.max(end, 0), len);

            if (start > end) 
                [start, end] = [end, start];

            let s : string = "";
            let a : uint = start;
            let b : uint = end;

            for ( let i : uint = a ; i < b ; i++ )
                s += charAt(i);

            return s;
        }

        /* E262-3 15.5.4.16: String.prototype.toLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLowerCase()
            ToString(this).toLowerCase();

        static function toLowerCase(selft)
            ToString(self).toLowerCase();

        override intrinsic function toLowerCase() : string {
            let s   : string = "";
            let len : uint = length;
            for ( let i : uint = 0u ; i < len ; i++ )
                s += magic::fromCharCode(Unicode.toLowerCaseCharCode(magic::charCodeAt(this,i)));
            return s;
        }

        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleLowerCase()
            ToString(this).toLocaleLowerCase();

        prototype function toLocaleLowerCase(self)
            ToString(self).toLocaleLowerCase();

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        override intrinsic function toLocaleLowerCase() : string
            toLowerCase();

        /* E262-3 15.5.4.18: String.prototype.toUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toUpperCase()
            ToString(this).toUpperCase();

        static function toUpperCase(self)
            ToString(self).toUpperCase();

        override intrinsic function toUpperCase() : string {
            let s   : string = "";
            let len : uint = this.length;
            for ( let i : uint = 0u ; i < len ; i++ )
                s += magic::fromCharCode(Unicode.toUpperCaseCharCode(magic::charCodeAt(this,i)));
            return s;
        }

        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase 
           E262-4 draft proposals:static_generics
         */
        prototype function toLocaleUpperCase()
            ToString(this).toLocaleUpperCase();

        static function toLocaleUpperCase(self)
            ToString(self).toLocaleUpperCase();

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        override intrinsic function toLocaleUpperCase() : string
            toUpperCase();

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON() 
            this.parseJSON();

        override intrinsic function parseJSON(...args)
            JSON.parse.apply(null, args.unshift(this));

        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            this.trim();

        override intrinsic function trim() : string {
            let len  : uint = length;
            let i, j : uint;

            for ( i=0 ; i < len && Unicode.isTrimmableSpace(magic::charCodeAt(this, i)) ; i++ )
                ;
            for ( j=len-1 ; j >= i && Unicode.isTrimmableSpace(magic::charCodeAt(this, j)) ; j-- )
                ;
            return substring(i,j+1);
        }

        /* E262-3 15.5.5.1: length. */
        override function get length() : uint
            magic::stringLength(this);
    }
}
