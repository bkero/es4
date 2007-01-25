/* -*- indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "String" object
 *
 * E262-3 15.5
 * E262-4 proposals:json_encoding_and_decoding
 * E262-4 proposals:string.prototype.trim
 * E262-4 proposals:bug_fixes
 *
 * Status: incomplete; not reviewed; not tested.
 *
 * Strings are constructed by:
 *    new String + magic::setStringValue
 *    magic::fromCharCode
 *    magic::stringAppend
 *
 * A string's length is obtained by:
 *    magic::stringLength
 *
 * Strings are picked apart by:
 *    magic::charCodeAt
 *
 * (We would expect "+" to map to magic::stringAppend().)
 */

package
{
    use namespace intrinsic;
    use strict;

    /* Tentatively final non-dynamic, awaiting resolution */
    final class String extends Object
    {       
        /* E262-3 15.5.1: The String Constructor Called as a Function */
        static intrinsic function call(value) {
            if (arguments.length === 0)
                return "";
            else
                return ToString(value);
        }

        /* 15.5.2 The String Constructor 
           Be careful to always return a new String object here, so don't
           optimize by returning the return value of 
         */
        function String(value) {
            if (arguments.length === 0)
                magic::setStringValue(this,"");
            else if (value instanceof String)
                magic::setStringValue(this,value);  // Be c
            else
                magic::setStringValue(this,ToString(value));
        }
        
        /* E262-3 15.5.3.2: String.fromCharCode
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        public static function fromCharCode(...args)
            builtinFromCharCode(args);

        intrinsic static function fromCharCode(...args) : Number
            builtinFromCharCode(args);

        static function builtinFromCharCode(codes : Array) : Number {
            let s : String = "";
            let n : uint = codes.length;
            for (let i : uint = 0 ; i < n ; ++i)
                s += magic::fromCharCode(codes[i]);
            return s;
        }

        /* E262-3 15.5.4.2: String.prototype.toString */
        prototype function toString(this : String)
            this;

        intrinsic function toString() : String
            this;
        
        /* E262-3 15.5.4.3: String.prototype.valueOf */
        prototype function valueOf(this : String)
            this;

        intrinsic function valueOf() : Object
            this;

        /* E262-3 15.5.4.4: String.prototype.charAt */
        prototype function charAt(pos)
            ToString(this).charAt(pos);

        intrinsic function charAt(pos : Number = 0) : String
            let (ipos = ToInteger(pos))
                (ipos < 0 || ipos >= length) ? "" : magic::fromCharCode(magic::charCodeAt(this, ToUint(ipos)));

        /* E262-3 15.5.4.5: String.prototype.charCodeAt */
        prototype function charCodeAt(pos)
            ToString(this).charCodeAt(pos);

        intrinsic function charCodeAt(pos : Number = 0) : Number
            let (ipos = ToInteger(pos))
                (ipos < 0 || ipos >= length) ? NaN : return magic::charCodeAt(this, ToUint(ipos));

        /* E262-3 15.5.4.6: String.prototype.concat.
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
         */
        prototype function concat(...args)
            this.builtinConcat(args);

        intrinsic function concat(...args) : String
            builtinConcat(args);

        function builtinConcat(strings : Array) : String {
            let s : String = ToString(this);
            let n : uint = ToUint(strings.length);
            for (let i : uint = 0; i < n ; i++)
                s += ToString(strings[i]);
            return s;
        }

        /* E262-3 15.5.4.7: String.prototype.indexOf
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function indexOf(searchString, position)
            ToString(this).indexOf(searchString, position);

        intrinsic function indexOf(searchString : String!, position : Number = 0) : Number {
            position = ToInteger(position);

            let slen  : uint = length;
            let m     : uint = ToUint(Math.min(Math.max(position, 0), slen));
            let sslen : uint = searchString.length;
            let lim   : uint = slen - sslen + 1;

            outer:
            for ( let k : uint = m ; k < lim ; k++ ) {
                for ( let w : uint = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(this, k+w) !== magic::charCodeAt(searchString, w)) 
                        continue outer;
                }
                return k;
            }
            return -1;
        }

        /* E262-3 15.5.4.8: String.prototype.lastIndexOf
           E262-4 draft proposals:bug_fixes - FUNCTION.LENGTH
        */
        prototype function lastIndexOf(searchString, position)
            ToString(this).lastIndexOf(searchString, position);

        intrinsic function lastIndexOf(searchString : String!, position : Number) : Number {
            position = isNaN(Number) ? Infinity : ToInteger(x);

            let slen  : uint = length;
            let m     : uint = ToUint(Math.min(Math.max(position, 0), slen));
            let sslen : uint = searchString.length;

            outer:
            for ( let k : uint = m ; k >= 0 ; k-- ) {
                for ( let w : uint = 0 ; w < sslen ; w++ ) {
                    if (magic::charCodeAt(this, k+w) !== magic::charCodeAt(searchString, w)) 
                        continue outer;
                }
                return k;
            }
            return -1;
        }

        /* E262-3 15.5.4.9: String.prototype.localeCompare */
        prototype function localeCompare(that)
            ToString(this).localeCompare(that);

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        intrinsic function localeCompare(that : String!) : Number {
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

        /* E262-3 15.5.4.10: String.prototype.match */
        prototype function match(regexp)
            ToString(this).match(regexp);

        intrinsic function match(r) : Array {
            let regexp : RegExp! = r instanceof RegExp ? r : new RegExp(r);

            if (!regexp.global)
                return regexp.exec(this);  // ie, intrinsic::exec

            let matches : [String!] = [] : [String!];

            regexp.lastIndex = 0;
            while (true) {
                let oldLastIndex : Number = re.lastIndex;
                let res : [String] = re.exec(this);

                if (res === null) 
                    break;

                matches.push(res[0]);
                if (regexp.lastIndex === oldLastIndex)
                    ++regexp.lastIndex;
            }
            return matches;
        }

        /* E262-3 15.5.4.11: String.prototype.replace */
        prototype function replace(searchValue, replaceValue)
            ToString(this).replace(searchValue, replaceValue);

        intrinsic function replace(s, r) : String {

            /* paragraph 4 */
            function substituteFunction(start : uint, end : uint, m : uint, cap : Array) : String! {
                let A : Array = [];
                A[0] = this.substring(start, end);
                for ( let i : uint=0 ; i < m ; i++ )
                    A[i+1] = cap[i+1];
                A[m+2] = start;
                A[m+3] = this;
                return ToString(replaceFun.apply(null, A));
            }

            /* paragraph 5 */
            function substituteString(start : uint, end : uint, m : uint, cap : Array) : String! {
                let s   : String! = "";
                let i   : uint = 0;
                /* FIXME: use regex literal here when lexer can handle them */
                let r   : RegExp = new RegExp("\\$(?:(\\$)|(\\&)|(\\`)|(\\')|([0-9]{1,2}))","g");
                let res : Array;

                while ((res = r.exec(replaceString)) !== null) {
                    s += replaceString.substring(i, r.lastIndex - res[0].length);
                    i = r.lastIndex;

                    if (res[1])      s += "$";
                    else if (res[2]) s += substring(start, end);
                    else if (res[3]) s += substring(0, start);
                    else if (res[4]) s += substring(end);
                    else             let c : int = parseInt(res[5])+1 { if (c in cap) s += cap[c]; }
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

            let replaceString : String = r instanceof String ? r cast String : null;
            let replaceFun    : Function = r instanceof Function ? r cast Function : null;

            let substitute : function (uint, uint, uint, Array) : String! =
                replaceFun !== null ? substituteFunction : substituteString;

            if (s instanceof RegExp) {
                /* paragraph 2 */
                let regexp : RegExp = s cast RegExp;
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
                    let s : String! = "";

                    while (true) {
                        let oldi : uint = i;
                        let [i, res] : [uint, [String]] = match(i);

                        if (res === null)
                            return s + substring(oldi);

                        ...;
                    }
                }
            }
            else {
                /* paragraph 3 */
                let searchString : String! = ToString(s);
                let pos : Number = indexOf(searchString, 0);

                /* paragraph 6: the new string is derived from the old string by
                   making replacements for each match.  If there are no matches,
                   then the new string must therefore be the old string.  */
                if (pos === -1)
                    return this;

                let end : uint = pos + searchString.length;
                return substring(0,pos) + substitute(pos, end, 0, []) + substring(end);
            }
        }
        
        /* E262-3 15.5.4.12: String.prototype.search */
        prototype function search(regexp)
            ToString(this).search(regexp);

        intrinsic function search(r) : Number {
            let regexp : RegExp = r instanceof RegExp ? r : new RegExp(r);
            let lim    : uint = length;

            for ( let i : uint=0 ; i < lim ; i++ )
                if (regexp.match(this, i) !== null)
                    return i;
            return -1;
        }

        /* E262-3 15.5.4.13: String.prototype.slice */
        prototype function slice(start, end)
            ToString(this).slice(start, end);

        intrinsic function slice(s, e) : Array
        {
            let len   : double = ToDouble(length);
            let start : double = ToInteger(s);
            let end   : double = e === undefined ? len : ToInteger(e);

            let startpos = start < 0 ? Math.max(len+start,0) : Math.min(start,len);
            let endpos = end < 0 ? Math.max(len+end,0) : Math.min(end,len);
            let n = Math.max(endpos-startpos,0);

            return substring(startpos, startpos+n);
        }
        
        /* ES262-3 15.5.4.14: String.prototype.split */
        prototype function split(separator, limit)
            ToString(this).split(separator, limit);

        intrinsic function split(separator, limit) : Array {

            type matcher = (String!,RegExp!);

            function splitMatch(R : matcher, S : String!, q : uint) : [uint, [String!]] {
                switch type (R : matcher) {
                case (x : String!) : {
                    let r : uint = x.length;
                    if (q + r <= S.length && S.substring(q, q + r) === R)
                        return [q+r, []];
                    else
                        return null;
                }

                case (x : RegExp!) : {
                    let mr : MatchResult = x.match(S, q);
                    if (mr === null)
                        return null;
                    else
                        return [mr.endIndex, mr.cap];
                }
                }
            }

            /* 1-6 */
            let A   : [String!] = [] : [String!];
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
                let z : [String!]? = splitMatch(R, this, 0);
                if (z === null)
                    A[0] = this;
                return A;
            }

            /* 10-27 */
            for ( let q : uint = p ; q !== s ; ) {
                /* 12-13; 26-27 */
                let z : [String!]? = splitMatch(R, this, q);
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

        /* E262-3 15.5.4.15: String.prototype.substring */
        prototype function substring (start, end)
            ToString(this).substring(ToNumber(start), ToNumber(end));

        intrinsic function substring(start : Number, end : Number) : String! {
            let len : Number = length;

            start = ToInteger(start);
            end = isNaN(end) ? len : ToInteger(end);

            start = Math.min(Math.max(start, 0), len);
            end = Math.min(Math.max(end, 0), len);

            if (start > end) 
                [start, end] = [end, start];

            let s : String! = "";
            let a : uint = ToUint(start);
            let b : uint = ToUint(end);

            for ( let i : uint = a ; i < b ; i++ )
                s += charCodeAt(i);

            return s;
        }

        /* E262-3 15.5.4.16: String.prototype.toLowerCase */
        prototype function toLowerCase()
            ToString(this).toLowerCase();

        intrinsic function toLowerCase() : String! {
            let s   : String! = "";
            let len : uint = length;
            for ( let i : uint = 0 ; i < len ; i++ )
                s += magic::fromCharCode(Unicode.toLowerCaseCharCode(magic::charCodeAt(this,i)));
            return s;
        }

        /* E262-3 15.5.4.17: String.prototype.toLocaleLowerCase */
        prototype function toLocaleLowerCase()
            ToString(this).toLocaleLowerCase();

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        intrinsic function toLocaleLowerCase() : String!
            toLowerCase();

        /* E262-3 15.5.4.18: String.prototype.toUpperCase */
        prototype function toUpperCase()
            ToString(this).toUpperCase();

        intrinsic function toUpperCase() : String! {
            let s   : String! = "";
            let len : uint = this.length;
            for ( let i : uint = 0 ; i < len ; i++ )
                s += magic::fromCharCode(Unicode.toUpperCaseCharCode(magic::charCodeAt(this,i)));
            return s;
        }

        /* E262-3 15.5.4.19: String.prototype.toLocaleUpperCase */
        prototype function toLocaleUpperCase()
            ToString(this).toLocaleUpperCase();

        /* INFORMATIVE - this is correct for a "simple" locale, eg English */
        intrinsic function toLocaleUpperCase() : String!
            toUpperCase();

        /* E262-4 draft proposals:json_encoding_and_decoding */
        prototype function parseJSON() 
            this.parseJSON();

        intrinsic function parseJSON(...args)
            JSON.parse.apply(null, args.unshift(this));

        /* E262-4 draft proposals:string.prototype.trim */
        prototype function trim()
            this.trim();

        intrinsic function trim() : String! {
            let len  : uint = length;
            let i, j : uint;

            for ( i=0 ; i < len && Unicode.isTrimmableSpace(magic::charCodeAt(i)) ; i++ )
                ;
            for ( j=len-1 ; j >= i && Unicode.isTrimmableSpace(magic::charCodeAt(j)) ; j-- )
                ;
            return substring(i,j+1);
        }

        /* E262-3 15.5.5.1: length */
        public function get length() : uint
            magic::stringLength(this);
    }
}
