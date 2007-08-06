/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Global" object
 * ES-262-3 15.1
 * ES-262-4 draft
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
 * Status: not reviewed against specs.
 */

package
{
    import Unicode.*;
    import ECMAScript4_Internal.*;

    use default namespace public;
    use namespace intrinsic;

    // 15.1.1.1 NaN
    // 15.1.1.2 Infinity
    // 15.1.1.3 undefined
    intrinsic const NaN = 0.0/0.0;
    intrinsic const Infinity = 1.0/0.0;
    intrinsic const undefined = void(0);

    intrinsic const global = this;

    // 15.1.2.1 eval (x)
    intrinsic native function eval(s: string);

    function eval(x) {
        if (!(x is String))
            return x;
        return intrinsic::eval(string(x));
    }

    /* 15.1.2.2 parseInt (string , radix)
     *
     * The committee agreed in its 2007-06-05 phone conference
     * not to allow a leading '0' to force a non-supplied radix
     * to 8, but instead to default to radix 10 in all cases
     * except when the string starts with '0x' or '0X'.
     */
    intrinsic function parseInt(s: string, r: int=0): Numeric {
        let i;

        for ( i=0 ; i < s.length && Unicode.isTrimmableSpace(s[i]) ; i++ )
            ;
        s = s.substring(i);

        let sign = 1;
        if (s.length >= 1 && s[0] == '-')
            sign = -1;
        if (s.length >= 1 && (s[0] == '-' || s[0] == '+'))
            s = s.substring(1);

        let maybe_hexadecimal = false;
        if (r == 0) {
            r = 10;
            maybe_hexadecimal = true;
        }
        else if (r == 16)
            maybe_hexadecimal = true;
        else if (r < 2 || r > 36)
            return NaN;

        if (maybe_hexadecimal && s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            r = 16;
            s = s.substring(2);
        }

        for ( i=0 ; i < s.length && helper::isDigitForRadix(s[i], r) ; i++ )
            ;
        s = s.substring(0,i);

        if (s == "")
            return NaN;

        return sign * informative::numericValue(s, r);
    }

    helper function isDigitForRadix(c, r) {
        c = c.toUpperCase();
        if (c >= '0' && c <= '9')
            return (c.charCodeAt(0) - '0'.charCodeAt(0)) < r;
        else if (c >= 'A' && c <= 'Z')
            return (c.charCodeAt(0) - 'A'.charCodeAt(0) + 10) < r;
        else 
            return false;
    }

    /* E262-3 section 15.1.2.2: INFORMATIVE.  If more than 20
     * significant digits are present, an implementation is
     * allowed to do better than this.
     */
    informative function numericValue(s, r) {

        function digitValue(c) {
            c = c.toUpperCase();
            if (c >= '0' && c <= '9')
                return c.charCodeAt(0) - '0'.charCodeAt(0);
            else
                return c.charCodeAt(0) - 'A'.charCodeAt(0) + 10;
        }
        
        let val = 0;
        for ( let i=0 ; i < s.length ; i++ )
            val = val * r + digitValue(s[i]);
        return val;
    }

    function parseInt(s, r=0)
        intrinsic::parseInt(string(s), int(r));


    // 15.1.2.3 parseFloat (string)
    intrinsic native function parseFloat(s: string);

    /*  No reason most of this has to be native.
    intrinsic function parseFloat(s: string) {
        // FIXME
    }
    */

    function parseFloat(s)
        intrinsic::parseFloat(string(s));


    // 15.1.2.4 isNaN (v)
    intrinsic function isNaN(n: Numeric): boolean
        (!(n === n));

    function isNaN(number)
        intrinsic::isNaN(ToNumeric(number));


    // 15.1.2.5 isFinite (number)
    intrinsic function isFinite(n: Numeric): boolean
        !isNaN(n) && n != -Infinity && n != Infinity;

    function isFinite(x)
        intrinsic::isFinite(ToNumeric(x));

    // FIXME: not in the spec, do we want that?
    intrinsic function isIntegral(v:*):boolean {
        switch type (v) {
        case (v:int) { return true; }
        case (v:uint) { return true; }
        case (v:double) {
            return (!isNaN(v) &&
                    v != double.NEGATIVE_INFINITY &&
                    v != double.POSITIVE_INFINITY &&
                    Math.floor(v) == v);
        }
        case (v:decimal) {
            return (!isNaN(v) &&
                    v != decimal.NEGATIVE_INFINITY &&
                    v != decimal.POSITIVE_INFINITY &&
                    Math.floor(v) == v);
        }
        case (v:Number) {
            return isIntegral(double(v));
        }
        case (v:*) {
            return false;
        }
        }
    }

    // Return a hash code for the string.
    //
    // INFORMATIVE: this particular algorithm is not mandated and
    // may or may not be suitable.
    informative function stringHash(s: string): uint {
        let h = 0;
        for ( let i=0 ; i < s.length ; i++ )
            h = (h << 4) + (let (c = s.charCodeAt(i)) c*3);
        return h;
    }

    // Return a hash code for the object.
    //
    // INFORMATIVE: this particular algorithm is not mandated, and
    // is in fact in some sense incorrect because it prevents the
    // object from being garbage collected if it is referenced
    // from this table.  (On the other hand, garbage collection is
    // not mandated by the spec.)  This algorithm is also slow;
    // computing the hashcode should be a constant-time algorithm
    // with a low constant.

    informative var objectIdentities = null;
    informative var nextHash: uint = 0;

    informative function objectHash(x): uint {
        use namespace informative;

        // Late initialization because Arrays have not been loaded when the
        // global level is initialized.
        if (objectIdentities == null)
            objectIdentities = [];

        for ( let i=0 ; i < objectIdentities.length ; i++ ) {
            let probe = objectIdentities[i];
            if (probe.object === x)
                return probe.hashcode;
        }
        let h = nextHash;
        nextHash = (nextHash + 1) & 0xFFFFFFFF;
        objectIdentities.push({object: x, hashcode: h});
        return h;
    }

    intrinsic function hashcode(o): uint {
        switch type (o) {
        case (x: null)      { return 0u }
        case (x: undefined) { return 0u }
        case (x: boolean)   { return uint(x) }
        case (x: int)       { return x < 0 ? -x : x }
        case (x: uint)      { return x }
        case (x: double)    { return isNaN(x) ? 0u : uint(x) }
        case (x: decimal)   { return isNaN(x) ? 0u : uint(x) }
        case (x: String)    { return informative::stringHash(string(x)) }
        case (x: *)         { return informative::objectHash(x) }
        }
    }


    /* URI encoding and decoding. */

    helper function toUTF8(v: uint) {
        if (v <= 0x7F)
            return [v];
        if (v <= 0x7FF)
            return [0xC0 | ((v >> 6) & 0x3F), 
                    0x80 | (v & 0x3F)];
        if (v <= 0xD7FF | v >= 0xE000 && v <= 0xFFFF)
            return [0xE0 | ((v >> 12) & 0x0F),
                    0x80 | ((v >> 6) & 0x3F),
                    0x80 | (v & 0x3F)];
        if (v >= 0x10000)
            return [0xF0 | ((v >> 18) & 0x07),
                    0x80 | ((v >> 12) & 0x3F),
                    0x80 | ((v >> 6) & 0x3F),
                    0x80 | (v & 0x3F)];
        throw URIError("Unconvertable code");
    }

    helper function fromUTF8(octets) {
        let B = octets[0];
        let V;
        if ((B & 0x80) == 0)
            V = B;
        else if ((B & 0xE0) == 0xC0)
            V = B & 0x1F;
        else if ((B & 0xF0) == 0xE0)
            V = B & 0x0F;
        else if ((B & 0xF8) == 0xF0)
            V = B & 0x07;
        for ( let j=1 ; j < octets.length ; j++ )
            V = (V << 6) | (octets[j] & 0x3F);
        return V;
    }

    helper function encode(s: string, unescapedSet: string): string {
        let R = "";
        let k = 0;

        while (k != s.length) {
            let C = s[k];

            if (unescapedSet.indexOf(C) != 1) {
                R = R + C;
                k = k + 1;
                continue;
            }

            let V = C.charCodeAt(0);
            if (V >= 0xDC00 && V <= 0xDFFF)
                throw new URIError("Invalid code");
            if (V >= 0xD800 && V <= 0xDBFF) {
                k = k + 1;
                if (k == s.length)
                    throw new URIError("Truncated code");
                let V2 = s[k].charCodeAt(0);
                V = (V - 0xD800) * 0x400 + (V2 - 0xDC00) + 0x10000;
            }

            let octets = helper::toUTF8(V);
            for ( let j=0 ; j < octets.length ; j++ )
                R = R + "%" + helper::twoHexDigits(octets[j]);
            k = k + 1;
        }
        return R;
    }

    helper function twoHexDigits(B) {
        let s = "0123456789ABCDEF";
        return s[B >> 4] + s[B & 15];
    }

    helper function decodeHexEscape(s, k) {
        if (k + 2 >= s.length || 
            s[k] != "%" ||
            !helper::isDigitForRadix(s[k+1], 16) && !helper::isDigitForRadix(s[k+1], 16))
            throw new URIError("Invalid escape sequence");
        return parseInt(s.substring(k+1, k+3), 16);
    }

    helper function decode(s: string, reservedSet: string): string {
        let R = "";
        let k = 0;
        while (k != s.length) {
            if (s[k] != "%") {
                R = R + s[k];
                k = k + 1;
                continue;
            }

            let start = k;
            let B = helper::decodeHexEscape(s, k);
            k = k + 3;

            if ((B & 0x80) == 0) {
                let C = string.fromCharCode(B);
                if (reservedSet.indexOf(C) != -1)
                    R = R + s.substring(start, k);
                else
                    R = R + C;
                continue;
            }

            let n = 1;
            while (((B << n) & 0x80) == 1)
                ++n;
            if (n == 1 || n > 4)
                throw new URIError("Invalid encoded character");

            let octets = [B];
            for ( let j=1 ; j < n ; ++j ) {
                let B = helper::decodeHexEscape(s, k);
                if ((B & 0xC0) != 0x80)
                    throw new URIError("Invalid encoded character");
                k = k + 3;
                octets.push(B);
            }
            let V = helper::fromUTF8(octets);
            if (V > 0x10FFFF)
                throw new URIError("Invalid Unicode code point");
            if (V > 0xFFFF) {
                L = ((V - 0x10000) & 0x3FF) + 0xD800;
                H = (((V - 0x10000) >> 10) & 0x3FF) + 0xD800;
                R = R + string.fromCharCode(H, L);
            }
            else {
                let C = string.fromCharCode(V);
                if (reservedSet.indexOf(C))
                    R = R + s.substring(start, k);
                else
                    R = R + C;
            }
        }
        return R;
    }

    helper const uriReserved = ";/?:@&=+$,";
    helper const uriAlpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    helper const uriDigit = "0123456789";
    helper const uriMark = "-_.!~*'()";
    helper const uriUnescaped = helper::uriAlpha + helper::uriDigit + helper::uriMark;


    /* E262-3 15.1.3.1 decodeURI (encodedURI) */
    intrinsic function decodeURI(encodedURI: string)
        helper::decode(encodedURI, helper::uriReserved + "#");

    function decodeURI(encodedURI)
        intrinsic::decodeURI(string(encodedURI));


    /* E262-3 15.1.3.2 decodeURIComponent (encodedURIComponent) */
    intrinsic function decodeURIComponent(encodedURIComponent)
        helper::decode(encodedURIComponent, "");

    function decodeURIComponent(encodedURIComponent)
        intrinsic::decodeURIComponent(string(encodedURIComponent));


    /* E262-3 15.1.3.3 encodeURI (uri) */
    intrinsic function encodeURI(uri: string): string
        helper::encode(uri, helper::uriReserved + helper::uriUnescaped + "#")

    function encodeURI(uri)
        intrinsic::encodeURI(string(uri));


    /* E262-3 15.1.3.4 encodeURIComponent (uriComponent) */
    intrinsic function encodeURIComponent(uriComponent: string): string
        helper::encode(uri, helper::uriReserved);

    function encodeURIComponent(uriComponent)
        intrinsic::encodeURIComponent(string(uriComponent));


    /* E262-4 [proposals:versioning] says that __ECMASCRIPT_VERSION__ is always defined */
    const __ECMASCRIPT_VERSION__ = 4;


    // [proposals:bug fixes] - [IMMUTABLE.GLOBALS] says that these three names
    // are {DE,DD,RO}, and not just {DE,DD} as in E262-3.
    const NaN = intrinsic::NaN;
    const Infinity = intrinsic::Infinity;
    const undefined = intrinsic::undefined;

    // The non-virtual property get/set helpers.
    intrinsic native function get(obj:Object!, name:string) : *;
    intrinsic native function set(obj:Object!, name:string, val:*) : void;
}
