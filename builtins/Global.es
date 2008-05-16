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

    // 15.1.1.1 NaN
    // 15.1.1.2 Infinity
    // 15.1.1.3 undefined
    // [proposals:bug fixes] - [IMMUTABLE.GLOBALS] says that these three names
    // are {DE,DD,RO}, and not just {DE,DD} as in E262-3.
    public const NaN = 0.0/0.0;
    public const Infinity = 1.0/0.0;
    public const undefined = void(0);

    /* E262-4 [proposals:versioning] says that __ECMASCRIPT_VERSION__ is always defined */
    public const __ECMASCRIPT_VERSION__ = 4;

    /* Ticket 152: This is __ES4__, not intrinsic */
    __ES4__ const global = this;

    __ES4__ namespace iterator;

    __ES4__ type EnumerableId = (double|string|Name);
    __ES4__ type EnumerableIdArray = Array; // FIXME: [...EnumerableId]

    helper function toEnumerableId(x) {
        switch type (x) {
        case (x: EnumerableId) { return x; }
        case (x: *)            { return string(x); }
        }
    }

    iterator type Iterator = { // FIXME: .<T>
        next: function () : * // FIXME: should be return type T
    };

    iterator class Enumerator { // FIXME: .<T>
        type ResultFun = function(EnumerableId, Object!) : *; // FIXME: T

        function Enumerator(v, f: ResultFun, e: boolean = false)
            : result_fun = f, enumerate = e
        {
            initial_obj = (v is Object) ? v : null;
            current_obj = initial_obj;
            current_ids = helper::getEnumerableIds(initial_obj);
        }

        // FIXME: why no ResultFun, boolean args?
        meta static function invoke(v) : iterator::Iterator
            new iterator::Enumerator(v);

        iterator function get(e : boolean = false) : iterator::Iterator
            (e == enumerate) ? this : new iterator::Enumerator(initial_obj, result_fun, e); // FIXME: .<T>

        public function next() : * { // FIXME: T
            if (current_obj === null)
                throw iterator::StopIteration;

        loop:
            while (true) {
                while (current_index === current_ids.length) {
                    if (!enumerate)
                        throw iterator::StopIteration;

                    // No more properties in current_obj: try walking up the prototype chain.
                    current_obj = helper::getPrototype(current_obj);
                    if (current_obj === null)
                        throw iterator::StopIteration;

                    current_ids = helper::getEnumerableIds(current_obj);
                    current_index = 0;
                }

                let id : EnumerableId = current_ids[current_index++];

                // Check for a shadowing property from initial_obj to current_obj on the prototype chain.
                for (let obj : Object = initial_obj; obj !== current_obj; obj = helper::getPrototype(obj)) {
                    if (helper::hasOwnProperty(obj, id))
                        continue loop;
                }

                // Check whether name is still bound in order to skip deleted properties.
                if (helper::hasOwnProperty(current_obj, id))
                    return result_fun(id, initial_obj);
            }
        }

        private var initial_obj   : Object,
                    current_obj   : Object,
                    current_ids   : EnumerableIdArray,
                    current_index : double,
                    result_fun    : ResultFun,
                    enumerate     : boolean;
    }

    iterator const function GET(start: Object!, deep: boolean): iterator::Iterator
        (start is iterator::Iterable) // FIXME: like
        ? start.iterator::get(deep)
        : iterator::DEFAULT_GET(start, deep)

    iterator const function DEFAULT_GET(start: Object!, deep: boolean): iterator::Iterator // FIXME: .<string>
        new iterator::Enumerator(start,
                                 function (id: EnumerableId, obj: Object!): string
                                     (id is Name) ? id : string(id), // FIXME: id.identifier
                                 deep)

    iterator type Iterable = { // FIXME: .<T>
        iterator::get: function (boolean=): iterator::Iterator // FIXME: .<T>
    };
      
    iterator class StopIterationClass {
        function toString() : string 
            "[object StopIteration]";
    }

    iterator const StopIteration: iterator::StopIterationClass = new iterator::StopIterationClass;

    iterator interface Generator {
        function next();
        function send(i);
        // FIXME: needs to be called `throw' but the parser can't handle this right now
        function throw_(e);
        function close() : void;
    }

    helper class GeneratorImpl implements iterator::Generator {
        public function next()
            this.send(undefined)

        public function send(i)
            helper::genSend(this, i)

        public function throw_(e)
            helper::genThrow(this, e)

        public function close() : void
            helper::genClose(this)

        // FIXME: this gets trumped by Mach.primitiveToUstring
        //public function toString()
        //    "[object Generator]"
    }

    // 15.1.2.1 eval (x)
    //
    // FIXME: This should probably be an intrinsic::eval that looks
    // like public::eval below but which delegates to helper::eval,
    // passing "this function" as the object from which to extract the
    // scope chain.

    intrinsic native function eval(s: string);

    public function eval(x) {
        if (this !== global)
            throw EvalError();
        if (!(x is AnyString))
            return x;
        return global.intrinsic::eval(string(x));
    }

    /* 15.1.2.2 parseInt (string , radix)
     *
     * The committee agreed in its 2007-06-05 phone conference
     * not to allow a leading '0' to force a non-supplied radix
     * to 8, but instead to default to radix 10 in all cases
     * except when the string starts with '0x' or '0X'.
     *
     * FIXME: use slice syntax here instead of intrinsic::substring,
     * when it's supported in the RI.
     */
    intrinsic const function parseInt(s: string, r: double=0): AnyNumber {
        let i;

        for ( i=0 ; i < s.length && helper::isTrimmableSpace(s[i]) ; i++ )
            ;
        s = s.intrinsic::substring(i);

        let sign = 1;
        if (s.length >= 1 && s[0] == '-')
            sign = -1;
        if (s.length >= 1 && (s[0] == '-' || s[0] == '+'))
            s = s.intrinsic::substring(1);

        let maybe_hexadecimal = false;
        r = intrinsic::toInt(r);
        if (r == 0) {
            r = 10;
            maybe_hexadecimal = true;
        }
        else if (r == 16)
            maybe_hexadecimal = true;
        else if (r < 2 || r > 36)
            return NaN;

        if (maybe_hexadecimal && 
            s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            r = 16;
            s = s.intrinsic::substring(2);
        }

        for ( i=0 ; i < s.length && helper::isDigitForRadix(s[i], r) ; i++ )
            ;
        s = s.intrinsic::substring(0,i);

        if (s == "")
            return NaN;

        return sign * informative::numericValue(s, r);
    }

    helper function isDigitForRadix(c, r) {
        c = c.intrinsic::toUpperCase();
        if (c >= '0' && c <= '9')
            return (c.intrinsic::charCodeAt(0) - '0'.intrinsic::charCodeAt(0)) < r;
        if (c >= 'A' && c <= 'Z')
            return (c.intrinsic::charCodeAt(0) - 'A'.intrinsic::charCodeAt(0) + 10) < r;
        return false;
    }

    /* E262-3 section 15.1.2.2: INFORMATIVE.  If more than 20
     * significant digits are present, an implementation is
     * allowed to do better than this.
     */
    informative function numericValue(s, r) {

        function digitValue(c) {
            c = c.intrinsic::toUpperCase();
            if (c >= '0' && c <= '9')
                return c.intrinsic::charCodeAt(0) - '0'.intrinsic::charCodeAt(0);
            else
                return c.intrinsic::charCodeAt(0) - 'A'.intrinsic::charCodeAt(0) + 10;
        }
        
        let val = 0;
        for ( let i=0 ; i < s.length ; i++ )
            val = val * r + digitValue(s[i]);
        return val;
    }

    public function parseInt(s, r=0)
        intrinsic::parseInt(string(s), double(r));


    // 15.1.2.3 parseFloat (string)
    intrinsic native function parseFloat(s: string);

    /*  No reason most of this has to be native.
    intrinsic const function parseFloat(s: string) {
        // Implement this according to E262-3 
    }
    */

    public function parseFloat(s)
        intrinsic::parseFloat(string(s));


    // 15.1.2.4 isNaN (v)
    // parens to avoid interpretation "boolean!"
    intrinsic const function isNaN(n: AnyNumber): boolean
        (!(n === n));

    public function isNaN(x)
        intrinsic::isNaN(Number(x));


    // 15.1.2.5 isFinite (number)
    // parens to avoid interpretation "boolean!"
    intrinsic const function isFinite(n: AnyNumber): boolean
        (!intrinsic::isNaN(n) && n != -Infinity && n != Infinity);

    public function isFinite(x)
        intrinsic::isFinite(Number(x));

    intrinsic const function isIntegral(v:AnyNumber): boolean
        intrinsic::isFinite(v) && Math.intrinsic::floor(v) == v;

    __ES4__ function isIntegral(v)
        intrinsic::isIntegral(Number(v));

    // Returns x with the sign of y
    intrinsic const function copysign(x:AnyNumber, y:AnyNumber): AnyNumber {
        if (intrinsic::isNaN(x)) 
            return x;

        let s = intrinsic::sign(y);
        if (x < 0) {
            if (s < 0)
                return x;
            else 
                return -x;
        }
        else if (x > 0) {
            if (s < 0)
                return -x;
            else
                return x;
        }
        else {
            if (s < 0)
                return -0.0;
            else
                return +0.0;
        }
    }

    __ES4__ function copysign(x,y)
        intrinsic::copysign(Number(x), Number(y));

    // Returns -1 for negative, 1 for positive, 0 for nan
    intrinsic const function sign(x:AnyNumber): double {
        if (intrinsic::isNaN(x))
            return 0;
        if (x < 0)
            return -1;
        if (x > 0)
            return 1;
        if (1/x < 0)
            return -1;
        return 1;
    }

    __ES4__ function sign(x)
        intrinsic::sign(Number(x));
        
    intrinsic const function isInt(n:AnyNumber) : boolean
        intrinsic::isIntegral(n) && n >= -0x7FFFFFFF && n <= 0x7FFFFFFF;

    __ES4__ function isInt(x)
        intrinsic::isInt(Number(x));

    intrinsic const function isUint(n:AnyNumber) : boolean
        intrinsic::isIntegral(n) && n >= 0 && n <= 0xFFFFFFFF;

    __ES4__ function isUint(x)
        intrinsic::isUint(Number(x));

    intrinsic const function toInt(n:AnyNumber) : double
        n | 0;

    __ES4__ function toInt(x)
        intrinsic::toInt(Number(x));

    intrinsic const function toUint(n:AnyNumber) : double
        n >>> 0;

    __ES4__ function toUint(x) : boolean
        intrinsic::toUint(Number(x));

    helper function isIndex(k): boolean
        intrinsic::isUint(k) && k != 0xFFFFFFFF;
    
    // Note, this rounds toward zero
    helper function toInteger(value): AnyNumber {
        value = Number(value);
        if (intrinsic::isNaN(value))
            return 0;
        if (value === 0 || !intrinsic::isFinite(value))
            return value;
        return intrinsic::sign(value) * Math.intrinsic::floor(Math.intrinsic::abs(value));
    }

    // Return a hash code for the string.
    //
    // INFORMATIVE: this particular algorithm is not mandated and
    // may or may not be suitable.

    informative function stringHash(str: AnyString): double {
        let s = string(str);
        let h = 0;
        for ( let i=0 ; i < s.length ; i++ )
            h = (h << 4) + (let (c = s.intrinsic::charCodeAt(i)) c*3);
        return h;
    }

    informative function namespaceHash(x)
        informativ::stringHash(x);

    informative function nameHash(x)
        informativ::stringHash(x);

    // Return a hash code for the object.

    informative native function objectHash(x:Object!): double;

    intrinsic const function hashcode(o): double {
        switch type (o) {
        case (x: null)       { return 0 }
        case (x: undefined)  { return 0 }
        case (x: AnyBoolean) { return Number(x) }
        case (x: AnyNumber)  { return intrinsic::toUint(x) }
        case (x: AnyString)  { return informative::stringHash(x) }
        case (x: Namespace)  { return informative::namespaceHash(x) }
        case (x: Name)       { return informative::nameHash(x) }
        case (x: *)          { return informative::objectHash(x) }
        }
    }


    /* URI encoding and decoding. */

    helper function toUTF8(v) {
        if (v <= 0x7F)
            return [v];
        if (v <= 0x7FF)
            return [0xC0 | ((v >> 6) & 0x3F), 
                    0x80 | (v & 0x3F)];
        if (v <= 0xD7FF || v >= 0xE000 && v <= 0xFFFF)
            return [0xE0 | ((v >> 12) & 0x0F),
                    0x80 | ((v >> 6) & 0x3F),
                    0x80 | (v & 0x3F)];
        if (v >= 0x10000)
            return [0xF0 | ((v >> 18) & 0x07),
                    0x80 | ((v >> 12) & 0x3F),
                    0x80 | ((v >> 6) & 0x3F),
                    0x80 | (v & 0x3F)];
        throw URIError(/* Unconvertible code */);
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

            if (unescapedSet.intrinsic::indexOf(C) != -1) {
                R = R + C;
                k = k + 1;
                continue;
            }

            let V = C.intrinsic::charCodeAt(0);
            if (V >= 0xDC00 && V <= 0xDFFF)
                throw new URIError(/* Invalid code */);
            if (V >= 0xD800 && V <= 0xDBFF) {
                k = k + 1;
                if (k == s.length)
                    throw new URIError(/* Truncated code */);
                let V2 = s[k].intrinsic::charCodeAt(0);
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
            (!helper::isDigitForRadix(s[k+1], 16) && 
             !helper::isDigitForRadix(s[k+1], 16)))
            throw new URIError(/* Invalid escape sequence */);
        return intrinsic::parseInt(s.intrinsic::substring(k+1, k+3), 16);
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
                let C = string.intrinsic::fromCharCode(B);
                if (reservedSet.intrinsic::indexOf(C) != -1)
                    R = R + s.intrinsic::substring(start, k);
                else
                    R = R + C;
                continue;
            }

            let n = 1;
            while (((B << n) & 0x80) == 1)
                ++n;
            if (n == 1 || n > 4)
                throw new URIError(/* Invalid encoded character */);

            let octets = [B];
            for ( let j=1 ; j < n ; ++j ) {
                let B = helper::decodeHexEscape(s, k);
                if ((B & 0xC0) != 0x80)
                    throw new URIError(/* Invalid encoded character */);
                k = k + 3;
                octets.intrinsic::push(B);
            }
            let V = helper::fromUTF8(octets);
            if (V > 0x10FFFF)
                throw new URIError(/* Invalid Unicode code point */);
            if (V > 0xFFFF) {
                L = ((V - 0x10000) & 0x3FF) + 0xD800;
                H = (((V - 0x10000) >> 10) & 0x3FF) + 0xD800;
                R = R + string.intrinsic::fromCharCode(H, L);
            }
            else {
                let C = string.intrinsic::fromCharCode(V);
                if (reservedSet.intrinsic::indexOf(C))
                    R = R + s.intrinsic::substring(start, k);
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
    intrinsic const function decodeURI(encodedURI: string)
        helper::decode(encodedURI, helper::uriReserved + "#");

    public function decodeURI(encodedURI)
        intrinsic::decodeURI(string(encodedURI));


    /* E262-3 15.1.3.2 decodeURIComponent (encodedURIComponent) */
    intrinsic const function decodeURIComponent(encodedURIComponent)
        helper::decode(encodedURIComponent, "");

    public function decodeURIComponent(encodedURIComponent)
        intrinsic::decodeURIComponent(string(encodedURIComponent));


    /* E262-3 15.1.3.3 encodeURI (uri) */
    intrinsic const function encodeURI(uri: string): string
        helper::encode(uri, helper::uriReserved + helper::uriUnescaped + "#")

    public function encodeURI(uri)
        intrinsic::encodeURI(string(uri));


    /* E262-3 15.1.3.4 encodeURIComponent (uriComponent) */
    intrinsic const function encodeURIComponent(uriComponent: string): string
        helper::encode(uri, helper::uriReserved);

    public function encodeURIComponent(uriComponent)
        intrinsic::encodeURIComponent(string(uriComponent));


    // The non-virtual property get/set helpers.
    intrinsic native function get(obj:Object!, name:string) : *;
    intrinsic native function set(obj:Object!, name:string, val:*) : void;

    __ES4__ class DefaultBehaviorClass { }
    __ES4__ const DefaultBehavior = new DefaultBehaviorClass();