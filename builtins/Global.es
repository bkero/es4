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
    use default namespace public;
    use namespace intrinsic;

    // 15.1.1.1 NaN
    // 15.1.1.2 Infinity
    // 15.1.1.3 undefined
    intrinsic const NaN = 0.0/0.0;
    intrinsic const Infinity = 1.0/0.0;
    intrinsic const undefined = void(0);
    
    // @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
    

    // 15.1.2.1 eval (x)
    intrinsic native function eval(x);

    /* 15.1.2.2 parseInt (string , radix)
     *
     * The committee agreed in its 2007-06-05 phone conference
     * not to allow a leading '0' to force a non-supplied radix
     * to 8, but instead to default to radix 10 in all cases
     * except when the string starts with '0x' or '0X'.
     */
    intrinsic function parseInt(s, radix=undefined) {

        function digitValue(c) {
            if (c >= '0' && c <= '9')
                return c.charCodeAt(0) - '0'.charCodeAt(0);
            if (c >= 'A' && c <= 'Z')
                return c.charCodeAt(0) - 'A'.charCodeAt(0) + 10;
            if (c >= 'a' && c <= 'z')
                return c.charCodeAt(0) - 'a'.charCodeAt(0) + 10;
            return -1;
        }

        function isDigitForRadix(c, r) {
            let v = digitValue(c);
            return v != -1 && v < r;
        }

        /* E262-3 section 15.1.2.2: INFORMATIVE.  If more than 20
         * significant digits are present, an implementation is
         * allowed to do better than this.
         */
        function numericValue(s, r) {
            let val = 0;
            for ( let i=0 ; i < s.length ; i++ )
                val = val * r + digitValue(s[i]);
            return val;
        }

        let i;

        s = ToString(s);
        for ( i=0 ; i < s.length && Unicode.isTrimmableSpace(s[i]) ; i++ )
            ;
        s = s.substring(i);

        let sign = 1;
        if (s.length >= 1 && s[0] == '-')
            sign = -1;
        if (s.length >= 1 && (s[0] == '-' || s[0] == '+'))
            s = s.substring(1);

        let r = int(radix);
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

        for ( i=0 ; i < s.length && isDigitForRadix(s[i], r) ; i++ )
            ;
        s = s.substring(0,i);

        if (s == "")
            return NaN;

        return sign * numericValue(s, r);
    }

    // 15.1.2.3 parseFloat (string)
    intrinsic native function parseFloat(str:string);

    // 15.1.2.4 isNaN (v)
    intrinsic function isNaN(v:*):boolean
        (!(v === v));

    // 15.1.2.5 isFinite (number)
    intrinsic function isFinite(v:*):boolean 
    {
        let const n = intrinsic::ToDouble(v)
        return n != Number.NaN && 
               n != Number.NEGATIVE_INFINITY && 
               n != Number.POSTITVE_INFINITY 
    }
    
    
    // 15.1.3.1 decodeURI (encodedURI)
    intrinsic native function decodeURI(encodedURI);

    // 15.1.3.2 decodeURIComponent (encodedURIComponent)
    intrinsic native function decodeURIComponent(encodedURIComponent);
    
    // 15.1.3.3 encodeURI (uri)
    intrinsic native function encodeURI(uri);
    
    // 15.1.3.4 encodeURIComponent (uriComponent)
    intrinsic native function encodeURIComponent(uriComponent);

    // [proposals:versioning] says that __ECMASCRIPT_VERSION__ is always defined
    const __ECMASCRIPT_VERSION__ = 4;

    // [proposals:bug fixes] - [IMMUTABLE.GLOBALS] says that these three names
    // are {DE,DD,RO}, and not just {DE,DD} as in E262-3.
    const NaN = intrinsic::NaN;
    const Infinity = intrinsic::Infinity;
    const undefined = intrinsic::undefined;
    
    // Mutable public properties defaulting to their intrinsic namesakes.
    var eval = intrinsic::eval;
    var parseInt = intrinsic::parseInt;
    var parseFloat = intrinsic::parseFloat;
    var isNaN = intrinsic::isNaN;
    var isFinite = intrinsic::isFinite;
    var decodeURI = intrinsic::decodeURI;
    var decodeURIComponent = intrinsic::decodeURIComponent;
    var encodeURI = intrinsic::encodeURI;
    var encodeURIComponent = intrinsic::encodeURIComponent;
    var global = this;

    // The non-virtual property get/set helpers.
    intrinsic native function get(obj:Object!, name:string) : *;
    intrinsic native function set(obj:Object!, name:string, val:*) : void;

}
