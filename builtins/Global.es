/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Global" object
 * ES-262-3 15.1
 * ES-262-4 draft
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
    // {DE,DD,RO} -- change from ECMA-262, which has them as {DE,DD}
    intrinsic const NaN = 0.0/0.0;
    intrinsic const Infinity = 1.0/0.0;
    intrinsic const undefined = void(0);
    
    // @todo: "dynamic function" is probably redundant at the top level, but is useful for clarity
    

    // 15.1.2.1 eval (x)
    intrinsic native function eval(x);

    // 15.1.2.2 parseInt (string , radix)
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
        let maybe_octal = false;
        let maybe_hexadecimal = false;

        if (r == 0) {
            r = 10;
            maybe_octal = true;
            maybe_hexadecimal = true;
        }
        else if (r == 16)
            maybe_hexadecimal = true;
        else if (r < 2 || r > 36)
            return NaN;

        if (maybe_octal && s.length >= 1 && s[0] == '0')
            r = 8;
        if (maybe_hexadecimal && s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[2] == 'X')) {
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
    intrinsic native function parseFloat(string:string);

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

    // Mutable public properties defaulting to their intrinsic namesakes.
    var NaN = intrinsic::NaN;
    var Infinity = intrinsic::Infinity;
    var undefined = intrinsic::undefined;
    
    var eval = intrinsic::eval;
    var parseInt = intrinsic::parseInt;
    var parseFloat = intrinsic::parseFloat;
    var isNaN = intrinsic::isNaN;
    var isFinite = intrinsic::isFinite;
    var decodeURI = intrinsic::decodeURI;
    var decodeURIComponent = intrinsic::decodeURIComponent;
    var encodeURI = intrinsic::encodeURI;
    var encodeURIComponent = intrinsic::encodeURIComponent;
    
    // The non-virtual property get/set helpers.
    intrinsic native function get(obj:Object!, name:string) : *;
    intrinsic native function set(obj:Object!, name:string, val:*) : void;

}
