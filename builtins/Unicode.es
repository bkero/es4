/* -*- mode: java; indent-tabs-mode: nil -*-
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
 */

    use namespace ECMAScript4_Internal;
    use namespace intrinsic;
    use namespace Unicode;
    use default namespace Unicode;

    /* Unicode line break characters */
    const linebreaks = "\u000A\u000D\u0085\u2028\u2029";

    /* Unicode space characters, less character values below SPACE */
    const spaces = ("\u0020\u00A0\u1680\u180E\u2000\u2001\u2002\u2003\u2004" +
                           "\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000");

    /* "Blank" characters: spaces plus space-like control characters */
    const blanks = "\t\v\f" + spaces;

    /* Unicode decimal digit characters */
    const decimal_digits = "0123456789";

    /* Unicode hex digit characters */
    const hex_digits = "0123456789abcdefABCDEF";

    /* Word characters */
    const alphanumerics = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                  "abcdefghijklmnopqrstuvwxyz" +
                                  "0123456789" +
                                  "_");

    /* Unicode format control characters */
    const format_controls = ("\u00AD\u0600\u0601\u0602\u0603" +
                             "\u06DD\u070F\u17B4\u17B5\u200B" +
                             "\u200C\u200D\u200E\u200F\u202A" +
                             "\u202B\u202C\u202D\u202E\u2060" +
                             "\u2061\u2062\u2063\u206A\u206B" +
                             "\u206C\u206D\u206E\u206F\uFEFF" +
                             "\uFFF9\uFFFA\uFFFB");

    /* Simple utility class for managing a constant set of characters
     * with quick lookup.
     */
    class CharSet
    {
        function CharSet(cs) {
            for (let i=0 ; i < buckets.length ; i++ )
                buckets[i] = new Array();
            for (let i=0 ; i < cs.length ; i++ ) {
                let s : string = cs[i];
                for (let j=0; j < s.length; j++) {
                    let cc : double = s.charCodeAt(j);
                    buckets[cc % buckets.length].push(cc);
                }
            }
        }

        function containsCode(cc) {
            let bucket : Array = buckets[cc % nbuckets];
            let lim : double = bucket.length;
            for (let i : double = 0; i < lim; ++i)
                if (bucket[i] == cc)
                    return true;
            return false;
        }

        function contains(str) {
            let lim = str.length;
            for (let i : double = 0; i < lim; ++i)
                if (!containsCode(str.charCodeAt(i)))
                    return false;
            return true;
        }

        private static const nbuckets : double = 37;
        private var buckets = new Array(nbuckets);
    }

    /* Utility functions for String */

    const trimmable_space_chars =
        new CharSet(explodeString(blanks + "\n\r"));

    function isTrimmableSpace(c: string) : boolean
        trimmable_space_chars.contains(c);


    /* Utility functions for Regular Expressions */

    function isIdentifierStart(c) {
        return isUnicodeLetter(c.charCodeAt(0)) || c == '_' || c == '$';
    }

    function isIdentifierPart(c) {
        let cc: double = c.charCodeAt(0);
        return isIdentifierStart(c) ||
            isUnicodeMn(cc) || isUnicodeMc(cc) || isUnicodeNd(cc) || isUnicodePc(cc);
    }

    const terminator_chars =
        new CharSet(explodeString(linebreaks));

    function isTerminator(c : string) : boolean
        terminator_chars.contains(c);

    function isTerminatorCode(cc : double) : boolean
        terminator_chars.containsCode(cc);

    const decimal_digit_chars =
        new CharSet(explodeString(decimal_digits));

    function isDecimalDigit(c: string) : boolean
        decimal_digit_chars.contains(c);

    function decimalValue(c: string) : boolean
        c.charCodeAt(0) - "0".charCodeAt(0);

    const hex_digit_chars =
        new CharSet(explodeString(hex_digits));

    function isHexDigit(c: string) : boolean
        hex_digit_chars.contains(c);

    function hexValue(c) : double {
        if (c >= "0" && c <= "9")
            return c.charCodeAt(0) - "0".charCodeAt(0);
        else
            return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
    }

    const blank_chars =
        new CharSet(explodeString(blanks));

    function isBlank(c : string) : boolean
        blank_chars.contains(c);

    function isBlankCode(cc : double) : boolean
        blank_chars.containsCode(cc);

    const word_chars =
        new CharSet(explodeString(alphanumerics));

    function isWordChar(c: string) : boolean
        word_chars.contains(c);

    const format_control = new CharSet(explodeString(format_controls));

    function isFormatControl(c)
        format_control.contains(c);

    function isFormatControlCode(cc:double)
        format_control.containsCode(cc);

    function explodeString(s : string) : [string] {
        let cs = [] : [string];
        for ( let i=0 ; i < s.length ; i++ )
            cs[i] = s[i];
        return cs;
    }

    /* Character classification according to the Unicode data table */

    function isUnicodeLu(cc: double): boolean
        classify(table_Lu, cc);

    function isUnicodeLl(cc: double): boolean
        classify(table_Ll, cc);

    function isUnicodeLt(cc: double): boolean
        classify(table_Lt, cc);

    function isUnicodeLm(cc: double): boolean
        classify(table_Lm, cc);

    function isUnicodeLo(cc: double): boolean
        classify(table_Lo, cc);

    function isUnicodeL(cc: double): boolean
        isUnicodeLu(cc) ||
        isUnicodeLl(cc) ||
        isUnicodeLt(cc) ||
        isUnicodeLm(cc) ||
        isUnicodeLo(cc);

    function isUnicodeLetter(cc: double): boolean /* E262-3 7.6 */
        isUnicodeL(cc) ||
        isUnicodeNl(cc);

    function isUnicodeMn(cc: double): boolean
        classify(table_Mn, cc);

    function isUnicodeMc(cc: double): boolean
        classify(table_Mc, cc);

    function isUnicodeMe(cc: double): boolean
        classify(table_Me, cc);

    function isUnicodeM(cc: double): boolean
        isUnicodeMn(cc) ||
        isUnicodeMc(cc) ||
        isUnicodeMe(cc);

    function isUnicodeNd(cc: double): boolean
        classify(table_Nd, cc);

    function isUnicodeNl(cc: double): boolean
        classify(table_Nl, cc);

    function isUnicodeNo(cc: double): boolean
        classify(table_No, cc);

    function isUnicodeN(cc: double): boolean
        isUnicodeNd(cc) ||
        isUnicodeNl(cc) ||
        isUnicodeNo(cc);

    function isUnicodePc(cc: double): boolean
        classify(table_Pc, cc);

    function isUnicodePd(cc: double): boolean
        classify(table_Pd, cc);

    function isUnicodePs(cc: double): boolean
        classify(table_Ps, cc);

    function isUnicodePe(cc: double): boolean
        classify(table_Pe, cc);

    function isUnicodePi(cc: double): boolean
        classify(table_Pi, cc);

    function isUnicodePf(cc: double): boolean
        classify(table_Pf, cc);

    function isUnicodePo(cc: double): boolean
        classify(table_Po, cc);

    function isUnicodeP(cc: double): boolean
        isUnicodePc(cc) ||
        isUnicodePd(cc) ||
        isUnicodePs(cc) ||
        isUnicodePe(cc) ||
        isUnicodePi(cc) ||
        isUnicodePf(cc) ||
        isUnicodePo(cc);

    function isUnicodeSm(cc: double): boolean
        classify(table_Sm, cc);

    function isUnicodeSc(cc: double): boolean
        classify(table_Sc, cc);

    function isUnicodeSk(cc: double): boolean
        classify(table_Sk, cc);

    function isUnicodeSo(cc: double): boolean
        classify(table_So, cc);

    function isUnicodeS(cc: double): boolean
        isUnicodeSm(cc) ||
        isUnicodeSc(cc) ||
        isUnicodeSk(cc) ||
        isUnicodeSo(cc);

    function isUnicodeZs(cc: double): boolean
        classify(table_Zs, cc);

    function isUnicodeZl(cc: double): boolean
        classify(table_Zl, cc);

    function isUnicodeZp(cc: double): boolean
        classify(table_Zp, cc);

    function isUnicodeZ(cc: double): boolean
        isUnicodeZs(cc) ||
        isUnicodeZl(cc) ||
        isUnicodeZp(cc);

    function isUnicodeCc(cc: double): boolean
        classify(table_Cc, cc);

    function isUnicodeCf(cc: double): boolean
        classify(table_Cf, cc);

    function isUnicodeCs(cc: double): boolean
        classify(table_Cs, cc);

    function isUnicodeCo(cc: double): boolean
        classify(table_Co, cc);

    function isUnicodeCn(cc: double): boolean
        classify(table_Cn, cc);

    function isUnicodeC(cc: double): boolean
        isUnicodeCc(cc) ||
        isUnicodeCf(cc) ||
        isUnicodeCs(cc) ||
        isUnicodeCo(cc) ||
        isUnicodeCn(cc);

    function toUpperCaseCharCode(i: double) : (double|[/*uint*/]) {
        var c = toUpperTbl(i);
        return c ? c : i;
    }

    function toLowerCaseCharCode(i: double) : (double|[/*uint*/]) {
        var c = toLowerTbl(i);
        return c ? c : i;
    }

    helper function isTrimmableSpace(x)
        Unicode.isTrimmableSpace(x);
