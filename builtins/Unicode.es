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

package Unicode
{
    use namespace intrinsic;
    use namespace __ES4__;
    use strict;

    /* Unicode line break characters */
    public const linebreaks = "\u000A\u000D\u0085\u2028\u2029";

    /* Unicode space characters, less character values below SPACE */
    public const spaces = ("\u0020\u00A0\u1680\u180E\u2000\u2001\u2002\u2003\u2004" +
                           "\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000");

    /* "Blank" characters: spaces plus space-like control characters */
    public const blanks = "\t\v\f" + spaces;

    /* Unicode decimal digit characters */
    public const decimal_digits = "0123456789";

    /* Unicode hex digit characters */
    public const hex_digits = "0123456789abcdefABCDEF";

    /* Word characters */
    public const alphanumerics = ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                  "abcdefghijklmnopqrstuvwxyz" +
                                  "0123456789" +
                                  "_");

    /* Unicode format control characters */
    public const format_controls = ("\u00AD\u0600\u0601\u0602\u0603" +
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
                    let cc : uint = s.charCodeAt(j);
                    buckets[cc % buckets.length].push(cc);
                }
            }
        }

        function containsCode(cc) {
            let bucket : Array = buckets[cc % nbuckets];
            let lim : uint = bucket.length;
            for (let i : uint = 0u; i < lim; ++i)
                if (bucket[i] == cc)
                    return true;
            return false;
        }

        function contains(str) {
            let lim = str.length;
            for (let i : uint = 0u; i < lim; ++i)
                if (!containsCode(str.charCodeAt(i)))
                    return false;
            return true;
        }

        private static const nbuckets : uint = 37u;
        private var buckets = new Array(nbuckets);
    }

    /* Utility functions for String */

    const trimmable_space_chars =
        new CharSet(explodeString(blanks + "\n\r"));

    public function isTrimmableSpace(c: string) : boolean
        trimmable_space_chars.contains(c);


    /* Utility functions for Regular Expressions */

    public function isIdentifierStart(c) {
        return isUnicodeLetter(c.charCodeAt(0)) || c == '_' || c == '$';
    }

    public function isIdentifierPart(c) {
        let cc: uint = c.charCodeAt(0);
        return isIdentifierStart(c) ||
            isUnicodeMn(cc) || isUnicodeMc(cc) || isUnicodeNd(cc) || isUnicodePc(cc);
    }

    const terminator_chars =
        new CharSet(explodeString(linebreaks));

    public function isTerminator(c : string) : boolean
        terminator_chars.contains(c);

    public function isTerminatorCode(cc : uint) : boolean
        terminator_chars.containsCode(cc);

    const decimal_digit_chars =
        new CharSet(explodeString(decimal_digits));

    public function isDecimalDigit(c: string) : boolean
        decimal_digit_chars.contains(c);

    public function decimalValue(c: string) : boolean
        c.charCodeAt(0) - "0".charCodeAt(0);

    const hex_digit_chars =
        new CharSet(explodeString(hex_digits));

    public function isHexDigit(c: string) : boolean
        hex_digit_chars.contains(c);

    public function hexValue(c) : double {
        if (c >= "0" && c <= "9")
            return c.charCodeAt(0) - "0".charCodeAt(0);
        else
            return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
    }

    const blank_chars =
        new CharSet(explodeString(blanks));

    public function isBlank(c : string) : boolean
        blank_chars.contains(c);

    public function isBlankCode(cc : uint) : boolean
        blank_chars.containsCode(cc);

    const word_chars =
        new CharSet(explodeString(alphanumerics));

    public function isWordChar(c: string) : boolean
        word_chars.contains(c);

    const format_control = new CharSet(explodeString(format_controls));

    public function isFormatControl(c)
        format_control.contains(c);

    public function isFormatControlCode(cc:uint)
        format_control.containsCode(cc);

    public function explodeString(s : string) : [string] {
        let cs = [] : [string];
        for ( let i=0 ; i < s.length ; i++ )
            cs[i] = s[i];
        return cs;
    }

    /* Character classification according to the Unicode data table */

    public function isUnicodeLu(cc: uint): boolean
        classify(table_Lu, cc);

    public function isUnicodeLl(cc: uint): boolean
        classify(table_Ll, cc);

    public function isUnicodeLt(cc: uint): boolean
        classify(table_Lt, cc);

    public function isUnicodeLm(cc: uint): boolean
        classify(table_Lm, cc);

    public function isUnicodeLo(cc: uint): boolean
        classify(table_Lo, cc);

    public function isUnicodeL(cc: uint): boolean
        isUnicodeLu(cc) ||
        isUnicodeLl(cc) ||
        isUnicodeLt(cc) ||
        isUnicodeLm(cc) ||
        isUnicodeLo(cc);

    public function isUnicodeLetter(cc: uint): boolean /* E262-3 7.6 */
        isUnicodeL(cc) ||
        isUnicodeNl(cc);

    public function isUnicodeMn(cc: uint): boolean
        classify(table_Mn, cc);

    public function isUnicodeMc(cc: uint): boolean
        classify(table_Mc, cc);

    public function isUnicodeMe(cc: uint): boolean
        classify(table_Me, cc);

    public function isUnicodeM(cc: uint): boolean
        isUnicodeMn(cc) ||
        isUnicodeMc(cc) ||
        isUnicodeMe(cc);

    public function isUnicodeNd(cc: uint): boolean
        classify(table_Nd, cc);

    public function isUnicodeNl(cc: uint): boolean
        classify(table_Nl, cc);

    public function isUnicodeNo(cc: uint): boolean
        classify(table_No, cc);

    public function isUnicodeN(cc: uint): boolean
        isUnicodeNd(cc) ||
        isUnicodeNl(cc) ||
        isUnicodeNo(cc);

    public function isUnicodePc(cc: uint): boolean
        classify(table_Pc, cc);

    public function isUnicodePd(cc: uint): boolean
        classify(table_Pd, cc);

    public function isUnicodePs(cc: uint): boolean
        classify(table_Ps, cc);

    public function isUnicodePe(cc: uint): boolean
        classify(table_Pe, cc);

    public function isUnicodePi(cc: uint): boolean
        classify(table_Pi, cc);

    public function isUnicodePf(cc: uint): boolean
        classify(table_Pf, cc);

    public function isUnicodePo(cc: uint): boolean
        classify(table_Po, cc);

    public function isUnicodeP(cc: uint): boolean
        isUnicodePc(cc) ||
        isUnicodePd(cc) ||
        isUnicodePs(cc) ||
        isUnicodePe(cc) ||
        isUnicodePi(cc) ||
        isUnicodePf(cc) ||
        isUnicodePo(cc);

    public function isUnicodeSm(cc: uint): boolean
        classify(table_Sm, cc);

    public function isUnicodeSc(cc: uint): boolean
        classify(table_Sc, cc);

    public function isUnicodeSk(cc: uint): boolean
        classify(table_Sk, cc);

    public function isUnicodeSo(cc: uint): boolean
        classify(table_So, cc);

    public function isUnicodeS(cc: uint): boolean
        isUnicodeSm(cc) ||
        isUnicodeSc(cc) ||
        isUnicodeSk(cc) ||
        isUnicodeSo(cc);

    public function isUnicodeZs(cc: uint): boolean
        classify(table_Zs, cc);

    public function isUnicodeZl(cc: uint): boolean
        classify(table_Zl, cc);

    public function isUnicodeZp(cc: uint): boolean
        classify(table_Zp, cc);

    public function isUnicodeZ(cc: uint): boolean
        isUnicodeZs(cc) ||
        isUnicodeZl(cc) ||
        isUnicodeZp(cc);

    public function isUnicodeCc(cc: uint): boolean
        classify(table_Cc, cc);

    public function isUnicodeCf(cc: uint): boolean
        classify(table_Cf, cc);

    public function isUnicodeCs(cc: uint): boolean
        classify(table_Cs, cc);

    public function isUnicodeCo(cc: uint): boolean
        classify(table_Co, cc);

    public function isUnicodeCn(cc: uint): boolean
        classify(table_Cn, cc);

    public function isUnicodeC(cc: uint): boolean
        isUnicodeCc(cc) ||
        isUnicodeCf(cc) ||
        isUnicodeCs(cc) ||
        isUnicodeCo(cc) ||
        isUnicodeCn(cc);

    public function toUpperCaseCharCode(i: uint) : (uint|[/*uint*/]) {
        var c = toUpperTbl(i);
        return c ? c : i;
    }

    public function toLowerCaseCharCode(i: uint) : (uint|[/*uint*/]) {
        var c = toLowerTbl(i);
        return c ? c : i;
    }
}
