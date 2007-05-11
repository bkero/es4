/* -*- mode: java; indent-tabs-mode: nil -*- */

package Unicode
{
    use namespace intrinsic;

    /* Simple utility class for managing a constant set of characters
     * with quick lookup.
     */
    class CharSet 
    {
        function CharSet(cs) {
            for (let i=0 ; i < buckets.length ; i++ )
                buckets[i] = "";
            for (let i=0 ; i < cs.length ; i++ )
                buckets[cs[i].charCodeAt(0) % buckets.length] += cs[i];
        }

        function contains(c)
            buckets[c.charCodeAt(0) % buckets.length].indexOf(c) != -1;

        var buckets = new Array(37);  /* FIXME: private */
    }

    /* These are all the Unicode space characters, less character values below SPACE */

    public const spaces = ("\u0020\u00A0\u1680\u180E\u2000\u2001\u2002\u2003\u2004" +
                           "\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000");


    /* Utility functions for String */

    const trimmable_spaces =
        new CharSet(explodeString("\t\n\v\f\r" + spaces));

    public function isTrimmableSpace(c: string) : boolean
        trimmable_spaces.contains(c);


    /* Utility functions for Regular Expressions */

    public function isIdentifierStart(c) { // FIXME -- hairy, but not yet important
        return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'z' || c == '_' || c == '$';
    }

    public function isIdentifierPart(c) { // FIXME -- hairy, but not yet important
        return isIdentifierStart(c) || c >= '0' && c <= '9';
    }

    const terminators = 
        new CharSet(explodeString("\u000D\u000A\u2028\u2029\u0085"));

    public function isTerminator(c : string) : boolean
        terminators.contains(c);

    const decimal_digits = 
        new CharSet(explodeString("0123456789"));

    public function isDecimalDigit(c: string) : boolean
        decimal_digits.contains(c);

    const hex_digits = 
        new CharSet(explodeString("0123456789abcdefABCDEF"));

    public function isHexDigit(c: string) : boolean
        hex_digits.contains(c);

    const blank_chars = 
        new CharSet(explodeString("\t\v\f" + spaces));

    public function isBlank(c : string) : boolean
        blank_chars.contains(c);

    const word_chars = 
        new CharSet(explodeString("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                  "abcdefghijklmnopqrstuvwxyz" +
                                  "0123456789" +
                                  "_"));

    public function isWordChar(c: string) : boolean 
        word_chars.contains(c);

    const format_control =
        new CharSet(explodeString("\u00AD\u0600\u0601\u0602\u0603" +
                                  "\u06DD\u070F\u17B4\u17B5\u200B" +
                                  "\u200C\u200D\u200E\u200F\u202A" +
                                  "\u202B\u202C\u202D\u202E\u2060" +
                                  "\u2061\u2062\u2063\u206A\u206B" +
                                  "\u206C\u206D\u206E\u206F\uFEFF" +
                                  "\uFFF9\uFFFA\uFFFB"));

    public function isFormatControl(c)
        format_control.contains(c);

    public function explodeString(s : string) : [string] {
        let cs : [string] = [] : [string];
        for ( let i=0 ; i < s.length ; i++ )
            cs[i] = s[i];
        return cs;
    }

    /* FIXME: implementme */
    public function toUpperCaseCharCode(i : uint) : uint 
        uint(i);

    /* FIXME: implementme */
    public function toLowerCaseCharCode(i : uint) : uint 
        uint(i);
}
