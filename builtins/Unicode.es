/* -*- mode: java; indent-tabs-mode: nil -*- */

package Unicode
{
    use namespace intrinsic;

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

        static const nbuckets : uint = 37u;
        var buckets = new Array(nbuckets);  /* FIXME: private */
    }

    /* Utility functions for String */

    const trimmable_space_chars =
        new CharSet(explodeString(blanks + "\n\r"));

    public function isTrimmableSpace(c: string) : boolean
        trimmable_space_chars.contains(c);


    /* Utility functions for Regular Expressions */

    public function isIdentifierStart(c) {
        /* FIXME -- hairy, but not yet important.  For now, ASCII.  Ticket #49. */
        return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c == '$';
    }

    public function isIdentifierPart(c) {
        /* FIXME -- hairy, but not yet important.  For now, ASCII.  Ticket #49. */
        return isIdentifierStart(c) || c >= '0' && c <= '9';
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

    public function isOctalDigit(c: string) : boolean
        c >= "0" && c <= "7";

    public function octalValue(c: string) : boolean
        c.charCodeAt(0) - "0".charCodeAt(0);
        
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
        let cs : [string] = [] : [string];
        for ( let i=0 ; i < s.length ; i++ )
            cs[i] = s[i];
        return cs;
    }

    public function toUpperCaseCharCode(i : uint) : uint {
        /* FIXME: hairy but not yet important.  For now, ASCII.  Ticket #49. */
        if (i >= 97 && i <= 122)  // a..z?
            return uint(i-32);
        else
            return i;
    }

    public function toLowerCaseCharCode(i : uint) : uint {
        /* FIXME: hairy but not yet important.  For now, ASCII.  Ticket #49. */
        if (i >= 65 && i <= 90)
            return uint(i+32);
        else
            return i;
    }
}
