/* -*- mode: java; indent-tabs-mode: nil -*- */

package Unicode
{
    /* FIXME: Unicode

    const spaces = ("\u0020\u00A0\u1680\u180E\u2000\u2001\u2002\u2003\u2004" +
                    "\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000");
    */
    const spaces = ("\u0020");

    public function isIdentifierStart(c) { // FIXME -- hairy, but not yet important
        return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'z' || c == '_' || c == '$';
    }

    /* Utility functions for String */

    const trimmable_spaces = {};

    /* FIXME: for-each-in is broken

    for each ( let c in explodeString("\t\n\v\f\r" + spaces) ) {
        trimmable_spaces[c.charCodeAt(0)] = true;
    }
    */

    for ( let i=0, cs=explodeString("\t\n\v\f\r" + spaces) ; i < cs.length ; i++ )
        trimmable_spaces[cs[i].charCodeAt(0)] = true;

    public function isTrimmableSpace(c : uint) : boolean {
        return trimmable_spaces.intrinsic::hasOwnProperty(c);        /* FIXME: "intrinsic::" should not be necessary */
    }

    /* Utility functions for Regular Expressions */

    public function isIdentifierPart(c) { // FIXME -- hairy, but not yet important
        return isIdentifierStart(c) || c >= '0' && c <= '9';
    }

    const char_CR : string = "\u000D";
    const char_LF : string = "\u000A";
    const char_LS : string = "\u2028";
    const char_PS : string = "\u2029";
    const char_NEL: string = "\u0085";

    public function isTerminator(c : string) : boolean {
        return (c === char_LF || c === char_CR /* || c === char_LS || c === char_PS || c == char_NEL */ );  /* FIXME: Unicode */
    }

    public function isHexDigit(c) {
        return c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';
    }

    const blank_chars = {};

    /* FIXME: for-each-in is broken

    for each ( let c in explodeString("\t\v\f" + spaces) ) {
        blank_chars[c] = true;
    }
    */

    for ( let i=0, cs=explodeString("\t\v\f" + spaces) ; i < cs.length ; i++ )
        blank_chars[cs[i]] = true;

    public function isBlank(c : string) : boolean
        blank_chars.intrinsic::hasOwnProperty(c);  /* FIXME: "intrinsic::" should not be necessary */

    const word_chars = {};

    /* FIXME: for-each-in is broken

    for each ( let c in explodeString("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                      "abcdefghijklmnopqrstuvwxyz" +
                                      "0123456789" +
                                      "_") ) {
        word_chars[c] = true;
    }
    */

    for ( let i=0, cs=explodeString("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                                    "abcdefghijklmnopqrstuvwxyz" +
                                    "0123456789" +
                                    "_");
          i < cs.length ; 
          i++ ) 
        word_chars[cs[i]] = true;

    public function isWordChar(c: string) : boolean 
        word_chars.intrinsic::hasOwnProperty(c);  /* FIXME: "intrinsic::" should not be necessary */

    const format_control = {};

    /* FIXME: for-each-in is broken

    for each ( let c in explodeString("\u00AD\u0600\u0601\u0602\u0603" +
                                      "\u06DD\u070F\u17B4\u17B5\u200B" +
                                      "\u200C\u200D\u200E\u200F\u202A" +
                                      "\u202B\u202C\u202D\u202E\u2060" +
                                      "\u2061\u2062\u2063\u206A\u206B" +
                                      "\u206C\u206D\u206E\u206F\uFEFF" +
                                      "\uFFF9\uFFFA\uFFFB") ) {
        format_control[c] = true;
    }
    */

    /* FIXME: Unicode

    for ( let i=0, cs=explodeString("\u00AD\u0600\u0601\u0602\u0603" +
                                    "\u06DD\u070F\u17B4\u17B5\u200B" +
                                    "\u200C\u200D\u200E\u200F\u202A" +
                                    "\u202B\u202C\u202D\u202E\u2060" +
                                    "\u2061\u2062\u2063\u206A\u206B" +
                                    "\u206C\u206D\u206E\u206F\uFEFF" +
                                    "\uFFF9\uFFFA\uFFFB") ;
          i < cs.length ;
          i++ )
        format_control[cs[i]] = true;

    public function isFormatControl(c)
        format_control.intrinsic::hasOwnProperty(c);  // FIXME: "intrinsic::" should not be necessary
    */

    public function isFormatControl(c)
        false;

    public function explodeString(s : string) : [string] {
        let cs : [string] = [] : [string];
        for ( let i=0 ; i < s.length ; i++ )
            cs[i] = s.charAt(i);   /* FIXME: use [] syntax when it works */
        return cs;
    }

    public function toUpperCaseCharCode(i : uint) : uint 
        uint(i);

    public function toLowerCaseCharCode(i : uint) : uint 
        uint(i);

}
