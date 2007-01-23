/* -*- indent-tabs-mode: nil -*- */

package Unicode
{
    public function isIdentifierStart(c) { // FIXME -- hairy, but not yet important
        return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'z' || c == '_' || c == '$';
    }

    /* Utility functions for Regular Expressions */

    public function isIdentifierPart(c) { // FIXME -- hairy, but not yet important
        return isIdentifierStart(c) || c >= '0' && c <= '9';
    }

    const char_CR : String! = "\u000D";
    const char_LF : String! = "\u000A";
    const char_LS : String! = "\u2028";
    const char_PS : String! = "\u2029";
    const char_NEL: String! = "\u0085";

    public function isTerminator(c : String!) : Boolean {
        return (c === char_LF || c === char_CR || c === char_LS || c === char_PS || c == char_NEL);
    }

    public function isHexDigit(c) {
        return c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';
    }

    const word_chars = {};

    for each ( let c in ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
						 "abcdefghijklmnopqrstuvwxyz" +
						 "0123456789" +
						 "_").split("") ) {
        word_chars[c] = true;
    }

    public function isWordChar(c: String!) : Boolean 
        word_chars.hasOwnProperty(c);

    const format_control = {};

    for each ( let c in ("\u00AD\u0600\u0601\u0602\u0603" +
                         "\u06DD\u070F\u17B4\u17B5\u200B" +
                         "\u200C\u200D\u200E\u200F\u202A" +
                         "\u202B\u202C\u202D\u202E\u2060" +
                         "\u2061\u2062\u2063\u206A\u206B" +
                         "\u206C\u206D\u206E\u206F\uFEFF" +
                         "\uFFF9\uFFFA\uFFFB").split("") ) {
        format_control[c] = true;
    }

    public function isFormatControl(c)
        format_control.hasOwnProperty(c);
}
