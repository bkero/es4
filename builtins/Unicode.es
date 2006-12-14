/* -*- mode: java; mode: font-lock; tab-width: 4 -*-  */

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
}
