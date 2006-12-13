/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 *
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 * E262-4 proposals:update_unicode
 *
 * Regular expression compiler.
 *
 * TO DO:
 *
 * - Whitespace / comment handling for the /x qualifier are 
 *   not correct, these are ignored everywhere now but the spec
 *   says to do so only in some contexts
 *
 * - NEL handling?
 */

package RegExp
{
	/* Returns a matcher, the number of capturing parens, and an array
	   of length equal to the number of capturing parens, where an
	   element is non-null if that capturing paren has a name.  */

	private static function compile( source : String!, flags : String! ) : [Matcher, int, [String]!] {
		return (new RECompile(source, flags).compile());
	}

	private class RECompile
	{
		// Invariant: either idx==source.length or source[idx] is a significant char

		const source;                  // expression source, sans leading and trailing /
		var   idx;                     // current character in the source
		const extended;                // true iff expression has /x flag
		const names = [];              // capturing names, or null for capturing exprs that are not named
		const nCapturingParens = 0;    // number of capturing parens (including those that are named)

		function RECompile( source, flags ) 
			: extended(flags.indexOf("x") != -1)
			, source(source)
			, idx(0)
		{
			skipBlanksAndComments();
		}

		function compile() : [Matcher, int, [String]] {
			var p = pattern;
			if (p === null)
				throw new SyntaxError("Empty regular expression");
			if (idx !== source.length)
				throw new SyntaxError("Invalid character in input");
			return [p, nCapturingparens, names];
		}

		function pattern() : Matcher {
			return disjunction();
		}

		function disjunction() : Matcher {
			var alt = alternative();
			if (alt == null)
				return new Empty;
			if (eat("|")) {
				var dis = disjunction();
				if (dis === null)
					throw new SyntaxError("Missing disjunction following '|'");
				return new Disjunct(alt, dis);
			}
			else
				return alt;
		}

		function alternative() : Matcher? {
			var t = term();
			if (t === null)
				return t;
			for (;;) {
				var p = term();
				if (!p)
					return t;
				t = new Alternative(t, p);
			}
		}

		function term() : Matcher? {
			var x = assertion();
			if (x != null)
				return x;
			var x = atom();
			if (x == null)
				return x;
			var y = quantifier();
			if (y == null)
				return x;
			else {
				var [min, max, greedy] = y;
				return Quantified(x, min, max, greedy);
			}
		}

		function assertion() : Matcher? {
			     if (eat("^"))   return new AssertStartOfInput;
			else if (eat("$"))   return new AssertEndOfInput;
			else if (eat("\\b")) return new AssertWordBoundary;
			else if (eat("\\B")) return new AssertNotWordBoundary;
			else			     return null;
		}

		function quantifier() : [Number,Number,Boolean]? {
			var x = quantifierPrefix();
			if (x == null)
				return x;
			var [min,max] = x;
			var greedy = false;
			if (eat("?"))
				greedy = true;
			return [min,max,greedy];
		}

		function quantifierPrefix() : [Number, Number]? {
			if (eat("*"))      return [0,Infinity];
			else if (eat("+")) return [1,Infinity];
			else if (eat("?")) return [0,1];
			else if (eat("{")) {
				var min = decimalDigits();
				var max = n;
				if (eat(",")) {
					if (eat("}"))
						max = Infinity;
					else {
						max = decimalDigits();
						match("}");
					}
				}
				return [min,max];
			}
			else 
				return null;
		}

		function atom() : Matcher {
			if (eat("."))
				return new AtomDot;
			else if (eat("(")) {
				if (eat("?")) {
					if (eat(":")) {
						let d = disjunction();
						match(")");
						return d;
					}
					if (eat("=")) {
						let d = disjunction();
						match(")");
						return new PositiveLookahead(d);
					}
					if (eat("!")) {
						let d = disjunction();
						match(")");
						return new NegativeLookahead(d);
					}
					if (eat("#")) {
						consumeUntil(")");
						match(")");
						return new Empty;
					}
					if (eat("P")) {
						let function consumeName() : String! {
							// FIXME: here, we should not skip blanks/comments inside the name
							let c = consume();
							if (!isIdentifierStart(c))
								throw new SyntaxError("Invalid name");
							let name = c;
							while (isIdentPart(c = next()))
								name += consume();
							return name;
						}

						if (eat("<")) {
							let name = consumeName();
							match(">");
							let d = disjunction();
							let capno = nCapturingParens++;
							match(")");
							for each ( let n in names ) {
								if (n == name)
									throw new SyntaxError("Multiply defined capture name: " + name );
							}
							names[capno] = name;
							return new Capturing(d);
						}
						if (eat("=")) {
							let name = consumeName();
							match(")");
							return new NamedBackref(name);
						}
					}
					throw new SyntaxError("Bogus (? pattern");
				}
				else {
					let d = disjunction();
					match(")");
					return new Capturing(d);
				}
			}
			else if (lookingAt("[")) 
				return characterClass();
			else if (lookingAt("\\"))
				return atomEscape();
			else {
				if (lookingAt("^") || 
					lookingAt("$") || 
					lookingAt("*") || 
					lookingAt("+") || 
					lookingAt("?") || 
					lookingAt("{") ||
					lookingAt("}") ||
					lookingAt("]") ||
					lookingAt(")") ||
					lookingAt("|"))
					throw new SyntaxError("Illegal character in expression.");
				return new Character(consume());
			}
		}

		function atomEscape() : Matcher {
			var t;
			match("\\");
			if (t = decimalEscape())
				return new CharsetMatcher(new CharsetAdhoc([t]));
			if (t = characterClassEscape())
				return new CharsetMatcher(t);
			if (t = characterEscape())
				return new CharsetMatcher(new CharsetAdhoc([t]));
			throw new SyntaxError("Failed to match AtomEscape");
		}

		function characterClass() : Matcher {
			match("[");
			var inverted = false;
			if (eat("^"))
				inverted = true;
			var ranges : Charset = classRanges();
			match("]");
			return new CharsetMatcher(inverted ? new CharsetComplement(ranges) : ranges);
		}

		/* The grammar for class ranges looks hairy.  The trick is to
		   rewrite the grammar.  See ClassRanges.txt for an explanation.
		   The final grammar is:

             ClassRanges ::= [empty] | NonemptyClassRanges ;

             NonemptyClassRanges ::= ClassAtom
                                   | ClassAtom "-" 
                                   | ClassAtom "-" ClassAtom
                                   | ClassAtom "-" ClassAtom NonemptyClassRanges
                                   | ClassAtom [lookahead != "-"] NonemptyClassRanges ;

             ClassAtom ::= "-" | ClassAtomNoDash ;
						   
		  To this we add the intersection and subtraction operations.
		  I'm guessing left associative is right, but we need to check
		  this:

		     ClassRanges ::= [empty] 
			               | ClassRanges2

             ClassRanges2 ::= NonemptyClassRanges[true]
						    | ClassRanges2 "\-" NonemptyClassRanges[true]
						    | ClassRanges2 "\&" NonemptyClassRanges[true] ;

		*/

		function classRanges() : Charset {
			if (lookingAt("]"))
				return new CharsetEmpty;
			else {
				let s = nonemptyClassRanges(true, null);
				while (eat("\\")) {
					if (eat("&")) {
						let t = nonemptyClassRanges(true, null);
						s = CharsetIntersect(s, t);
					}
					else if (eat("-")) {
						let t = nonemptyClassRanges(true, null);
						s = CharsetSubtract(s, t);
					}
					else
						throw new SyntaxError("Expected ']' to end character set");
				}
				else
					return s;
			}
		}

		function nonemptyClassRanges(dash, acc) : Charset {

			function accumulate(acc, x) : Charset {
				return acc === null ? x : CharsetUnion(acc, x);
			}

			let a1 = classAtom(dash);

			if (lookingAt("]"))
				return a1;

			if (lookingAt("-")) {
				consume();
				if (lookingAt("]"))
					return accumulate(acc, CharsetAdhoc(["-"]));

				let a2 = classAtom();
				let a3 = accumulate(acc, CharsetRange(a1, a2));
				if (lookingAt("]"))
					return a3;

				return nonemptyClassRanges(true, a3);
			}
			
			return nonemptyClassRanges(false, accumulate(acc,a1));
		}

		function classAtom() : Charset {
			if (lookingAt("]") || atEnd())
				throw SyntaxError("Premature end of input");

			if (lookingAt("\\"))
				return classEscape();

			return new CharsetAdhoc(consume());
		}

		function classEscape() : Charset {
			match("\\");
			if (t = decimalEscape())
				return new CharsetAdhoc([t]);
			if (lookingAt("b"))
				return new CharsetAdhoc(["\u0008"]);
			if (t = characterEscape())
				return new CharsetAdhoc([t]);
			if (t = characterClassEscape())
				return t;
			throw SyntaxError("Bad classEscape");
		}

		/* Normally returns a character set.

		   Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails.

		   The initial \ has been consumed already.	*/

		function characterClassEscape() : Charset {
			var c;
			switch (c = next()) {
			case "D": case "d": 
				consume();
				return c == 'd' ? chars_digit : chars_notdigit;
			case "S": case "s": 
				consume();
				return c == 's' ? chars_space : chars_notspace;
			case "w": case "W": 
				consume();
				return c == 'w' ? chars_word : chars_notword;
			case "p": case "P": {
				let function getName() : String! {
					// FIXME: here, we should not skip blanks/comments inside the name
					let name = "";
					let c;
					while (!atEnd() && (c = consume()) != "}")
						name += c;
					return name;
				}

				consume();
				match("{");
				let name = getName();
				match("}");
				let cls = unicode_named_classes[name];
				if (!cls)
					throw new SyntaxError("Invalid unicode named class " + name);
				return c == 'p' ? cls : new CharsetComplement(cls);
			}
			default:
				return null;
			}
		}

		/* Normally returns a single character.

		   Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails.

		   The initial \ has been consumed already.	*/

		function decimalEscape() : String? {
			switch (next()) {
			case "0": case "1": case "2": case "3": case "4": 
			case "5": case "6": case "7": case "8": case "9":
				let (k = 0) {
					while (isDecimalDigit(next()))
						k = k*10 + decimalValue(consume());
					return String.fromCharCode(k);
				}
			default:
				return null;
			}
		}

		/* Normally returns a single character.

		   The initial \ has been consumed already when this is called.

		   Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails. */

		function characterEscape() : String? {

			function extendedHexEscape() : Number {
				var k = 0;
				match("{");
				if (!isHexDigit(next))
					throw new SyntaxError("missing hex sequence");
				while (isHexDigit(next()))
					k = k*16 + hexValue(consume());
				match("}");
				return k;
			}

			function hexDigits(n) : Number {
				var k = 0;
				while (n > 0 && isHexDigit(next())) {
					k = k*16 + hexValue(consume());
					--n;
				}
				if (n > 0)
					throw new SyntaxError("hex sequence too short");
				return k;
			}

			var c;
			switch (c = next()) {
			case "f":
				consume();
				return "\f";
			case "n": 
				consume();
				return "\n";
			case "r": 
				consume();
				return "\r";
			case "t":
				consume();
				return "\t";
			case "c": 
				consume();
				let (l = consume()) {
					if (l >= "A" && l <= "Z")
						return String.fromCharCode(l.charCodeAt(0) - "A".charCodeAt(0));
					if (l >= "a" && l <= "z")
						return String.fromCharCode(l.charCodeAt(0) - "a".charCodeAt(0));
					throw new SyntaxError("Bogus \\c sequence");
				}
			case "x": case "X":
				consume();
				return String.fromCharCode(lookingAt("{") ? extendedHexEscape() : hexDigits(2));
			case "u": case "U": 
				consume();
				return String.fromCharCode(lookingAt("{") ? extendedHexEscape() : hexDigits(4));
			case "*END*":
				return null;
			default:
				if (isIdentifierPart(c))
					return null;
				consume();
				return c;
			}
		}

		function eat(c : String!) : Boolean {
			if (lookingAt(c)) {
				consume();
				return true;
			}
			else
				return false;
		}

		function match(c : String!) : void {
			if (!lookingAt(c))
				throw new SyntaxError("Expected character here: " + c);
			consume();
		}

		function lookingAt(c : String!) : void {
			return idx < source.length && source[idx] === c;
		}

		function consume() : String! {
			if (idx < source.length) {
				var c = source[idx++];
				skipBlanksAndComments();
				return c;
			}
			else
				return "*END*";
		}

		function consumeUntil(c : String!) : void {
			while (idx < source.length && source[idx] != c)
				++idx;
			skipBlanksAndComments();
		}

		function skipBlanksAndComments() : void {
			if (!extended)
				return;
			while (idx < source.length) {
				if (source[idx] == '#') {
					while (idx < source.length && !isNewline(source[idx]))
						++idx;
				}
				else if (isBlank(source[idx]) || isNewline(source[idx]))
					++idx;
				else
					return;
			}
		}

		function decimalValue(c) : Number {
			return c.charCodeAt(0) - "0".charCodeAt(0);
		}

		function hexValue(c) : Number {
			if (c >= "0" && c <= "9")
				return decimalValue(c);
			else
				return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
	}
}
