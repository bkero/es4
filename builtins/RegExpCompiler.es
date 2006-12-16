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
 * Status: Complete; Not reviewed; Not tested.
 */

package RegExp
{
	import Unicode.*;
	use strict;

	class RegExpCompiler
	{
		// Invariant: either idx==source.length or source[idx] is a significant char

		const source : String!;        // expression source, sans leading and trailing /
		var   idx : uint;              // current character in the source
		const extended : Boolean;      // true iff expression has /x flag
		const names : [String] = [];   // capturing names, or null for capturing exprs that are not named
		const parenIndex : uint = 0;   // number of capturing parens (including those that are named)
		const parenCount : uint = 0;   // current depth of capture nesting

		function RegExpCompiler( source : String!, flags : String! )
			: extended = flags.indexOf("x") != -1
			, source = source
			, idx = 0
		{
			skip();
		}

		function compile() : RegExpMatcher {
			var p : Matcher = pattern();
			if (p === null)
				throw new SyntaxError("Empty regular expression");
			if (idx !== source.length)
				throw new SyntaxError("Invalid character in input");
			return new RegExpMatcher(p, parenIndex, names);
		}

		function pattern() : Matcher {
			return disjunction();
		}

		function disjunction() : Matcher {
			var alt : Matcher = alternative();
			if (alt == null)
				return new Empty;
			if (eat("|")) {
				var dis : Matcher = disjunction();
				if (dis === null)
					throw new SyntaxError("Missing disjunction following '|'");
				return new Disjunct(alt, dis);
			}
			else
				return alt;
		}

		function alternative() : Matcher? {
			var t : Matcher? = term();
			if (t === null)
				return null;
			for (;;) {
				var p : Matcher? = term();
				if (p === null)
					return t;
				t = new Alternative(t, p);
			}
		}

		function term() : Matcher? {
			var x : Matcher? = assertion();
			if (x !== null)
				return x;
			var x : Matcher? = atom();
			if (x === null)
				return null;
			var y : [Number, Number, Boolean]? = quantifier();
			if (y === null)
				return x;
			var [min, max, greedy] : [Number,Number,Boolean] = y;
			return new Quantified(parenIndex, parenCount, min, max, greedy);
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
				var min : Number = decimalDigits();
				var max : Number = n;
				if (eat(",")) {
					if (eat("}"))
						max = Infinity;
					else {
						max = decimalDigits();
						match("}");
					}
				}
				if (intrinsic::isFinite(max) && max < min)
					throw new SyntaxError("max must be at least as large as min");
				return [min,max];
			}
			else 
				return null;
		}

		function atom() : Matcher {
			if (eat("."))
				return new CharsetMatcher(charset_notlinebreak);

			if (eat("(?:")) {
				let d : Matcher = disjunction();
				match(")");
				return d;
			}

			if (eat("(?=")) {
				let d : Matcher = disjunction();
				match(")");
				return new PositiveLookahead(d);
			}

			if (eat("(?!")) {
				let d : Matcher = disjunction();
				match(")");
				return new NegativeLookahead(d);
			}

			if (eat("(?#")) {
				consumeUntil(")");
				match(")");
				return new Empty;
			}
			
			if (eat("(?P<")) {
				let name : String! = identifier();
				match(">");
				let capno : uint = parenIndex++;
				parenCount++;
				let d : Matcher = disjunction();
				parenCount--;
				match(")");
				for each ( let n : String! in names ) {
					if (n === name)
						throw new SyntaxError("Multiply defined capture name: " + name );
				}
				names[capno] = name;
				return new Capturing(d, capno);
			}

			if (eat("(?P=")) {
				let name : String! = identifier();
				match(")");
				for ( let [i,n] : [String!,String] in names ) {
					if (n === name)
						return new Backref(uint(i));
				}
				throw new SyntaxError("Unknown backref name " + name );
			}

			if (eat("(?"))
				throw new SyntaxError("Bogus (? pattern");

			if (eat("(")) {
				let capno : uint = parenIndex++;
				parenCount++;
				let d : Matcher = disjunction();
				parenCount--;
				match(")");
				return new Capturing(d, capno);
			}

			if (lookingAt("[")) 
				return characterClass();

			if (lookingAt("\\"))
				return atomEscape();

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

			return new CharsetMatcher(new CharsetAdhoc[consume()]);
		}

		function atomEscape() : Matcher {

			function decimalEscape(t : Number) : Matcher {
				if (t >= nCapturingParens)
					throw new SyntaxError("Illegal backreference " + t);
				if (t === 0)
					return new CharsetMatcher(new CharsetAdhoc(["\x00"]));
				else {
					largest_backref = Math.max(largest_backref, t);  // Will check validity later
					return new Backref(t);
				}
			}

			function characterClassEscape(t : Charset) : Matcher {
					return new CharsetMatcher(t);
			}

			function characterEscape(t : String) : Matcher {
				return new CharsetMatcher(new CharsetAdhoc([t]));
			}

			return escape( decimalEscape, characterClassEscape, characterEscape );
		}

		function characterClass() : Matcher {
			match("[");
			var inverted : Boolean = false;
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

			let s : Charset = nonemptyClassRanges();
			for (;;) {
				if (eat("\\&")) {
					let t : Charset = nonemptyClassRanges();
					s = CharsetIntersect(s, t);
				}
				else if (eat("\\-")) {
					let t : Charset = nonemptyClassRanges();
					s = CharsetSubtract(s, t);
				}
				else
					break;
			}
			return s;
		}

		function nonemptyClassRanges(acc : Charset? = null) : Charset {

			function accumulate(acc : Charset?, x : Charset) : Charset {
				return acc === null ? x : CharsetUnion(acc, x);
			}

			let a1 : Charset = classAtom();

			if (lookingAt("]"))
				return a1;

			if (lookingAt("-")) {
				consume();
				if (lookingAt("]"))
					return accumulate(acc, CharsetAdhoc(["-"]));

				let a2 : Charset = classAtom();
				let a3 : Charset = accumulate(acc, CharsetRange(a1, a2));
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
			return escape( function(t : Number) : Charset new CharsetAdhoc([String.fromCharCode(t)]),
						   function(t : Charset) : Charset t,
						   function(t : String) : Charset new CharsetAdhoc(t) );
		}

		/* Parse an escape sequence. */
		function escape( de : function (Number) : (Matcher,Charset),
						 ce : function (Charset) : (Matcher,Charset),
						 ch : function (String) : (Matcher,Charset),
						 allow_b : Boolean ) : (Matcher,Charset) {
			consumeChar("\\");

			let (t : Number? = decimalEscape()) {
				if (t !== null) 
					return de(t);
			}

			let (t : Charset? = characterClassEscape()) {
				if (t !== null)
					return ce(t);
			}

			let (t : String = characterEscape(allow_b)) {
				if (t !== null)
					return ch(t);
			}

			throw new SyntaxError("Failed to match AtomEscape");
		}

		/* Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails.

		   The initial \ has been consumed already.	*/

		function characterClassEscape() : Charset? {
			var c : String!;
			switch (c = peekChar()) {
			case "d": match("d"); return charset_digit;
			case "D": match("D"); return charset_notdigit;
			case "s": match("s"); return charset_space;
			case "S": match("S"); return charset_notspace;
			case "w": match("w"); return charset_word;
			case "W": match("W"); return charset_notword;
			case "p": 
			case "P":
				match(c + "{");
				let (name : String! = identifier()) {
					match("}");
					let (cls : Charset? = unicodeClass(name, c === 'P')) {
						if (!cls)
							throw new ReferenceError("Unsupported unicode character class " + name);
						return cls;
					}
				}
			default:
				return null;
			}
		}

		/* Normally returns a single number;

		   Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails.

		   The initial \ has been consumed already.	*/

		function decimalEscape() : Number? {
			switch (peekChar()) {
			case "0": case "1": case "2": case "3": case "4": 
			case "5": case "6": case "7": case "8": case "9":
				let (k : Number = 0) {
					while (isDecimalDigit(c = peekChar()))
						k = k*10 + consumeChar(c).charCodeAt(0) - "0".charCodeAt(0));
					skip();
					return k;
				}
			default:
				return null;
			}
		}

		/* Normally returns a single character.

		   The initial \ has been consumed already when this is called.

		   Returns null if it does not consume anything but fails;
		   throws an error if it consumes and then fails. */

		function characterEscape(allow_b : Boolean) : String? {

			function hexValue(c) : Number {
				if (c >= "0" && c <= "9")
					return c.charCodeAt(0) - "0".charCodeAt(0);
				else
					return c.toUpperCase().charCodeAt(0) - "A".charCodeAt(0) + 10;
			}

			function extendedHexEscape() : Number {
				var k : Number = 0;
				var c;
				match("{");
				if (!isHexDigit(next))
					throw new SyntaxError("missing hex sequence");
				while (isHexDigit(c = peekChar()))
					k = k*16 + hexValue(consumeChar(c));
				skip();
				match("}");
				return k;
			}

			function hexDigits(n : uint) : Number {
				var k : Number = 0;
				var c;
				while (n > 0 && isHexDigit(c = peekChar())) {
					k = k*16 + hexValue(consumeChar(c));
					--n;
				}
				if (n > 0)
					throw new SyntaxError("hex sequence too short");
				skip();
				return k;
			}

			var c : String!;
			switch (c = next()) {
			case "b":
				if (allow_b)
					return "b";
				else
					return null;
			case "f":
				match(c);
				return "\f";
			case "n": 
				match(c);
				return "\n";
			case "r": 
				match(c);
				return "\r";
			case "t":
				match(c);
				return "\t";
			case "c": 
				match("c");
				let (l : String! = peekChar()) {
					if (l >= "A" && l <= "Z") {
						match(l);
						return String.fromCharCode(l.charCodeAt(0) - "A".charCodeAt(0));
					}
					if (l >= "a" && l <= "z") {
						match(l);
						return String.fromCharCode(l.charCodeAt(0) - "a".charCodeAt(0));
					}
					throw new SyntaxError("Bogus \\c sequence");
				}
			case "x": case "X":
				match(c);
				return String.fromCharCode(lookingAt("{") ? extendedHexEscape() : hexDigits(2));
			case "u": case "U": 
				match(c);
				return String.fromCharCode(lookingAt("{") ? extendedHexEscape() : hexDigits(4));
			case "*END*":
				return null;
			default:
				if (isIdentifierPart(c))
					return null;
				match(c);
				return c;
			}
		}


		/* If c matches the next characters (not skipping blanks), consume those
		   characters and any intertoken space following, and return true.
		   Otherwise return false.  */

		function match(c : String!) : void {
			if (!eat(c))
				throw new SyntaxError("Expected token here: " + c);
		}

		function eat(c : String!) : Boolean {
			if (!lookingAt(c))
				return false;
			idx += c.length;
			skip();
			return true;
		}

		function lookingAt(c : String!) : void {
			for ( var i=0 ; i < c.length && i+idx < source.length ; i++ )
				if (c[i] != source[i+idx])
					return false;
			return true;
		}

		function identifier() : String! {
			let name : String = null;
			if (idx < source.length) {
				let c : String! = source[idx++];
				if (!isIdentifierStart(c))
					throw new SyntaxError("Expected identifier");
				let name = c;
				while (idx < source.length && isIdentPart(source[idx]))
					name += source[idx++];
				skip();
				return name;
			}
			else
				throw new SyntaxError("Expected identifier");
		}
		
		//////

		function peekChar() {
			if (idx < source.length)
				return source[idx];
			else
				return "*END*";
		}

		function consumeChar(c : String!) : void {
			if (idx < source.length && source[idx] == c)
				++idx;
			else
				throw new SyntaxError("Expected " + c);
		}

		function consumeUntil(c : String!) : void {
			while (idx < source.length && source[idx] != c)
				++idx;
			skip();
		}

		function skip() : void {
			if (!extended)
				return;

			// FIXME: also skip unicode format control characters
			while (idx < source.length) {
				if (source[idx] == '#') {
					while (idx < source.length && !isTerminator(source[idx]))
						++idx;
				}
				else if (isBlank(source[idx]) || isTerminator(source[idx]))
					++idx;
				else
					return;
			}
		}
	}
}
