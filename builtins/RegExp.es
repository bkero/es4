/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 *
 * Public RegExp class.
 *
 * The regular expression compiler is in RegExpCompiler.es, and the
 * representation of compiled code plus a tree-walking evaluator is
 * in RegExpEvaluator.es.
 *
 * This program constitutes a definition of regular expression syntax
 * and semantics in the following sense.  The compiler defines the
 * source syntax of regular expressions as a translation to an
 * executable representation (an abstract syntax tree); the evaluator
 * defines the meaning of the expression as a walk of that tree.
 *
 * Because the compiler defines the syntax the ASTs do not capture the
 * syntactic richness of the expression language.
 */

package RegExp
{
	/* Regular expression class */

	dynamic class RegExp
	{
		function RegExp( _source : String!, _flags : String! ) 
			: [matcher, nCapturingParens, names] = compile(_source, _flags)
	        , source = _source
			, flags = _flags
		{
		}

		/* E262-4 draft: RegExp instances are callable */
		intrinsic function call(...args) {
			// FIXME
		}

		RegExp.prototype.toString = function () {
			return this.intrinsic::toString();
		}

		intrinsic function toString() {
			return "/" + (source.length == 0 ? "(?:)" : source) + "/" + flags;
		}

		/* Returns an array of matches, with additional named properties
		   on the array for named submatches */

		private function Match( input : String!, endIndex : int ) : Array {
			let ctx = new Context(input, flags);
			let c = function (x : State) : State? { return x };
			for (;;) {
				let (result = matcher.match(ctx, new State(endIndex, new Array(nCapturingParens)), c)) {
					if (result !== failure) {
						let res = result.cap;
						for (let [i,name] in names) {
							res[name] = res[i];
						}
						return res;
					}
				}

				if (ctx.noSearch)
					return null;
				endIndex++;
				if (endIndex == ctx.inputLength)
					return null;
			}
		}

		private matcher : Matcher;
		private nCapturingParens : int;
		private names : [String]!;
		private source : String!;
		private flags : String!;
	}

	private function isIdentifierStart(c) { // FIXME -- hairy, but not yet important
		return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'z' || c == '_' || c == '$';
	}

	private function isIdentifierPart(c) { // FIXME -- hairy, but not yet important
		return isIdentifierStart(c) || c >= '0' && c <= '9';
	}

	private const char_CR : String! = "\u000D";
	private const char_LF : String! = "\u000A";
	private const char_LS : String! = "\u2028";
	private const char_PS : String! = "\u2029";

	private function isTerminator(c : String!) : Boolean {
		return (c === char_LF || c === char_CR || c === char_LS || c === char_PS);
	}

	private function isWordChar(ctx : Context, e : int) : Boolean {
		if (e === -1 || e === ctx.inputLength)
			return false;
		let c = ctx.input[e];
		return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_';
	}

	private function isHexDigit(c) {
		return c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';
	}
}
