/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 *
 * Public RegExp class, plus some private utility functions.
 *
 * See RegExpCompiler.es for the compiler.
 * See RegExpEvaluator.es for the evaluator and compiled code representation.
 */

package RegExp
{
	/* E262-3 15.10: Regular expression object */
	public dynamic class RegExp
	{
		/* E262-3 15.10.3.1: The RegExp constructor called as a function */
		public static intrinsic function call( pattern, flags ) {
			if (pattern is RegExp && flags === intrinsic::undefined)
				return pattern;
			else
				return new RegExp(pattern, flags);
		}

		/* E262-3 15.10.4.1: The RegExp constructor */
		public function RegExp( pattern, flags ) {
			let source : String! = "";
			let multiline : Boolean, ignorecase : Boolean, global : Boolean, extended : Boolean, nosearch : Boolean;

			if (pattern is RegExp) {
				if (flags === intrinsic::undefined) {
					source = pattern.source;
					flags = pattern.flags;
				}
				else 
					throw new TypeError("Illegal construction of regular expression");
			}
			else {
				source = pattern === intrinsic::undefined ? "" : String(pattern);
				flags = flags === intrinsic::undefined ? "" : String(flags);
			}

			let (usedflags : Object! = {}) {
				for each ( let f : String! in flags.split("") ) {
					if (f in usedflags)
						throw new SyntaxError("Duplicated flag: " + f);
					usedflags[f] = true;
					switch (f) {
					case "m": multiline = true; break;
					case "i": ignorecase = true; break;
					case "g": global = true; break;
					case "x": extended = true; break;
					case "y": nosearch = true; break;
					default: throw new SyntaxError("Invalid flag: " + f);
					}
				}
			}

			this.matcher = (new RegExpCompiler(source, flags)).compile();

			this.multiline = multiline;
			this.ignorecase = ignorecase;
			this.global = global;
			this.extended = extended;
			this.nosearch = nosearch;
			this.lastIndex = 0;
			this.source = source;
			this.flags = flags;
		}

		/* E262-4 draft: RegExp instances are callable, and a call to
		   an instance is equivalent to calling its exec() method */
		public intrinsic function call(s) : Array {
			return this.intrinsic::exec(s);
		}

		/* E262-3 15.10.6.2: RegExp.prototype.exec */
		RegExp.prototype.exec = function (s) {
			return this.intrinsic::exec(s);
		}

		public intrinsic function exec(s : String!) : Array {
			let S = intrinsic::ToString(s);
			let length = S.length;
			let i = intrinsic::ToInteger(lastIndex);
			if (!global)
				i = 0;
			let res = failure;
			for (;;) {
				if (i < 0 || i > length) {
					lastIndex = 0;
					return null;
				}
				res = matcher.match(s, i);
				if (res !== failure)
					break;
				++i;
			}
			if (global)
				lastIndex = res.endIndex;
			let a = new Array(res.cap.length+1);
			a.index = i;
			a.input = S;
			a.length = res.cap.length+1;
			a[0] = S.substring(i,res.endIndex);
			for ( var j=1 ; j <= res.cap.length ; j++ )
				a[j] = res.cap[j];
			return a;
		}

		/* E262-3 15.10.6.3: RegExp.prototype.test */
		RegExp.prototype.test = function (s) {
			return this.intrinsic::test(s);
		}

		public intrinsic function test(s : String!) : Boolean {
			return this.intrinsic::exec(s) != null;     // Spec says !=, not !==
		}

		/* E262-3 15.10.6.4: RegExp.prototype.toString */
		RegExp.prototype.toString = function () {
			return this.intrinsic::toString();
		}

		public intrinsic function toString() : String! {
			return "/" + (source.length == 0 ? "(?:)" : source) + "/" + flags;
		}

		/* E262-3 15.10.7: properties of regexp instances */
		public const multiline  : Boolean;
		public const ignoreCase : Boolean;
		public const global     : Boolean;
		public const extended   : Boolean;  // E262-4 proposals:extend_regexps
		public const nosearch   : Boolean;  // E262-4 proposals:extend_regexps
		public const source     : String!;
		public var   lastIndex  : Number;

		/* Internal */
		const matcher : RegExpMatcher;      // the internal [[Match]] property
		const flags   : String!;            // redundantly, the original flags string
	}

	/* Captures array.

	   This captures array can be an array that's copied like the
	   E262-3 states, or it could be a functional data structure.  */

	type CapArray = [String];

	function makeCapArray(nCapturingParens : uint) {
		var a = [] : [String];
		for ( let i : uint = 0 ; i <= nCapturingParens ; i++ )
			a[i] = null;
		return a;
	}

	function copyCapArray(a : CapArray, parenIndex : uint, parenCount : uint) : CapArray {
		let b : String = [] : [String];
		for ( let i : uint = 0 ; i < a.length ; i++ )
			b[i] = a[i];
		for ( let k : uint = parenIndex+1 ; k <= parenIndex+parenCount ; k++ )
			b[i] = null;
		return b;
	}


	/* Encapsulation of compiled regular expression as returned by the
	   compiler.  */

	class RegExpMatcher!
	{
		function RegExpMatcher(matcher : Matcher, nCapturingParens : int, names : [String]!) {
			this.matcher = matcher;
			this.nCapturingParens = nCapturingParens;
			this.names = names;
		}

		/* Returns an array of matches, with additional named properties
		   on the array for named submatches */

		function match( input : String!, endIndex : int ) : MatchResult {
			return matcher.match(new Context(input, flags),
								 new State(endIndex, makeCapArray(nCapturingParens)), 
								 function (x : State) : State? { return x });
		}

		var matcher : Matcher;
		var nCapturingParens : int;
		var names : [String]!;
	}

	function isWordChar(ctx : Context, e : int) : Boolean {
		if (e === -1 || e === ctx.inputLength)
			return false;
		let c = ctx.input[e];
		return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_';
	}
}
