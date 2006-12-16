/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 * E262-4 proposals:extend_regexps
 *
 * Status: not reviewed against specs; not tested.
 *
 * See RegExpCompiler.es for the compiler.
 * See RegExpEvaluator.es for the evaluator and compiled code representation.
 */

package RegExp
{
	import Unicode.*;
	use strict;

	/* E262-3 15.10: Regular expression object */
	public dynamic class RegExp
	{
		/* E262-3 15.10.3.1: The RegExp constructor called as a function */
		static intrinsic function call( pattern, flags ) {
			if (pattern is RegExp && flags === intrinsic::undefined)
				return pattern;
			else
				return new RegExp(pattern, flags);
		}

		/* E262-3 15.10.4.1: The RegExp constructor */
		public function RegExp( pattern, flags ) {
			let source : String! = "";

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

			let usedflags : Object! = { m: null, i: null, g: null, x: null, y: null };

			for each ( let f : String! in flags.split("") ) {
				if (!(f in usedflags))
					throw new SyntaxError("Invalid flag: " + f);
				if (usedflags.f is Boolean)
					throw new SyntaxError("Duplicated flag: " + f);
				usedflags[f] = true;
			}

			this.matcher = (new RegExpCompiler(source, flags)).compile();

			this.multiline = usedflags.m is Boolean ? usedflags.m : false;
			this.ignoreCase = usedflags.i is Boolean ? usedflags.i : false;
			this.global = usedflags.g is Boolean ? usedflags.g : false;
			this.extended = usedflags.x is Boolean ? usedflags.x : false;
			this.nosearch = usedflags.y is Boolean ? usedflags.y : false;
			this.lastIndex = 0;
			this.source = source;
		}

		/* E262-4 proposals:extend_regexps: RegExp instances are
		   callable, and a call to an instance is equivalent to
		   calling its exec() method.
		*/
		function call(s) : Array {
			return this.intrinsic::exec(s);
		}

		intrinsic function call(s : String!) : Array {
			return this.intrinsic::exec(s);
		}

		/* E262-3 15.10.6.2: RegExp.prototype.exec */
		intrinsic function exec(s : String!) : Array {
			let S : String! = intrinsic::ToString(s);
			let length : uint = S.length;
			let i : Number = intrinsic::ToInteger(lastIndex);
			if (!global)
				i = 0;
			let res : MatchResult = failure;
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
			let a : Array = new Array(res.cap.length+1);
			a.index = i;
			a.input = S;
			a.length = res.cap.length+1;
			a[0] = S.substring(i,res.endIndex);
			for ( var j=1 ; j <= res.cap.length ; j++ )
				a[j] = res.cap[j];
			return a;
		}

		prototype function exec(s) {
			return this.intrinsic::exec(s);
		}

		/* E262-3 15.10.6.3: RegExp.prototype.test */
		intrinsic function test(s : String!) : Boolean {
			return this.intrinsic::exec(s) !== null;
		}

		prototype function test(s) {
			return this.intrinsic::test(s);
		}

		/* E262-3 15.10.6.4: RegExp.prototype.toString */
		intrinsic function toString() : String! {
			return "/" + (source.length == 0 ? "(?:)" : source) + "/" + flags;
		}

		prototype function toString() {
			return this.intrinsic::toString();
		}

		/* E262-3 15.10.7: properties of regexp instances */
		public const multiline  : Boolean;
		public const ignoreCase : Boolean;
		public const global     : Boolean;
		public const extended   : Boolean;  // E262-4 proposals:extend_regexps
		public const noSearch   : Boolean;  // E262-4 proposals:extend_regexps
		public const source     : String!;
		public var   lastIndex  : Number;

		/* Internal */
		const matcher : RegExpMatcher;      // The [[Match]] property

		function get flags() : String! {
			return (multiline ? "m" : "") +
				   (ignoreCase ? "i" : "") +
				   (global ? "g" : "") +
				   (extended ? "x" : "") +
				   (noSearch ? "y" : "");
		}
	}

}
