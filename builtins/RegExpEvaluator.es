/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "RegExp" object
 * E262-3 15.10
 *
 * Abstract syntax tree classes and evaluation.
 */

package RegExp
{
	private const failure : State? = null;

	private class State!
	{
		var endIndex : int = 0;
		var cap : Array;

		function State(e : int, cap : Array) {
			this.endIdex = e;
			this.cap = cap;
		}
	}

	private class Context!
	{
		const input : String!;
		const inputLength : int;
		const global : Boolean;       // g
		const ignoreCase : Boolean;   // i
		const multiLine : Boolean;    // m
		const noSearch : Boolean;     // y

		function Context(input : String!, flags : String!) 
			: input(input)
			, inputLength(input.length);
			, global(flags.indexOf("g") != -1)
			, ignoreCase(flags.indexOf("i") != -1)
			, multiLine(flags.indexOf("m") != -1)
			, noSearch(flags.indexOf("y") != -1)
		{
		}
	}

	private interface Matcher!
	{
		function match(ctx : Context, x : State, c : Continuation) : State?;
	}

	private type Continuation = function(ctx : Context, x : State) : State?;

	/* Syntax trees double as compiled code.  Since these are syntax
	   trees they are a little more elaborate than they need be, eg
	   AtomDisjunct is not strictly necessary but present for expository
	   purposes.  */

	private class Disjunct! implements Matcher
	{
		function Disjunct(m1 : Matcher, m2 : Matcher) : m1(m1), m2(m2) {}

		function match(ctx : Context, x : State, c : Continuation) : State? {
			let r = m1.match(ctx, x, c);
			if (r !== failure)
				return r;
			return m2.match(ctx, x, c);
		}

		var m1 : Matcher, m2 : Matcher;
	}

	private class Alternative! implements Matcher
	{
		function Alternative(m1 : Matcher, m2 : Matcher) : m1(m1), m2(m2) {}

		function match(ctx : Context, x : State, c : Continuation) : State? {
			return m1.match(ctx, x, function (ctx : Context, y : State) { return m2.match(ctx, y, c); } );
		}

		var m1 : Matcher, m2 : Matcher;
	}

	private class Assertion! implements Matcher
	{
		function match(ctx : Context, x : State, c : Continuation) : State? {
			if (!testAssertion(ctx, x))
				return failure;
			return c(ctx, x);
		}

		abstract function testAssertion(ctx : Context, x : State) : Boolean;
	}

	private class AssertStartOfInput extends Assertion
	{
		function testAssertion(ctx : Context, x : State) : Boolean {
			let e : int = x.endIndex;
			if (e === 0)
				return true;
			if (ctx.multiLine)
				return isTerminator(ctx.input[e-1]);
			return false;
		}
	}

	private class AssertEndOfInput extends Assertion
	{
		function testAssertion(ctx : Context, x : State) : Boolean {
			let e : int = x.endIndex;
			if (e === ctx.inputLength)
				return true;
			if (ctx.multiLine)
				return isTerminator(ctx.input[e-1]);
			return false;
		}
	}

	private class AssertWordboundary extends Assertion
	{
		function testAssertion(ctx : Context, x : State) : Boolean {
			let e : int = x.endIndex;
			return isWordChar(ctx, e-1) !== isWordChar(ctx, e);
		}
	}

	private class AssertNotWordboundary extends Assertion
	{
		function testAssertion(ctx : Context, x : State) : Boolean {
			let e : int = x.endIndex;
			return isWordChar(ctx, e-1) === isWordChar(ctx, e);
		}
	}

	private class Quantified! implements Matcher
	{
		function Quantified(m : Matcher, min : Number, max : Number, greedy : Boolean) 
			: m(m), min(min), max(max), greedy(greedy) {}

		function match(ctx : Context, x : State, c : Continuation) : State? {
			// FIXME
		}

		var m : Matcher, min : Number, max : Number, greedy : Boolean;
	}

	private class Dot! implements Matcher
	{
		function match(ctx : Context, x : State, c : Continuation) : State? {
			let e : int = x.endIndex;
			if (e === ctx.inputLength)
				return failure;
			if (isTerminator(ctx.input[e]))
				return failure;
			return c(ctx, new State(e+1, x.cap));
		}
	}

	private class CharsetMatcher! implements Matcher
	{
		function match(ctx : Context, x : State, c : Continuation) : State? {
			// FIXME
		}
	}

	private class PositiveLookahead! implements Matcher
	{
		function match(ctx : Context, x : State, c : Continuation) : State? {
			let r : State? = m(ctx, x, function (y : State!) : State? { return y });
			if (r === failure)
				return failure;
			return c(new State(x.endIndex, r.cap));
		}
	}

	private class NegativeLookahead! implements Matcher
	{
		function match(ctx : Context, x : State, c : Continuation) : State? {
			let r : State? = m(ctx, x, function (y : State!) : State? { return y });
			if (r !== failure)
				return failure;
			return c(x);
		}
	}

	/*** Character sets ***/

	private interface CharSet!
	{
		function match(c : String!, invert : Boolean) : Boolean;
	}

	private class CharsetUnion implements CharSet 
	{
		function CharsetUnion(m1 : CharSet, m2 : CharSet) : m1(m1), m2(m2) {}

		function match(c : String!, invert : Boolean) : Boolean {
			let res : Boolean = m1.match(c, true) || m2.match(c, true);
			return res !== invert;
		}

		var m1 : CharSet, m2 : CharSet;
	}

	private class CharsetIntersection implements CharSet 
	{
		function CharsetIntersection(m1 : CharSet, m2 : CharSet) : m1(m1), m2(m2) {}

		function match(c : String!, invert : Boolean) : Boolean {
			let res : Boolean = m1.match(c, true) && m2.match(c, true);
			return res !== invert;
		}

		var m1 : CharSet, m2 : CharSet;
	}

	private class CharsetComplement implements CharSet 
	{
		function match(c : String!, invert : Boolean) : Boolean {
			return m.match(c, true) === invert;
		}
	}

	private class CharsetRange implements CharSet 
	{
		function match(c : String!, invert : Boolean) : Boolean {
			// FIXME
		}
	}

	private class CharsetAdhoc implements CharSet 
	{
		function match(c : String!, invert : Boolean) : Boolean {
			// FIXME
		}
	}

	private class CharsetMagic implements CharSet
	{
		function CharsetMagic(name : String!) : name(name) {}

		function match(c : String!, invert : Boolean) : Boolean {
			throw new Error("character set not yet implemented: " + name);
		}

		var name : String!;
	}

	private const chars_space : CharSet = new CharsetAdhoc(["\u0020" /* etc */]);
	private const chars_notspace : CharSet = new CharsetComplement(chars_space);

	private const unicode_named_classes = {
		"L":  new CharsetMagic("Letter"),
		"Lu": new CharsetMagic("Letter, Uppercase"),
		"Ll": new CharsetMagic("Letter, Lowercase"),
		"Lt": new CharsetMagic("Letter, Titlecase"),
		"Lm": new CharsetMagic("Letter, Modifier"),
		"Lo": new CharsetMagic("Letter, Other"),
		"M":  new CharsetMagic("Mark"),
		"Mn": new CharsetMagic("Mark, Nonspacing"),
		"Mc": new CharsetMagic("Mark, Spacing Combining"),
		"Me": new CharsetMagic("Mark, Enclosing"),
		"N":  new CharsetMagic("Number"),
		"Nd": new CharsetMagic("Number, Decimal Digit"),
		"Nl": new CharsetMagic("Number, Letter"),
		"No": new CharsetMagic("Number, Other"),
		"P":  new CharsetMagic("Punctuation"),
		"Pc": new CharsetMagic("Punctuation, Connector"),
		"Pd": new CharsetMagic("Punctuation, Dash"),
		"Ps": new CharsetMagic("Punctuation, Open"),
		"Pe": new CharsetMagic("Punctuation, Close"),
		"Pi": new CharsetMagic("Punctuation, Initial quote (may behave like Ps or Pe depending on usage)"),
		"Pf": new CharsetMagic("Punctuation, Final quote (may behave like Ps or Pe depending on usage)"),
		"Po": new CharsetMagic("Punctuation, Other"),
		"S":  new CharsetMagic("Symbol"),
		"Sm": new CharsetMagic("Symbol, Math"),
		"Sc": new CharsetMagic("Symbol, Currency"),
		"Sk": new CharsetMagic("Symbol, Modifier"),
		"So": new CharsetMagic("Symbol, Other"),
		"Z":  new CharsetMagic("Separator"),
		"Zs": new CharsetMagic("Separator, Space"),
		"Zl": new CharsetMagic("Separator, Line"),
		"Zp": new CharsetMagic("Separator, Paragraph"),
		"C":  new CharsetMagic("Other"),
		"Cc": new CharsetMagic("Other, Control"),
		"Cf": new CharsetMagic("Other, Format"),
		"Cs": new CharsetMagic("Other, Surrogate"),
		"Co": new CharsetMagic("Other, Private Use"),
		"Cn": new CharsetMagic("Other, Not Assigned (no characters in the file have this property)") 
	};
}
