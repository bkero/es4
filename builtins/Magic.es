/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - magic functions
 *
 * Magic functions are implementation traps that express aspects of
 * the language that cannot be expressed without either (a) modelling
 * the language in the language, eg, by modelling objects and their
 * property lists, or (b) breaking security.
 *
 * A good example is [[Prototype]], which exists on all objects but
 * which is presumed not to be available for reading or writing by the
 * user program.  We can model it directly using eg ___proto___, but
 * not without creating facilities that are presumed not to exist
 * (reading that field).
 *
 * Library code references magic functions as global functions in the
 * "magic" namespace, eg "magic::getPrototype(o)".
 *
 * Do note that a conforming implementation can't express the magic
 * hooks quite like shown here because the magic namespace pollutes
 * the name space for user programs.  An implementation might instead
 * supply a flag --magic to the compiler that allows the compiler to
 * magically know about "magic" when the library files are being
 * compiled.
 */
package
{
	namespace magic;

	/* Retrieve the [[Class]] property of o */
	native magic function getClassName(o : Object!) : String!;

	/* Retrieve the [[Prototype]] property of o */
	native magic function getPrototype(o : Object!) : Object?;

	/* Return true iff o has a local property named by p. */
	native magic function hasOwnProperty(o : Object!, p : String!) : Boolean!;

	/* Return true iff the property p exists locally on o and is
	   enumerable */
	native magic function getPropertyIsEnumerable(o : Object!, p : String!) : Boolean!;

	/* Provided that the property p exists locally on o, set its enumerable
	   flag according to f.  If the property p does not exist locally on
	   o, it does nothing. */
	native magic function setPropertyIsEnumerable(o : Object!, p : String!, f : Boolean!) : void;

	/* Retrieve the [[Value]] property of o */
	native magic function getValue(o : Object!) : *;

	/* Set the [[Value]] of o to v */
	native magic function setValue(o : Object!, v : *) : void;
		
	/* Compile the function body in the context of the parameter list
	   and return an opaque representation for the compiled code,
	   suitable for passing to magic::invoke.  */
	native magic magic function compile(formals : [String!], body : String!) : *;

	/* Given some code created by magic::compile, an environment of
	   some sort (either a global object or an environment structure
	   created by function closing; we could clean this up), and some
	   argument values, invoke the code on those arguments in the
	   given environment by extending the environment with the
	   formals, binding actuals to formals, and running the code. */
	native magic function invoke(code : *, env : *, args : [*]) : *;

	/* Given a function object, a this object, and an array of argument
	   values, call the function with the this object and arguments. */
	native magic function apply(fn : Function!, t : Object!, args : [*]) : *;
}


