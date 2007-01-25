/* -*- indent-tabs-mode: nil -*- 
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

    /* --------------------------------------------------------------

       PROPERTY MANIPULATION.  */

    /* Retrieve the [[Class]] property of o */
    magic native function getClassName(o : Object!) : String!;

    /* Retrieve the possibly null [[Prototype]] property of o */
    magic native function getPrototype(o : Object!) : Object;

    /* Return true iff o has a local property named by p. */
    magic native function hasOwnProperty(o : Object!, p : String!) : Boolean;

    /* Return true if the property p does exists locally on o and its
       DontEnum bit is set */
    magic native function getPropertyIsDontEnum(o : Object!, p : String!) : Boolean;

    /* Return true if the property p does exists locally on o and its
       DontDelete bit is set */
    magic native function getPropertyIsDontDelete(o : Object!, p : String!) : Boolean;

    /* Provided that the property p exists locally on o, set its DontEnum
       flag according to f.  If the property p does not exist locally on
       o, it does nothing. */
    magic native function setPropertyIsDontEnum(o : Object!, p : String!, f : Boolean) : void;

    /* Retrieve the [[Value]] property of o */
    magic native function getValue(o : Object!) : *;

    /* Set the [[Value]] of o to v */
    magic native function setValue(o : Object!, v : *) : void;


    /* ----------------------------------------------------------------

       FUNCTION MANIPULATION.  */

    /* Compile the function body in the context of the formals and
       return an opaque representation for the compiled code, suitable
       for passing to magic::invoke, along with the arity of the
       function.

       Throws a SyntaxError if the code could not be compiled. */
    magic native function compile(formals : String!, body : String!) : [*, Number];

    /* Given some code created by magic::compile, an environment of
       some sort (either a global object or an environment structure
       created by function closing; we could clean this up), and some
       argument values, invoke the code on those arguments in the
       given environment by extending the environment with the
       formals, binding actuals to formals, and running the code. */
    magic native function invoke(code : *, env : *, args : Array) : *;

    /* Given a function object, a this object, and an array of argument
       values, call the function with the this object and arguments. */
    magic native function apply(fn : Function!, t : Object!, args : Array) : *;


    /* ----------------------------------------------------------------

       STRING MANIPULATION.  Strings contain string data in some
       unspecified way - there is no representation of string data in
       the language.  The following magic functions access and set
       those string data.  */

    /* Given a String object 'src', copy its internal string data into
       another String object 'dest', replacing whatever data might
       have been in 'dest' to begin with.  */
    magic native function setStringValue(dest : String!, src : String!) : void;

    /* Given a string and a position in that string, return the
       numeric value of the character at that position in the
       string.  */
    magic native function charCodeAt(s : String!, pos : uint) : String!;

    /* Given a numeric character value, return a string of length 1
       whose element 0 is the character with that same value.  */
    magic native function fromCharCode(ch : uint) : String!;

    /* Given a string object, return the number of characters in the
     * string. */
    magic native function stringLength(s : String!) : uint;

    /* Given two string objects A and B , return a new string object
       containing the characters from A followed by the characters
       from B.  */
    magic native function stringAppend(a : String!, b : String!) : String!;
}

