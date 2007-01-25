Resolution December meeting: 

  * "intrinsic" is a namespace
  * "prototype" is not a namespace
  * "intrinsic function" implies final, DontDelete, ReadOnly
  * "intrinsic" properties are never dynamic.  This implies that intrinsic::NaN references the global NaN
    value provided the class or its base classes do not define a property of that name.
  * "prototype function" implies DontEnum

Open questions for January meeting:

  * Can an intrinsic method be extracted?  If so, all of the intrinsics in the
    library need this:T annotations.
  * Are library classes "final", "dynamic"?  Esp String should probably be final, non-dynamic.

Code style:

  * Generally put "use namespace intrinsic" at the top of the package
    and use public:: or other qualifiers when necessary to invoke
    subclassed methods

  * Prototype functions should defer to intrinsic functions:

      prototype function toString()
          this.toString()   /* implicitly calls this.intrinsic::toString */

  * Use these global, intrinsic-only conversion functions for conversions:

     - ToObject
     - ToString
     - ToNumber
     - ToInt
     - ToUint
     - ToDouble
     - ToDecimal
     - ToBoolean
     - ToInteger
     - ToPrimitive

  * Use the "magic" namespace for magic things

  * Use native functions to access system data and the C library

  * Use === and !=== unless there is very good reason to use == or !=.

  * To test whether a value v is undefined, use "v === undefined" (or
    "v === intrinsic::undefined" if you're not using "use namespace
    intrinsic").  Any compiler worth its salt gets this right.  Using
    "void 0" is legal (and the compiler should get that right too) but
    obscure, sort of like shifting for division.

  * Use "while (true) { ... }" for infinite loops, not "for (;;) { ... }"

Formatting style:

  * Don't use tabs

  * Indent 4 spaces

  * No line longer than 110 characters (not counting linebreak)

  * Opening brace for package, class on next line, aligned:
       class Bar extends Foo 
       {

  * Opening brace for everything else on the same line
       function foo() {
       if (fnord > chthulu) {
       else {

  * Closing brace always on line by itself:

       }
       else {

  * Semicolons always (helps Emacs figure things out)

  * Expression functions when natural

  * Cite the relevant specs for every function

  * Use "let" or "let const" (rather than "var" or "const") to bind variables 


/*
	Attempt at modeling ECMA-262 builtin classes using the new ECMA4 language.
	
	Note that these are intended to reflect ECMA-262 behavior, which may omit
	common-but-nonstandard extensions used in various implementations (e.g., SpiderMonkey).
	Such extensions may be specified using a well-defined namespace:

		namespace SpiderMonkey		// Mozilla's ECMA-262 implementation
		namespace ActionScript		// Adobe Flash ActionScript 3.0 implementation
		namespace JScriptNET		// Microsoft JScript implementation

	There is also the ECMA4 namespace, which is designed to allow for higher-performance
	early-binding by ECMA4-specific compilers, allowing for potentially better compile-time
	error-checking (in strict mode) and runtime performance, but at the expense of some backwards compatibility.
	
	Note that where method implementations are provided, there is no attempt at efficiency;
	clarity and simplicity are preferred in all cases.

	Some notes on the syntax, which deviates from ECMA4 in a few ways:

	--  By default, declared properties inside a class are {DontEnum,DontDelete}, and are part of the 
		type constraint for the class type, instance type, or prototype that they’re on.

		* native: means "provided by the underlying implementation".
	    * dynamic: means {!DontDelete}, in effect describing the initial state of an object that can change at runtime.
	    * static: means the property is declared on C itself, not C.prototype or instances of C.
	    * this:T if given as the first parameter of an unbound (prototype or anonymous) function, 
			the type of this is constrained to T. Error on bound (instance) functions. 
			In static functions, use of this is an error.
	    * anonymous and global functions have this:Object by default
	    * {DE} = {DontEnum}
	    * {DD} = {DontDelete}
	    * {RO} = {ReadOnly}

	class C 
	{
	    function							// {DD,DE,RO} bound to instance, this:C
	    static function						// {DD,DE,RO} bound to C, this causes error
	    prototype.g = function(this:Object) // generic dynamic prototype function, e.g. Array.slice()
											// note that this syntax does not mark the function as {DE},
											// but that is almost universally done for prototype functions,
											// and is accomplished via a separate call
	}

*/
