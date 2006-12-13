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