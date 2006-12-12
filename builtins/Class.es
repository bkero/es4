/* -*- mode: java; mode: font-lock; tab-width: 4 -*- 
 *
 * ECMAScript 4 builtins - the "Class" object
 */

package
{
	/* Metaclasses.

	   The class "Class" is the base class for the class metaobjects
	   that are defined as the result of defining new classes, ie, the
	   object that results from defining some class C is an instance
	   of an internal class that directly subclasses Class.

	   Suppose C is defined like this:

	       class C
		   {
		       function C(a,b,c) {
			       ....
			   }
		   }

	   Let [C] denote the metaclass of C.  A class definition C
	   results in the system effectively defining the class [C] as the
	   following; see below for more information:

	       class [C] extends Class 
		   {
			   intrinsic function construct(a,b,c) : C!
			   {
			       const o : C! = super.construct(C);  // allocate and initialize; see below
				   const x : * = o.C(o, a, b, c);      // call the constructor
				   return (x is Object) ? x to C! : o;
			   }

			   // Static methods defined on C appear as instance methods here

		       // Probably much information about C
		   }

		   const C : [C] = new [C];

       where the last "new" is "logical new"; use your imagination.

	   (Note the use of "super" as scope resolution in that method.  A
	   consequence of its existence is that super must be allowed in a
	   similar role in static class methods in user defined classes.)

	   If C contains a "static instrinsic construct()" then that
	   method replaces (without "static") the generated construct
	   function shown above (this follows from normal rules), and the
	   only constraints on it is that it must return an object of type
	   C (or a subclass of C).  It need not allocate storage.  If it
	   does allocate storage, it must follow the protocol shown above,
	   however.

	   Observe that the signature of the generated construct method reflects 
	   the signature of the programmer-defined constructor.

	   The expression "new C(e, ...)" is expanded by the compiler as
	   "C.intrinsic::construct(e,...)".


	   Security note.

	   It seems fairly clear that class "Class" should *not* be
	   accessible to user programs; we don't want people to go around
	   creating metaclasses willy-nilly. */

	class Class 
	{
		/* Becomes available as Object.prototype, Boolean.prototype,
		   and so on */

		const prototype;

		/* Magic: Instantiation.

		   The "intrinsic construct(classobj)" method of the Class
		   metaclass does the following.

		   - allocates an instance o of a particular class (eg, "C") based
		     on information in the classobj
		   - sets the [[Class]] of o to reflect the name of the class
		   - sets the [[Prototype]] of o from classobj.prototype
		   - sets o.prototype to a fresh Object instance, provided the
		     Object class has been initialized.  Otherwise, it sets 
			 o.prototype to null.
		   - sets the constructor field of o.prototype (if not null) to
		     classobj.
		   - initializes fields of o to their defaults, using information
		     from the classobj
		   - returns o.

		   construct() does not call the new object's user-defined
		   constructor, the client must do that.  */

		intrinsic native function construct(classobj : Class!);

		private classname : String;
		
		intrinsic function getClass() : String! { return classname; }

		/* More fields here common to all metaclasses, but these are
		   implementation dependent. */

		// ...
	}
}
