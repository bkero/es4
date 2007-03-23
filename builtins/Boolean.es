/* -*- mode: java -*- 
 *
 * ECMAScript 4 builtins - the "Boolean" wrapper object
 *
 * E262-3 15.6
 * E262-4 19.x
 * E262-4 proposals:builtin_classes
 */

package
{
    namespace core;

    dynamic class Boolean extends Object
    {       
        /* E262-3 15.6.1: The boolean Constructor Called as a Function. */
        core static function invoke(value) : Boolean
	    value ? true : false;

        /* E262-3 15.6.2.1: The boolean Constructor. */
        function Boolean(value) {
	    magic::setValue(this, value ? true : false);
	}


        /* E262-4 draft ch 19 */
	private final function toString() : string
            magic::getValue(this) ? "true" : "false";
	
        /* E262-3 15.6.4.2: Boolean.prototype.toString.  */
        prototype function toString(this : Boolean) 
            this.private::toString();

	/* Early-binding variant. */
        override intrinsic function toString() : string
	    private::toString();



        /* E262-4 draft ch 19 */
	private final function valueOf() : boolean
	    magic::getValue(this);

        /* E262-3 15.6.4.3: Boolean.prototype.valueOf. */
        prototype function valueOf(this : Boolean)
            this.private::valueOf();

	/* Early-binding variant. */
        override intrinsic function valueOf() : boolean
            private::valueOf();
    }
}
