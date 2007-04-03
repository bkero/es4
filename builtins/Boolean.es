/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Boolean" wrapper object
 *
 * E262-3 15.6
 * E262-4 19.x
 * E262-4 proposals:builtin_classes
 */

package
{
    use default namespace public;

    dynamic class Boolean
    {       
        /* E262-3 15.6.1: The Boolean Constructor Called as a Function. */
        meta static function invoke(value) : boolean
	    boolean(value);

        /* E262-4 draft ch 19 */
	private final function toString() : string
            this ? "true" : "false";

	/* E262-4 early-binding variant. */
        override intrinsic function toString() : string
	    private::toString();
	
        /* E262-3 15.6.4.2: Boolean.prototype.toString.  */
        prototype function toString(this : Boolean) 
            this.private::toString();


        /* E262-4 draft ch 19 */
	private final function valueOf() : boolean
	    boolean(this);

	/* E262-4 early-binding variant. */
        override intrinsic function valueOf() : boolean
            private::valueOf();

        /* E262-3 15.6.4.3: Boolean.prototype.valueOf. */
        prototype function valueOf(this : Boolean)
            this.private::valueOf();
    }
}
