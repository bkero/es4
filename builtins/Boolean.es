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
    use namespace intrinsic;

    dynamic class Boolean
    {    
        /* E262-3 15.6.1: The Boolean Constructor Called as a Function. */
        meta static function invoke(x=null) : boolean
	    boolean(x);

        /* E262-3 15.6.2: The Boolean Constructor. */
        function Boolean(x=null)
            magic::bindBoolean(this, x);

	/* E262-4 early-binding variant. */
        override intrinsic function toString() : string
            this ? "true" : "false";
	
        /* E262-3 15.6.4.2: Boolean.prototype.toString.  */
        prototype function toString() {
            if (this is Boolean)
                return this ? "true" : "false";
            throw new TypeError("Boolean.prototype.toString called on incompatible " + typeof(this));
        }

	/* E262-4 early-binding variant. */
        override intrinsic function valueOf() : boolean
            boolean(this);

        /* E262-3 15.6.4.3: Boolean.prototype.valueOf. */
        prototype function valueOf()
        {
            if (this is Boolean)
                return boolean(this);
            throw new TypeError("Boolean.prototype.valueOf called on incompatible " + typeof(this));
        }
    }
}
