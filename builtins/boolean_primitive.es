/* -*- mode: java -*- 
 *
 * ECMAScript 4 builtins - the "boolean" object
 *
 * E262-3 15.6
 * E262-4 19.x
 * E262-4 proposals:builtin_classes
 */

package
{
    use default namespace public;

    /* 
     * The boolean class is final and non-dynamic because most
     * implementations will in fact represent boolean objects not
     * using objects, but using some magic tagged value, of which
     * there will be only two, one for "true" and the other for
     * "false".
     * 
     * The literals true and false denote instances of this class.
     */
    final class boolean! extends Boolean
    {       
        /* E262-3 15.6.1: The boolean Constructor Called as a Function. */
        meta static function invoke(value) : boolean
            value ? true : false;

        /* E262-4 draft ch 19 */
        override intrinsic function toString() : string
            this === true ? "true" : "false";

        /* E262-4 draft ch 19 */
        override intrinsic function valueOf() : boolean
            this;
    }
}
