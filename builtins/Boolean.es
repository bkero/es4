/* -*- mode: java; mode: font-lock; tab-width: 4; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Boolean" object
 *
 * E262-3 15.6
 * E262-4 19.x
 * E262-4 proposals:builtin_classes
 */

package
{
    /* Notionally, the boolean constants "true" and "false" reference
       instances of the Boolean class.  These two instances are
       necessarily magically constructed.

       The Boolean constructor returns one of these instances, so
       there are never more than two Boolean instances in the system.

       The Boolean class is final and non-dynamic because most
       implementations will in fact represent Boolean objects not
       using objects, but using some magic tagged value, of which
       there will be only two, one for "true" and the other for
       "false".
     */
    final class Boolean! extends Object
    {       
        /* E262-3 15.6.1: The Boolean Constructor Called as a Function. */
        static intrinsic function call(value) : Boolean {
            return value ? true : false;
        }

        /* E262-3 15.6.2.1: The Boolean Constructor. */
        function Boolean(value) {
            return value ? true : false;
        }

        /* E262-3 15.6.4.2: Boolean.prototype.toString.  */
        prototype function toString(this : Boolean) {
            return this.intrinsic::toString();
        }

        /* E262-4 draft ch 19 */
        intrinsic function toString() : String {
            return this === true ? "true" : "false";
        }
        
        /* E262-3 15.6.4.3: Boolean.prototype.valueOf. */
        prototype function valueOf(this : Boolean) {
            return this;
        }

        /* E262-4 draft ch 19 */
        intrinsic function valueOf() : Boolean {
            return this;
        }
    }
}
