/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Class" object
 *
 * E262-4 proposals:meta_objects
 * E262-4 not yet documented
 */

package
{
    use default namespace public;

    class Class
    {
        /* Becomes available as Object.prototype, Boolean.prototype,
           and so on */

        const prototype;

        function Class(value)
            magic::copyValue(value, this);

        private var classname : String;
        
        intrinsic function getClass() : String! { return classname; }
    }
}
