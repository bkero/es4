/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "Namespace" object
 *
 * E262-4 not yet documented
 */

package
{
    use default namespace public;

    intrinsic class Namespace
    {
        // FIXME: we might need a static make-a-user-namespace 
        // function here for dynamic namespaces. Not a ctor!
        //
        // For bootstrapping purposes, the ctor of Namespace must *not* 
        // call magic::foo on any foo, since it'll feed back on itself 
        // during construction of the magic namespace itself. So we
        // insist on an empty ctor for the time being.
    }
}
