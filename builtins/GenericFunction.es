/* Simple generic functions.

   The GenericFunction constructor takes the number of required
   arguments and a flag stating whether the function can also take
   optional or rest arguments.

   The addMethod method adds a method
*/
package
{
    type Set.<T> = Map.<T,boolean>;

    public class GenericFunction extends Object
    {
        function GenericFunction(required: uint, more: boolean) 
            : required = required
            , more = more
        {
        }

        // Type names used by the translator

        public static const t_null = { "null": true };
        public static const t_undefined = { "undefined": true };
        public static const t_any = { "any": true };
    
        meta final function invoke(...args) {
            // Select applicable methods.

            var ms = methods[args.length];
            var ts = args.map(typeOf(a));
            var as = [];
        outer:
            for ( let i=0 ; i < ms.length ; i++ ) {
                let m = ms[i];
                let s = m.signature;
                for ( let j=0 ; j < args.length ; j++ ) {
                    if (!isSubtype(ts[j], s[j]))
                        continue outer;
                }
                as.push(m);
            }

            // Sort the methods and split the set at the first uncomparable method.

            let uncomparable = new Set.<*>();
            as.sort(methodComparator(ts, uncomparable));
            for ( let i=0 ; i < as.length ; i++ ) {
                if (uncomparable.has(as[i])) {
                    as.length = i;
                    break;
                }
            }

            // Put nextMethod() in as the first argument, then call the
            // methods in order.  nextMethod() throws private exceptions
            // when it's called, and we retry.

            args.unshift(nextMethod);
            for ( let acount=0; acount < as.length ; acount++ ) {
                try {
                    return as[acount].apply(null, args);
                }
                catch (e : NextMethodExn) {
                    if (acount == as.length)
                        throw new NoApplicableMethodError;
                }
                catch (e : NextMethodWithArgsExn) {
                    if (acount == as.length)
                        throw new NoApplicableMethodError;
                    if (args.length-1 != e.args.length)
                        throw new ArgumentCounteExn;
                    for ( let i=1 ; i < args.length ; i++ )
                        args[i] = e.args[i-1];
                }
            }
            throw new NoApplicableMethodError;
        }

        public function addMethod(fixedTypes: Array, body: function()) {
            if (fixedTypes.length != required)
                throw "Generic function requires exactly " + required + " discriminators";
            methods.push({discriminators: fixedTypes, body: body});
        }

        /* For each method we want to know if one precedes the other for
         * at least one particular argument, whether they are the same, or
         * whether they are uncomparable because one precedes the other
         * for one argument and the other precedes the one for another.
         *
         * The function returns 0 if they are not comparable but notes
         * this fact in a separate table.
         */
        static function methodComparator(ts, uncomparable) {
            function cplPrecedes(t, t1, t2) {
                var cpl = classPrecedenceList(t);
                for ( let i=0, limit=cpl.length ; i < limit ; i++ ) {
                    if (cpl[i] == t1)
                        return true;
                    if (cpl[i] == t2)
                        return false;
                }
                return false;
            }

            function cmp(x, y) {
                var precedes=false, 
                    succeeds=false;
                for ( let i=0, limit=ts.length ; i < limit ; i++ ) {
                    if (x.signature[i] == y.signature[i])
                        ;
                    else if (isSubtype(x.signature[i], y.signature[i]))
                        precedes = true;
                    else if (isSubtype(y.signature[i], x.signature[i]))
                        succeeds = true;
                    else if (cplPrecedes(ts[i], x.signature[i], y.signature[i]))
                        precedes = true;
                    else if (cplPrecedes(ts[i], y.signature[i], x.signature[i]))
                        succeeds = true;
                }
                if (precedes && !succeeds)
                    return -1;
                if (succeeds && !precedes)
                    return 1;
                if (precedes && succeeds) {
                    uncomparable.put(x, true);
                    uncomparable.put(y, true);
                }
                return 0;
            }

            return cmp;
        }

        var required;
        var more;
        const methods = [];
    }

    function nextMethod(...args) { 
        if (args.length == 0)
            throw SingletonNextMethodExn;
        else
            throw new NextMethodWithArgsExn(args);
    }

    class NextMethodExn 
    {
        // Nothing to see, move along
    }

    class NextMethodWithArgsExn 
    {
        function NextMethodWithArgsExn(args) : args=args {}
            var args;
    }

    const SingletonNextMethodExn = new NextMethodExn;


    /**********************************************************************
     *
     * Type systems support -- a (working) sketch.
     */

    // Since the ES4 meta objects systems is not yet mature, we
    // represent the type of undefined as undefined, the type of null
    // as null, and the source-level translation must cooperate in
    // this, ie,
    //
    //  generic function f( x: null ) { ... }
    //
    // turns into
    //
    //  f.addMethod( [null], function (nextMethod, x: null) { ... }
    //
    // In practice this does not matter much.
    //
    // We can't handle union types here yet.

    function typeOf(v) {
        switch type (v) {
        case (v:undefined) { return GenericFunction.t_undefined };
        case (v:null) { return GenericFunction.t_null; }
        case (v:*) { return magic::classOf(v) }
        }
    }

    // Subtype relation, simplified.  Must be compatible with the
    // subtype relation in the language, but is simplified for generic
    // functions.
    //
    // t1 is "type undefined", "type null", or a class type
    // t2 can be any of those, an interface type, or a union type
    //
    // We can't handle union types here yet.
     
    function isSubtype(t1, t2) {
        if (t1 === t2)
            return true;

        let cpl = classPrecedenceList(t1);
        for ( let i=0, limit=cpl.length ; i < limit ; i++ )
            if (t2 === cpl[i])
                return true;
        return false;
    }

    /******************************************************************
     *
     * Class precedence list computation.   In a sense, this is not part
     * of the generic functions proposal but a meta-operation on classes
     * and interfaces.
     *
     * This does not need to be fast, it's only done once per type.
     */

    var class_precedences = new Map.<*,Array>();   // Cached result

    function classPrecedenceList(t) {
        if (!class_precedences.has(t)) {
            if (t is Class) 
                class_precedences.put(t, computeClassPrecedences(t));
            else
                class_precedences.put(t, [t]);
        }
        return class_precedences.get(t);
    }

    /* Translated from the Dylan Reference Manual */
    function computeClassPrecedences(t) {

        let function merge(acc, inputs) {

            // Returns c if it can go in the acc now, otherwise false
            let function candidate(c) {

                let function headp(l)
                    l.length > 0 && c == l[0];

                let function tailp(l) {
                    for ( let i=1 ; i < l.length ; i++ )
                        if (c == l[i])
                            return true;
                    return false;
                }

                return (any(inputs, headp) && !any(inputs, tailp)) ? c : false;
            }

            let function candidateDirectSuperclass(c) 
                any(directSuperclasses(c), candidate);

            let function emptyp(l)
                l.length == 0;

            if (Array.every(inputs, emptyp))
                return acc;

            let (next = any(acc, candidateDirectSuperclass)) {
                if (next) {
                    let function removeNext(l) {
                        if (head(l) == next) 
                            l.shift();
                        return l;
                    }
                    acc.push(next);
                    return merge(acc, Array.map(inputs, removeNext));
                }
                else
                    throw "Inconsistent precedence graph";
            }
        }

        let (dsc = directSuperClasses(t)) {
            return merge([c], Array.map(dsc, classPrecedenceList).unshift(dsc));
        }
    }

    function any(a, p) {
        let probe = Array.indexOf(a, p);
        return (probe == -1) ? false : a[probe];
    }

    /* Returns list of any base class plus interfaces, in order.
     *
     * Cache this result.
     *
     * Really want the class object to have this as a method,
     * but this will do.
     */

    var direct_superclasses = new Map.<*,Array>();  // Cached result

    function directSuperClasses(cls) {
        if (!direct_superclasses.has(cls)) {
            let dsci = getDirectSuperClassAndInterfaces(cls);
            direct_superclasses.put(cls, dsci);
        }
        return direct_superclasses.get(cls);
    }

    /* System hook: get the base class and the implemented interfaces.
     *
     * This must return an array of the direct superclass followed by
     * the direct superinterfaces, in declaration order.  Right now we
     * depend on undocumented behavior of the RI: the interfaces are
     * actually in order in the internal data structures.
     *
     * This should be replaced by an appropriate use of the
     * meta-objects functionality, eventually; the hooks used here are
     * those that would be used by that layer.
     */

    function getDirectSuperClassAndInterfaces(clsOrInterface: (Class,Interface)) {
        let supers = [];
        switch type (clsOrInterface) {
        case (cls:Class) {
            let probe = magic::getSuperClass(cls);
            if (probe !== null)
                supers.push(probe);
            for ( let i=0 ; ; i++ ) {
                let probe = magic::getImplementedInterface(cls, uint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        case (iface:Interface) {
            for ( let i=0 ; ; i++ ) {
                let probe = magic::getSuperInterface(iface, uint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        }
        return supers;
    }
}
