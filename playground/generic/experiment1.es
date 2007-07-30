/* Experiment 1: generic instancde functions, no rest args, no optional
   args, a single shape only.  nextMethod is provided.

   If the shape of the function is constrained, the compiler can check
   that the constraints are valid during translation, it doesn't show up
   here.
*/

/* Consider a class: */

class C {
    generic function f(x, y);

    generic function f(x:int, y) { 
    }
    generic function f(x, y:string) { 
    }
}

/* This becomes rewritten as follows.  Notice that the individual methods
   are turned into real methods, this is to get the scoping right.  The
   data structure maps lists of argument type objects to methods.  The
   structure has one entry for each arity.

   Also note that '*' is expanded as '(null,undefined,Object)' which
   is nuttily expensive but OK for expository purposes.

   Snag: null and undefined are structural types, and null could easily
   be considered a member of Object...

   (Unfortunately instance creation becomes a little expensive here
   because closures are extracted for the methods with bound "this".
   A more practical system can optimize this by passing the "this"
   object and method pointers separately, or by some other trampoline.
   Another cost is the consing of argument lists etc, these are often
   liftable, though not if types internal to the class are used.)

   (The use of ...args is not required, it just simplifies.  A faster
   system would have dispatch1(), dispatch2(), ... up to a reasonable
   number of fixed arguments.)
*/

class C 
{
    private function __meth1_f(nextMethod, x:int, y) { }
    private function __meth2_f(nextMethod, x, y:string) { }

    private var __generic_f = {
        2: [ { signature: [int, type null],    method: __meth1_f },
             { signature: [int, type undefined],    method: __meth1_f },
             { signature: [int, Object],    method: __meth1_f },
             { signature: [type null, string], method: __meth2_f },
             { signature: [type undefined, string], method: __meth2_f },
             { signature: [Object, string], method: __meth2_f } ] };

    function f(...args) {
        GenericFunction.dispatch(__generic_f, args);
    }
}

/* The package GenericFunction contains the following.

   For this to work we need:
    - "type x" must yield a type object in an expression context
    - function typeOf(obj) to extract an object describing its type
    - function isSubtype(t1, t2) which returns true iff t1 is a subtype of t2
    - function classPrecedenceList(cls) which returns the class precedence list
      of a class (in the form of an array), with cls at the start.  This 
      should return [t] for non-class types t.

   It is possible the functions can be implemented with the meta-objects proposal.
*/

type Set.<T> = Map.<T,boolean>;

package GenericFunction 
{
    import TypeOperations.*;  // see typeops.es

    public function dispatch(g, args) {
        // Select applicable methods.

        var ms = g[args.length];
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

    /* For each method we want to know if one precedes the other for
     * at least one particular argument, whether they are the same, or
     * whether they are uncomparable because one precedes the other
     * for one argument and the other precedes the one for another.
     *
     * The function returns 0 if they are not comparable but notes
     * this fact in a separate table.
     */
    function methodComparator(ts, uncomparable) {
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
            var precedes=false, succeeds=false;
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
}


