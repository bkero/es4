// the only way to construct this is as "new GenericFunction(...)"
//
// The surface syntax
//   generic function f(a, b);
// turns into
//   var f = new GenericFunction("...args", "return (this function).genericDispatch2(args)")

// Can't extend Function because the "invoke" function in Function is final


class GenericFunction extends Object
{
    private static type Set.<T> = Map.<T,boolean>;

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

    public function addShape(fixed: uint, optional: uint=0, rest: boolean=false) {
        ...
    }

    public function addMethod(fixedTypes: Array, body: function()) {
        ...
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

    const methods = []
}


/* Example:

generic function f(x, y);

generic function f(x:int, y) { return x*2 + 4 }

generic function f(x, y:string) { return y-12 + x }
*/

var f;
if (!(f is GenericFunction))
    f = new GenericFunction;
f.addShape(2);
f.addMethod([int, type *], function (nextMethod: function(), x:int, y) { return x*2 + 4 });
f.addMethod([type *, string], function (nextMethod: function(), x, y:string) { return y-12 + x });


