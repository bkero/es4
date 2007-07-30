package TypeOperations
{
    var class_precedences = new Map.<*,Array>();

    public function classPrecedenceList(t) {
        if (!class_precedences.has(t)) {
            if (t is Class) 
                class_precedences.put(t, computeClassPrecedences(t));
            else
                class_precedences.put(t, [t]);
        }
        return class_precedences.get(t);
    }

    /* Translated from the DRM */
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

            if (every(inputs, emptyp))
                return acc;

            let (next = any(acc, candidateDirectSuperclass)) {
                if (next) {
                    let function removeNext(l) {
                        if (head(l) == next) 
                            l.shift();
                        return l;
                    }
                    acc.push(next);
                    return merge(acc, map(inputs, removeNext));
                }
                else
                    throw "Inconsistent precedence graph";
            }
        }

        let (dsc = directSuperClasses(t)) {
            return merge([c], map(dsc, classPrecedenceList).unshift(dsc));
        }
    }

    var direct_superclasses = new Map.<*,Array>();

    function directSuperClasses(cls) {
        // returns list of any base class plus interfaces, in order
        // cache this result.
        // really want the class object to have this as a method.
        if (!direct_superclasses.has(cls)) {
            let dsc = ...;
            direct_superclasses.put(cls, dsc);
        }
        return direct_superclasses.get(cls);
    }

    function every(a, p) {
        for ( let i=0, limit=a.length ; i < limit ; i++ ) {
            let v = a[i];
            if (!p(v))
                return false;
        }
        return true;
    }

    function any(a, p) {
        for ( let i=0, limit=a.length ; i < limit ; i++ ) {
            let v = a[i];
            if (p(v))
                return v;
        }
        return false;
    }

    function map(a, f) {
        let r = [];
        for ( let i=0, limit=a.length ; i < limit ; i++ )
            r[i] = f(a[i]);
        return r;
    }

    function nextMethod(...args) { 
        if (args.length == 0)
            throw SingletonNextMethodExn
            else
                throw new NextMethodWithArgsExn(args);
    }
}