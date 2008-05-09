/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - Generic functions
 *
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 */

/* Very simple generic functions -- demo code.

   The GenericFunction constructor takes the number of required
   arguments and a flag stating whether the function can also take
   optional or rest arguments.

   The addMethod() method adds a method to the function by providing a
   list of specializers and a closure.  The list of specializers must
   be as long as the number of required arguments.  Each specializer
   is a type object.  The first argument to the closure must always be
   "nextMethod" and its type, if any, should be "function()".

   The seal() method sets a flag that makes the function reject
   attempts to add further methods.

   The type objects in the specializer list can be class objects,
   interface objects, or the three objects exported from this file,
   NullType, UndefinedType, and AnyType.  These objects will be replaced
   by something from the meta-objects system eventually.

   There is no way to do union types at this point, the meta-objects
   system must be working.

   A function will be passed through the "nextMethod" argument.  This
   function takes no arguments; it calls the next applicable method
   and returns the result of that call.

   --------------

   Typical usage in the translator.

   The translator will see a generic function definition like this:

       generic function f(a:double, b, c=10, ...rest);

   and will turn it into something like this:

       var f = new GenericFunction([int,AnyType,AnyType], [,,10], true);

   Then methods scattered throughout the program, like these:

       generic function f(a: double, b: boolean, c: AnyNumber, ...rest) {
           return a+b
       }

   are turned into calls to addMethod:

       f.addMethod([int, boolean, AnyNumber], 
                   function (nextMethod, a:double, b:boolean, c:AnyNumber, ...rest) { return a+b })

   Generally few errors are signalled when a method is added, most of
   the interesting work happens when f is called.

   Note that the translator must translate the annotations '*' into
   AnyType, 'null' into NullType, and 'undefined' into UndefinedType.
   Missing annotations must become AnyType.

   --------------

   Implementation.

   Several points to be made here.

   (1) The RI is still pretty weak--no parameterized types for one
       thing--so this implementation has its own Map class and its own
       Set class.  No big deal, just extra code.

   (2) The RI has no real meta-object functionality at this point, so
       this code uses some primitives that have been hacked into the
       system as helper hooks.  These hooks will eventually be useful
       for the meta-objects system anyhow.

   (3) Using arrays like I do here is nuts, we should switch to
       lisp-like lists.  (Why? because when you use an array to
       simulate a list, what's a functional operation--like cdr--in a
       reasonable language is a destructive operation--like
       shift--here.  So you end up copying arrays when you shouldn't
       have to.)  An argument could be made that lisp-like lists
       deserve a standard library of their own, one is sketched below,
       I have not converted the code.

   (4) Few interesting optimizations are implemented; many are possible
       and desirable.

   (5) The class linearization algorithm is out of the Dylan reference
       manual.  There is a more recent version of that algorithm that
       should be used instead -- this will happen eventually.

   (6) The set of "direct superclasses" is taken to be the base class,
       if any, followed by the list of implemented interfaces in order,
       if any.  This seems reasonable but there may or may not be a
       more reasonable approach for ECMAScript, as that is modelled on
       a multiple-inheritance language.  MultiJava may offer insight.

   (7) GenericFunction subclasses Object instead of Function because the
       meta invoke() method of the latter is final.  I don't know why
       it should be, really; it would be reasonable for GenericFunction
       to subclass Function.
*/

    const DEBUG = false;

    class TypeClass 
    {
        function TypeClass(name) 
            : name=name 
        {
        }
            
        var name;
    }

    public const NullType = new TypeClass("null");
    public const UndefinedType = new TypeClass("undefined");
    public const AnyType = new TypeClass("any");
    
    public class GenericFunction extends Object
    {
        function GenericFunction(constraints:Array, returnConstraint:Object, defaults:Array, more: boolean) 
            : required = constraints.length
            , constraints = constraints
            , returnConstraint = returnConstraint
            , defaults = defaults
            , more = more
        {
        }

        public function addMethod(specializers: Array, returnType: Object, body: function()) {
            if (sealed)
                throw "Adding methods to a sealed generic function is not allowed.";

            if (specializers.length != required)
                throw "Generic function requires exactly " + required + " specializers";

            for ( let i=0, limit=specializers.length ; i < limit ; i++ )
                if (!isSubtype(specializers[i], constraints[i]))
                    throw "Specializer type at position " + i + " is not subtype of declared constraint";

            if (!isSubtype(returnType, returnConstraint))
                throw "Return type is not subtype of declared constraint";

            for ( let i=0, limit=methods.length ; i < limit ; i++ ) {
                let thesame = true;
                let m_specializers = methods[i].specializers;
                for ( let j=0, limit=specializers.length ; thesame && j < limit ; j++ )
                    if (specializers[j] !== m_specializers[j])
                        thesame = false;
                if (thesame)
                    throw "Generic method redefinition is not allowed.";
            }

            methods.push({specializers: specializers, body: body});
        }

        public function seal() {
            sealed = true;
        }

        meta final function invoke(...args) {
            checkCongruence(args);
            
            let types = computeManifestTypes(args);
            let [applicable, uncomparable] = sortMethods(selectApplicableMethods(methods, types), types);

            return callMethodsInOrder(applicable, args);
        }

        function checkCongruence(args) {
            let nactuals = args.length;
            if (nactuals > required && !more)
                throw new TypeError("Too many arguments to generic function");
            if (nactuals < required) {
                if (defaults == null || !defaults.hasOwnProperty(nactuals))
                    throw new TypeError("Not enough arguments to generic function");
                for ( let i=nactuals ; i < required ; i++ )
                    args[i] = defaults[i];
            }
        }

        function computeManifestTypes(args) {
            let types = [];
            for ( let i=0 ; i < required ; i++ )
                types[i] = typeOf(args[i]);
            return types;
        }

        function selectApplicableMethods(methods, types) {
            let applicable = [];
        outer:
            for ( let i=0, limit=methods.length ; i < limit ; i++ ) {
                let s = methods[i].specializers;
                for ( let j=0, limit=types.length ; j < limit ; j++ ) {
                    if (!isSubtype(types[j], s[j]))
                        continue outer;
                }
                applicable.push(methods[i]);
            }
            if (applicable.length == 0)
                throw new TypeError("No applicable methods");
            return applicable;
        }

        function sortMethods(applicable, types) {
            let uncomparable = new Set();
            applicable.sort(methodComparator(types, uncomparable));

            for ( let i=0, limit=applicable.length ; i < limit ; i++ ) {
                if (uncomparable.has(applicable[i])) {
                    let rest = applicable.slice(i);
                    applicable.length = i;
                    if (applicable.length == 0)
                        throw new TypeError("No unambiguous method");
                    return [applicable, rest];
                }
            }

            return [applicable, []];
        }

        function callMethodsInOrder(applicable, args) {
            if (DEBUG)
                printApplicableMethods(applicable);

            let applicable_index = 0;
            args.unshift(function () { 
                             if (applicable_index == applicable.length)
                                 throw new TypeError("No next method");
                             else
                                 return applicable[applicable_index++].body.apply(null, args);
                         });
            return applicable[applicable_index++].body.apply(null, args);
        }

        /* For each method we want to know if one precedes the other for
         * at least one particular argument, whether they are the same, or
         * whether they are uncomparable because one precedes the other
         * for one argument and the other precedes the one for another.
         *
         * The function returns 0 if they are not comparable but notes
         * this fact in a separate table.
         */
        static function methodComparator(types, uncomparable) {
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
                for ( let i=0, limit=types.length ; i < limit ; i++ ) {
                    if (x.specializers[i] == y.specializers[i])
                        ;
                    else if (isSubtype(x.specializers[i], y.specializers[i]))
                        precedes = true;
                    else if (isSubtype(y.specializers[i], x.specializers[i]))
                        succeeds = true;
                    else if (cplPrecedes(types[i], x.specializers[i], y.specializers[i]))
                        precedes = true;
                    else if (cplPrecedes(types[i], y.specializers[i], x.specializers[i]))
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

        const required;
        const defaults;
        const constraints;
        const returnConstraint;
        const more;
        const methods = new Array;
        var   sealed = false;
    }

    /******************************************************************
     *
     * Class precedence list computation.   In a sense, this is not part
     * of the generic functions proposal but a meta-operation on classes
     * and interfaces.
     *
     * This does not need to be fast, it's only done once per type.
     */

    var class_precedences = new Map();   // Cached result

    function classPrecedenceList(t) {
        if (!class_precedences.has(t)) {
            if (t is Class) 
                class_precedences.put(t, computeClassPrecedences(t));
            else
                class_precedences.put(t, [t]);
        }
        return copyArray(class_precedences.get(t));
    }

    /* Translated from the Dylan Reference Manual */
    function computeClassPrecedences(t) {

        function merge(acc, inputs) {

            // Returns c if it can go in the acc now, otherwise false
            function candidate(c) {

                function headp(l)
                    c === l[0];

                function tailp(l) 
                    Array.indexOf(l, c, 1) != -1;

                return (any(inputs, headp) && !any(inputs, tailp)) ? c : false;
            }

            function candidateDirectSuperclass(c)
                any(directSuperClasses(c), candidate);

            function emptyp(l)
                l.length === 0;

            if (Array.every(inputs, emptyp))
                return acc;

            let (next = any(acc, candidateDirectSuperclass)) {
                if (next) {
                    let removeNext = 
                        function (l) {
                            if (l[0] === next)
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
            return merge([t], prepend(dsc, Array.map(dsc, classPrecedenceList)));
        }
    }

    function any(a, p) {
        for ( let i=0, limit=a.length ; i < limit ; i++ ) {
            let x = p(a[i]);
            if (x)
                return x;
        }
        return false;
    }

    function prepend(x, a) {
        a.unshift(x);
        return a;
    }

    /* Returns list of any base class plus interfaces, in order.
     *
     * Cache this result.
     *
     * Really want the class object to have this as a method,
     * but this will do.
     */

    var direct_superclasses = new Map();  // Cached result

    function directSuperClasses(cls) {
        if (!direct_superclasses.has(cls))
            direct_superclasses.put(cls, getDirectSuperClassAndInterfaces(cls));
        return copyArray(direct_superclasses.get(cls));
    }

    function copyArray(x) {
        let result = [];
        for ( let i=0, limit=x.length ; i < limit ; i++ )
            if (i in x)
                result[i] = x[i];
        return result;
    }


    /**********************************************************************
     *
     * System hooks -- a sketch for meta-objects functionality.
     */

    public function typeOf(v) {
        switch type (v) {
        case (v:undefined) { return UndefinedType }
        case (v:null) { return NullType }
        case (v:*) { return helper::getClassOfObject(v) }
        }
    }

    // Subtype relation, simplified.  Must be compatible with the
    // subtype relation in the language, but is simplified for generic
    // functions.
    //
    // t1 is "type undefined", "type null", or a class type
    // t2 can be any of those, an interface type, a union type, or "*"
    //
    // We can't handle union types here yet.
     
    function isSubtype(t1, t2) {
        if (t2 === AnyType)
            return true;

        if (t1 === t2)
            return true;

        if (t1 === UndefinedType || t1 === NullType)
            return t1 === t2;

        if (t2 === UndefinedType || t2 === NullType)
            return false;

        let cpl = classPrecedenceList(t1);
        for ( let i=0, limit=cpl.length ; i < limit ; i++ )
            if (t2 === cpl[i])
                return true;
        return false;
    }


    /* Get the base class and the implemented interfaces.
     *
     * This must return an array of the direct superclass followed by
     * the direct superinterfaces, in declaration order.  Right now we
     * depend on undocumented behavior of the RI: the interfaces are
     * actually in order in the internal data structures.
     */

    function getDirectSuperClassAndInterfaces(clsOrInterface: (Class,Interface)) {
        let supers = [];
        switch type (clsOrInterface) {
        case (cls:Class) {
            let probe = helper::getSuperClass(cls);
            if (probe !== null)
                supers.push(probe);
            for ( let i=0 ; ; i++ ) {
                let probe = helper::getImplementedInterface(cls, toUint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        case (iface:Interface) {
            for ( let i=0 ; ; i++ ) {
                let probe = helper::getSuperInterface(iface, toUint(i));
                if (probe === null)
                    break;
                supers.push(probe);
            }
        }
        }
        return supers;
    }


    /**********************************************************************
     *
     * Debugging functionality 
     */

    function printApplicableMethods(applicable) {
        print(applicable.length + " applicable functions");
        for ( let i=0 ; i < applicable.length ; i++ ) { 
            let s = applicable[i].specializers;
            let n = "  ";
            for ( let j=0 ; j < s.length ; j++ )
                n = n + nameThatType(s[j]) + " ";
            print(n);
        }
    }

    /* lame-o */
    function nameThatType(x) {
        if (x == Object) return "Object";
        if (x == String) return "String";
        if (x == string) return "string";
        if (x == int) return "int";
        if (x == double) return "double";
        if (x == AnyType) return "*";
        if (x == Number) return "Number";
        return "???";
    }


    /**********************************************************************
     *
     * Data structures that go away later 
     */

    class Set {
        function put(X) {
            if (Array.indexOf(contents, X) == -1)
                contents.push(X);
        }

        function has(X)
            Array.indexOf(contents, X) != -1;

        var contents = [];
    }

    class Map {
        function get(X) {
            let probe = Array.indexOf(keys, X);
            if (probe == -1)
                return null;
            return values[probe];
        }

        function put(X, Y) {
            let probe = Array.indexOf(keys, X);
            if (probe == -1) {
                keys.push(X);
                values.push(Y);
            }
            else
                values[probe] = X;
        }

        function has(X)
            Array.indexOf(keys, X) != -1;

        var keys = new Array;
        var values = new Array;
    }
