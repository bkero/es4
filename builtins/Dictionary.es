/* -*- mode: java -*-
 *
 * ECMAScript 4 samples - a "Dictionary" object
 *
 * Status: proposal, not discussed.
 *
 * Dictionaries are hash tables mapping values to values.
 *
 * These are built on top of Object.intrinsic::hashcode() for hashing
 * and the iterator protocol for enumeration.  
 *
 * Misc:
 *
 *  - Flash has a "Dictionary" class that's pretty different:
 *       http://livedocs.adobe.com/flex/2/langref/flash/utils/Dictionary.html
 *
 * Implementation notes:
 *
 * This implementation uses chaining for collisions and load factors
 * to trigger rehashing; neither is normative.
 *
 * A good implementation would defer creating the iterator
 * intermediate array until insertions or deletions make it necessary
 * to create it.
 *
 * The implementation uses an ad-hoc weak slot mechanism whereby a
 * property on an object is registered as weakly held, and when the GC
 * discovers that the value in the location is only referenced from
 * locations that hold it weakly then it clears the location, clears
 * the "weak" bit, and calls the callback with the object, the
 * property name, and the value that was weakly held.  This is an
 * implementation artifact; other mechanisms could be used (eg
 * Lisp-style weak cells with periodic sweeping of the table).  No
 * guarantees about promptness are implied.
 */

public class Dictionary.<K,V> {

    public function Dictionary(weak: boolean = false) : weak=weak {
    }

    /* This is a little weird.  We assume we only want to capture
     * names that have no namespace, and ignore the others.  Also,
     * the key type 'K' must be 'String' for this to work.
     */
    public function to Dictionary(x : Object!) {
        let d = new Dictionary.<String,V>;
        for ( let n in x )
            if (!(n is Name))
                if (x.hasOwnProperty(n))
                    d.put(n, x[n]);
        return d;
    }

    /* Return the number of mappings in the dictionary */
    public function size() : uint {
        return population;
    }

    /* Return the value associated with 'key', or null if 'key' does
     * not exist in the dictionary
     */
    public function get(key: K) : V? {
        let [l] = find(key);
        return l ? l.value : null;
    }

    /* Associate 'value' with 'key', overwriting any previous
     * association for 'key'
     */
    public function put(key:K, value:V) : void {
        let [l] = find(key);
        if (l)
            l.value = value;
        else
            insert( key, value );
    }

    /* Return true iff the dictionary has an association for 'key'
     */
    public function has(key:K) : boolean {
        let [l] = find(key);
        return l to boolean;
    }

    /* Remove any association for 'key' in the dictionary.  Returns
     * true if an association was in fact removed
     */
    public function remove(key:K) : boolean {
        let [l,p] = find(key);
        if (l) {
            if (p)
                p.link = l.link;
            else
                tbl[h] = l.link;
            population -= 1;
            if (population < limit*REHASH_DOWN)
                rehash(false);
            return true;
        }
        return false;
    }

    iterator function get(deep: boolean = false) : iterator::IteratorType.<K> 
        getKeys(deep);

    iterator function getKeys(deep: boolean = false) : iterator::IteratorType.<K> 
        iterate.<K>(function (a,k,v) { a.push(k) });

    iterator function getValues(deep: boolean = false) : iterator::IteratorType.<V>
        iterate.<V>(function (a,k,v) { a.push(v) });

    iterator function getItems(deep: boolean = false) : iterator::IteratorType.<[K,V]>
        iterate.<[K,V]>(function (a,k,v) { a.push([k,v]) });

    private function iterate.<T>(f: function(k,v):*) {
        let a: [T] = [] : [T];
        allElements( tbl, limit, function (k,v) { f(a,k,v) } );
        let i = 0;
        return { 
            next: function () : T {
                if (i === a.length)
                    throw StopIteration;
                return a[i++];
            }
        };
    }

    private function find(key:K): [box,box] {
        let h = K.intrinsic::hashcode() % limit;
        let l = tbl[h];
        let p = null;
        while (l && l.key !== key) {
            p = l;
            l = l.link;
        }
        return [l,p];
    }

    private function insert(key:K, value:V) : void {
        let hash = K.intrinsic::hashcode();
        if (population > limit*REHASH_UP)
            rehash(true);
        let h = hash % limit;
        let o = {key: key, value: value, link: tbl[h]};
        if (weak)
            magic::setWeaklyHeld(o, "key", function (o,k,v) { remove(v) });
        tbl[h] = o;
        population += 1;
    }

    private function rehash(grow: boolean) : void {
        let oldtbl = tbl;
        let oldlimit = limit;

        population = 0;
        limit = grow ? limit * 2 : limit / 2;
        tbl = newTbl(limit);

        allElements( oldtbl, oldlimit, insert );
    }

    private function allElements(tbl: [box], limit: uint, fn: function (K,V):*) {
        for ( let i=0 ; i < limit ; i++ )
            for ( let p=tbl[i] ; p ; p = p.link )
                fn(p.key, p.value);
    }

    private static function newTbl(limit: uint) : [box] {
        let a = [] : [box];
        a.limit = limit;
        return a;
    }

    /* We need to have REHASH_UP > REHASH_DOWN*2 for things to work */

    private const REHASH_UP = 1;           /* rehash if population > REHASH_UP*limit */
    private const REHASH_DOWN = 1/3;       /* rehash if population < REHASH_DOWN*limit */

    private type box = {key:K, value:V, link:* /*box*/};

    private var population: uint = 0;      /* number of elements in the table */
    private var limit: uint = 10;          /* number of buckets in the table */
    private var tbl: [box] = newTbl(limit);/* hash table */

    private const weak: boolean;           /* True if keys are weakly held */
}
