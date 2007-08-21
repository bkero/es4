/* -*- mode: java -*-
 *
 * ECMAScript 4 samples - a "Dictionary" object
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

/* Status: proposal, not discussed.
 *
 * Maps are hash tables mapping keys to values.  Keys can be arbitrary
 * values and are not converted in any way.
 *
 * The name "Map" was chosen for its succinctness (and pressure from
 * Brendan :)
 *
 * Key hashing and equality testing are user-selectable.  The default
 * values for hashing and equality use intrinsic::hashcode and
 * intrinsic::=== unless the key objects implement the HashProtocol
 * type, providing their own equals() and hash() functions.
 *
 * The use of HashProtocol is brittle, since users may inadvertently
 * create functions with the right signatures yet not actually intend
 * for them to be used by the Map.
 *
 * This implementation is not testable until we have parameterized
 * types and a bunch of bug fixes.
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
 */

package Library
{
    use namespace intrinsic;

    public class Map.<K,V>
    {
        use default namespace public;

        /* Create the map.  Note that the equality and hashcode
         * predicates must always agree: if two objects are equal,
         * they must hash to the same value.
         */
        function Map(equals=intrinsic::===, hashcode=intrinsic::hashcode) 
            : equals(equals)
            , hashcode(hashcode)
        {
        }

        /* Create a Map from an Object */
        meta static function convert(x : Object!) {
            let d = new Map.<EnumerableId,V>;
            for ( let n in x )
                if (x.hasOwnProperty(n))
                    d.put(n, x[n]);
            return d;
        }

        /* Return the number of mappings in the dictionary */
        function size() : uint
            population;

        /* Return the value associated with 'key', or null if 'key' does
         * not exist in the dictionary
         */
        function get(key: K) : V? {
            let [l] = find(key);
            return l ? l.value : null;
        }

        /* Associate 'value' with 'key', overwriting any previous
         * association for 'key'
         */
        function put(key:K, value:V) : void {
            let [l] = find(key);
            if (l)
                l.value = value;
            else
                insert( key, value );
        }

        /* Return true iff the dictionary has an association for 'key'
         */
        function has(key:K) : boolean {
            let [l] = find(key);
            return l to boolean;
        }

        /* Remove any association for 'key' in the dictionary.  Returns
         * true if an association was in fact removed
         */
        function remove(key:K) : boolean {
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
            let a = [] : [T];
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
            let h = this.hashcode(key) % limit;
            let l = tbl[h];
            let p = null;
            // Documented behavior: the key in the table is the left
            // operand, while the key we're looking for is the right
            // operand.
            while (l && !this.equals(l.key,key)) {
                p = l;
                l = l.link;
            }
            return [l,p];
        }

        private function insert(key:K, value:V) : void {
            let hash = this.hashcode(key);
            if (population > limit*REHASH_UP)
                rehash(true);
            let h = hash % limit;
            let o = {key: key, value: value, link: tbl[h]};
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

        private const REHASH_UP = 1;                  /* rehash if population > REHASH_UP*limit */
        private const REHASH_DOWN = 1/3;              /* rehash if population < REHASH_DOWN*limit */

        private type box = {key:K, value:V, link:* /*box*/};

        private var hashcode : function(K):uint;      /* key hash function (should be const?) */
        private var equals : function (K,K):boolean;  /* key equality tester (should be const?) */
        private var population: uint = 0;             /* number of elements in the table */
        private var limit: uint = 10;                 /* number of buckets in the table */
        private var tbl: [box] = newTbl(limit);       /* hash table */
    }

    /* FIXME: Do we really want this to be parameterized? */
    public interface ObjectIdentity.<T>
    {
        function equals(that: ObjectIdentity.<T>): boolean;
        function hashcode(): uint;
    }

    public class IdentityMap.<K /* implements ObjectIdentity.<K> */, V> extends Map.<K,V> {
        function IdentityMap() 
            : super(function (x: ObjectIdentity.<K>, y: ObjectIdentity.<K>): boolean { return x.equals(y) },
                    function (x: ObjectIdentity.<K>): uint { return x.hashcode() })
        {
        }
    }
}
