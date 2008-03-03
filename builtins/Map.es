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

package
{
    import ECMAScript4_Internal.*;

    __ES4__ dynamic class Map.<K,V>
    {
        static public const length = 2;

        /* Create the map.  Note that the equality and hashcode
         * predicates must always agree: if two objects are equal,
         * they must hash to the same value.
         */
        /* FIXME: #153, #XXX: support "function" as a type meaning "any function" */
        public function Map(equals   /*: function*/ = (function (x,y) x === y), 
                            hashcode /*: function*/ = intrinsic::hashcode)
            : equals = equals
            , hashcode = function (k) uint(hashcode(k) cast AnyNumber)
            , population = 0
        {
        }

        static meta function invoke(object: Object): Map.<EnumerableId,*> {
            if (object is Map.<*,*>)
                return object;
            let d = new Map.<EnumerableId,*>;
            for (let n in object)
                if (object.intrinsic::hasOwnProperty(n))
                    d.put(n, object[n]);
            return d;
        }

        /* Return the number of mappings in the dictionary */
        intrinsic function size() : uint
            population;

        /* Return the value associated with 'key', or the default
         * value if 'key' does not exist in the dictionary
         */
        intrinsic function get(key: K, notfound: (V|undefined)=undefined) : (V|undefined) {
            let probe = informative::find(key);
            return probe ? probe.value : notfound;
        }

        /* Associate 'value' with 'key', overwriting any previous
         * association for 'key'.  Return the previous associated
         * value, or the default value if 'key' does not exist in the
         * dictionary.
         */
        intrinsic function put(key: K, value: V, notfound: (V|undefined)=undefined) : (V|undefined) {
            let oldvalue = notfound;
            let probe = informative::find(key);
            if (probe) {
                oldvalue = probe.value;
                probe.value = value;
            }
            else {
                ++population;
                informative::insert(key, value);
            }
            return oldvalue;
        }

        /* Return true iff the dictionary has an association for 'key'
         */
        intrinsic function has(key:K) : boolean {
            let probe = informative::find(key);
            return probe ? true : false;
        }

        /* Remove any association for 'key' in the dictionary.  Returns
         * true if an association was in fact removed
         */
        intrinsic function remove(key:K) : boolean {
            let probe = informative::find(key);
            if (probe) {
                --population;
                informative::eject(probe);
                return true;
            }
            return false;
        }

        /* Remove all associations.  Note that this implementation is
         * normative; subclasses that override remove() or
         * iterator::get will be able to observe the calls.
         */
        intrinsic function clear() : void {
            for (let k in this)
                intrinsic::remove(k);
        }

        prototype function size(this: Map.<*,*>)
            this.intrinsic::size();

        prototype function get(this: Map.<*,*>, key)
            this.intrinsic::get(key);

        prototype function put(this: Map.<*,*>, key, value)
            this.intrinsic::put(key, value);

        prototype function has(this: Map.<*,*>, key)
            this.intrinsic::has(key);

        prototype function remove(this: Map.<*,*>, key)
            this.intrinsic::remove(key);

        prototype function clear(this: Map.<*,*>)
            this.intrinsic::clear();

        iterator function get(deep: boolean = false) : iterator::IteratorType.<K>
            iterator::getKeys(deep);

        iterator function getKeys(deep: boolean = false) : iterator::IteratorType.<K>
            helper::iterate.<K>(function (a,k,v) { a.push(k) });

        iterator function getValues(deep: boolean = false) : iterator::IteratorType.<V>
            helper::iterate.<V>(function (a,k,v) { a.push(v) });

        iterator function getItems(deep: boolean = false) : iterator::IteratorType.<[K,V]>
            helper::iterate.<[K,V]>(function (a,k,v) { a.push([k,v]) });

        helper function iterate.<T>(f: function(*,*,*):*) {
            let a = [];
            informative::allElements(function (k,v) { f(a,k,v) });
            return {
                const next:
                    let (i=0, limit=a.length)
                        function () : T {
                            if (i < limit)
                                return a[i++];
                            throw iterator::StopIteration;
                        }
            } : iterator::IteratorType.<T>;
        }

        // Documented behavior: the key in the table is the left
        // operand, while the key we're looking for is the right
        // operand.

        informative function find(key:K): Box.<K,V> {
            let l = null;
            for (l=tbl[hashcode(key) % limit] ; l && !equals(l.key,key) ; l=l.next )
                ;
            return l;
        }

        informative function insert(key:K, value:V) : void {
            if (population > limit*REHASH_UP)
                informative::rehash(true);
            let box = new Box.<K,V>(key, hashcode(key), value);
            let h = box.hash % limit;
            box.prev = null;
            box.next = (tbl[h] || null);
            tbl[h] = box;
        }

        informative function eject(box: Box.<K,V>): void {
            if (box.prev)
                box.prev.next = box.next;
            else
                tbl[box.hash % limit] = box.next;
            if (box.next)
                box.next.prev = box.prev;
            if (population < limit*REHASH_DOWN)
                informative::rehash(false);
        }

        // This is inefficient, it would be better to reuse the existing boxes.
        informative function rehash(grow: boolean) : void {
            let oldtbl = tbl;
            let oldlimit = limit;

            population = 0;
            limit = grow ? limit * 2 : limit / 2;
            tbl = Map.informative::newTbl.<K,V>(limit);

            informative::allElementsCore(oldtbl, 
                                         oldlimit, 
                                         function (k,v) { ++population; informative::insert(k, v) });
        }

        informative function allElements(fn)
            informative::allElementsCore(tbl, limit, fn);

        informative function allElementsCore(tbl, limit, fn) {
            for (let i=0 ; i < limit ; i++)
                for (let p=tbl[i] ; p ; p = p.next)
                    fn(p.key, p.value);
        }

        informative static function newTbl.<K,V>(limit: uint) : [internal::Box.<K,V>] {
            let a = [] : [internal::Box.<K,V>];
            a.limit = limit;
            return a;
        }

        /* These are part of the spec */

        /* FIXME: #153, #XXX: support "function" as a type here */
        private const hashcode /*: function*/;        // key hash function
        private const equals /*: function*/;          // key equality tester
        private var population: uint = 0;             // number of elements in the table */

        /* These are private to the implementation */
        /* We need to have REHASH_UP > REHASH_DOWN*2 for things to work */

        private const REHASH_UP = 1;                  /* rehash if population > REHASH_UP*limit */
        private const REHASH_DOWN = 1/3;              /* rehash if population < REHASH_DOWN*limit */

        private var limit: uint = 10;                 /* number of buckets in the table */

	/*
	 * hash table : FIXME: should be "limit" but fixture is not visible during init (?)
	 *               Also might want to fiddle with the static/instance scope of type params.
	 */
        private var tbl: [internal::Box.<K,V>] = Map.informative::newTbl.<K,V>(10);
    }

    internal class Box.<K,V> 
    {
        function Box(key:K, hash:uint, value:V) 
            : key = key
            , hash = hash
            , value = value
            , prev = null
            , next = null
        {
        }

        var key: K;
        var hash: uint;
        var value: V;
        var prev: Box.<K,V>;
        var next: Box.<K,V>;
    }
}
