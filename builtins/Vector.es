/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Vector" object
 * ES-262-4 draft
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
 *
 * Status: not reviewed against specs.
 */

package
{
    use default namespace public;
    import ECMAScript4_Internal.*;

    final class Vector.<T>
    {
        function Vector(length: uint=0, fixed: boolean=false) 
            : fixed = fixed
        {
            setLength(length);
        }

        function get length()
            informative::getLength();

        function set length(len: uint) {
            if (fixed)
                throw new RangeError();
            informative::setLength(len);
        }

        meta function get(idx: uint): T {
            if (idx >= length)
                throw new RangeError();
            return informative::getValue(idx);
        }

        meta function set(idx: uint, v: T) {
            if (fixed && idx > length-1 || !fixed && idx > length)
                throw new RangeError();
            informative::setValue(idx, v);
        }

        function toString()
            join();

        function toLocaleString() {
            function cleanString(s) {
                if (s === undefined || s === null)
                    return "";
                return Object(s).toLocaleString();
            }

            let limit = length;
            let separator = ",";

            if (limit == 0)
                return "";

            let s = cleanString(this[0]);
            for ( let i=1 ; i < limit ; i++ )
                s = s + separator + cleanString(this[i]);
            return s;
        }

        function concat(...items): Vector.<T> {
            let v = new Vector.<T>;
            let k = 0;

            // Note that "each" enumeration here and below do the right thing.
            for each ( let x in this )
                v[k++] = x;
            // The type annotation here performs type checking, do not remove it.
            for each ( let item: Vector.<T> in items )
                for each ( let x in item )
                    v[k++] = x;

            return v;
        }

        function every(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (!checker(this[i], i, thisObj))
                    return false;
            return true;
        }

        function filter(checker: Checker, thisObj: Object=null): Vector.<T> { 
            var result = new Vector.<T>;
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker(this[i], i, thisObj))
                    result.push(this[i]);
            return result;
        }

        function forEach(eacher: Eacher, thisObj: Object=null): void { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                eacher(this[i], i, thisObj);
        }

        function indexOf(value: T, from: Numeric=0): Numeric {
            let start = ...; /* FIXME */
            for ( let i=start, limit=length ; i < limit ; i++ )
                if (this[i] === value)
                    return i;
            return -1;
        }

        function join(separator: string=","): string {
            function cleanString(s) {
                if (s === undefined || s === null)
                    return "";
                return string(s);
            }

            let limit = length;
            if (limit == 0)
                return "";

            let s = cleanString(this[0]);
            for ( let i=1 ; i < limit ; i++ )
                s = s + separator + cleanString(this[i]);
            return s;
        }

        function lastIndexOf(value: T, from: Numeric=Infinity): Numeric { 
            let start = ...; /* FIXME */
            for ( let i=start ; i >= 0 ; i-- )
                if (this[i] === value)
                    return i;
            return -1;
        }

        function map(mapper:Mapper, thisObj:Object=null) { 
            var result = new Vector.<T>(length);
            for ( let i=0, limit=length ; i < limit ; i++ )
                result[i] = mapper(this[i], i, thisObj);
            return result;
        }

        function pop(): T {
            if (length == 0)
                return undefined;

            let v = this[length];
            length--;
            return v;
        }

        function push(...items): uint {
            for ( let i=0, limit=items.length ; i < limit ; i++ )
                this[length] = items[i];
            return length;
        }

        function reverse(): Vector.<T> {
            for ( let i=0, j=length-1 ; i < j ; i++, j-- )
                [this[i], this[j]] = [this[j], this[i]];
            return this;
        }

        function shift(): T {
            if (length == 0)
                return undefined;
            let v = this[0];
            for ( let i=1, limit=length ; i < limit ; i++ )
                this[i-1] = this[i];
            length--;
            return v;
        }

        function slice(start: uint, end: uint=uint.MAX_VALUE): Vector.<T> {
            /* FIXME */
        }

        function some(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker(this[i], i, thisObj))
                    return true;
            return false;
        }

        // Signature of comparefn too constraining?
        //
        // Note that with comparefn required, various strange
        // behaviors surrounding how sorting are gone.  But a good
        // spec of this function would provide an alternative
        // sortCompare method, probably, and then parameterize a "sort
        // engine" that could be defined in the Array package and used
        // there as well.
        //
        // FIXME.

        function sort(comparefn: function(T, T): Numeric) 
            informative::sort(comparefn);

        function splice(start: uint, deleteCount: uint, ...items) {
            /* FIXME */
        }

        function unshift(...items): uint {
            let numitems = items.length;
            let oldlimit = length;
            let newlimit = oldlimit + numitems;

            for ( let i=0 ; i < numitems ; i++ )
                this[newlimit-i] = this[oldlimit-i];
            for ( let i=0 ; i < numitems ; i++ )
                this[i] = items[i];
            return newlength;
        }

        var fixed: boolean;

        informative function getLength()
            storage.length;

        informative function setLength(newlength: uint) {
            let oldlength = storage.length;
            if (newlength > oldlength) {
                storage.length = newlength;
                for ( let i=oldlength ; i < newlength ; i++ )
                    storage[i] = undefined;
            }
            else
                storage.length = newlength;
        }

        informative function getValue(idx: uint)
            storage[idx];

        informative function setValue(idx: uint, val: T)
            storage[idx] = val;

        informative function sort(comparefn)
            Array.sort(storage, comparefn);

        private const storage: T = new [T];
    }
}

