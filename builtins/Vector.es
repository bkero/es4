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
 * Status: not reviewed against specs, not tested.
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

        meta static function invoke(object) {
            let length = uint(object.length);
            let result = new Vector.<T>(length);
            for ( let i=0 ; i < length ; i++ )
                result[i] = object[i];
            return result;
        }

        function get length()
            informative::getLength();

        function set length(len: Numeric) {
            if (fixed)
                throw new RangeError();
            if (!helper::isIntegral(idx) || idx < 0 || idx > uint.MAX_VALUE)
                throw new RangeError();
            informative::setLength(len);
        }

        meta function get(name): T {
            if (name is Numeric) {
                let idx = name;
                if (!helper::isIntegral(idx) || idx < 0 || idx >= length)
                    throw new RangeError();
                return informative::getValue(uint(idx));
            }
            else 
                this.intrinsic::get(name);
        }

        meta function set(name, v) {
            if (name is Numeric) {
                let idx = name;
                let value: T = v;  // Note, effectful
                if (!helper::isIntegral(idx) || 
                    idx < 0 || 
                    fixed && idx >= length || 
                    !fixed && idx > length)
                    throw new RangeError();
                informative::setValue(uint(idx), value);
            }
            else
                this.intrinsic::set(name, v);
        }

        function toString()
            join();

        function toLocaleString() {
            let limit = length;
            let separator = ",";
            let s = "";
            let i = 0;

            while (true) {
                let x = this[i];
                if (x !== undefined && x !== null)
                    s += x.toLocaleString();
                if (++i == limit)
                    break;
                s += separator;
            }
            return s;
        }

        function concat(...items): Vector.<T> {
            let v = new Vector.<T>;
            let k = 0;

            for ( let i=0 ; i < length ; i++ )
                v[k++] = this[i];

            for ( let j=0 ; j < items.length ; j++ ) {
                let item: Vector.<T> = items[j];         // Note the type check is effectful
                for ( let i=0 ; i < item.length ; i++ )
                    v[k++] = item[i];
            }

            return v;
        }

        function every(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (!checker.call(thisObj, this[i], i, this))
                    return false;
            return true;
        }

        function filter(checker: Checker, thisObj: Object=null): Vector.<T> { 
            var result = new Vector.<T>;
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker.call(thisObj, this[i], i, this))
                    result.push(this[i]);
            return result;
        }

        function forEach(eacher: Eacher, thisObj: Object=null): void { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                eacher.call(thisObj, this[i], i, this);
        }

        function indexOf(value: T, from: Numeric=0): Numeric {
            let start = helper::clamp( helper::toInteger(from), length );
            for ( let i=start, limit=length ; i < limit ; i++ )
                if (this[i] === value)
                    return i;
            return -1;
        }

        function join(separator: string=","): string {
            let limit = length;
            let s = "";
            let i = 0;

            while (true) {
                let x = this[i];
                if (x !== undefined && x !== null)
                    s += string(x);
                if (++i == limit)
                    break;
                s += separator;
            }
            return s;
        }

        function lastIndexOf(value: T, from: Numeric=Infinity): Numeric { 
            let start = helper::clamp( helper::toInteger(from), length );
            for ( let i=start ; i >= 0 ; i-- )
                if (this[i] === value)
                    return i;
            return -1;
        }

        function map(mapper:Mapper, thisObj:Object=null) { 
            var result = new Vector.<T>(length);
            for ( let i=0, limit=length ; i < limit ; i++ )
                result[i] = mapper.call(thisObj, this[i], i, this);
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

        function slice(start: Numeric=0, end: Numeric=Infinity): Vector.<T> {
            let first = helper::clamp(start, length);
            let limit = helper::clamp(end, length);
            let result = new Vector.<T>(limit-first);
            for ( let i=first, n=0 ; i < limit ; i++, n++ )
                result[n] = this[i];
            return result;
        }

        function some(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker.call(thisObj, this[i], i, this))
                    return true;
            return false;
        }

        // FIXME: Is the signature of comparefn too constraining?

        function sort(comparefn: function(T, T): Numeric): Vector.<T> {
            if (length > 0)
                informative::sortEngine(this, 0, length-1, this.helper::sortCompare, comparefn);
            return this;
        }

        helper function sortCompare(j: uint, k: uint, comparefn: Comparator): Numeric {
            let x = this[j];
            let y = this[k];
            return comparefn(x, y);
        }

        function splice(start: Numeric, deleteCount: Numeric, ...items): Vector.<T> {
            let first  = helper::clamp(start, length);
            let delcnt = helper::clamp( helper::toInteger(deleteCount), length-first );

            let result = new Vector.<T>;
            for ( let n=0, i=first ; n < delcnt ; n++, i++ )
                result[n] = this[i];

            if (items.length < delcnt) {
                let shift = delcnt - items.length;
                for ( let n=0, i=first; n < shift ; n++, i++ )
                    this[i] = this[i+shift];
                length -= shift;
            }
            else {
                let shift = items.length - delcnt;
                for ( let n=shift-1, i=first+shift; n >= 0 ; n--, i-- )
                    this[i] = this[i-shift];
            }
            for ( let n=0, i=first ; n < items.lenth ; n++, i++ )
                this[i] = items[n];

            return result;
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

        private const storage: T = new [T];
    }
}

