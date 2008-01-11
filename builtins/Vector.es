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
        static const length = 2;

        function Vector(length: uint=0, fixed: boolean=false) 
            : fixed = fixed
        {
            informative::setLength(length);
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

        function set length(len: AnyNumber) {
            if (fixed)
                throw new RangeError();
            if (!helper::isIntegral(len) ||
                len < 0 ||
                len > 0xFFFFFFFF)
                throw new RangeError();
            informative::setLength(uint(len));
        }

        meta function get(name): T {
            let idx : double = double(name);
            if (!intrinsic::isNaN(idx)) {
                if (!helper::isIntegral(idx) || idx < 0 || idx >= length)
                    throw new RangeError();
                return informative::getValue(uint(idx));
            }
            else 
                intrinsic::get(this,name);
        }

        meta function set(name, v) {
            let idx : double = double(name);
            if (!intrinsic::isNaN(idx)) {
                let value: T = v;  // Note, effectful
                if (!helper::isIntegral(idx) || 
                    idx < 0 || 
                    fixed && idx >= length || 
                    !fixed && idx > length)
                    throw new RangeError();
                informative::setValue(uint(idx), value);
            }
            else
                intrinsic::set(this, name, v);
        }

        override intrinsic function toString()
            join();

        override intrinsic function toLocaleString() {
            let limit = length;
            let separator = informative::localeSpecificSeparatorString;
            let s = "";
            let i = 0;

            while (true) {
                let x = this[i];
                if (x !== undefined && x !== null)
                    s += x.public::toLocaleString();
                if (++i == limit)
                    break;
                s += separator;
            }
            return s;
        }

        informative var localeSpecificSeparatorString = ",";

        intrinsic function concat(...items): Vector.<T>
            helper::concat(items);

        helper function concat(items) {
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

        intrinsic function every(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (!checker.call(thisObj, this[i], i, this))
                    return false;
            return true;
        }

        intrinsic function filter(checker: Checker, thisObj: Object=null): Vector.<T> { 
            var result = new Vector.<T>;
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker.call(thisObj, this[i], i, this))
                    result.push(this[i]);
            return result;
        }

        intrinsic function forEach(eacher: Eacher, thisObj: Object=null): void { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                eacher.call(thisObj, this[i], i, this);
        }

        intrinsic function indexOf(value: T, from: AnyNumber=0): AnyNumber {
            let start = helper::clamp( from, length );
            for ( let i=start, limit=length ; i < limit ; i++ )
                if (this[i] === value)
                    return i;
            return -1;
        }

        intrinsic function join(separator: string=","): string {
            let limit = length;
            let s = "";
            let i = 0;

            for (let i = 0; i < limit; i++) {
                let x = this[i];
                if (i != 0)
                    s += separator;
                if (x !== undefined && x !== null)
                    s += string(x);
            }
            return s;
        }

        intrinsic function lastIndexOf(value: T, from: AnyNumber=Infinity): AnyNumber { 
            let start = helper::clamp( from, length );
            for ( let i=start ; i >= 0 ; i-- )
                if (this[i] === value)
                    return i;
            return -1;
        }

        intrinsic function map(mapper:Mapper, thisObj:Object=null) { 
            var result = new Vector.<T>(length);
            for ( let i=0, limit=length ; i < limit ; i++ )
                result[i] = mapper.call(thisObj, this[i], i, this);
            return result;
        }

        intrinsic function pop(): T {
            if (length == 0)
                return undefined;

            let v = this[length];
            length--;
            return v;
        }

        intrinsic function push(...items): uint
            helper::push(items);

        helper function push(items) {
            for ( let i=0, limit=items.length ; i < limit ; i++ )
                this[length] = items[i];
            return length;
        }

        intrinsic function reverse(): Vector.<T> {
            for ( let i=0, j=length-1 ; i < j ; i++, j-- )
                [this[i], this[j]] = [this[j], this[i]];
            return this;
        }

        intrinsic function shift(): T {
            if (length == 0)
                return undefined;
            let v = this[0];
            for ( let i=1, limit=length ; i < limit ; i++ )
                this[i-1] = this[i];
            length--;
            return v;
        }

        intrinsic function slice(start: AnyNumber, end: AnyNumber, step: AnyNumber): Vector.<T> {

            step = int(step);
            if (step == 0)
                step = 1;

            if (intrinsic::isNaN(start))
                start = step > 0 ? 0 : (length-1);
            else
                start = helper::clamp(start, length);
            
            if (intrinsic::isNaN(end))
                end = step > 0 ? len : (-1);
            else
                end = helper::clamp(end, length);
            
            let out:Vector.<T> = new Vector.<T>;
            for (let i = start; step > 0 ? i < end : i > end; i += step)
                out.push(this[i]);

            return out;
        }

        intrinsic function some(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                if (checker.call(thisObj, this[i], i, this))
                    return true;
            return false;
        }

        // FIXME: Is the signature of comparefn too constraining?

        intrinsic function sort(comparefn: function(T, T): AnyNumber): Vector.<T> {
            if (length > 0) {
                let object = this;
                informative::sortEngine(this, 
                                        0, 
                                        length-1, 
                                        (function (j, k)
                                             comparefn(object[j], object[k])));
                return this;
            }
        }

        intrinsic function splice(start: AnyNumber, deleteCount: AnyNumber, ...items): Vector.<T>
            helper::splice(start, deleteCount, items);

        helper function splice(start, deleteCount, items) {
            let first  = helper::clamp( start, length );
            let delcnt = helper::clamp( deleteCount, length-first );

            let result = new Vector.<T>;
            for ( let n=0, i=first ; n < delcnt ; n++, i++ )
                result.push(this[i]);

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
            for ( let n=0, i=first ; n < items.length ; n++, i++ )
                this[i] = items[n];

            return result;
        }

        intrinsic function unshift(...items): uint
            helper::unshift(items);

        helper function unshift(items) {
            let numitems = items.length;
            let oldlimit = length;
            let newlimit = oldlimit + numitems;

            for ( let i=0 ; i < numitems ; i++ )
                this[newlimit-i] = this[oldlimit-i];
            for ( let i=0 ; i < numitems ; i++ )
                this[i] = items[i];
            return newlength;
        }

        prototype function toString(this:Vector.<*>)
            this.intrinsic::toString();

        prototype function toLocaleString(this:Vector.<*>) 
            this.intrinsic::toLocaleString();

        prototype function concat(this:Vector.<*>, ...items)
            this.helper::concat(items);

        prototype function every(this:Vector.<*>, checker, thisObj=undefined)
            (this.intrinsic::every(checker, thisObj is Object) ? thisObj : null);

        prototype function filter(this:Vector.<*>, checker, thisObj=undefined)
            (this.intrinsic::filter(checker, thisObj is Object) ? thisObj : null);

        prototype function forEach(this:Vector.<*>, eacher, thisObj=undefined)
            (this.intrinsic::forEach(checker, thisObj is Object) ? thisObj : null);

        prototype function indexOf(this:Vector.<*>, value, from=undefined)
            this.intrinsic::indexOf(value, Number(from));

        prototype function join(this:Vector.<*>, separator=undefined)
            this.intrinsic::join(separator == undefined ? "," : string(separator));

        prototype function lastIndexOf(this:Vector.<*>, value, from=undefined)
            this.intrinsic::indexOf(value, from == undefined ? Infinity : Number(from));

        prototype function map(this:Vector.<*>, mapper, thisObj=undefined)
            (this.intrinsic::map(mapper, thisObj is Object) ? thisObj : null);

        prototype function pop(this:Vector.<*>)
            this.intrinsic::pop();

        prototype function push(this:Vector.<*>, ...items)
            this.helper::push(items);

        prototype function reverse(this:Vector.<*>)
            this.intrinsic::reverse();

        prototype function shift(this:Vector.<*>)
            this.intrinsic::shift();

        prototype function slice(this:Vector.<*>, start, end, step)
            this.intrinsic::slice(Number(start), Number(end), Number(step));

        prototype function some(this:Vector.<*>, checker, thisObj=undefined)
            (this.intrinsic::some(checker, thisObj is Object) ? thisObj : null);

        prototype function sort(this:Vector.<*>, comparefn)
            this.intrinsic::sort(comparefn);

        prototype function splice(this:Vector.<*>, start, deleteCount, ...items)
            this.helper::splice(Number(start), Number(deleteCount), items);

        prototype function unshift(this:Vector.<*>, ...items)
            this.helper::unshift(items);

        iterator function get(deep: boolean = false) : iterator::IteratorType.<uint>
            getKeys(deep);

        iterator function getKeys(deep: boolean = false) : iterator::IteratorType.<uint> {
            let i = 0;
            let a = this;
            return { 
                next:
                function () : uint {
                    if (i === a.length)
                        throw iterator::StopIteration;
                    return i++;
                }
            }
        }

        iterator function getValues(deep: boolean = false) : iterator::IteratorType.<T> {
            let i = 0;
            let a = this;
            return { 
                next:
                function () : T {
                    if (i === a.length)
                        throw iterator::StopIteration;
                    return a[i++];
                }
            }
        }

        iterator function getItems(deep: boolean = false) : iterator::IteratorType.<[uint,V]> {
            let i = 0;
            let a = this;
            return { 
                next:
                function () : T {
                    if (i === a.length)
                        throw iterator::StopIteration;
                    return [i,a[i++]];  // Yes, that's well-defined
                }
            }
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

        private const storage = new Array();
    }
}

