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

    __ES4__ class Vector.<T>
    {
        public function Vector(length: double=0, fixed: boolean=false) 
            : fixed = fixed
        {
            informative::setLength(length);
        }

        static public const length = 2;

        /* Not what we want 
        static meta function invoke(object) {
            if (object is Vector.<*>)
                return object;
            let length = intrinsic::toUint(object.length);
            let result = new Vector.<*>(length);
            for ( let i=0 ; i < length ; i++ )
                result[i] = object[i];
            return result;
        }
        */

        public var fixed: boolean;

        public final function get length()
            informative::getLength();

        public final function set length(len: AnyNumber) {
            if (fixed)
                throw new RangeError();
            if (!isIntegral(len) || len < 0 || len > 0xFFFFFFFF) 
                throw new RangeError();
            informative::setLength(intrinsic::toUint(len));
        }

        meta final function get(name): T {
            let idx = double(name);
            if (!intrinsic::isNaN(idx)) {
                if (!isIntegral(idx) || idx < 0 || idx >= length)
                    throw new RangeError();
                return informative::getValue(intrinsic::toUint(idx));
            }
            else 
                return intrinsic::get(this,name);
        }

        meta final function set(name, v): void {
            let idx = double(name);
            if (!intrinsic::isNaN(idx)) {
                let value: T = v;  // Note, effectful
                if (!isIntegral(idx) || 
                    idx < 0 || 
                    fixed && idx >= length || 
                    !fixed && idx > length)
                    throw new RangeError();
                informative::setValue(intrinsic::toUint(idx), value);
            }
            else
                intrinsic::set(this, name, v);
        }

        meta final function has(name) {
            // To be written
        }

        /* Not possible in current refimpl
        meta final function delete(name) {
            // To be written
        }
        */

        override intrinsic function toString()
            intrinsic::join();

        override intrinsic function toLocaleString() {
            let limit = length;
            let separator = informative::localeSpecificSeparatorString();
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

        informative function localeSpecificSeparatorString()
            ",";

        intrinsic function concat(...items): Vector.<T>
            helper::concat(items);

        helper function concat(items) {
            let v = new Vector.<T>;
            let k = 0;

            for ( let i=0, limit=length ; i < limit ; i++ )
                v[k++] = this[i];

            for ( let j=0 ; j < items.length ; j++ ) {
                let item = items[j];
                for ( let i=0, limit=item.length ; i < limit ; i++ )
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
            for ( let i=0, limit=length ; i < limit ; i++ ) {
                let item = this[i];
                if (checker.call(thisObj, item, i, this))
                    result[result.length] = item;
            }
            return result;
        }

        intrinsic function forEach(eacher: Eacher, thisObj: Object=null): void { 
            for ( let i=0, limit=length ; i < limit ; i++ )
                eacher.call(thisObj, this[i], i, this);
        }

        intrinsic function indexOf(value: T, from: AnyNumber=0): AnyNumber {
            let start = helper::clamp( from, length );
            for ( let i=start, limit=length ; i < limit ; i++ ) {
                let item = this[i];
                if (item === value)
                    return i;
            }
            return -1;
        }

        intrinsic function join(separator: string=","): string {
            let limit = length;
            let s = "";
            let i = 0;

            for (let i = 0; i < limit; i++) {
                let item = this[i];
                if (i != 0)
                    s += separator;
                if (item is Object)
                    s += string(x);
            }
            return s;
        }

        intrinsic function lastIndexOf(value: T, from: AnyNumber=Infinity): AnyNumber { 
            let start = helper::clamp( from, length );
            for ( let i=start ; i >= 0 ; i-- ) {
                let item = this[i];
                if (item === value)
                    return i;
            }
            return -1;
        }

        intrinsic function map(mapper:Mapper, thisObj:Object=null) { 
            var result = new Vector.<T>(length);
            for ( let i=0, limit=length ; i < limit ; i++ ) {
                let item = this[i];
                result[i] = mapper.call(thisObj, item, i, this);
            }
            return result;
        }

        intrinsic function pop(): T {
            if (length == 0)
                return undefined;

            let v = this[length-1];
            length--;
            return v;
        }

        intrinsic function push(...items): double
            helper::push(items);

        helper function push(items) {
            for ( let i=0, limit=items.length ; i < limit ; i++ )
                this[length] = items[i];
            return length;
        }

        intrinsic function reduce(reducer/*: function*/, initialValue:(T|None)=NONE ): T {
            let result;
            let i = 0;

            if (initialValue !== NONE)
                result = initialValue;
            else if (length == 0)
                throw new TypeError();
            else {
                result = this[0];
                i++;
            }

            for (let limit=length ; i < limit ; i++) {
                let item = this[i];
                result = reducer.call(null, result, item, i, this);
            }

            return result;
        }

        intrinsic function reduceRight(reducer/*: function*/, initialValue:(T|None)=NONE ): T {
            let result;
            let i = length-1;

            if (initialValue !== NONE)
                result = initialValue;
            else if (length == 0)
                throw new TypeError();
            else {
                result = this[length-1];
                i--;
            }

            while (i >= 0) {
                let item = this[i];
                result = reducer.call(null, result, item, i, this);
                i--;
            }

            return result;
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

        intrinsic function slice(start: AnyNumber=0, end: AnyNumber=Infinity, step: AnyNumber=1): Vector.<T> {
            step = helper::toInteger(step);
            if (step == 0)
                step = 1;

            start = helper::clamp(start, length);
            end = helper::clamp(end, length);
            
            let result = new Vector.<T>;
            if (step > 0)
                for (let i=start; i < end ; i += step)
                    result[result.length] = this[i];
            else
                for (let i=start; i > end ; i += step)
                    result[result.length] = this[i];

            return result;
        }

        intrinsic function some(checker: Checker, thisObj: Object=null): boolean { 
            for ( let i=0, limit=length ; i < limit ; i++ ) {
                let item = this[i];
                if (checker.call(thisObj, item, i, this))
                    return true;
            }
            return false;
        }

        // FIXME: Is the signature of comparefn too constraining?

        intrinsic function sort(comparefn: function(T, T): AnyNumber): Vector.<T> {
            let object = this;
            return informative::sortEngine(object,
                                           0, 
                                           length-1, 
                                           (function (j, k) comparefn(object[j], object[k])));
        }

        intrinsic function splice(start: AnyNumber, deleteCount: AnyNumber, ...items): Vector.<T>
            helper::splice(start, deleteCount, items);

        helper function splice(start, deleteCount, items) {
            let out = new Vector.<T>;
            let len = intrinsic::toUint(length);

            start = helper::clamp( start, len );
            deleteCount = helper::clamp( deleteCount, len - start );

            let end = start + deleteCount;

            // Copy out the elements we are going to remove
            for (let i = 0; i < deleteCount; i++)
                out.push(this[i + start]);

            let insertCount = items.length;
            let shiftAmount = insertCount - deleteCount;

            // delete items by shifting elements past end (of delete) by l_shiftAmount
            if (shiftAmount < 0) {
                // Shift the remaining elements down
                shiftAmount = -shiftAmount;

                for (let i = end; i < len; i++)
                    this[i - shiftAmount] = this[i];
            }
            else {
                // Shift the remaining elements up.
                for (let i = len; i > end; ) {
                    --i;
                    this[i + shiftAmount] = this[i];
                }
            }

            // Add the items to insert
            for (let i = 0; i < insertCount; i++)
                this[start+i] = items[i];

            // shrink array if shiftAmount is negative
            length = len + shiftAmount;
            return out;
        }

        intrinsic function unshift(...items): double
            helper::unshift(items);

        helper function unshift(items) {
            let numitems = items.length;
            let oldlimit = length;
            let newlimit = oldlimit + numitems;

            length = newlimit;
            for ( let i=0 ; i < length ; i++ )
                this[newlimit-i] = this[oldlimit-i];
            for ( let i=0 ; i < numitems ; i++ )
                this[i] = items[i];
            return newlimit;
        }

        prototype function toString(this:Vector.<*>)
            this.intrinsic::toString();

        prototype function toLocaleString(this:Vector.<*>) 
            this.intrinsic::toLocaleString();

        prototype function concat(this:Vector.<*>, ...items)
            this.helper::concat(items);

        prototype function every(this:Vector.<*>, checker, thisObj=undefined)
            this.intrinsic::every(checker, thisObj is Object ? thisObj : null);

        prototype function filter(this:Vector.<*>, checker, thisObj=undefined)
            this.intrinsic::filter(checker, thisObj is Object ? thisObj : null);

        prototype function forEach(this:Vector.<*>, eacher, thisObj=undefined)
            this.intrinsic::forEach(checker, thisObj is Object ? thisObj : null);

        prototype function indexOf(this:Vector.<*>, value, from=undefined)
            this.intrinsic::indexOf(value, Number(from));

        prototype function join(this:Vector.<*>, separator=undefined)
            this.intrinsic::join(separator == undefined ? "," : string(separator));

        prototype function lastIndexOf(this:Vector.<*>, value, from=undefined)
            this.intrinsic::indexOf(value, from == undefined ? Infinity : Number(from));

        prototype function map(this:Vector.<*>, mapper, thisObj=undefined)
            this.intrinsic::map(mapper, thisObj is Object ? thisObj : null);

        prototype function pop(this:Vector.<*>)
            this.intrinsic::pop();

        prototype function push(this:Vector.<*>, ...items)
            this.helper::push(items);

        prototype function reduce(this:Vector.<*>, reducer, initialValue=NONE)
            this.intrinsic::reduce(reducer, initialValue);

        prototype function reduceRight(this:Vector.<*>, reducer, initialValue=NONE)
            this.intrinsic::reverse(reducer, initialValue);

        prototype function reverse(this:Vector.<*>)
            this.intrinsic::reverse();

        prototype function shift(this:Vector.<*>)
            this.intrinsic::shift();

        prototype function slice(this:Vector.<*>, start, end, step)
            this.intrinsic::slice(Number(start), Number(end), Number(step));

        prototype function some(this:Vector.<*>, checker, thisObj=undefined)
            this.intrinsic::some(checker, thisObj is Object ? thisObj : null);

        prototype function sort(this:Vector.<*>, comparefn)
            this.intrinsic::sort(comparefn);

        prototype function splice(this:Vector.<*>, start, deleteCount, ...items)
            this.helper::splice(Number(start), Number(deleteCount), items);

        prototype function unshift(this:Vector.<*>, ...items)
            this.helper::unshift(items);

        iterator function get(deep: boolean = false) : iterator::Iterator.<double>
            getKeys(deep);

        iterator function getKeys(deep: boolean = false) : iterator::Iterator.<double> {
            let i = 0;
            let a = this;
            return { 
                const next:
                    function () : double {
                        if (i < a.length)
                            return i++;
                        throw iterator::StopIteration;
                    }
            } : iterator::Iterator.<double>;
        }

        iterator function getValues(deep: boolean = false) : iterator::Iterator.<T> {
            let i = 0;
            let a = this;
            return { 
                const next:
                    function () : T {
                        if (i < a.length)
                            return a[i++];
                        throw iterator::StopIteration;
                }
            } : iterator::Iterator.<T>;
        }

        iterator function getItems(deep: boolean = false) : iterator::Iterator.<[double,T]> {
            let i = 0;
            let a = this;
            return { 
                const next:
                function () : T {
                    if (i === a.length)
                        return [i,a[i++]];  // Yes, that's well-defined
                    throw iterator::StopIteration;
                }
            } : iterator::Iterator.<[double,T]>;
        }

        informative function getLength()
            storage.length;

        informative function setLength(newlength: double) {
            let oldlength = storage.length;
            if (newlength > oldlength) {
                storage.length = newlength;
                for ( let i=oldlength ; i < newlength ; i++ )
                    storage[i] = undefined;  // FIXME: informative::defaultValue(T);
            }
            else
                storage.length = newlength;
        }

        informative function defaultValue(t) {
            if (t === double || t === decimal || t === Number)
                return 0;
            if (t === string || t === String)
                return "";
            if (t === boolean || t === Boolean)
                return false;
            if (t === type undefined)
                return undefined;
            return null;
        }

        informative function getValue(idx: double)
            storage[idx];

        informative function setValue(idx: double, val: T)
            storage[idx] = val;

        private const storage = new Array();
    }

