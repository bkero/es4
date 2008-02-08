/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Array" object
 * ES-262-3 15.X
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
    use namespace intrinsic;
    use namespace __ES4__;

    import ECMAScript4_Internal.*;
    import JSON.*;

    // Array "extras" from JS1.6 (@todo: and JS1.8 -- reduce/reduceRight)
    // See http://developer.mozilla.org/en/docs/New_in_JavaScript_1.6#Array_extras
    // The callback function typically takes (item, i, list) parameters
    type Mapper  = function (*, uint, Object):*;
    type Eacher  = function (*, uint, Object):void;    // FIXME: 'void' seems too strict
    type Checker = function (*, uint, Object):boolean;
    type Reducer = function (*, *, uint, Object):*;

    // INFORMATIVE: this is an implementation that meets the spec, but the spec
    // allows for different sort implementations (quicksort is not required)
    type Comparator = (function (*,*):AnyNumber|undefined);

    dynamic class Array
    {
        static const length = 1;

        // 15.4.1 The Array Constructor Called as a Function
        meta static function invoke(...items) {
            if (items.length == 1)
                return new Array(items[0]);
            else
                return items;
        }

        // 15.4.2 The Array Constructor
        // 15.4.2.1 new Array( [ item0 [ , item1 [ , ... ] ] ] )
        // 15.4.2.2 new Array(len)
        //
        // Implemented in a special case of the evaluator. Necessary
        // because the implementation here would rely on rest args,
        // and we use Array to *implement* those.

        /* For the benefit of the spec, please do not remove this:

        function Array(...items) {
            if (items.length === 1) {
                let item = items[0];
                if (item is AnyNumber) {
                    if (uint(item) === item)
                        this.length = uint(item);
                    else
                        throw new RangeError("Invalid array length");
                }
                else {
                    this.length = 1;
                    this[0] = item;
                }
            }
            else {
                this.length = items.length;
                for ( let i=0, limit=items.length ; i < limit ; i++ )
                    this[i] = items[i];
            }
        }

        */

        // 15.4.4 Properties of the Array Prototype Object
        // prototype.[[Prototype]] = prototype;
        // prototype.[[Class]] = "Array";
        // prototype.length = 0;

        // 15.4.4.2 Array.prototype.toString ( )
        prototype function toString(this:Array)
            this.join();

        override intrinsic function toString():string
            join();

        // 15.4.4.3 Array.prototype.toLocaleString ( )
        prototype function toLocaleString(this:Array)
            this.toLocaleString();

        override intrinsic function toLocaleString():string {
            let out = "";
            for (let i = 0, limit = this.length; i < limit ; i++) {
                if (i > 0)
                    out += ",";
                let x = this[i];
                if (x !== null && x !== undefined)
                    out += x.toLocaleString();
            }
            return out;
        }

        prototype function toJSONString(this:Array, pretty=false)
            this.intrinsic::toJSONString(pretty);

        override intrinsic function toJSONString(pretty: boolean=false): string
            JSON.formatArray(this, pretty);


        // 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , ... ] ] ] )

        // FIXME #155: type system bug
        helper static function concat(object/*: Object!*/, items: Array): Array {
            let out = new Array;

            let function emit(x) {
                if (x is Array) {
                    for (let i=0, limit=x.length ; i < limit ; i++)
                        out[out.length] = x[i];
                }
                else
                    out[out.length] = x;
            }

            emit( object );
            for (let i=0, limit=items.length ; i < limit ; i++)
                emit( items[i] );

            return out;
        }

        // FIXME #155: type system bug
        static function concat(object/*: Object!*/, ...items): Array
            helper::concat(object, items);

        prototype function concat(...items)
            Array.helper::concat(this, items);

        intrinsic function concat(...items): Array
            Array.helper::concat(this, items);

        // 15.4.4.5 Array.prototype.join (separator)
        // FIXME #155: type system bug
        static function join(object/*: Object!*/, separator: string=","): string {
            let out = "";

            for (let i=0, limit=uint(object.length) ; i < limit ; i++) {
                if (i > 0)
                    out += separator;
                let x = object[i];
                if (x !== undefined && x !== null)
                    out += string(x);
            }

            return out;
        }

        prototype function join(separator=undefined)
            Array.join(this, separator === undefined ? "," : string(separator));

        intrinsic function join(separator: string=","): string
            Array.join(this, separator);

        // 15.4.4.6 Array.prototype.pop ( )
        // FIXME #155: type system bug
        static function pop(object/*:Object!*/) {
            let len = uint(object.length);

            if (len != 0) {
                len = len - 1;
                let x = object[len];
                delete object[len]
                object.length = len;
                return x;
            }
            else {
                object.length = len;
                return undefined;
            }
        }

        prototype function pop()
            Array.pop(this);

        intrinsic function pop()
            Array.pop(this);

        // 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , … ] ] ] )
        // FIXME #155: type system bug
        helper static function push(object/*:Object!*/, args: Array): uint {
            let len = uint(object.length);

            for (let i=0, limit=args.length ; i < limit ; i++)
                object[len++] = args[i];

            object.length = len;
            return len;
        }

        static function push(object/*: Object!*/, ...args): uint
            Array.helper::push(object, args);

        prototype function push(...args)
            Array.helper::push(this, args);

        intrinsic function push(...args): uint
            Array.helper::push(this, args);

        // 15.4.4.8 Array.prototype.reverse ( )
        // FIXME #155: type system bug
        static function reverse(object/*: Object!*/)/*: Object!*/ {
            let len = uint(object.length);
            let middle = Math.floor(len / 2);

            for ( let k=0 ; k < middle ; ++k ) {
                let j = len - k - 1;
                if (j in object) {
                    if (k in object)
                        [object[k], object[j]] = [object[j], object[k]];
                    else {
                        object[k] = object[j];
                        delete object[j];
                    }
                }
                else if (k in object) {
                    object[j] = object[k];
                    delete object[k];
                }
                else {
                    // FIXME #157: this seems redundant, they are both absent already.
                    // Is this ever observable (in ES3)?
                    delete object[j];
                    delete object[k];
                }
            }

            return object;
        }

        prototype function reverse()
            Array.reverse(this);

        // FIXME #155: type system bug
        intrinsic function reverse()/*: Object!*/
            Array.reverse(this);

        // 15.4.4.9 Array.prototype.shift ( )
        // FIXME #155: type system bug
        static function shift(object/*: Object!*/) {
            let len = uint(object.length);
            if (len == 0) {
                object.length = 0;        // ECMA-262 requires explicit set here
                return undefined;
            }

            // Get the 0th element to return
            let x = object[0];

            // Move all of the elements down
            for (let i = 1; i < len; i++)
                object[i-1] = object[i];
            delete object[len - 1];
            object.length = len - 1;
            return x;
        }

        prototype function shift()
            Array.shift(this);

        intrinsic function shift()
            Array.shift(this);

        // 15.4.4.10 Array.prototype.slice (start, end, step)
        // FIXME #155: type system bug
        static function slice(object/*: Object!*/, start: AnyNumber, end: AnyNumber, step: AnyNumber) {
            let len = uint(object.length);

            step = int(step);
            if (step == 0)
                step = 1;

            if (intrinsic::isNaN(start))
                start = step > 0 ? 0 : (len-1);
            else
                start = helper::clamp(start, len);
            
            if (intrinsic::isNaN(end))
                end = step > 0 ? len : (-1);
            else
                end = helper::clamp(end, len);
            
            let out = new Array;
            for (let i = start; step > 0 ? i < end : i > end; i += step)
                out.push(object[i]);

            return out;
        }

        prototype function slice(start, end, step)
            Array.slice(this, Number(start), Number(end), Number(step))

        intrinsic function slice(start: AnyNumber, end: AnyNumber, step: AnyNumber): Array
            Array.slice(this, start, end, step);

        // FIXME #155: type system bug
        static function sort(object/*: Object!*/, comparefn) {

            function compare(j, k) {
                if (!(j in object) && !(k in object))
                    return 0;
                if (!(j in object))
                    return 1;
                if (!(k in object))
                    return -1;
                    
                let x = object[j];
                let y = object[k];
                    
                if (x === undefined && y === undefined)
                    return 0;
                if (x === undefined)
                    return 1;
                if (y === undefined)
                    return -1;
                    
                if (comparefn !== undefined)
                    return comparefn(x, y);

                x = x.toString();
                y = y.toString();
                if (x < y) return -1;
                if (x > y) return 1;
                return 0;
            }

            let len = uint(object.length);
            if (len > 0)
                informative::sortEngine(object, 0, len-1, compare);
            return object;
        }

        prototype function sort(comparefn)
            Array.sort(this, comparefn);

        // 15.4.4.11 Array.prototype.sort (comparefn)
        intrinsic function sort(comparefn:Comparator):Array
            Array.sort(this, comparefn);

        // 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , ... ] ] ] )
        // FIXME #155: type system bug
        helper static function splice(object/*: Object!*/, start: AnyNumber, deleteCount: AnyNumber, items: Array) {
            let out = new Array();
            let len = uint(object.length);

            start = helper::clamp( start, len );
            deleteCount = helper::clamp( deleteCount, len - start );

            let end = start + deleteCount;

            // Copy out the elements we are going to remove
            for (let i = 0; i < deleteCount; i++)
                out.push(object[i + start]);

            let insertCount = items.length;
            let shiftAmount = insertCount - deleteCount;

            // delete items by shifting elements past end (of delete) by l_shiftAmount
            if (shiftAmount < 0) {
                // Shift the remaining elements down
                shiftAmount = -shiftAmount;

                for (let i = end; i < len; i++)
                    object[i - shiftAmount] = object[i];

                // delete top elements here to match ECMAscript spec (generic object support)
                for (let i = len - shiftAmount; i < len; i++)
                    delete object[i];
            }
            else {
                // Shift the remaining elements up.
                for (let i = len; i > end; ) {
                    --i;
                    object[i + shiftAmount] = object[i];
                }
            }

            // Add the items to insert
            for (let i = 0; i < insertCount; i++)
                object[start+i] = items[i];

            // shrink array if shiftAmount is negative
            object.length = len + shiftAmount;
            return out;
        }

        // FIXME #155: type system bug
        static function splice(object/*: Object!*/, start: AnyNumber, deleteCount: AnyNumber, ...items): Array
            Array.helper::splice(object, start, deleteCount, items);

        prototype function splice(start, deleteCount, ...items)
            Array.helper::splice(this, Number(start), Number(deleteCount), items);

        intrinsic function splice(start: AnyNumber, deleteCount: AnyNumber, ...items): Array
            Array.helper::splice(this, start, deleteCount, items);

        // FIXME #155: type system bug
        helper static function unshift(object/*: Object!*/, items: Array) : uint {
            let len = uint(object.length);
            let numitems = items.length;

            for ( let k=len-1 ; k >= 0 ; --k ) {
                let d = k + numitems;
                if (k in object)
                    object[d] = object[k];
                else
                    delete object[d];
            }

            for (let i=0; i < numitems; i++)
                object[i] = items[i];

            object.length = len+numitems;   // Required by E262-3; observable by means of a setter method on A

            return len+numitems;
        }

        // 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , … ] ] ] )
        // FIXME #155: type system bug
        static function unshift(object/*: Object!*/, ...items) : uint
            Array.helper::unshift(this, object, items);

        prototype function unshift(...items)
            Array.helper::unshift(this, items);

        intrinsic function unshift(...items): uint
            Array.helper::unshift(this, items);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:map
        // ES4 draft -- static generics

        // FIXME #153: we want a generic "function" shortand to
        // signify "any function" here, but it should signify
        // "callable", not "subtype of Function"

        // FIXME #155: type system bug
        static function map(object/*:Object!*/, mapper/*:function*/, thisObj:Object=null): Array {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof mapper != "function")
                throw new TypeError("Function object required to 'map'");

            let result = [];
            for (let i = 0, limit = object.length; i < limit ; i++)
                if (i in object)
                    result[i] = mapper.call(thisObj, object[i], i, object);
            return result;
        }

        prototype function map(mapper, thisObj=null)
            Array.map(this, mapper, thisObj);

        intrinsic function map(mapper:Mapper, thisObj:Object=null): Array
            Array.map(this, mapper, thisObj);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:forEach
        // ES4 draft

        // FIXME #155: type system bug
        static function forEach(object/*:Object!*/, eacher/*function*/, thisObj:Object=null): void {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof eacher != "function")
                throw new TypeError("Function object required to 'forEach'");

            for (let i=0, limit = object.length ; i < limit ; i++)
                if (i in object)
                    eacher.call(thisObj, object[i], i, object);
        }

        prototype function forEach(eacher, thisObj=null) {
            Array.forEach(this, eacher, thisObj);
        }

        intrinsic function forEach(eacher:Eacher, thisObj:Object=null): void {
            Array.forEach(this, eacher, thisObj);
        }


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:filter
        // ES4 draft

        // FIXME #155: type system bug
        static function filter(object/*:Object!*/, checker/*function*/, thisObj:Object=null): Array {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof checker != "function")
                throw new TypeError("Function object required to 'filter'");

            let result = [];
            for (let i = 0, limit=object.length ; i < limit ; i++) {
                if (i in object) {
                    let item = object[i];
                    if (checker.call(thisObj, item, i, object))
                        result[result.length] = item;
                }
            }
            return result;
        }

        prototype function filter(checker, thisObj=null)
            Array.filter(this, checker, thisObj);

        intrinsic function filter(checker:Checker, thisObj:Object=null): Array
            Array.filter(this, checker, thisObj);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:every
        // ES4 draft

        // FIXME #155: type system bug
        static function every(object/*:Object!*/, checker/*:function*/, thisObj:Object=null): boolean {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof checker != "function")
                throw new TypeError("Function object required to 'every'");

            for (let i=0, limit=object.length ; i < limit ; i++) {
                if (i in object)
                    if (!checker.call(thisObj, object[i], i, object))
                        return false;
            }
            return true;
        }

        prototype function every(checker, thisObj=null)
            Array.every(this, checker, thisObj);

        intrinsic function every(checker:Checker, thisObj:Object=null): boolean 
            Array.every(this, checker, thisObj);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:some
        // ES4 draft

        // FIXME #155: type system bug
        static function some(object/*:Object!*/, checker/*:function*/, thisObj:Object=null): boolean {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof checker != "function")
                throw new TypeError("Function object required to 'some'");

            for (let i=0, limit=object.length; i < limit ; i++) {
                if (i in object)
                    if (checker.call(thisObj, object[i], i, object))
                        return true;
            }
            return false;
        }

        prototype function some(checker, thisObj=null)
            Array.some(this, checker, thisObj);

        intrinsic function some(checker:Checker, thisObj:Object=null): boolean
            Array.some(this, checker, thisObj);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:indexOf
        // ES4 draft: static intrinsics

        static function indexOf(object/*:Object!*/, value, from:AnyNumber=0): AnyNumber {
            let len = object.length;

            from = from < 0 ? Math.ceil(from) : Math.floor(from);
            if (from < 0)
                from = from + len;
            
            while (from < len) {
                if (from in object)
                    if (value === object[from])
                        return from;
                from = from + 1;
            }
            return -1;
        }

        prototype function indexOf(value, from=0)
            Array.indexOf(this, value, Number(from));

        intrinsic function indexOf(value, from:AnyNumber=0): AnyNumber
            Array.indexOf(this, value, from);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:lastIndexOf
        // ES4 draft: static intrinsics

        static function lastIndexOf(object/*:Object!*/, value, from:AnyNumber=NaN): AnyNumber {
            let len = object.length;

            if (isNaN(from))
                from = len - 1;
            else {
                from = from < 0 ? Math.ceil(from) : Math.floor(from);
                if (from < 0)
                    from = from + len;
                else if (from >= len)
                    from = len - 1;
            }

            while (from > -1) {
                if (from in object)
                    if (value === object[from])
                        return from;
                from = from - 1;
            }
            return -1;
        }

        prototype function lastIndexOf(value, from=NaN)
            Array.lastIndexOf(this, value, Number(from));

        intrinsic function lastIndexOf(value, from:AnyNumber=NaN): AnyNumber
            Array.lastIndexOf(this, value, from);


        // 15.4.5.1 [[Put]] (P, V)
        // @todo: ensure that catchall-set for undeclared properties runs on every set
        meta function set(id, value):void {
            let oldLength:uint = this.length;
            intrinsic::set(this, id, value);
            let idAsDouble:double = double(id);
            let idAsUint:uint = uint(idAsDouble);
            if (idAsUint == idAsDouble && idAsUint >= oldLength)
                this.length = idAsUint+1;
        }

        // 15.4.5.2 length
        private var _length:uint = 0;
        function get length():uint
            this.private::_length;

        // ECMA-262 requires a RangeError if non-ints are passed in,
        // so we must not type it as uint in the setter's signature
        function set length(newLength):void {
            let oldLength:uint = this.private::_length;
            let newLengthAsDouble:double = double(newLength);
            let newLengthAsUint:uint = uint(newLengthAsDouble);
            if (newLengthAsUint != newLengthAsDouble)
                throw new RangeError();
            for (let i:uint = newLengthAsUint; i < oldLength; ++i)
                if (this.hasOwnProperty(i.toString()))  // FIXME: when type annos work, won't need explicit conversion
                    delete this[i];
            this.private::_length = newLengthAsUint;
        }
    }

    // Convert val to integer (this rounds it toward zero and discards NaN).
    // If the number is negative, add "len".
    // Then clamp it between 0 and len inclusive, and cast it to uint.
    //
    // Also used by Vector.es
    helper function clamp(val: AnyNumber, len: uint): uint {
        val = helper::toInteger(val);
        if (val < 0)
            val += len;
        return uint( Math.min( Math.max( val, 0 ), len ) );
    }

    // INFORMATIVE note: as noted above, this is a very simple recursive
    // implementation of Quicksort.  While it suffices for spec purposes,
    // it is not efficient enough for a real implementation, which
    // typically faces mostly-ordered inputs.  It is also not a stable
    // sort, which may be desirable but is not required by the spec

    informative function sortEngine(v, lo: uint, hi: uint, sortCompare): void {

        function qsort(lo, hi) {
            if (lo >= hi)
                return;

            let size  = (hi - lo) + 1;
            let pivot = lo + Math.floor(size / 2);
            let i     = lo;
            let j     = hi;
            while (i <= j) {
                while (sortCompare(i, pivot) < 0)
                    ++i;
                while (sortCompare(j, pivot) > 0)
                    --j;
                if (i <= j) {
                    [v[i], v[j]] = [v[j], v[i]];
                    ++i;
                    --j;
                }
            }
        
            if (lo < j)
                qsort(lo, j);
            if (i < hi)
                qsort(i, hi);
        }

        qsort( lo, hi );
    }
}
