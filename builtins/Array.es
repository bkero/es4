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
    use strict;
    import ECMAScript4_Internal.*;

    // Array "extras" from JS1.6 (@todo: and JS1.8 -- reduce/reduceRight)
    // See http://developer.mozilla.org/en/docs/New_in_JavaScript_1.6#Array_extras
    // The callback function typically takes (item, i, list) parameters
    type Mapper  = function (*, uint, Object):*;
    type Eacher  = function (*, uint, Object):void;    // FIXME: 'void' seems too strict
    type Checker = function (*, uint, Object):Boolean;
    type Reducer = function (*, *, uint, Object):*;

    // INFORMATIVE: this is an implementation that meets the spec, but the spec
    // allows for different sort implementations (quicksort is not required)
    type Comparator = (function (*,*):double, undefined);

    dynamic class Array
    {
        // 15.4.1 The Array Constructor Called as a Function
        meta static function invoke(...args) {
            if (args.length == 1)
                return new Array(args[0]);
            else
                return args; /* Already an array */
        }

        // 15.4.2 The Array Constructor
        // 15.4.2.1 new Array( [ item0 [ , item1 [ , ... ] ] ] )
        // 15.4.2.2 new Array(len)
        //
        // Implemented in a special case of the evaluator. Necessary
        // because the implementation here would rely on rest args,
        // and we use Array to *implement* those.


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
            let out:string = "";
            for (let i:uint = 0, n:uint = this.length; i < n; i++) {
                if (i != 0)
                    out += ",";
                let x = this[i];
                if (x != null)
                    out += x.toLocaleString();
            }
            return out;
        }

        // 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , ... ] ] ] )
        helper static function concat(object, args) {
            let out = new Array;
            let outlen = 0;

            let function emit(x) {
                if (x is Array) {
                    for (let j = 0; j < x.length; j++)
                        out[outlen++] = x[j];
                }
                else
                    out[outlen++] = x;
            }

            emit( object );
            for (let i = 0; i < args.length; i++)
                emit( args[i] );

            return out;
        }

        static function concat(object, ...args)
            helper::concat(object, args);

        prototype function concat(...args)
            Array.helper::concat(this, args);

        intrinsic function concat(...args):Array
            Array.helper::concat(this, args);

        // 15.4.4.5 Array.prototype.join (separator)
        static function join(object, sep = undefined) {
            let s   = (sep === undefined) ? "," : string(sep);
            let out = "";
            let len = object.length;

            for (let i = 0; i < len; i++) {
                if (i != 0)
                    out += s;
                let x = object[i];
                if (x !== undefined && x !== null)
                    out += string(x);
            }

            return out;
        }

        prototype function join(sep = undefined)
            Array.join(this, sep);

        intrinsic function join(sep = undefined):string
            Array.join(this, sep);

        // 15.4.4.6 Array.prototype.pop ( )
        static function pop(object) {
            let len = object.length;

            if (len != 0) {
                let x = object[--len];
                delete object[len];
                object.length = len;
                return x;
            }
            return undefined;
        }

        prototype function pop()
            Array.pop(this);

        intrinsic function pop()
            Array.pop(this);

        // 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , … ] ] ] )
        helper static function push(object, args) {
            let len = object.length;
            let argslen = args.length;

            for (let i = 0; i < argslen; i++)
                object[len++] = args[i];

            object.length = len;
            return len;
        }

        static function push(object, ...args)
            Array.helper::push(object, args);

        prototype function push(...args)
            Array.helper::push(this, args);

        intrinsic function push(...args:Array):uint
            Array.helper::push(this, args);

        // 15.4.4.8 Array.prototype.reverse ( )
        static function reverse(object) {
            let i = 0;
            let j = object.length;
            let h = j >>> 1;

            while (i < h) {
                --j;
                [object[i], object[j]] = [object[j], object[i]];
                i++;
            }
            return object;
        }

        prototype function reverse()
            Array.reverse(this);

        intrinsic function reverse():Array
            Array.reverse(this);

        // 15.4.4.9 Array.prototype.shift ( )
        static function shift(object) {
            let len = object.length;
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

        // 15.4.4.10 Array.prototype.slice (start, end)
        static function slice(object, start, end) {
            if (start === undefined)
                start = 0;
            if (end === undefined)
                end = Infinity;

            let len = uint(object.length);

            // If a param is passed then the first one is start.
            // If no params are passed then start = 0.
            let a = helper::clamp(start, len);
            let b = helper::clamp(end, len);
            if (b < a)
                b = a;

            let out = new Array;
            for (let i = a; i < b; i++)
                out.push(object[i]);

            return out;
        }

        prototype function slice(start, end)
            Array.slice(this, start, end);

        intrinsic function slice(start:double = 0, end:double = Infinity):Array
            Array.slice(this, start, end);

        static function sort(self, comparefn) {
            let len:uint = self.length;

            if (len > 1)
                self.private::qsort(0, len-1, comparefn);  /* FIXME: "private::" should not be necessary */

            return self;
        }

        prototype function sort(comparefn)
            Array.sort(this, comparefn);

        // 15.4.4.11 Array.prototype.sort (comparefn)
        intrinsic function sort(comparefn:Comparator):Array
            Array.sort(this, comparefn);

        // 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , ... ] ] ] )
        static function splice(self, start, deleteCount, ...args) {
            let out:Array = new Array();

            let argslen:uint = uint(args.length);
            if (argslen == 0)
                return undefined;

            let len:uint = self.length;
            let start:uint = helper::clamp(double(args[0]), len);
            let d_deleteCount:double = argslen > 1 ? double(args[1]) : (len - start);
            let deleteCount:uint = (d_deleteCount < 0) ? 0 : uint(d_deleteCount);
            if (deleteCount > len - start)
                deleteCount = len - start;

            let end:uint = start + deleteCount;

            // Copy out the elements we are going to remove
            for (let i:uint = 0; i < deleteCount; i++)
                out.push(self[i + start]);

            let insertCount:uint = (argslen > 2) ? (argslen - 2) : 0;
            let l_shiftAmount:double = insertCount - deleteCount;
            let shiftAmount:uint;

            // delete items by shifting elements past end (of delete) by l_shiftAmount
            if (l_shiftAmount < 0) {
                // Shift the remaining elements down
                shiftAmount = uint(-l_shiftAmount);

                for (let i:uint = end; i < len; i++)
                    self[i - shiftAmount] = self[i];

                // delete top elements here to match ECMAscript spec (generic object support)
                for (let i:uint = len - shiftAmount; i < len; i++)
                    delete self[i];
            }
            else {
                // Shift the remaining elements up.
                shiftAmount = uint(l_shiftAmount);

                for (let i:uint = len; i > end; ) {
                    --i;
                    self[i + shiftAmount] = self[i];
                }
            }

            // Add the items to insert
            for (let i:uint = 0; i < insertCount; i++)
                self[start+i] = args[i + 2];

            // shrink array if shiftAmount is negative
            self.length = len + l_shiftAmount;
            return out;
        }

        prototype function splice(start, deleteCount)
            Array.splice(this, arguments);

        intrinsic function splice(...args:Array):Array
            Array.splice(this, arguments);

        static function unshift(A:Array, args:Array) : uint {
            let len:uint = A.length;
            let argslen:uint = uint(args.length);
            let k:uint = len;

            while (k != 0) {
                k--;
                let d:uint = k + argslen;
                if (k in A)
                    A[d] = A[k];
                else
                    delete A[d];
            }

            for (let i:uint = 0; i < argslen; i++)
                A[i] = args[i];

            A.length = len+argslen;   /* Required by E262-3; observable by means of a setter method on A */

            return len+argslen;
        }

        // 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , … ] ] ] )
        prototype function unshift(...args)
            Array.unshift(this, args);

        intrinsic function unshift(...args:Array):uint
            Array.unshift(this, args);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:map
        // ES4 draft -- static generics

        // FIXME #153: we want a generic "function" shortand to
        // signify "any function" here, but it should signify
        // "callable", not "subtype of Function"

        static function map(object:Object!, mapper/*:function*/, thisObj:Object=null): Array {
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

        static function forEach(object:Object!, eacher/*function*/, thisObj:Object=null): void {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof eacher != "function")
                throw new TypeError("Function object required to 'forEach'");

            for (let i = 0, limit = object.length ; i < limit ; i++)
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

        static function filter(object:Object!, checker/*function*/, thisObj:Object=null): Array {
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

        static function every(object:Object!, checker/*:function*/, thisObj:Object): boolean {
            // FIXME #153: this type test goes away if the type annotation
            // above can be used.
            if (typeof checker != "function")
                throw new TypeError("Function object required to 'every'");

            for (let i = 0, limit = object.length ; i < limit ; i++) {
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

        static function some(object:Object!, checker/*:function*/, thisObj:Object=null): boolean {
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

        static function indexOf(object:Object!, elt, from:Numeric=0): Numeric {
            let len = object.length;

            from = from < 0 ? Math.ceil(from) : Math.floor(from);
            if (from < 0)
                from = from + len;
            
            while (from < len) {
                if (from in object)
                    if (object[from] === elt)
                        return from;
                from = from + 1;
            }
            return -1;
        }

        prototype function indexOf(elt, from=0)
            Array.indexOf(this, elt, ToNumeric(from));

        intrinsic function indexOf(elt, from:Numeric=0): Numeric
            Array.indexOf(this, elt, from);


        // JS1.6 -- http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array:lastIndexOf
        // ES4 draft: static intrinsics

        static function lastIndexOf(object:Object!, elt, from:Numeric=NaN): Numeric {
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
                    if (object[from] === elt)
                        return from;
                from = from - 1;
            }
            return -1;
        }

        prototype function lastIndexOf(elt, from=NaN)
            Array.lastIndexOf(this, elt, ToNumeric(from));

        intrinsic function lastIndexOf(elt, from:Numeric=NaN): Numeric
            Array.lastIndexOf(this, elt, from);


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


        // --------------------------------------------------
        // private utility methods
        // --------------------------------------------------
        helper static function clamp(intValue:double, len:uint):uint
        {
            return (intValue < 0.0)
                 ? (intValue + len < 0.0) ? 0 : uint(intValue + len)
                 : (intValue > len) ? len : uint(intValue);
        }

        private function compare(j:uint, k:uint, comparefn:Comparator):double {
            let x = this[j];
            let y = this[k];
            if (x === undefined) {
                if (y === undefined)
                    return 0;
                return 1;
            }
            if (y === undefined)
                return -1;
            if (comparefn === undefined) {
                x = x.toString();
                y = y.toString();
                if (x < y) return -1;
                if (x > y) return 1;
                return 0;
            }
            return comparefn(x, y);
        }

        // INFORMATIVE note: as noted above, this is a very simple recursive
        // implementation of Quicksort.  While it suffices for spec purposes,
        // it is not efficient enough for a real implementation, which
        // typically faces mostly-ordered inputs.  It is also not a stable
        // sort, which may be desirable but is not required by the spec

        private function qsort(lo:uint, hi:uint, comparefn:Comparator):void {
            if (lo >= hi)
                return;

            let size:uint  = (hi - lo) + 1;
            let pivot:uint = lo + (size / 2);
            let i:uint = lo;
            let j:uint = hi;
            while (i <= j) {
                while (private::compare(i, pivot, comparefn) < 0)  /* FIXME: "private::" should not be necessary */
                    ++i;
                while (private::compare(j, pivot, comparefn) > 0)  /* FIXME: "private::" should not be necessary */
                    --j;
                if (i <= j) {
                    let temp = this[i];
                    this[i] = this[j];
                    this[j] = temp;
                    ++i;
                    --j;
                }
            }

            if (lo < j)
                qsort(lo, j, comparefn);
            if (i < hi)
                qsort(i, hi, comparefn);
        }
    }
}
