/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "Array" object
 * ES-262-3 15.X
 * ES-262-4 draft
 *
 * Status: not reviewed against specs.
 */

package
{
    use default namespace public;

    dynamic class Array
    {
        use namespace intrinsic;
        use strict;

        // 15.4.1 The Array Constructor Called as a Function
        meta static function invoke(...args) {
            // args is already an Array. just return it.
            return args;
        }

        // 15.4.2 The Array Constructor
        // 15.4.2.1 new Array( [ item0 [ , item1 [ , ... ] ] ] )
        // 15.4.2.2 new Array(len)
        //
        // Here we rely on magic or optimization, since the Array
        // constructor takes a variable number of arguments, which
        // itself requires an array to be constructed.
        //
        // Optimization gets around this by avoiding allocation of the
        // array if all references to args is in the form of
        // args.length and args[n].
        //
        // Magic would solve this by creating an array without
        // invoking the array constructor, ie, essentially making this
        // code pointless.

        function Array(args=[]/*FIXME:...args*/) {
            let argslen:uint = uint(args.length);
            if (argslen == 1 && (args[0] is Number)) {
                let dlen:Number = args[0];
                let ulen:uint = uint(dlen);
                if (ulen != dlen)
                    throw new RangeError;
                length = ulen;
            }
            else {
                length = argslen;
                for (let i:uint = 0; i < argslen; i++)
                    this[i] = args[i];
            }
        }

        // 15.4.4 Properties of the Array Prototype Object
        // prototype.[[Prototype]] = prototype;
        // prototype.[[Class]] = "Array";
        prototype._length = 0;

        // 15.4.4.2 Array.prototype.toString ( )
        prototype function toString(this:Array)
            this.join();

        override intrinsic function toString():String
            this.join();

        // 15.4.4.3 Array.prototype.toLocaleString ( )
        prototype function toLocaleString(this:Array)
            this.toLocaleString();

        override intrinsic function toLocaleString():string {
            let out:String = "";
            for (let i:uint = 0, n:uint = this.length; i < n; i++) {
                if (i != 0)
                    out += ",";
                let x = this[i];
                if (x != null)
                    out += x.toLocaleString();
            }
            return out;
        }

        // 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , … ] ] ] )
        private static function concatHelper(self, args) {
            let out:Array = new Array;
            let outlen:uint = 0;

            if (self is Array)
                args.unshift(self);

            let argslen:uint = args.length;
            for (let i:uint = 0; i < argslen; i++) {
                let x = args[i];
                if (x is Array) {
                    let xlen:uint = x._length;
                    for (let j:uint = 0; j < xlen; j++)
                        out[outlen] = x[j];
                    outlen++;
                    continue;
                }
                out[outlen++] = x;
            }

            return out;
        }

        public static function concat(self, ...args)
            concatHelper(self, args);

        prototype function concat(...args)
            concatHelper(this, args);

        intrinsic function concat(...args):Array
            concatHelper(this, args);

        // 15.4.4.5 Array.prototype.join (separator)
        public static function join(self, sep = undefined) {
            let s:String = (sep === undefined) ? "," : String(sep);
            let out:String = "";
            let len:uint = self.length;
            for (let i:uint = 0; i < len; i++) {
                if (i != 0)
                    out += s;
                let x = self[i];
                if (x != null)
                    out += String(x);
            }
            return out;
        }

        prototype function join(sep = undefined)
            Array.join(this, sep);

        intrinsic function join(sep = undefined):String
            Array.join(this, sep);

        // 15.4.4.6 Array.prototype.pop ( )
        public static function pop(self) {
            let len:uint = self.length;

            if (len != 0) {
                let x = self[--len];
                delete self[len];
                self.length = len;
                return x;
            }
            return undefined;
        }

        prototype function pop()
            Array.pop(this);

        intrinsic function pop():*
            Array.pop(this);

        // 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , … ] ] ] )
        public static function pushHelper(self, args) {
            let len:uint = self.length;
            let argslen:uint = args.length;

            for (let i:uint = 0; i < argslen; i++)
                self[len++] = args[i];
            self.length = len;
            return len;
        }

        public static function push(self, ...args)
            Array.pushHelper(this, args);

        prototype function push(...args)
            Array.pushHelper(this, args);

        intrinsic function push(...args:Array):uint
            Array.pushHelper(this, args);

        // 15.4.4.8 Array.prototype.reverse ( )
        public static function reverse(self) {
            let i:uint = 0;
            let j:uint = self.length;
            let h:uint = j >>> 1;

            while (i < h) {
                --j;
                [self[i], self[j]] = [self[j], self[i]];
                i++;
            }
            return self;
        }

        prototype function reverse()
            Array.reverse(this);

        intrinsic function reverse():Array
            Array.reverse(this);

        // 15.4.4.9 Array.prototype.shift ( )
        public static function shift(self) {
            let len:uint = self.length;
            if (len == 0) {
                self.length = 0;        // ECMA-262 requires explicit set here
                return undefined;
            }

            // Get the 0th element to return
            let x = self[0];

            // Move all of the elements down
            for (let i:uint = 1; i < len; i++)
                self[i-1] = self[i];
            delete self[len - 1];
            self.length = len - 1;
            return x;
        }

        prototype function shift()
            Array.shift(this);

        intrinsic function shift()
            Array.shift(this);

        // 15.4.4.10 Array.prototype.slice (start, end)
        public static function slice(self, start, end) {
            if (start === undefined)
                start = 0;
            if (end === undefined)
                end = Infinity;

            let len:uint = uint(self.length);

            // If a param is passed then the first one is start.
            // If no params are passed then start = 0.
            let a:uint = clamp(start, len);
            let b:uint = clamp(end, len);
            if (b < a)
                b = a;

            let out:Array = new Array;
            for (let i:uint = a; i < b; i++)
                out.push(self[i]);

            return out;
        }

        prototype function slice(start, end)
            Array.slice(this, start, end);

        intrinsic function slice(start:double = 0, end:double = Infinity):Array
            Array.slice(this, start, end);

        // 15.4.4.11 Array.prototype.sort (comparefn)
        // INFORMATIVE: this is an implementation that meets the spec, but the spec
        // allows for different sort implementations (quicksort is not required)
        type Comparator = function (*,*):double;

        public static function sort(self, comparefn) {
            let len:uint = self.length;

            if (len > 1)
                qsort(0, len-1, comparefn);

            return self;
        }

        prototype function sort(comparefn)
            Array.sort(this, comparefn);

        intrinsic function sort(comparefn:Comparator):Array
            Array.sort(this, comparefn);

        // 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , … ] ] ] )
        public static function splice(self, start, deleteCount) {
            let out:Array = new Array();

            let argslen:uint = uint(args.length);
            if (argslen == 0)
                return undefined;

            let len:uint = self.length;
            let start:uint = clamp(double(args[0]), len);
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

        // 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , … ] ] ] )
        prototype function unshift(...args)
            this.unshift(this, args);

        intrinsic function unshift(...args:Array):uint {
            let len:uint = this.length;
            let argslen:uint = uint(args.length);
            let k:uint = len;
            while (k != 0) {
                k--;
                let d:uint = k + argslen;
                if (k in this)
                    this[d] = this[k];
                else
                    delete this[d];
            }

            for (let i:uint = 0; i < argslen; i++)
                this[k++] = args[i];

            len += argslen;
            this.length = len;
            return len;
        }

        // 15.4.5.1 [[Put]] (P, V)
        // @todo: ensure that catchall-set for undeclared properties runs on every set
        meta function set(id, value):void {
            let oldLength:uint = this.length;
            intrinsic::set(id, value);
            let idAsDouble:double = double(id);
            let idAsUint:uint = uint(idAsDouble);
            if (idAsUint == idAsDouble && idAsUint >= oldLength)
                this.length = idAsUint+1;
        }

        // 15.4.5.2 length
        private var _length:uint = 0;
        public function get length():uint
            this._length;

        // ECMA-262 requires a RangeError if non-ints are passed in,
        // so we must not type it as uint in the setter's signature
        public function set length(newLength:uint):void {
            let oldLength:uint = _length;
            let newLengthAsDouble:double = double(newLength);
            let newLengthAsUint:uint = uint(newLengthAsDouble);
            if (newLengthAsUint != newLengthAsDouble)
                throw new RangeError();
            for (let i:uint = newLengthAsUint; i < oldLength; ++i)
                if (this.hasOwnProperty(i))
                    delete this[i];
            _length = newLengthAsUint;
        }

        // --------------------------------------------------
        // private utility methods
        // --------------------------------------------------
        private static function clamp(intValue:double, len:uint):uint
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
                while (compare(i, pivot, comparefn) < 0)
                    ++i;
                while (compare(j, pivot, comparefn) > 0)
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

        // Array "extras" from JS1.6 (@todo: and JS1.8 -- reduce/reduceRight)
        // See http://developer.mozilla.org/en/docs/New_in_JavaScript_1.6#Array_extras
        // The callback function typically takes (item, i, list) parameters
        type Mapper  = function (*, uint, Object):*;
        type Eacher  = function (*, uint, Object):void;
        type Checker = function (*, uint, Object):Boolean;
        type Reducer = function (*, *, uint, Object):*;

        prototype function map(mapper, thisObj)
            this.map(mapper, thisObj);

        intrinsic function map(mapper:Mapper, thisObj:Object):Array {
            let result:Array = [];
            for (let i:uint = 0; i < this.length; i++)
                result[i] = mapper.call(thisObj, this[i], i, this);
            return result;
        }

        prototype function filter(checker, thisObj)
            this.filter(checker, thisObj);

        intrinsic function filter(checker:Checker, thisObj:Object):Array {
            let result:Array = [];
            for (let i:uint = 0; i < this.length; i++) {
                let item = this[i];
                if (checker.call(thisObj, item, i, this))
                    result[result.length] = item;
            }
            return result;
        }

        prototype function every(checker, thisObj)
            this.every(checker, thisObj);

        intrinsic function every(checker:Checker, thisObj:Object):Boolean {
            for (let i:uint = 0; i < this.length; i++) {
                if (!checker.call(thisObj, this[i], i, this))
                    return false;
            }
            return true;
        }

        prototype function some(checker, thisObj)
            this.some(checker, thisObj);

        intrinsic function some(checker:Checker, thisObj:Object):Boolean {
            for (let i:uint = 0; i < this.length; i++) {
                if (checker.call(thisObj, this[i], i, this))
                    return true;
            }
            return false;
        }
    }
}
