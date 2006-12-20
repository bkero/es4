/* -*- mode: java; mode: font-lock; tab-width: 4 -*-
 *
 * ECMAScript 4 builtins - the "Array" object
 * ES-262-3 15.X
 * ES-262-4 draft
 *
 * Status: not reviewed against specs.
 */

dynamic class Array extends Object
{		
    // 15.4.1 The Array Constructor Called as a Function
    static intrinsic function call(...args) {
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

    function Array(...args) {
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
    prototype.length = 0;

    // 15.4.4.1 Array.prototype.constructor
    // prototype.constructor = Array;
    
    // 15.4.4.2 Array.prototype.toString ( )
    prototype function toString()
        this.intrinsic::join(",");
    intrinsic function toString():String
        this.intrinsic::join(",");

    // 15.4.4.3 Array.prototype.toLocaleString ( )
    prototype function toLocaleString()
        this.intrinsic::toLocaleString();
    intrinsic function toLocaleString():String {
        let out:String = "";
        for (let i:uint = 0, n:uint = a.length; i < n; i++) {
            if (i != 0)
                out += ",";
            let x = a[i];
            if (x != null)
                out += x.toLocaleString();
        }
        return out;
    }

    // 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , … ] ] ] )
    prototype function concat(array)
        this.intrinsic::concat.apply(arguments);
    intrinsic function concat(...args):Array {
        let out:Array = new Array;
        
        if (this is Array) {
            let len:uint = uint(this.length);
            for (let i:uint = 0; i < len; i++)
                out.push(this[i]);
        }

        let argslen:uint = (args != null) ? uint(args.length) : 0;
        for (let i:uint = 0; i < argslen; i++) {
            let x = args[i];
            if (x is Array) {
                let xlen:uint = uint(x.length);
                for (let j:uint = 0; j < xlen; j++)
                    out.push(x[j]);
                continue;
            }
            out.push(x);
        }

        return out;
    }

    // 15.4.4.5 Array.prototype.join (separator)
    prototype function join(sep = void 0)
        this.intrinsic::join(sep);
    intrinsic function join(sep = void 0):String {
        let s:String = (sep === void 0) ? "," : String(sep);
        let out:String = "";
        let len:uint = uint(this.length);
        for (let i:uint = 0; i < len; i++) {
            if (i != 0)
                out += s;
            let x = this[i];
            if (x != null)
                out += String(x);
        }
        return out;
    }

    // 15.4.4.6 Array.prototype.pop ( )
    prototype function pop()
        this.intrinsic::pop();
    intrinsic function pop():* {
        let len:uint = uint(this.length);

        if (len != 0) {
            --len;
            let x = this[len];
            delete this[len];
            this.length = len;
            return x;
        }
        return void 0;
    }

    // 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , … ] ] ] )
    prototype function push(item)
        this.intrinsic::push.apply(arguments);
    intrinsic function push(...args:Array):uint {
        let len:uint = uint(this.length);
        let argslen:uint = uint(args.length);

        for (let i:uint = 0; i < argslen; i++)
            this[len++] = args[i];
        this.length = len;
        return len;
    }

    // 15.4.4.8 Array.prototype.reverse ( )
    prototype function reverse()
        this.intrinsic::reverse();
    intrinsic function reverse():Array {
        let i:uint = 0;
        let j:uint = uint(this.length);
        if (j)
            --j;

        while (i < j) {
            [this[i], this[j]] = [this[j], this[i]];
            i++;
            j--;
        }
        return this;
    }

    // 15.4.4.9 Array.prototype.shift ( )
    prototype function shift()
        this.intrinsic::shift();
    intrinsic function shift() {
        let len:uint = uint(this.length);
        if (len == 0) {
            this.length = 0;	// ECMA-262 requires explicit set here
            return void 0;
        }

        // Get the 0th element to return
        let x = this[0];

        // Move all of the elements down
        for (let i:uint = 1; i < len; i++)
            this[i-1] = this[i];
        delete this[len - 1];
        this.length = len - 1;
    }

    // 15.4.4.10 Array.prototype.slice (start, end)
    prototype function slice(start, end) {
        if (start === void 0)
            start = 0;
        if (end === void 0)
            end = Infinity;
        return this.intrinsic::slice(Number(start), Number(end))
    }

    intrinsic function slice(start:Number = 0, end:Number = Infinity):Array {
        let len:uint = uint(this.length);

        // if a param is passed then the first one is start
        // if no params are passed then start = 0
        let a:uint = clampIndex(start, len);
        let b:uint = clampIndex(end, len);
        if (b < a)
            b = a;

        let out:Array = new Array;
        for (let i:uint = a; i < b; i++)
            out.push(this[i]);

        return out;
    }

    // 15.4.4.11 Array.prototype.sort (comparefn)
    // Note: this is an implementation that meets the spec, but the spec
    // allows for different sort implementations (quicksort is not required)
    type Comparator = function (x:*, y:*):Number;

    prototype function sort(comparefn)
        this.intrinsic::sort(comparefn);

    intrinsic function sort(comparefn:Comparator):Array {
        let len:uint = uint(this.length);

        if (len > 1)
            _qsort(0, len-1, comparefn);

        return this;
    }

    // 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , … ] ] ] )
    prototype function splice(start, deleteCount)
        this.intrinsic::splice.apply(arguments);
    intrinsic function splice(...args:Array):Array {
        let argslen:uint = uint(args.length);
        if (argslen == 0)
            return void 0;

        let len:uint = uint(this.length);
        let start:uint = clampIndex(Number(args[0]), len);
        let d_deleteCount:Number = argslen > 1 ? Number(args[1]) : (len - start);
        let deleteCount:uint = (d_deleteCount < 0) ? 0 : uint(d_deleteCount);
        if (deleteCount > (len - start))
        {
                deleteCount = len - start;
        }
        let end:uint = start + deleteCount;

        // Copy out the elements we are going to remove
        let out:Array = new Array();

        for (let i:uint = 0; i < deleteCount; i++)
        {
                out.push(this[i + start]);
        }

        let insertCount:uint = (argslen > 2) ? (argslen - 2) : 0;
        let l_shiftAmount:Number = insertCount - deleteCount; // Number because result could be negative
        let shiftAmount:uint;

        // delete items by shifting elements past end (of delete) by l_shiftAmount
        if (l_shiftAmount < 0)
        {
                // Shift the remaining elements down
                shiftAmount = uint(-l_shiftAmount);

                for (let i:uint = end; i < len; i++)
                {
                        this[i - shiftAmount] = this[i];
                }
                                
                // delete top elements here to match ECMAscript spec (generic object support)
                for (let i:uint = len - shiftAmount; i < len; i++)
                {
                        delete this[i];
                }
        }
        else
        {
                // Shift the remaining elements up.
                shiftAmount = uint(l_shiftAmount);

                for (let i:uint = len; i > end; )  // Note: i is unsigned, can't check if --i >=0.
                {
                        --i;
                        this[i + shiftAmount] = this[i];
                }
        }

        // Add the items to insert
        for (let i:uint = 0; i < insertCount; i++)
        {
                this[start+i] = args[i + 2];
        }

        // shrink array if shiftAmount is negative
        this.length = len + l_shiftAmount;
                
        return out;
    }

    // 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , … ] ] ] )
    prototype function unshift(...args)
    {
            return _unshift(this, args);
    }
    intrinsic function unshift(...args:Array):uint
    {
            return _unshift(this, args);
    }
    private static function _unshift(o, args):uint
    {
            let len:uint = uint(o.length);
            let argslen:uint = uint(args.length);
            let k:uint;
            for (k = len; k > 0; /*nothing*/)
            {
                    k--;
                    let d:uint = k + argslen;
                    if (k in o)
                            o[d] = o[k];
                    else
                            delete o[d];
            }

            for (let i:uint = 0; i < argslen; i++)
                    o[k++] = args[i];

            len += argslen;
            o.length = len;
            return len;
    }		

    // 15.4.5.1 [[Put]] (P, V)
    // @todo: this will not function the way we want with current catchall behavior!
    function set *(propertyName, value):void
    {
            let curLength:uint = uint(this.length);
            intrinsic::set(propertyName, value);
            let propertyNameAsNumber:Number = Number(propertyName);
            let propertyNameAsInt:uint = uint(propertyNameAsNumber);
            if (propertyNameAsInt == propertyNameAsNumber && propertyNameAsInt >= curLength)
            {
                    this.length = propertyNameAsInt+1;	
            }
    }
    
    // 15.4.5.2 length
    private var _length:uint = 0;
    public function get length():*
    {
            return this._length;
    }
    
    // ECMA-262 requires a RangeError if non-ints are passed in,
    // so we must not type it as uint in the setter's signature
    public function set length(newLength:*):void
    {
            let curLength:uint = uint(this.length);
            let valueAsNumber:Number = Number(newLength);
            let valueAsInt:uint = uint(valueAsNumber);
            if (valueAsInt != valueAsNumber)
            {
                    throw new RangeError();
            }
            for (let i:uint = valueAsInt; i < curLength; ++i)
            {
                    if (this.hasOwnProperty(i))
                            delete this[i];
            }
            this._length = valueAsInt;
    }

    // --------------------------------------------------
    // private utility methods
    // --------------------------------------------------
    private static function clampIndex(intValue:Number, len:uint):uint
    {
            let clamped:uint;
            if (intValue < 0.0)
            {
                    if (intValue + len < 0.0)
                            clamped = 0;
                    else
                            clamped = uint(intValue + len);
            }
            else if (intValue > len)
                    clamped = len;
            else if (isNaN(intValue)) // is NaN->uint conversion well-defined? if so, this may be unnecessary
                    clamped = 0;
            else
                    clamped = uint(intValue);

            return clamped;
    }

    private function _compare(j:uint, k:uint, comparefn:Comparator):Number
    {
            let x = this[j];
            let y = this[k];
            if (x === void 0)
            {
                    if (y === void 0)
                            return 0;
                    return 1;
            }
            else if (y === void 0)
            {
                    return -1;
            }
            else if (comparefn === void 0)
            {
                    x = x.toString();
                    y = y.toString();
                    if (x < y) return -1;
                    if (x > y) return 1;
                    return 0;
            }
            return comparefn(x, y);
    }

    // note that this is (deliberately) a very simple recursive implementation of Quicksort.
    // while it suffices for spec purposes, it is not efficient or performant enough
    // for a real implementation.
    private function _qsort(lo:uint, hi:uint, comparefn:Comparator):void
    {
            if (lo >= hi)
                    return;

            let size:uint  = (hi - lo) + 1;
            let pivot:uint = lo + (size / 2);
            let i:uint = lo;
            let j:uint = hi;
            while (i <= j)
            {
                    while (_compare(i, pivot, comparefn) < 0)
                    {
                            ++i;
                    }
                    while (_compare(j, pivot, comparefn) > 0)
                    {
                            --j;
                    }
                    if (i <= j)
                    {
                            let temp = this[i];
                            this[i] = this[j];
                            this[j] = temp;
                            ++i;
                            --j;
                    }
            }

            if (lo < j)
            {
                    _qsort(lo, j, comparefn);
            }
            if (i < hi)
            {
                    _qsort(i, hi, comparefn);
            }
    } // qsort

    // Array "extras" from JS1.6
    // See http://developer.mozilla.org/en/docs/New_in_JavaScript_1.6#Array_extras
    // The callback function typically takes (item, i, list) parameters
    type Mapper  = function (_:*, _:uint, _:Object):*;
    type Eacher  = function (_:*, _:uint, _:Object):void;
    type Checker = function (_:*, _:uint, _:Object):Boolean;
    type Reducer = function (_:*, _:*, _:uint, _:Object):*;

    prototype function map(mapper, thisObj)
    {
            return _map(mapper, thisObj);
    }
    intrinsic function map(mapper:Mapper, thisObj:Object):Array
    {
            return _map(mapper, thisObj);
    }

    private function _map(mapper:Mapper, thisObj:Object):Array
    {
            let result:Array = [];
            for (let i:uint = 0; i < this.length; i++)
            {
                    result[i] = mapper.call(thisObj, this[i], i, this);
            }
            return result;
    }

    prototype function filter(checker, thisObj)
    {
            return _filter(checker, thisObj);
    }
    intrinsic function filter(checker:Checker, thisObj:Object):Array
    {
            return _filter(checker, thisObj);
    }

    private function _filter(checker:Checker, thisObj:Object):Array
    {
            let result:Array = [];
            for (let i:uint = 0; i < this.length; i++)
            {
                    let item = this[i];
                    if (checker.call(thisObj, item, i, this))
                            result[result.length] = item;
            }
            return result;
    }

    prototype function every(checker, thisObj)
    {
            return _every(checker, thisObj);
    }
    intrinsic function every(checker:Checker, thisObj:Object):Boolean
    {
            return _every(checker, thisObj);
    }

    private function _every(checker:Checker, thisObj:Object):Boolean
    {
            for (let i:uint = 0; i < this.length; i++)
            {
                    if (!checker.call(thisObj, this[i], i, this))
                            return false;
            }
            return true;
    }

    prototype function some(checker, thisObj)
    {
            return _some(checker, thisObj);
    }
    intrinsic function some(checker:Checker, thisObj:Object):Boolean
    {
            return _some(checker, thisObj);
    }

    private function _some(checker:Checker, thisObj:Object):Boolean
    {
            for (let i:uint = 0; i < this.length; i++)
            {
                    if (checker.call(thisObj, this[i], i, this))
                            return true;
            }
            return false;
    }

} // class
