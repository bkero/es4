

package
{
	public dynamic class Array extends Object
	{		
		// E262 {DontEnum, DontDelete}
		public native function get length():uint;
		public native function set length(newLength:uint);

		// Array.length = 1 per ES3
		public static const length:int = 1;
		
		// ECMA 15.4.2.2
		public function Array(...args)
		{
			var argslen:uint = uint(args.length);
			if (argslen == 1 && (args.getvalue(0) is Number))
			{
				var dlen:Number = args.getvalue(0);
				var ulen:uint = dlen;
				if (ulen != dlen)
				{
					throw new RangeError("Array Index Not Integer:" + dlen);
				}
				length = ulen;
			}
			else
			{
				length = argslen;
				for (var i:uint = 0; i < argslen; i++)
					this.putvalue(i, args.getvalue(i));
			}
		}

//        // the Array constructor called as a function
 // @todo
//  static intrinsic function call()

// --------------------------------------------------
// non-generic unbound {DE} properties of Array.prototype
// --------------------------------------------------
//	dynamic prototype function toString(this:Array)
		prototype.toString = function():String
		{
			var a:Array = this;  // TypeError if not compatible
			return _join(a, ",");
		}

//	dynamic prototype function toLocaleString(this:Array)
		prototype.toLocaleString = function():String
		{
			var a:Array = this; // TypeError if not compatible
			var out:String = "";
			for (var i:uint = 0, n:uint = a.length; i < n; i++)
			{
				var x = a.getvalue(i);
				if (x != null)
					out += x.toLocaleString();
				if (i+1 < n)
					out += ",";
			}
			return out;
		}

// --------------------------------------------------
// bound {DE,DD,RO} methods 
// --------------------------------------------------
//	ActionScript function toString()
		public function toString():String
		{
			return _join(this, ",");
		}

//	ActionScript function toLocaleString()
		public function toLocaleString():String
		{
			var out:String = "";
			for (var i:uint = 0, n:uint = this.length; i < n; i++)
			{
				var x = this.getvalue(i);
				if (x != null)
					out += x.toLocaleString();
				if (i+1 < n)
					out += ",";
			}
			return out;
		}

// --------------------------------------------------
// generic unbound {DE} properties of Array.prototype
// --------------------------------------------------
//	dynamic prototype function concat(this:Object, ...items)
		private static function _concat(o, args:Array):Array
		{
			var out:Array = new Array;
			
			if (o is Array)
			{
				var olen:uint = uint(o.length);
				for (var i:uint = 0; i < olen; i++) 
				{
					out.push(o.getvalue(i));
				}
			}

			var argslen:uint = (args != null) ? uint(args.length) : 0;
			for (var i:uint = 0; i < argslen; i++) 
			{
				var x = args.getvalue(i);
				if (x is Array) 
				{
					var xlen:uint = uint(x.length);
					for (var j:uint = 0; j < xlen; j++)
					{
						out.push(x.getvalue(j));
					}
				}
				else
				{
					out.push(x);
				}
			}

			return out;
		}

		public function concat(...args):Array
		{
			return _concat(this, args);
		}
		prototype.concat = function(...args):Array
		{
			return _concat(this, args);
		}

//	dynamic prototype function join(this:Object, separator)
		private static function _join(o, sep):String
		{
			var s:String = (sep === undefined) ? "," : String(sep);
			var out:String = "";
			var olen:uint = uint(o.length);
			for (var i:uint = 0; i < olen; i++)
			{
				var x = o.getvalue(i);
				if (x != null)
					out += String(x);
				if (i+1 < olen)
					out += s;
			}
			return out;
		}
		public function join(sep = void 0):String
		{
			return _join(this, sep);
		}
		prototype.join = function(sep = void 0):String
		{
			return _join(this, sep);
		}

//	dynamic prototype function pop(this:Object)
		private static function _pop(o)
		{
			var olen:uint = uint(o.length);

			if (olen != 0)
			{
				--olen;
				var x = o.getvalue(olen);
				o.length = olen;
				return x;
			} 
			else
			{
				return undefined;
			}
		}
		public function pop()
		{
			return _pop(this);
		}
		prototype.pop = function()
		{
			return _pop(this);
		}

//	dynamic prototype function push(this:Object, ...items)
		private static function _push(o, args):uint
		{
			var olen:uint = uint(o.length);
			var argslen:uint = uint(args.length);
			for (var i:uint = 0, ; i < argslen; i++)
				o.putvalue(olen++, args.getvalue(i));
			o.length = olen;
			return olen;
		}
		public function push(...args):uint
		{
			return _push(this, args);
		}
		prototype.push = function(...args):uint
		{
			return _push(this, args);
		}

//	dynamic prototype function reverse(this:Object)
		private static function _reverse(o)
		{
			var i:uint = 0;
			var j:uint = uint(o.length);
			if (j)
				j--;

			while (i < j) 
			{
				var front = o.getvalue(i);
				var back = o.getvalue(j);
				o.putvalue(i++, back);
				o.putvalue(j--, front);
			}
			return o;
		}
		public function reverse():Array
		{
			return _reverse(this);  // return will cast to Array
		}
		prototype.reverse = function()
		{
			return _reverse(this);
		}

//	dynamic prototype function shift(this:Object)
		private static function _shift(o)
		{
			var olen:uint = uint(o.length);
			if (olen == 0)
			{
				o.length = 0;	// ES3 requires explicit set here
				return undefined;
			}
			else
			{
				// Get the 0th element to return
				var x = o.getvalue(0);

				// Move all of the elements down
				for (var i:uint = 1; i < olen; i++) 
				{
					o.putvalue(i-1, o.getvalue(i));
				}
				o.deletevalue(olen - 1);
				o.length = olen - 1;
			}
		}
		public function shift()
		{
			return _shift(this)
		}
		prototype.shift = function()
		{
			return _shift(this)
		}

//	dynamic prototype function slice(this:Object, start, end)
		private static function _slice(o, A:Number, B:Number):Array
		{
			var olen:uint = uint(o.length);

			// if a param is passed then the first one is A
			// if no params are passed then A = 0
			var a:uint = _clampIndex(A, olen);
			var b:uint = _clampIndex(B, olen);
			if (b < a)
				b = a;

			var out:Array = new Array;
			for (var i:uint = a; i < b; i++) 
			{
				out.push(o.getvalue(i));
			}

			return out;
		}
		public function slice(A = 0, B = 0xffffffff):Array
		{
			return _slice(this, Number(A), Number(B))
		}
		prototype.slice = function(A = 0, B = 0xffffffff):Array
		{
			return _slice(this, Number(A), Number(B))
		}

//	dynamic prototype function sort(this:Object, comparefn)
		// note this is an implementation that meets the spec, but the spec
		// allows for different sort implementations (quicksort is not required)
		private static function _sort(o, compareFn):Array
		{
			var olen:uint = uint(o.length);

			if (olen != 0)
				qsort(0, olen-1, compareFn);

			return this;
		}
		public function sort(...args):Array
		{
			return _sort(this, args);
		}
		prototype.sort = function(...args):Array
		{
			return _sort(this, args);
		}

//	dynamic prototype function splice(this:Object, start, deleteCount, ...items)
		private static function _splice(o, args:Array)
		{
			var argslen:uint = uint(args.length);
			if (argslen == 0)
				return undefined;

			if (!(o is Object))
				return null;
			
			var olen:uint = uint(o.length);
			var start:uint = _clampIndex(Number(args.getvalue(0)), olen);
			var d_deleteCount:Number = argslen > 1 ? Number(args.getvalue(1)) : (olen - start); 
			var deleteCount:uint = (d_deleteCount < 0) ? 0 : uint(d_deleteCount);
			if (deleteCount > (olen - start)) 
			{
				deleteCount = olen - start;
			}
			var end:uint = start + deleteCount;

			// Copy out the elements we are going to remove
			var out:Array = new Array();

			for (var i:uint = 0; i < deleteCount; i++) 
			{
				out.push(o.getvalue(i + start));
			}

			var insertCount:uint = (argslen > 2) ? (argslen - 2) : 0;
			var l_shiftAmount:Number = insertCount - deleteCount; // Number because result could be negative
			var shiftAmount:uint;

			// delete items by shifting elements past end (of delete) by l_shiftAmount
			if (l_shiftAmount < 0) 
			{
				// Shift the remaining elements down
				shiftAmount = uint(-l_shiftAmount);

				for (var i:uint = end; i < olen; i++) 
				{
					o.putvalue(i - shiftAmount, o.getvalue(i));
				}
						
				// delete top elements here to match ECMAscript spec (generic object support)
				for (var i:uint = olen - shiftAmount; i < olen; i++) 
				{
					o.deletevalue(i);
				}
			} 
			else 
			{
				// Shift the remaining elements up. 
				shiftAmount = uint(l_shiftAmount);

				for (var i:uint = olen; i > end; )  // Note: i is unsigned, can't check if --i >=0.
				{
					--i;
					o.putvalue(i + shiftAmount, o.getvalue(i));
				}
			}

			// Add the items to insert
			for (var i:uint = 0; i < insertCount; i++) 
			{
				o.putvalue(start+i, args.getvalue(i + 2));
			}

			// shrink array if shiftAmount is negative
			o.length = olen + l_shiftAmount;
				
			return out;
		}

		// splice with zero args returns undefined. All other cases return Array.
		public function splice(...args)
		{
			return _splice(this, args);
		}
		prototype.splice = function(...args)
		{
			return _splice(this, args);
		}

//	dynamic prototype function unshift(this:Object, ...items) // length=0, bug in E262 that unshift.length=1
		private static function _unshift(o, args:Array):uint
		{
			var olen:uint = uint(o.length);
			var argslen:uint = args.length;
			var k:uint;
			for (k = olen; k > 0; /*nothing*/)
			{
				k--;
				var d:uint = k + argslen;
				if (k in o)
					o.putvalue(d, o.getvalue(k));
				else
					o.deletevalue(d);
			}

			for (var i:uint = 0; i < argslen; i++)
				o.putvalue(k++, args.getvalue(i));

			olen += argslen;
			o.length = olen;
			return olen;
		}		
		public function unshift(...args):uint
		{
			return _unshift(this, args);
		}
		prototype.unshift = function(...args):uint
		{
			return _unshift(this, args);
		}

// --------------------------------------------------
// --------------------------------------------------
		_dontEnumPrototype(prototype);

// --------------------------------------------------
// bound internal methods of Array instances
		private native function putvalue(propertyName, value):void;
		private native function getvalue(propertyName):*;
		private native function deletevalue(propertyName):void;
		
// --------------------------------------------------
// private utility methods
// --------------------------------------------------
		private static function _clampIndex(intValue:Number, len:uint):uint
		{
			var clamped:uint;
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

		private function swap(j:uint, k:uint):void
		{
			var temp = this.getvalue(j);
			this.putvalue(j, this.getvalue(k));
			this.putvalue(k, temp);
		}

		private function compare(j:uint, k:uint, compareFn:Function):int
		{
			var x = this.getvalue(j);
			var y = this.getvalue(k);
			if (x === undefined)
			{
				if (y === undefined)
				{
					return 0;
				}
				else
				{
					return 1;
				}
			} 
			else if (y === undefined)
			{
				return -1;
			}
			else if (compareFn === undefined)
			{
				x = x.toString();
				y = y.toString();
				if (x < y) return -1; 
				else if (x > y) return 1; 
				else return 0; 
			}
			else
			{
				return compareFn(x, y);
			}
		}

		private function qsort(lo:uint, hi:uint, compareFn:Function):void
		{
			// This is an iterative implementation of the recursive quick sort.
			// Recursive implementations are basically storing nested (lo,hi) pairs
			// in the stack frame, so we can avoid the recursion by storing them
			// in an array.
			//
			// Once partitioned, we sub-partition the smaller half first. This means
			// the greatest stack depth happens with equal partitions, all the way down,
			// which would be 1 + log2(size), which could never exceed 33.

			var size:uint;
			type StackFrame = { lo:uint, hi:uint };
			var stk:[StackFrame] = []:[StackFrame];
			var stkptr:uint = 0;

			// leave without doing anything if the array is empty (lo > hi) or only one element (lo == hi)
			if (lo >= hi)
				return;

			// code below branches to this label instead of recursively calling qsort()
			for (;;)
			{
				size = (hi - lo) + 1; // number of elements in the partition

				// an efficient implementation would special-case small partitions,
				// but this is skipped here in the interest of code simplicity
				if (size >= 2) 
				{
					// qsort()-ing a near or already sorted list goes much better if
					// you use the midpoint as the pivot, but the algorithm is simpler
					// if the pivot is at the start of the list, so move the middle
					// element to the front!
					var pivot:uint = lo + (size / 2);
					swap(pivot, lo);

					var left:uint = lo;
					var right:uint = hi + 1;

					for (;;) 
					{
						// Move the left right until it's at an element greater than the pivot.
						// Move the right left until it's at an element less than the pivot.
						// If left and right cross, we can terminate, otherwise swap and continue.
						//
						// As each pass of the outer loop increments left at least once,
						// and decrements right at least once, this loop has to terminate.

						do  {
							left++;
						} while ((left <= hi) && (compare(left, lo, compareFn) <= 0));

						do  {
							right--;
						} while ((right > lo) && (compare(right, lo, compareFn) >= 0));

						if (right < left)
							break;

						swap(left, right);
					}

					// move the pivot after the lower partition
					swap(lo, right);

					// The array is now in three partions:
					//	1. left partition	: i in [lo, right), elements less than or equal to pivot
					//	2. center partition	: i in [right, left], elements equal to pivot
					//	3. right partition	: i in (left, hi], elements greater than pivot
					// NOTE : [ means the range includes the lower bounds, ( means it excludes it, with the same for ] and ).

					// Many quick sorts recurse into the left partition, and then the right.
					// The worst case of this can lead to a stack depth of size -- for instance,
					// the left is empty, the center is just the pivot, and the right is everything else.
					//
					// If you recurse into the smaller partition first, then the worst case is an
					// equal partitioning, which leads to a depth of log2(size).
					if ((right - 1 - lo) >= (hi - left)) 
					{
						if ((lo + 1) < right) 
						{
							stk.putvalue(stkptr++, {lo: lo, hi:right-1}:StackFrame);
						}

						if (left < hi)
						{
							lo = left;
							continue;	/* do small recursion */
						}
					}
					else
					{
						if (left < hi)
						{
							stk.putvalue(stkptr++, {lo:left, hi:hi}:StackFrame);
						}

						if ((lo + 1) < right)
						{
							hi = right - 1;
							continue;	/* do small recursion */
						}
					}
				}

				// we reached the bottom of the well, pop the nested stack frame
				if (--stkptr >= 0)
				{
					lo = stk.getvalue(stkptr).lo;
					hi = stk.getvalue(stkptr).hi;
					continue;
				}

				// we've returned to the top, so we are done!
				break;

			} // endless for
		}

	}


}
