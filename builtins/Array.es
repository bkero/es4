/*
	Attempt at modeling ECMA-262 builtin classes using the new ECMA4 language.
	
	Note that these are intended to reflect ECMA-262 behavior, which may omit
	common-but-nonstandard extensions used in various implementations (e.g., SpiderMonkey).
	
	This also makes no attempt at efficiency of implementation, preferring clarity and
	simplicity instead.
*/

package
{
	public dynamic class Array extends Object
	{		
		// 15.4.1 The Array Constructor Called as a Function
		static function call *(ident, args)
		{
			if (ident == "Array")
			{
				// args is already an Array. just return it.
				return args;
			}
			
			// @todo : is this the right syntax for a super call in this case?
			return super.call(ident, args);
		}

		// 15.4.2 The Array Constructor 
		// 15.4.2.1 new Array( [ item0 [ , item1 [ , ... ] ] ] ) 
		// 15.4.2.2 new Array(len) 
		public function Array(...args)
		{
			var argslen:uint = uint(args.length);
			if (argslen == 1 && (args[0] is Number))
			{
				var dlen:Number = args[0];
				var ulen:uint = uint(dlen);
				if (ulen != dlen)
				{
					throw new RangeError;
				}
				length = ulen;
			}
			else
			{
				length = argslen;
				for (var i:uint = 0; i < argslen; i++)
					this[i] = args[i];
			}
		}

		// 15.4.4 Properties of the Array Prototype Object
		// { DontEnum, DontDelete, ReadOnly } 
		// @todo

		// 15.4.4.1 Array.prototype.constructor 
		// @todo
		
		// 15.4.4.2 Array.prototype.toString ( )
		prototype.toString = function():String
		{
			var a:Array = this;  // throws TypeError if not compatible
			return _join(a, ",");
		}
		public function toString():String
		{
			return _join(this, ",");
		}

		// 15.4.4.3 Array.prototype.toLocaleString ( )
		prototype.toLocaleString = function():String
		{
			return _toLocaleString(this);
		}
		public function toLocaleString():String
		{
			return _toLocaleString(this);
		}
		private static function _toLocaleString(o):String
		{
			var a:Array = o; // throws TypeError if not compatible
			var out:String = "";
			for (var i:uint = 0, n:uint = a.length; i < n; i++)
			{
				var x = a[i];
				if (x != null)
					out += x.toLocaleString();
				if (i+1 < n)
					out += ",";
			}
			return out;
		}

		// 15.4.4.4 Array.prototype.concat ( [ item1 [ , item2 [ , … ] ] ] )
		prototype.concat = function(...args):Array
		{
			return _concat(this, args);
		}
		public function concat(...args):Array
		{
			return _concat(this, args);
		}
		private static function _concat(o, args):Array
		{
			var out:Array = new Array;
			
			if (o is Array)
			{
				var olen:uint = uint(o.length);
				for (var i:uint = 0; i < olen; i++) 
				{
					out.push(o[i]);
				}
			}

			var argslen:uint = (args != null) ? uint(args.length) : 0;
			for (var i:uint = 0; i < argslen; i++) 
			{
				var x = args[i];
				if (x is Array) 
				{
					var xlen:uint = uint(x.length);
					for (var j:uint = 0; j < xlen; j++)
					{
						out.push(x[j]);
					}
				}
				else
				{
					out.push(x);
				}
			}

			return out;
		}

		// 15.4.4.5 Array.prototype.join (separator)
		prototype.join = function(sep = void(0)):String
		{
			return _join(this, sep);
		}
		public function join(sep = void(0)):String
		{
			return _join(this, sep);
		}
		private static function _join(o, sep):String
		{
			var s:String = (sep === void(0)) ? "," : String(sep);
			var out:String = "";
			var olen:uint = uint(o.length);
			for (var i:uint = 0; i < olen; i++)
			{
				var x = o[i];
				if (x != null)
					out += String(x);
				if (i+1 < olen)
					out += s;
			}
			return out;
		}

		// 15.4.4.6 Array.prototype.pop ( )
		prototype.pop = function()
		{
			return _pop(this);
		}
		public function pop()
		{
			return _pop(this);
		}
		private static function _pop(o)
		{
			var olen:uint = uint(o.length);

			if (olen != 0)
			{
				--olen;
				var x = o[olen];
				delete o[olen];
				o.length = olen;
				return x;
			} 
			else
			{
				return void(0);
			}
		}

		// 15.4.4.7 Array.prototype.push ( [ item1 [ , item2 [ , … ] ] ] )
		prototype.push = function(...args):uint
		{
			return _push(this, args);
		}
		public function push(...args):uint
		{
			return _push(this, args);
		}
		private static function _push(o, args):uint
		{
			var olen:uint = uint(o.length);
			var argslen:uint = uint(args.length);
			for (var i:uint = 0, ; i < argslen; i++)
				o[olen++] = args[i];
			o.length = olen;
			return olen;
		}

		// 15.4.4.8 Array.prototype.reverse ( )
		prototype.reverse = function()
		{
			return _reverse(this);
		}
		public function reverse():Array
		{
			return _reverse(this);  // return will cast to Array
		}
		private static function _reverse(o)
		{
			var i:uint = 0;
			var j:uint = uint(o.length);
			if (j)
				j--;

			while (i < j) 
			{
				var front = o[i];
				var back = o[j];
				o[i++] = back;
				o[j--] = front;
			}
			return o;
		}

		// 15.4.4.9 Array.prototype.shift ( )
		prototype.shift = function()
		{
			return _shift(this);
		}
		public function shift()
		{
			return _shift(this);
		}
		private static function _shift(o)
		{
			var olen:uint = uint(o.length);
			if (olen == 0)
			{
				o.length = 0;	// ECMA-262 requires explicit set here
				return void(0);
			}
			else
			{
				// Get the 0th element to return
				var x = o[0];

				// Move all of the elements down
				for (var i:uint = 1; i < olen; i++) 
				{
					o[i-1] = o[i];
				}
				delete o[olen - 1];
				o.length = olen - 1;
			}
		}

		// 15.4.4.10 Array.prototype.slice (start, end)
		prototype.slice = function(A = 0, B = 0xffffffff):Array
		{
			return _slice(this, Number(A), Number(B))
		}
		public function slice(A = 0, B = 0xffffffff):Array
		{
			return _slice(this, Number(A), Number(B))
		}
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
				out.push(o[i]);
			}

			return out;
		}

		// 15.4.4.11 Array.prototype.sort (comparefn)
		// note: this is an implementation that meets the spec, but the spec
		// allows for different sort implementations (quicksort is not required)
		prototype.sort = function(...args):Array
		{
			return _sort(this, args);
		}
		public function sort(...args):Array
		{
			return _sort(this, args);
		}
		private static function _sort(o, compareFn):Array
		{
			var olen:uint = uint(o.length);

			if (olen != 0)
				qsort(0, olen-1, compareFn);

			return this;
		}

		// 15.4.4.12 Array.prototype.splice (start, deleteCount [ , item1 [ , item2 [ , … ] ] ] )
		prototype.splice = function(...args)
		{
			return _splice(this, args);
		}
		public function splice(...args)
		{
			return _splice(this, args);
		}
		private static function _splice(o, args)
		{
			var argslen:uint = uint(args.length);
			if (argslen == 0)
				return void(0);

			if (!(o is Object))
				return null;
			
			var olen:uint = uint(o.length);
			var start:uint = _clampIndex(Number(args[0]), olen);
			var d_deleteCount:Number = argslen > 1 ? Number(args[1]) : (olen - start); 
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
				out.push(o[i + start]);
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
					o[i - shiftAmount] = o[i];
				}
						
				// delete top elements here to match ECMAscript spec (generic object support)
				for (var i:uint = olen - shiftAmount; i < olen; i++) 
				{
					delete o[i];
				}
			} 
			else 
			{
				// Shift the remaining elements up. 
				shiftAmount = uint(l_shiftAmount);

				for (var i:uint = olen; i > end; )  // Note: i is unsigned, can't check if --i >=0.
				{
					--i;
					o[i + shiftAmount] = o[i];
				}
			}

			// Add the items to insert
			for (var i:uint = 0; i < insertCount; i++) 
			{
				o[start+i] = args[i + 2];
			}

			// shrink array if shiftAmount is negative
			o.length = olen + l_shiftAmount;
				
			return out;
		}

		// 15.4.4.13 Array.prototype.unshift ( [ item1 [ , item2 [ , … ] ] ] )
		prototype.unshift = function(...args):uint
		{
			return _unshift(this, args);
		}
		public function unshift(...args):uint
		{
			return _unshift(this, args);
		}
		private static function _unshift(o, args):uint
		{
			var olen:uint = uint(o.length);
			var argslen:uint = uint(args.length);
			var k:uint;
			for (k = olen; k > 0; /*nothing*/)
			{
				k--;
				var d:uint = k + argslen;
				if (k in o)
					o[d] = o[k];
				else
					delete o[d];
			}

			for (var i:uint = 0; i < argslen; i++)
				o[k++] = args[i];

			olen += argslen;
			o.length = olen;
			return olen;
		}		

		// 15.4.5.1 [[Put]] (P, V)
		public function set *(propertyName, value):void
		{
			if (!this.canPut(propertyName))
				return;
			
			var curLength:uint = uint(this.length);
			if (propertyName == "length")
			{
				var valueAsNumber:Number = Number(value);
				var valueAsInt:uint = uint(valueAsNumber);
				if (valueAsInt != valueAsNumber)
				{
					throw new RangeError();
				}
				for (var i:uint = valueAsInt; i < curLength; ++i)
				{
					if (this.hasOwnProperty(i))
						delete this[i];
				}
				// @todo : is this the right syntax for a super call in this case?
				super.set("length", valueAsInt);
			}
			else
			{
				// @todo : is this the right syntax for a super call in this case?
				super.set(propertyName, value);
				var propertyNameAsNumber:Number = Number(propertyName);
				var propertyNameAsInt:uint = uint(propertyNameAsNumber);
				if (propertyNameAsInt == propertyNameAsNumber && propertyNameAsInt >= curLength)
				{
					// @todo : is this the right syntax for a super call in this case?
					super.set("length", propertyNameAsInt+1);	
				}
			}
		}
		
		// 15.4.5.2 length
		prototype.length = 0;
		public function get length():uint
		{
			// @todo: verify this will go thru get*() catchall and not recurse
			return this["length"];
		}
		public function set length(newLength:uint):void
		{
			// @todo: verify this will go thru set*() catchall and not recurse
			this["length"] = newLength;
		}

		// Array.length = 1 per ECMA-262
		public static const length:int = 1;
		
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
			var temp = this[j];
			this[j] = this[k];
			this[k] = temp;
		}

		private function compare(j:uint, k:uint, compareFn:Function):int
		{
			var x = this[j];
			var y = this[k];
			if (x === void(0))
			{
				if (y === void(0))
				{
					return 0;
				}
				else
				{
					return 1;
				}
			} 
			else if (y === void(0))
			{
				return -1;
			}
			else if (compareFn === void(0))
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
							stk[stkptr++] = {lo: lo, hi:right-1}:StackFrame;
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
							stk[stkptr++] = {lo:left, hi:hi}:StackFrame;
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
					lo = stk[stkptr].lo;
					hi = stk[stkptr].hi;
					continue;
				}

				// we've returned to the top, so we are done!
				break;

			} // endless for
		} // qsort
		
	} // Array
} // package
