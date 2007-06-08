(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
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
 *)
(* *)

structure DecimalNative = struct

open DecimalParams

val decop = _import "decop" : int * int * int * string * string * char array -> int;

fun rmToInt (rm:ROUNDING_MODE)
    : int =
    (case rm of
         Ceiling => 0
       | Floor => 1
       | Up => 2
       | Down => 3
       | HalfUp => 4
       | HalfDown => 5
       | HalfEven => 6);

fun opToInt (dop:DECIMAL_OPERATOR)
    : int =
    (case dop of
         Abs => 0
       | Add => 1
       | Compare => 2
       | CompareTotal => 3
       | Divide => 4
       | DivideInteger => 4
       | Exp => 5
       | Ln => 6
       | Log10 => 7
       | Max => 8
       | Min => 9
       | Minus => 10
       | Multiply => 11
       | Normalize => 12
       | Plus => 13
       | Power => 14
       | Quantize => 15
       | Remainder => 16
       | RemainderNear => 17
       | Rescale => 18
       | SameQuantum => 19
       | SquareRoot => 20
       | Subtract => 21
       | ToIntegralValue => 22);

val DECIMAL128_String = 43; (* from decimal128.h *)

fun arrayToList a = Array.foldr (op ::) [] a;

fun takeUntil p [] = []
  | takeUntil p (x::ls) = if (p x)
                          then []
                          else x::(takeUntil p ls);

fun bufferToString b =
    String.implode (takeUntil (fn c => c = #"\000") (arrayToList b));

fun decErrorString n =
    (case n of
         1 => "bad precision number"
       | 2 => "bad rounding mode"
       | 3 => "bad operator code"
       | 4 => "invalid syntax"
       | 5 => "out of memory"
       | 6 => "decNumber completely wedged"
       | _ => "unknown error")

fun runOp (precision:int)
          (mode:ROUNDING_MODE)
          (operator:DECIMAL_OPERATOR)
          (left:DEC)
          (right:DEC option)
    : DEC =
    let
        val buffer = Array.tabulate (DECIMAL128_String, fn _ => #"\000")
        val Dec left = left
        val right = (case right of
                         NONE => "0"
                       | SOME (Dec s) => s)
    in
        (case decop (precision, rmToInt mode, opToInt operator, left, right, buffer) of
             0 => Dec (bufferToString buffer)
           | n => raise DecimalException (decErrorString n))
    end;

end;
