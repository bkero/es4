(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
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
