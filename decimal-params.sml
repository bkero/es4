(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)

structure DecimalParams = struct

datatype DEC = Dec of string

exception DecimalException of string

datatype ROUNDING_MODE =
         Ceiling
       | Floor
       | Up
       | Down
       | HalfUp
       | HalfDown
       | HalfEven

fun rmToString (rm:ROUNDING_MODE) 
    : string = 
    case rm of 
         Ceiling => "Ceiling"
       | Floor => "Floor"
       | Up => "Up"
       | Down => "Down"
       | HalfUp => "HalfUp"
       | HalfDown => "HalfDown"
       | HalfEven => "HalfEven"	

datatype DECIMAL_OPERATOR =
         Abs
       | Add
       | Compare
       | CompareTotal
       | Divide
       | DivideInteger
       | Exp
       | Ln
       | Log10
       | Max
       | Min
       | Minus
       | Multiply
       | Normalize
       | Plus
       | Power
       | Quantize
       | Remainder
       | RemainderNear
       | Rescale
       | SameQuantum
       | SquareRoot
       | Subtract
       | ToIntegralValue

fun opToString (dop:DECIMAL_OPERATOR)
    : string =
    case dop of
         Abs => "abs"
       | Add => "add"
       | Compare => "compare"
       | CompareTotal => "compareTotal"
       | Divide => "divide"
       | DivideInteger => "divideInteger"
       | Exp => "exp"
       | Ln => "ln"
       | Log10 => "log10"
       | Max => "max"
       | Min => "min"
       | Minus => "minus"
       | Multiply => "multiply"
       | Normalize => "normalize"
       | Plus => "plus"
       | Power => "power"
       | Quantize => "quantize"
       | Remainder => "remainder"
       | RemainderNear => "remainderNear"
       | Rescale => "rescale"
       | SameQuantum => "sameQuantum"
       | SquareRoot => "squareRoot"
       | Subtract => "subtract"
       | ToIntegralValue => "toIntegralValue"

end
