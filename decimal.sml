(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)

structure Decimal = struct 

open DecimalParams DecimalNative

val defaultPrecision = 34
val defaultRoundingMode = HalfEven

fun fromString (prec:int)
	           (mode:ROUNDING_MODE)
	           (s:string)
    : DEC option =
    (* Sanitize string: we don't want any shell escapes making it 
     * through here to our subprocess. *)
    let 
        fun safeChar c = (Char.isAlphaNum c orelse 
                          c = #"." orelse 
                          c = #"-" orelse
                          c = #"+")
    in
        if List.all safeChar (String.explode s)
        then (SOME (runOp prec mode Normalize (Dec s) NONE)
              handle DecimalException e => NONE)
        else NONE
    end
				 
fun fromStringDefault (s:string)
    : DEC option = 
    fromString defaultPrecision defaultRoundingMode s


fun toString (d:DEC)
    : string = 
    case d of
	Dec d' => d'


fun add (prec:int)
	    (mode:ROUNDING_MODE)
	    (a:DEC) 
	    (b:DEC) 
    : DEC = 
    runOp prec mode Add a (SOME b)


fun subtract (prec:int)
	         (mode:ROUNDING_MODE)
	         (a:DEC) 
	         (b:DEC) 
    : DEC = 
    runOp prec mode Subtract a (SOME b)


fun multiply (prec:int)
	         (mode:ROUNDING_MODE)
	         (a:DEC) 
	         (b:DEC) 
    : DEC = 
    runOp prec mode Multiply a (SOME b)


fun divide (prec:int)
	       (mode:ROUNDING_MODE)
	       (a:DEC) 
	       (b:DEC) 
    : DEC = 
    runOp prec mode Divide a (SOME b)


fun abs (prec:int)
	    (mode:ROUNDING_MODE)
	    (a:DEC) 
    : DEC = 
    runOp prec mode Abs a NONE


fun fromLargeInt (i:LargeInt.int)
    : DEC =
    case fromStringDefault (LargeInt.toString i) of
        SOME d => d
      | NONE => raise (DecimalException "converting from LargeInt")


fun toLargeInt (prec:int)
	           (mode:ROUNDING_MODE)
	           (a:DEC) 
    : LargeInt.int = 
    case runOp prec mode ToIntegralValue a NONE of
        Dec s => case LargeInt.fromString s of 
                     SOME i => i
                   | NONE => raise (DecimalException "parsing integral value")


fun floor (a:DEC)
    : LargeInt.int = 
    toLargeInt defaultPrecision Floor a


fun minus (prec:int)
	      (mode:ROUNDING_MODE)
	      (a:DEC) 
    : DEC = 
    runOp prec mode Minus a NONE


fun remainder (prec:int)
	          (mode:ROUNDING_MODE)
	          (a:DEC) 
	          (b:DEC) 
    : DEC = 
    runOp prec mode Remainder a (SOME b)


fun compare (prec:int)
	        (mode:ROUNDING_MODE)
	        (a:DEC) 
	        (b:DEC) 
    : order = 
    case runOp prec mode Compare a (SOME b) of
        Dec "-1" => LESS
      | Dec "0" => EQUAL
      | Dec "1" => GREATER
      | _ => raise (DecimalException "bad comparison")


fun isPositiveZero (a:DEC) 
    : bool = 
    case a of 
        Dec "0" => true
      | _ => false


fun isPositiveInf (a:DEC) 
    : bool = 
    case a of 
        Dec "Infinity" => true
      | _ => false


fun isNegativeInf (a:DEC) 
    : bool = 
    case a of 
        Dec "-Infinity" => true
      | _ => false


fun isNaN (a:DEC) 
    : bool = 
    case a of 
        Dec "NaN" => true
      | _ => false


val NaN = Dec "NaN"
val zero = Dec "0"
val one = Dec "1"

end
