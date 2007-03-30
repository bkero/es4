(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* An implementation of decimal128 arithmetic (via an external helper program). *)

structure Decimal = struct 

exception DecimalException of string

fun log ss = (List.app (fn x => (TextIO.print x; TextIO.print " ")) ss; 
	      TextIO.print "\n")
val doTrace = ref false
fun trace ss = if (!doTrace) then log ("[decimal]" :: ss) else ()


datatype DEC = Dec of string


datatype ROUNDING_MODE =
         Ceiling
       | Floor
       | Up
       | Down
       | HalfUp
       | HalfDown
       | HalfEven


val defaultPrecision = 34
val defaultRoundingMode = HalfEven


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


fun runOp (precision:int)
	  (mode:ROUNDING_MODE)
	  (operator:string)
	  (a:DEC)
	  (b:DEC option) 
    : DEC = 
    let 
	val prog = "decimal"
	val precStr = Int.toString precision
	val modeStr = rmToString mode
	val (Dec aval) = a
	val argv = case b of 
		       NONE => [prog, precStr, modeStr, operator, aval]
		     | SOME (Dec bval) => [prog, precStr, modeStr, operator, aval, bval]

	val {infd, outfd} = Posix.IO.pipe ()
	val pid = Posix.Process.fork ()
    in
	case pid of 
	    NONE => 
	    (Posix.IO.close infd;
	     trace ("query:" :: argv);
	     Posix.IO.dup2 {old=outfd, new=Posix.FileSys.stdout}; 
	     Posix.Process.execp (prog, argv))

	  | SOME child => 
	    let 
		val _ = Posix.IO.close outfd
		val vec = Posix.IO.readVec (infd, 1024)                  
		fun getCharN (n:int) = Char.chr (Word8.toInt (Word8Vector.sub (vec, n)))
		val _ = Posix.Process.wait ()
        val _ = Posix.IO.close infd
		val res = CharVector.tabulate (Word8Vector.length vec, getCharN)
	    in
		if String.isPrefix "ERROR:" res
		then raise (DecimalException res)
		else (trace ["reply:", res];
		      (Dec (List.hd (String.tokens (fn c => not (Char.isGraph c)) res))))
	    end
    end


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
        then (SOME (runOp prec mode "normalize" (Dec s) NONE)
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
    runOp prec mode "add" a (SOME b)


fun subtract (prec:int)
	         (mode:ROUNDING_MODE)
	         (a:DEC) 
	         (b:DEC) 
    : DEC = 
    runOp prec mode "subtract" a (SOME b)


fun multiply (prec:int)
	         (mode:ROUNDING_MODE)
	         (a:DEC) 
	         (b:DEC) 
    : DEC = 
    runOp prec mode "multiply" a (SOME b)


fun divide (prec:int)
	       (mode:ROUNDING_MODE)
	       (a:DEC) 
	       (b:DEC) 
    : DEC = 
    runOp prec mode "divide" a (SOME b)


fun abs (prec:int)
	    (mode:ROUNDING_MODE)
	    (a:DEC) 
    : DEC = 
    runOp prec mode "abs" a NONE


fun fromLargeInt (i:LargeInt.int)
    : DEC =
    case fromStringDefault (LargeInt.toString i) of
        SOME d => d
      | NONE => raise (DecimalException "converting from LargeInt")


fun toLargeInt (prec:int)
	           (mode:ROUNDING_MODE)
	           (a:DEC) 
    : LargeInt.int = 
    case runOp prec mode "toIntegralValue" a NONE of
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
    runOp prec mode "minus" a NONE


fun remainder (prec:int)
	          (mode:ROUNDING_MODE)
	          (a:DEC) 
	          (b:DEC) 
    : DEC = 
    runOp prec mode "remainder" a (SOME b)


fun compare (prec:int)
	        (mode:ROUNDING_MODE)
	        (a:DEC) 
	        (b:DEC) 
    : order = 
    case runOp prec mode "compare" a (SOME b) of
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
