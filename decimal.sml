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
    SOME (runOp prec mode "normalize" (Dec s) NONE)
    handle DecimalException e => NONE
				 
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

end
