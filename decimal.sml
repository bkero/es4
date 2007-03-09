(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* An implementation of decimal128 arithmetic (via an external helper program). *)

structure Decimal = struct 

datatype DEC = Dec of string

fun runOp (precision:int)
	  (mode:Ast.ROUNDING_MODE)
	  (operator:string)
	  (a:DEC)
	  (b:DEC option) 
    : DEC = 
    let 
	val prog = "decimal"
	val precStr = Int.toString precision
	val modeStr = case mode of 
			  Ast.Ceiling => "ROUND_CEILING"
			| Ast.Floor => "ROUND_FLOOR"
			| Ast.Up => "ROUND_UP"
			| Ast.Down => "ROUND_DOWN"
			| Ast.HalfUp => "ROUND_HALF_UP"
			| Ast.HalfDown => "ROUND_HALF_DOWN"
			| Ast.HalfEven => "ROUND_HALF_EVEN"
	val (Dec aval) = a
	val argv = case b of 
		       NONE => [prog, precStr, modeStr, aval]
		     | SOME (Dec bval) => [prog, precStr, modeStr, aval, bval]

	val {infd, outfd} = Posix.IO.pipe ()
	val pid = Posix.Process.fork ()
    in
	case pid of 
	    SOME child => 
	    let 
		val _ = Posix.IO.close outfd
		val vec = Posix.IO.readVec (infd, 1024)
		fun getCharN (n:int) = Char.chr (Word8.toInt (Word8Vector.sub (vec, n)))
	    in
		Posix.Process.wait ();
		Dec (CharVector.tabulate (Word8Vector.length vec, getCharN))
	    end

	  | NONE => 
	    (Posix.IO.close infd;
	     Posix.IO.dup2 {old=outfd, new=Posix.FileSys.stdout}; 
	     Posix.Process.execp (prog, argv))
    end

fun add (mode:Ast.NUMERIC_MODE)
	(a:DEC) 
	(b:DEC) 
    : DEC = 
    runOp (#precision mode) (#roundingMode mode) "add" a (SOME b)

end
