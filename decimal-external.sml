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
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
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
(* An implementation of decimal128 arithmetic (via an external helper program). *)

structure DecimalNative = struct
open DecimalParams

fun runOp (precision:int)
	  (mode:ROUNDING_MODE)
	  (operator:DECIMAL_OPERATOR)
	  (a:DEC)
	  (b:DEC option)
    : DEC =
    let
	val prog = "decimal"
	val precStr = Int.toString precision
	val modeStr = rmToString mode
	val (Dec aval) = a
    val operator = opToString operator
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

end
