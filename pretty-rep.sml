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
structure PP = PPStreamFn(structure Token = StringToken
                          structure Device = SimpleTextIODev)

structure PrettyRep = struct

datatype smlDataRep =
         Ctor of string * (smlDataRep option)
       | Rec of (string * smlDataRep) list
       | List of smlDataRep list
       | Tuple of smlDataRep list
       | String of string
       | Ref of smlDataRep
       | Int of int
       | Real64 of Real64.real
       | Int32 of Int32.int
       | UInt32 of Word32.word
       | Dec of Decimal.DEC
       | DecRm of Decimal.ROUNDING_MODE
       | Bool of bool
       | UniStr of Ustring.STRING

fun ppSmlDataRep stream (rep : smlDataRep) =
    let
        fun nbsp _ = PP.nbSpace stream 1
	fun sp _ = PP.space stream 1
        fun ob _ = PP.openHVBox stream (PP.Rel 2)
        fun cb _ = PP.closeBox stream
        fun str s = PP.string stream s
        fun sub (r : smlDataRep) = ppSmlDataRep stream r
        fun sublist [] = ()
          | sublist (r::rs) = (sub r; List.app (fn x => (str ","; sp(); sub x)) rs)
    in
    case rep of
        Ctor (ctor, NONE) => str ctor
      | Ctor (ctor, SOME arg) => (ob(); str ctor; sp (); sub arg; cb())
      | Rec [] => str "{}"
      | Rec (row::rows) =>
        let
            fun doRow (n,v) = (str n; nbsp(); str "="; nbsp(); sub v)
        in
            (ob(); str "{"; nbsp(); doRow row;
             List.app (fn r => (str ","; sp(); doRow r)) rows;
             str "}"; cb())
        end

      | List rs => (ob(); str "["; sublist rs; str "]"; cb())
      | Tuple rs => (ob(); str "("; sublist rs; str ")"; cb())
      | String s => (str "\""; str (String.toString s); str "\"")
      | Ref r => (ob(); str "("; str "ref"; sp; sub r; str ")"; cb())
      | Bool true => str "true"
      | Bool false => str "false"
      | Int i => str (Int.toString i)
      | Real64 r => str ("(Real64.fromString \"" ^ (Real64.toString r) ^ "\"")
      | UInt32 r => str ("(Word32.fromString \"" ^ (LargeInt.toString (Word32.toLargeInt r)) ^ "\"")
      | Int32 r => str ("(Int32.fromString \"" ^ (Int32.toString r) ^ "\"")
      | Dec r => str ("(Decimal.fromString \"" ^ (Decimal.toString r) ^ "\"")
      | DecRm r => str ("Decimal." ^ (Decimal.rmToString r))
      | UniStr us => (str "\""; str (Ustring.toAscii us); str "\"")
    end

end
