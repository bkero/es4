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

structure ES4Model
  : sig val main: string option array option -> unit
    end = 
struct
    fun confirmExit args = List.exists (fn s => (s = "x") orelse (s = "X"))
                                       args

    fun when b thunk = if b then SOME (thunk()) else NONE

    fun isParam (p : string) : bool = String.isPrefix "-" p

    fun parseParam (p : string) : string option =
    (
        when (isParam p)
            (fn _ => String.extract (p, 1, NONE))
    )

    fun main' (a : string list) =
    (
        let val asts = List.map Parser.parseFile a
        in
            TextIO.print "evaluating...\n";
            List.map Eval.evalProgram asts;
            TextIO.print "evaluated!\n"
        end
    )

    fun main (a : string option array option) = 
    (
        let val args = case a of
                            NONE => []
                          | SOME a => List.mapPartial (fn x => x)
													  (Array.foldr (fn (a,b) => a::b) [] a)
            val params = List.mapPartial parseParam args
        in
            main' (List.filter (not o isParam) args);
            when (confirmExit params)
              (fn _ => (
                           TextIO.print("press <enter> to exit\n");
                           System.Console.ReadLine()
                       ));
            ()
        end
    )
end
