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

structure QuasiQuote =
struct

    open MLAst MLIdent SMLofNJ

    fun parseMLExp s =
        case MLParser.parseString ("val a = " ^ s) of
             [MARKdecl (_, VALdecl [VALbind (_, e)])] => e
           | _ => raise (Fail "parser returned something unexpected")

    fun parseMLPat s =
        case MLParser.parseString ("val (" ^ s ^ ") = 0") of
             [MARKdecl (_, VALdecl [VALbind (p, _)])] => p
           | _ => raise (Fail "parser returned something unexpected")

    fun lookup (x, []) = NONE
      | lookup (x, ((y,z)::pairs)) = if (x = y) then SOME z else lookup (x, pairs)

    datatype hole = SRChole of string
                  | EXPhole of exp
                  | PAThole of pat

    exception HoleType of string * string

    fun holeType (SRChole _) = "raw"
      | holeType (EXPhole _) = "expression"
      | holeType (PAThole _) = "pattern"

    (* TODO: could be generalized to produce either exps or pats *)

    fun parseQuasiML frags =
        let fun skolemize [] = ([], [])
              | skolemize (((QUOTE s) | (ANTIQUOTE (SRChole s)))::frags) =
                    let val (ss, skolems) = skolemize frags
                    in
                        (s::ss, skolems)
                    end
              | skolemize ((ANTIQUOTE x)::frags) =
                    let val skolem = gensym "qqSkolem"
                        val (ss, skolems) = skolemize frags
                    in
                        (skolem::ss, (skolem, x)::skolems)
                    end
            val (ss, skolems) = skolemize frags
            fun exp _ (e as (IDexp (IDENT ([], x)))) =
                    (case lookup (x, skolems) of
                          NONE => e
                        | SOME (EXPhole e') => e'
                        | SOME h => raise (HoleType ("exp", holeType h)))
              | exp _ e = e
            fun pat _ (p as (IDpat x)) =
                    (case lookup (x, skolems) of
                          NONE => p
                        | SOME (PAThole p') => p'
                        | SOME h => raise (HoleType ("pat", holeType h)))
              | pat _ p = p
            val NIL = MLAstRewriter.noRewrite
            val rw = #exp(MLAstRewriter.rewrite
                              {exp=exp,pat=pat,decl=NIL,ty=NIL,sexp=NIL})
        in
            rw (parseMLExp (String.concat ss))
        end

    val % = parseQuasiML

end
